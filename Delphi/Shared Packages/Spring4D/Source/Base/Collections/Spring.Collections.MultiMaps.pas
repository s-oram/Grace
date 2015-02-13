{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Collections.MultiMaps;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring.Collections,
  Spring.Collections.Base,
  Spring.Collections.Dictionaries;

type
  TMultiMapBase<TKey, TValue> = class abstract(TMapBase<TKey, TValue>, IMultiMap<TKey, TValue>)
  private
    type
      TGenericPair = Generics.Collections.TPair<TKey, TValue>;

      TEnumerator = class(TEnumeratorBase<TGenericPair>)
      private
        fSource: TMultiMapBase<TKey, TValue>;
        fDictionaryEnumerator: IEnumerator<TPair<TKey, ICollection<TValue>>>;
        fCollectionEnumerator: IEnumerator<TValue>;
      protected
        function GetCurrent: TGenericPair; override;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        function MoveNext: Boolean; override;
      end;

      TValueEnumerator = class(TEnumeratorBase<TValue>)
      private
        fSource: TMultiMapBase<TKey, TValue>;
        fSourceEnumerator: IEnumerator<TGenericPair>;
      protected
        function GetCurrent: TValue; override;
      public
        constructor Create(const source: TMultiMapBase<TKey, TValue>);
        function MoveNext: Boolean; override;
      end;

      TValueCollection = class(TContainedReadOnlyCollection<TValue>)
      private
        fOwner: TMultiMapBase<TKey, TValue>;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const owner: TMultiMapBase<TKey, TValue>);

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}
      end;
  private
    fDictionary: TDictionary<TKey, ICollection<TValue>>;
    fCount: Integer;
    fEmpty: ICollection<TValue>;
    fValues: TValueCollection;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItems(const key: TKey): IReadOnlyCollection<TValue>;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
  {$ENDREGION}
    function CreateCollection: ICollection<TValue>; virtual; abstract;
    function CreateDictionary: TDictionary<TKey, ICollection<TValue>>; virtual; abstract;
  public
    constructor Create; override;
    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TGenericPair>; override;
    function Contains(const value: TGenericPair;
      const comparer: IEqualityComparer<TGenericPair>): Boolean; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); reintroduce; overload;
    function Remove(const key: TKey): Boolean; reintroduce; overload;
    function RemovePair(const key: TKey; const value: TValue): Boolean; override;
    function ExtractPair(const key: TKey; const value: TValue): TGenericPair; override;
    function ContainsPair(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IMultiMap<TKey, TValue>'}
    function ExtractValues(const key: TKey): IReadOnlyCollection<TValue>;
    function TryGetValues(const key: TKey; out values: IReadOnlyCollection<TValue>): Boolean;
    property Items[const key: TKey]: IReadOnlyCollection<TValue> read GetItems; default;
  {$ENDREGION}
  end;

  TMultiMap<TKey, TValue> = class(TMultiMapBase<TKey, TValue>)
  private
    procedure DoKeyChanged(Sender: TObject; const Item: TKey;
      Action: TCollectionChangedAction);
    procedure DoValueChanged(Sender: TObject; const Item: TValue;
      Action: TCollectionChangedAction);
  protected
    function CreateCollection: ICollection<TValue>; override;
    function CreateDictionary: TDictionary<TKey, ICollection<TValue>>; override;
  end;

  TObjectMultiMap<TKey, TValue> = class(TMultiMap<TKey, TValue>)
  private
    fOwnerships: TDictionaryOwnerships;
  protected
    procedure KeyChanged(const item: TKey; action: TCollectionChangedAction); override;
    procedure ValueChanged(const item: TValue; action: TCollectionChangedAction); override;
  public
    constructor Create(ownerships: TDictionaryOwnerships);
  end;

implementation

uses
  Classes,
  RTLConsts,
  SysUtils,
  TypInfo,
  Spring,
  Spring.Collections.Lists,
  Spring.ResourceStrings;


{$REGION 'TMultiMapBase<TKey, TValue>'}

constructor TMultiMapBase<TKey, TValue>.Create;
begin
  inherited Create;
  fDictionary := CreateDictionary;
  fEmpty := TList<TValue>.Create;
  fValues := TValueCollection.Create(Self);
end;

destructor TMultiMapBase<TKey, TValue>.Destroy;
begin
  fValues.Free;
  fDictionary.Free;
  inherited;
end;

procedure TMultiMapBase<TKey, TValue>.Add(const key: TKey; const value: TValue);
var
  list: ICollection<TValue>;
begin
  if not fDictionary.TryGetValue(key, list) then
  begin
    list := CreateCollection;
    fDictionary[key] := list;
  end;

  list.Add(value);
  Inc(fCount);
end;

procedure TMultiMapBase<TKey, TValue>.Clear;
begin
  fDictionary.Clear;
  fCount := 0;
end;

function TMultiMapBase<TKey, TValue>.Contains(const value: TGenericPair;
  const comparer: IEqualityComparer<TGenericPair>): Boolean;
var
  list: ICollection<TValue>;
begin
  if fDictionary.TryGetValue(value.key, list) then
    Result := list.Contains(value.Value)
  else
    Result := False;
end;

function TMultiMapBase<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TMultiMapBase<TKey, TValue>.ContainsPair(const key: TKey;
  const value: TValue): Boolean;
var
  values: IReadOnlyCollection<TValue>;
begin
  Result := TryGetValues(key, values) and values.Contains(value);
end;

function TMultiMapBase<TKey, TValue>.ContainsValue(const value: TValue): Boolean;
var
  list: ICollection<TValue>;
begin
  for list in fDictionary.Values do
    if list.Contains(value) then
      Exit(True);
  Result := False;
end;

function TMultiMapBase<TKey, TValue>.ExtractPair(const key: TKey;
  const value: TValue): TGenericPair;
var
  list: ICollection<TValue>;
begin
  if fDictionary.TryGetValue(key, list) then
  begin
    Result.Key := key;
    Result.Value := list.Extract(value);
  end
  else
    Result := Default(TGenericPair);
end;

function TMultiMapBase<TKey, TValue>.ExtractValues(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  list: ICollection<TValue>;
begin
  if not fDictionary.TryGetValue(key, list) then
    raise EListError.CreateRes(@SGenericItemNotFound);

  Dec(fCount, list.Count);
  fDictionary.Remove(key);
  Result := list as IReadOnlyCollection<TValue>;
end;

function TMultiMapBase<TKey, TValue>.GetCount: Integer;
begin
  Result := fCount;
end;

function TMultiMapBase<TKey, TValue>.GetEnumerator: IEnumerator<TGenericPair>;
begin
  Result := TEnumerator.Create(Self);
end;

function TMultiMapBase<TKey, TValue>.GetItems(
  const key: TKey): IReadOnlyCollection<TValue>;
var
  list: ICollection<TValue>;
begin
  if not fDictionary.TryGetValue(key, list) then
    list := fEmpty;
  Result := list as IReadOnlyCollection<TValue>;
end;

function TMultiMapBase<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fDictionary.Keys;
end;

function TMultiMapBase<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TMultiMapBase<TKey, TValue>.RemovePair(const key: TKey;
  const value: TValue): Boolean;
var
  list: ICollection<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list) and list.Remove(value);
  if Result then
  begin
    Dec(fCount);
    if not list.Any then
      fDictionary.Remove(key)
  end;
end;

function TMultiMapBase<TKey, TValue>.Remove(const key: TKey): Boolean;
var
  list: ICollection<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list);
  if Result then
  begin
    Dec(fCount, list.Count);
    fDictionary.Remove(key);
  end;
end;

function TMultiMapBase<TKey, TValue>.TryGetValues(const key: TKey;
  out values: IReadOnlyCollection<TValue>): Boolean;
var
  list: ICollection<TValue>;
begin
  Result := fDictionary.TryGetValue(key, list);
  if Result then
    values := list as IReadOnlyCollection<TValue>;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.GetCurrent: TGenericPair;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(fDictionaryEnumerator, 'dictionaryEnumerator');
  Guard.CheckNotNull(fCollectionEnumerator, 'collectionEnumerator');
{$ENDIF}

  Result.Key := fDictionaryEnumerator.Current.Key;
  Result.Value := fCollectionEnumerator.Current;
end;

function TMultiMapBase<TKey, TValue>.TEnumerator.MoveNext: Boolean;
begin
  if not Assigned(fDictionaryEnumerator) then
    fDictionaryEnumerator := fSource.fDictionary.GetEnumerator;

  repeat
    if Assigned(fCollectionEnumerator) and fCollectionEnumerator.MoveNext then
      Exit(True)
    else
    begin
      Result := fDictionaryEnumerator.MoveNext;
      if Result then
        fCollectionEnumerator := fDictionaryEnumerator.Current.Value.GetEnumerator;
    end;
  until not Result;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TValueEnumerator'}

constructor TMultiMapBase<TKey, TValue>.TValueEnumerator.Create(
  const source: TMultiMapBase<TKey, TValue>);
begin
  inherited Create;
  fSource := source;
end;

function TMultiMapBase<TKey, TValue>.TValueEnumerator.GetCurrent: TValue;
begin
  Result := fSourceEnumerator.Current.Value;
end;

function TMultiMapBase<TKey, TValue>.TValueEnumerator.MoveNext: Boolean;
begin
  if not Assigned(fSourceEnumerator) then
    fSourceEnumerator := fSource.GetEnumerator;
  Result := fSourceEnumerator.MoveNext;
end;

{$ENDREGION}


{$REGION 'TMultiMapBase<TKey, TValue>.TValueCollection'}

constructor TMultiMapBase<TKey, TValue>.TValueCollection.Create(
  const owner: TMultiMapBase<TKey, TValue>);
begin
  inherited Create(owner);
  fOwner := owner;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.Contains(const value: TValue;
  const comparer: IEqualityComparer<TValue>): Boolean;
begin
  Result := fOwner.ContainsValue(value);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fOwner.Count;
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TValueEnumerator.Create(fOwner);
end;

function TMultiMapBase<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  list: ICollection<TValue>;
  i: Integer;
begin
  SetLength(Result, fOwner.Count);
  i := 0;
  for list in fOwner.fDictionary.Values do
  begin
    list.CopyTo(Result, 0);
    Inc(i, list.Count);
  end;
end;

{$ENDREGION}


{$REGION 'TMultiMap<TKey, TValue>'}

function TMultiMap<TKey, TValue>.CreateCollection: ICollection<TValue>;
var
  list: IList<TValue>;
begin
  list := TList<TValue>.Create;
  list.OnChanged.Add(DoValueChanged);
  Result := list;
end;

function TMultiMap<TKey, TValue>.CreateDictionary: TDictionary<TKey, ICollection<TValue>>;
begin
  Result := TContainedDictionary<TKey, ICollection<TValue>>.Create(Self);
  Result.OnKeyChanged.Add(DoKeyChanged);
end;

procedure TMultiMap<TKey, TValue>.DoKeyChanged(Sender: TObject;
  const Item: TKey; Action: TCollectionChangedAction);
begin
  KeyChanged(Item, Action);
end;

procedure TMultiMap<TKey, TValue>.DoValueChanged(Sender: TObject;
  const Item: TValue; Action: TCollectionChangedAction);
begin
  ValueChanged(Item, Action);
end;

{$ENDREGION}


{$REGION 'TObjectMultiMap<TKey, TValue>'}

constructor TObjectMultiMap<TKey, TValue>.Create(
  ownerships: TDictionaryOwnerships);
begin
  if (doOwnsKeys in ownerships) and (KeyType.Kind <> tkClass) then
    raise ENotSupportedException.CreateResFmt(@SNotClassType, [KeyType.TypeName]);
  if (doOwnsValues in ownerships) and (ValueType.Kind <> tkClass) then
    raise ENotSupportedException.CreateResFmt(@SNotClassType, [ValueType.TypeName]);
  inherited Create;
  fOwnerships := ownerships;
end;

procedure TObjectMultiMap<TKey, TValue>.KeyChanged(const item: TKey;
  action: TCollectionChangedAction);
begin
  inherited;
  if (action = caRemoved) and (doOwnsKeys in fOwnerships) then
{$IFNDEF AUTOREFCOUNT}
    PObject(@item).Free;
{$ELSE}
    PObject(@item).DisposeOf;
{$ENDIF}
end;

procedure TObjectMultiMap<TKey, TValue>.ValueChanged(const item: TValue;
  action: TCollectionChangedAction);
begin
  inherited;
  if (action = caRemoved) and (doOwnsValues in fOwnerships) then
{$IFNDEF AUTOREFCOUNT}
    PObject(@item).Free;
{$ELSE}
    PObject(@item).DisposeOf;
{$ENDIF}
end;

{$ENDREGION}


end.
