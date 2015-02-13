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

unit Spring.Collections.Dictionaries;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  ///	<summary>
  ///	  Represents a collection of keys and values.
  ///	</summary>
  ///	<typeparam name="TKey">
  ///	  The type of the keys in the dictionary.
  ///	</typeparam>
  ///	<typeparam name="TValue">
  ///	  The type of the values in the dictionary.
  ///	</typeparam>
  TDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>,
    IDictionary<TKey, TValue>, IReadOnlyDictionary<TKey, TValue>)
  protected
    type
      TGenericDictionary = Generics.Collections.TDictionary<TKey, TValue>;
      TGenericPair = Generics.Collections.TPair<TKey, TValue>;

      TKeyCollection = class(TContainedReadOnlyCollection<TKey>)
      private
        fDictionary: TGenericDictionary;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const controller: IInterface;
          const dictionary: TGenericDictionary);

      {$REGION 'Implements IEnumerable<TKey>'}
        function GetEnumerator: IEnumerator<TKey>; override;
        function Contains(const value: TKey;
          const comparer: IEqualityComparer<TKey>): Boolean; override;
        function ToArray: TArray<TKey>; override;
      {$ENDREGION}
      end;

      TValueCollection = class(TContainedReadOnlyCollection<TValue>)
      private
        fDictionary: TGenericDictionary;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const controller: IInterface;
          const dictionary: TGenericDictionary);

      {$REGION 'Implements IEnumerable<TValue>'}
        function GetEnumerator: IEnumerator<TValue>; override;
        function Contains(const value: TValue;
          const comparer: IEqualityComparer<TValue>): Boolean; override;
        function ToArray: TArray<TValue>; override;
      {$ENDREGION}
      end;

      TOrderedEnumerable = class(TContainedIterator<TGenericPair>)
      private
        fDictionary: TGenericDictionary;
        fComparer: IComparer<TKey>;
        fSortedKeys: TArray<TKey>;
        fIndex: Integer;
      protected
      {$REGION 'Property Accessors'}
        function GetCount: Integer; override;
      {$ENDREGION}
      public
        constructor Create(const controller: IInterface;
          const dictionary: TGenericDictionary;
          const comparer: IComparer<TKey>);
        function Clone: TIterator<TGenericPair>; override;
        function MoveNext: Boolean; override;
      end;

{$IFDEF DELPHI2010}
      TKeyCollectionHelper = class(TGenericDictionary.TKeyCollection)
      public
        function ToArray: TArray<TKey>;
      end;
{$ENDIF}
  private
    fDictionary: TGenericDictionary;
    fOwnership: TOwnershipType;
    fKeys: TKeyCollection;
    fValues: TValueCollection;
    fOnKeyNotify: TCollectionNotifyEvent<TKey>;
    fOnValueNotify: TCollectionNotifyEvent<TValue>;
    procedure DoKeyNotify(Sender: TObject; const Item: TKey;
      Action: TCollectionNotification);
    procedure DoValueNotify(Sender: TObject; const Item: TValue;
      Action: TCollectionNotification);
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue; virtual;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue); virtual;
  {$ENDREGION}
  public
    constructor Create; overload; override;
    constructor Create(capacity: Integer); overload;
    constructor Create(const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(capacity: Integer;
      const comparer: IEqualityComparer<TKey>); overload;
    constructor Create(dictionary: TGenericDictionary;
      ownership: TOwnershipType); overload;

    destructor Destroy; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
    function GetEnumerator: IEnumerator<TGenericPair>; override;
    function Contains(const value: TGenericPair;
      const comparer: IEqualityComparer<TGenericPair>): Boolean; override;
    function Ordered: IEnumerable<TGenericPair>; override;
    function ToArray: TArray<TGenericPair>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); reintroduce; overload;
    function Remove(const key: TKey): Boolean; reintroduce; overload;
    function RemovePair(const key: TKey; const value: TValue): Boolean; override;
    function ExtractPair(const key: TKey; const value: TValue): TGenericPair; overload; override;
    function ContainsPair(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; reintroduce; overload;
    function ExtractPair(const key: TKey): TGenericPair; reintroduce; overload;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;

    property Items[const key: TKey]: TValue read GetItem write SetItem; default;
  {$ENDREGION}
  end;

  TContainedDictionary<TKey, TValue> = class(TDictionary<TKey, TValue>)
  private
    fController: Pointer;
    function GetController: IInterface;
  protected
  {$REGION 'Implements IInterface'}
    function _AddRef: Integer; override;
    function _Release: Integer; override;
  {$ENDREGION}
  public
    constructor Create(const controller: IInterface);
    property Controller: IInterface read GetController;
  end;

  TBidiDictionary<TKey, TValue> = class(TMapBase<TKey, TValue>,
    IReadOnlyDictionary<TKey, TValue>, IDictionary<TKey, TValue>,
    IBidiDictionary<TKey, TValue>)
  private
    type
      TGenericPair = Generics.Collections.TPair<TKey, TValue>;
  private
    fValuesByKey: IDictionary<TKey, TValue>;
    fKeysByValue: IDictionary<TValue, TKey>;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetItem(const key: TKey): TValue; inline;
    function GetKey(const value: TValue): TKey;
    function GetKeys: IReadOnlyCollection<TKey>; override;
    function GetValue(const key: TKey): TValue;
    function GetValues: IReadOnlyCollection<TValue>; override;
    procedure SetItem(const key: TKey; const value: TValue); inline;
    procedure SetKey(const value: TValue; const key: TKey);
    procedure SetValue(const key: TKey; const value: TValue);
  {$ENDREGION}
  public
    constructor Create; overload; override;

  {$REGION 'Implements IEnumerable<TPair<TKey, TValue>>'}
//    function Contains(const value: TGenericPair;
//      const comparer: IEqualityComparer<TGenericPair>): Boolean; override;
    function GetEnumerator: IEnumerator<TGenericPair>; override;
  {$ENDREGION}

  {$REGION 'Implements ICollection<TPair<TKey, TValue>>'}
    procedure Clear; override;
  {$ENDREGION}

  {$REGION 'Implements IMap<TKey, TValue>'}
    procedure Add(const key: TKey; const value: TValue); reintroduce; overload;
    function Remove(const key: TKey): Boolean; reintroduce; overload;
    function RemovePair(const key: TKey; const value: TValue): Boolean; override;
    function ExtractPair(const key: TKey; const value: TValue): TGenericPair; overload; override;
    function ContainsPair(const key: TKey; const value: TValue): Boolean; override;
    function ContainsKey(const key: TKey): Boolean; override;
    function ContainsValue(const value: TValue): Boolean; override;
    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property Values: IReadOnlyCollection<TValue> read GetValues;
  {$ENDREGION}

  {$REGION 'Implements IDictionary<TKey, TValue>'}
    procedure AddOrSetValue(const key: TKey; const value: TValue);
    function Extract(const key: TKey): TValue; reintroduce; overload; inline;
    function ExtractPair(const key: TKey): TGenericPair; reintroduce; overload;
    function AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
  {$ENDREGION}

  {$REGION 'Implements IBidiDictionary<TKey, TValue>'}
    function ExtractKey(const value: TValue): TKey;
    function ExtractValue(const key: TKey): TValue;
    function RemoveKey(const key: TKey): Boolean;
    function RemoveValue(const value: TValue): Boolean;
    function TryGetKey(const value: TValue; out key: TKey): Boolean;
    function TryGetValue(const key: TKey; out value: TValue): Boolean;
  {$ENDREGION}
  end;

implementation

uses
  Spring.Collections.Extensions;


{$REGION 'TDictionary<TKey, TValue>'}

constructor TDictionary<TKey, TValue>.Create(dictionary: TGenericDictionary;
  ownership: TOwnershipType);
begin
  inherited Create;
  fDictionary := dictionary;
  fKeys := TKeyCollection.Create(Self, fDictionary);
  fValues := TValueCollection.Create(Self, fDictionary);
  fOwnership := ownership;
  fOnKeyNotify := fDictionary.OnKeyNotify;
  fOnValueNotify := fDictionary.OnValueNotify;
  fDictionary.OnKeyNotify := DoKeyNotify;
  fDictionary.OnValueNotify := DoValueNotify;
end;

constructor TDictionary<TKey, TValue>.Create;
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create;
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(capacity: Integer);
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create(capacity);
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(
  const comparer: IEqualityComparer<TKey>);
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create(comparer);
  Create(dictionary, otOwned);
end;

constructor TDictionary<TKey, TValue>.Create(capacity: Integer;
  const comparer: IEqualityComparer<TKey>);
var
  dictionary: TGenericDictionary;
begin
  dictionary := TGenericDictionary.Create(capacity, comparer);
  Create(dictionary, otOwned);
end;

destructor TDictionary<TKey, TValue>.Destroy;
begin
  fKeys.Free;
  fValues.Free;
  if fOwnership = otOwned then
    fDictionary.Free
  else
  begin
    fDictionary.OnKeyNotify := fOnKeyNotify;
    fDictionary.OnValueNotify := fOnValueNotify;
  end;

  inherited Destroy;
end;

procedure TDictionary<TKey, TValue>.DoKeyNotify(Sender: TObject;
  const Item: TKey; Action: TCollectionNotification);
begin
  inherited KeyChanged(Item, TCollectionChangedAction(action));
end;

procedure TDictionary<TKey, TValue>.DoValueNotify(Sender: TObject;
  const Item: TValue; Action: TCollectionNotification);
begin
  inherited ValueChanged(Item, TCollectionChangedAction(Action));
end;

function TDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TGenericPair>;
var
  dictionary: TEnumerable<TGenericPair>;
begin
  dictionary := TEnumerable<TGenericPair>(fDictionary);
  Result := TEnumeratorAdapter<TGenericPair>.Create(dictionary);
end;

procedure TDictionary<TKey, TValue>.Clear;
begin
  fDictionary.Clear;
end;

function TDictionary<TKey, TValue>.Contains(const value: TGenericPair;
  const comparer: IEqualityComparer<TGenericPair>): Boolean;
var
  item: TValue;
begin
  Result := fDictionary.TryGetValue(value.Key, item);
  if Result then
    Result := comparer.Equals(TGenericPair.Create(value.Key, item), value);
end;

function TDictionary<TKey, TValue>.ExtractPair(const key: TKey;
  const value: TValue): TGenericPair;
var
  found: Boolean;
  foundValue: TValue;
  comparer: IEqualityComparer<TValue>;
begin
  found := fDictionary.TryGetValue(key, foundValue);
  if found then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    found := comparer.Equals(foundValue, value);
    if found then
{$IFDEF DELPHIXE2_UP}
      Result := fDictionary.ExtractPair(key);
{$ELSE}
    begin
      Result.Key := key;
      Result.Value := value;
      fDictionary.ExtractPair(key);
    end;
{$ENDIF}
  end;
  if not found then
  begin
    Result.Key := Default(TKey);
    Result.Value := Default(TValue);
  end;
end;

function TDictionary<TKey, TValue>.ToArray: TArray<TGenericPair>;
{$IFDEF DELPHI2010}
var
  pair: TGenericPair;
  index: Integer;
begin
  SetLength(Result, fDictionary.Count);
  index := 0;
  for pair in fDictionary do
  begin
    Result[index] := pair;
    Inc(index);
  end;
{$ELSE}
begin
  Result := fDictionary.ToArray;
{$ENDIF}
end;

function TDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

procedure TDictionary<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
begin
  fDictionary.Add(key, value);
end;

procedure TDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

function TDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

function TDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
end;

function TDictionary<TKey, TValue>.ContainsPair(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := fDictionary.TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(item, value);
end;

function TDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := fDictionary.ContainsValue(value);
end;

function TDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  Result := ExtractPair(key).Value;
end;

function TDictionary<TKey, TValue>.ExtractPair(
  const key: TKey): TGenericPair;
begin
{$IFDEF DELPHIXE2_UP}
  Result := fDictionary.ExtractPair(key);
{$ELSE}
  if fDictionary.TryGetValue(key, Result.Value) then
  begin
    Result.Key := key;
    fDictionary.ExtractPair(key);
  end
  else
    Result := fDictionary.ExtractPair(key);
{$ENDIF}
end;

function TDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fDictionary.TryGetValue(key, value);
end;

function TDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
begin
  Result := fDictionary.ContainsKey(key);
  if Result then
    fDictionary.Remove(key);
end;

function TDictionary<TKey, TValue>.RemovePair(const key: TKey;
  const value: TValue): Boolean;
var
  comparer: IEqualityComparer<TValue>;
begin
  Result := fDictionary.ContainsKey(key);
  if Result then
  begin
    comparer := TEqualityComparer<TValue>.Default;
    Result := comparer.Equals(fDictionary[key], value);
    if Result then
      fDictionary.Remove(key);
  end;
end;

function TDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fKeys;
end;

function TDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fValues;
end;

function TDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  Result := fDictionary[key];
end;

function TDictionary<TKey, TValue>.Ordered: IEnumerable<TGenericPair>;
begin
  Result := TOrderedEnumerable.Create(Self, fDictionary, TComparer<TKey>.Default());
end;

procedure TDictionary<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  fDictionary.AddOrSetValue(key, value);
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TKeyCollection'}

constructor TDictionary<TKey, TValue>.TKeyCollection.Create(
  const controller: IInterface; const dictionary: TGenericDictionary);
begin
  inherited Create(controller);
  fDictionary := dictionary;
end;

function TDictionary<TKey, TValue>.TKeyCollection.Contains(const value: TKey;
  const comparer: IEqualityComparer<TKey>): Boolean;
begin
  Result := fDictionary.ContainsKey(value);
end;

function TDictionary<TKey, TValue>.TKeyCollection.ToArray: TArray<TKey>;
var
  key: TKey;
  index: Integer;
begin
  index := 0;
  SetLength(Result, fDictionary.Count);
  for key in fDictionary.Keys do
  begin
    Result[index] := key;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetEnumerator: IEnumerator<TKey>;
begin
  Result := TEnumeratorAdapter<TKey>.Create(fDictionary.Keys);
end;

function TDictionary<TKey, TValue>.TKeyCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TValueCollection'}

constructor TDictionary<TKey, TValue>.TValueCollection.Create(
  const controller: IInterface; const dictionary: TGenericDictionary);
begin
  inherited Create(controller);
  fDictionary := dictionary;
end;

function TDictionary<TKey, TValue>.TValueCollection.Contains(const value: TValue;
  const comparer: IEqualityComparer<TValue>): Boolean;
begin
  Result := fDictionary.ContainsValue(value);
end;

function TDictionary<TKey, TValue>.TValueCollection.ToArray: TArray<TValue>;
var
  value: TValue;
  index: Integer;
begin
  index := 0;
  SetLength(Result, fDictionary.Count);
  for value in fDictionary.Values do
  begin
    Result[index] := value;
    Inc(index);
  end;
end;

function TDictionary<TKey, TValue>.TValueCollection.GetEnumerator: IEnumerator<TValue>;
begin
  Result := TEnumeratorAdapter<TValue>.Create(fDictionary.Values);
end;

function TDictionary<TKey, TValue>.TValueCollection.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

{$ENDREGION}


{$REGION 'TContainedDictionary<TKey, TValue>'}

constructor TContainedDictionary<TKey, TValue>.Create(
  const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedDictionary<TKey, TValue>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedDictionary<TKey, TValue>._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TContainedDictionary<TKey, TValue>._Release: Integer;
begin
  Result := IInterface(FController)._Release;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TOrderedEnumerable'}

constructor TDictionary<TKey, TValue>.TOrderedEnumerable.Create(
  const controller: IInterface; const dictionary: TGenericDictionary;
  const comparer: IComparer<TKey>);
begin
  inherited Create(controller);
  fDictionary := dictionary;
  fComparer := comparer;
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.Clone: TIterator<TGenericPair>;
begin
  Result := TOrderedEnumerable.Create(Controller, fDictionary, fComparer);
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.GetCount: Integer;
begin
  Result := fDictionary.Count;
end;

function TDictionary<TKey, TValue>.TOrderedEnumerable.MoveNext: Boolean;
begin
  Result := False;

  if fState = STATE_ENUMERATOR then
  begin
    fIndex := -1;
{$IFDEF DELPHI2010}
    fSortedKeys := TKeyCollectionHelper(fDictionary.Keys).ToArray;
{$ELSE}
    fSortedKeys := fDictionary.Keys.ToArray;
{$ENDIF}
    TArray.Sort<TKey>(fSortedKeys, fComparer);
    fState := STATE_RUNNING;
  end;

  if fState = STATE_RUNNING then
  begin
    if fIndex < High(fSortedKeys) then
    begin
      Inc(fIndex);
      fCurrent.Key := fSortedKeys[fIndex];
      fCurrent.Value := fDictionary[fSortedKeys[fIndex]];
      Exit(True);
    end;
    fState := STATE_FINISHED;
    fSortedKeys := nil;
  end;
end;

{$ENDREGION}


{$REGION 'TDictionary<TKey, TValue>.TKeyCollectionHelper'}

{$IFDEF DELPHI2010}
function TDictionary<TKey, TValue>.TKeyCollectionHelper.ToArray: TArray<TKey>;
var
  item: TKey;
  i: Integer;
begin
  SetLength(Result, Count);
  i := 0;
  for item in Self do
  begin
    Result[i] := item;
    Inc(i);
  end;
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TBidiDictionary<TKey, TValue>'}

constructor TBidiDictionary<TKey, TValue>.Create;
begin
  inherited Create;
  fKeysByValue := TDictionary<TValue, TKey>.Create;
  fValuesByKey := TDictionary<TKey, TValue>.Create;
end;

procedure TBidiDictionary<TKey, TValue>.Add(const key: TKey;
  const value: TValue);
begin
  if fValuesByKey.ContainsKey(key) then
    raise EInvalidOperationException.Create('key');
  if fKeysByValue.ContainsKey(value) then
    raise EInvalidOperationException.Create('value');
  fValuesByKey.Add(key, value);
  fKeysByValue.Add(value, key);
end;

procedure TBidiDictionary<TKey, TValue>.AddOrSetValue(const key: TKey;
  const value: TValue);
var
  oldValue: TValue;
begin
  RemoveValue(value);
  if fValuesByKey.TryGetValue(key, oldValue) then
    fKeysByValue.Remove(oldValue);
  fKeysByValue.Add(value, key);
  fValuesByKey[key] := value;
end;

function TBidiDictionary<TKey, TValue>.AsReadOnlyDictionary: IReadOnlyDictionary<TKey, TValue>;
begin
  Result := Self;
end;

procedure TBidiDictionary<TKey, TValue>.Clear;
begin
  fValuesByKey.Clear;
  fKeysByValue.Clear;
end;

function TBidiDictionary<TKey, TValue>.ContainsKey(const key: TKey): Boolean;
begin
  Result := fValuesByKey.ContainsKey(key);
end;

function TBidiDictionary<TKey, TValue>.ContainsPair(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  Result := fValuesByKey.TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(value, item);
end;

function TBidiDictionary<TKey, TValue>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := fKeysByValue.ContainsKey(value);
end;

function TBidiDictionary<TKey, TValue>.Extract(const key: TKey): TValue;
begin
  Result := ExtractValue(key);
end;

function TBidiDictionary<TKey, TValue>.ExtractKey(const value: TValue): TKey;
begin
  if fKeysByValue.TryGetValue(value, Result) then
  begin
    fKeysByValue.Extract(value);
    fValuesByKey.Extract(Result);
  end
  else
    Result := Default(TKey);
end;

function TBidiDictionary<TKey, TValue>.ExtractPair(
  const key: TKey): TGenericPair;
begin
  raise ENotImplementedException.Create('ExtractPair');
end;

function TBidiDictionary<TKey, TValue>.ExtractPair(const key: TKey;
  const value: TValue): TGenericPair;
begin
  Result := fValuesByKey.ExtractPair(key, value);
  fKeysByValue.ExtractPair(value, key);
end;

function TBidiDictionary<TKey, TValue>.ExtractValue(const key: TKey): TValue;
begin
  if fValuesByKey.TryGetValue(key, Result) then
  begin
    fKeysByValue.Extract(Result);
    fValuesByKey.Extract(key);
  end
  else
    Result := Default(TValue);
end;

function TBidiDictionary<TKey, TValue>.GetCount: Integer;
begin
  Result := fValuesByKey.Count;
end;

function TBidiDictionary<TKey, TValue>.GetEnumerator: IEnumerator<TGenericPair>;
begin
  Result := fValuesByKey.GetEnumerator();
end;

function TBidiDictionary<TKey, TValue>.GetItem(const key: TKey): TValue;
begin
  Result := GetValue(key);
end;

function TBidiDictionary<TKey, TValue>.GetKey(const value: TValue): TKey;
begin
  Result := fKeysByValue[value];
end;

function TBidiDictionary<TKey, TValue>.GetKeys: IReadOnlyCollection<TKey>;
begin
  Result := fValuesByKey.Keys;
end;

function TBidiDictionary<TKey, TValue>.GetValue(const key: TKey): TValue;
begin
  Result := fValuesByKey[key];
end;

function TBidiDictionary<TKey, TValue>.GetValues: IReadOnlyCollection<TValue>;
begin
  Result := fKeysByValue.Keys;
end;

function TBidiDictionary<TKey, TValue>.Remove(const key: TKey): Boolean;
begin
  Result := RemoveKey(key);
end;

function TBidiDictionary<TKey, TValue>.RemovePair(const key: TKey;
  const value: TValue): Boolean;
var
  item: TValue;
begin
  if fValuesByKey.TryGetValue(key, item)
    and TEqualityComparer<TValue>.Default.Equals(value, item) then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);
  end;
end;

function TBidiDictionary<TKey, TValue>.RemoveKey(const key: TKey): Boolean;
var
  value: TValue;
begin
  Result := fValuesByKey.TryGetValue(key, value);
  if Result then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);

    // notify
  end;
end;

function TBidiDictionary<TKey, TValue>.RemoveValue(const value: TValue): Boolean;
var
  key: TKey;
begin
  Result := fKeysByValue.TryGetValue(value, key);
  if Result then
  begin
    fValuesByKey.Remove(key);
    fKeysByValue.Remove(value);

    // notify
  end;
end;

procedure TBidiDictionary<TKey, TValue>.SetItem(const key: TKey;
  const value: TValue);
begin
  SetValue(key, value);
end;

procedure TBidiDictionary<TKey, TValue>.SetKey(const value: TValue;
  const key: TKey);
var
  oldKey: TKey;
begin
  if fValuesByKey.ContainsKey(key) then
    raise EInvalidOperationException.Create('key');
  if fKeysByValue.TryGetValue(value, oldKey) then
    fValuesByKey.Remove(oldKey);
  fValuesByKey.Add(key, value);
  fKeysByValue[value] := key;
end;

procedure TBidiDictionary<TKey, TValue>.SetValue(const key: TKey;
  const value: TValue);
var
  oldValue: TValue;
begin
  if fKeysByValue.ContainsKey(value) then
    raise EInvalidOperationException.Create('value');
  if fValuesByKey.TryGetValue(key, oldValue) then
    fKeysByValue.Remove(oldValue);
  fKeysByValue.Add(value, key);
  fValuesByKey[key] := value;
end;

function TBidiDictionary<TKey, TValue>.TryGetKey(const value: TValue;
  out key: TKey): Boolean;
begin
  Result := fKeysByValue.TryGetValue(value, key);
end;

function TBidiDictionary<TKey, TValue>.TryGetValue(const key: TKey;
  out value: TValue): Boolean;
begin
  Result := fValuesByKey.TryGetValue(key, value);
end;

{$ENDREGION}


end.
