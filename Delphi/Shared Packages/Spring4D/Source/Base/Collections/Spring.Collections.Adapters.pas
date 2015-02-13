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

unit Spring.Collections.Adapters;

interface

uses
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type
  TCollectionAdapter<T> = class(TEnumerableBase<T>, ICollection)
  private
    fSource: ICollection<T>;

    function GetIsReadOnly: Boolean;
    function GetOnChanged: IEvent;

    procedure Add(const item: TValue);
    procedure AddRange(const values: array of TValue); overload;
    procedure AddRange(const collection: IEnumerable); overload;

    function Remove(const item: TValue): Boolean;
    procedure RemoveRange(const values: array of TValue); overload;
    procedure RemoveRange(const collection: IEnumerable); overload;

    function Extract(const item: TValue): TValue; overload;
    procedure ExtractRange(const values: array of TValue); overload;
    procedure ExtractRange(const collection: IEnumerable); overload;

    procedure Clear;
  protected
    function GetCount: Integer; override;
    function GetElementType: PTypeInfo; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: ICollection<T>);
    function GetEnumerator: IEnumerator<T>; override;
  end;

  TListAdapter<T> = class(TCollectionAdapter<T>, IList, IReadOnlyList)
  private
    fSource: IList<T>;

    function GetCapacity: Integer;
    function GetItem(index: Integer): TValue;
    procedure SetCapacity(value: Integer);
    procedure SetItem(index: Integer; const item: TValue);

    function Add(const item: TValue): Integer;

    procedure Insert(index: Integer; const item: TValue);
    procedure InsertRange(index: Integer; const values: array of TValue); overload;
    procedure InsertRange(index: Integer; const collection: IEnumerable); overload;

    procedure Delete(index: Integer);
    procedure DeleteRange(index, count: Integer);

    procedure Exchange(index1, index2: Integer);
    procedure Move(currentIndex, newIndex: Integer);

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload;

    procedure Sort;

    function IndexOf(const item: TValue): Integer; overload;
    function IndexOf(const item: TValue; index: Integer): Integer; overload;
    function IndexOf(const item: TValue; index, count: Integer): Integer; overload;

    function LastIndexOf(const item: TValue): Integer; overload;
    function LastIndexOf(const item: TValue; index: Integer): Integer; overload;
    function LastIndexOf(const item: TValue; index, count: Integer): Integer; overload;

    function AsReadOnlyList: IReadOnlyList;
    procedure TrimExcess;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IList<T>);
  end;

  TDictionaryAdapter<TKey, T> = class(TCollectionAdapter<TPair<TKey, T>>,
    IDictionary, IReadOnlyDictionary)
  private
    fSource: IDictionary<TKey, T>;

    function GetKeyType: PTypeInfo;
    function GetOnKeyChanged: IEvent;
    function GetOnValueChanged: IEvent;
    function GetValueType: PTypeInfo;

    procedure Add(const key, value: TValue);
    procedure AddOrSetValue(const key, value: TValue);

    function ExtractPair(const key: TValue): TPair<TValue, TValue>;
    procedure Remove(const key: TValue); overload;

    function ContainsKey(const key: TValue): Boolean;
    function ContainsValue(const value: TValue): Boolean;

    function TryGetValue(const key: TValue; out value: TValue): Boolean;

    function AsReadOnlyDictionary: IReadOnlyDictionary;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IDictionary<TKey, T>);
  end;

  TStackAdapter<T> = class(TEnumerableBase<T>, IStack)
  private
    fSource: IStack<T>;

    function GetOnChanged: IEvent;

    procedure Clear;
    procedure Push(const item: TValue);
    function Pop: TValue;
    function Peek: TValue;
    function PeekOrDefault: TValue;
    function TryPeek(out item: TValue): Boolean;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IStack<T>);
  end;

  TQueueAdapter<T> = class(TEnumerableBase<T>, IQueue)
  private
    fSource: IQueue<T>;

    function GetOnChanged: IEvent;

    procedure Clear;
    procedure Enqueue(const item: TValue);
    function Dequeue: TValue;
    function Peek: TValue;
    function PeekOrDefault: TValue;
    function TryPeek(out item: TValue): Boolean;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: IQueue<T>);
  end;

  THashSetAdapter<T> = class(TCollectionAdapter<T>, ISet)
  private
    fSource: ISet<T>;

    function Add(const item: TValue): Boolean;
    procedure ExceptWith(const other: IEnumerable);
    procedure IntersectWith(const other: IEnumerable);
    procedure UnionWith(const other: IEnumerable);
    function IsSubsetOf(const other: IEnumerable): Boolean;
    function IsSupersetOf(const other: IEnumerable): Boolean;
    function Overlaps(const other: IEnumerable): Boolean;
    function SetEquals(const other: IEnumerable): Boolean;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    constructor Create(const source: ISet<T>);
  end;

  TAdapters = class
  public
    class function CreateCollection<T>(const source: ICollection<T>): ICollection;
    class function CreateList<T>(const source: IList<T>): IList;
    class function CreateDictionary<TKey, TValue>(const source: IDictionary<TKey, TValue>): IDictionary;
    class function CreateStack<T>(const source: IStack<T>): IStack;
    class function CreateQueue<T>(const source: IQueue<T>): IQueue;
    class function CreateSet<T>(const source: ISet<T>): ISet;
  end;

implementation

uses
  SysUtils;


{$REGION 'TCollectionAdapter<T>'}

constructor TCollectionAdapter<T>.Create(const source: ICollection<T>);
begin
  inherited Create;
  fSource := source;
end;

procedure TCollectionAdapter<T>.Add(const item: TValue);
begin
  fSource.Add(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.AddRange(const values: array of TValue);
var
  item: TValue;
begin
  for item in values do
    fSource.Add(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.AddRange(const collection: IEnumerable);
var
  item: TValue;
begin
  for item in collection do
    fSource.Add(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.Clear;
begin
  fSource.Clear;
end;

function TCollectionAdapter<T>.Extract(const item: TValue): TValue;
begin
  Result := TValue.From<T>(fSource.Extract(item.AsType<T>));
end;

procedure TCollectionAdapter<T>.ExtractRange(const values: array of TValue);
var
  item: TValue;
begin
  for item in values do
    fSource.Extract(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.ExtractRange(const collection: IEnumerable);
var
  item: TValue;
begin
  for item in collection do
    fSource.Extract(item.AsType<T>);
end;

function TCollectionAdapter<T>.GetCount: Integer;
begin
  Result := fSource.Count;
end;

function TCollectionAdapter<T>.GetElementType: PTypeInfo;
begin
  Result := fSource.GetElementType;
end;

function TCollectionAdapter<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fSource.GetEnumerator;
end;

function TCollectionAdapter<T>.GetIsReadOnly: Boolean;
begin
  Result := fSource.IsReadOnly;
end;

function TCollectionAdapter<T>.GetOnChanged: IEvent;
begin
  Result := fSource.OnChanged;
end;

function TCollectionAdapter<T>.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IsEqualGUID(IID, ICollection<T>) then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited;
end;

function TCollectionAdapter<T>.Remove(const item: TValue): Boolean;
begin
  Result := fSource.Remove(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.RemoveRange(const values: array of TValue);
var
  item: TValue;
begin
  for item in values do
    fSource.Remove(item.AsType<T>);
end;

procedure TCollectionAdapter<T>.RemoveRange(
  const collection: IEnumerable);
var
  item: TValue;
begin
  for item in collection do
    fSource.Remove(item.AsType<T>);
end;

{$ENDREGION}


{$REGION 'TListAdapter<T>'}

constructor TListAdapter<T>.Create(const source: IList<T>);
begin
  inherited Create(source);
  fSource := source;
end;

function TListAdapter<T>.Add(const item: TValue): Integer;
begin
  Result := fSource.Add(item.AsType<T>);
end;

function TListAdapter<T>.AsReadOnlyList: IReadOnlyList;
begin
  Result := Self;
end;

procedure TListAdapter<T>.Delete(index: Integer);
begin
  fSource.Delete(index);
end;

procedure TListAdapter<T>.DeleteRange(index, count: Integer);
begin
  fSource.DeleteRange(index, count);
end;

procedure TListAdapter<T>.Exchange(index1, index2: Integer);
begin
  fSource.Exchange(index1, index2);
end;

function TListAdapter<T>.GetCapacity: Integer;
begin
  Result := fSource.Capacity;
end;

function TListAdapter<T>.GetItem(index: Integer): TValue;
begin
  Result := TValue.From<T>(fSource[index]);
end;

function TListAdapter<T>.IndexOf(const item: TValue): Integer;
begin
  Result := fSource.IndexOf(item.AsType<T>);
end;

function TListAdapter<T>.IndexOf(const item: TValue; index: Integer): Integer;
begin
  Result := fSource.IndexOf(item.AsType<T>, index);
end;

function TListAdapter<T>.IndexOf(const item: TValue; index,
  count: Integer): Integer;
begin
  Result := fSource.IndexOf(item.AsType<T>, index, count);
end;

procedure TListAdapter<T>.Insert(index: Integer; const item: TValue);
begin
  fSource.Insert(index, item.AsType<T>);
end;

procedure TListAdapter<T>.InsertRange(index: Integer;
  const values: array of TValue);
var
  item: TValue;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  for item in values do
  begin
    fSource.Insert(index, item.AsType<T>);
    Inc(index);
  end;
end;

procedure TListAdapter<T>.InsertRange(index: Integer;
  const collection: IEnumerable);
var
  item: TValue;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  for item in collection do
  begin
    fSource.Insert(index, item.AsType<T>);
    Inc(index);
  end;
end;

function TListAdapter<T>.LastIndexOf(const item: TValue): Integer;
begin
  Result := fSource.LastIndexOf(item.AsType<T>);
end;

function TListAdapter<T>.LastIndexOf(const item: TValue; index: Integer): Integer;
begin
  Result := fSource.LastIndexOf(item.AsType<T>, index);
end;

function TListAdapter<T>.LastIndexOf(const item: TValue; index,
  count: Integer): Integer;
begin
  Result := fSource.LastIndexOf(item.AsType<T>, index, count);
end;

procedure TListAdapter<T>.Move(currentIndex, newIndex: Integer);
begin
  fSource.Move(currentIndex, newIndex);
end;

function TListAdapter<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, IList<T>) then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited;
end;

procedure TListAdapter<T>.Reverse;
begin
  fSource.Reverse;
end;

procedure TListAdapter<T>.Reverse(index, count: Integer);
begin
  fSource.Reverse(index, count);
end;

procedure TListAdapter<T>.SetCapacity(value: Integer);
begin
  fSource.Capacity := value;
end;

procedure TListAdapter<T>.SetItem(index: Integer; const item: TValue);
begin
  fSource[index] := item.AsType<T>;
end;

procedure TListAdapter<T>.Sort;
begin
  fSource.Sort;
end;

procedure TListAdapter<T>.TrimExcess;
begin
  fSource.TrimExcess;
end;

{$ENDREGION}


{$REGION 'TDictionaryAdapter<TKey, T>'}

constructor TDictionaryAdapter<TKey, T>.Create(
  const source: IDictionary<TKey, T>);
begin
  inherited Create(source);
  fSource := source;
end;

function TDictionaryAdapter<TKey, T>.ExtractPair(
  const key: TValue): TPair<TValue, TValue>;
var
  pair: TPair<TKey, T>;
begin
  pair := fSource.ExtractPair(key.AsType<TKey>);
  Result := TPair<TValue, TValue>.Create(TValue.From<TKey>(pair.Key), TValue.From<T>(pair.Value));
end;

procedure TDictionaryAdapter<TKey, T>.Add(const key, value: TValue);
begin
  fSource.Add(key.AsType<TKey>, value.AsType<T>);
end;

procedure TDictionaryAdapter<TKey, T>.AddOrSetValue(const key, value: TValue);
begin
  fSource.AddOrSetValue(key.AsType<TKey>, value.AsType<T>);
end;

function TDictionaryAdapter<TKey, T>.AsReadOnlyDictionary: IReadOnlyDictionary;
begin
  Result := Self;
end;

function TDictionaryAdapter<TKey, T>.ContainsKey(const key: TValue): Boolean;
begin
  Result := fSource.ContainsKey(key.AsType<TKey>);
end;

function TDictionaryAdapter<TKey, T>.ContainsValue(
  const value: TValue): Boolean;
begin
  Result := fSource.ContainsValue(value.AsType<T>);
end;

function TDictionaryAdapter<TKey, T>.GetKeyType: PTypeInfo;
begin
  Result := fSource.KeyType;
end;

function TDictionaryAdapter<TKey, T>.GetOnKeyChanged: IEvent;
begin
  Result := fSource.OnKeyChanged;
end;

function TDictionaryAdapter<TKey, T>.GetOnValueChanged: IEvent;
begin
  Result := fSource.OnValueChanged;
end;

function TDictionaryAdapter<TKey, T>.GetValueType: PTypeInfo;
begin
  Result := fSource.ValueType;
end;

function TDictionaryAdapter<TKey, T>.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if IsEqualGUID(IID, IDictionary<TKey, T>) then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited;
end;

procedure TDictionaryAdapter<TKey, T>.Remove(const key: TValue);
begin
  fSource.Remove(key.AsType<TKey>);
end;

function TDictionaryAdapter<TKey, T>.TryGetValue(const key: TValue;
  out value: TValue): Boolean;
var
  item: T;
begin
  Result := fSource.TryGetValue(key.AsType<TKey>, item);
  if Result then
    value := TValue.From<T>(item);
end;

{$ENDREGION}


{$REGION 'TStackAdapter<T>'}

constructor TStackAdapter<T>.Create(const source: IStack<T>);
begin
  inherited Create;
  fSource := source;
end;

procedure TStackAdapter<T>.Clear;
begin
  fSource.Clear;
end;

function TStackAdapter<T>.GetOnChanged: IEvent;
begin
  Result := fSource.OnChanged;
end;

function TStackAdapter<T>.Peek: TValue;
begin
  Result := TValue.From<T>(fSource.Peek);
end;

function TStackAdapter<T>.PeekOrDefault: TValue;
begin
  Result := TValue.From<T>(fSource.PeekOrDefault);
end;

function TStackAdapter<T>.Pop: TValue;
begin
  Result := TValue.From<T>(fSource.Pop);
end;

procedure TStackAdapter<T>.Push(const item: TValue);
begin
  fSource.Push(item.AsType<T>);
end;

function TStackAdapter<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, IStack<T>) then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited;
end;

function TStackAdapter<T>.TryPeek(out item: TValue): Boolean;
var
  value: T;
begin
  Result := fSource.TryPeek(value);
  if Result then
    item := TValue.From<T>(value);
end;

{$ENDREGION}


{$REGION 'TQueueAdapter<T>'}

constructor TQueueAdapter<T>.Create(const source: IQueue<T>);
begin
  inherited Create;
  fSource := source;
end;

procedure TQueueAdapter<T>.Clear;
begin
  fSource.Clear;
end;

function TQueueAdapter<T>.Dequeue: TValue;
begin
  Result := TValue.From<T>(fSource.Dequeue);
end;

procedure TQueueAdapter<T>.Enqueue(const item: TValue);
begin
  fSource.Enqueue(item.AsType<T>);
end;

function TQueueAdapter<T>.GetOnChanged: IEvent;
begin
  Result := fSource.OnChanged;
end;

function TQueueAdapter<T>.Peek: TValue;
begin
  Result := TValue.From<T>(fSource.Peek);
end;

function TQueueAdapter<T>.PeekOrDefault: TValue;
begin
  Result := TValue.From<T>(fSource.PeekOrDefault);
end;

function TQueueAdapter<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, IQueue<T>) then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited;
end;

function TQueueAdapter<T>.TryPeek(out item: TValue): Boolean;
var
  value: T;
begin
  Result := fSource.TryPeek(value);
  if Result then
    item := TValue.From<T>(value);
end;

{$ENDREGION}


{$REGION 'THashSetAdapter<T>'}

constructor THashSetAdapter<T>.Create(const source: ISet<T>);
begin
  inherited Create(source);
  fSource := source;
end;

function THashSetAdapter<T>.Add(const item: TValue): Boolean;
begin
  Result := fSource.Add(item.AsType<T>);
end;

procedure THashSetAdapter<T>.ExceptWith(const other: IEnumerable);
begin
  fSource.ExceptWith(IEnumerable<T>(other));
end;

procedure THashSetAdapter<T>.IntersectWith(const other: IEnumerable);
begin
  fSource.IntersectWith(IEnumerable<T>(other));
end;

function THashSetAdapter<T>.IsSubsetOf(const other: IEnumerable): Boolean;
begin
  Result := fSource.IsSubsetOf(IEnumerable<T>(other));
end;

function THashSetAdapter<T>.IsSupersetOf(const other: IEnumerable): Boolean;
begin
  Result := fSource.IsSupersetOf(IEnumerable<T>(other));
end;

function THashSetAdapter<T>.Overlaps(const other: IEnumerable): Boolean;
begin
  Result := fSource.Overlaps(IEnumerable<T>(other));
end;

function THashSetAdapter<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, ISet<T>) then
  begin
    IInterface(obj) := fSource;
    Result := 0;
  end
  else
    Result := inherited;
end;

function THashSetAdapter<T>.SetEquals(const other: IEnumerable): Boolean;
begin
  Result := fSource.SetEquals(IEnumerable<T>(other));
end;

procedure THashSetAdapter<T>.UnionWith(const other: IEnumerable);
begin
  fSource.UnionWith(IEnumerable<T>(other));
end;

{$ENDREGION}


{$REGION 'TAdapters'}

class function TAdapters.CreateCollection<T>(
  const source: ICollection<T>): ICollection;
begin
  Result := TCollectionAdapter<T>.Create(source);
end;

class function TAdapters.CreateDictionary<TKey, TValue>(
  const source: IDictionary<TKey, TValue>): IDictionary;
begin
  Result := TDictionaryAdapter<TKey, TValue>.Create(source);
end;

class function TAdapters.CreateList<T>(const source: IList<T>): IList;
begin
  Result := TListAdapter<T>.Create(source);
end;

class function TAdapters.CreateQueue<T>(const source: IQueue<T>): IQueue;
begin
  Result := TQueueAdapter<T>.Create(source);
end;

class function TAdapters.CreateSet<T>(const source: ISet<T>): ISet;
begin
  Result := THashSetAdapter<T>.Create(source);
end;

class function TAdapters.CreateStack<T>(const source: IStack<T>): IStack;
begin
  Result := TStackAdapter<T>.Create(source);
end;

{$ENDREGION}


end.
