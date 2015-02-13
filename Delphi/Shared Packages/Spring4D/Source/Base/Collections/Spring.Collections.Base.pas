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

unit Spring.Collections.Base;

interface

uses
  Generics.Collections,
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections;

type
  ///	<summary>
  ///	  Provides an abstract implementation for the
  ///	  <see cref="Spring.Collections|IEnumerator" /> interface.
  ///	</summary>
  TEnumeratorBase = class abstract(TInterfacedObject, IEnumerator)
  private
    function GetCurrentNonGeneric: TValue; virtual; abstract;
    function IEnumerator.GetCurrent = GetCurrentNonGeneric;
  public
    function MoveNext: Boolean; virtual;
    procedure Reset; virtual;

    property Current: TValue read GetCurrentNonGeneric;
  end;

  ///	<summary>
  ///	  Provides a default implementation for the
  ///	  <see cref="Spring.Collections|IEnumerator&lt;T&gt;" /> interface.
  ///	</summary>
  TEnumeratorBase<T> = class abstract(TEnumeratorBase, IEnumerator<T>)
  private
    function GetCurrentNonGeneric: TValue; override; final;
  protected
    function GetCurrent: T; virtual;
  public
    property Current: T read GetCurrent;
  end;

  ///	<summary>
  ///	  Provides an abstract implementation for the
  ///	  <see cref="Spring.Collections|IEnumerable" /> interface.
  ///	</summary>
  TEnumerableBase = class abstract(TInterfacedObject, IInterface,
    IElementType, ICountable, IEnumerable)
  private
{$IFNDEF AUTOREFCOUNT}{$IFNDEF DELPHIXE7_UP}
    const objDestroyingFlag = Integer($80000000);
    function GetRefCount: Integer; inline;
{$ENDIF}{$ENDIF}
    function GetEnumeratorNonGeneric: IEnumerator; virtual; abstract;
    function IEnumerable.GetEnumerator = GetEnumeratorNonGeneric;
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; virtual;
    function GetElementType: PTypeInfo; virtual; abstract;
  {$ENDREGION}
  protected
  {$REGION 'Implements IInterface'}
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; virtual; stdcall;
    function _Release: Integer; virtual; stdcall;
  {$ENDREGION}
  public
{$IFNDEF AUTOREFCOUNT}{$IFNDEF DELPHIXE7_UP}
    procedure BeforeDestruction; override;
{$ENDIF}{$ENDIF}

    function AsObject: TObject;

    function Any: Boolean;
    function GetEnumerator: IEnumerator;

    property Count: Integer read GetCount;
    property ElementType: PTypeInfo read GetElementType;
{$IFNDEF AUTOREFCOUNT}{$IFNDEF DELPHIXE7_UP}
    property RefCount: Integer read GetRefCount;
{$ENDIF}{$ENDIF}
  end;

  ///	<summary>
  ///	  Provides a default implementation for the
  ///	  <see cref="Spring.Collections|IEnumerable&lt;T&gt;" /> interface.
  ///	</summary>
  TEnumerableBase<T> = class abstract(TEnumerableBase, IEnumerable<T>)
  private
    fComparer: IComparer<T>;
    class var fEqualityComparer: IEqualityComparer<T>;
    function GetEnumeratorNonGeneric: IEnumerator; override; final;
  protected
  {$REGION 'Property Accessors'}
    function GetComparer: IComparer<T>;
    function GetElementType: PTypeInfo; override;
    class function GetEqualityComparer: IEqualityComparer<T>; static;
  {$ENDREGION}
{$IFDEF WEAKREF}
    function HasWeakRef: Boolean; inline;
{$ENDIF}
    function IsManaged: Boolean; inline;
    function TryGetElementAt(out value: T; index: Integer): Boolean; virtual;
    function TryGetFirst(out value: T): Boolean; overload; virtual;
    function TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean; overload;
    function TryGetLast(out value: T): Boolean; overload; virtual;
    function TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean; overload;
    function TryGetSingle(out value: T): Boolean; overload; virtual;
    function TryGetSingle(out value: T; const predicate: TPredicate<T>): Boolean; overload;

    property Comparer: IComparer<T> read GetComparer;
    class property EqualityComparer: IEqualityComparer<T> read GetEqualityComparer;
  public
    constructor Create; overload; virtual;
    constructor Create(const comparer: IComparer<T>); overload;
    constructor Create(const comparer: TComparison<T>); overload;

    class destructor Destroy;

    function GetEnumerator: IEnumerator<T>; virtual;

    function All(const predicate: TPredicate<T>): Boolean;
    function Any(const predicate: TPredicate<T>): Boolean; overload;

    function Concat(const second: IEnumerable<T>): IEnumerable<T>;

    function Contains(const value: T): Boolean; overload; virtual;
    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; overload; virtual;
    function Contains(const value: T; const comparer: TEqualityComparison<T>): Boolean; overload;

    function ElementAt(index: Integer): T;
    function ElementAtOrDefault(index: Integer): T; overload;
    function ElementAtOrDefault(index: Integer; const defaultValue: T): T; overload;

    function EqualsTo(const values: array of T): Boolean; overload;
    function EqualsTo(const collection: IEnumerable<T>): Boolean; overload;
    function EqualsTo(const collection: IEnumerable<T>; const comparer: IEqualityComparer<T>): Boolean; overload;

    function First: T; overload; virtual;
    function First(const predicate: TPredicate<T>): T; overload;
    function FirstOrDefault: T; overload;
    function FirstOrDefault(const defaultValue: T): T; overload; virtual;
    function FirstOrDefault(const predicate: TPredicate<T>): T; overload;
    function FirstOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    procedure ForEach(const action: TAction<T>); overload;

    function Last: T; overload; virtual;
    function Last(const predicate: TPredicate<T>): T; overload;
    function LastOrDefault: T; overload;
    function LastOrDefault(const defaultValue: T): T; overload; virtual;
    function LastOrDefault(const predicate: TPredicate<T>): T; overload;
    function LastOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    function Max: T; overload;
{$IFDEF DELPHIXE_UP}
    function Max(const selector: TFunc<T, Integer>): Integer; overload;
{$ENDIF}
    function Max(const comparer: IComparer<T>): T; overload;
    function Max(const comparer: TComparison<T>): T; overload;
    function Min: T; overload;
{$IFDEF DELPHIXE_UP}
    function Min(const selector: TFunc<T, Integer>): Integer; overload;
{$ENDIF}
    function Min(const comparer: IComparer<T>): T; overload;
    function Min(const comparer: TComparison<T>): T; overload;

    function Ordered: IEnumerable<T>; overload; virtual;
    function Ordered(const comparer: IComparer<T>): IEnumerable<T>; overload; virtual;
    function Ordered(const comparer: TComparison<T>): IEnumerable<T>; overload; virtual;

    function Reversed: IEnumerable<T>; virtual;

    function Single: T; overload; virtual;
    function Single(const predicate: TPredicate<T>): T; overload;
    function SingleOrDefault: T; overload;
    function SingleOrDefault(const defaultValue: T): T; overload; virtual;
    function SingleOrDefault(const predicate: TPredicate<T>): T; overload;
    function SingleOrDefault(const predicate: TPredicate<T>; const defaultValue: T): T; overload;

    function Skip(count: Integer): IEnumerable<T>;
    function SkipWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;
    function SkipWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Take(count: Integer): IEnumerable<T>;
    function TakeWhile(const predicate: TPredicate<T>): IEnumerable<T>; overload;
    function TakeWhile(const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>; overload;

    function Where(const predicate: TPredicate<T>): IEnumerable<T>; virtual;

    function ToArray: TArray<T>; virtual;
  end;

  TIteratorBase<T> = class(TEnumerableBase<T>, IEnumerator)
  protected
    function GetCurrentNonGeneric: TValue; virtual; abstract;
    function IEnumerator.GetCurrent = GetCurrentNonGeneric;
  public
    function MoveNext: Boolean; virtual;
    procedure Reset; virtual;
  end;

  TIterator<T> = class(TIteratorBase<T>, IEnumerator<T>)
  private
    fInitialThreadId: Cardinal;
  protected
    fState: Integer;
    fCurrent: T;
    const
      STATE_INITIAL    = -2; // initial state, before GetEnumerator
      STATE_FINISHED   = -1; // end of enumerator
      STATE_ENUMERATOR = 0;  // before calling MoveNext
      STATE_RUNNING    = 1;  // enumeration is running
  protected
    function Clone: TIterator<T>; virtual; abstract;
    function GetCurrent: T;
    function GetCurrentNonGeneric: TValue; override; final;
  public
    constructor Create; override;
    function GetEnumerator: IEnumerator<T>; override; final;
  end;

  ///	<summary>
  ///	  Provides an abstract implementation for the
  ///	  <see cref="Spring.Collections|ICollection&lt;T&gt;" /> interface.
  ///	</summary>
  ///	<remarks>
  ///	  The Add/Remove/Extract/Clear methods are abstract. IsReadOnly returns
  ///	  <c>False</c> by default.
  ///	</remarks>
  TCollectionBase<T> = class abstract(TEnumerableBase<T>, ICollection<T>, IReadOnlyCollection<T>)
  protected
    fOnChanged: ICollectionChangedEvent<T>;
  {$REGION 'Property Accessors'}
    function GetIsReadOnly: Boolean; virtual;
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}
    procedure AddInternal(const item: T); virtual; abstract;
    procedure Changed(const item: T; action: TCollectionChangedAction); virtual;
  public
    constructor Create; override;
    constructor Create(const values: array of T); overload; virtual;
    constructor Create(const collection: IEnumerable<T>); overload; virtual;

    procedure Add(const item: T);
    procedure AddRange(const values: array of T); overload; virtual;
    procedure AddRange(const collection: IEnumerable<T>); overload; virtual;

    procedure Clear; virtual; abstract;

    function Remove(const item: T): Boolean; virtual; abstract;
    procedure RemoveAll(const predicate: TPredicate<T>); virtual;
    procedure RemoveRange(const values: array of T); overload; virtual;
    procedure RemoveRange(const collection: IEnumerable<T>); overload; virtual;

    function Extract(const item: T): T; virtual; abstract;
    procedure ExtractAll(const predicate: TPredicate<T>); virtual;
    procedure ExtractRange(const values: array of T); overload; virtual;
    procedure ExtractRange(const collection: IEnumerable<T>); overload; virtual;

    procedure CopyTo(var values: TArray<T>; index: Integer); virtual;
    procedure MoveTo(const collection: ICollection<T>); virtual;

    property IsReadOnly: Boolean read GetIsReadOnly;
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

  TContainedIterator<T> = class(TIterator<T>)
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

  TContainedCollectionBase<T> = class(TCollectionBase<T>)
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

  TContainedReadOnlyCollection<T> = class(TEnumerableBase<T>, IReadOnlyCollection<T>)
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

  TMapBase<TKey, T> = class(TCollectionBase<TPair<TKey, T>>, IMap<TKey, T>)
  private
    type
      TGenericPair = Generics.Collections.TPair<TKey, T>;
  private
    fOnKeyChanged: ICollectionChangedEvent<TKey>;
    fOnValueChanged: ICollectionChangedEvent<T>;
    function GetOnKeyChanged: ICollectionChangedEvent<TKey>;
    function GetOnValueChanged: ICollectionChangedEvent<T>;
  protected
  {$REGION 'Property Accessors'}
    function GetKeys: IReadOnlyCollection<TKey>; virtual; abstract;
    function GetKeyType: PTypeInfo; virtual;
    function GetValues: IReadOnlyCollection<T>; virtual; abstract;
    function GetValueType: PTypeInfo; virtual;
  {$ENDREGION}
    procedure AddInternal(const item: TGenericPair); override; final;
    procedure KeyChanged(const Item: TKey; Action: TCollectionChangedAction); virtual;
    procedure ValueChanged(const Item: T; Action: TCollectionChangedAction); virtual;
  public
    constructor Create; override;

    procedure Add(const key: TKey; const value: T); reintroduce; overload; virtual; abstract;

    function Remove(const item: TGenericPair): Boolean; overload; override; final;
    function Remove(const key: TKey): Boolean; reintroduce; overload; virtual; abstract;
    function RemovePair(const key: TKey; const value: T): Boolean; virtual; abstract;

    function Extract(const item: TGenericPair): TGenericPair; override; final;
    function ExtractPair(const key: TKey; const value: T): TGenericPair; virtual; abstract;

    function Contains(const item: TGenericPair): Boolean; override; final;
    function ContainsPair(const key: TKey; const value: T): Boolean; virtual; abstract;
    function ContainsKey(const key: TKey): Boolean; virtual; abstract;
    function ContainsValue(const value: T): Boolean; virtual; abstract;

    property Keys: IReadOnlyCollection<TKey> read GetKeys;
    property KeyType: PTypeInfo read GetKeyType;
    property OnKeyChanged: ICollectionChangedEvent<TKey> read GetOnKeyChanged;
    property OnValueChanged: ICollectionChangedEvent<T> read GetOnValueChanged;
    property Values: IReadOnlyCollection<T> read GetValues;
    property ValueType: PTypeInfo read GetValueType;
  end;

  ///	<summary>
  ///	  Provides an abstract implementation for the
  ///	  <see cref="Spring.Collections|IList&lt;T&gt;" /> interface.
  ///	</summary>
  TListBase<T> = class abstract(TCollectionBase<T>, IList<T>, IReadOnlyList<T>, IList)
  private
    function AsList: IList;
    function AsReadOnlyList: IReadOnlyList<T>;
  {$HINTS OFF}
    property List: IList read AsList implements IList;
  {$HINTS ON}
  protected
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetCapacity: Integer; virtual; abstract;
    function GetItem(index: Integer): T; virtual; abstract;
    procedure SetCount(count: Integer); virtual;
    procedure SetCapacity(value: Integer); virtual; abstract;
    procedure SetItem(index: Integer; const value: T); virtual; abstract;
  {$ENDREGION}
  {$REGION 'Implements IInterface'}
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
  {$ENDREGION}
    procedure AddInternal(const item: T); override;
    function TryGetElementAt(out value: T; index: Integer): Boolean; override;
    function TryGetFirst(out value: T): Boolean; override;
    function TryGetLast(out value: T): Boolean; override;
    function TryGetSingle(out value: T): Boolean; override;
  public
    destructor Destroy; override;

    function Add(const item: T): Integer; reintroduce; virtual;
    procedure AddRange(const values: array of T); override;
    procedure AddRange(const collection: IEnumerable<T>); override;

    function Remove(const item: T): Boolean; override;

    procedure Clear; override;

    function First: T; overload; override;
    function FirstOrDefault(const defaultValue: T): T; overload; override;

    function Last: T; overload; override;
    function LastOrDefault(const defaultValue: T): T; overload; override;

    function Single: T; overload; override;
    function SingleOrDefault(const defaultValue: T): T; overload; override;

    procedure Insert(index: Integer; const item: T); virtual; abstract;
    procedure InsertRange(index: Integer; const values: array of T); overload; virtual;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); overload; virtual;

    procedure Delete(index: Integer); virtual; abstract;
    procedure DeleteRange(index, count: Integer); virtual;

    function GetRange(index, count: Integer): IList<T>; virtual;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload; virtual;

    function LastIndexOf(const item: T): Integer; overload;
    function LastIndexOf(const item: T; index: Integer): Integer; overload;
    function LastIndexOf(const item: T; index, count: Integer): Integer; overload; virtual;

    procedure Exchange(index1, index2: Integer); virtual; abstract;
    procedure Move(currentIndex, newIndex: Integer); virtual; abstract;

    procedure Reverse; overload;
    procedure Reverse(index, count: Integer); overload; virtual; abstract;

    procedure Sort; overload;
    procedure Sort(const comparer: IComparer<T>); overload; virtual; abstract;
    procedure Sort(const comparison: TComparison<T>); overload;

    function ToArray: TArray<T>; override;
    procedure TrimExcess;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Items[index: Integer]: T read GetItem write SetItem; default;
  end;

implementation

uses
  Classes,
  Rtti,
  TypInfo,
  Spring.Collections.Adapters,
  Spring.Collections.Events,
  Spring.Collections.Extensions,
  Spring.Collections.Lists,
  Spring.ResourceStrings;


{$REGION 'TEnumeratorBase'}

function TEnumeratorBase.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TEnumeratorBase.Reset;
begin
  raise ENotSupportedException.CreateRes(@SCannotResetEnumerator);
end;

{$ENDREGION}


{$REGION 'TEnumeratorBase<T>'}

function TEnumeratorBase<T>.GetCurrent: T;
begin
  raise EInvalidOperationException.CreateRes(@SEnumEmpty);
end;

function TEnumeratorBase<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(GetCurrent);
end;

{$ENDREGION}


{$REGION 'TEnumerableBase'}

function TEnumerableBase.Any: Boolean;
var
  enumerator: IEnumerator;
begin
  enumerator := GetEnumerator;
  Result := enumerator.MoveNext;
end;

function TEnumerableBase.AsObject: TObject;
begin
  Result := Self;
end;

{$IFNDEF AUTOREFCOUNT}{$IFNDEF DELPHIXE7_UP}
procedure TEnumerableBase.BeforeDestruction;
begin
  inherited;
  FRefCount := objDestroyingFlag;
end;
{$ENDIF}{$ENDIF}

function TEnumerableBase.GetCount: Integer;
var
  enumerator: IEnumerator;
begin
  Result := 0;
  enumerator := GetEnumerator;
  while enumerator.MoveNext do
    Inc(Result);
end;

function TEnumerableBase.GetEnumerator: IEnumerator;
begin
  Result := GetEnumeratorNonGeneric;
end;

{$IFNDEF AUTOREFCOUNT}{$IFNDEF DELPHIXE7_UP}
function TEnumerableBase.GetRefCount: Integer;
begin
  Result := FRefCount and not objDestroyingFlag;
end;
{$ENDIF}{$ENDIF}

function TEnumerableBase.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := inherited QueryInterface(IID, Obj);
end;

function TEnumerableBase._AddRef: Integer;
begin
  Result := inherited _AddRef;
end;

function TEnumerableBase._Release: Integer;
begin
  Result := inherited _Release;
end;

{$ENDREGION}


{$REGION 'TEnumerableBase<T>'}

constructor TEnumerableBase<T>.Create;
begin
  inherited Create;
  if GetTypeKind(TypeInfo(T)) = tkClass then
    fComparer := TInstanceComparer<T>.Default
  else
    fComparer := TComparer<T>.Default;
end;

constructor TEnumerableBase<T>.Create(const comparer: IComparer<T>);
begin
  Create;
  if Assigned(comparer) then
    fComparer := comparer;
end;

constructor TEnumerableBase<T>.Create(const comparer: TComparison<T>);
begin
  Create(IComparer<T>(PPointer(@comparer)^));
end;

class destructor TEnumerableBase<T>.Destroy;
begin
  fEqualityComparer := nil;
end;

function TEnumerableBase<T>.All(const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := True;
  for item in Self do
    if not predicate(item) then
      Exit(False);
end;

function TEnumerableBase<T>.Any(const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := False;
  for item in Self do
    if predicate(item) then
      Exit(True);
end;

function TEnumerableBase<T>.Concat(
  const second: IEnumerable<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(second), 'second');
{$ENDIF}

  Result := TConcatIterator<T>.Create(Self, second);
end;

function TEnumerableBase<T>.Contains(const value: T): Boolean;
begin
  Result := Contains(value, EqualityComparer);
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull<T>(value, 'value');
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  for item in Self do
    if comparer.Equals(value, item) then
      Exit(True);
  Result := False;
end;

function TEnumerableBase<T>.Contains(const value: T;
  const comparer: TEqualityComparison<T>): Boolean;
begin
  Result := Contains(value, TEqualityComparer<T>.Construct(comparer, nil));
end;

function TEnumerableBase<T>.ElementAt(index: Integer): T;
begin
  if not TryGetElementAt(Result, index) then
    raise EArgumentOutOfRangeException.Create('index');
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer): T;
begin
  if not TryGetElementAt(Result, index) then
    Result := Default(T);
end;

function TEnumerableBase<T>.ElementAtOrDefault(index: Integer;
  const defaultValue: T): T;
begin
  if not TryGetElementAt(Result, index) then
    Result := defaultValue;
end;

function TEnumerableBase<T>.EqualsTo(const values: array of T): Boolean;
var
  collection: IEnumerable<T>;
begin
  collection := TArrayIterator<T>.Create(values);
  Result := EqualsTo(collection);
end;

function TEnumerableBase<T>.EqualsTo(const collection: IEnumerable<T>): Boolean;
begin
  Result := EqualsTo(collection, EqualityComparer);
end;

function TEnumerableBase<T>.EqualsTo(const collection: IEnumerable<T>;
  const comparer: IEqualityComparer<T>): Boolean;
var
  e1, e2: IEnumerator<T>;
  hasNext: Boolean;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  e1 := GetEnumerator;
  e2 := collection.GetEnumerator;

  while True do
  begin
    hasNext := e1.MoveNext;
    if hasNext <> e2.MoveNext then
      Exit(False)
    else if not hasNext then
      Exit(True);
    if hasNext and not comparer.Equals(e1.Current, e2.Current) then
      Exit(False);
  end;
end;

function TEnumerableBase<T>.First: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
    Result := enumerator.Current
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function TEnumerableBase<T>.First(const predicate: TPredicate<T>): T;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  for item in Self do
    if predicate(item) then
      Exit(item);
  raise EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement);
end;

function TEnumerableBase<T>.FirstOrDefault: T;
begin
  Result := FirstOrDefault(Default(T));
end;

function TEnumerableBase<T>.FirstOrDefault(const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
    Result := enumerator.Current
  else
    Result := defaultValue;
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := FirstOrDefault(predicate, Default(T));
end;

function TEnumerableBase<T>.FirstOrDefault(const predicate: TPredicate<T>;
  const defaultValue: T): T;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  for item in Self do
    if predicate(item) then
      Exit(item);
  Result := defaultValue;
end;

procedure TEnumerableBase<T>.ForEach(const action: TAction<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(action), 'action');
{$ENDIF}

  for item in Self do
    action(item);
end;

function TEnumerableBase<T>.GetComparer: IComparer<T>;
begin
  Result := fComparer;
end;

function TEnumerableBase<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

function TEnumerableBase<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorBase<T>.Create;
end;

function TEnumerableBase<T>.GetEnumeratorNonGeneric: IEnumerator;
begin
  Result := GetEnumerator;
end;

class function TEnumerableBase<T>.GetEqualityComparer: IEqualityComparer<T>;
begin
  if not Assigned(fEqualityComparer) then
    fEqualityComparer := TEqualityComparer<T>.Default;
  Result := fEqualityComparer;
end;

function TEnumerableBase<T>.IsManaged: Boolean;
begin
{$IFDEF DELPHIXE7_UP}
  Result := IsManagedType(T);
{$ELSE}
  Result := Rtti.IsManaged(TypeInfo(T));
{$ENDIF}
end;

{$IFDEF WEAKREF}
function TEnumerableBase<T>.HasWeakRef: Boolean;
begin
{$IFDEF DELPHIXE7_UP}
  Result := System.HasWeakRef(T);
{$ELSE}
  Result := System.TypInfo.HasWeakRef(TypeInfo(T));
{$ENDIF}
end;
{$ENDIF}

function TEnumerableBase<T>.Last: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  repeat
    Result := enumerator.Current;
  until not enumerator.MoveNext;
end;

function TEnumerableBase<T>.Last(const predicate: TPredicate<T>): T;
var
  item: T;
  found: Boolean;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  found := False;
  for item in Self do
  begin
    if predicate(item) then
    begin
      found := True;
      Result := item;
    end;
  end;
  if not found then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement);
end;

function TEnumerableBase<T>.LastOrDefault: T;
begin
  Result := LastOrDefault(Default(T));
end;

function TEnumerableBase<T>.LastOrDefault(const defaultValue: T): T;
var
  item: T;
begin
  Result := defaultValue;
  for item in Self do
    Result := item;
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := LastOrDefault(predicate, Default(T));
end;

function TEnumerableBase<T>.LastOrDefault(const predicate: TPredicate<T>;
  const defaultValue: T): T;
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := defaultValue;
  for item in Self do
    if predicate(item) then
      Result := item;
end;

function TEnumerableBase<T>.Max: T;
begin
  Result := Max(Comparer);
end;

{$IFDEF DELPHIXE_UP}
function TEnumerableBase<T>.Max(const selector: TFunc<T, Integer>): Integer;
begin
  Result := TEnumerable.Max<T>(Self, selector);
end;
{$ENDIF}

function TEnumerableBase<T>.Max(const comparer: IComparer<T>): T;
var
  flag: Boolean;
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  flag := False;
  for item in Self do
  begin
    if flag then
    begin
      if comparer.Compare(item, Result) > 0 then
        Result := item;
    end
    else
    begin
      flag := True;
      Result := item;
    end;
  end;
  if not flag then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function TEnumerableBase<T>.Max(const comparer: TComparison<T>): T;
begin
  Result := Max(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.Min: T;
begin
  Result := Min(Comparer);
end;

{$IFDEF DELPHIXE_UP}
function TEnumerableBase<T>.Min(const selector: TFunc<T, Integer>): Integer;
begin
  Result := TEnumerable.Min<T>(Self, selector);
end;
{$ENDIF}

function TEnumerableBase<T>.Min(const comparer: IComparer<T>): T;
var
  flag: Boolean;
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  flag := False;
  for item in Self do
  begin
    if flag then
    begin
      if fComparer.Compare(item, Result) < 0 then
        Result := item;
    end
    else
    begin
      flag := True;
      Result := item;
    end;
  end;
  if not flag then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function TEnumerableBase<T>.Min(const comparer: TComparison<T>): T;
begin
  Result := Min(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.Ordered: IEnumerable<T>;
begin
  Result := TOrderedIterator<T>.Create(Self, Comparer);
end;

function TEnumerableBase<T>.Ordered(
  const comparer: IComparer<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  Result := TOrderedIterator<T>.Create(Self, comparer);
end;

function TEnumerableBase<T>.Ordered(
  const comparer: TComparison<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(comparer), 'comparer');
{$ENDIF}

  Result := Ordered(IComparer<T>(PPointer(@comparer)^));
end;

function TEnumerableBase<T>.Reversed: IEnumerable<T>;
begin
  Result := TReversedIterator<T>.Create(Self);
end;

function TEnumerableBase<T>.Single: T;
var
  enumerator: IEnumerator<T>;
begin
  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  Result := enumerator.Current;
  if enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
end;

function TEnumerableBase<T>.Single(const predicate: TPredicate<T>): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  found: Boolean;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  found := False;
  repeat
    item := enumerator.Current;
    if predicate(item) then
    begin
      if found then
        raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneMatchingElement);
      found := True;
      Result := item;
    end;
  until not enumerator.MoveNext;
  if not found then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoMatchingElement);
end;

function TEnumerableBase<T>.SingleOrDefault: T;
begin
  Result := SingleOrDefault(Default(T));
end;

function TEnumerableBase<T>.SingleOrDefault(const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
begin
  Result := defaultValue;
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
  begin
    Result := enumerator.Current;
    if enumerator.MoveNext then
      raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
  end;
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: TPredicate<T>): T;
begin
  Result := SingleOrDefault(predicate, Default(T));
end;

function TEnumerableBase<T>.SingleOrDefault(const predicate: TPredicate<T>;
  const defaultValue: T): T;
var
  enumerator: IEnumerator<T>;
  item: T;
  found: Boolean;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  enumerator := GetEnumerator;
  if not enumerator.MoveNext then
    Exit(Default(T));
  found := False;
  repeat
    item := enumerator.Current;
    if predicate(item) then
    begin
      if found then
        raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneMatchingElement);
      found := True;
      Result := item;
    end;
  until not enumerator.MoveNext;
  if not found then
    Result := defaultValue;
end;

function TEnumerableBase<T>.Skip(count: Integer): IEnumerable<T>;
begin
  Result := TSkipIterator<T>.Create(Self, count);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TSkipWhileIterator<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.SkipWhile(
  const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TSkipWhileIndexIterator<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.Take(count: Integer): IEnumerable<T>;
begin
  Result := TTakeIterator<T>.Create(Self, count);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TTakeWhileIterator<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.TakeWhile(
  const predicate: TFunc<T, Integer, Boolean>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TTakeWhileIndexIterator<T>.Create(Self, predicate);
end;

function TEnumerableBase<T>.ToArray: TArray<T>;
var
  collection: ICollection<T>;
  count: Integer;
  item: T;
begin
  if Supports(Self, ICollection<T>, collection) then
  begin
    count := collection.Count;
    if count > 0 then
    begin
      SetLength(Result, count);
      collection.CopyTo(Result, 0);
    end;
  end
  else
  begin
    count := 0;
    for item in Self do
    begin
      if Result = nil then
        SetLength(Result, 4)
      else if Length(Result) = count then
        SetLength(Result, count * 2);
      Result[count] := item;
      Inc(count);
    end;
    SetLength(Result, count);
  end;
end;

function TEnumerableBase<T>.TryGetElementAt(out value: T;
  index: Integer): Boolean;
var
  item: T;
begin
  if index < 0 then
    Exit(False);
  for item in Self do
  begin
    if index = 0 then
    begin
      value := item;
      Exit(True);
    end;
    Dec(index);
  end;
  Result := False;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  Result := False;
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
  begin
    value := enumerator.Current;
    Result := True;
  end;
end;

function TEnumerableBase<T>.TryGetFirst(out value: T; const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  Result := False;
  for item in Self do
  begin
    if predicate(item) then
    begin
      value := item;
      Exit(True);
    end;
  end;
end;

function TEnumerableBase<T>.TryGetLast(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
begin
  Result := False;
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
  begin
    repeat
      value := enumerator.Current;
    until not enumerator.MoveNext;
    Result := True;
  end;
end;

function TEnumerableBase<T>.TryGetLast(out value: T; const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  Result := False;
  for item in Self do
  begin
    if predicate(item) then
    begin
      value := item;
      Result := True;
    end;
  end;
end;

function TEnumerableBase<T>.TryGetSingle(out value: T): Boolean;
var
  enumerator: IEnumerator<T>;
  item: T;
begin
  Result := False;
  enumerator := GetEnumerator;
  if enumerator.MoveNext then
  begin
    item := enumerator.Current;
    if not enumerator.MoveNext then
    begin
      value := item;
      Result := True;
    end;
  end;
end;

function TEnumerableBase<T>.TryGetSingle(out value: T;
  const predicate: TPredicate<T>): Boolean;
var
  item: T;
begin
  Result := False;
  for item in Self do
  begin
    if predicate(item) then
    begin
      if Result then
        Exit(False);
      value := item;
      Result := True;
    end;
  end;
end;

function TEnumerableBase<T>.Where(
  const predicate: TPredicate<T>): IEnumerable<T>;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(predicate), 'predicate');
{$ENDIF}

  Result := TWhereIterator<T>.Create(Self, predicate);
end;

{$ENDREGION}


{$REGION 'TIteratorBase<T>' }

function TIteratorBase<T>.MoveNext: Boolean;
begin
  Result := False;
end;

procedure TIteratorBase<T>.Reset;
begin
  raise ENotSupportedException.CreateRes(@SCannotResetEnumerator);
end;

{$ENDREGION}


{$REGION 'TIterator<T>'}

constructor TIterator<T>.Create;
begin
  inherited Create;
  fState := STATE_INITIAL;
  fInitialThreadId := TThread.CurrentThread.ThreadID;
end;

function TIterator<T>.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TIterator<T>.GetCurrentNonGeneric: TValue;
begin
  Result := TValue.From<T>(GetCurrent);
end;

function TIterator<T>.GetEnumerator: IEnumerator<T>;
var
  iterator: TIterator<T>;
begin
  if (fInitialThreadId = TThread.CurrentThread.ThreadID) and (fState = STATE_INITIAL) then
  begin
    fState := STATE_ENUMERATOR;
    Result := Self;
  end
  else
  begin
    iterator := Clone;
    iterator.fState := STATE_ENUMERATOR;
    Result := iterator;
  end;
end;

{$ENDREGION}


{$REGION 'TCollectionBase<T>'}

constructor TCollectionBase<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TCollectionBase<T>.Create(const values: array of T);
begin
  Create;
  AddRange(values);
end;

constructor TCollectionBase<T>.Create(const collection: IEnumerable<T>);
begin
  Create;
  AddRange(collection);
end;

procedure TCollectionBase<T>.Add(const item: T);
begin
  AddInternal(item);
end;

procedure TCollectionBase<T>.AddRange(const values: array of T);
var
  item: T;
begin
  for item in values do
    Add(item);
end;

procedure TCollectionBase<T>.AddRange(const collection: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
    Add(item);
end;

procedure TCollectionBase<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  if fOnChanged.IsInvokable then
    fOnChanged.Invoke(Self, item, action);
end;

procedure TCollectionBase<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(Length(values), index, Count);
{$ENDIF}

  for item in Self do
  begin
    values[index] := item;
    Inc(index);
  end;
end;

procedure TCollectionBase<T>.ExtractAll(const predicate: TPredicate<T>);
begin
  ExtractRange(Where(predicate).ToArray);
end;

procedure TCollectionBase<T>.ExtractRange(const values: array of T);
var
  item: T;
begin
  for item in values do
    Extract(item);
end;

procedure TCollectionBase<T>.ExtractRange(const collection: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
    Extract(item);
end;

function TCollectionBase<T>.GetIsReadOnly: Boolean;
begin
  Result := False;
end;

function TCollectionBase<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

procedure TCollectionBase<T>.MoveTo(const collection: ICollection<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  for item in ToArray do
  begin
    Extract(item);
    collection.Add(item);
  end;
end;

procedure TCollectionBase<T>.RemoveRange(const values: array of T);
var
  item: T;
begin
  for item in values do
    Remove(item);
end;

procedure TCollectionBase<T>.RemoveAll(const predicate: TPredicate<T>);
begin
  RemoveRange(Where(predicate).ToArray);
end;

procedure TCollectionBase<T>.RemoveRange(const collection: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
    Remove(item);
end;

{$ENDREGION}


{$REGION 'TContainedIterator<T>'}

constructor TContainedIterator<T>.Create(const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedIterator<T>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedIterator<T>._AddRef: Integer;
begin
  Result := IInterface(fController)._AddRef;
end;

function TContainedIterator<T>._Release: Integer;
begin
  Result := IInterface(fController)._Release;
end;

{$ENDREGION}


{$REGION 'TContainedCollectionBase<T>'}

constructor TContainedCollectionBase<T>.Create(const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedCollectionBase<T>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedCollectionBase<T>._AddRef: Integer;
begin
  Result := IInterface(fController)._AddRef;
end;

function TContainedCollectionBase<T>._Release: Integer;
begin
  Result := IInterface(fController)._Release;
end;

{$ENDREGION}


{$REGION 'TContainedReadOnlyCollection<T>'}

constructor TContainedReadOnlyCollection<T>.Create(const controller: IInterface);
begin
  inherited Create;
  fController := Pointer(controller);
end;

function TContainedReadOnlyCollection<T>.GetController: IInterface;
begin
  Result := IInterface(fController);
end;

function TContainedReadOnlyCollection<T>._AddRef: Integer;
begin
  Result := IInterface(fController)._AddRef;
end;

function TContainedReadOnlyCollection<T>._Release: Integer;
begin
  Result := IInterface(fController)._Release;
end;

{$ENDREGION}


{$REGION 'TMapBase<TKey, T>'}

constructor TMapBase<TKey, T>.Create;
begin
  inherited;
  fOnKeyChanged := TCollectionChangedEventImpl<TKey>.Create;
  fOnValueChanged := TCollectionChangedEventImpl<T>.Create;
end;

procedure TMapBase<TKey, T>.AddInternal(const item: TGenericPair);
begin
  Add(item.Key, item.Value);
end;

function TMapBase<TKey, T>.Contains(const item: TGenericPair): Boolean;
begin
  Result := ContainsPair(item.Key, item.Value);
end;

function TMapBase<TKey, T>.Extract(const item: TGenericPair): TGenericPair;
begin
  Result := ExtractPair(item.Key, item.Value);
end;

function TMapBase<TKey, T>.GetKeyType: PTypeInfo;
begin
  Result := TypeInfo(TKey);
end;

function TMapBase<TKey, T>.GetOnKeyChanged: ICollectionChangedEvent<TKey>;
begin
  Result := fOnKeyChanged;
end;

function TMapBase<TKey, T>.GetOnValueChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnValueChanged;
end;

function TMapBase<TKey, T>.GetValueType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;

procedure TMapBase<TKey, T>.KeyChanged(const Item: TKey;
  Action: TCollectionChangedAction);
begin
  if Assigned(fOnKeyChanged) and fOnKeyChanged.IsInvokable then
      fOnKeyChanged.Invoke(Self, Item, Action)
end;

function TMapBase<TKey, T>.Remove(const item: TGenericPair): Boolean;
begin
  Result := RemovePair(item.Key, item.Value);
end;

procedure TMapBase<TKey, T>.ValueChanged(const Item: T;
  Action: TCollectionChangedAction);
begin
  if Assigned(fOnValueChanged) and fOnValueChanged.IsInvokable then
    fOnValueChanged.Invoke(Self, Item, Action)
end;

{$ENDREGION}


{$REGION 'TListBase<T>'}

destructor TListBase<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TListBase<T>.Add(const item: T): Integer;
begin
  Result := Count;
  Insert(Result, item);
end;

procedure TListBase<T>.AddInternal(const item: T);
begin
  Add(item);
end;

procedure TListBase<T>.AddRange(const values: array of T);
begin
  InsertRange(Count, values);
end;

procedure TListBase<T>.AddRange(const collection: IEnumerable<T>);
begin
  InsertRange(Count, collection);
end;

function TListBase<T>.AsList: IList;
begin
  Result := TListAdapter<T>.Create(Self);
end;

function TListBase<T>.AsReadOnlyList: IReadOnlyList<T>;
begin
  Result := Self;
end;

procedure TListBase<T>.Clear;
begin
  if Count > 0 then
    DeleteRange(0, Count);
end;

procedure TListBase<T>.DeleteRange(index, count: Integer);
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  for i := 1 to count do
    Delete(index);
end;

function TListBase<T>.First: T;
begin
  if Count > 0 then
    Result := Items[0]
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function TListBase<T>.FirstOrDefault(const defaultValue: T): T;
begin
  if Count > 0 then
    Result := Items[0]
  else
    Result := defaultValue;
end;

function TListBase<T>.GetCount: Integer;
begin
  Result := inherited;
end;

function TListBase<T>.GetRange(index, count: Integer): IList<T>;
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  Result := TList<T>.Create;
  Result.Count := count;
  for i := index to index + count do
    Result[i] := Items[i];
end;

function TListBase<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, Count);
end;

function TListBase<T>.IndexOf(const item: T; index: Integer): Integer;
begin
  Result := IndexOf(item, index, Count - index);
end;

function TListBase<T>.IndexOf(const item: T; index, count: Integer): Integer;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  comparer := EqualityComparer;
  for i := index to index + count - 1 do
    if comparer.Equals(Items[i], item) then
      Exit(i);
  Result := -1;
end;

procedure TListBase<T>.InsertRange(index: Integer; const values: array of T);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  for item in values do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

procedure TListBase<T>.InsertRange(index: Integer;
  const collection: IEnumerable<T>);
var
  item: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
  Guard.CheckNotNull(Assigned(collection), 'collection');
{$ENDIF}

  for item in collection do
  begin
    Insert(index, item);
    Inc(index);
  end;
end;

function TListBase<T>.Last: T;
var
  count: Integer;
begin
  count := Self.Count;
  if count = 0 then
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
  Result := Items[count - 1];
end;

function TListBase<T>.LastOrDefault(const defaultValue: T): T;
var
  count: Integer;
begin
  count := Self.Count;
  if count = 0 then
    Result := defaultValue
  else
    Result := Items[count - 1];
end;

function TListBase<T>.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IsEqualGUID(IID, IObjectList) then
  begin
    if ElementType.Kind = tkClass then
      Result := inherited QueryInterface(IList<TObject>, Obj)
    else
      Result := E_NOINTERFACE;
  end
  else
    Result := inherited;
end;

function TListBase<T>.LastIndexOf(const item: T): Integer;
begin
  Result := LastIndexOf(item, Count - 1, Count);
end;

function TListBase<T>.LastIndexOf(const item: T; index: Integer): Integer;
begin
  Result := LastIndexOf(item, index, index + 1);
end;

function TListBase<T>.LastIndexOf(const item: T; index,
  count: Integer): Integer;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= index + 1), 'count');
{$ENDIF}

  comparer := EqualityComparer;
  for i := index downto index - count + 1 do
    if comparer.Equals(Items[i], item) then
      Exit(i);
  Result := -1;
end;

function TListBase<T>.Remove(const item: T): Boolean;
var
  index: Integer;
begin
  index := IndexOf(item);
  Result := index > -1;
  if Result then
    Delete(index);
end;

procedure TListBase<T>.Reverse;
begin
  Reverse(0, Count);
end;

procedure TListBase<T>.SetCount(count: Integer);
begin
  raise ENotSupportedException.Create('SetCount');
end;

function TListBase<T>.Single: T;
begin
  case Count of
    0: raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
    1: Result := Items[0];
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
  end;
end;

function TListBase<T>.SingleOrDefault(const defaultValue: T): T;
begin
  case Count of
    0: Result := defaultValue;
    1: Result := Items[0];
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsMoreThanOneElement);
  end;
end;

procedure TListBase<T>.Sort;
begin
  Sort(Comparer);
end;

procedure TListBase<T>.Sort(const comparison: TComparison<T>);
begin
  Sort(IComparer<T>(PPointer(@comparison)^));
end;

function TListBase<T>.ToArray: TArray<T>;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Length(Result) - 1 do
    Result[i] := Items[i];
end;

procedure TListBase<T>.TrimExcess;
begin
  Capacity := Count;
end;

function TListBase<T>.TryGetElementAt(out value: T; index: Integer): Boolean;
begin
  Result := (index >= 0) and (index < Count);
  if Result then
    value := Items[index];
end;

function TListBase<T>.TryGetFirst(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := Items[0];
end;

function TListBase<T>.TryGetLast(out value: T): Boolean;
begin
  Result := Count > 0;
  if Result then
    value := Items[Count - 1];
end;

function TListBase<T>.TryGetSingle(out value: T): Boolean;
begin
  Result := Count = 1;
  if Result then
    value := Items[0];
end;

{$ENDREGION}


end.
