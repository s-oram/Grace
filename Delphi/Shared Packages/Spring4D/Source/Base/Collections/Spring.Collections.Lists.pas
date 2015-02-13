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

unit Spring.Collections.Lists;

interface

uses
  Classes,
  Generics.Collections,
  Generics.Defaults,
  SysUtils,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type

{$IFNDEF DELPHIXE3_UP}
  {$DEFINE SPRING_ARRAYMANAGERS}
{$ENDIF}

{$IFDEF SPRING_ARRAYMANAGERS}
  TArrayManager<T> = class abstract
    procedure Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer); overload; virtual; abstract;
    procedure Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer); overload; virtual; abstract;
    procedure Finalize(var AArray: array of T; Index, Count: Integer); overload; virtual; abstract;
  end;

  TMoveArrayManager<T> = class(TArrayManager<T>)
    procedure Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer); overload; override;
    procedure Finalize(var AArray: array of T; Index, Count: Integer); override;
  end;
{$ENDIF}

  ///	<summary>
  ///	  Represents a strongly typed list of elements that can be accessed by
  ///	  index. Provides methods to search, sort, and manipulate lists.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The type of elements in the list.
  ///	</typeparam>
  TList<T> = class(TListBase<T>, IArrayAccess<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TList<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
        procedure Reset; override;
      end;
      TArrayOfT = array of T; // Delphi 2010 compatibility
  private
    fItems: TArray<T>;
    fCount: Integer;
    fVersion: Integer;
    fArrayManager: TArrayManager<T>;
    procedure DeleteInternal(index: Integer; notification: TCollectionChangedAction);
    procedure DeleteAllInternal(const predicate: TPredicate<T>;
      notification: TCollectionChangedAction);
    procedure IncreaseVersion; inline;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetItem(index: Integer): T; override;
    function GetItems: TArray<T>;
    procedure SetCapacity(value: Integer); override;
    procedure SetCount(value: Integer); override;
    procedure SetItem(index: Integer; const value: T); override;
  {$ENDREGION}

    procedure EnsureCapacity(capacity: Integer); inline;
    procedure Grow(capacity: Integer);
  public
    constructor Create; override;
    constructor Create(const values: array of T); override;
    constructor Create(const collection: IEnumerable<T>); override;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Clear; override;

    function Contains(const value: T; const comparer: IEqualityComparer<T>): Boolean; override;
    function IndexOf(const item: T; index, count: Integer): Integer; override;

    procedure Insert(index: Integer; const item: T); override;
    procedure InsertRange(index: Integer; const values: array of T); override;
    procedure InsertRange(index: Integer; const collection: IEnumerable<T>); override;

    procedure Delete(index: Integer); override;
    procedure DeleteRange(index, count: Integer); override;

    procedure RemoveAll(const predicate: TPredicate<T>); override;

    function Extract(const item: T): T; override;
    procedure ExtractAll(const predicate: TPredicate<T>); override;

    function GetRange(index, count: Integer): IList<T>; override;

    procedure Exchange(index1, index2: Integer); override;
    procedure Move(currentIndex, newIndex: Integer); override;

    procedure Reverse(index, count: Integer); override;
    procedure Sort(const comparer: IComparer<T>); override;

    procedure CopyTo(var values: TArray<T>; index: Integer); override;
    function ToArray: TArray<T>; override;
  end;

{$IFDEF SUPPORTS_GENERIC_FOLDING}
  TObjectList<T: class> = class(TList<TObject>, ICollectionOwnership, IObjectList)
{$ELSE}
  TObjectList<T: class> = class(TList<T>, ICollectionOwnership)
{$ENDIF}
  private
    fOwnsObjects: Boolean;
  {$REGION 'Property Accessors'}
    function GetOwnsObjects: Boolean;
    procedure SetOwnsObjects(const value: Boolean);
  {$ENDREGION}
  protected
{$IFDEF SUPPORTS_GENERIC_FOLDING}
    function GetElementType: PTypeInfo; override;
    procedure Changed(const item: TObject; action: TCollectionChangedAction); override;
{$ELSE}
    procedure Changed(const item: T; action: TCollectionChangedAction); override;
{$ENDIF}
  public
    constructor Create; override;
    constructor Create(ownsObjects: Boolean); overload;
    constructor Create(const comparer: IComparer<T>; ownsObjects: Boolean = True); overload;

    property OwnsObjects: Boolean read GetOwnsObjects write SetOwnsObjects;
  end;

{$IFDEF SUPPORTS_GENERIC_FOLDING}
  TInterfaceList<T: IInterface> = class(TList<IInterface>, IInterfaceList)
{$ELSE}
  TInterfaceList<T: IInterface> = class(TList<T>)
{$ENDIF}
  protected
{$IFDEF SUPPORTS_GENERIC_FOLDING}
    function GetElementType: PTypeInfo; override;
{$ENDIF}
  public
    constructor Create(const comparer: IComparer<T>); overload;
  end;

  TSortedList<T> = class(TList<T>)
  protected
    procedure SetItem(index: Integer; const value: T); override;
  public
    function Add(const item: T): Integer; override;
    procedure Insert(index: Integer; const item: T); override;

    function Contains(const value: T): Boolean; override;
    function IndexOf(const item: T; index, count: Integer): Integer; override;
    function LastIndexOf(const item: T; index, count: Integer): Integer; override;

    procedure Exchange(index1, index2: Integer); override;
    procedure Move(currentIndex, newIndex: Integer); override;
  end;

  TCollectionList<T: TCollectionItem> = class(TListBase<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TCollectionList<T>;
        fIndex: Integer;
        fVersion: Integer;
        fCurrent: T;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TCollectionList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
        procedure Reset; override;
      end;
  private
    fCollection: TCollection;
    fVersion: Integer;
    procedure DeleteInternal(index: Integer; notification: TCollectionChangedAction);
    procedure IncreaseVersion; inline;
  protected
  {$REGION 'Property Accessors'}
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetElementType: PTypeInfo; override;
    function GetItem(index: Integer): T; override;
    procedure SetCapacity(value: Integer); override;
    procedure SetItem(index: Integer; const value: T); override;
  {$ENDREGION}
  public
    constructor Create(const collection: TCollection);
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Insert(index: Integer; const item: T); override;

    procedure Delete(index: Integer); override;
    procedure DeleteRange(index, count: Integer); override;

    function Extract(const item: T): T; override;

    procedure Exchange(index1, index2: Integer); override;
    procedure Move(currentIndex, newIndex: Integer); override;
  end;

  TAnonymousReadOnlyList<T> = class(TEnumerableBase<T>, IReadOnlyList<T>)
  private
    fCount: TFunc<Integer>;
    fItems: TFunc<Integer, T>;
    fIterator: IEnumerable<T>;
  protected
  {$REGION 'Property Accessors'}
    function GetItem(index: Integer): T;
  {$ENDREGION}
  public
    constructor Create(const count: TFunc<Integer>;
      const items: TFunc<Integer, T>;
      const iterator: IEnumerable<T>{$IFDEF DELPHIXE3_UP} = nil{$ENDIF});

    function GetEnumerator: IEnumerator<T>; override;

    function IndexOf(const item: T): Integer; overload;
    function IndexOf(const item: T; index: Integer): Integer; overload;
    function IndexOf(const item: T; index, count: Integer): Integer; overload;
  end;

implementation

uses
  Spring.Collections.Extensions,
  Spring.ResourceStrings;


{$REGION 'TMoveArrayManager<T>'}

{$IFDEF SPRING_ARRAYMANAGERS}
procedure TMoveArrayManager<T>.Finalize(var AArray: array of T; Index, Count: Integer);
begin
  System.FillChar(AArray[Index], Count * SizeOf(T), 0);
end;

procedure TMoveArrayManager<T>.Move(var AArray: array of T; FromIndex, ToIndex, Count: Integer);
begin
  System.Move(AArray[FromIndex], AArray[ToIndex], Count * SizeOf(T));
end;

procedure TMoveArrayManager<T>.Move(var FromArray, ToArray: array of T; FromIndex, ToIndex, Count: Integer);
begin
  System.Move(FromArray[FromIndex], ToArray[ToIndex], Count * SizeOf(T));
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TList<T>'}

constructor TList<T>.Create;
begin
  inherited Create;
{$IFDEF WEAKREF}
  if HasWeakRef then
    fArrayManager := TManualArrayManager<T>.Create
  else
{$ENDIF}
    fArrayManager := TMoveArrayManager<T>.Create;
end;

constructor TList<T>.Create(const values: array of T);
var
  i: Integer;
begin
  Create;
  fCount := Length(values);
  if fCount > 0 then
  begin
    SetLength(fItems, fCount);
    for i := Low(values) to High(values) do
      fItems[i] := values[i];
  end;
end;

constructor TList<T>.Create(const collection: IEnumerable<T>);
var
  c: ICollection<T>;
begin
  if Supports(collection, ICollection<T>, c) then
  begin
    Create;
    fCount := c.Count;
    if fCount > 0 then
    begin
      SetLength(fItems, fCount);
      c.CopyTo(fItems, 0);
    end;
  end
  else
    inherited;
end;

destructor TList<T>.Destroy;
begin
  inherited Destroy;
  fArrayManager.Free;
end;

function TList<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TList<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
{$ENDIF}

  Result := fItems[index];
end;

function TList<T>.GetItems: TArray<T>;
begin
  Result := fItems;
end;

function TList<T>.GetRange(index, count: Integer): IList<T>;
var
  list: TList<T>;
{$IFNDEF DELPHIXE2_UP}
  i: Integer;
{$ENDIF}
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  list := TList<T>.Create;
  list.fCount := count;
{$IFDEF DELPHIXE2_UP}
  list.fItems := Copy(fItems, index, count);
{$ELSE}
  SetLength(list.fItems, count);
  for i := 0 to count - 1 do
  begin
    list.fItems[i] := fItems[index];
    Inc(index);
  end;
{$ENDIF}
  Result := list;
end;

procedure TList<T>.Grow(capacity: Integer);
var
  newCapacity: Integer;
begin
  newCapacity := Length(fItems);
  if newCapacity = 0 then
    newCapacity := capacity
  else
    repeat
      newCapacity := newCapacity * 2;
      if newCapacity < 0 then
        OutOfMemoryError;
    until newCapacity >= capacity;
  SetCapacity(newCapacity);
end;

{$IFOPT Q+}{$DEFINE OVERFLOW_CHECKS_ON}{$Q-}{$ENDIF}
procedure TList<T>.IncreaseVersion;
begin
  Inc(fVersion);
end;
{$IFDEF OVERFLOW_CHECKS_ON}{$Q+}{$ENDIF}

function TList<T>.IndexOf(const item: T; index, count: Integer): Integer;
{$IFDEF DELPHI2010}
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  comparer := EqualityComparer;
  for i := index to index + count - 1 do
    if comparer.Equals(fItems[i], item) then
      Exit(i);
  Result := -1;
{$ELSE}
begin
  Result := TArray.IndexOf<T>(fItems, item, index, count, EqualityComparer);
{$ENDIF}
end;

procedure TList<T>.SetItem(index: Integer; const value: T);
var
  oldItem: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
{$ENDIF}

  oldItem := fItems[index];
  fItems[index] := value;
  IncreaseVersion;

  Changed(oldItem, caRemoved);
  Changed(value, caAdded);
end;

procedure TList<T>.Insert(index: Integer; const item: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
{$ENDIF}

  EnsureCapacity(fCount + 1);
  if index <> fCount then
  begin
    fArrayManager.Move(fItems, index, index + 1, fCount - index);
    fArrayManager.Finalize(fItems, index, 1);
  end;
  fItems[index] := item;
  Inc(fCount);
  IncreaseVersion;

  Changed(item, caAdded);
end;

procedure TList<T>.InsertRange(index: Integer; const values: array of T);
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
{$ENDIF}

  EnsureCapacity(fCount + Length(values));
  if index <> fCount then
  begin
    fArrayManager.Move(fItems, index, index + Length(values), fCount - index);
    fArrayManager.Finalize(fItems, index, Length(values));
  end;

  if not IsManaged{$IFDEF WEAKREF} and not HasWeakRef{$ENDIF} then
    System.Move(values[0], fItems[index], Length(values) * SizeOf(T))
  else
    for i := Low(values) to High(values) do
      fItems[index + i] := values[i];

  Inc(fCount, Length(values));
  IncreaseVersion;

  for i := Low(values) to High(values) do
    Changed(values[i], caAdded);
end;

procedure TList<T>.InsertRange(index: Integer;
  const collection: IEnumerable<T>);
var
  list: TList<T>;
  i: Integer;
begin
  if collection.AsObject is TList<T> then
  begin
{$IFDEF SPRING_ENABLE_GUARD}
    Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
{$ENDIF}

    list := TList<T>(collection.AsObject);

    EnsureCapacity(fCount + Length(list.fItems));
    if index <> fCount then
    begin
      fArrayManager.Move(fItems, index, index + Length(list.fItems), fCount - index);
      fArrayManager.Finalize(fItems, index, Length(list.fItems));
    end;

    if not IsManaged{$IFDEF WEAKREF} and not HasWeakRef{$ENDIF} then
      System.Move(list.fItems[0], fItems[index], list.fCount * SizeOf(T))
    else
      for i := Low(list.fItems) to list.fCount - 1 do
        fItems[index + i] := list.fItems[i];

    Inc(fCount, list.fCount);
    IncreaseVersion;

    for i := 0 to list.fCount - 1 do
      Changed(list.fItems[i], caAdded);
  end
  else
    inherited InsertRange(index, collection);
end;

procedure TList<T>.DeleteInternal(index: Integer;
  notification: TCollectionChangedAction);
var
  oldItem: T;
begin
  oldItem := fItems[index];
  fItems[index] := Default(T);
  Dec(fCount);
  if index <> fCount then
  begin
    fArrayManager.Move(fItems, index + 1, index, fCount - index);
    fArrayManager.Finalize(fItems, fCount, 1);
  end;
  IncreaseVersion;

  Changed(oldItem, notification);
end;

procedure TList<T>.DeleteRange(index, count: Integer);
var
  oldItems: TArray<T>;
  tailCount,
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  SetLength(oldItems, count);
  fArrayManager.Move(fItems, oldItems, index, 0, count);

  tailCount := fCount - (index + count);
  if tailCount > 0 then
  begin
    fArrayManager.Move(fItems, index + count, index, tailCount);
    fArrayManager.Finalize(fItems, fCount - count, count);
  end
  else
    fArrayManager.Finalize(fItems, index, count);

  Dec(fCount, count);
  IncreaseVersion;

  for i := Low(oldItems) to High(oldItems) do
    Changed(oldItems[i], caRemoved);
end;

procedure TList<T>.Sort(const comparer: IComparer<T>);
begin
  TArray.Sort<T>(fItems, comparer, 0, fCount);
  IncreaseVersion;

  Changed(Default(T), caReseted);
end;

procedure TList<T>.Move(currentIndex, newIndex: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((currentIndex >= 0) and (currentIndex < fCount), 'currentIndex');
  Guard.CheckRange((newIndex >= 0) and (newIndex < fCount), 'newIndex');
{$ENDIF}

  temp := fItems[currentIndex];
  fItems[currentIndex] := Default(T);
  if currentIndex < newIndex then
    fArrayManager.Move(fItems, currentIndex + 1, currentIndex, newIndex - currentIndex)
  else
    fArrayManager.Move(fItems, newIndex, newIndex + 1, currentIndex - newIndex);

  fArrayManager.Finalize(fItems, newIndex, 1);
  fItems[newIndex] := temp;
  IncreaseVersion;

  Changed(temp, caMoved);
end;

procedure TList<T>.Clear;
begin
  inherited Clear;
  Capacity := 0;
end;

procedure TList<T>.EnsureCapacity(capacity: Integer);
begin
  if capacity > Length(fItems) then
    Grow(capacity)
  else if capacity < 0 then
    OutOfMemoryError;
end;

procedure TList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index1 >= 0) and (index1 < fCount), 'index1');
  Guard.CheckRange((index2 >= 0) and (index2 < fCount), 'index2');
{$ENDIF}

  temp := fItems[index1];
  fItems[index1] := fItems[index2];
  fItems[index2] := temp;
  IncreaseVersion;

  Changed(fItems[index2], caMoved);
  Changed(fItems[index1], caMoved);
end;

function TList<T>.GetCapacity: Integer;
begin
  Result := Length(fItems);
end;

procedure TList<T>.RemoveAll(const predicate: TPredicate<T>);
begin
  DeleteAllInternal(predicate, caRemoved);
end;

procedure TList<T>.Reverse(index, count: Integer);
var
  temp: T;
  index1, index2: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  index1 := index;
  index2 := index + count - 1;
  while index1 < index2 do
  begin
    temp := fItems[index1];
    fItems[index1] := fItems[index2];
    fItems[index2] := temp;
    Inc(index1);
    Dec(index2);
  end;
  IncreaseVersion;

  Changed(Default(T), caReseted);
end;

procedure TList<T>.SetCapacity(value: Integer);
begin
  if value < fCount then
    DeleteRange(value, fCount - value);
  SetLength(fItems, value);
end;

procedure TList<T>.SetCount(value: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(count >= 0, 'count');
{$ENDIF}

  if value > Capacity then
    SetCapacity(value);
  if value < fCount then
    DeleteRange(value, fCount - value);
  fCount := value;
end;

procedure TList<T>.Delete(index: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < fCount), 'index');
{$ENDIF}

  DeleteInternal(index, caRemoved);
end;

procedure TList<T>.DeleteAllInternal(const predicate: TPredicate<T>;
  notification: TCollectionChangedAction);
var
  i, n: Integer;
  items: TArray<T>;
  item: T;
begin
  n := 0;
  i := 0;
  while i < fCount do
  begin
    if predicate(fItems[i]) then
    begin
      item := fItems[i];
      Inc(n);
      fArrayManager.Move(fItems, i + 1, i, fCount - i - 1);
      fArrayManager.Finalize(fItems, fCount - 1, 1);
      Dec(fCount);
      Changed(item, notification);
    end
    else
      Inc(i)
  end;
end;

function TList<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := fItems[index];
    DeleteInternal(index, caExtracted);
  end;
end;

procedure TList<T>.ExtractAll(const predicate: TPredicate<T>);
begin
  DeleteAllInternal(predicate, caExtracted);
end;

function TList<T>.Contains(const value: T;
  const comparer: IEqualityComparer<T>): Boolean;
var
  index: Integer;
begin
  for index := 0 to fCount - 1 do
    if comparer.Equals(value, fItems[index]) then
      Exit(True);
  Result := False;
end;

procedure TList<T>.CopyTo(var values: TArray<T>; index: Integer);
var
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange(Length(values), index, fCount);
{$ENDIF}

  for i := 0 to fCount - 1 do
  begin
    values[index] := fItems[i];
    Inc(index);
  end;
end;

function TList<T>.ToArray: TArray<T>;
begin
  Result := fItems;
  SetLength(Result, fCount);
end;

{$ENDREGION}


{$REGION 'TList<T>.TEnumerator'}

constructor TList<T>.TEnumerator.Create(const list: TList<T>);
begin
  inherited Create;
  fList := list;
  fList._AddRef;
  fVersion := fList.fVersion;
end;

destructor TList<T>.TEnumerator.Destroy;
begin
  fList._Release;
  inherited Destroy;
end;

function TList<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if fIndex < fList.fCount then
  begin
    fCurrent := fList.fItems[fIndex];
    Inc(fIndex);
    Result := True;
  end
  else
    fCurrent := Default(T);
end;

function TList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

procedure TList<T>.TEnumerator.Reset;
begin
  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  fIndex := 0;
  fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TObjectList<T>'}

constructor TObjectList<T>.Create;
begin
  Create(True);
end;

constructor TObjectList<T>.Create(ownsObjects: Boolean);
begin
  inherited Create;
  fOwnsObjects := ownsObjects;
end;

constructor TObjectList<T>.Create(const comparer: IComparer<T>;
  ownsObjects: Boolean);
begin
{$IFDEF SUPPORTS_GENERIC_FOLDING}
  inherited Create(IComparer<TObject>(comparer));
{$ELSE}
  inherited Create(comparer);
{$ENDIF}
  fOwnsObjects := ownsObjects;
end;

{$IFDEF SUPPORTS_GENERIC_FOLDING}
function TObjectList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;
{$ENDIF}

function TObjectList<T>.GetOwnsObjects: Boolean;
begin
  Result := fOwnsObjects;
end;

procedure TObjectList<T>.SetOwnsObjects(const value: Boolean);
begin
  fOwnsObjects := value;
end;

{$IFDEF SUPPORTS_GENERIC_FOLDING}
procedure TObjectList<T>.Changed(const item: TObject; action: TCollectionChangedAction);
{$ELSE}
procedure TObjectList<T>.Changed(const item: T; action: TCollectionChangedAction);
{$ENDIF}
begin
  inherited Changed(item, action);
  if OwnsObjects and (action = caRemoved) then
{$IFNDEF AUTOREFCOUNT}
    item.Free;
{$ELSE}
    item.DisposeOf;
{$ENDIF}
end;

{$ENDREGION}


{$REGION 'TInterfaceList<T>'}

constructor TInterfaceList<T>.Create(const comparer: IComparer<T>);
begin
{$IFDEF SUPPORTS_GENERIC_FOLDING}
  inherited Create(IComparer<IInterface>(comparer));
{$ELSE}
  inherited Create(comparer);
{$ENDIF}
end;

{$IFDEF SUPPORTS_GENERIC_FOLDING}
function TInterfaceList<T>.GetElementType: PTypeInfo;
begin
  Result := TypeInfo(T);
end;
{$ENDIF}

{$ENDREGION}


{$REGION 'TSortedList<T>'}

function TSortedList<T>.Add(const item: T): Integer;
begin
  TArray.BinarySearch<T>(fItems, item, Result, Comparer, 0, fCount);
  inherited Insert(Result, item);
end;

function TSortedList<T>.Contains(const value: T): Boolean;
var
  index: Integer;
begin
  Result := TArray.BinarySearch<T>(fItems, value, index, Comparer, 0, fCount);
end;

procedure TSortedList<T>.Exchange(index1, index2: Integer);
begin
  raise EInvalidOperationException.Create('Exchange');
end;

function TSortedList<T>.IndexOf(const item: T; index, count: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= fCount), 'index');
  Guard.CheckRange((count >= 0) and (count <= fCount - index), 'count');
{$ENDIF}

  TArray.BinarySearch<T>(fItems, item, Result, Comparer, index, count);
end;

procedure TSortedList<T>.Insert(index: Integer; const item: T);
begin
  raise EInvalidOperationException.Create('Insert');
end;

function TSortedList<T>.LastIndexOf(const item: T; index,
  count: Integer): Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= index + 1), 'count');
{$ENDIF}

  inherited;
//  TArray.BinarySearch<T>(fItems, item, Result, fComparer, index - count + 1, count);
end;

procedure TSortedList<T>.Move(currentIndex, newIndex: Integer);
begin
  raise EInvalidOperationException.Create('Move');
end;

procedure TSortedList<T>.SetItem(index: Integer; const value: T);
begin
  Delete(index);
  Add(value);
end;

{$ENDREGION}


{$REGION 'TCollectionList<T>' }

constructor TCollectionList<T>.Create(const collection: TCollection);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckNotNull(collection, 'collection');
  Guard.CheckInheritsFrom(collection.ItemClass, TClass(T), 'collection.ItemClass');
{$ENDIF}

  inherited Create;
  fCollection := collection;
end;

destructor TCollectionList<T>.Destroy;
begin
  // not calling inherited because we don't want to call Clear
end;

procedure TCollectionList<T>.Delete(index: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  DeleteInternal(index, caRemoved);
end;

procedure TCollectionList<T>.DeleteInternal(index: Integer;
  notification: TCollectionChangedAction);
var
  oldItem: T;
begin
  oldItem := T(fCollection.Items[index]);
  oldItem.Collection := nil;
  IncreaseVersion;

  Changed(oldItem, notification);
  if notification = caRemoved then
    oldItem.Free;
end;

procedure TCollectionList<T>.DeleteRange(index, count: Integer);
var
  oldItems: array of T;
  i: Integer;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Self.Count), 'index');
  Guard.CheckRange((count >= 0) and (count <= Self.Count - index), 'count');
{$ENDIF}

  if count = 0 then
    Exit;

  SetLength(oldItems, count);

  for i := count downto 1 do
  begin
    oldItems[count - i] := T(fCollection.Items[index]);
    fCollection.Items[index].Collection := nil;
  end;
  IncreaseVersion;

  for i := Low(oldItems) to High(oldItems) do
  begin
    Changed(oldItems[i], caRemoved);
    oldItems[i].Free;
  end;
end;

procedure TCollectionList<T>.Exchange(index1, index2: Integer);
var
  temp: T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index1 >= 0) and (index1 < Count), 'index1');
  Guard.CheckRange((index2 >= 0) and (index2 < Count), 'index2');
{$ENDIF}

  temp := T(fCollection.Items[index1]);
  fCollection.Items[index2].Index := index1;
  temp.Index := index2;
  IncreaseVersion;

  Changed(fCollection.Items[index2], caMoved);
  Changed(fCollection.Items[index1], caMoved);
end;

function TCollectionList<T>.Extract(const item: T): T;
var
  index: Integer;
begin
  index := IndexOf(item);
  if index < 0 then
    Result := Default(T)
  else
  begin
    Result := T(fCollection.Items[index]);
    DeleteInternal(index, caExtracted);
  end;
end;

function TCollectionList<T>.GetCapacity: Integer;
begin
  Result := fCollection.Capacity;
end;

function TCollectionList<T>.GetCount: Integer;
begin
  Result := fCollection.Count;
end;

function TCollectionList<T>.GetElementType: PTypeInfo;
begin
  Result := fCollection.ItemClass.ClassInfo;
end;

function TCollectionList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TCollectionList<T>.GetItem(index: Integer): T;
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  Result := T(fCollection.Items[index]);
end;

{$IFOPT Q+}{$DEFINE OVERFLOW_CHECKS_ON}{$Q-}{$ENDIF}
procedure TCollectionList<T>.IncreaseVersion;
begin
  Inc(fVersion);
end;
{$IFDEF OVERFLOW_CHECKS_ON}{$Q+}{$ENDIF}

procedure TCollectionList<T>.Insert(index: Integer; const item: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index <= Count), 'index');
{$ENDIF}

  item.Collection := fCollection;
  item.Index := index;
  IncreaseVersion;

  Changed(item, caAdded);
end;

procedure TCollectionList<T>.Move(currentIndex, newIndex: Integer);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((currentIndex >= 0) and (currentIndex < Count), 'currentIndex');
  Guard.CheckRange((newIndex >= 0) and (newIndex < Count), 'newIndex');
{$ENDIF}

  fCollection.Items[currentIndex].Index := newIndex;
  IncreaseVersion;

  Changed(fCollection.Items[newIndex], caMoved);
end;

procedure TCollectionList<T>.SetCapacity(value: Integer);
begin
  fCollection.Capacity := value;
end;

procedure TCollectionList<T>.SetItem(index: Integer; const value: T);
begin
{$IFDEF SPRING_ENABLE_GUARD}
  Guard.CheckRange((index >= 0) and (index < Count), 'index');
{$ENDIF}

  fCollection.Items[index] := value;
end;

{$ENDREGION}


{$REGION 'TCollectionList<T>.TEnumerator'}

constructor TCollectionList<T>.TEnumerator.Create(const list: TCollectionList<T>);
begin
  inherited Create;
  fList := list;
  fList._AddRef;
  fVersion := fList.fVersion;
end;

destructor TCollectionList<T>.TEnumerator.Destroy;
begin
  fList._Release;
  inherited Destroy;
end;

function TCollectionList<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if fIndex < fList.Count then
  begin
    fCurrent := fList.Items[fIndex];
    Inc(fIndex);
    Result := True;
  end
  else
    fCurrent := Default(T);
end;

function TCollectionList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

procedure TCollectionList<T>.TEnumerator.Reset;
begin
  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  fIndex := 0;
  fCurrent := Default(T);
end;

{$ENDREGION}


{$REGION 'TAnonymousReadOnlyList<T>'}

constructor TAnonymousReadOnlyList<T>.Create(const count: TFunc<Integer>;
  const items: TFunc<Integer, T>; const iterator: IEnumerable<T>);
begin
  inherited Create;
  fCount := count;
  fItems := items;
  fIterator := iterator;
  if not Assigned(fIterator) then
    fIterator := TAnonymousIterator<T>.Create(fCount, fItems);
end;

function TAnonymousReadOnlyList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := fIterator.GetEnumerator;
end;

function TAnonymousReadOnlyList<T>.GetItem(index: Integer): T;
begin
  Result := fItems(index);
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T): Integer;
begin
  Result := IndexOf(item, 0, Count)
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T;
  index: Integer): Integer;
begin
  Result := IndexOf(item, index, Count - index);
end;

function TAnonymousReadOnlyList<T>.IndexOf(const item: T; index,
  count: Integer): Integer;
var
  comparer: IEqualityComparer<T>;
  i: Integer;
begin
  comparer := EqualityComparer;
  for i := index to index + count - 1 do
    if comparer.Equals(fItems(i), item) then
      Exit(i);
  Result := -1;
end;

{$ENDREGION}


end.
