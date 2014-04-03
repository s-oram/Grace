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

unit Spring.Collections.LinkedLists;

interface

uses
  Generics.Defaults,
  Spring,
  Spring.Collections,
  Spring.Collections.Base;

type

  {$REGION 'Documentation'}
  /// <summary>
  ///   Represents a doubly linked list.
  /// </summary>
  /// <typeparam name="T">
  ///   Specifies the element type of the linked list.
  /// </typeparam>
  {$ENDREGION}
  TLinkedList<T> = class(TCollectionBase<T>, ILinkedList<T>)
  private
    type
      TEnumerator = class(TEnumeratorBase<T>)
      private
        fList: TLinkedList<T>;
        fVersion: Integer;
        fNode: TLinkedListNode<T>;
        fCurrent: T;
      protected
        function GetCurrent: T; override;
      public
        constructor Create(const list: TLinkedList<T>);
        destructor Destroy; override;
        function MoveNext: Boolean; override;
        procedure Reset; override;
      end;
  private
    fOnChanged: ICollectionChangedEvent<T>;
    fFirstFree: TLinkedListNode<T>;
    fCount: Integer;
    fVersion: Integer;
    function EnsureNode(const value: T): TLinkedListNode<T>;
    procedure IncreaseVersion; inline;
    procedure InternalInsertNodeBefore(const node: TLinkedListNode<T>;
      const newNode: TLinkedListNode<T>);
    procedure InternalInsertNodeToEmptyList(const newNode: TLinkedListNode<T>);
    procedure InternalRemoveNode(const node: TLinkedListNode<T>);
    procedure InvalidateNode(const node: TLinkedListNode<T>);
    procedure ValidateNewNode(const node: TLinkedListNode<T>);
    procedure ValidateNode(const node: TLinkedListNode<T>);
  protected
    fHead: TLinkedListNode<T>;
  {$REGION 'Property Accessors'}
    function GetCount: Integer; override;
    function GetFirst: TLinkedListNode<T>;
    function GetLast: TLinkedListNode<T>;
    function GetOnChanged: ICollectionChangedEvent<T>;
  {$ENDREGION}
    procedure Changed(const item: T; action: TCollectionChangedAction); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Add(const item: T); override;

    procedure AddAfter(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;
    function AddAfter(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;
    procedure AddBefore(const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>); overload;
    function AddBefore(const node: TLinkedListNode<T>; const value: T): TLinkedListNode<T>; overload;
    procedure AddFirst(const node: TLinkedListNode<T>); overload;
    function AddFirst(const value: T): TLinkedListNode<T>; overload;
    procedure AddLast(const node: TLinkedListNode<T>); overload;
    function AddLast(const value: T): TLinkedListNode<T>; overload;

    procedure Clear; override;

    function Extract(const item: T): T; override;

    function Find(const value: T): TLinkedListNode<T>;
    function FindLast(const value: T): TLinkedListNode<T>;

    function First: T; override;
    function Last: T; override;

    function Remove(const item: T): Boolean; overload; override;
    procedure Remove(const node: TLinkedListNode<T>); reintroduce; overload;
    procedure RemoveFirst;
    procedure RemoveLast;
  end;

implementation

uses
  Spring.Collections.Events,
  Spring.ResourceStrings;


{$REGION 'TLinkedList<T>'}

constructor TLinkedList<T>.Create;
begin
  inherited Create;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

destructor TLinkedList<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLinkedList<T>.Add(const item: T);
begin
  AddLast(item);
end;

procedure TLinkedList<T>.AddAfter(const node, newNode: TLinkedListNode<T>);
begin
  ValidateNode(node);
  ValidateNewNode(newNode);
  InternalInsertNodeBefore(node.fNext, newNode);
end;

function TLinkedList<T>.AddAfter(const node: TLinkedListNode<T>;
  const value: T): TLinkedListNode<T>;
begin
  ValidateNode(node);
  Result := EnsureNode(value);
  InternalInsertNodeBefore(node.fNext, Result);
end;

procedure TLinkedList<T>.AddBefore(const node, newNode: TLinkedListNode<T>);
begin
  ValidateNode(node);
  ValidateNewNode(newNode);
  InternalInsertNodeBefore(node, newNode);
  if node = fHead then
    fHead := newNode;
end;

function TLinkedList<T>.AddBefore(const node: TLinkedListNode<T>;
  const value: T): TLinkedListNode<T>;
begin
  ValidateNode(node);
  Result := EnsureNode(value);
  InternalInsertNodeBefore(node, Result);
  if node = fHead then
    fHead := Result;
end;

procedure TLinkedList<T>.AddFirst(const node: TLinkedListNode<T>);
begin
  ValidateNewNode(node);
  if not Assigned(fHead) then
    InternalInsertNodeToEmptyList(node)
  else
  begin
    InternalInsertNodeBefore(fHead, node);
    fHead := node;
  end;
end;

function TLinkedList<T>.AddFirst(const value: T): TLinkedListNode<T>;
begin
  Result := EnsureNode(value);
  if not Assigned(fHead) then
    InternalInsertNodeToEmptyList(Result)
  else
  begin
    InternalInsertNodeBefore(fHead, Result);
    fHead := Result;
  end;
end;

procedure TLinkedList<T>.AddLast(const node: TLinkedListNode<T>);
begin
  ValidateNewNode(node);
  if not Assigned(fHead) then
    InternalInsertNodeToEmptyList(node)
  else
    InternalInsertNodeBefore(fHead, node);
end;

function TLinkedList<T>.AddLast(const value: T): TLinkedListNode<T>;
begin
  Result := EnsureNode(value);
  if not Assigned(fHead) then
    InternalInsertNodeToEmptyList(Result)
  else
    InternalInsertNodeBefore(fHead, Result);
end;

procedure TLinkedList<T>.Changed(const item: T;
  action: TCollectionChangedAction);
begin
  fOnChanged.Invoke(Self, item, action);
end;

procedure TLinkedList<T>.Clear;
var
  oldItems: array of T;
  i: Integer;
  node1, node2: TLinkedListNode<T>;
begin
  SetLength(oldItems, fCount);
  i := 0;

  node1 := fHead;
  while Assigned(node1) do
  begin
    oldItems[i] := node1.fItem;
    Inc(i);

    node2 := node1;
    node1 := node1.Next;
    node2.Free;
  end;
  fHead := nil;
  fCount := 0;

  while Assigned(fFirstFree) do
  begin
    node1 := fFirstFree;
    fFirstFree := fFirstFree.fNext;
    node1.Free;
  end;
  IncreaseVersion;

  for i := 0 to Length(oldItems) - 1 do
    Changed(oldItems[i], caRemoved);
end;

function TLinkedList<T>.EnsureNode(const value: T): TLinkedListNode<T>;
begin
  if not Assigned(fFirstFree) then
    Result := TLinkedListNode<T>.Create(value)
  else
  begin
    Result := fFirstFree;
    Result.fItem := value;
    fFirstFree := fFirstFree.fNext;
  end;
end;

function TLinkedList<T>.Extract(const item: T): T;
var
  node: TLinkedListNode<T>;
begin
  node := Find(item);
  if not Assigned(node) then
    Result := Default(T)
  else
  begin
    Result := node.fItem;
    InternalRemoveNode(node);
  end;
end;

function TLinkedList<T>.Find(const value: T): TLinkedListNode<T>;
var
  node: TLinkedListNode<T>;
  comparer: IEqualityComparer<T>;
begin
  Result := nil;
  node := fHead;
  comparer := EqualityComparer;
  if Assigned(node) then
  begin
    while not comparer.Equals(node.fItem, value) do
    begin
      node := node.fNext;
      if node = fHead then
        Exit;
    end;
    Result := node;
  end;
end;

function TLinkedList<T>.FindLast(const value: T): TLinkedListNode<T>;
var
  node1, node2: TLinkedListNode<T>;
  comparer: IEqualityComparer<T>;
begin
  if not Assigned(fHead) then
    Exit(nil);
  node1 := fHead.fPrev;
  node2 := node1;
  comparer := EqualityComparer;
  if Assigned(node2) then
  begin
    while not comparer.Equals(node2.fItem, value) do
    begin
      node2 := node2.fPrev;
      if node2 = node1 then
        Exit;
    end;
    Result := node2;
  end;
end;

function TLinkedList<T>.First: T;
begin
  if Assigned(fHead) then
    Result := fHead.fItem
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

function TLinkedList<T>.GetCount: Integer;
begin
  Result := fCount;
end;

function TLinkedList<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumerator.Create(Self);
end;

function TLinkedList<T>.GetFirst: TLinkedListNode<T>;
begin
  Result := fHead;
end;

function TLinkedList<T>.GetLast: TLinkedListNode<T>;
begin
  if Assigned(fHead) then
    Result := fHead.fPrev
  else
    Result := nil;
end;

function TLinkedList<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

{$IFOPT Q+}{$DEFINE OVERFLOW_CHECKS_ON}{$Q-}{$ENDIF}
procedure TLinkedList<T>.IncreaseVersion;
begin
  Inc(fVersion);
end;
{$IFDEF OVERFLOW_CHECKS_ON}{$Q+}{$ENDIF}

procedure TLinkedList<T>.InternalInsertNodeBefore(
  const node: TLinkedListNode<T>; const newNode: TLinkedListNode<T>);
begin
  newNode.fList := Self;
  newNode.fNext := node;
  newNode.fPrev := node.fPrev;
  node.fPrev.fNext := newNode;
  node.fPrev := newNode;
  IncreaseVersion;
  Inc(fCount);
  Changed(newNode.Value, caAdded);
end;

procedure TLinkedList<T>.InternalInsertNodeToEmptyList(
  const newNode: TLinkedListNode<T>);
begin
  newNode.fList := Self;
  newNode.fNext := newNode;
  newNode.fPrev := newNode;
  fHead := newNode;
  IncreaseVersion;
  Inc(fCount);
  Changed(newNode.Value, caAdded);
end;

procedure TLinkedList<T>.InternalRemoveNode(const node: TLinkedListNode<T>);
var
  item: T;
begin
  item := node.Value;
  if node.fNext = node then
    fHead := nil
  else
  begin
    node.fNext.fPrev := node.fPrev;
    node.fPrev.fNext := node.fNext;
    if fHead = node then
      fHead := node.fNext;
  end;
  InvalidateNode(node);
  Dec(fCount);
  IncreaseVersion;
  Changed(item, caRemoved);
end;

procedure TLinkedList<T>.InvalidateNode(const node: TLinkedListNode<T>);
begin
  node.fList := nil;
  node.fPrev := nil;
  node.fNext := fFirstFree;
  node.fItem := Default(T);
  fFirstFree := node;
end;

function TLinkedList<T>.Last: T;
begin
  if Assigned(fHead) then
    Result := fHead.fPrev.fItem
  else
    raise EInvalidOperationException.CreateRes(@SSequenceContainsNoElements);
end;

procedure TLinkedList<T>.Remove(const node: TLinkedListNode<T>);
begin
  ValidateNode(node);
  InternalRemoveNode(node);
end;

procedure TLinkedList<T>.RemoveFirst;
begin
  if not Assigned(fHead) then
    raise EInvalidOperationException.CreateRes(@SLinkedListEmpty);
  InternalRemoveNode(fHead);
end;

procedure TLinkedList<T>.RemoveLast;
begin
  if not Assigned(fHead) then
    raise EInvalidOperationException.CreateRes(@SLinkedListEmpty);
  InternalRemoveNode(fHead.fPrev);
end;

function TLinkedList<T>.Remove(const item: T): Boolean;
var
  node: TLinkedListNode<T>;
begin
  node := Find(item);
  Result := Assigned(node);
  if Result then
    InternalRemoveNode(node);
end;

procedure TLinkedList<T>.ValidateNewNode(const node: TLinkedListNode<T>);
begin
  Guard.CheckNotNull(Assigned(node), 'node');
  if Assigned(node.fList) then
    raise EInvalidOperationException.CreateRes(@SLinkedListNodeIsAttached);
end;

procedure TLinkedList<T>.ValidateNode(const node: TLinkedListNode<T>);
begin
  Guard.CheckNotNull(Assigned(node), 'node');
  if node.fList <> Pointer(Self) then
    raise EInvalidOperationException.CreateRes(@SLinkedListNodeIsAttached);
end;

{$ENDREGION}


{$REGION 'TLinkedList<T>.TEnumerator'}

constructor TLinkedList<T>.TEnumerator.Create(const list: TLinkedList<T>);
begin
  inherited Create;
  fList := list;
  fList._AddRef;
  fVersion := fList.fVersion;
  fNode := fList.fHead;
end;

destructor TLinkedList<T>.TEnumerator.Destroy;
begin
  fList._Release;
  inherited;
end;

function TLinkedList<T>.TEnumerator.GetCurrent: T;
begin
  Result := fCurrent;
end;

function TLinkedList<T>.TEnumerator.MoveNext: Boolean;
begin
  Result := False;

  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  if Assigned(fNode) then
  begin
    fCurrent := fNode.fItem;
    fNode := fNode.Next;
    Result := True;
  end;
end;

procedure TLinkedList<T>.TEnumerator.Reset;
begin
  if fVersion <> fList.fVersion then
    raise EInvalidOperationException.CreateRes(@SEnumFailedVersion);

  fCurrent := Default(T);
  fNode := fList.fHead;
end;

{$ENDREGION}


end.
