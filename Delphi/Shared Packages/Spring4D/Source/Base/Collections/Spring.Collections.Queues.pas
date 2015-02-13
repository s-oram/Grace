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

unit Spring.Collections.Queues;

interface

uses
  Generics.Collections,
  Spring.Collections,
  Spring.Collections.Base;

type
  TQueue<T> = class(TEnumerableBase<T>, IQueue<T>)
  private
    type
      TGenericQueue = Generics.Collections.TQueue<T>;
  private
    fQueue: TGenericQueue;
    fOwnership: TOwnershipType;
    fOnChanged: ICollectionChangedEvent<T>;
    fOnNotify: TCollectionNotifyEvent<T>;
    procedure DoNotify(Sender: TObject; const Item: T;
      Action: TCollectionNotification);
    function GetOnChanged: ICollectionChangedEvent<T>;
{$IFDEF DELPHIXE_UP}
    function GetCapacity: Integer;
    procedure SetCapacity(const value: Integer);
{$ENDIF}
  protected
    function GetCount: Integer; override;
    procedure Changed(const item: T; action: TCollectionChangedAction); virtual;
  public
    constructor Create; overload; override;
    constructor Create(const values: array of T); overload;
    constructor Create(const collection: IEnumerable<T>); overload;
    constructor Create(queue: TGenericQueue; ownership: TOwnershipType); overload;
    destructor Destroy; override;

    function GetEnumerator: IEnumerator<T>; override;

    procedure Clear;
    procedure Enqueue(const item: T);
    function Dequeue: T;
    function Peek: T;
    function PeekOrDefault: T;
    function TryPeek(out item: T): Boolean;

    procedure TrimExcess;

{$IFDEF DELPHIXE_UP}
    property Capacity: Integer read GetCapacity write SetCapacity;
{$ENDIF}
    property OnChanged: ICollectionChangedEvent<T> read GetOnChanged;
  end;

implementation

uses
  Spring.Collections.Events,
  Spring.Collections.Extensions;


{$REGION 'TQueue<T>'}

constructor TQueue<T>.Create(queue: TGenericQueue; ownership: TOwnershipType);
begin
  inherited Create;
  fQueue := queue;
  fOnNotify := fQueue.OnNotify;
  fQueue.OnNotify := DoNotify;
  fOwnership := ownership;
  fOnChanged := TCollectionChangedEventImpl<T>.Create;
end;

constructor TQueue<T>.Create(const values: array of T);
var
  item: T;
begin
  Create;
  for item in values do
    Enqueue(item);
end;

constructor TQueue<T>.Create(const collection: IEnumerable<T>);
var
  item: T;
begin
  Create;
  for item in collection do
    Enqueue(item);
end;

constructor TQueue<T>.Create;
var
  queue: TGenericQueue;
begin
  queue := TGenericQueue.Create;
  Create(queue, otOwned);
end;

destructor TQueue<T>.Destroy;
begin
  if fOwnership = otOwned then
    fQueue.Free
  else
    fQueue.OnNotify := fOnNotify;

  inherited Destroy;
end;

procedure TQueue<T>.DoNotify(Sender: TObject; const Item: T;
  Action: TCollectionNotification);
begin
  Changed(Item, TCollectionChangedAction(Action));
end;

function TQueue<T>.GetEnumerator: IEnumerator<T>;
begin
  Result := TEnumeratorAdapter<T>.Create(fQueue);
end;

procedure TQueue<T>.Enqueue(const item: T);
begin
  fQueue.Enqueue(item);
end;

function TQueue<T>.Dequeue: T;
begin
  Result := fQueue.Dequeue;
end;

procedure TQueue<T>.Clear;
begin
  fQueue.Clear;
end;

function TQueue<T>.Peek: T;
begin
  Result := fQueue.Peek;
end;

function TQueue<T>.PeekOrDefault: T;
begin
  if fQueue.Count > 0 then
    Result := fQueue.Peek
  else
    Result := Default(T);
end;

{$IFDEF DELPHIXE_UP}
procedure TQueue<T>.SetCapacity(const value: Integer);
begin
  fQueue.Capacity := value;
end;
{$ENDIF}

procedure TQueue<T>.TrimExcess;
begin
  fQueue.TrimExcess;
end;

function TQueue<T>.TryPeek(out item: T): Boolean;
begin
  Result := fQueue.Count > 0;
  if Result then
    item := fQueue.Peek
  else
    item := Default(T);
end;

{$IFDEF DELPHIXE_UP}
function TQueue<T>.GetCapacity: Integer;
begin
  Result := fQueue.Capacity;
end;
{$ENDIF}

function TQueue<T>.GetCount: Integer;
begin
  Result := fQueue.Count;
end;

function TQueue<T>.GetOnChanged: ICollectionChangedEvent<T>;
begin
  Result := fOnChanged;
end;

procedure TQueue<T>.Changed(const item: T; action: TCollectionChangedAction);
begin
  if fOnChanged.IsInvokable then
    fOnChanged.Invoke(Self, item, action);
end;

{$ENDREGION}


end.
