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

unit Spring.Collections.Events;

interface

uses
  Generics.Collections,
  Spring,
  Spring.Collections,
  Spring.Events.Base;

type
  TCollectionChangedEventImpl<T> = class(TEventBase, ICollectionChangedEvent<T>)
  private
    function GetInvoke: TCollectionChangedEvent<T>;
    procedure Add(handler: TCollectionChangedEvent<T>);
    procedure Remove(handler: TCollectionChangedEvent<T>);
    procedure ForEach(const action: TAction<TCollectionChangedEvent<T>>);

    procedure InternalInvoke(Sender: TObject; const Item: T;
      Action: TCollectionChangedAction);
  public
    constructor Create;
  end;

implementation


{$REGION 'TCollectionChangedEventImpl<T>'}

constructor TCollectionChangedEventImpl<T>.Create;
begin
  inherited;
  fInvoke.Code := @TCollectionChangedEventImpl<T>.InternalInvoke;
  fInvoke.Data := Self;
end;

procedure TCollectionChangedEventImpl<T>.Add(
  handler: TCollectionChangedEvent<T>);
begin
  inherited Add(TMethod(handler));
end;

procedure TCollectionChangedEventImpl<T>.ForEach(
  const action: TAction<TCollectionChangedEvent<T>>);
var
  i: Integer;
begin
  for i := 0 to Handlers.Count - 1 do
    action(TCollectionChangedEvent<T>(Handlers.Items[i]));
end;

function TCollectionChangedEventImpl<T>.GetInvoke: TCollectionChangedEvent<T>;
begin
  Result := TCollectionChangedEvent<T>(inherited Invoke);
end;

procedure TCollectionChangedEventImpl<T>.InternalInvoke(Sender: TObject;
  const Item: T; Action: TCollectionChangedAction);
var
  i: Integer;
begin
  if Enabled then
    for i := 0 to Handlers.Count - 1 do
      TCollectionChangedEvent<T>(Handlers.Items[i])(Sender, Item, Action);
end;

procedure TCollectionChangedEventImpl<T>.Remove(
  handler: TCollectionChangedEvent<T>);
begin
  inherited Remove(TMethod(handler));
end;

{$ENDREGION}


end.
