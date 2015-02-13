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
  TCollectionChangedEvent<T>(fInvoke) := InternalInvoke;
end;

procedure TCollectionChangedEventImpl<T>.Add(
  handler: TCollectionChangedEvent<T>);
begin
  inherited Add(TMethodPointer(handler));
end;

procedure TCollectionChangedEventImpl<T>.ForEach(
  const action: TAction<TCollectionChangedEvent<T>>);
var
  handler: TMethodPointer;
begin
  for handler in Handlers do
    action(TCollectionChangedEvent<T>(handler));
end;

function TCollectionChangedEventImpl<T>.GetInvoke: TCollectionChangedEvent<T>;
begin
  Result := TCollectionChangedEvent<T>(inherited Invoke);
end;

procedure TCollectionChangedEventImpl<T>.InternalInvoke(Sender: TObject;
  const Item: T; Action: TCollectionChangedAction);
var
  handler: TMethodPointer;
begin
  if Enabled then
    for handler in Handlers do
      TCollectionChangedEvent<T>(handler)(Sender, Item, Action);
end;

procedure TCollectionChangedEventImpl<T>.Remove(
  handler: TCollectionChangedEvent<T>);
begin
  inherited Remove(TMethodPointer(handler));
end;

{$ENDREGION}


end.
