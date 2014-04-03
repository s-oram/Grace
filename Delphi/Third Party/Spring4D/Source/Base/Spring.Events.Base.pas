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

unit Spring.Events.Base;

{$I Spring.inc}

interface

uses
  Classes,
  Generics.Collections,
  Spring;

type
  ///	<summary>
  ///	  Base class for multicast event implementation
  ///	</summary>
  TEventBase = class(TInterfacedObject, IEvent)
  private
    fEnabled: Boolean;
    fHandlers: TList<TMethod>;
    fOnChanged: TNotifyEvent;

    {$REGION 'Property Accessors'}
    function GetCount: Integer;
    function GetEnabled: Boolean;
    function GetInvoke: TMethod;
    function GetIsEmpty: Boolean;
    function GetOnChanged: TNotifyEvent;
    procedure SetEnabled(const value: Boolean);
    procedure SetOnChanged(const value: TNotifyEvent);
    {$ENDREGION}
  protected
    fInvoke: TMethod;
    procedure Notify(Sender: TObject; const Item: TMethod;
      Action: TCollectionNotification); virtual;
    property Handlers: TList<TMethod> read fHandlers;
  public
    constructor Create;
    destructor Destroy; override;

    {$REGION 'IEvent Methods'}
    procedure Add(const handler: TMethod);
    procedure Remove(const handler: TMethod);
    procedure RemoveAll(instance: Pointer);
    procedure Clear;
    procedure ForEach(const action: TAction<TMethod>);
    {$ENDREGION}

    property Count: Integer read GetCount;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Invoke: TMethod read GetInvoke;
    property OnChanged: TNotifyEvent read GetOnChanged write SetOnChanged;
  end;

implementation


{$REGION 'TEventBase'}

constructor TEventBase.Create;
begin
  inherited Create;
  fEnabled := True;
  fHandlers := TList<TMethod>.Create;
  fHandlers.OnNotify := Notify;
end;

destructor TEventBase.Destroy;
begin
  fHandlers.Free;
  inherited;
end;

procedure TEventBase.Add(const handler: TMethod);
begin
  fHandlers.Add(handler);
end;

procedure TEventBase.Clear;
begin
  fHandlers.Clear;
end;

procedure TEventBase.ForEach(const action: TAction<TMethod>);
var
  i: Integer;
begin
  for i := 0 to fHandlers.Count - 1 do
    action(fHandlers[i]);
end;

function TEventBase.GetCount: Integer;
begin
  Result := fHandlers.Count;
end;

function TEventBase.GetEnabled: Boolean;
begin
  Result := fEnabled;
end;

function TEventBase.GetInvoke: TMethod;
begin
  Result := fInvoke;
end;

function TEventBase.GetIsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TEventBase.GetOnChanged: TNotifyEvent;
begin
  Result := fOnChanged;
end;

procedure TEventBase.Notify(Sender: TObject; const Item: TMethod;
  Action: TCollectionNotification);
begin
  if Assigned(fOnChanged) then
    fOnChanged(Self);
end;

procedure TEventBase.Remove(const handler: TMethod);
begin
  fHandlers.Remove(handler);
end;

procedure TEventBase.RemoveAll(instance: Pointer);
var
  i: Integer;
begin
  for i := fHandlers.Count - 1 downto 0 do
    if fHandlers[i].Data = instance then
      fHandlers.Delete(i)
end;

procedure TEventBase.SetEnabled(const value: Boolean);
begin
  fEnabled := value;
end;

procedure TEventBase.SetOnChanged(const value: TNotifyEvent);
begin
  fOnChanged := value;
end;

{$ENDREGION}


end.
