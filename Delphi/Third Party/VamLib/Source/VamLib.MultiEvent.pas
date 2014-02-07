unit VamLib.MultiEvent;

interface

uses
  System.Classes,
  Vcl.Controls,
  VamLib.Collections.Lists;

  //  TODO: It's currently possible to add the same event watcher to a MultiEvent
  //  multiple times. The MultiEvent class should be changed so duplicate
  //  events are ignored. Theres no point in calling an event handler
  //  twice. It will double the workload.

type
  TCustomMultiEvent = class
  private
    List : TSimplePointerList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
  end;

  TNotifyMultiEvent = class(TCustomMultiEvent)
  public
    procedure Add(Watcher:TNotifyEvent);
    procedure Remove(Watcher:TNotifyEvent);
    procedure TriggerAll(Sender : TObject);
  end;

  TMouseMultiEvent = class(TCustomMultiEvent)
  public
    procedure Add(Watcher:TMouseEvent);
    procedure Remove(Watcher:TMouseEvent);
    procedure TriggerAll(Sender : TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

  TMouseMoveMultiEvent = class(TCustomMultiEvent)
  public
    procedure Add(Watcher:TMouseMoveEvent);
    procedure Remove(Watcher:TMouseMoveEvent);
    procedure TriggerAll(Sender : TObject; Shift: TShiftState; X, Y: Integer);
  end;

implementation

type
  PMethod = ^TMethod;

{ TCustomMultiEvent }

constructor TCustomMultiEvent.Create;
begin
  List := TSimplePointerList.Create;
  List.GrowBy := 1;
end;

destructor TCustomMultiEvent.Destroy;
begin
  Clear;
  List.Free;
  inherited;
end;

procedure TCustomMultiEvent.Clear;
var
  c1:integer;
  h:PMethod;
begin
  for c1 := List.Count - 1 downto 0 do
  begin
    h := List.Items[c1];
    Dispose(h);
    List.Delete(c1);
  end;
end;



{ TNotifyMutliEvent }

procedure TNotifyMultiEvent.Add(Watcher: TNotifyEvent);
var
  h:PMethod;
begin
  h := New(PMethod);
  h^.Code := TMethod(Watcher).Code;
  h^.Data := TMethod(Watcher).Data;
  List.Add(h);
end;

procedure TNotifyMultiEvent.Remove(Watcher: TNotifyEvent);
var
  c1:integer;
  Code,Data:Pointer;
  h:PMethod;
begin
  for c1 := List.Count - 1 downto 0 do
  begin
    Code := TMethod(Watcher).Code;
    Data := TMethod(Watcher).Data;

    h := List.Items[c1];

    if (Code = h^.Code) and (Data = h^.Data) then
    begin
      Dispose(h);
      List.Delete(c1);
    end;

  end;
end;

procedure TNotifyMultiEvent.TriggerAll(Sender: TObject);
var
  c1:integer;
  h:PMethod;
  ev:TNotifyEvent;
begin
  for c1 := 0 to List.Count - 1 do
  begin
    h := List.Items[c1];
    ev := TNotifyEvent(h^);
    ev(Sender);
  end;
end;

{ TMouseMultiEvent }

procedure TMouseMultiEvent.Add(Watcher: TMouseEvent);
var
  c1:integer;
  Code,Data:Pointer;
  h:PMethod;
begin
  for c1 := List.Count - 1 downto 0 do
  begin
    Code := TMethod(Watcher).Code;
    Data := TMethod(Watcher).Data;

    h := List.Items[c1];

    if (Code = h^.Code) and (Data = h^.Data) then
    begin
      Dispose(h);
      List.Delete(c1);
    end;

  end;
end;

procedure TMouseMultiEvent.Remove(Watcher: TMouseEvent);
var
  c1:integer;
  Code,Data:Pointer;
  h:PMethod;
begin
  for c1 := List.Count - 1 downto 0 do
  begin
    Code := TMethod(Watcher).Code;
    Data := TMethod(Watcher).Data;

    h := List.Items[c1];

    if (Code = h^.Code) and (Data = h^.Data) then
    begin
      Dispose(h);
      List.Delete(c1);
    end;

  end;
end;

procedure TMouseMultiEvent.TriggerAll(Sender: TObject; Button: TMouseButton;  Shift: TShiftState; X, Y: Integer);
var
  c1:integer;
  h:PMethod;
  ev:TMouseEvent;
begin
  for c1 := 0 to List.Count - 1 do
  begin
    h := List.Items[c1];
    ev := TMouseEvent(h^);
    ev(Sender, Button, Shift, X, Y);
  end;
end;



{ TMouseMoveMultiEvent }

procedure TMouseMoveMultiEvent.Add(Watcher: TMouseMoveEvent);
var
  c1:integer;
  Code,Data:Pointer;
  h:PMethod;
begin
  for c1 := List.Count - 1 downto 0 do
  begin
    Code := TMethod(Watcher).Code;
    Data := TMethod(Watcher).Data;

    h := List.Items[c1];

    if (Code = h^.Code) and (Data = h^.Data) then
    begin
      Dispose(h);
      List.Delete(c1);
    end;

  end;
end;


procedure TMouseMoveMultiEvent.Remove(Watcher: TMouseMoveEvent);
var
  c1:integer;
  Code,Data:Pointer;
  h:PMethod;
begin
  for c1 := List.Count - 1 downto 0 do
  begin
    Code := TMethod(Watcher).Code;
    Data := TMethod(Watcher).Data;

    h := List.Items[c1];

    if (Code = h^.Code) and (Data = h^.Data) then
    begin
      Dispose(h);
      List.Delete(c1);
    end;

  end;
end;

procedure TMouseMoveMultiEvent.TriggerAll(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  c1:integer;
  h:PMethod;
  ev:TMouseMoveEvent;
begin
  for c1 := 0 to List.Count - 1 do
  begin
    h := List.Items[c1];
    ev := TMouseMoveEvent(h^);
    ev(Sender, Shift, X, Y);
  end;
end;

end.
