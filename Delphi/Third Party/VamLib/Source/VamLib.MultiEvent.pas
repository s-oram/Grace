unit VamLib.MultiEvent;

interface

uses
  System.Classes,
  VamLib.Collections.Lists;

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

end.
