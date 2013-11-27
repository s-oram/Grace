{
  EventList holds a list of TNotifyEvent methods. It can be used to trigger
  multiple events.


  References:

  - Simulating multicast events in Win32 Delphi
    http://delphi.about.com/library/weekly/aa051005a.htm


  by:
  Shannon Faulkner (27 June 2009)

}

unit uEventList;

interface

uses
  Classes;

type
  PMethod = ^TMethod;

  TEventList = class
  private
    function GetCount: integer;
  protected
    List:TList;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure TriggerAll(Sender:TObject);
    procedure Add(NotifyEvent:TNotifyEvent);
    procedure Remove(NotifyEvent:TNotifyEvent);

    procedure Clear;

    property Count:integer read GetCount;
  end;

implementation

{ TEventList }

constructor TEventList.Create;
begin
  List := TList.Create;
end;

destructor TEventList.Destroy;
begin
  Clear;

  List.Free;
  inherited;
end;

procedure TEventList.Add(NotifyEvent: TNotifyEvent);
var
  h:PMethod;
begin
  h := New(PMethod);

  h^.Code := TMethod(NotifyEvent).Code;
  h^.Data := TMethod(NotifyEvent).Data;

  List.Add(h);
end;

procedure TEventList.Remove(NotifyEvent: TNotifyEvent);
var
  c1:integer;
  Code,Data:Pointer;
  h:PMethod;
begin
  for c1 := List.Count - 1 downto 0 do
  begin
    Code := TMethod(NotifyEvent).Code;
    Data := TMethod(NotifyEvent).Data;

    h := List.Items[c1];

    if (Code = h^.Code) and (Data = h^.Data) then
    begin
      Dispose(h);
      List.Delete(c1);
    end;

  end;
end;



procedure TEventList.Clear;
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



function TEventList.GetCount: integer;
begin
  result := List.Count;
end;

procedure TEventList.TriggerAll(Sender:TObject);
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
