unit eeMultiCast;

interface

uses
  Classes;

type
  PMethod = ^TMethod;

  TMultiCastEvent = class
  private
    fMethodsList: TList;
    function GetCount: Integer;
    function GetMethod(const aIndex: Integer): TMethod;
  protected
    procedure Add(const aMethod: TMethod);
    procedure Remove(const aMethod: TMethod);

    property Method[const aIndex: Integer]: TMethod read GetMethod;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property Count: Integer read GetCount;
  end;


  TMultiCastNotifyEvent = class(TMultiCastEvent)
  public
    procedure Trigger(Sender:TObject);
    procedure AddEventHandler(EventHandler:TNotifyEvent);
    procedure RemoveEventHandler(EventHandler:TNotifyEvent);
  end;




implementation

{ TMultiCastEvent }

constructor TMultiCastEvent.Create;
begin
  fMethodsList := TList.Create;
end;

destructor TMultiCastEvent.Destroy;
begin
  fMethodsList.Free;
  inherited;
end;

function TMultiCastEvent.GetCount: Integer;
begin
  result := fMethodsList.Count;
end;

function TMultiCastEvent.GetMethod(const aIndex: Integer): TMethod;
begin
  result := TMethod(fMethodsList[aIndex]^);
end;

procedure TMultiCastEvent.Add(const aMethod: TMethod);
var
  c1:integer;
  handler: PMethod;
begin
  // Check to ensure that the specified method is not already attached
  for c1 := 0 to fMethodsList.Count-1 do
  begin
    handler := fMethodsList[c1];
    if (aMethod.Code = handler.Code) and (aMethod.Data = handler.Data) then exit;
  end;

  // Not already attached - create a new TMethod reference and copy the
  //  details from the specific method, then add to our list of handlers
  handler := New(PMethod);
  handler.Code := aMethod.Code;
  handler.Data := aMethod.Data;
  fMethodsList.Add(handler);
end;

procedure TMultiCastEvent.Remove(const aMethod: TMethod);
var
  c1: Integer;
  handler: PMethod;
begin
  for c1 := 0 to fMethodsList.Count-1 do
  begin
    handler := fMethodsList[c1];

    if (aMethod.Code = handler.Code) and (aMethod.Data = handler.Data) then
    begin
      Dispose(handler);
      fMethodsList.Delete(c1);

      // Only one reference to any method can be attached to any one event, so
      //  once we have found and removed the method there is no need to check the
      //  remaining method references.
      BREAK;
    end;
  end;
end;

{ TMultiCastNotifyEvent }

procedure TMultiCastNotifyEvent.AddEventHandler(EventHandler: TNotifyEvent);
begin
  self.Add(TMethod(EventHandler));
end;

procedure TMultiCastNotifyEvent.RemoveEventHandler(EventHandler: TNotifyEvent);
begin
  self.Remove(TMethod(EventHandler));
end;

procedure TMultiCastNotifyEvent.Trigger(Sender: TObject);
var
  c1: Integer;
  MyEvent : TNotifyEvent;
begin
  for c1 := 0 to Count-1 do
  begin
    //Cast the event handler reference to a variable of our event type.
    MyEvent := TNotifyEvent(Method[c1]);
    //Call the event handler.
    MyEvent(Sender);
  end;
end;

end.
