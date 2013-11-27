unit uLucidityClock;

interface

uses
   Generics.Collections;

type
  TClockEvent = procedure(Sender : TObject; ClockID : cardinal) of object;

  TClockListener = record
    ClockID       : cardinal;
    TargetObject  : TObject;
    TargetHandler : TClockEvent;
    ListenerID    : string;
  end;

  TCustomClockListenerList = TList<TClockListener>;

  TClockListenerList = class(TCustomClockListenerList)
  public
    function IndexOf(const ListenerID : string; const TargetObject : TObject):integer; overload;
  end;

  TLucidityVoiceClockManager = class
  private
  protected
    ClockListeners : TClockListenerList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddListener(const ClockID : cardinal; const TargetObject : TObject; const TargetHandler : TClockEvent; const ListenerID : string);
    procedure RemoveListener(const ListenerID : string; const TargetObject : TObject);

    procedure SendClockEvent(const ClockID : cardinal);
  end;








implementation

uses
  SysUtils,
  uConstants,
  eeDsp;


{ TClockListenerList }

function TClockListenerList.IndexOf(const ListenerID: string; const TargetObject: TObject): integer;
var
  c1: Integer;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if (Items[c1].ListenerID = ListenerID) and (Items[c1].TargetObject = TargetObject) then
    begin
      exit(c1);
    end;
  end;

  //If we've made it this far, we haven't found a match
  result := -1;
end;


{ TLucidityVoiceClockManager }

constructor TLucidityVoiceClockManager.Create;
begin
  ClockListeners := TClockListenerList.Create;

end;

destructor TLucidityVoiceClockManager.Destroy;
begin
  ClockListeners.Free;
  inherited;
end;

procedure TLucidityVoiceClockManager.AddListener(const ClockID: cardinal; const TargetObject: TObject; const TargetHandler: TClockEvent; const ListenerID : string);
var
  Index : integer;
  aListener: TClockListener;
begin
  Index :=  ClockListeners.IndexOf(ListenerID, TargetObject);
  if Index <> -1 then ClockListeners.Delete(Index);

  aListener.ClockID       := ClockID;
  aListener.TargetObject  := TargetObject;
  aListener.TargetHandler := TargetHandler;
  aListener.ListenerID    := ListenerID;
  ClockListeners.Add(aListener);
end;

procedure TLucidityVoiceClockManager.RemoveListener(const ListenerID: string; const TargetObject: TObject);
var
  Index : integer;
begin
  Index :=  ClockListeners.IndexOf(ListenerID, TargetObject);
  if Index <> -1 then ClockListeners.Delete(Index);
end;

procedure TLucidityVoiceClockManager.SendClockEvent(const ClockID: cardinal);
var
  c1: Integer;
begin
  for c1 := 0 to ClockListeners.Count-1 do
  begin
    if ClockListeners[c1].ClockID = ClockID then
    begin
      ClockListeners[c1].TargetHandler(self, ClockID);
    end;
  end;
end;


end.
