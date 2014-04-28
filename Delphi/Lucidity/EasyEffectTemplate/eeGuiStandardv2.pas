unit eeGuiStandardv2;

interface

uses
  Contnrs,
  VamLib.ZeroObject;

type
  IStandardControlHandler = interface
    ['{4EE51F43-F6EC-458F-8043-457823A5D45E}']


    // SetupControl() is called by TGuiStandard. The handler
    // should set event handling etc so that the event handler
    // is called when the user interacts with the control.
    procedure RegisterControl(const c : TObject);
    procedure DeregisterControl(const c : TObject);

    // UpdateControl() is called by TGuiStandard. When called
    // the handler should check the plugin for parameter state
    // changes and update the control if required.
    procedure UpdateControl(const c : TObject);
    procedure UpdateAllControls;
  end;

  THandlerReference = class
  public
    Name     : string;
    Handler  : IStandardControlHandler;
  end;

  TGuiStandard = class(TZeroObject)
  private
    HandlerList : TObjectList;

    function FindHandlerIndex(const Name : string):integer;
    function FindHandler(const Name : string):IStandardControlHandler;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterHandler(const HandlerName : string; const Handler : IStandardControlHandler);
    procedure RegisterControl(const HandlerName : string; const c : TObject);
    procedure DeregisterControl(const c : TObject);

    procedure UpdateControls;
  end;

implementation

uses
  SysUtils;

{ TGuiStandard }

constructor TGuiStandard.Create;
begin
  HandlerList := TObjectList.Create;
  HandlerList.OwnsObjects := true;
end;

destructor TGuiStandard.Destroy;
begin
  HandlerList.Free;
  inherited;
end;

function TGuiStandard.FindHandler(const Name: string): IStandardControlHandler;
var
  c1: Integer;
  s : string;
begin
  for c1 := 0 to HandlerList.Count-1 do
  begin
    s := (HandlerList[c1] as THandlerReference).Name;
    if SameText(s, Name) then
    begin
      result := (HandlerList[c1] as THandlerReference).Handler;
      exit; //=========================>> exit >>========>>
    end;
  end;

  //If we've made it this far, no match has been found, exit.
  result := nil;
end;

function TGuiStandard.FindHandlerIndex(const Name: string): integer;
var
  c1: Integer;
  s : string;
begin
  for c1 := 0 to HandlerList.Count-1 do
  begin
    s := (HandlerList[c1] as THandlerReference).Name;
    if SameText(s, Name)
      then exit(c1);
  end;

  //If we've made it this far, no match has been found, exit.
  result := -1;
end;


procedure TGuiStandard.RegisterHandler(const HandlerName : string; const Handler : IStandardControlHandler);
var
  Index : integer;
  hr : THandlerReference;
begin
  Index := FindHandlerIndex(HandlerName);
  if Index <> -1 then raise Exception.Create('Handler with that name already registered.');

  hr := THandlerReference.Create;
  hr.Name    := HandlerName;
  hr.Handler := Handler;

  HandlerList.Add(hr);
end;

procedure TGuiStandard.RegisterControl(const HandlerName : string; const c : TObject);
var
  h : IStandardControlHandler;
begin
  h := FindHandler(HandlerName);
  if not assigned(h)
    then raise Exception.Create('Handler with name "' + HandlerName + '" not found.');

  h.RegisterControl(c);
end;

procedure TGuiStandard.DeregisterControl(const c: TObject);
var
  c1 : integer;
begin
  for c1 := 0 to HandlerList.Count-1 do
  begin
    (HandlerList[c1] as THandlerReference).Handler.DeregisterControl(c);
  end;
end;

procedure TGuiStandard.UpdateControls;
var
  c1: Integer;
begin
  for c1 := 0 to HandlerList.Count-1 do
  begin
    (HandlerList[c1] as THandlerReference).Handler.UpdateAllControls;
  end;
end;



end.
