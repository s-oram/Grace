unit eeGuiStandardv2;

interface

uses
  Contnrs,
  VamLib.ZeroObject;

type
  IStandardControlHandler = interface
    ['{4EE51F43-F6EC-458F-8043-457823A5D45E}']

    // UpdateControl() is called by TGuiStandard. When called
    // the handler should check the plugin for parameter state
    // changes and update the control if required.
    procedure UpdateControl(const c : TObject);

    // SetupControl() is called by TGuiStandard. The handler
    // should set event handling etc so that the event handler
    // is called when the user interacts with the control.
    procedure SetupControl(const c : TObject);
  end;

  TControlReference = class
  public
    Control  : TObject;
    Handler  : IStandardControlHandler;
  end;

  THandlerReference = class
  public
    Name     : string;
    Handler  : IStandardControlHandler;
  end;

  TGuiStandard = class(TZeroObject)
  private
    ControlList : TObjectList;
    HandlerList : TObjectList;

    function FindReferenceIndex(const c : TObject):integer;
    function FindHandlerIndex(const Name : string):integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterHandler(const Handler : IStandardControlHandler; const Name : string);
    procedure RegisterControl(const c : TObject; const Handler : IStandardControlHandler);
    procedure DeregisterControl(const c : TObject);

    procedure UpdateControls;
  end;

implementation

uses
  SysUtils;

{ TGuiStandard }

constructor TGuiStandard.Create;
begin
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := true;

  HandlerList := TObjectList.Create;
  HandlerList.OwnsObjects := true;
end;

destructor TGuiStandard.Destroy;
begin
  ControlList.Free;
  HandlerList.Free;
  inherited;
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

function TGuiStandard.FindReferenceIndex(const c: TObject): integer;
var
  c1: Integer;
begin
  for c1 := 0 to ControlList.Count-1 do
  begin
    if (ControlList[c1] as TControlReference).Control = c then
    begin
      exit(c1); //===============================>> exit >>========>>
    end;
  end;

  //If we've made it this far, no match has been found, exit.
  result := -1;
end;

procedure TGuiStandard.RegisterHandler(const Handler: IStandardControlHandler; const Name: string);
var
  Index : integer;
  hr : THandlerReference;
begin
  Index := FindHandlerIndex(Name);
  if Index <> -1 then raise Exception.Create('Handler with that name already registered.');

  hr := THandlerReference.Create;
  hr.Name := Name;
  hr.Handler := Handler;

  HandlerList.Add(hr);
end;

procedure TGuiStandard.RegisterControl(const c: TObject; const Handler: IStandardControlHandler);
var
  Index : integer;
  cr : TControlReference;
begin
  Index := FindReferenceIndex(c);
  if Index = -1 then
  begin
    cr := TControlReference.Create;
    ControlList.Add(cr);
  end else
  begin
    cr := ControlList[Index] as TControlReference;
  end;

  cr.Control := c;
  cr.Handler := Handler;
  Handler.SetupControl(c);
end;

procedure TGuiStandard.DeregisterControl(const c: TObject);
var
  Index : integer;
begin
  Index := FindReferenceIndex(c);

  if Index <> -1 then
  begin
    ControlList.Delete(Index);
  end;
end;

procedure TGuiStandard.UpdateControls;
var
  c1: Integer;
  cr : TControlReference;
begin
  for c1 := 0 to ControlList.Count-1 do
  begin
    cr := ControlList[c1] as TControlReference;
    cr.Handler.UpdateControl(cr.Control);
  end;
end;



end.
