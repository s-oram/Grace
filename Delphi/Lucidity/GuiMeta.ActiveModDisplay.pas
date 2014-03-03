unit GuiMeta.ActiveModDisplay;

interface

uses
  eePlugin,
  Controls,
  VamLib.ZeroObject;

type
  TModDisplayDetector = class(TZeroObject)
  private
    Plugin : TeePlugin;
    FCurrentMouseOverControl: TControl;
    procedure SetCurrentMouseOverControl(const Value: TControl);
  protected
    property CurrentMouseOverControl : TControl read FCurrentMouseOverControl write SetCurrentMouseOverControl;

    procedure Handle_OnControlEnterMsg(Data : Pointer);
    procedure Handle_OnControlLeaveMsg(Data : Pointer);

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  eeVstParameterEx,
  VamKnob,
  uConstants;

{ TModDisplayDetector }

constructor TModDisplayDetector.Create(aPlugin : TeePlugin);
begin
  Plugin := aPlugin;
end;

destructor TModDisplayDetector.Destroy;
begin

  inherited;
end;

procedure TModDisplayDetector.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  if MsgID = TLucidMsgID.OnControlEnter then self.Handle_OnControlEnterMsg(Data);
  if MsgID = TLucidMsgID.OnControlLeave then self.Handle_OnControlLeaveMsg(Data);
end;

procedure TModDisplayDetector.Handle_OnControlEnterMsg(Data: Pointer);
var
  c : TControl;
begin
  if TObject(Data^).InheritsFrom(TControl) then
  begin
    c := TControl(Data^);
    CurrentMouseOverControl := c;
  end;
end;

procedure TModDisplayDetector.Handle_OnControlLeaveMsg(Data: Pointer);
var
  c : TControl;
begin
  if TObject(Data^).InheritsFrom(TControl) then
  begin
    c := TControl(Data^);

    if c = CurrentMouseOverControl
      then CurrentMouseOverControl := nil;
  end;
end;

procedure TModDisplayDetector.SetCurrentMouseOverControl(const Value: TControl);
var
  ActiveModParIndex : integer;
  k : TVamKnob;
  Par : TVstParameterEx;
begin
  FCurrentMouseOverControl := Value;

  if FCurrentMouseOverControl = nil then
  begin
    ActiveModParIndex := -1;
    Plugin.Globals.MotherShip.SendMessage(TLucidMsgID.ActiveModParIndexChanged, @ActiveModParIndex);
  end else
  if FCurrentMouseOverControl.InheritsFrom(TVamKnob) then
  begin
    k := FCurrentMouseOverControl as TVamKnob;

    Par := Plugin.Globals.VstParameters.Parameter[k.ParameterIndex] as TVstParameterEx;

    if Par.HasModLink
      then ActiveModParIndex := Par.ModLinkIndex
      else ActiveModParIndex := -1;

    Plugin.Globals.MotherShip.SendMessage(TLucidMsgID.ActiveModParIndexChanged, @ActiveModParIndex);
  end;
end;



end.
