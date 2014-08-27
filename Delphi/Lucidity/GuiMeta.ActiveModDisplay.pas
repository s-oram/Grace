unit GuiMeta.ActiveModDisplay;

interface

uses
  eePlugin,
  Controls,
  VamLib.ZeroObject;

type
  TActiveParameterDetector = class(TZeroObject)
  private
    CurrentFocus_ParName : string; // this is a parameter name.
    Plugin : TeePlugin;
  protected
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IZeroMessageData);  override;
  public
    constructor Create(aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  VamKnob,
  uConstants;

{ TModDisplayDetector }

constructor TActiveParameterDetector.Create(aPlugin : TeePlugin);
begin
  Plugin := aPlugin;
  CurrentFocus_ParName := '';
end;

destructor TActiveParameterDetector.Destroy;
begin

  inherited;
end;

procedure TActiveParameterDetector.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IZeroMessageData);
var
  s : string;
begin
  if MsgID = TLucidMsgID.OnParControlEnter then
  begin
    s := string(Data^);
    if s <> CurrentFocus_ParName then
    begin
      CurrentFocus_ParName := s;
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.OnActiveParameterChanged, @CurrentFocus_ParName, nil);
    end;
  end;

  if MsgID = TLucidMsgID.OnParControlLeave then
  begin
    s := string(Data^);
    if s = CurrentFocus_ParName then
    begin
      CurrentFocus_ParName := '';
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.OnActiveParameterChanged, @CurrentFocus_ParName, nil);
    end;
  end;
end;





end.
