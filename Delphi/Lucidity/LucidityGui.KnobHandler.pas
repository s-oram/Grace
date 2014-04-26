unit LucidityGui.KnobHandler;

interface

uses
  Controls,
  Classes,
  eePlugin,
  VamLib.ZeroObject,
  eeGuiStandardv2;

type
  TKnobHandler = class(TRefCountedZeroObject, IStandardControlHandler)
  private
  protected
    Plugin : TeePlugin;
    CurrentModSlot : integer;
    procedure UpdateControl(const c : TObject);
    procedure SetupControl(const c : TObject);

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_KnobPosChanged(Sender: TObject);
    procedure Handle_ModAmountChanged(Sender: TObject);

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  VamKnob,
  uConstants,
  uGuiUtils;

{ TKnobHandler }

constructor TKnobHandler.Create(const aPlugin : TeePlugin);
begin
  Plugin := aPlugin;
end;

destructor TKnobHandler.Destroy;
begin

  inherited;
end;

procedure TKnobHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  inherited;

  if MsgID = TLucidMsgID.ModSlotChanged then
  begin
    CurrentModSlot := Plugin.Globals.SelectedModSlot;
  end;
end;



procedure TKnobHandler.SetupControl(const c: TObject);
var
  Knob : TVamKnob;
begin
  assert(c is TVamKnob);
  Knob := c as TVamKnob;

  Knob.OnMouseEnter       := Handle_MouseEnter;
  Knob.OnMouseLeave       := Handle_MouseLeave;
  Knob.OnMouseDown        := Handle_MouseDown;
  Knob.OnMouseUp          := Handle_MouseUp;
  Knob.OnKnobPosChanged   := Handle_KnobPosChanged;
  Knob.OnModAmountChanged := Handle_ModAmountChanged;
end;

procedure TKnobHandler.UpdateControl(const c: TObject);
begin

end;

procedure TKnobHandler.Handle_ModAmountChanged(Sender: TObject);
begin

end;

procedure TKnobHandler.Handle_MouseEnter(Sender: TObject);
begin

end;

procedure TKnobHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TKnobHandler.Handle_KnobPosChanged(Sender: TObject);
begin

end;

procedure TKnobHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TKnobHandler.Handle_MouseLeave(Sender: TObject);
begin

end;





end.
