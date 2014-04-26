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
    procedure UpdateControl(const c : TObject);
    procedure SetupControl(const c : TObject);

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_KnobPosChanged(Sender: TObject);
    procedure Handle_ModAmountChanged(Sender: TObject);
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation



{ TKnobHandler }

constructor TKnobHandler.Create(const aPlugin : TeePlugin);
begin
  Plugin := aPlugin;
end;

destructor TKnobHandler.Destroy;
begin

  inherited;
end;

procedure TKnobHandler.SetupControl(const c: TObject);
begin

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
