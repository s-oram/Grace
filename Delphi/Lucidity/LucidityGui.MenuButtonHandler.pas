unit LucidityGui.MenuButtonHandler;

interface

uses
  Contnrs,
  Controls,
  Classes,
  eePlugin,
  VamLib.ZeroObject,
  eeGuiStandardv2;

type
  TMenuButtonHandler = class(TRefCountedZeroObject, IStandardControlHandler)
  private
  protected
    ControlList : TObjectList;

    Plugin : TeePlugin;

    procedure UpdateAllControls;

    procedure UpdateControl(const c : TObject);
    procedure RegisterControl(const c : TObject);
    procedure DeregisterControl(const c : TObject);

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer); override;
  public
    constructor Create(const aPlugin : TeePlugin);
    destructor Destroy; override;
  end;

implementation

uses
  VamTextBox;

{ TMenuButtonHandler }

constructor TMenuButtonHandler.Create(const aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;
end;

destructor TMenuButtonHandler.Destroy;
begin
  ControlList.Free;
  inherited;
end;

procedure TMenuButtonHandler.RegisterControl(const c: TObject);
begin
  if ControlList.IndexOf(c) <> -1
    then ControlList.Add(c);
end;

procedure TMenuButtonHandler.DeregisterControl(const c: TObject);
begin
  ControlList.Remove(c);
end;

procedure TMenuButtonHandler.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin
  inherited;

end;

procedure TMenuButtonHandler.UpdateAllControls;
var
  c1: Integer;
begin
  for c1 := 0 to ControlList.Count-1 do
  begin
    UpdateControl(ControlList[c1]);
  end;
end;

procedure TMenuButtonHandler.UpdateControl(const c: TObject);
begin

end;

procedure TMenuButtonHandler.Handle_MouseEnter(Sender: TObject);
begin

end;

procedure TMenuButtonHandler.Handle_MouseLeave(Sender: TObject);
begin

end;

procedure TMenuButtonHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TMenuButtonHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;


end.
