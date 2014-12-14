unit ee3AddOn.ControlHandler.VstPar.Knob;

interface

uses
  Contnrs,
  Controls,
  Types,
  Classes,
  DAEffect,
  DAEffectX,
  DAudioEffectX;

type
  TShowControlContextMenuEvent = procedure(Sender, Control : TObject; X, Y : integer) of object;

  TVstParKnobHandler = class
  private
    fOnShowControlContextMenu: TShowControlContextMenuEvent;
  protected
    Effect : AudioEffectX;
    ControlList : TObjectList;
  public
    constructor Create(aEffect : AudioEffectX);
    destructor Destroy; override;

    procedure RegisterControl(const c : TObject; VstParIndex : integer);
    procedure DeregisterControl(const c : TObject);
    procedure UpdateAllControls;

    procedure Handle_MouseEnter(Sender : TObject);
    procedure Handle_MouseLeave(Sender : TObject);
    procedure Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Handle_KnobPosChanged(Sender: TObject);
    procedure Handle_ModAmountChanged(Sender: TObject);

    property OnShowControlContextMenu : TShowControlContextMenuEvent read fOnShowControlContextMenu write fOnShowControlContextMenu;
  end;

implementation

uses
  SysUtils,
  VamGuiControlInterfaces;

{ TVstParKnobHandler }

constructor TVstParKnobHandler.Create(aEffect: AudioEffectX);
begin
  Effect := aEffect;

  Controllist := TObjectList.Create;
  ControlList.OwnsObjects := false;
end;

destructor TVstParKnobHandler.Destroy;
begin
  ControlList.Free;
  inherited;
end;

procedure TVstParKnobHandler.UpdateAllControls;
var
  c1: Integer;
  KnobControl : IKnobControl;
  c : TObject;
  VstParIndex : integer;
  KnobValue : single;
begin
  for c1 := ControlList.Count-1 downto 0 do
  begin
    c := ControlList[c1];
    if Supports(c, IKnobControl, KnobControl) then
    begin
      VstParIndex := KnobControl.GetParameterIndex;
      KnobValue := Effect.GetParameter(VstParIndex);
      KnobControl.SetKnobValue(KnobValue);
    end;
  end;
end;

procedure TVstParKnobHandler.RegisterControl(const c: TObject; VstParIndex : integer);
var
  KnobControl : IKnobControl;
begin
  assert(Supports(c, IKnobControl), 'Control doesn''t support IKnobControl.');

  if ControlList.IndexOf(c) = -1 then ControlList.Add(c);

  if Supports(c, IKnobControl, KnobControl) then
  begin
    KnobControl.SetParameterIndex(VstParIndex);

    KnobControl.SetOnMouseEnter(Handle_MouseEnter);
    KnobControl.SetOnMouseLeave(Handle_MouseLeave);
    KnobControl.SetOnMouseDown(Handle_MouseDown);
    KnobControl.SetOnMouseUp(Handle_MouseUp);
    KnobControl.SetOnKnobPosChanged(Handle_KnobPosChanged);
    KnobControl.SetOnModAmountChanged(Handle_ModAmountChanged);
  end;
end;

procedure TVstParKnobHandler.DeregisterControl(const c: TObject);
var
  KnobControl : IKnobControl;
begin
  ControlList.Remove(c);

  if Supports(c, IKnobControl, KnobControl) then
  begin
    KnobControl.SetOnMouseEnter(nil);
    KnobControl.SetOnMouseLeave(nil);
    KnobControl.SetOnMouseDown(nil);
    KnobControl.SetOnMouseUp(nil);
    KnobControl.SetOnKnobPosChanged(nil);
    KnobControl.SetOnModAmountChanged(nil);
  end;
end;

procedure TVstParKnobHandler.Handle_MouseEnter(Sender: TObject);
var
  KnobControl : IKnobControl;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin

  end;
end;

procedure TVstParKnobHandler.Handle_MouseLeave(Sender: TObject);
var
  KnobControl : IKnobControl;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin

  end;
end;

procedure TVstParKnobHandler.Handle_MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  KnobControl : IKnobControl;
  VstParIndex : integer;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    VstParIndex := KnobControl.GetParameterIndex;

    if (Button = TMouseButton.mbLeft) then
    begin
      Effect.BeginEdit(VstParIndex);
    end else
    if (Button = TMouseButton.mbRight) then
    begin
      if assigned(OnShowControlContextMenu) then OnShowControlContextMenu(self, Sender, X, Y);       
    end;
  end;
end;

procedure TVstParKnobHandler.Handle_MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  KnobControl : IKnobControl;
  VstParIndex : integer;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    VstParIndex := KnobControl.GetParameterIndex;

    if Button = TMouseButton.mbLeft then
    begin
      Effect.EndEdit(VstParIndex);
    end;
  end;
end;

procedure TVstParKnobHandler.Handle_KnobPosChanged(Sender: TObject);
var
  KnobControl : IKnobControl;
  VstParIndex : integer;
  KnobValue : single;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin
    VstParIndex := KnobControl.GetParameterIndex;
    KnobValue   := KnobControl.GetKnobValue;
    Effect.SetParameterAutomated(VstParIndex, KnobValue);
  end;
end;

procedure TVstParKnobHandler.Handle_ModAmountChanged(Sender: TObject);
var
  KnobControl : IKnobControl;
begin
  if Supports(Sender, IKnobControl, KnobControl) then
  begin

  end;
end;




end.
