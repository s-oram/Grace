unit eeRedFoxDropFileTarget;

interface

{
  TODO: instead of explicit references to particular components, I wonder if an interface could be used instead.
}

uses
  Controls,
  Classes, Types, Contnrs,
  WinApi.ActiveX,
  DragDrop, DropTarget,
  DragDropFile,
  DragDropText,
  eeOleDropHandler,
  RedFoxContainer,
  eeTypes,
  VamVisibleControl;

const
  DropEffectNone   = WinApi.ActiveX.DROPEFFECT_NONE;
  DropEffectCopy   = WinApi.ActiveX.DROPEFFECT_COPY;
  DropEffectMove   = WinApi.ActiveX.DROPEFFECT_MOVE;
  DropEffectLink   = WinApi.ActiveX.DROPEFFECT_LINK;
  DropEffectScroll = WinApi.ActiveX.DROPEFFECT_SCROLL;

type
  TRedFoxDropFileTarget = class(TPureInterfacedObject, IVamDragData)
  private
    fCurrentDragTarget : TControl;
    function GetText  : string;
    function GetFiles : TStringList;
  protected
    RedFoxContainer : TRedFoxContainer;

    //DropFileTargetProxy : TDropFileTarget;
    DropTargetProxy : TMutantDropTarget;

    Targets : TObjectList;
    fFiles  : TStringList;

    procedure ProxyDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
    procedure ProxyDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
    procedure ProxyEndAsyncTransfer(Sender: TObject);
    procedure ProxyEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
    procedure ProxyGetDropEffect(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
    procedure ProxyLeave(Sender: TObject);
    procedure ProxyStartAsyncTransfer(Sender: TObject);

    function GetControlAt(aPoint : TPoint):TControl;

    property CurrentDragTarget: TControl read fCurrentDragTarget;
    procedure UpdateDragTarget(NewTarget : TControl; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
    procedure ClearDragTarget;

    function CalcRelativeDragPoint(aPoint:TPoint; TargetControl : TControl):TPoint;
  public
    constructor Create(aTargetContainer:TRedFoxContainer);
    destructor Destroy; override;

    procedure RegisterTarget(Target:TControl);
    procedure UnregisterTarget(Target:TControl);

    property Files : TStringList read GetFiles;
    property Text  : string      read GetText;
  end;

implementation

uses
  Dialogs,
  RedFox,
  SysUtils;

{ TRedFoxDropFileTarget }

constructor TRedFoxDropFileTarget.Create(aTargetContainer:TRedFoxContainer);
begin
  RedFoxContainer := aTargetContainer;

  DropTargetProxy := TMutantDropTarget.Create(nil);

  DropTargetProxy.Target := aTargetContainer;

  DropTargetProxy.OnDragOver           := self.ProxyDragOver;
  DropTargetProxy.OnDrop               := self.ProxyDrop;
  DropTargetProxy.OnEndAsyncTransfer   := self.ProxyEndAsyncTransfer;
  DropTargetProxy.OnEnter              := self.ProxyEnter;
  DropTargetProxy.OnGetDropEffect      := self.ProxyGetDropEffect;
  DropTargetProxy.OnLeave              := self.ProxyLeave;
  DropTargetProxy.OnStartAsyncTransfer := self.ProxyStartAsyncTransfer;

  DropTargetProxy.GetDataOnEnter := true;









  Targets := TObjectList.Create;
  Targets.OwnsObjects := false;

  fFiles := TStringList.Create;

  fCurrentDragTarget := nil;
end;

destructor TRedFoxDropFileTarget.Destroy;
begin
  fFiles.Free;
  DropTargetProxy.Free;
  Targets.Free;
  inherited;
end;

procedure TRedFoxDropFileTarget.UpdateDragTarget(NewTarget: TControl; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  RelativePoint : TPoint;
  CurrentTargetIntF : IVamVisibleControl;
  NewTargetIntF     : IVamVisibleControl;
begin
  if (fCurrentDragTarget <> nil) and (Supports(fCurrentDragTarget,IVamVisibleControl, CurrentTargetIntF)) then
  begin
    CurrentTargetIntF.OleDragLeave(self);
  end;

  if (NewTarget <> nil) and (Supports(NewTarget,IVamVisibleControl, NewTargetIntF)) then
  begin
    RelativePoint := CalcRelativeDragPoint(aPoint, NewTarget);
    NewTargetIntF.OleDragEnter(self, ShiftState, RelativePoint, Effect, self);
  end;

  fCurrentDragTarget := NewTarget;
end;

function TRedFoxDropFileTarget.CalcRelativeDragPoint(aPoint: TPoint; TargetControl : TControl): TPoint;
var
  TempPoint : TPoint;
begin
  if assigned(DropTargetProxy.Target)
    then TempPoint := DropTargetProxy.Target.ClientToScreen(aPoint)
    else TempPoint := aPoint;

  if assigned(TargetControl)
    then result := TargetControl.ScreenToClient(TempPoint)
    else result := TempPoint;
end;

procedure TRedFoxDropFileTarget.ClearDragTarget;
var
  CurrentTargetIntF : IVamVisibleControl;
begin
  if (fCurrentDragTarget <> nil) and (Supports(fCurrentDragTarget, IVamVisibleControl, CurrentTargetIntF)) then
  begin
    CurrentTargetIntF.OleDragLeave(self);
  end;
end;


function TRedFoxDropFileTarget.GetControlAt(aPoint: TPoint): TControl;
var
  c1: Integer;
  Control : TControl;
  ControlIntF : IRedFoxVisibleControl;
begin
  result := nil;
  for c1 := 0 to Targets.Count-1 do
  begin
    Control := Targets[c1] as TControl;

    if Supports(Control, IRedFoxVisibleControl, ControlIntF) then
    begin
      if (ControlIntF.GetIsShowing) and (InRect(aPoint, ControlIntF.GetAbsoluteBoundsRect)) then
      begin
        result := Control;
        exit; //=================>>exit>>=========>>
      end;
    end;
  end;
end;

function TRedFoxDropFileTarget.GetFiles: TStringList;
begin
  result := fFiles;
end;

function TRedFoxDropFileTarget.GetText: string;
begin
  result := DropTargetProxy.Text;
end;

procedure TRedFoxDropFileTarget.ProxyEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  c1 : integer;
  Control : TControl;
begin
  //Store files in a string list for later use...
  fFiles.Clear;
  for c1 := 0 to DropTargetProxy.Files.Count-1 do
  begin
    fFiles.Add(DropTargetProxy.Files[c1]);
  end;

  //Check to see if any controls were entered.
  Control := GetControlAt(aPoint);
  if Control <> CurrentDragTarget then UpdateDragTarget(Control, ShiftState, aPoint, Effect);
end;

procedure TRedFoxDropFileTarget.ProxyDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  c1 : integer;
  Control : TControl;
  ControlIntF : IVamVisibleControl;
  RelativePoint : TPoint;
begin
  //check if the files list needs to be updated.
  if fFiles.Count <> DropTargetProxy.Files.Count then
  begin
    fFiles.Clear;
    for c1 := 0 to DropTargetProxy.Files.Count-1 do
    begin
      fFiles.Add(DropTargetProxy.Files[c1]);
    end;
  end;

  //perform drag over operation
  Control := GetControlAt(aPoint);
  if Control <> CurrentDragTarget then UpdateDragTarget(Control, ShiftState, aPoint, Effect);
  if (Control <> nil) and (Supports(Control, IVamVisibleControl, ControlIntF)) then
  begin
    RelativePoint := CalcRelativeDragPoint(aPoint, Control);
    ControlIntF.OleDragOver(Self, ShiftState, RelativePoint, Effect, Self);
  end;
end;

procedure TRedFoxDropFileTarget.ProxyDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
var
  c1 : integer;
  Control : TControl;
  ControlIntF : IVamVisibleControl;
  RelativePoint : TPoint;
begin
    //check if the files list needs to be updated.
  if fFiles.Count <> DropTargetProxy.Files.Count then
  begin
    fFiles.Clear;
    for c1 := 0 to DropTargetProxy.Files.Count-1 do
    begin
      fFiles.Add(DropTargetProxy.Files[c1]);
    end;
  end;

  //perform drag over operation
  Control := GetControlAt(aPoint);
  if Control <> CurrentDragTarget then UpdateDragTarget(Control, ShiftState, aPoint, Effect);
  if (Control <> nil) and (Supports(Control, IVamVisibleControl, ControlIntF)) then
  begin
    RelativePoint := CalcRelativeDragPoint(aPoint, Control);
    ControlIntF.OleDragDrop(Self, ShiftState, RelativePoint, Effect, Self);
  end;
end;

procedure TRedFoxDropFileTarget.ProxyLeave(Sender: TObject);
begin
  ClearDragTarget;
end;

procedure TRedFoxDropFileTarget.ProxyStartAsyncTransfer(Sender: TObject);
begin

end;

procedure TRedFoxDropFileTarget.ProxyEndAsyncTransfer(Sender: TObject);
begin

end;

procedure TRedFoxDropFileTarget.ProxyGetDropEffect(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer);
begin

end;

procedure TRedFoxDropFileTarget.RegisterTarget(Target: TControl);
begin
  if Targets.IndexOf(Target) = -1 then
  begin
    Targets.Add(Target);
  end else
  begin
    raise Exception.Create('Target already registered.');
  end;

end;

procedure TRedFoxDropFileTarget.UnregisterTarget(Target: TControl);
begin
  if Targets.IndexOf(Target) <> -1 then
  begin
    Targets.Remove(Target);
  end else
  begin
    raise Exception.Create('Target not registered.');
  end;
end;


end.
