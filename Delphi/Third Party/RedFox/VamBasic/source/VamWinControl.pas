unit VamWinControl;

interface

uses
  VamLib.MultiEvent,
  VamGuiControlInterfaces,
  WinApi.Messages, WinApi.Windows, VamLayoutWizard,
  Classes, Types, Controls, Graphics,
  RedFoxWinControl, VamVisibleControl;

type
  //TVamWinControl = class(TRedFoxWinControl, IAccessible, IVamVisibleControl, IVamLayoutWizard)
  TVamWinControl = class(TRedFoxWinControl, IVamVisibleControl, IVamLayoutWizard)
  private
    fIsGrabbedByLeft, fIsGrabbedByRight : boolean;
    fOnOleDragEnter: TOleDragEvent;
    fOnOleDragDrop: TOleDragEvent;
    fOnOleDragOver: TOleDragEvent;
    fOnOleDragLeave: TOleDragLeaveEvent;
    fUpdatingCount : integer;
    fLayout: TVamLayoutWizard;
    fOnShowContextMenu: TShowContextMenuEvent;

    fChangedMultiEvent    : TNotifyMultiEvent;
    fMouseEnterMultiEvent : TNotifyMultiEvent;
    fMouseLeaveMultiEvent : TNotifyMultiEvent;
    fMouseDownMultiEvent  : TMouseMultiEvent;
    fMouseUpMultiEvent    : TMouseMultiEvent;
    fMouseMoveMultiEvent  : TMouseMoveMultiEvent;

    function GetIsControlGrabbed: boolean;
    function GetIsUpdating: boolean;

    procedure WMMouseWheel(var Message : TWMMouseWheel); message WM_MouseWheel;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;


  protected
    function GetObject : TObject;

    function GetFont: TFont; virtual;
    procedure SetFont(const Value: TFont); virtual;

    procedure MouseEnter; override;
    procedure MouseLeave; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure OleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); virtual;
    procedure OleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); virtual;
    procedure OleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData); virtual;
    procedure OleDragLeave(Sender: TObject); virtual;

    property Font : TFont read GetFont write SetFont;

    //==========================================================================================================================
    // It looks like the mouse wheel handling has been implemented by TWinControl but I've never much
    // luck getting it to work reliably. So I've re-implemented mouse-wheel handling here.
    procedure MouseWheelUp(Shift : TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure MouseWheelDown(Shift : TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); virtual;

    property OnMouseWheelUp;
    property OnMouseWheelDown;

    // NOTE: For future reference, this stack overflow Q/A describes a 'generic' method for
    // capturing mouse wheel events for any control. Haven't read the answer in great detail yet.
    // but it may proove useful if the mouse wheel handling code here proves to be deficient
    // for some reason.
    //==========================================================================================================================


    procedure SetOnMouseEnter(Handler:TNotifyEvent);
    procedure SetOnMouseLeave(Handler:TNotifyEvent);
    procedure SetOnMouseDown(Handler:TMouseEvent);
    procedure SetOnMouseUp(Handler:TMouseEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // AlignToParent() resizes the control to the same size as the containing parent control.
    procedure AlignToParent(const UsingMargins : boolean = false);

    // Resize calls SetBounds() then fires the OnResize() event.
    procedure Resize(aLeft, aTop, aWidth, aHeight: Integer); reintroduce;

    // BeginUpdate() / EndUpdate() calls can be nested. A BeginUpdate() call must
    // always be followed by a EndUpdate() call.
    procedure BeginUpdate;
    procedure EndUpdate;
    property IsUpdating : boolean read GetIsUpdating;

    property Layout : TVamLayoutWizard read fLayout implements IVamLayoutWizard;

    property ChangedMultiEvent    : TNotifyMultiEvent    read fChangedMultiEvent; //Changed is used by descend controls to indicate a control value has changed.
    property MouseEnterMultiEvent : TNotifyMultiEvent    read fMouseEnterMultiEvent;
    property MouseLeaveMultiEvent : TNotifyMultiEvent    read fMouseLeaveMultiEvent;
    property MouseDownMultiEvent  : TMouseMultiEvent     read fMouseDownMultiEvent;
    property MouseUpMultiEvent    : TMouseMultiEvent     read fMouseUpMultiEvent;
    property MouseMoveMultiEvent  : TMouseMoveMultiEvent read fMouseMoveMultiEvent;
  published
    property Text;
    property HitTest;

    property IsControlGrabbed : boolean read GetIsControlGrabbed;

    property OnOleDragOver  : TOleDragEvent      read fOnOleDragOver  write fOnOleDragOver;
    property OnOleDragDrop  : TOleDragEvent      read fOnOleDragDrop  write fOnOleDragDrop;
    property OnOleDragEnter : TOleDragEvent      read fOnOleDragEnter write fOnOleDragEnter;
    property OnOleDragLeave : TOleDragLeaveEvent read fOnOleDragLeave write fOnOleDragLeave;
    property OnShowContextMenu : TShowContextMenuEvent read fOnShowContextMenu write fOnShowContextMenu;
  end;

implementation

uses
  VamLib.LoggingProxy,
  SysUtils;

{ TVamWinControl }

constructor TVamWinControl.Create(AOwner: TComponent);
begin
  inherited;
  fUpdatingCount := 0;
  fLayout := TVamLayoutWizard.Create(self);

  fChangedMultiEvent    := TNotifyMultiEvent.Create;
  fMouseEnterMultiEvent := TNotifyMultiEvent.Create;
  fMouseLeaveMultiEvent := TNotifyMultiEvent.Create;
  fMouseDownMultiEvent  := TMouseMultiEvent.Create;
  fMouseUpMultiEvent    := TMouseMultiEvent.Create;
  fMouseMoveMultiEvent  := TMouseMoveMultiEvent.Create;
end;

destructor TVamWinControl.Destroy;
begin
  fChangedMultiEvent.Free;
  fMouseEnterMultiEvent.Free;
  fMouseLeaveMultiEvent.Free;
  fMouseDownMultiEvent.Free;
  fMouseUpMultiEvent.Free;
  fMouseMoveMultiEvent.Free;
  fLayout.Free;
  inherited;
end;



procedure TVamWinControl.AlignToParent(const UsingMargins: boolean);
begin
  if not assigned(Parent) then exit;

  BeginUpdate;
  try
    if UsingMargins then
    begin
      Top    := Margins.Top  + Parent.Padding.Top;
      Left   := Margins.Left + Parent.Padding.Left;
      Width  := Parent.Width  - (Margins.Left + Margins.Right)  - (Parent.Padding.Left + Parent.Padding.Right);
      Height := Parent.Height - (Margins.Top  + Margins.Bottom) - (Parent.Padding.Top  + Parent.Padding.Bottom);
    end else
    begin
      Top    := 0;
      Left   := 0;
      Width  := Parent.Width  - (Parent.Padding.Left + Parent.Padding.Right);
      Height := Parent.Height - (Parent.Padding.Top  + Parent.Padding.Bottom);
    end;
  finally
    EndUpdate;
  end;
end;

procedure TVamWinControl.BeginUpdate;
begin
  inc(fUpdatingCount);
end;

procedure TVamWinControl.EndUpdate;
begin
  dec(fUpdatingCount);
  if fUpdatingCount = 0 then self.Invalidate;
  if fUpdatingCount < 0 then raise Exception.Create('Begin/End Update mismatch.');
end;

function TVamWinControl.GetFont: TFont;
begin
  result := inherited Font;
end;

function TVamWinControl.GetIsControlGrabbed: boolean;
begin
  result := (fIsGrabbedByLeft) or (fIsGrabbedByRight);
end;

function TVamWinControl.GetIsUpdating: boolean;
begin
  if fUpdatingCount > 0
    then result := true
    else result := false;
end;

function TVamWinControl.GetObject: TObject;
begin
  result := self;
end;

procedure TVamWinControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft  then fIsGrabbedByLeft  := true;
  if Button = mbRight then fIsGrabbedByRight := true;
  MouseDownMultiEvent.TriggerAll(self, Button, Shift, X, Y);
end;

procedure TVamWinControl.MouseEnter;
begin
  inherited;
  MouseEnterMultiEvent.TriggerAll(self);
end;

procedure TVamWinControl.MouseLeave;
begin
  inherited;
  MouseLeaveMultiEvent.TriggerAll(self);
end;

procedure TVamWinControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  MouseMoveMultiEvent.TriggerAll(self, Shift, X, Y);
end;

procedure TVamWinControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (fIsGrabbedByLeft) then
  begin
    fIsGrabbedByLeft  := false;
  end;

  if (Button = mbRight) and (fIsGrabbedByRight) then
  begin
    fIsGrabbedByRight := false;
    if (assigned(OnShowContextMenu)) then OnShowContextMenu(self, X, Y);
  end;

  MouseUpMultiEvent.TriggerAll(self, Button, Shift, X, Y);
end;

procedure TVamWinControl.OleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
begin
  if assigned(OnOleDragDrop) then OnOleDragDrop(Self, ShiftState, APoint, Effect, Data);
end;

procedure TVamWinControl.OleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
begin
  if assigned(OnOleDragEnter) then OnOleDragEnter(Self, ShiftState, APoint, Effect, Data);
end;

procedure TVamWinControl.OleDragLeave(Sender: TObject);
begin
  if assigned(OnOleDragLeave) then OnOleDragLeave(Self);

end;

procedure TVamWinControl.OleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
begin
  if assigned(OnOleDragOver) then OnOleDragOver(Self, ShiftState, APoint, Effect, Data);
end;



procedure TVamWinControl.Resize(aLeft, aTop, aWidth, aHeight: Integer);
begin
    // NOTE: Resize calls SetBounds() then fires the OnResize() event.
  // I'm not sure why the OnResize() event isn't called after SetBounds().
  // I considered overriding the SetBounds() method so that the OnResize()
  // event is fired, but I'm guessing there must be a reason the VCL designers
  // didn't do this.
  // Adding the Resize() method allows the control to be resized and the Resize()
  // event to be triggered where appropiate.

  if (aLeft <> Left) or (aTop <> Top) or (aWidth <> Width) or (aHeight <> aHeight) then
  begin
    BeginUpdate;
    try
      SetBounds(aLeft, aTop, aWidth, aHeight);
      if assigned(OnResize) then OnResize(self);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TVamWinControl.SetFont(const Value: TFont);
begin
  inherited Font := Value;
end;

procedure TVamWinControl.SetOnMouseEnter(Handler: TNotifyEvent);
begin
  OnMouseEnter := Handler;
end;

procedure TVamWinControl.SetOnMouseLeave(Handler: TNotifyEvent);
begin
  OnMouseLeave := Handler;
end;

procedure TVamWinControl.SetOnMouseDown(Handler: TMouseEvent);
begin
  OnMouseDown := Handler;
end;

procedure TVamWinControl.SetOnMouseUp(Handler: TMouseEvent);
begin
  OnMouseUp := Handler;
end;

procedure TVamWinControl.WMHScroll(var Message: TWMHScroll);
begin
  inherited;
end;

procedure TVamWinControl.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
end;

procedure TVamWinControl.WMMouseWheel(var Message: TWMMouseWheel);
const
  // NOTE: Constants defined here. I'm not sure where in the VCL they are
  // defined.
  // http://msdn.microsoft.com/en-us/library/windows/desktop/ms645617%28v=vs.85%29.aspx
  MK_Control = $0008; //0x0008
  MK_LBUTTON = $0001; //0x0001
  MK_MBUTTON = $0010; //0x0010
  MK_RBUTTON = $0002; //0x0002
  MK_SHIFT   = $0004; //0x0004
var
  Shift : TShiftState;
  MousePos : TPoint;
  Handled : boolean;
begin
  inherited;

  //x := Message.WheelDelta;

  Shift := [];

  if (Message.Keys and MK_SHIFT) >= 1
    then Shift := Shift + [ssShift];

  if (Message.Keys and MK_CONTROL) >= 1
    then Shift := Shift + [ssCtrl];

  if (Message.Keys and MK_LBUTTON) >= 1
    then Shift := Shift + [ssLeft];

  if (Message.Keys and MK_MBUTTON) >= 1
    then Shift := Shift + [ssMiddle];

  if (Message.Keys and MK_RBUTTON) >= 1
    then Shift := Shift + [ssRight];

  MousePos := Point(Message.XPos,Message.YPos);

  Handled := false;

  if Message.WheelDelta > 0 then MouseWheelUp(Shift, Message.WheelDelta , MousePos, Handled);
  if Message.WheelDelta < 0 then MouseWheelDown(Shift, Message.WheelDelta , MousePos, Handled);


  // TODO: NOTE: Is returning 1 or 0 the correct way to return the result?
  if Handled
    then Message.Result := 1
    else Message.Result := 0;

end;



procedure TVamWinControl.CMMouseWheel(var Message: TCMMouseWheel);
begin
  inherited;
  //This doesn't seem to be called. I'm not sure under what conditions it is called.
end;

procedure TVamWinControl.MouseWheelDown(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // NOTE: WheelDelta is always a multiple of 120. (If I understand correctly.)
  // http://msdn.microsoft.com/en-us/library/windows/desktop/ms645617%28v=vs.85%29.aspx
  // Each increment of 120 represents one mouse wheel notch up or down.

  if assigned(OnMouseWheelDown) then OnMouseWheelDown(self, Shift, MousePos, Handled);

end;

procedure TVamWinControl.MouseWheelUp(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  // NOTE: WheelDelta is always a multiple of 120. (If I understand correctly.)
  // http://msdn.microsoft.com/en-us/library/windows/desktop/ms645617%28v=vs.85%29.aspx
  // Each increment of 120 represents one mouse wheel notch up or down.

  if assigned(OnMouseWheelUp) then OnMouseWheelUp(self, Shift, MousePos, Handled);
end;





end.
