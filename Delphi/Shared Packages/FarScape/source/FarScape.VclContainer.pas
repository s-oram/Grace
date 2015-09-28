unit FarScape.VclContainer;

interface

uses
  Windows,
  Classes,
  Controls,
  Messages,
  FarScape.CustomControl,
  FarScape.RootControl;

type
  //TFarScapeContainerVCL = class(TCustomControl)
  TFarScapeContainerVCL = class(TWinControl)
  private
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    function GetFarScapeRoot: TFarScapeAbstractRoot;
  protected
    LastMousePoint : TPoint;
    FarScapeRoot : TFarScapeRootControl;
    procedure HandleInvalidateRootRegion(Region : TRect);

    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure PaintWindow(DC: HDC); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure Invalidate; override;

    procedure AddControl(const aControl : TFarScapeControl);
    procedure RemoveControl(const aControl : TFarScapeControl); // Removes the child but doesn't free it.

    property Root : TFarScapeAbstractRoot read GetFarScapeRoot;
  published
    //=== TControl Properties ===
    property Align;
    property AlignWithMargins;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Anchors;
    property Constraints;
    property Visible;

    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    //=== END TControl Properties ==
  end;


implementation

uses
  VamLib.WinUtils,
  SysUtils;

{ TFarScapeAggPasContainer }

constructor TFarScapeContainerVCL.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  FarScapeRoot := TFarScapeRootControl.Create;
  FarScapeRoot.OnInvalidateRootRegion := self.HandleInvalidateRootRegion;
end;

destructor TFarScapeContainerVCL.Destroy;
begin
  FarScapeRoot.Free;
  inherited;
end;

function TFarScapeContainerVCL.GetFarScapeRoot: TFarScapeAbstractRoot;
begin
  result := FarScapeRoot as TFarScapeAbstractRoot;
end;

procedure TFarScapeContainerVCL.HandleInvalidateRootRegion(Region: TRect);
begin
  InvalidateRect(self.Handle, @Region, false);
end;

procedure TFarScapeContainerVCL.AddControl(const aControl: TFarScapeControl);
begin
  aControl.Parent := FarScapeRoot;
end;

procedure TFarScapeContainerVCL.RemoveControl(const aControl: TFarScapeControl);
begin
  aControl.Parent := nil;
end;

procedure TFarScapeContainerVCL.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseEnter;
end;

procedure TFarScapeContainerVCL.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseLeave;
end;

procedure TFarScapeContainerVCL.Invalidate;
begin
  inherited;

end;

procedure TFarScapeContainerVCL.MouseEnter;
begin
  FarScapeRoot.UserInteraction.MouseEnter;
end;

procedure TFarScapeContainerVCL.MouseLeave;
begin
  FarScapeRoot.UserInteraction.MouseLeave;
end;

procedure TFarScapeContainerVCL.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FarScapeRoot.UserInteraction.MouseDown(Button, Shift, X, Y);
end;

procedure TFarScapeContainerVCL.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (x <> LastMousePoint.X) or (Y <> LastMousePoint.Y) then
  begin
    LastMousePoint.X := X;
    LastMousePoint.Y := Y;
    FarScapeRoot.UserInteraction.MouseMove(Shift, X, Y);
  end;
end;

procedure TFarScapeContainerVCL.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FarScapeRoot.UserInteraction.MouseUp(Button, Shift, X, Y);
end;

procedure TFarScapeContainerVCL.PaintWindow(DC: HDC);
var
  ClipBox : TRect;
begin
  inherited;

  if (assigned(FarScapeRoot)) and (FarScapeRoot.Width > 0) and (FarScapeRoot.Height > 0) then
  begin
    GetClipBox(DC, ClipBox);

    // NOTE: Increase the bounds of the clip box by one pixel. I was seeing thin regions
    // not being repainted with rapidly moving a window across the farscape control.
    ClipBox.Right := ClipBox.Right + 1;
    ClipBox.Bottom := ClipBox.Bottom + 1;
    FarScapeRoot.PaintRegion(ClipBox);

    // Blit the region. There doesn't appear to be any reason to clip the blit region to the clip box.
    // The blit region is clipped to the clip box further downstream according to my tests.
    BitBlt(DC,0,0,Width,Height, FarScapeRoot.BackBuffer.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TFarScapeContainerVCL.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FarScapeRoot.SetBounds(0,0,aWidth, aHeight);
end;

procedure TFarScapeContainerVCL.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  // Prevent the background from being erased. We don't need it. Our control is fully opaque.
  // It also helps prevent flickering.
  Message.Result := 1;
end;

procedure TFarScapeContainerVCL.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;


end.
