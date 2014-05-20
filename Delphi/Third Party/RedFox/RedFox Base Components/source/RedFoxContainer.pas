unit RedFoxContainer;

interface

uses
  Windows, Graphics, Messages,
  Classes, Controls,
  Agg2d,
  RedFox, RedFoxImageBuffer, RedFoxColor;


type
  //forward declarations.
  IRedFoxVisibleControl = interface;
  TRedFoxContainer = class;


  // Any visible RedFox control must implement IRedFoxVisibleControl. The interface
  // provides methods for the parent container to manage drawing the controls to
  // the offscreen back buffer and ultimately to the screen.
  IRedFoxVisibleControl = interface
    ['{5E27895D-A6B1-4992-B3E2-0FFEC15621D2}']
    function GetObject:TObject;
    procedure GetTopMostContainer(out Container:TRedFoxContainer; out ContainerOffset:TPoint);
    function GetBackBuffer:TRedFoxImageBuffer;

    function GetBoundsRect: TRect;
    function GetAbsoluteBoundsRect : TRect; // Return the control bounds rect relative to the top most container control.
    function GetIsShowing : boolean;        // Is this control and all parents visible property TRUE?
    procedure OffscreenUpdate(x1, y1, x2, y2 : integer); //Asks the control to update the specified section to the container controls off-screen buffer.

    function GetIsBackBufferDirty : boolean;

    function GetIsTransparent : boolean;

    function GetDisplayClass : string;

    // GetIsSubComponent() - SubComponents are controls that have been instantiated as child controls
    // - Not needed by application developers.
    // - Only needed during component development.
    // TODO: write better comment.
    // TODO: Delphi VCL already has a SetSubComponent() method. It does something
    // different to GetIsSubComponent() here.
    // Perhaps this should be renamed to something else to avoid confusion with the
    // VCL SetSubComponent() method.
    function GetIsSubComponent: boolean;


    // MarkAsInvalidateRequired()
    // This MarkAsInvalidateRequired() invalidate required is kind of a hacky workaround.
    // Currently it is only called from the RedFoxInvalidator class.
    // The MarkAsInvalidateRequired() ensures the container back buffer is blitted to
    // screen. I found one situation where a control wouldn't blit to the screen
    // if multiple controls lying partially over one another were invalidated at the same time.
    // There might be a better approach then this one off workaround.. but nothing
    // jumped out at me. Perhaps the container control could maintain a list of
    // areas requiring blitting.. while it might work, it's a lot of new code to
    // write to fix something that seems to be close to working now.
    procedure MarkAsInvalidateRequired;

    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: boolean;
    function AreParentsUpdating : boolean;
  end;


  TRedFoxContainer = class(TWinControl, IRedFoxVisibleControl)
  private
    fColor: TRedFoxColor;
    fOffscreenBuffer: TRedFoxImageBuffer;
    fIsTopMostContainer : boolean;
    fIsSubComponent: boolean;
    IsContainerInvalidateRequired : boolean;
    function GetColor: TRedFoxColorString;
    procedure SetColor(const Value: TRedFoxColorString);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd);message WM_ERASEBKGND;
    function GetIsSubComponent: boolean;
    function GetIsBackBufferDirty : boolean;
    procedure MarkAsInvalidateRequired;
    function GetIsTransparent : boolean;
    function GetDisplayClass : string;
  protected
    procedure PaintWindow(DC: HDC); override;
    procedure RequestAlign; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;

    procedure SetParent(AParent: TWinControl); override;

    procedure DrawDesignTimeRect(const aRect:TRect);

    function GetObject:TObject;
    procedure GetTopMostContainer(out Container:TRedFoxContainer; out ContainerOffset:TPoint);
    function GetBackBuffer:TRedFoxImageBuffer;
    function GetBoundsRect: TRect;
    function GetAbsoluteBoundsRect : TRect; //Return the control bounds rect relative to the top most control.
    procedure OffscreenUpdate(x1, y1, x2, y2 : integer);

    property IsTopMostContainer : boolean read fIsTopMostContainer;

    function GetIsShowing : boolean;

    function IsPaintRequiredForControl(const aControl:TControl; aRegion:TRect):boolean;
    procedure PaintControl(const aControl:TControl; aRegion:TRect);
    procedure BlitRegionToBackBuffer(aRegion:TRect);

  protected
    fUpdatingCount : integer;
  public
    // BeginUpdate() / EndUpdate() calls can be nested. A BeginUpdate() call must
    // always be followed by a EndUpdate() call.
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: boolean;
    function AreParentsUpdating : boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateParams(var Params: TCreateParams); override;


    // PaintRegion() will check if any controls in the region
    // are dirty. If so, it will blit all controls in the region to
    // the back buffer.
    procedure PaintRegion(aRegion:TRect);

    // InvalidateRegion() can be called to force the container
    // to blit all controls in the region to it's back buffer.
    procedure InvalidateRegion(aRegion:TRect);

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property OffscreenBuffer : TRedFoxImageBuffer read fOffscreenBuffer write fOffscreenBuffer;

    // AlignToParent() resizes the control to the same size as the containing parent control.
    procedure AlignToParent(const UsingMargins : boolean = false);

    property IsSubComponent : boolean read fIsSubComponent write fIsSubComponent;
  published
    property Color : TRedFoxColorString read GetColor write SetColor;

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

    //=== TWinControl Properties ===
    property AutoSize;
    property Padding;
    //=== END TWinControl Properties ==
  end;

implementation

uses
  SysUtils,
  AggRoundedRect, AggPathStorage,
  RedFoxGraphicControl, RedFoxWinControl;

{ TRedFoxContainer }

constructor TRedFoxContainer.Create(AOwner: TComponent);
begin
  inherited;
  fUpdatingCount := 0;
  ControlStyle := ControlStyle + [csAcceptsControls] + [csOpaque];
  DoubleBuffered := true;
  OffscreenBuffer := TRedFoxImageBuffer.Create;
  fColor.SetColor(255,238,238,238);
end;

procedure TRedFoxContainer.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and (not WS_CLIPCHILDREN);
end;

destructor TRedFoxContainer.Destroy;
begin
  OffscreenBuffer.Free;
  inherited;
end;

procedure TRedFoxContainer.DrawDesignTimeRect(const aRect: TRect);
var
  Rc: TAggRoundedRect;
  Path: TAggPathStorage;
begin
  rc := TAggRoundedRect.Create;
  Path := TAggPathStorage.Create;
  try
    OffscreenBuffer.BufferInterface.LineWidth := 1;
    OffscreenBuffer.BufferInterface.SetLineColor(120,120,120,255);
    OffscreenBuffer.BufferInterface.NoFill;
    OffscreenBuffer.BufferInterface.Rectangle(aRect.Left+0.5, aRect.Top+0.5, aRect.Right-0.5, aRect.Bottom-0.5);

    rc.Rect(aRect.Left+0.5, aRect.Top+0.5, aRect.Right-0.5, aRect.Bottom-0.5);
    Path.AddPath(rc);
    OffscreenBuffer.BufferInterface.SetLineColor(190,190,190,255);
    OffscreenBuffer.BufferInterface.ResetPath;
    OffscreenBuffer.BufferInterface.AddPath(Path);
    OffscreenBuffer.BufferInterface.AddDash(7,7);
    OffscreenBuffer.BufferInterface.DrawPath;
    OffscreenBuffer.BufferInterface.RemoveAllDashes;
  finally
    rc.Free;
    Path.Free;
  end;

end;

procedure TRedFoxContainer.AlignToParent(const UsingMargins: boolean);
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

function TRedFoxContainer.AreParentsUpdating: boolean;
var
  p : TRedFoxWinControl;
begin
  if (assigned(Parent)) and (Parent is TRedFoxWinControl)
    then p := (Parent as TRedFoxWinControl)
    else p := nil;

  while assigned(p) do
  begin
    if p.IsUpdating then exit(true);

    if (assigned(p.Parent)) and (p.Parent is TRedFoxWinControl)
      then p := (p.Parent as TRedFoxWinControl)
      else p := nil;
  end;

  // if we've made it this far, no updating parent has been found.
  result := false;
end;

procedure TRedFoxContainer.BeginUpdate;
begin
  inc(fUpdatingCount);
end;

procedure TRedFoxContainer.EndUpdate;
begin
  dec(fUpdatingCount);
  if fUpdatingCount = 0 then self.Invalidate;
  if fUpdatingCount < 0 then raise Exception.Create('Begin/End Update mismatch.');
end;

function TRedFoxContainer.IsUpdating: boolean;
begin
  if fUpdatingCount > 0
    then result := true
    else result := false;
end;

function TRedFoxContainer.GetAbsoluteBoundsRect: TRect;
var
  aContainer : TRedFoxContainer;
  aOffset : TPoint;
begin
  GetTopMostContainer(aContainer, aOffset);
  result := Rect(aOffset.X, aOffset.Y, Width + aOffset.X, Height + aOffset.Y);
end;

function TRedFoxContainer.GetBackBuffer: TRedFoxImageBuffer;
begin
  result := nil;
end;

function TRedFoxContainer.GetBoundsRect: TRect;
begin
  result := self.BoundsRect;
end;

function TRedFoxContainer.GetColor: TRedFoxColorString;
begin
  result := fColor.AsString;
end;

function TRedFoxContainer.GetDisplayClass: string;
begin
  // not used.
  result := '';
end;

function TRedFoxContainer.GetIsBackBufferDirty: boolean;
begin
  // TODO: This method has been added to temporarily satisfy interface
  // requirements. I'm not sure if it needs to be implemented more fully.
  result := false;
end;

function TRedFoxContainer.GetIsShowing: boolean;
var
  aControl : TControl;
begin
  // Controls are always 'showing' at design time.
  if csDesigning in ComponentState then exit(true);

  aControl := self;

  while assigned(aControl.Parent) do
  begin
    if aControl.Visible = false then
    begin
      result := false;
      exit; //=============>>exit>>=========>>
    end else
    begin
      aControl := aControl.Parent;
    end;
  end;

  if (aControl.Visible = true)
    then result := true
    else result := false;
end;



function TRedFoxContainer.GetIsSubComponent: boolean;
begin
  Result := fIsSubComponent;
end;

function TRedFoxContainer.GetIsTransparent: boolean;
begin
  result := false;
end;

function TRedFoxContainer.GetObject: TObject;
begin
  result := self;
end;

procedure TRedFoxContainer.GetTopMostContainer(out Container: TRedFoxContainer; out ContainerOffset: TPoint);
var
  aControl : TControl;
  aOffset : TPoint;
begin
  aControl := self;
  aOffset := Point(0,0);
  while Supports(aControl.Parent, IRedFoxVisibleControl) do
  begin
    aOffset.Offset(aControl.Left, aControl.Top);
    aControl := aControl.Parent;
  end;

  if (aControl is TRedFoxContainer) then
  begin
    Container := aControl as TRedFoxContainer;
    ContainerOffset := aOffset;
  end else
  begin
    Container := nil;
    ContainerOffset := Point(0,0);
  end;
end;

procedure TRedFoxContainer.OffscreenUpdate(x1, y1, x2, y2: integer);
begin
  //Do nothing. This control doesn't have a backbuffer.
end;

procedure TRedFoxContainer.InvalidateRegion(aRegion: TRect);
begin
  BlitRegionToBackBuffer(aRegion);
end;

procedure TRedFoxContainer.PaintRegion(aRegion: TRect);
var
  c1 : integer;
  IsPaintRequired : boolean;
begin
  IsPaintRequired := false;
  for c1 := 0 to ControlCount-1 do
  begin
    if IsPaintRequiredForControl(Controls[c1], aRegion) then
    begin
      IsPaintRequired := true;
      break;
    end;
  end;

  if IsPaintRequired then BlitRegionToBackBuffer(aRegion);
end;

procedure TRedFoxContainer.BlitRegionToBackBuffer(aRegion: TRect);
var
  c1: Integer;
begin
  OffscreenBuffer.BufferInterface.ClipBox(aRegion.Left, aRegion.Top, aRegion.Right, aRegion.Bottom);

  //TODO:HIGH this bit of code is causing problems in combination with begin/end update.
  // Perhaps it should only clear the background here at Design time.
  {
  OffscreenBuffer.BufferInterface.LineWidth := 0;
  OffscreenBuffer.BufferInterface.NoLine;
  OffscreenBuffer.BufferInterface.SetFillColor(fColor.R, fColor.G, fColor.B, 255);
  OffscreenBuffer.BufferInterface.FillColor := GetRedFoxColor(clPurple);
  OffscreenBuffer.BufferInterface.Rectangle(aRegion.Left, aRegion.Top, aRegion.Right, aRegion.Bottom);
  }
  for c1 := 0 to ControlCount-1 do PaintControl(Controls[c1], aRegion);
end;



function TRedFoxContainer.IsPaintRequiredForControl(const aControl: TControl; aRegion: TRect): boolean;
var
  c1: Integer;
  aVisibleControl : IRedFoxVisibleControl;
  ControlBounds : TRect;
  aWinControl : TWinControl;
  ClippedRegion : TRect;
  xChildRequiresRepaint : boolean;
begin
  // assume no controls need to be painted by default.
  result := false;

  // Start checking for clipped controls, if any are 'dirty' we can exit immediately. Only one control needs to be
  // dirty in the clipped region to require the area to be repainted.
  if (Supports(aControl, IRedFoxVisibleControl, aVisibleControl)) and (aVisibleControl.GetIsShowing) and (aControl.Width > 0) and (aControl.Height > 0) then
  begin
    //TODO:HIGH not sure if these checks are required..
    if (aVisibleControl.IsUpdating) or (aVisibleControl.AreParentsUpdating)
      then exit(false); // no update required... exit now...

    ControlBounds := aVisibleControl.GetAbsoluteBoundsRect;

    ClippedRegion := aRegion;
    ClippedRegion.Intersect(ControlBounds);


    // check if control is in the repaint region, if not it doesn't need to
    // be repainted.
    if ClippedRegion.IsEmpty
      then exit(false);


    // Check if the clipped control is dirty. If it is, return true and exit. No need to
    // continue hunting for dirty controls.
    if (not ClippedRegion.IsEmpty) and (aVisibleControl.GetIsBackBufferDirty)
      then exit(true);


    // check if child controls exist and if they are dirty...
    if (not ClippedRegion.IsEmpty) and (aVisibleControl.GetObject is TWinControl) then
    begin
      aWinControl := (aVisibleControl.GetObject as TWinControl);
      for c1 := 0 to aWinControl.ControlCount-1 do
      begin
        xChildRequiresRepaint := IsPaintRequiredForControl(aWinControl.Controls[c1], ClippedRegion);

        if xChildRequiresRepaint
          then exit(true); //we've found a dirty child control. Exit now...
      end;
    end;

  end;

end;







procedure TRedFoxContainer.MarkAsInvalidateRequired;
begin
  //do nothin.
end;

procedure TRedFoxContainer.PaintControl(const aControl: TControl; aRegion: TRect);
var
  c1: Integer;
  aVisibleControl : IRedFoxVisibleControl;
  TempRect : TRect;
  ControlBounds : TRect;
  aWinControl : TWinControl;
  DesignRect : TRect;
  ClippedRegion : TRect;
begin
  if (Supports(aControl, IRedFoxVisibleControl, aVisibleControl)) and (aVisibleControl.GetIsShowing) and (aControl.Width > 0) and (aControl.Height > 0) then
  begin
    ControlBounds := aVisibleControl.GetAbsoluteBoundsRect;

    ClippedRegion := aRegion;
    ClippedRegion.Intersect(ControlBounds);

    if (not ClippedRegion.IsEmpty) {and (not aVisibleControl.IsUpdating) and (not aVisibleControl.AreParentsUpdating)} then
    begin
      OffscreenBuffer.BufferInterface.ClipBox(ClippedRegion.Left, ClippedRegion.Top, ClippedRegion.Right, ClippedRegion.Bottom);

      TempRect := ClippedRegion;
      TempRect.Offset(-ControlBounds.Left, -ControlBounds.Top);

      if aVisibleControl.GetIsTransparent = false
        then aVisibleControl.OffscreenUpdate(TempRect.Left, TempRect.Top, TempRect.Right, TempRect.Bottom);

      // Only TWinControl descendents can contain child controls.
      if (aVisibleControl.GetObject is TWinControl) and (not aVisibleControl.IsUpdating) then
      begin
        aWinControl := (aVisibleControl.GetObject as TWinControl);
        for c1 := 0 to aWinControl.ControlCount-1 do PaintControl(aWinControl.Controls[c1], ClippedRegion);
      end;
    end;

    if (csDesigning in Self.ComponentState) and (aVisibleControl.GetIsSubComponent = false) then
    begin
      OffscreenBuffer.BufferInterface.ClipBox(ClippedRegion.Left, ClippedRegion.Top, ClippedRegion.Right, ClippedRegion.Bottom);
      DesignRect := aVisibleControl.GetAbsoluteBoundsRect;
      DrawDesignTimeRect(DesignRect);
    end;
  end;
end;

procedure TRedFoxContainer.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth <> 0) and (AHeight <> 0) then
  begin
    OffscreenBuffer.Width := AWidth;
    OffscreenBuffer.Height := AHeight;
  end;
end;

procedure TRedFoxContainer.SetColor(const Value: TRedFoxColorString);
begin
  if Value <> fColor.AsString then
  begin
    fColor.SetColor(Value);
    Invalidate;
  end;
end;

procedure TRedFoxContainer.SetParent(AParent: TWinControl);
var
  aContainer : TRedFoxContainer;
  aOffset : TPoint;
begin
  inherited;
  GetTopMostContainer(aContainer, aOffset);
  if aContainer = self
    then fIsTopMostContainer := true
    else fIsTopMostContainer := false;
end;

procedure TRedFoxContainer.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
  // NOTE: Maybe this blog post is relevent.
  // http://www.stevetrefethen.com/blog/QuickTip2FixingflickercausedbyWM_ERASEBKGNDinaDelphiVCLapp.aspx
end;

procedure TRedFoxContainer.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TRedFoxContainer.PaintWindow(DC: HDC);
var
  aCanvas : TCanvas;
  aRect   : TRect;
  aContainer : TRedFoxContainer;
  aOffset    : TPoint;
begin
  //TODO: Currently the control is repainting the
  // entire DC area. I think it's possible to
  // detect what section needs to be repainted an only
  // repaint that section.

  if IsTopMostContainer then
  begin
    aRect := Rect(0,0, Width, Height);

    if IsContainerInvalidateRequired then
    begin
      IsContainerInvalidateRequired := false;
      InvalidateRegion(aRect);
    end else
    begin
      PaintRegion(aRect);
    end;


    //TODO: See if we can implement a bitblit here.
    OffscreenBuffer.DrawTo(DC);

    if csDesigning in Self.ComponentState then
    begin
      aCanvas := TCanvas.Create;
      aCanvas.Handle := DC;
      aCanvas.Lock;
      try
        aCanvas.Pen.Color := clGray;
        aCanvas.Pen.Style := psDash;
        aCanvas.Brush.Style := bsClear;
        aCanvas.Rectangle(0,0,Width,Height);
      finally
        aCanvas.Unlock;
        aCanvas.Free;
      end;
    end;
  end else
  begin
    GetTopMostContainer(aContainer, aOffset);

    if aContainer <> nil then
    begin
      aRect := Rect(0,0, Width, Height);
      aRect.Offset(aOffset.X, aOffset.Y);

      aContainer.PaintRegion(aRect);
      aContainer.OffscreenBuffer.DrawTo(DC,-aOffset.X,-aOffset.Y);
    end;
  end;
end;


procedure TRedFoxContainer.RequestAlign;
begin
  inherited;
end;

procedure TRedFoxContainer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TRedFoxContainer.AlignControls(AControl: TControl; var Rect: TRect);
begin
  IsContainerInvalidateRequired := true;
  inherited;
end;





end.
