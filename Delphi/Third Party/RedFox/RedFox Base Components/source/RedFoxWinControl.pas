unit RedFoxWinControl;

interface

uses
  Windows, Types,
  Messages, Classes, Controls, WinApi.Oleacc,
  RedFoxImageBuffer, RedFoxContainer, RedFoxAccessible;

type
  TRedFoxWinControl = class(TWinControl, IRedFoxVisibleControl, IAccessible)
  private
    fBackBuffer: TRedFoxImageBuffer;
    fHitTest: boolean;
    fOnMouseEnter: TNotifyEvent;
    fOnMouseLeave: TNotifyEvent;
    fIsSubComponent: boolean;
    fIsBackBufferDirty: boolean;
    fInvalidateRequired : boolean;
    fTransparent: boolean;

    fIAccessibleWrapper: IAccessible;
    fAccessible: TRedFoxAccessibleProperties;
    fDisplayClass: string;

    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd);message WM_ERASEBKGND;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    function GetIsSubComponent: boolean;
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    procedure SetTransparent(const Value: boolean);
    procedure SetAccessible(const Value: TRedFoxAccessibleProperties);

    property IsBackBufferDirty : boolean read fIsBackBufferDirty;
    function GetIsBackBufferDirty : boolean;
    procedure MarkAsInvalidateRequired;
    function GetIsTransparent : boolean;

    procedure WMGetObject(var Message : TMessage); message WM_GETOBJECT;
    property AccessibleIntF : IAccessible read fIAccessibleWrapper implements IAccessible;

    function GetDisplayClass : string;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; override;
    property Accessible : TRedFoxAccessibleProperties read fAccessible write SetAccessible;

    procedure PaintWindow(DC: HDC); override;

    procedure WndProc(var Message: TMessage); override;
    procedure ControlsAligned; override;

    function GetObject:TObject;
    procedure GetTopMostContainer(out Container:TRedFoxContainer; out ContainerOffset:TPoint);
    function GetBackBuffer:TRedFoxImageBuffer;
    procedure OffscreenUpdate(x1, y1, x2, y2 : integer);
    function GetAbsoluteBoundsRect : TRect; //Return the control bounds rect relative to the top most control.

    function GetIsShowing : boolean;

    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;

    property HitTest : boolean read fHitTest write fHitTest; //when false mouse events will fall through to the window below.

    property OnMouseEnter : TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read fOnMouseLeave write fOnMouseLeave;

    //It's possible that a controls Paint() method should only be called by itself.. therefore it should be moved to protected.
    procedure Paint; virtual;

    // A transparent control will not be rendered to the Redfox containers back
    // buffer. It shouldn't have it's Paint() method called either.
    property Transparent : boolean read fTransparent write SetTransparent;


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    function GetBoundsRect:TRect;

    procedure Invalidate; override;

    property BackBuffer:TRedFoxImageBuffer read fBackBuffer write fBackBuffer;

    property IsSubComponent : boolean read fIsSubComponent write fIsSubComponent;

    property Visible : boolean read GetVisible write SetVisible;
  published

    property DisplayClass : string read fDisplayClass write fDisplayClass;
  end;




implementation

uses
  VamLib.Utils,
  SysUtils, Graphics,
  Agg2d, AggWin32Bmp,
  RedFox, RedFoxBlend, RedFoxInvalidator;


{ TRedFoxControl }

constructor TRedFoxWinControl.Create(AOwner: TComponent);
begin
  inherited;
  fHitTest := true;
  ControlStyle := ControlStyle + [csAcceptsControls] + [csOpaque];
  BackBuffer := TRedFoxImageBuffer.Create;
  fIsBackBufferDirty := true;
  fTransparent := false;

  fAccessible := TRedFoxAccessibleProperties.Create;
  fIAccessibleWrapper := TRedFoxAccessibleWrapper.Create(self, fAccessible);
end;

destructor TRedFoxWinControl.Destroy;
begin
  fAccessible.Free;
  CancelInvalidateRequests(self);
  BackBuffer.Free;
  inherited;
end;

procedure TRedFoxWinControl.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and (not WS_CLIPCHILDREN) and (not WS_CLIPSIBLINGS);
end;

procedure TRedFoxWinControl.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  MouseEnter;
end;

procedure TRedFoxWinControl.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  MouseLeave;
end;



procedure TRedFoxWinControl.ControlsAligned;
begin
  inherited;


end;

procedure TRedFoxWinControl.WndProc(var Message: TMessage);
begin
  inherited;

  {$IFDEF DesignTimeLogging}
  //RedFoxLog.LogMessage(IntToStr(Message.Msg));
  //RedFoxLog.LogText('Class = ' + Self.ClassName + '; Name = ' + Self.Name + ' :: MSG ' + IntToStr(Message.Msg), '');
  {$ENDIF}

  // This is message is sent when a child control is added or removed.
  // It's refreshes the redfox offscreen buffer so you don't see
  // old component images.
  //if Message.Msg = CM_CONTROLLISTCHANGE then self.Invalidate;
end;



procedure TRedFoxWinControl.Paint;
begin

  //  NOTE: There are a whole heap of blend modes. I'm still figuring out
  //  what is suitable for different drawing sceniarios.
  //
  //  Clearing to a white background and drawing using "Source Over"
  //  seems to produce the most natural results when drawing with
  //  semi-transparent colors.
  //
  //  BackBuffer.BufferInterface.ClearAll(255,255,255,0);
  //  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
  //
  //
  //
  //  TAggBlendMode = (bmClear, bmSource, bmDestination, bmSourceOver,
  //      bmDestinationOver, bmSourceIn, bmDestinationIn, bmSourceOut,
  //      bmDestinationOut, bmSourceATop, bmDestinationATop, bmXor, bmPlus, bmMinus,
  //      bmMultiply, bmScreen, bmOverlay, bmDarken, bmLighten, bmColorDodge,
  //      bmColorBurn, bmHardLight, bmSoftLight, bmDifference, bmExclusion,
  //      bmContrast, bmInvert, bmInvertRgb, bmAlpha);




end;

procedure TRedFoxWinControl.PaintWindow(DC: HDC);
var
  aContainer : TRedFoxContainer;
  aOffset    : TPoint;
  aRect      : TRect;
begin
  GetTopMostContainer(aContainer, aOffset);

  if aContainer <> nil then
  begin
    aRect := Rect(0,0, Width, Height);
    aRect.Offset(aOffset.X, aOffset.Y);

    if fInvalidateRequired then
    begin
      fInvalidateRequired := false;
      aContainer.InvalidateRegion(aRect);
    end else
    begin
      aContainer.PaintRegion(aRect);
    end;
    aContainer.OffscreenBuffer.DrawTo(DC,-aOffset.X,-aOffset.Y);
  end;

end;

function TRedFoxWinControl.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if IID = IID_IAccessible then
    result := fIAccessibleWrapper.QueryInterface(IID, Obj)
  else
    result := inherited QueryInterface(IID, Obj);
end;

procedure TRedFoxWinControl.SetAccessible(const Value: TRedFoxAccessibleProperties);
begin
  Value.AssignTo(fAccessible);
end;

procedure TRedFoxWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  //Call inherited to update the control size and position.
  inherited;

  if (AWidth > 0) and (AHeight > 0) then
  begin
    BackBuffer.Width := AWidth;
    BackBuffer.Height := AHeight;
    Invalidate;
  end;
end;

procedure TRedFoxWinControl.SetTransparent(const Value: boolean);
begin
  if Value <> fTransparent then
  begin
    fTransparent := Value;
    Invalidate;
  end;
end;

function TRedFoxWinControl.GetAbsoluteBoundsRect: TRect;
var
  aContainer : TRedFoxContainer;
  aOffset : TPoint;
begin
  GetTopMostContainer(aContainer, aOffset);
  result := Rect(aOffset.X, aOffset.Y, Width + aOffset.X, Height + aOffset.Y);
end;

function TRedFoxWinControl.GetBackBuffer: TRedFoxImageBuffer;
begin
  result := fBackBuffer;
end;

function TRedFoxWinControl.GetBoundsRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TRedFoxWinControl.GetDisplayClass: string;
begin
  result := fDisplayClass;
end;

function TRedFoxWinControl.GetIsBackBufferDirty: boolean;
begin
  result := fIsBackBufferDirty
end;

function TRedFoxWinControl.GetIsShowing: boolean;
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

function TRedFoxWinControl.GetIsSubComponent: boolean;
begin
  Result := fIsSubComponent;
end;

function TRedFoxWinControl.GetIsTransparent: boolean;
begin
  result := fTransparent;
end;

function TRedFoxWinControl.GetObject: TObject;
begin
  result := self;
end;

procedure TRedFoxWinControl.GetTopMostContainer(out Container:TRedFoxContainer; out ContainerOffset:TPoint);
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

procedure TRedFoxWinControl.Invalidate;
begin
  fIsBackBufferDirty := true;
  LaggyInvalidate(self);
end;

procedure TRedFoxWinControl.MarkAsInvalidateRequired;
begin
  fInvalidateRequired := true;
end;

procedure TRedFoxWinControl.MouseEnter;
begin
  if assigned(OnMouseEnter) then OnMouseEnter(self);
end;

procedure TRedFoxWinControl.MouseLeave;
begin
  if assigned(OnMouseLeave) then OnMouseLeave(self);
end;

procedure TRedFoxWinControl.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TRedFoxWinControl.WMGetObject(var Message: TMessage);
var
  ac : IAccessible;
begin
  // Source for code in this method.
  // http://stackoverflow.com/a/16442500/395461

  if (Message.Msg = WM_GETOBJECT) then
  begin
    ac :=  fIAccessibleWrapper;
    QueryInterface(IID_IAccessible, ac);
    Message.Result := LresultFromObject(IID_IAccessible, Message.WParam, fIAccessibleWrapper);
  end
  else
    Message.Result := DefWindowProc(Handle, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TRedFoxWinControl.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if (HitTest = false) and ((csDesigning in Self.ComponentState) = false) then
  begin
    Message.Result := HTTRANSPARENT;
  end else
  begin
    inherited;
  end;
end;

procedure TRedFoxWinControl.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TRedFoxWinControl.OffscreenUpdate(x1, y1, x2, y2: integer);
var
  TP : TRedFoxContainer;
  Offset : TPoint;
  DestX, DestY : integer;
begin
  // TODO: HACK: NOTE: There is a bug when a control has it's AutoSize property set to true. Sometimes (all of the time?)
  // OffscreenUpdate() is called while the BackBuffer has the incorrect width and height. Here I've added a hack to
  // ensure the backbuffer meets the correct minimum size. It would be better if the resize was detected and
  // the control resized the back buffer then. [Shannon Oram Janurary 23rd 2013]
  if CastToCardinal(x2-x1) > BackBuffer.Width
    then BackBuffer.Width := CastToCardinal(x2-x1);

  if CastToCardinal(y2-y1) > BackBuffer.Height
    then BackBuffer.Height := CastToCardinal(y2-y1);

  if IsBackBufferDirty then
  begin
    fIsBackBufferDirty := false;
    Paint;
  end;

  GetTopMostContainer(TP, Offset);

  DestX      := Offset.X + x1;
  DestY      := Offset.Y + y1;

  if TP <> nil
    then BackBuffer.RedFoxInterface.BlendTo(TP.OffScreenBuffer.RedFoxInterface, x1, y1, x2, y2, DestX, DestY);
end;


function TRedFoxWinControl.GetVisible: boolean;
begin
  result := inherited Visible;
end;

procedure TRedFoxWinControl.SetVisible(const Value: boolean);
var
  IsVisibleChanging : boolean;
  Container : TRedFoxContainer;
  ContainerOffset : TPoint;
  aRect : TRect;
begin
  if Value <> Visible then
  begin
    GetTopMostContainer(Container, ContainerOffset);
    aRect := GetAbsoluteBoundsRect;

    //Change visibility state.
    inherited Visible := value;

    // NOTE: Checking if the control has been made invisible is important. The container control
    // needs to be told to update it's offscreen buffer.
    if (Value = false) and (aRect.Width <> 0) and (aRect.Height <> 0) and (assigned(Container)) then
    begin
      Container.InvalidateRegion(aRect);
    end;

  end else
  begin
    // Visibility isn't being change, but still set the property
    // to give ancestor methods a change to respond to the visible property
    // being set.
    inherited Visible := value;
  end;

end;









end.
