unit RedFoxGraphicControl;

interface

uses
  Messages, Classes, Controls, RedFoxImageBuffer, Types, RedFoxContainer;

type
  TRedFoxGraphicControl = class(TControl, IRedFoxVisibleControl)
  private
    fBackBuffer: TRedFoxImageBuffer;
    fOnMouseEnter: TNotifyEvent;
    fOnMouseLeave: TNotifyEvent;
    fHitTest: boolean;
    fIsSubComponent: boolean;
    fIsBackBufferDirty: boolean;
    fInvalidateRequired : boolean;
    fTransparent: boolean;
    fDisplayClass: string;
    fOpacity: byte;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMHitTest(var Message: TCMHitTest); message CM_HITTEST;
    procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    function GetIsSubComponent: boolean;
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    procedure SetTransparent(const Value: boolean);
    procedure SetOpacity(const Value: byte);

    property IsBackBufferDirty : boolean read fIsBackBufferDirty;

    function GetIsBackBufferDirty : boolean;
    procedure MarkAsInvalidateRequired;
    function GetIsTransparent : boolean;
    function GetDisplayClass : string;
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;

    function GetObject:TObject;
    procedure GetTopMostContainer(out Container:TRedFoxContainer; out ContainerOffset:TPoint);
    function GetBackBuffer:TRedFoxImageBuffer;
    procedure OffscreenUpdate(x1, y1, x2, y2 : integer);

    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;

    function GetAbsoluteBoundsRect : TRect; //Return the control bounds rect relative to the top most control.

    function GetIsShowing : boolean;

    property OnMouseEnter : TNotifyEvent read fOnMouseEnter write fOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read fOnMouseLeave write fOnMouseLeave;

    property HitTest : boolean read fHitTest write fHitTest; //when false mouse events will fall through to the window below.

    //It's possible that a controls Paint() method should only be called by itself.. therefore it should be moved to protected.
    procedure Paint; virtual;

    // A transparent control will not be rendered to the Redfox containers back
    // buffer. It shouldn't have it's Paint() method called either.
    property Transparent : boolean read fTransparent write SetTransparent;
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

    function GetBoundsRect:TRect;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure Invalidate; override;

    property BackBuffer:TRedFoxImageBuffer read fBackBuffer write fBackBuffer;

    property IsSubComponent : boolean read fIsSubComponent write fIsSubComponent;

    property Visible : boolean read GetVisible write SetVisible;
  published
    property Opacity      : byte   read fOpacity      write SetOpacity;
    property DisplayClass : string read fDisplayClass write fDisplayClass;
  end;




implementation

uses
  RedFox2D,
  Windows, VamLib.Utils,
  Agg2d, AggWin32Bmp, Graphics,
  SysUtils, RedFox, RedFoxBlend, RedFoxWinControl, RedFoxInvalidator;

{ TRedFoxControl }

constructor TRedFoxGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  fUpdatingCount := 0;
  fOpacity := 255;
  ControlStyle := ControlStyle + [csOpaque];
  BackBuffer := TRedFoxImageBuffer.Create;
  fHitTest := true;
  fIsBackBufferDirty := true;
  fTransparent := false;
end;

destructor TRedFoxGraphicControl.Destroy;
begin
  CancelInvalidateRequests(self);
  BackBuffer.Free;
  inherited;
end;

procedure TRedFoxGraphicControl.Paint;
begin
  //Override this method to draw the control. The control should
  // be drawn to the backbuffer.
end;

procedure TRedFoxGraphicControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
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

procedure TRedFoxGraphicControl.SetOpacity(const Value: byte);
begin
  if (Value <> fOpacity) then
  begin
    fOpacity := Value;
    Invalidate;
  end;
end;

procedure TRedFoxGraphicControl.SetTransparent(const Value: boolean);
begin
  if Value <> fTransparent then
  begin
    fTransparent := Value;
    Invalidate;
  end;
end;

function TRedFoxGraphicControl.GetAbsoluteBoundsRect: TRect;
var
  aContainer : TRedFoxContainer;
  aOffset : TPoint;
begin
  GetTopMostContainer(aContainer, aOffset);
  result := Rect(aOffset.X, aOffset.Y, Width + aOffset.X, Height + aOffset.Y);
end;

function TRedFoxGraphicControl.GetBackBuffer: TRedFoxImageBuffer;
begin
  result := fBackBuffer;
end;

function TRedFoxGraphicControl.GetBoundsRect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function TRedFoxGraphicControl.GetDisplayClass: string;
begin
  result := fDisplayClass;
end;

function TRedFoxGraphicControl.GetIsBackBufferDirty: boolean;
begin
  result := fIsBackBufferDirty
end;

function TRedFoxGraphicControl.GetIsShowing: boolean;
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

function TRedFoxGraphicControl.GetIsSubComponent: boolean;
begin
  Result := fIsSubComponent;
end;

function TRedFoxGraphicControl.GetIsTransparent: boolean;
begin
  result := fTransparent;
end;

function TRedFoxGraphicControl.GetObject: TObject;
begin
  result := self;
end;

procedure TRedFoxGraphicControl.GetTopMostContainer(out Container:TRedFoxContainer; out ContainerOffset:TPoint);
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

procedure TRedFoxGraphicControl.Invalidate;
begin
  fIsBackBufferDirty := true;

  if (IsUpdating = false) and (AreParentsUpdating = false) then
  begin
    //Log.LogMessage(self.Name + ' (' + self.ClassName + ') Invalidate');
    LaggyInvalidate(self);
  end;
end;

function TRedFoxGraphicControl.IsUpdating: boolean;
begin
  if fUpdatingCount > 0
    then result := true
    else result := false;
end;

procedure TRedFoxGraphicControl.MarkAsInvalidateRequired;
begin
  fInvalidateRequired := true;
end;

procedure TRedFoxGraphicControl.MouseEnter;
begin
  if assigned(OnMouseEnter) then OnMouseEnter(self);
end;

procedure TRedFoxGraphicControl.MouseLeave;
begin
  if assigned(OnMouseLeave) then OnMouseLeave(self);
end;



procedure TRedFoxGraphicControl.WMPaint(var Message: TWMPaint);
var
  aContainer : TRedFoxContainer;
  aOffset    : TPoint;
  aRect      : TRect;
begin
  if (Message.DC <> 0) and not (csDestroying in ComponentState) then
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
      aContainer.OffscreenBuffer.DrawTo(Message.DC,-aOffset.X,-aOffset.Y);
    end;

  end;
end;

procedure TRedFoxGraphicControl.OffscreenUpdate(x1, y1, x2, y2: integer);
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

  if TP <> nil then
  begin
    RedFox_AlphaBlit(TP.OffscreenBuffer.RedFoxInterface, BackBuffer.RedFoxInterface, x1, y1, x2, y2, DestX, DestY, Opacity);
  end;
end;

function TRedFoxGraphicControl.AreParentsUpdating: boolean;
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

procedure TRedFoxGraphicControl.BeginUpdate;
begin
  inc(fUpdatingCount);
end;

procedure TRedFoxGraphicControl.EndUpdate;
begin
  dec(fUpdatingCount);
  if fUpdatingCount = 0 then self.Invalidate;
  if fUpdatingCount < 0 then raise Exception.Create('Begin/End Update mismatch.');
end;



procedure TRedFoxGraphicControl.CMHitTest(var Message: TCMHitTest);
begin
  if (HitTest = false) and ((csDesigning in Self.ComponentState) = false) then
  begin
    Message.Result := HTNOWHERE;
  end else
  begin
    Message.Result := HTClient;
  end;
end;

procedure TRedFoxGraphicControl.CMMouseEnter(var Message: TMessage);
begin
  MouseEnter;
end;

procedure TRedFoxGraphicControl.CMMouseLeave(var Message: TMessage);
begin
  MouseLeave;
end;



procedure TRedFoxGraphicControl.CMMouseWheel(var Message: TCMMouseWheel);
begin
  //This never seems to get called.
  Message.Result := 0;
end;

function TRedFoxGraphicControl.GetVisible: boolean;
begin
  result := inherited Visible;
end;

procedure TRedFoxGraphicControl.SetVisible(const Value: boolean);
var
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



