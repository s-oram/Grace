unit RedFoxDiv;

interface

uses
  Windows,
  Types, Graphics,
  Messages, Classes, Vcl.Controls, RedFoxImageBuffer, RedFoxCustomControl;

type
  TRedFoxDiv = class(TWinControl)
  private
  protected
    Canvas : TCanvas;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DrawDesignOutline;
    procedure CreateParams(var params: TCreateParams); override;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    //-------------
    property Align;
    property AlignWithMargins;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Anchors;
    property Constraints;
    property Visible;

    property Font;
    property ParentFont;

    //TControl Events.
    property OnCanResize;
		property OnClick;
		property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    //end TControlEvents.
  end;

implementation

uses

  Agg2d, AggWin32Bmp,
  SysUtils, RedFox, RedFoxContainer;

{ TRedFoxDiv }

constructor TRedFoxDiv.Create(AOwner: TComponent);
begin
  inherited;
  //Self.InterceptMouse := true;
  ControlStyle := ControlStyle + [csAcceptsControls] - [csOpaque];

  Canvas := TCanvas.Create;
end;




procedure TRedFoxDiv.CreateParams(var params: TCreateParams);
begin
  inherited CreateParams(params);
  //params.ExStyle := params.ExStyle or WS_EX_TRANSPARENT or WS_CLIPCHILDREN;
  //params.ExStyle := params.ExStyle or WS_EX_TRANSPARENT;
  //with Params.WindowClass do
  // style := style and not (CS_HREDRAW or CS_VREDRAW);
end;

destructor TRedFoxDiv.Destroy;
begin
  Canvas.Free;
  inherited;
end;

procedure TRedFoxDiv.DrawDesignOutline;
begin

end;

procedure TRedFoxDiv.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

end;

procedure TRedFoxDiv.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  SrcRect, DstRect : TRect;
  Bitmap : TBitmap;
begin
  //setBkMode(Message.DC, TRANSParent);

  {
  if Message.DC <> 0 then
  begin
    Message.Result := 1;
    Canvas.Lock;
    try
      Canvas.Handle := Message.DC;
      Canvas.Brush.Color := RGB(random(255),random(255),random(255));
      Canvas.Brush.Style := bsSolid;
      Canvas.Rectangle(0,0,Width, Height);
    finally
      Canvas.Unlock;
    end;
  end;
  }
  {
  if Message.DC <> 0 then
  begin
    Message.Result := 1;
    Bitmap := TBitmap.Create;
    Canvas.Lock;
    Canvas.Handle := Message.DC;
    try
      Parent.PaintTo(Message.DC, 0, 0);
    finally
      Bitmap.Free;
      Canvas.Unlock;
    end;
  end;
  }
end;

procedure TRedFoxDiv.WMPaint(var Message: TWMPaint);
begin
  //Include(ControlState, csCustomPaint);
  inherited;
  //Exclude(ControlState, csCustomPaint);





end;

end.
