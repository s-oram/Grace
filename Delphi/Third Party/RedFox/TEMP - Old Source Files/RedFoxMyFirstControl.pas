unit RedFoxMyFirstControl;

interface

uses
  Windows,
  Types,
  Graphics,
  Contnrs,
  Classes, Controls, Messages,
  Agg2D,
  RedFoxImageBuffer,
  RedFoxTreeViewNode,
  RedFoxCustomTreeView,
  RedFoxCustomMoveableControl;

type
  TRedFoxMyFirstControl = class(TControl)
  private
  protected
    fBackBuffer: TRedFoxImageBuffer;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DrawDesignOutline;


    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint;


    property BackBuffer:TRedFoxImageBuffer read fBackBuffer write fBackBuffer;
  published

  end;

implementation

uses
  RedFox;



{ TRedFoxMyFirstControl }

constructor TRedFoxMyFirstControl.Create(AOwner: TComponent);
begin
  inherited;
  self.ControlStyle := self.ControlStyle - [csOpaque];
  BackBuffer := TRedFoxImageBuffer.Create;
end;

destructor TRedFoxMyFirstControl.Destroy;
begin
  BackBuffer.Free;
  inherited;
end;

procedure TRedFoxMyFirstControl.DrawDesignOutline;
begin
  //Canvas.Pen.Color := clSilver;
  //Canvas.Pen.Width := 1;
  //Canvas.Pen.Style := TPenStyle.psDot;
  //Canvas.Brush.Style := TBrushStyle.bsClear;
  //Canvas.Rectangle(0,0,Width-1,Height-1);
end;

procedure TRedFoxMyFirstControl.Paint;
var
  c1,c2: Integer;
  Bitmap: TBitmap;
  BlendFunction: TBlendFunction;
  Pixel : P32BitPixel;
begin
  BackBuffer.BufferInterface.ClearAll(Random(255),Random(255),Random(255),64);
  //BackBuffer.AlphaBlendTo(Canvas.Handle, Width, Height);




  if csDesigning in ComponentState then
  begin
    DrawDesignOutline;
  end;

end;

procedure TRedFoxMyFirstControl.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;

  if (AWidth <> 0) and (AHeight <> 0) then
  begin
    BackBuffer.Width := AWidth;
    BackBuffer.Height := AHeight;
  end;

end;

procedure TRedFoxMyFirstControl.WMPaint(var Message: TWMPaint);
begin
  Paint;

  Message.Result := 0;

  if Message.DC <> 0 then
  begin
    BackBuffer.AlphaBlendTo(Message.DC, 0, 0, Width, Height);
  end;

end;

end.
