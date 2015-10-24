unit FarScape.BitmapBufferControl;

interface

uses
  Windows,
  Types,
  FarScape.CustomControl,
  Graphics;

type
  TFarScapeBitmapBufferControl = class(TFarScapeControl)
  private
    fBackBuffer: TBitmap;
    fIsTransparent: boolean;
  protected
    procedure ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight : integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    // Draw the control to the back buffer.
    property BackBuffer : TBitmap read fBackBuffer;

    // Set to true if the control has transparent sections. Backbuffer will be rendered using the alpha layer.
    property IsTransparent : boolean read fIsTransparent write fIsTransparent;

    // Override the Paint() method to draw your control.
    procedure Paint(const r:TRect); virtual;

    procedure PaintToDc(DC: HDC); override;
  end;


implementation

uses
  FarScape.Assistant.Bitmap;

{ TFarScapeBitmapBufferControl }

constructor TFarScapeBitmapBufferControl.Create;
begin
  inherited;
  fBackBuffer := TBitmap.Create;
  fIsTransparent := true;
end;

destructor TFarScapeBitmapBufferControl.Destroy;
begin
  inherited;
  fBackBuffer.Free;
end;

procedure TFarScapeBitmapBufferControl.Paint(const r: TRect);
begin

end;

procedure TFarScapeBitmapBufferControl.PaintToDc(DC: HDC);
var
  ar : TRect;
  BlendFunction: TBlendFunction;
begin
  inherited;
  Paint(Rect(0,0,Width,Height));
  ar := self.GetAbsoluteRect;

  if IsTransparent then
  begin
    assert(BackBuffer.AlphaFormat = afPreMultiplied);

    //== BLIT with Alpha ==
    BlendFunction.BlendOp := AC_SRC_OVER;
    BlendFunction.BlendFlags := 0;
    BlendFunction.SourceConstantAlpha := 255;
    BlendFunction.AlphaFormat := AC_SRC_ALPHA;

    WinAlphaBlend(DC, ar.Left, ar.Top, ar.Width, ar.Height, BackBuffer.Canvas.Handle, 0, 0, Width, Height, BlendFunction);
  end else
  begin
    //== BLIT ==
    BitBlt(DC, ar.Left, ar.Top, ar.Width, ar.Height, BackBuffer.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;

procedure TFarScapeBitmapBufferControl.ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight: integer);
begin
  inherited;
  fBackBuffer.PixelFormat := pf32Bit;

  if (aWidth > 0) and (aHeight > 0)
    then fBackBuffer.SetSize(aWidth, aHeight)
    else fBackBuffer.SetSize(1,1);

  //Important: Set afPreMultiplied after setting pixel data.
  fBackBuffer.AlphaFormat := afPreMultiplied;
end;

end.
