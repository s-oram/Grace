unit FarScape.RootControl;

interface

uses
  Agg2d,
  Windows,
  Graphics,
  Types,
  FarScape.Assistant.Bitmap,
  FarScape.CustomControl,
  FarScape.Scene,
  FarScape.UserInteraction;

type
  TFarScapeRootControl = class(TFarScapeAbstractRoot)
  private
    FSharedAgg2d : TAgg2d;
    FBackBuffer : TBitmap;
    FDrawBuffer : TBitmap;
    FScene : TScene;
    procedure ResetSharedDrawingContext;
  protected
    procedure ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight : integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property BackBuffer : TBitmap read fBackBuffer;

    procedure PaintRegion(const ClipBox : TRect);

    property Scene : TScene read fScene;
  end;

implementation

uses
  SysUtils,
  VamLib.WinUtils,
  AggBasics;

{ TFarScapeRootControl }

constructor TFarScapeRootControl.Create;
begin
  inherited;

  fBackBuffer := TBitmap.Create;
  FDrawBuffer := TBitmap.Create;

  fScene := TScene.Create(self);

  FSharedAgg2d := TAgg2d.Create;
end;

destructor TFarScapeRootControl.Destroy;
begin
  FSharedAgg2d.Free;
  FreeAndNil(FBackBuffer);
  FreeAndNil(FDrawBuffer);
  FreeAndNil(FScene);
  inherited;
end;

procedure TFarScapeRootControl.ControlBoundsChanged(const aLeft, aTop, aWidth, aHeight: integer);
var
  BufferMem : Pointer;
  Wx, Hx : cardinal;
  Stride : integer;
begin
  inherited;
  fBackBuffer.PixelFormat := pf32Bit;
  FDrawBuffer.PixelFormat := pf32Bit;

  if (aWidth > 0) and (aHeight > 0) then
  begin
    fBackBuffer.SetSize(aWidth, aHeight);
    FDrawBuffer.SetSize(aWidth, aHeight);
  end else
  begin
    fBackBuffer.SetSize(1,1);
    FDrawBuffer.SetSize(1,1);
  end;

  //Important: Set afPreMultiplied after setting pixel data.
  FBackBuffer.AlphaFormat := afPreMultiplied;
  FDrawBuffer.AlphaFormat := afPreMultiplied;

  // NOTE: It might be better to release the FSharedAgg2d buffer interface if the control bounds is zero by zero pixels.
  // Right now, the control is assuming that the back buffer will always have a minimum size.
  // Zero sized controls would also require limiting drawing operations.
  BufferMem := FDrawBuffer.ScanLine[FDrawBuffer.Height-1];
  Wx := FDrawBuffer.Width;
  Hx := FDrawBuffer.Height;
  Stride := -Wx * 4; // AFAIK Stride is the pixel width * pixel byte size. (4 bytes for 32 bit pixels)
  FSharedAgg2d.Attach(BufferMem, Wx, Hx, Stride);
end;

procedure TFarScapeRootControl.PaintRegion(const ClipBox: TRect);
var
  c1: Integer;
  fsc : TFarScapeControl;
  c : TColor;
  FDrawInfo : TRootDrawInfo;
  Element : TSceneElement;
  WorldBounds : TRectDouble;
  ScreenBounds : TRectDouble;
  x1,y1,x2,y2 : integer;
  DstRect, SrcRect : TRect;
  BlendFunction: TBlendFunction;
begin
  c := RGB(55,55,55);
  BackBuffer.Canvas.Brush.Color := c;
  BackBuffer.Canvas.Brush.Style := TBrushStyle.bsSolid;
  BackBuffer.Canvas.Pen.Style := TPenStyle.psClear;
  BackBuffer.Canvas.Rectangle(ClipBox.Left, ClipBox.Top, ClipBox.Right, ClipBox.Bottom);

  // TODO:HIGH shouldn't need to rebuild the scene every time a region is painted.
  fScene.RebuildScene;

  for c1 := 0 to fScene.ElementCount-1 do
  begin
    Element := fScene.Element[c1];
    fsc := fScene.Element[c1].Control;
    // TODO:MED this size check should be pre-calculated and
    // combined with other checks when building the scene graph.
    if (fsc.Width > 0) and (fsc.Height > 0) then
    begin
      if fsc.DrawMethod = dmNewSchool then
      begin
        ResetSharedDrawingContext;

        // 1) paint using the new PaintToBackBuffer() method.
        FDrawInfo.BackBuffer := self.FDrawBuffer;
        FDrawInfo.Agg2d      := self.FSharedAgg2d;
        FSharedAgg2d.ClipBox(0,0,fsc.Width-1,fsc.Height-1);
        fsc.PaintToBackBuffer(@FDrawInfo);

        //== BLIT with Alpha ==
        SrcRect.Create(0,0,fsc.Width-1,fsc.Height-1);
        DstRect.Create(Element.AbsoluteBoundsRect.Left, Element.AbsoluteBoundsRect.Top, Element.AbsoluteBoundsRect.Right, Element.AbsoluteBoundsRect.Bottom);

        BlendFunction.BlendOp := AC_SRC_OVER;
        BlendFunction.BlendFlags := 0;
        BlendFunction.SourceConstantAlpha := 255;
        BlendFunction.AlphaFormat := AC_SRC_ALPHA;
        WinAlphaBlend(BackBuffer.Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Width, DstRect.Height, FDrawBuffer.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Width, SrcRect.Height, BlendFunction);
      end else
      begin
        // TODO:MED phase out dependence on the PaintToDc() method below.
        // 2) paint control using the old PaintToDc() method.
        fsc.PaintToDc(BackBuffer.Canvas.Handle);
      end;
    end;
  end;
end;



procedure TFarScapeRootControl.ResetSharedDrawingContext;
begin
  BitmapAssistant.ClearAll(FDrawBuffer, clBlack, 0);
  FSharedAgg2d.NoFill;
  FSharedAgg2d.NoLine;
  FSharedAgg2d.LineWidth := 1;
  FSharedAgg2d.LineCap := TAggLineCap.lcRound;
  FSharedAgg2d.LineJoin := TAggLineJoin.ljRound;
  FSharedAgg2d.ResetPath;
  FSharedAgg2d.RemoveAllDashes;
  FSharedAgg2d.ResetTransformations;
  FSharedAgg2d.SetImageBlendColor(0,0,0,0);

  // TODO:MED what are good values for these settings.
  //FSharedAgg2d.AntiAliasGamma
  //FSharedAgg2d.BlendMode
  //FSharedAgg2d.ImageFilter
end;

end.
