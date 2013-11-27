program PatternPerspective;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggTransBilinear in '..\..\Source\AggTransBilinear.pas',
  AggTransPerspective in '..\..\Source\AggTransPerspective.pas',
  AggSpanPattern in '..\..\Source\AggSpanPattern.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggImageFilters in '..\..\Source\AggImageFilters.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanInterpolatorTrans in '..\..\Source\AggSpanInterpolatorTrans.pas',
  AggSpanImageFilter in '..\..\Source\AggSpanImageFilter.pas',
  AggSpanPatternFilterRgb in '..\..\Source\AggSpanPatternFilterRgb.pas',
  AggInteractivePolygon;

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FQuad: TInteractivePolygon;
    FRadioBoxTransType: TAggControlRadioBox;
    FTestFlag: Boolean;

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLineUnpacked8;

    FBounds: TRectDouble;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLineUnpacked8.Create;

  FQuad := TInteractivePolygon.Create(4, 5.0);
  FRadioBoxTransType := TAggControlRadioBox.Create(460, 5.0, 420 + 170.0, 60.0,
    not FlipY);

  FTestFlag := False;

  FRadioBoxTransType.SetTextSize(8);
  FRadioBoxTransType.TextThickness := 1;

  FRadioBoxTransType.AddItem('Affine');
  FRadioBoxTransType.AddItem('Bilinear');
  FRadioBoxTransType.AddItem('Perspective');
  FRadioBoxTransType.SetCurrentItem(2);

  AddControl(FRadioBoxTransType);
end;

destructor TAggApplication.Destroy;
begin
  FQuad.Free;
  FRadioBoxTransType.Free;

  FScanLine.Free;
  FRasterizer.Free;

  inherited;
end;

procedure TAggApplication.OnInit;
var
  Trans: TRectDouble;
  Delta: TPointDouble;
begin
  FBounds := RectDouble(-150, -150, 150, 150);
  Trans := RectDouble(-200, -200, 200, 200);
  Delta := PointDouble(0.5 * (Width - (Trans.X2 + Trans.X1)),
    0.5 * (Height - (Trans.Y2 + Trans.Y1)));

  FQuad.Xn[0] := Floor(Trans.X1 + Delta.X);
  FQuad.Yn[0] := Floor(Trans.Y1 + Delta.Y);
  FQuad.Xn[1] := Floor(Trans.X2 + Delta.X); // - 150;
  FQuad.Yn[1] := Floor(Trans.Y1 + Delta.Y); // + 150;
  FQuad.Xn[2] := Floor(Trans.X2 + Delta.X);
  FQuad.Yn[2] := Floor(Trans.Y2 + Delta.Y);
  FQuad.Xn[3] := Floor(Trans.X1 + Delta.X);
  FQuad.Yn[3] := Floor(Trans.Y2 + Delta.Y);
end;

procedure TAggApplication.OnDraw;
const
  CSubdivShift = 2;
var
  Pixf, PixfPre: TAggPixelFormatProcessor;

  RendererBase, RendererBasePre: TAggRendererBase;

  Rgba: TAggColor;

  RenSpan: TAggRendererScanLineAASolid;
  SpanAllocator: TAggSpanAllocator;
  Fi: TAggImageFilterHanning;
  Sg: TAggSpanImageFilter;
  Ri: TAggRendererScanLineAA;
  Wx, Wy: TAggWrapModeReflectAutoPow2;

  ScanInterpolator: TAggSpanInterpolatorLinear;
  ScanInterpolatorSubdiv: TAggSpanInterpolatorLinearSubdiv;

  Filter: TAggImageFilter;

  Tr : TAggTransAffine;
  Trb: TAggTransBilinear;
  Trp: TAggTransPerspective23;
  Pd: PPointDouble;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);
  PixelFormatBgr24Pre(PixfPre, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  RendererBasePre := TAggRendererBase.Create(PixfPre, True);
  try
    RenSpan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      if not FTestFlag then
        RendererBase.Clear(CRgba8White);

      if FRadioBoxTransType.GetCurrentItem = 0 then
      begin
        // For the affine parallelogram transformations we
        // calculate the 4-th (implicit) point of the parallelogram
        FQuad.Xn[3] := FQuad.Xn[0] + (FQuad.Xn[2] - FQuad.Xn[1]);
        FQuad.Yn[3] := FQuad.Yn[0] + (FQuad.Yn[2] - FQuad.Yn[1]);
      end;

      if not FTestFlag then
      begin
        // Render the "quad" tool
        FRasterizer.AddPath(FQuad);

        Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.6);
        RenSpan.SetColor(@Rgba);
        RenderScanLines(FRasterizer, FScanLine, RenSpan);

        // Render the controls
        RenderControl(FRasterizer, FScanLine, RenSpan, FRadioBoxTransType);
      end;

      // Prepare the polygon to rasterize. Here we need to fill
      // the destination (transformed) polygon.
      FRasterizer.SetClipBox(0, 0, Width, Height);
      FRasterizer.Reset;
      FRasterizer.MoveToDouble(FQuad.Point[0]);
      FRasterizer.LineToDouble(FQuad.Point[1]);
      FRasterizer.LineToDouble(FQuad.Point[2]);
      FRasterizer.LineToDouble(FQuad.Point[3]);

      SpanAllocator := TAggSpanAllocator.Create;
      Fi := TAggImageFilterHanning.Create;
      Filter := TAggImageFilter.Create(Fi);

      // Render
      Wx := TAggWrapModeReflectAutoPow2.Create;
      Wy := TAggWrapModeReflectAutoPow2.Create;

      case FRadioBoxTransType.GetCurrentItem of
        0:
          begin
            // Note that we consruct an affine matrix that transforms
            // a parallelogram to a rectangle, i.e., it's inverted.
            // It's actually the same as:
            // Tr := TAggTransAffine.Create(FBounds.X1, FBounds.Y1, FBounds.X2,
            //   FBounds.Y2, FQuad.Polygon);
            // Tr.Invert;
            Tr := TAggTransAffine.Create(PAggParallelogram(FQuad.Polygon), FBounds.X1, FBounds.Y1,
              FBounds.X2, FBounds.Y2);
            try
              // Also note that we can use the linear InterPolator instead of
              // arbitrary TAggSpanInterpolatorTrans. It works much faster,
              // but the transformations must be linear and parellel.
              ScanInterpolator := TAggSpanInterpolatorLinear.Create(Tr);
              try
                Sg := TAggSpanPatternFilterRgb2x2.Create(SpanAllocator,
                  RenderingBufferImage[0], ScanInterpolator, Filter, Wx, Wy,
                  CAggOrderBgr);

                Ri := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                try
                  RenderScanLines(FRasterizer, FScanLine, Ri);
                finally
                  Ri.Free;
                end;
              finally
                ScanInterpolator.Free;
              end;
            finally
              Tr.Free;
            end;
          end;

        1:
          begin
            Pd := FQuad.Polygon;
            Trb := TAggTransBilinear.Create(PQuadDouble(Pd), FBounds);
            try
              if Trb.IsValid then
              begin
                ScanInterpolator := TAggSpanInterpolatorLinear.Create(Trb);
                try
                  Sg := TAggSpanPatternFilterRgb2x2.Create(SpanAllocator,
                    RenderingBufferImage[0], ScanInterpolator, Filter, Wx, Wy,
                    CAggOrderBgr);

                  Ri := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                  try
                    RenderScanLines(FRasterizer, FScanLine, Ri);
                  finally
                    Ri.Free;
                  end;
                finally
                  ScanInterpolator.Free;
                end;
              end;
            finally
              Trb.Free;
            end;
          end;

        2:
          begin
            Pd := FQuad.Polygon;
            Trp := TAggTransPerspective23.Create(PQuadDouble(Pd), FBounds);
            try
              if Trp.IsValid then
              begin
                ScanInterpolatorSubdiv := TAggSpanInterpolatorLinearSubdiv.Create(
                  Trp);
                try
                  Sg := TAggSpanPatternFilterRgb2x2.Create(SpanAllocator,
                    RenderingBufferImage[0], ScanInterpolatorSubdiv, Filter, Wx,
                    Wy, CAggOrderBgr);

                  Ri := TAggRendererScanLineAA.Create(RendererBasePre, Sg);
                  try
                    RenderScanLines(FRasterizer, FScanLine, Ri);
                  finally
                    Ri.Free;
                  end;
                finally
                  ScanInterpolatorSubdiv.Free;
                end;
              end;
            finally
              Trp.Free;
            end;
          end;
      end;

      // Free AGG resources
      SpanAllocator.Free;
      Filter.Free;

      if Sg <> nil then
        Sg.Free;

      Fi.Free;

    finally
      RenSpan.Free;
    end;
  finally
    Wx.Free;
    Wy.Free;
    RendererBase.Free;
    RendererBasePre.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FQuad.OnMouseMove(X, Y) then
      ForceRedraw;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FQuad.OnMouseButtonDown(X, Y) then
      ForceRedraw
    else
    begin
      StartTimer;

      FTestFlag := True;

      OnDraw;
      OnDraw;
      OnDraw;
      OnDraw;

      FTestFlag := False;

      ForceRedraw;

      DisplayMessage(Format('time=%.3f', [GetElapsedTime]));
    end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FQuad.OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Pattern perspective transformations. Essentially it''s the '
      + 'same as Demo "image_perspective", but working with a repeating '
      + 'pattern. Can be used for texturing.'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse button to move and distort the pattern.'
      + 'Click the left mouse outside the pattern to run the performance '
      + 'test.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  Text: AnsiString;
  ImageName, P, N, X: ShortString;

begin
  ImageName := 'agg';

{$IFDEF WIN32}
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    ImageName := FoldName(P, N, '');
  end;
{$ENDIF}

  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Pattern Perspective Transformations (F1-Help)';

    if not LoadImage(0, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'spheres' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(600, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
