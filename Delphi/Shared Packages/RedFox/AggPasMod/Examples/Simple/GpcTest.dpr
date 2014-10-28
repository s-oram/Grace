program GpcTest;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererPrimitives in '..\..\Source\AggRendererPrimitives.pas',
  AggRasterizerScanLine in '..\..\Source\AggRasterizerScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggMathStroke in '..\..\Source\AggMathStroke.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggSpanSolid in '..\..\Source\AggSpanSolid.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvClipPolygon in '..\..\Source\AggConvClipPolygon.pas',
  AggConvGPC in '..\..\Source\AggConvGPC.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggSpiral in '..\..\Source\AggSpiral.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',

  AggMakeGreatBritainPolygon,
  AggMakeArrows;

const
  CFlipY = True;

type
  TAggConvPolyCounter = class(TAggVertexSource)
  private
    FSource: TAggVertexSource;
    FContours, FPoints: Cardinal;
  public
    constructor Create(Src: TAggVertexSource);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FRadioBoxPolygons, FRadioBoxOperation: TAggControlRadioBox;
    FX, FY: Double;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure PerformRendering(ScanLine: TAggCustomScanLine;
      Rasterizer: TAggRasterizerScanLine; Ren: TAggCustomRendererScanLineSolid;
      Gpc: TAggConvGpc);

    function RenderGpc(ScanLine: TAggCustomScanLine; Rasterizer: TAggRasterizerScanLine): Cardinal;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;

    procedure StressTest;
  end;


{ TAggConvPolyCounter }

constructor TAggConvPolyCounter.Create;
begin
  FSource := Src;

  FContours := 0;
  FPoints := 0;
end;

procedure TAggConvPolyCounter.Rewind(PathID: Cardinal);
begin
  FContours := 0;
  FPoints := 0;

  FSource.Rewind(PathID);
end;

function TAggConvPolyCounter.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;

begin
  Cmd := FSource.Vertex(X, Y);

  if IsVertex(Cmd) then
    Inc(FPoints);

  if IsMoveTo(Cmd) then
    Inc(FContours);

  Result := Cmd;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FRadioBoxPolygons := TAggControlRadioBox.Create(5, 5, 210, 110, not FlipY);
  FRadioBoxOperation := TAggControlRadioBox.Create(555, 5, 635, 130, not FlipY);

  FRadioBoxOperation.AddItem('None');
  FRadioBoxOperation.AddItem('OR');
  FRadioBoxOperation.AddItem('AND');
  FRadioBoxOperation.AddItem('XOR');
  FRadioBoxOperation.AddItem('A-B');
  FRadioBoxOperation.AddItem('B-A');
  FRadioBoxOperation.SetCurrentItem(2);

  AddControl(FRadioBoxOperation);

  FRadioBoxPolygons.AddItem('Two Simple Paths');
  FRadioBoxPolygons.AddItem('Closed Stroke');
  FRadioBoxPolygons.AddItem('Great Britain and Arrows');
  FRadioBoxPolygons.AddItem('Great Britain and Spiral');
  FRadioBoxPolygons.AddItem('Spiral and Glyph');
  FRadioBoxPolygons.SetCurrentItem(3);

  AddControl(FRadioBoxPolygons);
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxPolygons.Free;
  FRadioBoxOperation.Free;

  inherited;
end;

procedure TAggApplication.PerformRendering(ScanLine: TAggCustomScanLine;
  Rasterizer: TAggRasterizerScanLine; Ren: TAggCustomRendererScanLineSolid;
  Gpc: TAggConvGpc);
var
  Counter: TAggConvPolyCounter;

  T1, T2, X, Y: Double;

  Cmd: Cardinal;

  Rgba: TAggColor;
  Txt : TAggGsvText;

  TxtStroke: TAggConvStroke;
begin
  if FRadioBoxOperation.GetCurrentItem > 0 then
  begin
    // Render clipped polygon
    Rasterizer.Reset;

    case FRadioBoxOperation.GetCurrentItem of
      1:
        Gpc.Operation(goOr);
      2:
        Gpc.Operation(goAnd);
      3:
        Gpc.Operation(goXor);
      4:
        Gpc.Operation(goAMinusB);
      5:
        Gpc.Operation(goBMinusA);
    end;

    Counter := TAggConvPolyCounter.Create(Gpc);

    StartTimer;
    Counter.Rewind(0);

    T1 := GetElapsedTime;

    Rasterizer.Reset;
    StartTimer;

    Cmd := Counter.Vertex(@X, @Y);

    while not IsStop(Cmd) do
    begin
      Rasterizer.AddVertex(X, Y, Cmd);

      Cmd := Counter.Vertex(@X, @Y);
    end;

    Rgba.FromRgbaDouble(0.5, 0, 0, 0.5);
    Ren.SetColor(@Rgba);
    RenderScanLines(Rasterizer, ScanLine, Ren);

    T2 := GetElapsedTime;

    // Render information text
    Txt := TAggGsvText.Create;
    TxtStroke := TAggConvStroke.Create(Txt);

    TxtStroke.Width := 1.5;
    TxtStroke.LineCap := lcRound;
    Txt.SetSize(10);
    Txt.SetStartPoint(250, 5);
    Txt.SetText(Format('Contours: %d   Points: %d', [Counter.FContours,
      Counter.FPoints]));

    Rasterizer.AddPath(TxtStroke);
    Ren.SetColor(CRgba8Black);
    RenderScanLines(Rasterizer, ScanLine, Ren);

    Txt.SetStartPoint(250, 20);
    Txt.SetText(Format('GPC=%.3fms Render=%.3fms', [T1, T2]));

    Rasterizer.AddPath(TxtStroke);
    Ren.SetColor(CRgba8Black);
    RenderScanLines(Rasterizer, ScanLine, Ren);

    // Free
    Txt.Free;
    TxtStroke.Free;
    Counter.Free;
  end;
end;

function TAggApplication.RenderGpc(ScanLine: TAggCustomScanLine; Rasterizer: TAggRasterizerScanLine)
  : Cardinal;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;
  RendererBase: TAggRendererBase;

  RenScan: TAggRendererScanLineAASolid;
  PathStorage: array [0..1] of TAggPathStorage;
  GreatBritainPoly, Arrows, Glyph: TAggPathStorage;

  Rgba: TAggColor;
  X, Y: Double;

  Mtx: array [0..1] of TAggTransAffine;

  Stroke, StrokeGreatBritainPoly: TAggConvStroke;

  Trans, TransGreatBritainPoly, TransArrows: TAggConvTransform;

  Curve: TAggConvCurve;

  Spiral : TSpiral;
  Gpc: TAggConvGpc;
begin
  PixelFormatBgr24(PixelFormatProcessor, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      case FRadioBoxPolygons.GetCurrentItem of
        0: // Two simple paths
          begin
            PathStorage[0] := TAggPathStorage.Create;
            PathStorage[1] := TAggPathStorage.Create;

            Gpc := TAggConvGpc.Create(PathStorage[0], PathStorage[1]);

            X := FX - FInitialWidth * 0.5 + 100;
            Y := FY - FInitialHeight * 0.5 + 100;

            PathStorage[0].MoveTo(X + 140, Y + 145);
            PathStorage[0].LineTo(X + 225, Y + 44);
            PathStorage[0].LineTo(X + 296, Y + 219);
            PathStorage[0].ClosePolygon;

            PathStorage[0].LineTo(X + 226, Y + 289);
            PathStorage[0].LineTo(X + 82, Y + 292);

            PathStorage[0].MoveTo(X + 220, Y + 222);
            PathStorage[0].LineTo(X + 363, Y + 249);
            PathStorage[0].LineTo(X + 265, Y + 331);

            PathStorage[0].MoveTo(X + 242, Y + 243);
            PathStorage[0].LineTo(X + 268, Y + 309);
            PathStorage[0].LineTo(X + 325, Y + 261);

            PathStorage[0].MoveTo(X + 259, Y + 259);
            PathStorage[0].LineTo(X + 273, Y + 288);
            PathStorage[0].LineTo(X + 298, Y + 266);

            PathStorage[1].MoveTo(100 + 32, 100 + 77);
            PathStorage[1].LineTo(100 + 473, 100 + 263);
            PathStorage[1].LineTo(100 + 351, 100 + 290);
            PathStorage[1].LineTo(100 + 354, 100 + 374);

            Rasterizer.Reset;
            Rasterizer.AddPath(PathStorage[0]);
            Rgba.FromRgbaDouble(0, 0, 0, 0.1);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);

            Rasterizer.Reset;
            Rasterizer.AddPath(PathStorage[1]);
            Rgba.FromRgbaDouble(0, 0.6, 0, 0.1);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);

            PerformRendering(ScanLine, Rasterizer, RenScan, Gpc);

            PathStorage[0].Free;
            PathStorage[1].Free;
            Gpc.Free;
          end;

        1: // Closed stroke
          begin
            PathStorage[0] := TAggPathStorage.Create;
            PathStorage[1] := TAggPathStorage.Create;
            Stroke := TAggConvStroke.Create(PathStorage[1]);
            Stroke.Width := 10;

            Gpc := TAggConvGpc.Create(PathStorage[0], Stroke);

            X := FX - FInitialWidth * 0.5 + 100;
            Y := FY - FInitialHeight * 0.5 + 100;

            PathStorage[0].MoveTo(X + 140, Y + 145);
            PathStorage[0].LineTo(X + 225, Y + 44);
            PathStorage[0].LineTo(X + 296, Y + 219);
            PathStorage[0].ClosePolygon;

            PathStorage[0].LineTo(X + 226, Y + 289);
            PathStorage[0].LineTo(X + 82, Y + 292);

            PathStorage[0].MoveTo(X + 220 - 50, Y + 222);
            PathStorage[0].LineTo(X + 265 - 50, Y + 331);
            PathStorage[0].LineTo(X + 363 - 50, Y + 249);
            PathStorage[0].ClosePolygon(CAggPathFlagsCcw);

            PathStorage[1].MoveTo(100 + 32, 100 + 77);
            PathStorage[1].LineTo(100 + 473, 100 + 263);
            PathStorage[1].LineTo(100 + 351, 100 + 290);
            PathStorage[1].LineTo(100 + 354, 100 + 374);
            PathStorage[1].ClosePolygon;

            Rasterizer.Reset;
            Rasterizer.AddPath(PathStorage[0]);
            Rgba.FromRgbaDouble(0, 0, 0, 0.1);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);

            Rasterizer.Reset;
            Rasterizer.AddPath(Stroke);
            Rgba.FromRgbaDouble(0, 0.6, 0, 0.1);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);

            PerformRendering(ScanLine, Rasterizer, RenScan, Gpc);

            PathStorage[0].Free;
            PathStorage[1].Free;
            Stroke.Free;
            Gpc.Free;
          end;

        2: // Great Britain and Arrows
          begin
            GreatBritainPoly := TAggPathStorage.Create;
            Arrows := TAggPathStorage.Create;

            MakeGreatBritainPolynom(GreatBritainPoly);
            MakeArrows(Arrows);

            Mtx[0] := TAggTransAffine.Create;
            Mtx[1] := TAggTransAffine.Create;

            Mtx[0].Translate(-1150, -1150);
            Mtx[0].Scale(2);

            Mtx[1].Assign(Mtx[0]);
            Mtx[1].Translate(FX - FInitialWidth * 0.5,
              FY - FInitialHeight * 0.5);

            TransGreatBritainPoly := TAggConvTransform.Create(GreatBritainPoly,
              Mtx[0]);
            TransArrows := TAggConvTransform.Create(Arrows, Mtx[1]);

            Gpc := TAggConvGpc.Create(TransGreatBritainPoly, TransArrows);

            Rasterizer.AddPath(TransGreatBritainPoly);
            Rgba.FromRgbaDouble(0.5, 0.5, 0, 0.1);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);

            StrokeGreatBritainPoly := TAggConvStroke.Create(TransGreatBritainPoly);
            StrokeGreatBritainPoly.Width := 0.1;
            Rasterizer.AddPath(StrokeGreatBritainPoly);
            Rgba.Black;
            RenScan.SetColor(@Rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);

            Rasterizer.AddPath(TransArrows);
            Rgba.FromRgbaDouble(0, 0.5, 0.5, 0.1);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);

            PerformRendering(ScanLine, Rasterizer, RenScan, Gpc);

            GreatBritainPoly.Free;
            Arrows.Free;
            StrokeGreatBritainPoly.Free;
            TransGreatBritainPoly.Free;
            TransArrows.Free;
            Mtx[0].Free;
            Mtx[1].Free;
            Gpc.Free;
          end;

        3: // Great Britain and a Spiral
          begin
            Spiral := TSpiral.Create(FX, FY, 10, 150, 30, 0);
            try
              Stroke := TAggConvStroke.Create(Spiral);
              try
                Stroke.Width := 15;

                GreatBritainPoly := TAggPathStorage.Create;
                MakeGreatBritainPolynom(GreatBritainPoly);

                Mtx[0] := TAggTransAffine.Create;
                Mtx[0].Translate(-1150, -1150);
                Mtx[0].Scale(2);

                TransGreatBritainPoly := TAggConvTransform.Create(GreatBritainPoly,
                  Mtx[0]);

                Gpc := TAggConvGpc.Create(TransGreatBritainPoly, Stroke);

                Rasterizer.AddPath(TransGreatBritainPoly);
                Rgba.FromRgbaDouble(0.5, 0.5, 0, 0.1);
                RenScan.SetColor(@Rgba);
                RenderScanLines(Rasterizer, ScanLine, RenScan);

                StrokeGreatBritainPoly := TAggConvStroke.Create(TransGreatBritainPoly);
                StrokeGreatBritainPoly.Width := 0.1;
                Rasterizer.AddPath(StrokeGreatBritainPoly);
                Rgba.Black;
                RenScan.SetColor(@Rgba);
                RenderScanLines(Rasterizer, ScanLine, RenScan);

                Rasterizer.AddPath(Stroke);
                Rgba.FromRgbaDouble(0, 0.5, 0.5, 0.1);
                RenScan.SetColor(@Rgba);
                RenderScanLines(Rasterizer, ScanLine, RenScan);

                PerformRendering(ScanLine, Rasterizer, RenScan, Gpc);

                Mtx[0].Free;
                GreatBritainPoly.Free;
                StrokeGreatBritainPoly.Free;
                TransGreatBritainPoly.Free;
                Gpc.Free;
              finally
                Stroke.Free;
              end;
            finally
              Spiral.Free;
            end;
          end;

        4: // Spiral and glyph
          begin
            Spiral := TSpiral.Create(FX, FY, 10, 150, 30, 0);
            try
              Stroke := TAggConvStroke.Create(Spiral);
              try
                Stroke.Width := 15;

                Glyph := TAggPathStorage.Create;
                Glyph.MoveTo(28.47, 6.45);
                Glyph.Curve3(21.58, 1.12, 19.82, 0.29);
                Glyph.Curve3(17.19, -0.93, 14.21, -0.93);
                Glyph.Curve3(9.57, -0.93, 6.57, 2.25);
                Glyph.Curve3(3.56, 5.42, 3.56, 10.60);
                Glyph.Curve3(3.56, 13.87, 5.03, 16.26);
                Glyph.Curve3(7.03, 19.58, 11.99, 22.51);
                Glyph.Curve3(16.94, 25.44, 28.47, 29.64);
                Glyph.LineTo(28.47, 31.40);
                Glyph.Curve3(28.47, 38.09, 26.34, 40.58);
                Glyph.Curve3(24.22, 43.07, 20.17, 43.07);
                Glyph.Curve3(17.09, 43.07, 15.28, 41.41);
                Glyph.Curve3(13.43, 39.75, 13.43, 37.60);
                Glyph.LineTo(13.53, 34.77);
                Glyph.Curve3(13.53, 32.52, 12.38, 31.30);
                Glyph.Curve3(11.23, 30.08, 9.38, 30.08);
                Glyph.Curve3(7.57, 30.08, 6.42, 31.35);
                Glyph.Curve3(5.27, 32.62, 5.27, 34.81);
                Glyph.Curve3(5.27, 39.01, 9.57, 42.53);
                Glyph.Curve3(13.87, 46.04, 21.63, 46.04);
                Glyph.Curve3(27.59, 46.04, 31.40, 44.04);
                Glyph.Curve3(34.28, 42.53, 35.64, 39.31);
                Glyph.Curve3(36.52, 37.21, 36.52, 30.71);
                Glyph.LineTo(36.52, 15.53);
                Glyph.Curve3(36.52, 9.13, 36.77, 7.69);
                Glyph.Curve3(37.01, 6.25, 37.57, 5.76);
                Glyph.Curve3(38.13, 5.27, 38.87, 5.27);
                Glyph.Curve3(39.65, 5.27, 40.23, 5.62);
                Glyph.Curve3(41.26, 6.25, 44.19, 9.18);
                Glyph.LineTo(44.19, 6.45);
                Glyph.Curve3(38.72, -0.88, 33.74, -0.88);
                Glyph.Curve3(31.35, -0.88, 29.93, 0.78);
                Glyph.Curve3(28.52, 2.44, 28.47, 6.45);
                Glyph.ClosePolygon;

                Glyph.MoveTo(28.47, 9.62);
                Glyph.LineTo(28.47, 26.66);
                Glyph.Curve3(21.09, 23.73, 18.95, 22.51);
                Glyph.Curve3(15.09, 20.36, 13.43, 18.02);
                Glyph.Curve3(11.77, 15.67, 11.77, 12.89);
                Glyph.Curve3(11.77, 9.38, 13.87, 7.06);
                Glyph.Curve3(15.97, 4.74, 18.70, 4.74);
                Glyph.Curve3(22.41, 4.74, 28.47, 9.62);
                Glyph.ClosePolygon;

                Mtx[0] := TAggTransAffine.Create;
                Mtx[0].Scale(4.0);
                Mtx[0].Translate(220, 200);

                Trans := TAggConvTransform.Create(Glyph, Mtx[0]);
                Curve := TAggConvCurve.Create(Trans);

                Gpc := TAggConvGpc.Create(Stroke, Curve);

                Rasterizer.Reset;
                Rasterizer.AddPath(Stroke);
                Rgba.FromRgbaDouble(0, 0, 0, 0.1);
                RenScan.SetColor(@Rgba);
                RenderScanLines(Rasterizer, ScanLine, RenScan);

                Rasterizer.Reset;
                Rasterizer.AddPath(Curve);
                Rgba.FromRgbaDouble(0, 0.6, 0, 0.1);
                RenScan.SetColor(@Rgba);
                RenderScanLines(Rasterizer, ScanLine, RenScan);

                PerformRendering(ScanLine, Rasterizer, RenScan, Gpc);

                Trans.Free;
                Mtx[0].Free;
                Glyph.Free;
                Curve.Free;
                Gpc.Free;
              finally
                Stroke.Free;
              end;
            finally
              Spiral.Free;
            end;
          end;
      end;

      Result := 0;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnInit;
begin
  FX := Width * 0.5;
  FY := Height * 0.5;
end;

procedure TAggApplication.OnDraw;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;

  RendererBase : TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  ScanLine : TAggScanLineUnpacked8;
  Rasterizer: TAggRasterizerScanLineAA;
begin
  // Initialize structures
  PixelFormatBgr24(PixelFormatProcessor, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      ScanLine := TAggScanLineUnpacked8.Create;
      Rasterizer := TAggRasterizerScanLineAA.Create;

      // Render
      RenderGpc(ScanLine, Rasterizer);

      // Render the controls
      RenderControl(Rasterizer, ScanLine, RenScan, FRadioBoxPolygons);
      RenderControl(Rasterizer, ScanLine, RenScan, FRadioBoxOperation);

      // Free AGG resources
      ScanLine.Free;
      Rasterizer.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    FX := X;
    FY := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    FX := X;
    FY := Y;

    ForceRedraw;
  end;

  if mkfMouseRight in Flags then
    DisplayMessage(Format('%d %d', [X, Y]));
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  case Key of
    Byte('t'), Byte('T'):
      StressTest;
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('General Polygon Clipper by Alan Murta is the most reliable implementation of the '#13
      + 'polygon boolean algebra. It implements Bala R. Vatti''s algorithm of arbitrary '#13
      + 'polygon clipping and allows you to calculate the Union, Intersection, Difference, '#13
      + 'and Exclusive OR between two poly-polygons (i.e., polygonal areas consisted of '#13
      + 'several contours). AGG has a simple wrapper class that can be used in the coordinate '#13
      + 'conversion pipeline. The implementation by Alan Murta has restrictions of using it '#13
      + 'in commercial software, so that, please contact the author to settle the legal issues. '#13
      + 'The example demonstrates the use of GPC. Note, that all operations are done in the '#13
      + 'vectorial representation of the contours before rendering.'#13#13 +
      'How to play with:'#13#13 +
      'You can drag one polygon with the left mouse button pressed.'#13 +
      'Press the "T" key to perform the random polygon clipping stress testing.'#13
      + '(may take some time)' +
      #13#13'Note: F2 key saves current "screenshot" file in this demo''s directory.  ');
end;

// Stress-test.
// Works quite well on random polygons, no crashes, no memory leaks!
// Sometimes takes long to produce the result
procedure TAggApplication.StressTest;
var
  ScanLine : TAggScanLineUnpacked8;
  Rasterizer: TAggRasterizerScanLineAA;

  PixelFormatProcessor: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  Ps1, Ps2: TAggPathStorage;

  Gpc : TAggConvGpc;
  Rgba: TAggColor;

  I, NumPoly1, NumPoly2, J, K, Np, Op: Cardinal;

  Txt: TAggGsvText;

  TxtStroke: TAggConvStroke;

begin
  ScanLine := TAggScanLineUnpacked8.Create;
  Rasterizer := TAggRasterizerScanLineAA.Create;

  PixelFormatBgr24(PixelFormatProcessor, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);

    Ps1 := TAggPathStorage.Create;
    Ps2 := TAggPathStorage.Create;
    Gpc := TAggConvGpc.Create(Ps1, Ps2);

    Txt := TAggGsvText.Create;
    TxtStroke := TAggConvStroke.Create(Txt);

    TxtStroke.Width := 1.5;
    TxtStroke.LineCap := lcRound;
    Txt.SetSize(10);
    Txt.SetStartPoint(5, 5);

    for I := 0 to 999 do
    begin
      RendererBase.Clear(CRgba8White);

      NumPoly1 := Random(10) mod 10 + 1;
      NumPoly2 := Random(10) mod 10 + 1;

      Ps1.RemoveAll;
      Ps2.RemoveAll;

      for J := 0 to NumPoly1 - 1 do
      begin
        Ps1.MoveTo(RandomMinMax(0, Width), RandomMinMax(0, Height));

        Np := Random(20) + 2;

        for K := 0 to Np - 1 do
          Ps1.LineTo(RandomMinMax(0, Width), RandomMinMax(0, Height));
      end;

      for J := 0 to NumPoly2 - 1 do
      begin
        Ps2.MoveTo(RandomMinMax(0, Width), RandomMinMax(0, Height));

        Np := Random(20) + 2;

        for K := 0 to Np - 1 do
          Ps2.LineTo(RandomMinMax(0, Width), RandomMinMax(0, Height));
      end;

      Op := Random(5);

      case Op of
        0:
          Gpc.Operation(goOr);

        1:
          Gpc.Operation(goAnd);

        2:
          Gpc.Operation(goXor);

        3:
          Gpc.Operation(goAMinusB);

      else
        Gpc.Operation(goBMinusA);
      end;

      // Clipping result
      Rasterizer.AddPath(Gpc);
      Rgba.FromRgbaDouble(0.5, 0, 0, 0.5);
      RenScan.SetColor(@Rgba);
      RenderScanLines(Rasterizer, ScanLine, RenScan);

      // Counter display
      Txt.SetText(Format('%d / 1000', [I + 1]));

      Txt.SetStartPoint(5, 5);

      Rasterizer.AddPath(TxtStroke);
      RenScan.SetColor(CRgba8Black);
      RenderScanLines(Rasterizer, ScanLine, RenScan);

      // Refresh
      UpdateWindow;
    end;

    DisplayMessage('Done');

    Ps1.Free;
    Ps2.Free;
    Gpc.Free;

    ScanLine.Free;
    Rasterizer.Free;

    Txt.Free;
    TxtStroke.Free;

    ForceRedraw;
  finally
    RendererBase.Free;
  end;
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. General Polygon Clipping - GPC (F1-Help)';

    if Init(640, 520, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
