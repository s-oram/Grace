program AlphaMask3;

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
  AggPixelFormatGray in '..\..\Source\AggPixelFormatGray.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererPrimitives in '..\..\Source\AggRendererPrimitives.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggPixelFormatAlphaMaskAdaptor in '..\..\Source\AggPixelFormatAlphaMaskAdaptor.pas',
  AggAlphaMaskUnpacked8 in '..\..\Source\AggAlphaMaskUnpacked8.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas',
  AggSpiral in '..\..\Source\AggSpiral.pas',

  AggMakeGreatBritainPolygon in 'AggMakeGreatBritainPolygon.pas',
  AggMakeArrows in 'AggMakeArrows.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FRadioButtonPolygons, FRadioButtonOperation: TAggControlRadioBox;

    FAlphaBuf: PInt8u;
    FAlphaAloc: Cardinal;
    FAlphaMaskRenderingBuffer: TAggRenderingBuffer;
    FAlphaMask: TAggAlphaMaskNoClipGray8;

    FRasterizerScanLine: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLinePacked8;

    FPixelFormat: TAggPixelFormatProcessor;

    FCenter: TPointDouble;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure DrawText(X, Y: Double; Str: AnsiString);

    procedure GenerateAlphaMask(Vs: TAggVertexSource);
    procedure PerformRendering(Vs: TAggVertexSource);

    function Render: Cardinal;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FRadioButtonPolygons := TAggControlRadioBox.Create(5, 5, 210, 110, not FlipY);
  FRadioButtonOperation := TAggControlRadioBox.Create(555, 5, 635, 55,
    not FlipY);

  FAlphaBuf := nil;
  FAlphaAloc := 0;

  FAlphaMaskRenderingBuffer := TAggRenderingBuffer.Create;
  FAlphaMask := TAggAlphaMaskNoClipGray8.Create(FAlphaMaskRenderingBuffer);

  FRasterizerScanLine := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;

  FCenter := PointDouble(0);

  FRadioButtonOperation.AddItem('AND');
  FRadioButtonOperation.AddItem('SUB');
  FRadioButtonOperation.SetCurrentItem(0);
  AddControl(FRadioButtonOperation);
  FRadioButtonOperation.NoTransform;

  FRadioButtonPolygons.AddItem('Two Simple Paths');
  FRadioButtonPolygons.AddItem('Closed Stroke');
  FRadioButtonPolygons.AddItem('Great Britain and Arrows');
  FRadioButtonPolygons.AddItem('Great Britain and Spiral');
  FRadioButtonPolygons.AddItem('Spiral and Glyph');
  FRadioButtonPolygons.SetCurrentItem(3);
  AddControl(FRadioButtonPolygons);
  FRadioButtonPolygons.NoTransform;


  PixelFormatBgr24(FPixelFormat, RenderingBufferWindow);
end;

destructor TAggApplication.Destroy;
begin
  FRadioButtonPolygons.Free;
  FRadioButtonOperation.Free;

  FAlphaMaskRenderingBuffer.Free;
  FAlphaMask.Free;

  FRasterizerScanLine.Free;
  FScanLine.Free;

  FPixelFormat.Free;

  AggFreeMem(Pointer(FAlphaBuf), FAlphaAloc);

  inherited;
end;

procedure TAggApplication.DrawText(X, Y: Double; Str: AnsiString);
var
  Pixf : TAggPixelFormatProcessor;
  RendererBase : TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Txt: TAggGsvText;
  TxtStroke: TAggConvStroke;
  Rgba: TAggColor;
begin
  PixelFormatBgr24(Pixf, RenderingBufferWindow);
  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      Txt := TAggGsvText.Create;
      try
        TxtStroke := TAggConvStroke.Create(Txt);
        try
          TxtStroke.Width := 1.5;
          TxtStroke.LineCap := lcRound;

          Txt.SetSize(10.0);
          Txt.SetStartPoint(X, Y);
          Txt.SetText(PAnsiChar(Str));

          FRasterizerScanLine.AddPath(TxtStroke);
        finally
          TxtStroke.Free;
        end;
      finally
        Txt.Free;
      end;

      RenScan.SetColor(CRgba8Black);
      RenderScanLines(FRasterizerScanLine, FScanLine, RenScan);
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.GenerateAlphaMask(Vs: TAggVertexSource);
var
  Cx, Cy: Cardinal;

  Pixf: TAggPixelFormatProcessor;
  Gray: TAggColor;

  Rb : TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;

  T1 : Double;
begin
  Cx := Trunc(Width);
  Cy := Trunc(Height);

  AggFreeMem(Pointer(FAlphaBuf), FAlphaAloc);

  FAlphaAloc := Cx * Cy;

  AggGetMem(Pointer(FAlphaBuf), FAlphaAloc);

  FAlphaMaskRenderingBuffer.Attach(FAlphaBuf, Cx, Cy, Cx);

  PixelFormatGray8(Pixf, FAlphaMaskRenderingBuffer);
  Rb := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(Rb);
    try
      StartTimer;

      if FRadioButtonOperation.GetCurrentItem = 0 then
      begin
        Gray.FromValueInteger(0);
        Rb.Clear(@Gray);

        Gray.FromValueInteger(255);
        RenScan.SetColor(@Gray);
      end
      else
      begin
        Gray.FromValueInteger(255);
        Rb.Clear(@Gray);

        Gray.FromValueInteger(0);
        RenScan.SetColor(@Gray);
      end;

      FRasterizerScanLine.AddPath(Vs);
      RenderScanLines(FRasterizerScanLine, FScanLine, RenScan);

      T1 := GetElapsedTime;

      DrawText(250, 20, Format('Generate AlphaMask: %.3fms', [T1]));
    finally
      RenScan.Free;
    end;
  finally
    Rb.Free;
  end;
end;

procedure TAggApplication.PerformRendering(Vs: TAggVertexSource);
var
  Pixf : TAggPixelFormatProcessor;
  Pixfa: TAggPixelFormatProcessorAlphaMaskAdaptor;
  RendererBase: TAggRendererBase;

  RenScan: TAggRendererScanLineAASolid;
  Rgba: TAggColor;

  T1 : Double;
begin
  PixelFormatBgr24(Pixf, RenderingBufferWindow);
  try
    Pixfa := TAggPixelFormatProcessorAlphaMaskAdaptor.Create(Pixf, FAlphaMask);
    try
      RendererBase := TAggRendererBase.Create(Pixfa);
      try
        RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
        try
          Rgba.FromRgbaDouble(0.5, 0.0, 0, 0.5);
          RenScan.SetColor(@Rgba);

          StartTimer;
          FRasterizerScanLine.Reset;
          FRasterizerScanLine.AddPath(Vs);
          RenderScanLines(FRasterizerScanLine, FScanLine, RenScan);

          T1 := GetElapsedTime;

          DrawText(250, 5, Format('Render with AlphaMask: %.3fms', [T1]));
        finally
          RenScan.Free;
        end;
      finally
        RendererBase.Free;
      end;
    finally
      Pixfa.Free;
    end;
  finally
    Pixf.Free;
  end;
end;

function TAggApplication.Render: Cardinal;
var
  RendererBase: TAggRendererBase;

  RendererScanLine: TAggRendererScanLineAASolid;
  PathStorage: array [0..1] of TAggPathStorage;
  GreatBritainPolygon, Arrows, Glyph: TAggPathStorage;

  Rgba: TAggColor;
  X, Y: Double;

  Mtx: array [0..1] of TAggTransAffine;

  Stroke, StrokeGreatBritainPolygon: TAggConvStroke;

  Trans, TransGreatBritainPolygon, TransArrows: TAggConvTransform;
  Curve: TAggConvCurve;
  Sp: TSpiral;
begin
  RendererBase := TAggRendererBase.Create(FPixelFormat);
  try
    RendererScanLine := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      case FRadioButtonPolygons.GetCurrentItem of
        0: // Two simple paths
          begin
            PathStorage[0] := TAggPathStorage.Create;
            PathStorage[1] := TAggPathStorage.Create;
            try
              X := FCenter.X - InitialWidth * 0.5 + 100;
              Y := FCenter.Y - InitialHeight * 0.5 + 100;

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

              FRasterizerScanLine.Reset;
              FRasterizerScanLine.AddPath(PathStorage[0]);
              Rgba.FromRgbaDouble(0, 0, 0, 0.1);
              RendererScanLine.SetColor(@Rgba);
              RenderScanLines(FRasterizerScanLine, FScanLine, RendererScanLine);

              FRasterizerScanLine.Reset;
              FRasterizerScanLine.AddPath(PathStorage[1]);
              Rgba.FromRgbaDouble(0, 0.6, 0, 0.1);
              RendererScanLine.SetColor(@Rgba);
              RenderScanLines(FRasterizerScanLine, FScanLine, RendererScanLine);

              GenerateAlphaMask(PathStorage[0]);
              PerformRendering(PathStorage[1]);
            finally
              PathStorage[0].Free;
              PathStorage[1].Free;
            end;
          end;

        1: // Closed stroke
          begin
            PathStorage[0] := TAggPathStorage.Create;
            PathStorage[1] := TAggPathStorage.Create;
            try
              Stroke := TAggConvStroke.Create(PathStorage[1]);
              try
                Stroke.Width := 10.0;

                X := FCenter.X - InitialWidth * 0.5 + 100;
                Y := FCenter.Y - InitialHeight * 0.5 + 100;

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

                FRasterizerScanLine.Reset;
                FRasterizerScanLine.AddPath(PathStorage[0]);
                Rgba.FromRgbaDouble(0, 0, 0, 0.1);
                RendererScanLine.SetColor(@Rgba);
                RenderScanLines(FRasterizerScanLine, FScanLine,
                  RendererScanLine);

                FRasterizerScanLine.Reset;
                FRasterizerScanLine.AddPath(Stroke);
                Rgba.FromRgbaDouble(0, 0.6, 0, 0.1);
                RendererScanLine.SetColor(@Rgba);
                RenderScanLines(FRasterizerScanLine, FScanLine,
                  RendererScanLine);

                GenerateAlphaMask(PathStorage[0]);
                PerformRendering(Stroke);
              finally
                Stroke.Free;
              end;
            finally
              PathStorage[0].Free;
              PathStorage[1].Free;
            end;
          end;

        2: // Great Britain and Arrows
          begin
            GreatBritainPolygon := TAggPathStorage.Create;
            Arrows := TAggPathStorage.Create;

            MakeGreatBritainPolynom(GreatBritainPolygon);
            MakeArrows(Arrows);

            Mtx[0] := TAggTransAffine.Create;
            Mtx[1] := TAggTransAffine.Create;

            Mtx[0].Translate(-1150, -1150);
            Mtx[0].Scale(2.0);

            Mtx[1].Assign(Mtx[0]);

            Mtx[1].Translate(FCenter.X - InitialWidth * 0.5,
              FCenter.Y - InitialHeight * 0.5);

            TransGreatBritainPolygon := TAggConvTransform.Create(
              GreatBritainPolygon, Mtx[0]);
            TransArrows := TAggConvTransform.Create(Arrows, Mtx[1]);

            FRasterizerScanLine.AddPath(TransGreatBritainPolygon);
            Rgba.FromRgbaDouble(0.5, 0.5, 0, 0.1);
            RendererScanLine.SetColor(@Rgba);
            RenderScanLines(FRasterizerScanLine, FScanLine, RendererScanLine);

            StrokeGreatBritainPolygon := TAggConvStroke.Create(
              TransGreatBritainPolygon);
            StrokeGreatBritainPolygon.Width := 0.1;
            FRasterizerScanLine.AddPath(StrokeGreatBritainPolygon);
            Rgba.Black;
            RendererScanLine.SetColor(@Rgba);
            RenderScanLines(FRasterizerScanLine, FScanLine, RendererScanLine);

            FRasterizerScanLine.AddPath(TransArrows);
            Rgba.FromRgbaDouble(0.0, 0.5, 0.5, 0.1);
            RendererScanLine.SetColor(@Rgba);
            RenderScanLines(FRasterizerScanLine, FScanLine, RendererScanLine);

            GenerateAlphaMask(TransGreatBritainPolygon);
            PerformRendering(TransArrows);

            TransGreatBritainPolygon.Free;
            TransArrows.Free;
            GreatBritainPolygon.Free;
            Arrows.Free;
            StrokeGreatBritainPolygon.Free;
            Mtx[0].Free;
            Mtx[1].Free;
          end;

        3: // Great Britain and a Spiral
          begin
            Sp := TSpiral.Create(FCenter.X, FCenter.Y, 10, 150, 30, 0.0);
            try
              Stroke := TAggConvStroke.Create(Sp);
              try
                Stroke.Width := 15.0;

                GreatBritainPolygon := TAggPathStorage.Create;
                MakeGreatBritainPolynom(GreatBritainPolygon);
                try
                  Mtx[0] := TAggTransAffine.Create;
                  try
                    Mtx[0].Translate(-1150, -1150);
                    Mtx[0].Scale(2.0);

                    TransGreatBritainPolygon := TAggConvTransform.Create(
                      GreatBritainPolygon, Mtx[0]);
                    try
                      FRasterizerScanLine.AddPath(TransGreatBritainPolygon);
                      Rgba.FromRgbaDouble(0.5, 0.5, 0, 0.1);
                      RendererScanLine.SetColor(@Rgba);
                      RenderScanLines(FRasterizerScanLine, FScanLine,
                        RendererScanLine);

                      StrokeGreatBritainPolygon := TAggConvStroke.Create(
                        TransGreatBritainPolygon);
                      try
                        StrokeGreatBritainPolygon.Width := 0.1;
                        FRasterizerScanLine.AddPath(StrokeGreatBritainPolygon);
                        Rgba.Black;
                        RendererScanLine.SetColor(@Rgba);
                        RenderScanLines(FRasterizerScanLine, FScanLine,
                          RendererScanLine);

                        FRasterizerScanLine.AddPath(Stroke);
                        Rgba.FromRgbaDouble(0.0, 0.5, 0.5, 0.1);
                        RendererScanLine.SetColor(@Rgba);
                        RenderScanLines(FRasterizerScanLine, FScanLine,
                          RendererScanLine);

                        GenerateAlphaMask(TransGreatBritainPolygon);
                        PerformRendering(Stroke);
                      finally
                        StrokeGreatBritainPolygon.Free;
                      end;
                    finally
                      TransGreatBritainPolygon.Free;
                    end;
                  finally
                    Mtx[0].Free;
                  end;
                finally
                  GreatBritainPolygon.Free;
                end;
              finally
                Stroke.Free;
              end;
            finally
              Sp.Free;
            end;
          end;

        4: // Spiral and glyph
          begin
            Sp := TSpiral.Create(FCenter.X, FCenter.Y, 10, 150, 30, 0.0);
            try
              Stroke := TAggConvStroke.Create(Sp);
              try
                Stroke.Width := 15.0;

                Glyph := TAggPathStorage.Create;
                try
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
                  try
                    Curve := TAggConvCurve.Create(Trans);
                    try
                      FRasterizerScanLine.Reset;
                      FRasterizerScanLine.AddPath(Stroke);
                      Rgba.FromRgbaDouble(0, 0, 0, 0.1);
                      RendererScanLine.SetColor(@Rgba);
                      RenderScanLines(FRasterizerScanLine, FScanLine,
                        RendererScanLine);

                      FRasterizerScanLine.Reset;
                      FRasterizerScanLine.AddPath(Curve);
                      Rgba.FromRgbaDouble(0, 0.6, 0, 0.1);
                      RendererScanLine.SetColor(@Rgba);
                      RenderScanLines(FRasterizerScanLine, FScanLine,
                        RendererScanLine);

                      GenerateAlphaMask(Stroke);
                      PerformRendering(Curve);
                    finally
                      Curve.Free;
                    end;
                  finally
                    Trans.Free;
                  end;
                  Mtx[0].Free;
                finally
                  Glyph.Free;
                end;
              finally
                Stroke.Free;
              end;
            finally
              Sp.Free;
            end;
          end;
      end;
    finally
      RendererScanLine.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnInit;
begin
  FCenter := PointDouble(0.5 * Width, 0.5 * Height);
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase : TAggRendererBase;
  RenSolid: TAggRendererScanLineAASolid;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);
  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenSolid := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Render
      Render;

      // Render the controls
      RenderControl(FRasterizerScanLine, FScanLine, RenSolid,
        FRadioButtonPolygons);
      RenderControl(FRasterizerScanLine, FScanLine, RenSolid,
        FRadioButtonOperation);
    finally
      RenSolid.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    FCenter.X := X;
    FCenter.Y := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    FCenter.X := X;
    FCenter.Y := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Yet another example of alpha-masking. It simulates '
      + 'arbitrary polygon clipping similar to "gpc_test". Alpha-Masking '
      + 'allows you to perform only the Intersection (AND) and Difference '
      + '(SUB) operations, but works much faster that conv_gpc. Actually, '
      + 'there''re different complexities and different dependencies. The '
      + 'performance of conv_gpc depends on the number of vertices, while '
      + 'Alpha-Masking depends on the area of the rendered polygons. Still, '
      + 'with typical screen resolutions, Alpha-Masking works much faster '
      + 'than General Polygon Clipper. Compare the timings between '
      + '"AlphaMask3" and "GpcTest".'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse button to move the upper shape around.'#13
      + 'Use the right mouse button to display a message with current '
      + 'coordinates.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.  ');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Alpha-Mask as a Polygon Clipper (F1-Help)';

    if Init(640, 520, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
