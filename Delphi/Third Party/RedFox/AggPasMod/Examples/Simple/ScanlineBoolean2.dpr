program ScanLineBoolean2;

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

  AggControl in '..\..\Source\Controls\AggControl.pas',
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
  AggScanLineBin in '..\..\Source\AggScanLineBin.pas',
  AggScanLineStorageAA in '..\..\Source\AggScanLineStorageAA.pas',
  AggScanLineStorageBin in '..\..\Source\AggScanLineStorageBin.pas',
  AggScanLineBooleanAlgebra in '..\..\Source\AggScanLineBooleanAlgebra.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggMathStroke in '..\..\Source\AggMathStroke.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggSpanSolid in '..\..\Source\AggSpanSolid.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggSpiral in '..\..\Source\AggSpiral.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',

  AggMakeGreatBritainPolygon,
  AggMakeArrows;

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FRadioBoxPolygons, FRadioBoxFillRule: TAggControlRadioBox;
    FRadioBoxScanLineType, FRadioBoxOperation: TAggControlRadioBox;
    FX, FY: Double;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure RenderScanLineBoolean(Ras1, Ras2: TAggRasterizerScanLine);
    function RenderScanBool(Ras1, Ras2: TAggRasterizerScanLine): Cardinal;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


function CountSpans(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine): Cardinal;
var
  N: Cardinal;
begin
  N := 0;

  if Ras.RewindScanLines then
  begin
    Sl.Reset(Ras.MinimumX, Ras.MaximumX);

    if Sl.IsEmbedded then
      while Ras.SweepScanLineEm(Sl) do
        Inc(N, Sl.NumSpans)
    else
      while Ras.SweepScanLine(Sl) do
        Inc(N, Sl.NumSpans);
  end;

  Result := N;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FRadioBoxPolygons := TAggControlRadioBox.Create(5, 5, 210, 110, not FlipY);
  FRadioBoxFillRule := TAggControlRadioBox.Create(200, 5, 305, 50, not FlipY);
  FRadioBoxScanLineType := TAggControlRadioBox.Create(300, 5, 415, 70,
    not FlipY);
  FRadioBoxOperation := TAggControlRadioBox.Create(535, 5, 650, 145.0,
    not FlipY);

  FRadioBoxOperation.AddItem('None');
  FRadioBoxOperation.AddItem('OR');
  FRadioBoxOperation.AddItem('AND');
  FRadioBoxOperation.AddItem('XOR Linear');
  FRadioBoxOperation.AddItem('XOR Saddle');
  FRadioBoxOperation.AddItem('A-B');
  FRadioBoxOperation.AddItem('B-A');
  FRadioBoxOperation.SetCurrentItem(2);

  AddControl(FRadioBoxOperation);

  FRadioBoxOperation.NoTransform;

  FRadioBoxFillRule.AddItem('Even-Odd');
  FRadioBoxFillRule.AddItem('Non Zero');
  FRadioBoxFillRule.SetCurrentItem(1);

  AddControl(FRadioBoxFillRule);

  FRadioBoxFillRule.NoTransform;

  FRadioBoxScanLineType.AddItem('ScanLine Packed');
  FRadioBoxScanLineType.AddItem('ScanLine Unpacked');
  FRadioBoxScanLineType.AddItem('ScanLine Binary');
  FRadioBoxScanLineType.SetCurrentItem(1);

  AddControl(FRadioBoxScanLineType);

  FRadioBoxScanLineType.NoTransform;

  FRadioBoxPolygons.AddItem('Two Simple Paths');
  FRadioBoxPolygons.AddItem('Closed Stroke');
  FRadioBoxPolygons.AddItem('Great Britain and Arrows');
  FRadioBoxPolygons.AddItem('Great Britain and Spiral');
  FRadioBoxPolygons.AddItem('Spiral and Glyph');
  FRadioBoxPolygons.SetCurrentItem(3);

  AddControl(FRadioBoxPolygons);

  FRadioBoxPolygons.NoTransform;
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxPolygons.Free;
  FRadioBoxFillRule.Free;
  FRadioBoxScanLineType.Free;
  FRadioBoxOperation.Free;

  inherited;
end;

procedure TAggApplication.RenderScanLineBoolean(Ras1,
  Ras2: TAggRasterizerScanLine);
var
  Op: TAggBoolScanLineOp;
  RendererBase: TAggRendererBase;

  PixelFormatProcessor: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  T1, T2: Double;

  I: Integer;

  NumSpans: Cardinal;

  RenP, RenU, RenText: TAggRendererScanLineAASolid;
  RenBin          : TAggRendererScanLineBinSolid;

  Slt : TAggScanLinePacked8;
  Slp: array [0..2] of TAggScanLinePacked8;
  Slu: array [0..2] of TAggScanLineUnpacked8;
  Slb: array [0..2] of TAggScanLineBin;
  Txt: TAggGsvText;

  TextStroke: TAggConvStroke;

  Storage: array [0..2] of TAggScanLineStorageAA8;
  StorageBin: array [0..2] of TAggScanLineStorageBin;
begin
  if FRadioBoxOperation.GetCurrentItem > 0 then
  begin
    case FRadioBoxOperation.GetCurrentItem of
      1:
        Op := bsoOr;
      2:
        Op := bsoAnd;
      3:
        Op := bsoXor;
      4:
        Op := bsoXorSaddle;
      5:
        Op := bsoAMinusB;
      6:
        Op := bsoBMinusA;
    end;

    PixelFormatBgr24(PixelFormatProcessor, RenderingBufferWindow);
    RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
    try
      T1 := 0.0;
      T2 := 0.0;

      NumSpans := 0;

      // Render Clipping
      case FRadioBoxScanLineType.GetCurrentItem of
        0:
          begin
            RenP := TAggRendererScanLineAASolid.Create(RendererBase);
            try
              Slp[0] := TAggScanLinePacked8.Create;
              Slp[1] := TAggScanLinePacked8.Create;
              Slp[2] := TAggScanLinePacked8.Create;
              try

                // The intermediate storage is used only to test the perfoprmance,
                // the short variant can be as follows:
                // ------------------------
                // Rgba.FromRgbaDouble(0.5, 0.0, 0, 0.5);
                // RenP.Color(@rgba );
                // BoolScanLineCombineShapesAA(op, ras1, ras2, Slp[1], Slp[2],
                //   Slp[0], RenP);

                Storage[0] := TAggScanLineStorageAA8.Create;
                Storage[1] := TAggScanLineStorageAA8.Create;
                Storage[2] := TAggScanLineStorageAA8.Create;
                try
                  RenderScanLines(Ras1, Slp[0], Storage[1]);
                  RenderScanLines(Ras2, Slp[0], Storage[2]);

                  StartTimer;

                  for I := 0 to 9 do
                    BoolScanLineCombineShapesAA(Op, Storage[1], Storage[2],
                      Slp[1], Slp[2], Slp[0], Storage[0]);

                  T1 := GetElapsedTime / 10;

                  StartTimer;

                  Rgba.FromRgbaDouble(0.5, 0, 0, 0.5);
                  RenP.SetColor(@Rgba);
                  RenderScanLines(Storage[0], Slp[0], RenP);

                  T2 := GetElapsedTime;

                  NumSpans := CountSpans(Storage[0], Slp[0]);
                finally
                  Storage[0].Free;
                  Storage[1].Free;
                  Storage[2].Free;
                end;
              finally
                Slp[0].Free;
                Slp[1].Free;
                Slp[2].Free;
              end;
            finally
              RenP.Free;
            end;
          end;

        1:
          begin
            RenU := TAggRendererScanLineAASolid.Create(RendererBase);

            Slu[0] := TAggScanLineUnpacked8.Create;
            Slu[1] := TAggScanLineUnpacked8.Create;
            Slu[2] := TAggScanLineUnpacked8.Create;

            { Rgba.FromRgbaDouble(0.5, 0, 0, 0.5);
              Renu.Color(@rgba );
              BoolScanLineCombineShapesAA(op, ras1, ras2, Slu[1], Slu[2],
                Slu[0], Renu ); }

            Storage[0] := TAggScanLineStorageAA8.Create;
            Storage[1] := TAggScanLineStorageAA8.Create;
            Storage[2] := TAggScanLineStorageAA8.Create;

            RenderScanLines(Ras1, Slu[0], Storage[1]);
            RenderScanLines(Ras2, Slu[0], Storage[2]);

            StartTimer;

            for I := 0 to 9 do
              BoolScanLineCombineShapesAA(Op, Storage[1], Storage[2], Slu[1],
                Slu[2], Slu[0], Storage[0]);

            T1 := GetElapsedTime / 10.0;

            StartTimer;

            Rgba.FromRgbaDouble(0.5, 0.0, 0, 0.5);
            Renu.SetColor(@Rgba);
            RenderScanLines(Storage[0], Slu[0], Renu);

            T2 := GetElapsedTime;

            NumSpans := CountSpans(Storage[0], Slu[0]); { }

            Slu[0].Free;
            Slu[1].Free;
            Slu[2].Free;

            Storage[0].Free;
            Storage[1].Free;
            Storage[2].Free;
            RenU.Free;
          end;

        2:
          begin
            RenBin := TAggRendererScanLineBinSolid.Create(RendererBase);

            Slb[0] := TAggScanLineBin.Create;
            Slb[1] := TAggScanLineBin.Create;
            Slb[2] := TAggScanLineBin.Create;

            StorageBin[0] := TAggScanLineStorageBin.Create;
            StorageBin[1] := TAggScanLineStorageBin.Create;
            StorageBin[2] := TAggScanLineStorageBin.Create;

            RenderScanLines(Ras1, Slb[0], StorageBin[1]);
            RenderScanLines(Ras2, Slb[0], StorageBin[2]);

            StartTimer;

            for I := 0 to 9 do
              BoolScanLineCombineShapesBin(Op, StorageBin[1], StorageBin[2],
                Slb[1], Slb[2], Slb[0], StorageBin[0]);

            T1 := GetElapsedTime / 10.0;

            StartTimer;

            Rgba.FromRgbaDouble(1, 0.0, 0);
            RenBin.SetColor(@Rgba);
            RenderScanLines(StorageBin[0], Slb[0], RenBin);

            T2 := GetElapsedTime;

            NumSpans := CountSpans(StorageBin[0], Slb[0]);

            StorageBin[0].Free;
            StorageBin[1].Free;
            StorageBin[2].Free;

            Slb[0].Free;
            Slb[1].Free;
            Slb[2].Free;

            RenBin.Free;
          end;
      end;

      // Render text
      RenText := TAggRendererScanLineAASolid.Create(RendererBase);
      try
        Slt := TAggScanLinePacked8.Create;
        Txt := TAggGsvText.Create;
        TextStroke := TAggConvStroke.Create(Txt);
        TextStroke.Width := 1.0;
        TextStroke.LineCap := lcRound;
        Txt.SetSize(8.0);
        Txt.SetStartPoint(420, 40);
        Txt.SetText(Format('Combine=%.3fms'#13#13 + 'Render=%.3fms'#13#13 +
          'NumSpans=%d', [T1, T2, NumSpans]));

        Ras1.Reset;
        Ras1.AddPath(TextStroke);
        Rgba.FromRgbaDouble(0.0, 0.0, 0.0);
        RenText.SetColor(@Rgba);
        RenderScanLines(Ras1, Slt, RenText);
      finally
        RenText.Free;
      end;
      Slt.Free;
      Txt.Free;
      TextStroke.Free;
    finally
      RendererBase.Free;
    end;
  end;
end;

function TAggApplication.RenderScanBool(Ras1,
  Ras2: TAggRasterizerScanLine): Cardinal;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;
  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Sl: TAggScanLinePacked8;

  GreatBritainPoly, Arrows, Glyph: TAggPathStorage;
  Ps: array [0..1] of TAggPathStorage;

  Rgba: TAggColor;
  X, Y: Double;

  Matrix: array [0..1] of TAggTransAffine;

  Stroke, StrokeGreatBritainPoly: TAggConvStroke;

  Trans, TransGreatBritainPoly, TransArrows: TAggConvTransform;

  Curve: TAggConvCurve;

  Sp: TSpiral;
begin
  PixelFormatBgr24(PixelFormatProcessor, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);

    Sl := TAggScanLinePacked8.Create;

    if FRadioBoxFillRule.GetCurrentItem <> 0 then
      Ras1.FillingRule := frNonZero
    else
      Ras1.FillingRule := frEvenOdd;

    if FRadioBoxFillRule.GetCurrentItem <> 0 then
      Ras2.FillingRule := frNonZero
    else
      Ras2.FillingRule := frEvenOdd;

    case FRadioBoxPolygons.GetCurrentItem of
      0: // Two simple paths
        begin
          Ps[0] := TAggPathStorage.Create;
          Ps[1] := TAggPathStorage.Create;
          try
            X := FX - FInitialWidth * 0.5 + 100;
            Y := FY - FInitialHeight * 0.5 + 100;

            Ps[0].MoveTo(X + 140, Y + 145);
            Ps[0].LineTo(X + 225, Y + 44);
            Ps[0].LineTo(X + 296, Y + 219);
            Ps[0].ClosePolygon;

            Ps[0].LineTo(X + 226, Y + 289);
            Ps[0].LineTo(X + 82, Y + 292);

            Ps[0].MoveTo(X + 220, Y + 222);
            Ps[0].LineTo(X + 363, Y + 249);
            Ps[0].LineTo(X + 265, Y + 331);

            Ps[0].MoveTo(X + 242, Y + 243);
            Ps[0].LineTo(X + 268, Y + 309);
            Ps[0].LineTo(X + 325, Y + 261);

            Ps[0].MoveTo(X + 259, Y + 259);
            Ps[0].LineTo(X + 273, Y + 288);
            Ps[0].LineTo(X + 298, Y + 266);

            Ps[1].MoveTo(100 + 32, 100 + 77);
            Ps[1].LineTo(100 + 473, 100 + 263);
            Ps[1].LineTo(100 + 351, 100 + 290);
            Ps[1].LineTo(100 + 354, 100 + 374);

            Ras1.Reset;
            Ras1.AddPath(Ps[0]);
            Rgba.FromRgbaDouble(0, 0, 0, { 0.1 } 0.0);
            RenScan.SetColor(@Rgba);
            { RenderScanLines(ras1, Sl, Ren); }

            Ras2.Reset;
            Ras2.AddPath(Ps[1]);
            Rgba.FromRgbaDouble(0, 0.6, 0, { 0.1 } 0.0);
            RenScan.SetColor(@Rgba);
            { RenderScanLines(ras2, Sl, Ren); }

            RenderScanLineBoolean(Ras1, Ras2);
          finally
            Ps[0].Free;
            Ps[1].Free;
          end;
        end;

      1: // Closed stroke
        begin
          Ps[0] := TAggPathStorage.Create;
          Ps[1] := TAggPathStorage.Create;
          try
            Stroke := TAggConvStroke.Create(Ps[1]);
            try
              Stroke.Width := 15.0;

              X := FX - FInitialWidth * 0.5 + 100;
              Y := FY - FInitialHeight * 0.5 + 100;

              Ps[0].MoveTo(X + 140, Y + 145);
              Ps[0].LineTo(X + 225, Y + 44);
              Ps[0].LineTo(X + 296, Y + 219);
              Ps[0].ClosePolygon;

              Ps[0].LineTo(X + 226, Y + 289);
              Ps[0].LineTo(X + 82, Y + 292);

              Ps[0].MoveTo(X + 220 - 50, Y + 222);
              Ps[0].LineTo(X + 265 - 50, Y + 331);
              Ps[0].LineTo(X + 363 - 50, Y + 249);
              Ps[0].ClosePolygon(CAggPathFlagsCcw);

              Ps[1].MoveTo(100 + 32, 100 + 77);
              Ps[1].LineTo(100 + 473, 100 + 263);
              Ps[1].LineTo(100 + 351, 100 + 290);
              Ps[1].LineTo(100 + 354, 100 + 374);
              Ps[1].ClosePolygon;

              Ras1.Reset;
              Ras1.AddPath(Ps[0]);
              Rgba.FromRgbaDouble(0, 0, 0, 0.1);
              RenScan.SetColor(@Rgba);
              RenderScanLines(Ras1, Sl, RenScan);

              Ras2.Reset;
              Ras2.AddPath(Stroke);
              Rgba.FromRgbaDouble(0, 0.6, 0, 0.1);
              RenScan.SetColor(@Rgba);
              RenderScanLines(Ras2, Sl, RenScan);

              RenderScanLineBoolean(Ras1, Ras2);
            finally
              Stroke.Free;
            end;
          finally
            Ps[0].Free;
            Ps[1].Free;
          end;
        end;

      2: // Great Britain and Arrows
        begin
          GreatBritainPoly := TAggPathStorage.Create;
          try
            Arrows := TAggPathStorage.Create;

            MakeGreatBritainPolynom(GreatBritainPoly);
            MakeArrows(Arrows);

            Matrix[0] := TAggTransAffine.Create;
            Matrix[1] := TAggTransAffine.Create;
            try
              Matrix[0].Translate(-1150, -1150);
              Matrix[0].Scale(2.0);

              Matrix[1].Assign(Matrix[0]);
              Matrix[1].Translate(FX - FInitialWidth * 0.5,
                FY - FInitialHeight * 0.5);

              TransGreatBritainPoly := TAggConvTransform.Create(GreatBritainPoly,
                Matrix[0]);
              TransArrows := TAggConvTransform.Create(Arrows, Matrix[1]);

              Ras2.AddPath(TransGreatBritainPoly);
              Rgba.FromRgbaDouble(0.5, 0.5, 0, 0.1);
              RenScan.SetColor(@Rgba);
              RenderScanLines(Ras2, Sl, RenScan);

              StrokeGreatBritainPoly := TAggConvStroke.Create(
                TransGreatBritainPoly);
              try
                StrokeGreatBritainPoly.Width := 0.1;
                Ras1.AddPath(StrokeGreatBritainPoly);
                Rgba.Black;
                RenScan.SetColor(@Rgba);
                RenderScanLines(Ras1, Sl, RenScan);

                Ras2.AddPath(TransArrows);
                Rgba.FromRgbaDouble(0.0, 0.5, 0.5, 0.1);
                RenScan.SetColor(@Rgba);
                RenderScanLines(Ras2, Sl, RenScan);

                Ras1.Reset;
                Ras1.AddPath(TransGreatBritainPoly);

                RenderScanLineBoolean(Ras1, Ras2);
              finally
                StrokeGreatBritainPoly.Free;
              end;
            finally
              Matrix[0].Free;
              Matrix[1].Free;
            end;

            TransGreatBritainPoly.Free;
            TransArrows.Free;
            Arrows.Free;
          finally
            GreatBritainPoly.Free;
          end;
        end;

      3: // Great Britain and a Spiral
        begin
          Sp := TSpiral.Create(FX, FY, 10, 150, 30, 0.0);
          try
            Stroke := TAggConvStroke.Create(Sp);
            try
              Stroke.Width := 15.0;

              GreatBritainPoly := TAggPathStorage.Create;
              try
                MakeGreatBritainPolynom(GreatBritainPoly);

                Matrix[0] := TAggTransAffine.Create;

                Matrix[0].Translate(-1150, -1150);
                Matrix[0].Scale(2.0);

                Matrix[0].Multiply(GetTransAffineResizing);

                TransGreatBritainPoly := TAggConvTransform.Create(
                  GreatBritainPoly, Matrix[0]);
                try
                  Ras1.AddPath(TransGreatBritainPoly);
                  Rgba.FromRgbaDouble(0.5, 0.5, 0, 0.1);
                  RenScan.SetColor(@Rgba);
                  RenderScanLines(Ras1, Sl, RenScan);

                  StrokeGreatBritainPoly := TAggConvStroke.Create(
                    TransGreatBritainPoly);
                  try
                    StrokeGreatBritainPoly.Width := 0.1;
                    Ras1.AddPath(StrokeGreatBritainPoly);
                    Rgba.Black;
                    RenScan.SetColor(@Rgba);
                    RenderScanLines(Ras1, Sl, RenScan);

                    Ras2.Reset;
                    Ras2.AddPath(Stroke);
                    Rgba.FromRgbaDouble(0.0, 0.5, 0.5, 0.1);
                    RenScan.SetColor(@Rgba);
                    RenderScanLines(Ras2, Sl, RenScan);

                    Ras1.Reset;
                    Ras1.AddPath(TransGreatBritainPoly);

                    RenderScanLineBoolean(Ras1, Ras2);
                  finally
                    StrokeGreatBritainPoly.Free;
                  end;
                finally
                  TransGreatBritainPoly.Free;
                end;
                Matrix[0].Free;
              finally
                GreatBritainPoly.Free;
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
          Sp := TSpiral.Create(FX, FY, 10, 150, 30, 0.0);
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

                Matrix[0] := TAggTransAffine.Create;
                Matrix[0].Scale(4.0);
                Matrix[0].Translate(220, 200);

                Trans := TAggConvTransform.Create(Glyph, Matrix[0]);
                try
                  Curve := TAggConvCurve.Create(Trans);
                  try
                    Ras1.Reset;
                    Ras1.AddPath(Stroke);
                    Rgba.FromRgbaDouble(0, 0, 0, 0.1);
                    RenScan.SetColor(@Rgba);
                    RenderScanLines(Ras1, Sl, RenScan);

                    Ras2.Reset;
                    Ras2.AddPath(Curve);
                    Rgba.FromRgbaDouble(0, 0.6, 0, 0.1);
                    RenScan.SetColor(@Rgba);
                    RenderScanLines(Ras2, Sl, RenScan);

                    RenderScanLineBoolean(Ras1, Ras2);
                  finally
                    Curve.Free;
                  end;
                finally
                  Trans.Free;
                end;
                Matrix[0].Free;
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

    Sl.Free;
    RenScan.Free;
  finally
    RendererBase.Free;
  end;

  Result := 0;
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
  RenSolid: TAggRendererScanLineAASolid;

  ScanLine: TAggScanLineUnpacked8;
  Rasterizer: array [0..1] of TAggRasterizerScanLineAA;
begin
  // Initialize structures
  PixelFormatBgr24(PixelFormatProcessor, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RenSolid := TAggRendererScanLineAASolid.Create(RendererBase);

    RendererBase.Clear(CRgba8White);

    Rasterizer[0] := TAggRasterizerScanLineAA.Create;
    Rasterizer[1] := TAggRasterizerScanLineAA.Create;
    try
      ScanLine := TAggScanLineUnpacked8.Create;
      // Render the controls
      RenderControl(Rasterizer[0], ScanLine, RenSolid, FRadioBoxPolygons);
      RenderControl(Rasterizer[0], ScanLine, RenSolid, FRadioBoxFillRule);
      RenderControl(Rasterizer[0], ScanLine, RenSolid, FRadioBoxScanLineType);
      RenderControl(Rasterizer[0], ScanLine, RenSolid, FRadioBoxOperation);

      // Render
      RenderScanBool(Rasterizer[0], Rasterizer[1]);

      // Free AGG resources
      ScanLine.Free;
    finally
      Rasterizer[0].Free;
      Rasterizer[1].Free;
    end;

    RenSolid.Free;
  finally
    RendererBase.Free
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

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
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

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('This is another example of using of the ScanLine boolean '
      + 'algebra. The example is similar to Demo gpc_test. Note that the '
      + 'cost of the boolean operation with Anti-Aliasing is comparable with '
      + 'rendering (the rasterization time is not included). Also note that '
      + 'there is a difference in timings between using of ScanLine_u and '
      + 'ScanLinePacked. Most often ScanLineUnpacked works faster, but it''s '
      + 'because of much less number of produced Spans. Actually, when using '
      + 'the ScanLineUnpacked the complexity of the algorithm becomes '
      + 'proportional to the area of the polygons, while in ScanLinePacked '
      + 'it''s proportional to the perimeter. Of course, the binary variant '
      + 'works much faster than the Anti-Aliased one.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory. ');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. ScanLine Boolean (F1-Help)';

    if Init(655, 520, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
