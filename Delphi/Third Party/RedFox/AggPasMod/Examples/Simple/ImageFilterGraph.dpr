program ImageFilterGraph;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanImageFilter in '..\..\Source\AggSpanImageFilter.pas',
  AggSpanImageFilterRgb in '..\..\Source\AggSpanImageFilterRgb.pas',
  AggImageFilters in '..\..\Source\AggImageFilters.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas';

const
  CFlipY = True;

type
  TFilterAdaptor = class
  private
    FFilter: TAggCustomImageFilter;
    function GetRadius: Double;
    procedure SetRadius(R: Double);
  public
    constructor Create(Filter: TAggCustomImageFilter);
    destructor Destroy; override;

    function CalculateWeight(X: Double): Double;

    property Radius: Double read GetRadius write SetRadius;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FSliderRadius: TAggControlSlider;

    FCheckBoxBilinear, FCheckBoxBicubic: TAggControlCheckBox;
    FCheckBoxSpline16, FCheckBoxSpline36: TAggControlCheckBox;
    FCheckBoxHanning, FCheckBoxHamming: TAggControlCheckBox;
    FCheckBoxHermite, FCheckBoxKaiser: TAggControlCheckBox;
    FCheckBoxQuadric, FCheckBoxCatrom: TAggControlCheckBox;
    FCheckBoxGaussian, FCheckBoxBessel: TAggControlCheckBox;
    FCheckBoxMitchell, FCheckBoxSinc: TAggControlCheckBox;
    FCheckBoxLanczos, FCheckBoxBlackman: TAggControlCheckBox;

    FCheckBoxFilters: array [0..31] of TAggControlCheckBox;

    FFilterBilinear, FFilterBicubic: TFilterAdaptor;
    FFilterSpline16, FFilterSpline36: TFilterAdaptor;
    FFilterHanning, FFilterHamming: TFilterAdaptor;
    FFilterHermite, FFilterKaiser: TFilterAdaptor;
    FFilterQuadric, FFilterCatrom: TFilterAdaptor;
    FFilterGaussian, FFilterBessel: TFilterAdaptor;
    FFilterMitchell, FFilterSinc: TFilterAdaptor;
    FFilterLanczos, FFilterBlackman: TFilterAdaptor;

    FFilterFunc: array [0..31] of TFilterAdaptor;
    FNumFilters: Cardinal;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TFilterAdaptor }

constructor TFilterAdaptor.Create(Filter: TAggCustomImageFilter);
begin
  FFilter := Filter;
end;

destructor TFilterAdaptor.Destroy;
begin
  if FFilter <> nil then
    FFilter.Free;

  inherited;
end;

function TFilterAdaptor.GetRadius;
begin
  if FFilter <> nil then
    Result := FFilter.Radius
  else
    Result := 0;
end;

function TFilterAdaptor.CalculateWeight;
begin
  if FFilter <> nil then
    Result := FFilter.CalculateWeight(Abs(X))
  else
    Result := 0;
end;

procedure TFilterAdaptor.SetRadius;
begin
  if FFilter <> nil then
    FFilter.Radius := R;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  I: Cardinal;
begin
  inherited Create(PixelFormat, FlipY);

  FSliderRadius := TAggControlSlider.Create(5, 5, 780 - 5, 10, not FlipY);
  FCheckBoxBilinear := TAggControlCheckBox.Create(8, 30, 'bilinear', not FlipY);
  FCheckBoxBicubic := TAggControlCheckBox.Create(8, 45, 'bicubic', not FlipY);
  FCheckBoxSpline16 := TAggControlCheckBox.Create(8, 60, 'spline16', not FlipY);
  FCheckBoxSpline36 := TAggControlCheckBox.Create(8, 75, 'spline36', not FlipY);
  FCheckBoxHanning := TAggControlCheckBox.Create(8, 90, 'hanning', not FlipY);
  FCheckBoxHamming := TAggControlCheckBox.Create(8, 105, 'hamming', not FlipY);
  FCheckBoxHermite := TAggControlCheckBox.Create(8, 120, 'hermite', not FlipY);
  FCheckBoxKaiser := TAggControlCheckBox.Create(8, 135, 'kaiser', not FlipY);
  FCheckBoxQuadric := TAggControlCheckBox.Create(8, 150, 'quadric ', not FlipY);
  FCheckBoxCatrom := TAggControlCheckBox.Create(8, 165, 'catrom', not FlipY);
  FCheckBoxGaussian := TAggControlCheckBox.Create(8, 180, 'gaussian', not FlipY);
  FCheckBoxBessel := TAggControlCheckBox.Create(8, 195, 'bessel', not FlipY);
  FCheckBoxMitchell := TAggControlCheckBox.Create(8, 210, 'mitchell', not FlipY);
  FCheckBoxSinc := TAggControlCheckBox.Create(8, 225, 'sinc', not FlipY);
  FCheckBoxLanczos := TAggControlCheckBox.Create(8, 240, 'lanczos ', not FlipY);
  FCheckBoxBlackman := TAggControlCheckBox.Create(8, 255, 'blackman', not FlipY);

  FFilterBilinear := TFilterAdaptor.Create(TAggImageFilterBilinear.Create);
  FFilterBicubic := TFilterAdaptor.Create(TAggImageFilterBicubic.Create);
  FFilterSpline16 := TFilterAdaptor.Create(TAggImageFilterSpline16.Create);
  FFilterSpline36 := TFilterAdaptor.Create(TAggImageFilterSpline36.Create);
  FFilterHanning := TFilterAdaptor.Create(TAggImageFilterHanning.Create);
  FFilterHamming := TFilterAdaptor.Create(TAggImageFilterHamming.Create);
  FFilterHermite := TFilterAdaptor.Create(TAggImageFilterHermite.Create);
  FFilterKaiser := TFilterAdaptor.Create(TAggImageFilterKaiser.Create);
  FFilterQuadric := TFilterAdaptor.Create(TAggImageFilterQuadric.Create);
  FFilterCatrom := TFilterAdaptor.Create(TAggImageFilterCatrom.Create);
  FFilterGaussian := TFilterAdaptor.Create(TAggImageFilterGaussian.Create);
  FFilterBessel := TFilterAdaptor.Create(TAggImageFilterBessel.Create);
  FFilterMitchell := TFilterAdaptor.Create(TAggImageFilterMitchell.Create);
  FFilterSinc := TFilterAdaptor.Create(TAggImageFilterSinc.Create(2));
  FFilterLanczos := TFilterAdaptor.Create(TAggImageFilterLanczos.Create(2));
  FFilterBlackman := TFilterAdaptor.Create(TAggImageFilterBlackman.Create(2));

  FFilterFunc[ 0] := FFilterBilinear;
  FFilterFunc[ 1] := FFilterBicubic;
  FFilterFunc[ 2] := FFilterSpline16;
  FFilterFunc[ 3] := FFilterSpline36;
  FFilterFunc[ 4] := FFilterHanning;
  FFilterFunc[ 5] := FFilterHamming;
  FFilterFunc[ 6] := FFilterHermite;
  FFilterFunc[ 7] := FFilterKaiser;
  FFilterFunc[ 8] := FFilterQuadric;
  FFilterFunc[ 9] := FFilterCatrom;
  FFilterFunc[10] := FFilterGaussian;
  FFilterFunc[11] := FFilterBessel;
  FFilterFunc[12] := FFilterMitchell;
  FFilterFunc[13] := FFilterSinc;
  FFilterFunc[14] := FFilterLanczos;
  FFilterFunc[15] := FFilterBlackman;

  FNumFilters := 0;

  FCheckBoxFilters[FNumFilters] := FCheckBoxBilinear;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxBicubic;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxSpline16;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxSpline36;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxHanning;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxHamming;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxHermite;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxKaiser;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxQuadric;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxCatrom;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxGaussian;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxBessel;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxMitchell;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxSinc;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxLanczos;
  Inc(FNumFilters);
  FCheckBoxFilters[FNumFilters] := FCheckBoxBlackman;
  Inc(FNumFilters);

  for I := 0 to FNumFilters - 1 do
    AddControl(FCheckBoxFilters[I]);

  FSliderRadius.SetRange(2, 8);
  FSliderRadius.Value := 4;
  FSliderRadius.Caption := 'Radius=%.3f';

  AddControl(FSliderRadius);
end;

destructor TAggApplication.Destroy;
begin
  FSliderRadius.Free;
  FCheckBoxBilinear.Free;
  FCheckBoxBicubic.Free;
  FCheckBoxSpline16.Free;
  FCheckBoxSpline36.Free;
  FCheckBoxHanning.Free;
  FCheckBoxHamming.Free;
  FCheckBoxHermite.Free;
  FCheckBoxKaiser.Free;
  FCheckBoxQuadric.Free;
  FCheckBoxCatrom.Free;
  FCheckBoxGaussian.Free;
  FCheckBoxBessel.Free;
  FCheckBoxMitchell.Free;
  FCheckBoxSinc.Free;
  FCheckBoxLanczos.Free;
  FCheckBoxBlackman.Free;

  FFilterBilinear.Free;
  FFilterBicubic.Free;
  FFilterSpline16.Free;
  FFilterSpline36.Free;
  FFilterHanning.Free;
  FFilterHamming.Free;
  FFilterHermite.Free;
  FFilterKaiser.Free;
  FFilterQuadric.Free;
  FFilterCatrom.Free;
  FFilterGaussian.Free;
  FFilterBessel.Free;
  FFilterMitchell.Free;
  FFilterSinc.Free;
  FFilterLanczos.Free;
  FFilterBlackman.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Sl: TAggScanLinePacked8;

  Ras: TAggRasterizerScanLineAA;

  Normalized: TAggImageFilterLUT;
  Weights: PInt16;

  Coord : TRectDouble;
  CenterX, X, Y, Ys, Radius, Xs, Sum, Xf: Double;
  Delta: TPointDouble;

  Xfract, Ir: Integer;

  I, J, N, Xint, Nn: Cardinal;

  P : TAggPathStorage;
  Pl: TAggConvStroke;
  Tr: TAggConvTransform;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      Ras := TAggRasterizerScanLineAA.Create;
      Sl := TAggScanLinePacked8.Create;

      // Render
      Coord.X1 := 125;
      Coord.X2 := FInitialWidth - 15;
      Coord.Y1 := 10;
      Coord.Y2 := FInitialHeight - 10;
      CenterX := Coord.CenterX;

      P := TAggPathStorage.Create;
      Pl := TAggConvStroke.Create(P);
      Tr := TAggConvTransform.Create(Pl, GetTransAffineResizing);

      for I := 0 to 15 do
      begin
        X := Coord.X1 + (Coord.X2 - Coord.X1) * I / 16;

        P.RemoveAll;
        P.MoveTo(X + 0.5, Coord.Y1);
        P.LineTo(X + 0.5, Coord.Y2);

        Ras.AddPath(Tr);

        if I = 8 then
          Rgba.FromRgbaInteger(0, 0, 0, 255)
        else
          Rgba.FromRgbaInteger(0, 0, 0, 100);

        RenScan.SetColor(@Rgba);
        RenderScanLines(Ras, Sl, RenScan);
      end;

      Ys := Coord.Y1 + (Coord.Y2 - Coord.Y1) / 6;

      P.RemoveAll;
      P.MoveTo(Coord.X1, Ys);
      P.LineTo(Coord.X2, Ys);
      Ras.AddPath(Tr);
      RenScan.SetColor(CRgba8Black);
      RenderScanLines(Ras, Sl, RenScan);

      Pl.Width := 1;

      for I := 0 to FNumFilters - 1 do
        if FCheckBoxFilters[I].Status then
        begin
          FFilterFunc[I].SetRadius(FSliderRadius.Value);

          Radius := FFilterFunc[I].Radius;

          N := Trunc(Radius * 512);
          Delta.Y := Coord.Y2 - Ys;

          Xs := Coord.CenterX - (Radius * (Coord.X2 - Coord.X1) / 16);
          Delta.X := (Coord.X2 - Coord.X1) * Radius * 0.125;

          P.RemoveAll;
          P.MoveTo(Xs + 0.5, Ys + Delta.Y * FFilterFunc[I].CalculateWeight(-Radius));

          J := 1;

          while J < N do
          begin
            P.LineTo(Xs + Delta.X * J / N + 0.5, Ys + Delta.Y * FFilterFunc[I]
              .CalculateWeight(J / 256 - Radius));

            Inc(J);
          end;

          Ras.AddPath(Tr);
          Rgba.FromRgbaInteger(100, 0, 0);
          RenScan.SetColor(@Rgba);
          RenderScanLines(Ras, Sl, RenScan);

          P.RemoveAll;

          Ir := Trunc(Ceil(Radius) + 0.1);

          for Xint := 0 to 255 do
          begin
            Sum := 0;

            Xfract := -Ir;

            while Xfract < Ir do
            begin
              Xf := Xint / 256 + Xfract;

              if (Xf >= -Radius) or (Xf <= Radius) then
                Sum := Sum + FFilterFunc[I].CalculateWeight(Xf);

              Inc(Xfract);
            end;

            X := CenterX + ((Xint - 128) / 128) * Radius *
              (Coord.X2 - Coord.X1) / 16;
            Y := Ys + Sum * 256 - 256;

            if Xint = 0 then
              P.MoveTo(X, Y)
            else
              P.LineTo(X, Y);
          end;

          Ras.AddPath(Tr);
          Rgba.FromRgbaInteger(0, 100, 0);
          RenScan.SetColor(@Rgba);
          RenderScanLines(Ras, Sl, RenScan);

          Normalized := TAggImageFilterLUT.Create(FFilterFunc[I].FFilter);

          Weights := Normalized.WeightArray;

          Xs := (Coord.X2 + Coord.X1) * 0.5 -
            (Normalized.Diameter * (Coord.X2 - Coord.X1) / 32);
          Nn := Normalized.Diameter * 256;

          P.RemoveAll;
          P.MoveTo(Xs + 0.5, Ys + Delta.Y * PInt16(Weights)^ / CAggImageFilterSize);

          J := 1;

          while J < Nn do
          begin
            P.LineTo(Xs + Delta.X * J / N + 0.5,
              Ys + Delta.Y * PInt16(PtrComp(Weights) + J * SizeOf(Int16))^ /
              CAggImageFilterSize);

            Inc(J);
          end;

          Ras.AddPath(Tr);
          Rgba.FromRgbaInteger(0, 0, 100, 255);
          RenScan.SetColor(@Rgba);
          RenderScanLines(Ras, Sl, RenScan);

          // Free
          Normalized.Free;
        end;

      // Render the controls
      for I := 0 to FNumFilters - 1 do
        RenderControl(Ras, Sl, RenScan, FCheckBoxFilters[I]);

      if FCheckBoxSinc.Status or FCheckBoxLanczos.Status or FCheckBoxBlackman.Status then
        RenderControl(Ras, Sl, RenScan, FSliderRadius);

      // Free AGG resources
      Ras.Free;
      Sl.Free;

      Tr.Free;

      P.Free;
      Pl.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Demonstration of the shapes of different interpolation '
      + 'filters. Just in case if you are curious.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'Image filters'' shape comparison (F1-Help)';

    if Init(780, 300, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
