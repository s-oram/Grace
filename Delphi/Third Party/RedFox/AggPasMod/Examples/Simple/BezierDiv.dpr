program BezierDiv;

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

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggBezierControl in '..\..\Source\Controls\AggBezierControl.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',
  AggRendererOutlineAA in '..\..\Source\AggRendererOutlineAA.pas',
  AggRendererOutlineImage in '..\..\Source\AggRendererOutlineImage.pas',

  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvDash in '..\..\Source\AggConvDash.pas',
  AggPatternFiltersRgba in '..\..\Source\AggPatternFiltersRgba.pas',
  AggArc in '..\..\Source\AggArc.pas',
  AggArray in '..\..\Source\AggArray.pas',
  AggCurves in '..\..\Source\AggCurves.pas',
  AggBezierArc in '..\..\Source\AggBezierArc.pas',
  AggVertexSequence in '..\..\Source\AggVertexSequence.pas',
  AggMath in '..\..\Source\AggMath.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas';

{$I- }

const
  CFlipY = True;

type
  PAggCurvePoint = ^TAggCurvePoint;
  TAggCurvePoint = record
    X, Y, Dist, Mu: Double;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FCtrlColor: TAggColor;

    FCurve1: TBezierControl;

    FSliderAngleTolerance: TAggControlSlider;
    FSliderApproximationScale: TAggControlSlider;
    FSliderCuspLimit: TAggControlSlider;
    FSliderWidth: TAggControlSlider;

    FCheckBoxShowPoints: TAggControlCheckBox;
    FCheckBoxShowOutline: TAggControlCheckBox;
    FRadioBoxCurveType: TAggControlRadioBox;
    FRadioBoxCaseType: TAggControlRadioBox;
    FRadioBoxInnerJoin: TAggControlRadioBox;
    FRadioBoxLineJoin: TAggControlRadioBox;
    FRadioBoxLineCap: TAggControlRadioBox;

    FCurCaseType: Integer;
  protected
    function MeasureTime(Curve: TAggCurve4): Double;
    function FindPoint(Path: TAggPodDeque; Dist: Double;
      I, J: PCardinal): Boolean;

    function CalcMaxError(Curve: TAggCurve4; Scale: Double;
      MaxAngleError: PDouble): Double;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
    procedure OnControlChange; override;
  end;

procedure Bezier4Point(X1, Y1, X2, Y2, X3, Y3, X4, Y4, Mu: Double;
  X, Y: PDouble);
var
  Mum1, Mu3: Double;
begin
  Mum1 := 1 - Mu;
  Mu3 := Sqr(Mu) * Mu;

  X^ := Mum1 * (X1 * Sqr(Mum1) + 3 * Mu * (Mum1 * X2 + Mu * X3)) + Mu3 * X4;
  Y^ := Mum1 * (Y1 * Sqr(Mum1) + 3 * Mu * (Mum1 * Y2 + Mu * Y3)) + Mu3 * Y4;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FCtrlColor.FromRgbaDouble(0, 0.3, 0.5, 0.8);

  FSliderAngleTolerance := TAggControlSlider.Create(5, 5, 240, 12, not FlipY);
  FSliderApproximationScale := TAggControlSlider.Create(5, 22, 240, 29,
    not FlipY);
  FSliderCuspLimit := TAggControlSlider.Create(5, 39, 240, 46, not FlipY);
  FSliderWidth := TAggControlSlider.Create(245, 5, 495, 12, not FlipY);
  FCheckBoxShowPoints := TAggControlCheckBox.Create(250, 20, 'Show Points',
    not FlipY);
  FCheckBoxShowOutline := TAggControlCheckBox.Create(250, 35, 'Show Stroke '
    + 'Outline', not FlipY);
  FRadioBoxCurveType := TAggControlRadioBox.Create(535, 5, 650, 55, not FlipY);
  FRadioBoxCaseType := TAggControlRadioBox.Create(535, 60, 650, 195, not FlipY);
  FRadioBoxInnerJoin := TAggControlRadioBox.Create(535, 200, 650, 290,
    not FlipY);
  FRadioBoxLineJoin := TAggControlRadioBox.Create(535, 295, 650, 385,
    not FlipY);
  FRadioBoxLineCap := TAggControlRadioBox.Create(535, 395, 650, 455, not FlipY);

  FCurCaseType := -1;

  FCurve1 := TBezierControl.Create;
  FCurve1.SetLineColor(@FCtrlColor);
  FCurve1.SetCurve(170, 424, 13, 87, 488, 423, 26, 333);
  // FCurve1.SetCurve(26, 333, 276, 126, 402, 479, 26, 333); // Loop with p1==p4
  // FCurve1.SetCurve(378, 439, 378, 497, 487, 432, 14, 338); // Narrow loop
  // FCurve1.SetCurve(288, 283, 232, 89, 66, 197, 456, 241); // Loop
  // FCurve1.SetCurve(519, 142, 97, 147, 69, 147, 30, 144); // Almost straight
  // FCurve1.SetCurve(100, 100, 200, 100, 100, 200, 200, 200); // A "Z" case
  // FCurve1.SetCurve(150, 150, 350, 150, 150, 150, 350, 150); // Degenerate
  // FCurve1.SetCurve(409, 330, 300, 200, 200, 200, 401, 263); // Strange cusp
  // FCurve1.SetCurve(129, 233, 172, 320, 414, 253, 344, 236); // Curve cap
  // FCurve1.SetCurve(100, 100, 100, 200, 100, 100, 110, 100); // A "boot"
  // FCurve1.SetCurve(225, 150, 60, 150, 460, 150, 295, 150); // 2----1----4----3
  // FCurve1.SetCurve(162.2, 248.801, 162.2, 248.801, 266, 284, 394, 335);  // Coinciding 1-2
  // FCurve1.SetCurve(162.200, 248.801, 162.200, 248.801, 257, 301, 394, 335); // Coinciding 1-2
  // FCurve1.SetCurve(394, 335, 257, 301, 162.2, 248.801, 162.2, 248.801); // Coinciding 3-4
  // FCurve1.SetCurve(84.2, 302.801, 84.2, 302.801, 79, 292.401, 97.001, 304.401); // From tiger.svg
  // FCurve1.SetCurve(97.001, 304.401, 79, 292.401, 84.2, 302.801, 84.2, 302.801); // From tiger.svg opposite dir
  // FCurve1.SetCurve(475, 157, 200, 100, 453, 100, 222, 157); // Cusp, failure for Adobe SVG

  AddControl(FCurve1);
  FCurve1.NoTransform;

  FSliderAngleTolerance.Caption := 'Angle Tolerance=%.0f deg';
  FSliderAngleTolerance.SetRange(0, 90);
  FSliderAngleTolerance.Value := 15;
  AddControl(FSliderAngleTolerance);
  FSliderAngleTolerance.NoTransform;

  FSliderApproximationScale.Caption := 'Approximation Scale=%.3f';
  FSliderApproximationScale.SetRange(0.1, 5);
  FSliderApproximationScale.Value := 1.0;
  AddControl(FSliderApproximationScale);
  FSliderApproximationScale.NoTransform;

  FSliderCuspLimit.Caption := 'Cusp Limit=%.0f deg';
  FSliderCuspLimit.SetRange(0, 90);
  FSliderCuspLimit.Value := 0;
  AddControl(FSliderCuspLimit);
  FSliderCuspLimit.NoTransform;

  FSliderWidth.Caption := 'Width=%.2f';
  FSliderWidth.SetRange(0.0, 100);
  FSliderWidth.Value := 50.0;
  AddControl(FSliderWidth);
  FSliderWidth.NoTransform;


  AddControl(FCheckBoxShowPoints);
  FCheckBoxShowPoints.NoTransform;
  FCheckBoxShowPoints.Status := True;

  AddControl(FCheckBoxShowOutline);
  FCheckBoxShowOutline.NoTransform;
  FCheckBoxShowOutline.Status := True;

  FRadioBoxCurveType.AddItem('Incremental');
  FRadioBoxCurveType.AddItem('Subdiv');
  FRadioBoxCurveType.SetCurrentItem(1);
  AddControl(FRadioBoxCurveType);
  FRadioBoxCurveType.NoTransform;

  FRadioBoxCaseType.SetTextSize(7);
  FRadioBoxCaseType.TextThickness := 1;

  FRadioBoxCaseType.AddItem('Random');
  FRadioBoxCaseType.AddItem('13---24');
  FRadioBoxCaseType.AddItem('Smooth Cusp 1');
  FRadioBoxCaseType.AddItem('Smooth Cusp 2');
  FRadioBoxCaseType.AddItem('Real Cusp 1');
  FRadioBoxCaseType.AddItem('Real Cusp 2');
  FRadioBoxCaseType.AddItem('Fancy Stroke');
  FRadioBoxCaseType.AddItem('Jaw');
  FRadioBoxCaseType.AddItem('Ugly Jaw');

  AddControl(FRadioBoxCaseType);

  FRadioBoxCaseType.NoTransform;

  FRadioBoxInnerJoin.SetTextSize(8);

  FRadioBoxInnerJoin.AddItem('Inner Bevel');
  FRadioBoxInnerJoin.AddItem('Inner Miter');
  FRadioBoxInnerJoin.AddItem('Inner Jag');
  FRadioBoxInnerJoin.AddItem('Inner Round');
  FRadioBoxInnerJoin.SetCurrentItem(3);

  AddControl(FRadioBoxInnerJoin);

  FRadioBoxInnerJoin.NoTransform;

  FRadioBoxLineJoin.SetTextSize(8);

  FRadioBoxLineJoin.AddItem('Miter Join');
  FRadioBoxLineJoin.AddItem('Miter Revert');
  FRadioBoxLineJoin.AddItem('Miter Round');
  FRadioBoxLineJoin.AddItem('Round Join');
  FRadioBoxLineJoin.AddItem('Bevel Join');
  FRadioBoxLineJoin.SetCurrentItem(1);

  AddControl(FRadioBoxLineJoin);

  FRadioBoxLineJoin.NoTransform;

  FRadioBoxLineCap.SetTextSize(8);

  FRadioBoxLineCap.AddItem('Butt Cap');
  FRadioBoxLineCap.AddItem('Square Cap');
  FRadioBoxLineCap.AddItem('Round Cap');
  FRadioBoxLineCap.SetCurrentItem(0);

  AddControl(FRadioBoxLineCap);

  FRadioBoxLineCap.NoTransform;
end;

destructor TAggApplication.Destroy;
begin
  FSliderAngleTolerance.Free;
  FSliderApproximationScale.Free;
  FSliderCuspLimit.Free;
  FSliderWidth.Free;
  FCheckBoxShowPoints.Free;
  FCheckBoxShowOutline.Free;
  FRadioBoxCurveType.Free;
  FRadioBoxCaseType.Free;
  FRadioBoxInnerJoin.Free;
  FRadioBoxLineJoin.Free;
  FRadioBoxLineCap.Free;
  FCurve1.Free;

  inherited;
end;

function TAggApplication.MeasureTime(Curve: TAggCurve4): Double;
var
  I: Integer;
  X, Y: Double;
begin
  StartTimer;

  for I := 0 to 99 do
  begin
    Curve.Init4(FCurve1.Point1, FCurve1.Point2, FCurve1.Point3, FCurve1.Point4);

    Curve.Rewind(0);

    while not IsStop(Curve.Vertex(@X, @Y)) do;
  end;

  Result := GetElapsedTime * 10;
end;

function TAggApplication.FindPoint(Path: TAggPodDeque; Dist: Double;
  I, J: PCardinal): Boolean;
var
  K: Integer;
begin
  J^ := Path.Size - 1;
  I^ := 0;

  while J^ - I^ > 1 do
  begin
    K := ShrInt32(I^ + J^, 1);

    if Dist < PAggVertexDistance(Path[K]).Dist then
      J^ := K
    else
      I^ := K;
  end;

  Result := True;
end;

function TAggApplication.CalcMaxError(Curve: TAggCurve4; Scale: Double;
  MaxAngleError: PDouble): Double;
var
  Cmd, I, Idx1, Idx2: Cardinal;

  X, Y, CurveDist, ReferenceDist: Double;
  MaxError, Err, Aerr, A1, A2, Da: Double;

  CurvePoints, ReferencePoints: TAggPodDeque;

  Vd: TAggVertexDistance;
  Cp: TAggCurvePoint;
const
  CMuScale: Double = 1 / 4095;
begin
  CurvePoints := TAggPodDeque.Create(SizeOf(TAggVertexDistance), 8);
  ReferencePoints := TAggPodDeque.Create(SizeOf(TAggCurvePoint), 8);

  Curve.ApproximationScale := FSliderApproximationScale.Value * Scale;

  Curve.Init4(FCurve1.Point1, FCurve1.Point2, FCurve1.Point3, FCurve1.Point4);

  Curve.Rewind(0);

  Vd.Dist := 0;

  Cmd := Curve.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    if IsVertex(Cmd) then
    begin
      Vd.Pos := PointDouble(X, Y);
      CurvePoints.Add(@Vd);
    end;

    Cmd := Curve.Vertex(@X, @Y);
  end;

  CurveDist := 0;

  I := 1;

  while I < CurvePoints.Size do
  begin
    PAggVertexDistance(CurvePoints[I - 1]).Dist := CurveDist;

    CurveDist := CurveDist + CalculateDistance
      (PAggVertexDistance(CurvePoints[I - 1]).Pos.X,
      PAggVertexDistance(CurvePoints[I - 1]).Pos.Y,
      PAggVertexDistance(CurvePoints[I]).Pos.X,
      PAggVertexDistance(CurvePoints[I]).Pos.Y);

    Inc(I);
  end;

  PAggVertexDistance(CurvePoints[CurvePoints.Size - 1]).Dist := CurveDist;

  Cp.Dist := 0;
  Cp.X := X;
  Cp.Y := Y;
  for I := 0 to 4095 do
  begin
    Cp.Mu := I * CMuScale;

    Bezier4Point(FCurve1.X1, FCurve1.Y1, FCurve1.X2, FCurve1.Y2,
      FCurve1.X3, FCurve1.Y3, FCurve1.X4, FCurve1.Y4, Cp.Mu, @Cp.X, @Cp.Y);

    ReferencePoints.Add(@Cp);
  end;

  ReferenceDist := 0;

  I := 1;

  while I < ReferencePoints.Size do
  begin
    PAggCurvePoint(ReferencePoints[I - 1]).Dist := ReferenceDist;

    ReferenceDist := ReferenceDist +
      CalculateDistance(PAggCurvePoint(ReferencePoints[I - 1]).X,
      PAggCurvePoint(ReferencePoints[I - 1]).Y,
      PAggCurvePoint(ReferencePoints[I]).X,
      PAggCurvePoint(ReferencePoints[I]).Y);

    Inc(I);
  end;

  PAggCurvePoint(ReferencePoints[ReferencePoints.Size - 1]).Dist :=
    ReferenceDist;

  Idx1 := 0;
  Idx2 := 1;

  MaxError := 0;

  I := 0;

  while I < ReferencePoints.Size do
  begin
    if FindPoint(CurvePoints,
      PAggCurvePoint(ReferencePoints[I]).Dist, @Idx1, @Idx2)
    then
    begin
      Err := Abs(CalculateLinePointDistance
        (PAggVertexDistance(CurvePoints[Idx1]).Pos.X,
        PAggVertexDistance(CurvePoints[Idx1]).Pos.Y,
        PAggVertexDistance(CurvePoints[Idx2]).Pos.X,
        PAggVertexDistance(CurvePoints[Idx2]).Pos.Y,
        PAggCurvePoint(ReferencePoints[I]).X,
        PAggCurvePoint(ReferencePoints[I]).Y));

      if Err > MaxError then
        MaxError := Err;
    end;

    Inc(I);
  end;

  Aerr := 0;

  I := 2;

  while I < CurvePoints.Size do
  begin
    A1 := ArcTan2(PAggVertexDistance(CurvePoints[I - 1]).Pos.Y -
      PAggVertexDistance(CurvePoints[I - 2]).Pos.Y,
      PAggVertexDistance(CurvePoints[I - 1]).Pos.X -
      PAggVertexDistance(CurvePoints[I - 2]).Pos.X);

    A2 := ArcTan2(PAggVertexDistance(CurvePoints[I]).Pos.Y -
      PAggVertexDistance(CurvePoints[I - 1]).Pos.Y,
      PAggVertexDistance(CurvePoints[I]).Pos.X -
      PAggVertexDistance(CurvePoints[I - 1]).Pos.X);

    Da := Abs(A1 - A2);

    if Da >= Pi then
      Da := 2 * Pi - Da;

    if Da > Aerr then
      Aerr := Da;

    Inc(I);
  end;

  MaxAngleError^ := Rad2Deg(Aerr);

  Result := MaxError * Scale;

  CurvePoints.Free;
  ReferencePoints.Free;
end;

procedure TAggApplication.OnDraw;
var
  RendererBase: TAggRendererBase;
  Rgba: TAggColor;

  Pixf : TAggPixelFormatProcessor;
  RenScan: TAggRendererScanLineAASolid;
  Rasterizer: TAggRasterizerScanLineAA;
  ScanLine : TAggScanLineUnpacked8;

  Path: TAggPathStorage;
  Curve: TAggCurve4;
  Stroke, Stroke2: TAggConvStroke;

  Circle: TAggCircle;

  Txt : TAggGsvText;
  Pt: TAggConvStroke;

  Cmd, NumPoints1: Cardinal;

  X, Y, CurveTime: Double;
  MaxAngleError, MaxError: array [0..4] of Double;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Rgba.FromRgbaDouble(1.0, 1.0, 0.95);
    RendererBase.Clear(@Rgba);

    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      Rasterizer := TAggRasterizerScanLineAA.Create;
      try
        ScanLine := TAggScanLineUnpacked8.Create;
        try
          // Render Curve
          Path := TAggPathStorage.Create;

          CurveTime := 0;

          Path.RemoveAll;
          Curve := TAggCurve4.Create;

          Curve.ApproximationMethod := TAggCurveApproximationMethod
            (FRadioBoxCurveType.GetCurrentItem);
          Curve.ApproximationScale := FSliderApproximationScale.Value;

          Curve.AngleTolerance := Deg2Rad(FSliderAngleTolerance.Value);
          Curve.CuspLimit := Deg2Rad(FSliderCuspLimit.Value);

          CurveTime := MeasureTime(Curve);

          MaxAngleError[0] := 0;
          MaxAngleError[1] := 0;
          MaxAngleError[2] := 0;
          MaxAngleError[3] := 0;
          MaxAngleError[4] := 0;
          MaxError[0] := 0;
          MaxError[1] := 0;
          MaxError[2] := 0;
          MaxError[3] := 0;
          MaxError[4] := 0;

          MaxError[0] := CalcMaxError(Curve, 0.01, @MaxAngleError[0]);
          MaxError[1] := CalcMaxError(Curve, 0.1, @MaxAngleError[1]);
          MaxError[2] := CalcMaxError(Curve, 1, @MaxAngleError[2]);
          MaxError[3] := CalcMaxError(Curve, 10, @MaxAngleError[3]);
          MaxError[4] := CalcMaxError(Curve, 100, @MaxAngleError[4]);

          Curve.ApproximationScale := FSliderApproximationScale.Value;
          Curve.AngleTolerance := Deg2Rad(FSliderAngleTolerance.Value);
          Curve.CuspLimit := Deg2Rad(FSliderCuspLimit.Value);

          Curve.Init4(FCurve1.Point1, FCurve1.Point2, FCurve1.Point3,
            FCurve1.Point4);

          Path.AddPath(Curve, 0, False);

          Stroke := TAggConvStroke.Create(Path);
          Stroke.Width := FSliderWidth.Value;

          Stroke.LineJoin := TAggLineJoin(FRadioBoxLineJoin.GetCurrentItem);
          Stroke.LineCap := TAggLineCap(FRadioBoxLineCap.GetCurrentItem);
          Stroke.InnerJoin := TAggInnerJoin(FRadioBoxInnerJoin.GetCurrentItem);
          Stroke.InnerMiterLimit := 1.01;

          Rasterizer.AddPath(Stroke);
          Rgba.FromRgbaDouble(0, 0.5, 0, 0.5);
          RenScan.SetColor(@Rgba);
          RenderScanLines(Rasterizer, ScanLine, RenScan);

          // Render internal points
          NumPoints1 := 0;

          Path.Rewind(0);

          Cmd := Path.Vertex(@X, @Y);

          while not IsStop(Cmd) do
          begin
            if FCheckBoxShowPoints.Status then
            begin
              Circle := TAggCircle.Create(X, Y, 1.5, 8);
              try
                Rasterizer.AddPath(Circle);
              finally
                Circle.Free;
              end;
              Rgba.FromRgbaDouble(0, 0, 0, 0.5);
              RenScan.SetColor(@Rgba);
              RenderScanLines(Rasterizer, ScanLine, RenScan);
            end;

            Inc(NumPoints1);

            Cmd := Path.Vertex(@X, @Y);
          end;

          // Render outline
          if FCheckBoxShowOutline.Status then
          begin
            // Draw a stroke of the stroke to see the internals
            Stroke2 := TAggConvStroke.Create(Stroke);
            Rasterizer.AddPath(Stroke2);
            Rgba.FromRgbaDouble(0, 0, 0, 0.5);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);
          end;

          // Check TAggCircle and arc for the number of points
          { a := TAggCircle.Create(100, 100, FSliderWidth.Value,
              FSliderWidth.Value, 0);
            Rasterizer.AddPath(a);
            rgba.FromRgbaDouble(0.5, 0, 0, 0.5);
            RenScan.SetColor(@rgba);
            RenderScanLines(Rasterizer, ScanLine, RenScan);

            a.Rewind(0);

            cmd:=a.vertex(@x, @y);

            while not IsStop(cmd) do
            begin
              if IsVertex(cmd) then
              begin
                Circle := TAggCircle.Create(x, y, 1.5, 8);
                try
                  Rasterizer.AddPath(Circle);
                finally
                  Circle.Free;
                end;
                Rgba.FromRgbaDouble(0, 0, 0, 0.5);
                RenScan.SetColor(@rgba);
                RenderScanLines(Rasterizer, ScanLine, RenScan);
              end;

              cmd := a.vertex(@x, @y);
            end;{ }

          // Render text
          Txt := TAggGsvText.Create;
          try
            Txt.SetSize(8.0);

            Pt := TAggConvStroke.Create(Txt);
            try
              Pt.LineCap := lcRound;
              Pt.LineJoin := ljRound;
              Pt.Width := 1.5;

              Txt.SetStartPoint(10.0, 85.0);
              Txt.SetText(Format('Num Points=%d Time=%.2fmks'#13#13 +
                'Dist Error: x0.01=%.5f x0.1=%.5f x1=%.5f x10=%.5f x100=%.5f'
                + #13#13 + 'Angle Error: x0.01=%.1f x0.1=%.1f x1=%.1f x10=%.1f '
                + 'x100=%.1f', [NumPoints1, CurveTime, MaxError[0], MaxError[1],
                MaxError[2], MaxError[3], MaxError[4], MaxAngleError[0],
                MaxAngleError[1], MaxAngleError[2], MaxAngleError[3],
                MaxAngleError[4]]));

              Rasterizer.AddPath(Pt);
            finally
              Pt.Free;
            end;
          finally
            Txt.Free;
          end;

          RenScan.SetColor(CRgba8Black);
          RenderScanLines(Rasterizer, ScanLine, RenScan);

          Path.Free;
          Curve.Free;
          Stroke.Free;
          if FCheckBoxShowOutline.Status then
            Stroke2.Free;

          // Render the controls
          RenderControl(Rasterizer, ScanLine, RenScan, FCurve1);
          RenderControl(Rasterizer, ScanLine, RenScan, FSliderAngleTolerance);
          RenderControl(Rasterizer, ScanLine, RenScan,
            FSliderApproximationScale);
          RenderControl(Rasterizer, ScanLine, RenScan, FSliderCuspLimit);
          RenderControl(Rasterizer, ScanLine, RenScan, FSliderWidth);
          RenderControl(Rasterizer, ScanLine, RenScan, FCheckBoxShowPoints);
          RenderControl(Rasterizer, ScanLine, RenScan, FCheckBoxShowOutline);
          RenderControl(Rasterizer, ScanLine, RenScan, FRadioBoxCurveType);
          RenderControl(Rasterizer, ScanLine, RenScan, FRadioBoxCaseType);
          RenderControl(Rasterizer, ScanLine, RenScan, FRadioBoxInnerJoin);
          RenderControl(Rasterizer, ScanLine, RenScan, FRadioBoxLineJoin);
          RenderControl(Rasterizer, ScanLine, RenScan, FRadioBoxLineCap);
        finally
          ScanLine.Free;
        end;
      finally
        Rasterizer.Free;
      end;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
var
  Fd : Text;
  Str: AnsiString;
begin
  if Key = Byte(' ') then
  begin
    AssignFile(Fd, 'coord');
    Rewrite(Fd);

    Str := Format('%.3f, %.3f, %.3f, %.3f, %.3f, %.3f, %.3f, %.3f', [FCurve1.X1,
      FCurve1.Y1, FCurve1.X2, FCurve1.Y2, FCurve1.X3, FCurve1.Y3, FCurve1.X4,
      FCurve1.Y4]);

    Write(Fd, PAnsiChar(@Str[1]));
    Close(Fd);
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('Demonstration of new methods of Bezier curve approximation.'
      + ' You can compare the old, incremental method with adaptive De '
      + 'Casteljau''s subdivion. The new method uses two criteria to stop '
      + 'subdivision: estimation of distance and estimation of angle. It gives '
      + 'us perfectly smooth result even for very sharp turns and loops.'
      + 'How to play with:'#13#13 + 'Use the mouse to change the shape of the '
      + 'curve. Press the spacebar to dump the curve''s coordinates into the '
      + '"coord" file.' + #13#13 + 'Note: F2 key saves current "screenshot" '
      + 'file in this demo''s directory.');
end;

procedure TAggApplication.OnControlChange;
var
  W, H: Integer;
begin
  if FRadioBoxCaseType.GetCurrentItem <> FCurCaseType then
  begin
    case FRadioBoxCaseType.GetCurrentItem of
      0: // FRadioBoxCaseType.AddItem("Random");
        begin
          W := Trunc(Width - 120);
          H := Trunc(Height - 80);

          FCurve1.SetCurve(Random(W), 80 + Random(H), Random(W), 80 + Random(H),
            Random(W), 80 + Random(H), Random(W), 80 + Random(H));
        end;

      1: // FRadioBoxCaseType.AddItem("13---24");
        FCurve1.SetCurve(150, 150, 350, 150, 150, 150, 350, 150);

      2: // FRadioBoxCaseType.AddItem("Smooth Cusp 1");
        FCurve1.SetCurve(50, 142, 483, 251, 496, 62, 26, 333);

      3: // FRadioBoxCaseType.AddItem("Smooth Cusp 2");
        FCurve1.SetCurve(50, 142, 484, 251, 496, 62, 26, 333);

      4: // FRadioBoxCaseType.AddItem("Real Cusp 1");
        FCurve1.SetCurve(100, 100, 300, 200, 200, 200, 200, 100);

      5: // FRadioBoxCaseType.AddItem("Real Cusp 2");
        FCurve1.SetCurve(475, 157, 200, 100, 453, 100, 222, 157);

      6: // FRadioBoxCaseType.AddItem("Fancy Stroke");
        begin
          FCurve1.SetCurve(129, 233, 32, 283, 258, 285, 159, 232);
          FSliderWidth.Value := 100;
        end;

      7: // FRadioBoxCaseType.AddItem("Jaw");
        FCurve1.SetCurve(100, 100, 300, 200, 264, 286, 264, 284);

      8: // FRadioBoxCaseType.AddItem("Ugly Jaw");
        FCurve1.SetCurve(100, 100, 413, 304, 264, 286, 264, 284);
    end;

    ForceRedraw;

    FCurCaseType := FRadioBoxCaseType.GetCurrentItem;
  end;
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example (F1-Help)';

    if Init(655, 520, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
