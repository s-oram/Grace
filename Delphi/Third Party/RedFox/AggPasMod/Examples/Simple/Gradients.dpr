program Gradients;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}
{$I-}

{ DEFINE AGG_GRAY8 }
{$DEFINE AGG_BGR24 }
{ DEFINE AGG_Rgb24 }
{ DEFINE AGG_BGRA32 }
{ DEFINE AGG_RgbA32 }
{ DEFINE AGG_ARGB32 }
{ DEFINE AGG_ABGR32 }
{ DEFINE AGG_Rgb565 }
{ DEFINE AGG_Rgb555 }

{$DEFINE UseGamma}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Math,
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSplineControl in '..\..\Source\Controls\AggSplineControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggGammaControl in '..\..\Source\Controls\AggGammaControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggArray in '..\..\Source\AggArray.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas'
{$I Pixel_Formats.inc}

const
  CFlipY = True;
  CCenterX: Double = 350;
  CCenterY: Double = 280;

type
  TColorFunctionProfile = class(TAggCustomArray)
  private
    FColors: PAggColor;
    {$IFDEF UseGamma}
    FGammaProfile: PInt8u;
    {$ENDIF}
    function GetSize: Cardinal; override;
  protected
    function ArrayOperator(I: Cardinal): Pointer; override;
  public
    constructor Create(Colors: PAggColor{$IFDEF UseGamma}; Profile: PInt8u
      {$ENDIF});
  end;

  TAggApplication = class(TPlatformSupport)
  private
    {$IFDEF UseGamma}
    FGammaProfile: TAggGammaControl;
    {$ENDIF}

    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLineUnpacked8;
    FSpanAllocator: TAggSpanAllocator;
    FMatrix: TAggTransAffine;

    FSplineRed, FSplineGreen, FSplineBlue, FSplineAlpha: TSplineControl;
    FRadioBoxRenderingBox: TAggControlRadioBox;

    FScale, FPrevScale, FAngle, FPrevAngle: Double;
    FPD, FCenter, FScalePoint, FPrevScalePoint: TPointDouble;
    FMouseMove: Boolean;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags); override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags); override;
  end;

constructor TColorFunctionProfile.Create(Colors: PAggColor{$IFDEF UseGamma};
  Profile: PInt8u{$ENDIF});
begin
  FColors := Colors;
{$IFDEF UseGamma}
  FGammaProfile := Profile;
{$ENDIF}
end;

function TColorFunctionProfile.GetSize;
begin
  Result := 256;
end;

function TColorFunctionProfile.ArrayOperator(I: Cardinal): Pointer;
var
  AggColor: PAggColor absolute Result;
begin
{$IFDEF UseGamma}
  Result := PAggColor(PtrComp(FColors) + PInt8u(PtrComp(FGammaProfile) + I)^ *
    SizeOf(TAggColor));
{$ELSE}
  AggColor := FColors;
  Inc(AggColor, I);
{$ENDIF}
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  Rgba: TAggColor;
  Fd : Text;
  Err: Integer;
  X, Y, X2, Y2, T: Double;
begin
  inherited Create(PixelFormat, FlipY);

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLineUnpacked8.Create;
  FSpanAllocator := TAggSpanAllocator.Create;
  FMatrix := TAggTransAffine.Create;

{$IFDEF UseGamma}
  FGammaProfile := TAggGammaControl.Create(10, 10, 200.0, 165, not FlipY);
{$ENDIF}
  FSplineRed := TSplineControl.Create(210, 10, 460, 45, 6, not FlipY);
  FSplineGreen := TSplineControl.Create(210, 50, 460, 85, 6, not FlipY);
  FSplineBlue := TSplineControl.Create(210, 90, 460, 125, 6, not FlipY);
  FSplineAlpha := TSplineControl.Create(210, 130, 460, 165, 6, not FlipY);
  FRadioBoxRenderingBox := TAggControlRadioBox.Create(10, 180, 200, 300,
    not FlipY);

  FPD := PointDouble(0);

  FCenter := PointDouble(CCenterX, CCenterY);

  FScale := 1;
  FPrevScale := 1;
  FAngle := 0;
  FPrevAngle := 0;
  FScalePoint := PointDouble(1);
  FPrevScalePoint := PointDouble(1);

  FMouseMove := False;

  {$IFDEF UseGamma}
  AddControl(FGammaProfile);
  {$ENDIF}
  AddControl(FSplineRed);
  AddControl(FSplineGreen);
  AddControl(FSplineBlue);
  AddControl(FSplineAlpha);
  AddControl(FRadioBoxRenderingBox);

  {$IFDEF UseGamma}
  FGammaProfile.SetBorderWidth(2, 2);
  {$ENDIF}

  Rgba.FromRgbaDouble(1, 0.8, 0.8);
  FSplineRed.BackgroundColor := Rgba;
  Rgba.FromRgbaDouble(0.8, 1, 0.8);
  FSplineGreen.BackgroundColor := Rgba;
  Rgba.FromRgbaDouble(0.8, 0.8, 1);
  FSplineBlue.BackgroundColor := Rgba;
  Rgba.White;
  FSplineAlpha.BackgroundColor := Rgba;

  FSplineRed.SetBorderWidth(1, 2);
  FSplineGreen.SetBorderWidth(1, 2);
  FSplineBlue.SetBorderWidth(1, 2);
  FSplineAlpha.SetBorderWidth(1, 2);
  FRadioBoxRenderingBox.SetBorderWidth(2, 2);

  FSplineRed.SetPoint(0, 0, 1);
  FSplineRed.SetPoint(1, 1 / 5, 4 / 5);
  FSplineRed.SetPoint(2, 2 / 5, 3 / 5);
  FSplineRed.SetPoint(3, 3 / 5, 2 / 5);
  FSplineRed.SetPoint(4, 4 / 5, 1 / 5);
  FSplineRed.SetPoint(5, 1, 0);
  FSplineRed.UpdateSpline;

  FSplineGreen.SetPoint(0, 0, 1);
  FSplineGreen.SetPoint(1, 1 / 5, 4 / 5);
  FSplineGreen.SetPoint(2, 2 / 5, 3 / 5);
  FSplineGreen.SetPoint(3, 3 / 5, 2 / 5);
  FSplineGreen.SetPoint(4, 4 / 5, 1 / 5);
  FSplineGreen.SetPoint(5, 1, 0);
  FSplineGreen.UpdateSpline;

  FSplineBlue.SetPoint(0, 0, 1);
  FSplineBlue.SetPoint(1, 1 / 5, 4 / 5);
  FSplineBlue.SetPoint(2, 2 / 5, 3 / 5);
  FSplineBlue.SetPoint(3, 3 / 5, 2 / 5);
  FSplineBlue.SetPoint(4, 4 / 5, 1 / 5);
  FSplineBlue.SetPoint(5, 1, 0);
  FSplineBlue.UpdateSpline;

  FSplineAlpha.SetPoint(0, 0, 1);
  FSplineAlpha.SetPoint(1, 1 / 5, 1);
  FSplineAlpha.SetPoint(2, 2 / 5, 1);
  FSplineAlpha.SetPoint(3, 3 / 5, 1);
  FSplineAlpha.SetPoint(4, 4 / 5, 1);
  FSplineAlpha.SetPoint(5, 1, 1);
  FSplineAlpha.UpdateSpline;

  FRadioBoxRenderingBox.AddItem('Circular');
  FRadioBoxRenderingBox.AddItem('Diamond');
  FRadioBoxRenderingBox.AddItem('Linear');
  FRadioBoxRenderingBox.AddItem('XY');
  FRadioBoxRenderingBox.AddItem('Sqrt(XY)');
  FRadioBoxRenderingBox.AddItem('Conic');
  FRadioBoxRenderingBox.SetCurrentItem(0);

  Err := IOResult;

  AssignFile(Fd, 'settings.dat');
  Reset(Fd);

  Err := IOResult;

  if Err = 0 then
  begin
    ReadLn(Fd, T);
    FCenter.X := T;
    ReadLn(Fd, T);
    FCenter.Y := T;
    ReadLn(Fd, T);
    FScale := T;
    ReadLn(Fd, T);
    FAngle := T;
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineRed.SetPoint(0, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineRed.SetPoint(1, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineRed.SetPoint(2, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineRed.SetPoint(3, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineRed.SetPoint(4, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineRed.SetPoint(5, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineGreen.SetPoint(0, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineGreen.SetPoint(1, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineGreen.SetPoint(2, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineGreen.SetPoint(3, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineGreen.SetPoint(4, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineGreen.SetPoint(5, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineBlue.SetPoint(0, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineBlue.SetPoint(1, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineBlue.SetPoint(2, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineBlue.SetPoint(3, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineBlue.SetPoint(4, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineBlue.SetPoint(5, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineAlpha.SetPoint(0, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineAlpha.SetPoint(1, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineAlpha.SetPoint(2, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineAlpha.SetPoint(3, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineAlpha.SetPoint(4, X, Y);
    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    FSplineAlpha.SetPoint(5, X, Y);

    FSplineRed.UpdateSpline;
    FSplineGreen.UpdateSpline;
    FSplineBlue.UpdateSpline;
    FSplineAlpha.UpdateSpline;

    ReadLn(Fd, X);
    ReadLn(Fd, Y);
    ReadLn(Fd, X2);
    ReadLn(Fd, Y2);

    {$IFDEF UseGamma}
    FGammaProfile.Values(X, Y, X2, Y2);
    {$ENDIF}

    Close(Fd);
  end;
end;

destructor TAggApplication.Destroy;
var
  Fd: Text;
  X1, Y1, X2, Y2: Double;
begin
  AssignFile(Fd, 'settings.dat');
  Rewrite(Fd);

  Writeln(Fd, FCenter.X:0:6);
  Writeln(Fd, FCenter.Y:0:6);
  Writeln(Fd, FScale:0:6);
  Writeln(Fd, FAngle:0:6);
  Writeln(Fd, FSplineRed.X[0]:0:6);
  Writeln(Fd, FSplineRed.Y[0]:0:6);
  Writeln(Fd, FSplineRed.X[1]:0:6);
  Writeln(Fd, FSplineRed.Y[1]:0:6);
  Writeln(Fd, FSplineRed.X[2]:0:6);
  Writeln(Fd, FSplineRed.Y[2]:0:6);
  Writeln(Fd, FSplineRed.X[3]:0:6);
  Writeln(Fd, FSplineRed.Y[3]:0:6);
  Writeln(Fd, FSplineRed.X[4]:0:6);
  Writeln(Fd, FSplineRed.Y[4]:0:6);
  Writeln(Fd, FSplineRed.X[5]:0:6);
  Writeln(Fd, FSplineRed.Y[5]:0:6);
  Writeln(Fd, FSplineGreen.X[0]:0:6);
  Writeln(Fd, FSplineGreen.Y[0]:0:6);
  Writeln(Fd, FSplineGreen.X[1]:0:6);
  Writeln(Fd, FSplineGreen.Y[1]:0:6);
  Writeln(Fd, FSplineGreen.X[2]:0:6);
  Writeln(Fd, FSplineGreen.Y[2]:0:6);
  Writeln(Fd, FSplineGreen.X[3]:0:6);
  Writeln(Fd, FSplineGreen.Y[3]:0:6);
  Writeln(Fd, FSplineGreen.X[4]:0:6);
  Writeln(Fd, FSplineGreen.Y[4]:0:6);
  Writeln(Fd, FSplineGreen.X[5]:0:6);
  Writeln(Fd, FSplineGreen.Y[5]:0:6);
  Writeln(Fd, FSplineBlue.X[0]:0:6);
  Writeln(Fd, FSplineBlue.Y[0]:0:6);
  Writeln(Fd, FSplineBlue.X[1]:0:6);
  Writeln(Fd, FSplineBlue.Y[1]:0:6);
  Writeln(Fd, FSplineBlue.X[2]:0:6);
  Writeln(Fd, FSplineBlue.Y[2]:0:6);
  Writeln(Fd, FSplineBlue.X[3]:0:6);
  Writeln(Fd, FSplineBlue.Y[3]:0:6);
  Writeln(Fd, FSplineBlue.X[4]:0:6);
  Writeln(Fd, FSplineBlue.Y[4]:0:6);
  Writeln(Fd, FSplineBlue.X[5]:0:6);
  Writeln(Fd, FSplineBlue.Y[5]:0:6);
  Writeln(Fd, FSplineAlpha.X[0]:0:6);
  Writeln(Fd, FSplineAlpha.Y[0]:0:6);
  Writeln(Fd, FSplineAlpha.X[1]:0:6);
  Writeln(Fd, FSplineAlpha.Y[1]:0:6);
  Writeln(Fd, FSplineAlpha.X[2]:0:6);
  Writeln(Fd, FSplineAlpha.Y[2]:0:6);
  Writeln(Fd, FSplineAlpha.X[3]:0:6);
  Writeln(Fd, FSplineAlpha.Y[3]:0:6);
  Writeln(Fd, FSplineAlpha.X[4]:0:6);
  Writeln(Fd, FSplineAlpha.Y[4]:0:6);
  Writeln(Fd, FSplineAlpha.X[5]:0:6);
  Writeln(Fd, FSplineAlpha.Y[5]:0:6);

  {$IFDEF UseGamma}
  FGammaProfile.Values(@X1, @Y1, @X2, @Y2);
  {$ENDIF}

  Writeln(Fd, X1:0:6);
  Writeln(Fd, Y1:0:6);
  Writeln(Fd, X2:0:6);
  Writeln(Fd, Y2:0:6);

  Close(Fd);

  {$IFDEF UseGamma}
  FGammaProfile.Free;
  {$ENDIF}
  FSplineRed.Free;
  FSplineGreen.Free;
  FSplineBlue.Free;
  FSplineAlpha.Free;
  FRadioBoxRenderingBox.Free;

  FMatrix.Free;
  FScanLine.Free;
  FRasterizer.Free;
  FSpanAllocator.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  RenScanCircle: TAggRendererScanLineAA;

  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  ColorProfile: array [0..255] of TAggColor;
  SplineData: array [0..3] of PDouble;

  Circle: TAggCircle;
  I: Integer;
  TransformedCircle: TAggConvTransform;

  GradFunc: TAggCustomGradient;
  GradRef: TAggGradientReflectAdaptor;

  Colors: TColorFunctionProfile;
  SpanInterpolator: TAggSpanInterpolatorLinear;
  SpanGradGen: TAggSpanGradient;
const
  CGradientClasses : array [0..5] of TAggCustomGradientClass = (
    TAggGradientRadial, TAggGradientDiamond, TAggGradientX, TAggGradientXY,
    TAggGradientSqrtXY, TAggGradientConic);
begin
  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8Black);

      // Render the controls
      {$IFDEF UseGamma}
      FGammaProfile.SetTextSize(8);
      RenderControl(FRasterizer, FScanLine, RenScan, FGammaProfile);
      {$ENDIF}

      RenderControl(FRasterizer, FScanLine, RenScan, FSplineRed);
      RenderControl(FRasterizer, FScanLine, RenScan, FSplineGreen);
      RenderControl(FRasterizer, FScanLine, RenScan, FSplineBlue);
      RenderControl(FRasterizer, FScanLine, RenScan, FSplineAlpha);
      RenderControl(FRasterizer, FScanLine, RenScan, FRadioBoxRenderingBox);

      // Rasterize Circle
      FMatrix.Reset;
      FMatrix.Translate(CCenterX, CCenterY);
      FMatrix.Multiply(GetTransAffineResizing);

      Circle := TAggCircle.Create(PointDouble(0), 110, 64);
      try
        TransformedCircle := TAggConvTransform.Create(Circle, FMatrix);
        try
          FRasterizer.AddPath(TransformedCircle);
        finally
          TransformedCircle.Free;
        end;
      finally
        Circle.Free;
      end;

      // Render
      SplineData[0] := FSplineRed.GetSpline;
      SplineData[1] := FSplineGreen.GetSpline;
      SplineData[2] := FSplineBlue.GetSpline;
      SplineData[3] := FSplineAlpha.GetSpline;
      for I := 0 to 255 do
      begin
        Rgba.FromRgbaDouble(SplineData[0]^, SplineData[1]^, SplineData[2]^,
          SplineData[3]^);

        Inc(SplineData[0]);
        Inc(SplineData[1]);
        Inc(SplineData[2]);
        Inc(SplineData[3]);

        ColorProfile[I] := Rgba;
      end;

      FMatrix.Reset;
      FMatrix.Scale(FScale, FScale);
      FMatrix.Scale(FScalePoint.X, FScalePoint.Y);
      FMatrix.Rotate(FAngle);
      FMatrix.Translate(FCenter.X, FCenter.Y);
      FMatrix.Multiply(GetTransAffineResizing);
      FMatrix.Invert;

      GradFunc := CGradientClasses[FRadioBoxRenderingBox.GetCurrentItem].Create;
      GradRef := TAggGradientReflectAdaptor.Create(GradFunc);

{$IFDEF UseGamma}
      Colors := TColorFunctionProfile.Create(@ColorProfile,
        PInt8u(FGammaProfile.Gamma));
{$ELSE}
      Colors := TColorFunctionProfile.Create(@ColorProfile);
{$ENDIF}
      try
        SpanInterpolator := TAggSpanInterpolatorLinear.Create(FMatrix);
        try
          SpanGradGen := TAggSpanGradient.Create(FSpanAllocator,
            SpanInterpolator, GradRef, Colors, 0, 100);
          try
            RenScanCircle := TAggRendererScanLineAA.Create(RendererBase,
              SpanGradGen);
            try
              RenderScanLines(FRasterizer, FScanLine, RenScanCircle);
            finally
              RenScanCircle.Free;
            end;
          finally
            SpanGradGen.Free;
          end;
        finally
          SpanInterpolator.Free;
        end;
      finally
        Colors.Free;
      end;

      // free gradients
      GradFunc.Free;
      GradRef.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
var
  X2, Y2, Dx, Dy: Double;
begin
  if FMouseMove then
  begin
    X2 := X;
    Y2 := Y;

    GetTransAffineResizing.InverseTransform(GetTransAffineResizing, @X2, @Y2);

    if mkfCtrl in Flags then
    begin
      Dx := X2 - FCenter.X;
      Dy := Y2 - FCenter.Y;

      FScalePoint.X := FPrevScalePoint.X * Dx / FPD.X;
      FScalePoint.Y := FPrevScalePoint.Y * Dy / FPD.Y;

      ForceRedraw;
    end
    else
    begin
      if mkfMouseLeft in Flags then
      begin
        FCenter.X := X2 + FPD.X;
        FCenter.Y := Y2 + FPD.Y;

        ForceRedraw;
      end;

      if mkfMouseRight in Flags then
      begin
        Dx := X2 - FCenter.X;
        Dy := Y2 - FCenter.Y;

        FScale := FPrevScale * Hypot(Dx, Dy) / Hypot(FPD.X, FPD.Y);
        FAngle := FPrevAngle + ArcTan2(Dy, Dx) - ArcTan2(FPD.Y, FPD.X);

        ForceRedraw;
      end;
    end;
  end;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  X2, Y2: Double;
begin
  FMouseMove := True;

  X2 := X;
  Y2 := Y;

  GetTransAffineResizing.InverseTransform(GetTransAffineResizing, @X2, @Y2);

  FPD := PointDouble(FCenter.X - X2, FCenter.Y - Y2);

  FPrevScale := FScale;
  FPrevAngle := FAngle + Pi;
  FPrevScalePoint := FScalePoint;

  ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FMouseMove := False;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
var
  I : Integer;
  Fd: Text;
  St: AnsiString;
  Rgba: TAggColor;
  Pntr: PInt8u;
begin
  if Key = Byte(' ') then
  begin
    AssignFile(Fd, 'colors.dat');
    Rewrite(Fd);

    for I := 0 to 255 do
    begin
      Rgba.FromRgbaDouble(PDouble(PtrComp(FSplineRed.GetSpline) + I * SizeOf(Double)
        )^, PDouble(PtrComp(FSplineGreen.GetSpline) + I * SizeOf(Double))^,
        PDouble(PtrComp(FSplineBlue.GetSpline) + I * SizeOf(Double))^,
        PDouble(PtrComp(FSplineAlpha.GetSpline) + I * SizeOf(Double))^);

      St := Format('    %3d, %3d, %3d, %3d,', [Rgba.Rgba8.R, Rgba.Rgba8.G,
        Rgba.Rgba8.B, Rgba.Rgba8.A]);

      Writeln(Fd, PAnsiChar(@St[1]));
    end;

    Close(Fd);

    AssignFile(Fd, 'profile.dat');
    Rewrite(Fd);

    {$IFDEF UseGamma}
    for I := 0 to 255 do
    begin
      Pntr := FGammaProfile.Gamma;
      Inc(Pntr, I);
      St := Format('%3d, ', [Pntr^]);
      Write(Fd, PAnsiChar(@St[1]));

      if (I and $F) = $F then
        Writeln(Fd);
    end;
    {$ENDIF}

    Close(Fd);
  end;

  if Key = Cardinal(kcF1) then
    DisplayMessage('This "sphere" is rendered with color gradients only. '
      + 'Initially there was an idea to compensate so called Mach Bands '
      + 'effect. To do so I added a Gradient profile functor. Then the concept '
      + 'was extended to set a color profile. As a result you can render '
      + 'simple geometrical objects in 2D looking like 3D ones. In this '
      + 'example you can construct your own color profile and select the '
      + 'Gradient function. There''re not so many Gradient functions in AGG, '
      + 'but you can easily add your own.'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse button to drag the "Gradient".'#13
      + 'Use the right mouse button to scale and rotate the "Gradient".'#13
      + 'Press the spacebar to write down the "colors.dat" file.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Gradients with Mach bands compensation (F1-Help)';

    if Init(512, 400, [wfResize, wfHardwareBuffer]) then
      Run;
  finally
    Free;
  end;
end.
