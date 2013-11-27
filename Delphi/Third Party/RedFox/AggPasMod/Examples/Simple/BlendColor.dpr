program BlendColor;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{ DEFINE AGG_GRAY8}
{ DEFINE AGG_BGR24}
{ DEFINE AGG_Rgb24}
{$DEFINE AGG_BGRA32}
{ DEFINE AGG_RgbA32}
{ DEFINE AGG_ARGB32}
{ DEFINE AGG_ABGR32}
{ DEFINE AGG_Rgb565}
{ DEFINE AGG_Rgb555}

{$IFDEF AGG_GRAY8}
  {$DEFINE AGG_PF8}
{$ELSE}
  {$IFDEF AGG_BGR24}
    {$DEFINE AGG_PF24}
  {$ELSE}
    {$IFDEF AGG_Rgb24}
      {$DEFINE AGG_PF24}
    {$ELSE}
      {$IFDEF AGG_BGRA32}
        {$DEFINE AGG_PF32}
      {$ELSE}
        {$IFDEF AGG_RgbA32}
          {$DEFINE AGG_PF32}
        {$ELSE}
          {$IFDEF AGG_ARGB32}
            {$DEFINE AGG_PF32}
          {$ELSE}
            {$IFDEF AGG_ABGR32}
              {$DEFINE AGG_PF32}
            {$ELSE}
              {$IFDEF AGG_Rgb555}
                {$DEFINE AGG_PF16}
              {$ELSE}
                {$IFDEF AGG_Rgb565}
                  {$DEFINE AGG_PF16}
                {$ELSE}
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

uses
{$IFDEF USE_FASTMM4}
  FastMM4,
{$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
{$IFNDEF AGG_PF8}
  AggPixelFormatGray in '..\..\Source\AggPixelFormatGray.pas',
{$ENDIF}
{$IFNDEF AGG_PF32}
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',
{$ENDIF}
  AggArray in '..\..\Source\AggArray.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggPolygonControl in '..\..\Source\Controls\AggPolygonControl.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvContour in '..\..\Source\AggConvContour.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggTransPerspective in '..\..\Source\AggTransPerspective.pas',
  AggBlur in '..\..\Source\AggBlur.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas'

{$I Pixel_Formats.inc}

const
  CFlipY = True;

  GGradientColors: array [0..1023] of Int8u = (
    255, 255, 255, 255, 255, 255, 254, 255, 255, 255, 254, 255, 255, 255, 254,
    255, 255, 255, 253, 255, 255, 255, 253, 255, 255, 255, 252, 255, 255, 255,
    251, 255, 255, 255, 250, 255, 255, 255, 248, 255, 255, 255, 246, 255, 255,
    255, 244, 255, 255, 255, 241, 255, 255, 255, 238, 255, 255, 255, 235, 255,
    255, 255, 231, 255, 255, 255, 227, 255, 255, 255, 222, 255, 255, 255, 217,
    255, 255, 255, 211, 255, 255, 255, 206, 255, 255, 255, 200, 255, 255, 254,
    194, 255, 255, 253, 188, 255, 255, 252, 182, 255, 255, 250, 176, 255, 255,
    249, 170, 255, 255, 247, 164, 255, 255, 246, 158, 255, 255, 244, 152, 255,
    254, 242, 146, 255, 254, 240, 141, 255, 254, 238, 136, 255, 254, 236, 131,
    255, 253, 234, 126, 255, 253, 232, 121, 255, 253, 229, 116, 255, 252, 227,
    112, 255, 252, 224, 108, 255, 251, 222, 104, 255, 251, 219, 100, 255, 251,
    216, 96, 255, 250, 214, 93, 255, 250, 211, 89, 255, 249, 208, 86, 255, 249,
    205, 83, 255, 248, 202, 80, 255, 247, 199, 77, 255, 247, 196, 74, 255, 246,
    193, 72, 255, 246, 190, 69, 255, 245, 187, 67, 255, 244, 183, 64, 255, 244,
    180, 62, 255, 243, 177, 60, 255, 242, 174, 58, 255, 242, 170, 56, 255, 241,
    167, 54, 255, 240, 164, 52, 255, 239, 161, 51, 255, 239, 157, 49, 255, 238,
    154, 47, 255, 237, 151, 46, 255, 236, 147, 44, 255, 235, 144, 43, 255, 235,
    141, 41, 255, 234, 138, 40, 255, 233, 134, 39, 255, 232, 131, 37, 255, 231,
    128, 36, 255, 230, 125, 35, 255, 229, 122, 34, 255, 228, 119, 33, 255, 227,
    116, 31, 255, 226, 113, 30, 255, 225, 110, 29, 255, 224, 107, 28, 255, 223,
    104, 27, 255, 222, 101, 26, 255, 221, 99, 25, 255, 220, 96, 24, 255, 219,
    93, 23, 255, 218, 91, 22, 255, 217, 88, 21, 255, 216, 86, 20, 255, 215, 83,
    19, 255, 214, 81, 18, 255, 213, 79, 17, 255, 212, 77, 17, 255, 211, 74, 16,
    255, 210, 72, 15, 255, 209, 70, 14, 255, 207, 68, 13, 255, 206, 66, 13, 255,
    205, 64, 12, 255, 204, 62, 11, 255, 203, 60, 10, 255, 202, 58, 10, 255, 201,
    56, 9, 255, 199, 55, 9, 255, 198, 53, 8, 255, 197, 51, 7, 255, 196, 50, 7,
    255, 195, 48, 6, 255, 193, 46, 6, 255, 192, 45, 5, 255, 191, 43, 5, 255,
    190, 42, 4, 255, 188, 41, 4, 255, 187, 39, 3, 255, 186, 38, 3, 255, 185, 37,
    2, 255, 183, 35, 2, 255, 182, 34, 1, 255, 181, 33, 1, 255, 179, 32, 1, 255,
    178, 30, 0, 255, 177, 29, 0, 255, 175, 28, 0, 255, 174, 27, 0, 255, 173, 26,
    0, 255, 171, 25, 0, 255, 170, 24, 0, 255, 168, 23, 0, 255, 167, 22, 0, 255,
    165, 21, 0, 255, 164, 21, 0, 255, 163, 20, 0, 255, 161, 19, 0, 255, 160, 18,
    0, 255, 158, 17, 0, 255, 156, 17, 0, 255, 155, 16, 0, 255, 153, 15, 0, 255,
    152, 14, 0, 255, 150, 14, 0, 255, 149, 13, 0, 255, 147, 12, 0, 255, 145, 12,
    0, 255, 144, 11, 0, 255, 142, 11, 0, 255, 140, 10, 0, 255, 139, 10, 0, 255,
    137, 9, 0, 255, 135, 9, 0, 255, 134, 8, 0, 255, 132, 8, 0, 255, 130, 7, 0,
    255, 128, 7, 0, 255, 126, 6, 0, 255, 125, 6, 0, 255, 123, 5, 0, 255, 121, 5,
    0, 255, 119, 4, 0, 255, 117, 4, 0, 255, 115, 4, 0, 255, 113, 3, 0, 255, 111,
    3, 0, 255, 109, 2, 0, 255, 107, 2, 0, 255, 105, 2, 0, 255, 103, 1, 0, 255,
    101, 1, 0, 255, 99, 1, 0, 255, 97, 0, 0, 255, 95, 0, 0, 255, 93, 0, 0, 255,
    91, 0, 0, 255, 90, 0, 0, 255, 88, 0, 0, 255, 86, 0, 0, 255, 84, 0, 0, 255,
    82, 0, 0, 255, 80, 0, 0, 255, 78, 0, 0, 255, 77, 0, 0, 255, 75, 0, 0, 255,
    73, 0, 0, 255, 72, 0, 0, 255, 70, 0, 0, 255, 68, 0, 0, 255, 67, 0, 0, 255,
    65, 0, 0, 255, 64, 0, 0, 255, 63, 0, 0, 255, 61, 0, 0, 255, 60, 0, 0, 255,
    59, 0, 0, 255, 58, 0, 0, 255, 57, 0, 0, 255, 56, 0, 0, 255, 55, 0, 0, 255,
    54, 0, 0, 255, 53, 0, 0, 255, 53, 0, 0, 255, 52, 0, 0, 255, 52, 0, 0, 255,
    51, 0, 0, 255, 51, 0, 0, 255, 51, 0, 0, 255, 50, 0, 0, 255, 50, 0, 0, 255,
    51, 0, 0, 255, 51, 0, 0, 255, 51, 0, 0, 255, 51, 0, 0, 255, 52, 0, 0, 255,
    52, 0, 0, 255, 53, 0, 0, 255, 54, 1, 0, 255, 55, 2, 0, 255, 56, 3, 0, 255,
    57, 4, 0, 255, 58, 5, 0, 255, 59, 6, 0, 255, 60, 7, 0, 255, 62, 8, 0, 255,
    63, 9, 0, 255, 64, 11, 0, 255, 66, 12, 0, 255, 68, 13, 0, 255, 69, 14, 0,
    255, 71, 16, 0, 255, 73, 17, 0, 255, 75, 18, 0, 255, 77, 20, 0, 255, 79, 21,
    0, 255, 81, 23, 0, 255, 83, 24, 0, 255, 85, 26, 0, 255, 87, 28, 0, 255, 90,
    29, 0, 255, 92, 31, 0, 255, 94, 33, 0, 255, 97, 34, 0, 255, 99, 36, 0, 255,
    102, 38, 0, 255, 104, 40, 0, 255, 107, 41, 0, 255, 109, 43, 0, 255, 112, 45,
    0, 255, 115, 47, 0, 255, 117, 49, 0, 255, 120, 51, 0, 255, 123, 52, 0, 255,
    126, 54, 0, 255, 128, 56, 0, 255, 131, 58, 0, 255, 134, 60, 0, 255, 137, 62,
    0, 255, 140, 64, 0, 255, 143, 66, 0, 255, 145, 68, 0, 255, 148, 70, 0, 255,
    151, 72, 0, 255, 154, 74, 0, 255);

type
  TAggApplication = class(TPlatformSupport)
  private
    FRadioBoxMethod, FRadioBoxBlendMode: TAggControlRadioBox;
    FSliderRadius: TAggControlSlider;

    FCheckBoxRed, FCheckBoxGreen, FCheckBoxBlue: TAggControlCheckBox;

    FShadowControl: TPolygonControl;

    FPathStorage: TAggPathStorage;
    FShape: TAggConvCurve;

    FRasterizerScanLineAA: TAggRasterizerScanLineAA;
    FScanLine : TAggScanLinePacked8;

    FShapeBounds: TRectDouble;

    FGray8Buffer: TAggPodArray;
    FGray8RenderingBuffer: array [0..1] of TAggRenderingBuffer;

    FColorLookUpTable: TAggPodArray;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnResize(Width, Height: Integer); override;
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
var
  ShapeMtx: TAggTransAffine;

  I: Cardinal;
  P: PInt8u;

  Rgba: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  FRadioBoxMethod := TAggControlRadioBox.Create(10.0, 10.0, 130.0, 55.0,
    not FlipY);
  FRadioBoxBlendMode := TAggControlRadioBox.Create(440, 5, 560, 395, not FlipY);
  FSliderRadius := TAggControlSlider.Create(140, 14, 430, 22, not FlipY);

  FCheckBoxRed := TAggControlCheckBox.Create(10, 65, 'Red', not FlipY);
  FCheckBoxGreen := TAggControlCheckBox.Create(10, 80, 'Green', not FlipY);
  FCheckBoxBlue := TAggControlCheckBox.Create(10, 95, 'Blue', not FlipY);

  FShadowControl := TPolygonControl.Create(4);

  FPathStorage := TAggPathStorage.Create;
  FShape := TAggConvCurve.Create(FPathStorage);

  FRasterizerScanLineAA := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;

  FGray8Buffer := TAggPodArray.Create(SizeOf(Int8u));
  FGray8RenderingBuffer[0] := TAggRenderingBuffer.Create;
  FGray8RenderingBuffer[1] := TAggRenderingBuffer.Create;

  FColorLookUpTable := TAggPodArray.Create(SizeOf(TAggColor));

  AddControl(FRadioBoxMethod);

  FRadioBoxMethod.SetTextSize(8);
  FRadioBoxMethod.AddItem('Single Color');
  FRadioBoxMethod.AddItem('Color LUT');
  FRadioBoxMethod.SetCurrentItem(1);

  AddControl(FSliderRadius);

  FSliderRadius.SetRange(0, 40);
  FSliderRadius.Value := 15.0;
  FSliderRadius.Caption := 'Blur Radius=%1.2f';

  AddControl(FCheckBoxRed);
  AddControl(FCheckBoxGreen);
  AddControl(FCheckBoxBlue);

  FCheckBoxRed.Status := True;
  FCheckBoxBlue.Status := True;

  AddControl(FRadioBoxBlendMode);

  FRadioBoxBlendMode.SetTextSize(6.6);
  FRadioBoxBlendMode.AddItem('no compositions');
  FRadioBoxBlendMode.AddItem('clear');
  FRadioBoxBlendMode.AddItem('src');
  FRadioBoxBlendMode.AddItem('dst');
  FRadioBoxBlendMode.AddItem('src-over');
  FRadioBoxBlendMode.AddItem('dst-over');
  FRadioBoxBlendMode.AddItem('src-in');
  FRadioBoxBlendMode.AddItem('dst-in');
  FRadioBoxBlendMode.AddItem('src-out');
  FRadioBoxBlendMode.AddItem('dst-out');
  FRadioBoxBlendMode.AddItem('src-atop');
  FRadioBoxBlendMode.AddItem('dst-atop');
  FRadioBoxBlendMode.AddItem('xor');
  FRadioBoxBlendMode.AddItem('plus');
  FRadioBoxBlendMode.AddItem('minus');
  FRadioBoxBlendMode.AddItem('multiply');
  FRadioBoxBlendMode.AddItem('screen');
  FRadioBoxBlendMode.AddItem('overlay');
  FRadioBoxBlendMode.AddItem('darken');
  FRadioBoxBlendMode.AddItem('lighten');
  FRadioBoxBlendMode.AddItem('color-dodge');
  FRadioBoxBlendMode.AddItem('color-burn');
  FRadioBoxBlendMode.AddItem('hard-light');
  FRadioBoxBlendMode.AddItem('soft-light');
  FRadioBoxBlendMode.AddItem('difference');
  FRadioBoxBlendMode.AddItem('exclusion');
  FRadioBoxBlendMode.AddItem('contrast');
  FRadioBoxBlendMode.AddItem('invert');
  FRadioBoxBlendMode.AddItem('invert-rgb');
  FRadioBoxBlendMode.SetCurrentItem(0);

  AddControl(FShadowControl);

  FShadowControl.InPolygonCheck := True;

  FPathStorage.RemoveAll;
  FPathStorage.MoveTo(28.47, 6.45);
  FPathStorage.Curve3(21.58, 1.12, 19.82, 0.29);
  FPathStorage.Curve3(17.19, -0.93, 14.21, -0.93);
  FPathStorage.Curve3(9.57, -0.93, 6.57, 2.25);
  FPathStorage.Curve3(3.56, 5.42, 3.56, 10.60);
  FPathStorage.Curve3(3.56, 13.87, 5.03, 16.26);
  FPathStorage.Curve3(7.03, 19.58, 11.99, 22.51);
  FPathStorage.Curve3(16.94, 25.44, 28.47, 29.64);
  FPathStorage.LineTo(28.47, 31.40);
  FPathStorage.Curve3(28.47, 38.09, 26.34, 40.58);
  FPathStorage.Curve3(24.22, 43.07, 20.17, 43.07);
  FPathStorage.Curve3(17.09, 43.07, 15.28, 41.41);
  FPathStorage.Curve3(13.43, 39.75, 13.43, 37.60);
  FPathStorage.LineTo(13.53, 34.77);
  FPathStorage.Curve3(13.53, 32.52, 12.38, 31.30);
  FPathStorage.Curve3(11.23, 30.08, 9.38, 30.08);
  FPathStorage.Curve3(7.57, 30.08, 6.42, 31.35);
  FPathStorage.Curve3(5.27, 32.62, 5.27, 34.81);
  FPathStorage.Curve3(5.27, 39.01, 9.57, 42.53);
  FPathStorage.Curve3(13.87, 46.04, 21.63, 46.04);
  FPathStorage.Curve3(27.59, 46.04, 31.40, 44.04);
  FPathStorage.Curve3(34.28, 42.53, 35.64, 39.31);
  FPathStorage.Curve3(36.52, 37.21, 36.52, 30.71);
  FPathStorage.LineTo(36.52, 15.53);
  FPathStorage.Curve3(36.52, 9.13, 36.77, 7.69);
  FPathStorage.Curve3(37.01, 6.25, 37.57, 5.76);
  FPathStorage.Curve3(38.13, 5.27, 38.87, 5.27);
  FPathStorage.Curve3(39.65, 5.27, 40.23, 5.62);
  FPathStorage.Curve3(41.26, 6.25, 44.19, 9.18);
  FPathStorage.LineTo(44.19, 6.45);
  FPathStorage.Curve3(38.72, -0.88, 33.74, -0.88);
  FPathStorage.Curve3(31.35, -0.88, 29.93, 0.78);
  FPathStorage.Curve3(28.52, 2.44, 28.47, 6.45);
  FPathStorage.ClosePolygon;

  FPathStorage.MoveTo(28.47, 9.62);
  FPathStorage.LineTo(28.47, 26.66);
  FPathStorage.Curve3(21.09, 23.73, 18.95, 22.51);
  FPathStorage.Curve3(15.09, 20.36, 13.43, 18.02);
  FPathStorage.Curve3(11.77, 15.67, 11.77, 12.89);
  FPathStorage.Curve3(11.77, 9.38, 13.87, 7.06);
  FPathStorage.Curve3(15.97, 4.74, 18.70, 4.74);
  FPathStorage.Curve3(22.41, 4.74, 28.47, 9.62);
  FPathStorage.ClosePolygon;

  ShapeMtx := TAggTransAffine.Create;
  ShapeMtx.Scale(4.0);
  ShapeMtx.Translate(150, 100);

  FPathStorage.Transform(ShapeMtx);

  BoundingRectSingle(FShape, 0, @FShapeBounds.X1, @FShapeBounds.Y1,
    @FShapeBounds.X2, @FShapeBounds.Y2);

  FShadowControl.Xn[0] := FShapeBounds.X1;
  FShadowControl.Yn[0] := FShapeBounds.Y1;
  FShadowControl.Xn[1] := FShapeBounds.X2;
  FShadowControl.Yn[1] := FShapeBounds.Y1;
  FShadowControl.Xn[2] := FShapeBounds.X2;
  FShadowControl.Yn[2] := FShapeBounds.Y2;
  FShadowControl.Xn[3] := FShapeBounds.X1;
  FShadowControl.Yn[3] := FShapeBounds.Y2;

  Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.3);
  FShadowControl.LineColor := Rgba;

  FColorLookUpTable.Resize(256);

  P := @GGradientColors[0];
  I := 0;

  while I < 256 do
  begin
    if I > 63 then
      PAggColor(FColorLookUpTable[I]).FromRgbaInteger(PInt8u(P)^,
        PInt8u(PtrComp(P) + SizeOf(Int8u))^,
        PInt8u(PtrComp(P) + 2 * SizeOf(Int8u))^, 255)
    else
      PAggColor(FColorLookUpTable[I]).FromRgbaInteger(PInt8u(P)^,
        PInt8u(PtrComp(P) + SizeOf(Int8u))^,
        PInt8u(PtrComp(P) + 2 * SizeOf(Int8u))^, I * 4);

    // PAggColor(FColorLookUpTable.array_operator(i ) ).preMultiply;

    Inc(PtrComp(P), 4 * SizeOf(Int8u));
    Inc(I);
  end;
  ShapeMtx.Free;
end;

destructor TAggApplication.Destroy;
begin
  FRadioBoxMethod.Free;
  FRadioBoxBlendMode.Free;
  FSliderRadius.Free;
  FCheckBoxRed.Free;
  FCheckBoxGreen.Free;
  FCheckBoxBlue.Free;

  FShadowControl.Free;

  FPathStorage.Free;
  FShape.Free;

  FRasterizerScanLineAA.Free;
  FScanLine.Free;

  FGray8Buffer.Free;
  FGray8RenderingBuffer[0].Free;
  FGray8RenderingBuffer[1].Free;

  FColorLookUpTable.Free;

  inherited;
end;

procedure TAggApplication.OnResize(Width, Height: Integer);
begin
  FGray8Buffer.Resize(Width * Height);
  FGray8RenderingBuffer[0].Attach(FGray8Buffer.Data, Width, Height, Width);
end;

procedure TAggApplication.OnDraw;
var
  Rgba: TAggColor;
  RenScan: TAggRendererScanLineAASolid;

  PixfGray8, Pixf, Pixf2, PixfBlend: TAggPixelFormatProcessor;

  RendererBaseGray8, RendererBase, RendererBaseBlend: TAggRendererBase;

  ShadowPersp: TAggTransPerspective;
  ShadowTrans: TAggConvTransform;

  Bbox, Cl: TRectDouble;

  Tm : Double;
  Txt: TAggGsvText;
  St: TAggConvStroke;
  R, G, B: Integer;
begin
  // Initialize structures
  FRasterizerScanLineAA.SetClipBox(0, 0, Width, Height);

  PixelFormatGray8(PixfGray8, FGray8RenderingBuffer[0]);

  RendererBaseGray8 := TAggRendererBase.Create(PixfGray8, True);

  Rgba.FromValueInteger(0);
  RendererBaseGray8.Clear(@Rgba);

  PixelFormatCustomBlendRgba(PixfBlend, RenderingBufferWindow,
    BlendModeAdaptorRgba, CAggOrderBgra);
  RendererBaseBlend := TAggRendererBase.Create(PixfBlend, True);

  if FRadioBoxBlendMode.GetCurrentItem > 0 then
    PixfBlend.BlendMode := TAggBlendMode(FRadioBoxBlendMode.GetCurrentItem - 1);

  CPixelFormat(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      Rgba.FromRgbaDouble(1, 0.95, 0.95);
      RendererBase.Clear(@Rgba);

      // Render the controls Before
      RenderControl(FRasterizerScanLineAA, FScanLine, RenScan, FRadioBoxMethod);
      RenderControl(FRasterizerScanLineAA, FScanLine, RenScan, FSliderRadius);
      RenderControl(FRasterizerScanLineAA, FScanLine, RenScan, FCheckBoxRed);
      RenderControl(FRasterizerScanLineAA, FScanLine, RenScan, FCheckBoxGreen);
      RenderControl(FRasterizerScanLineAA, FScanLine, RenScan, FCheckBoxBlue);
      RenderControl(FRasterizerScanLineAA, FScanLine, RenScan, FShadowControl);

      ShadowPersp := TAggTransPerspective.Create(FShapeBounds.X1, FShapeBounds.Y1,
        FShapeBounds.X2, FShapeBounds.Y2, Pointer(FShadowControl.Polygon));
      try
        ShadowTrans := TAggConvTransform.Create(FShape, ShadowPersp);
        try
          if FCheckBoxRed.Status then
            R := 100
          else
            R := 0;

          if FCheckBoxGreen.Status then
            G := 100
          else
            G := 0;

          if FCheckBoxBlue.Status then
            B := 100
          else
            B := 0;

          StartTimer;

          // Render shadow
          FRasterizerScanLineAA.AddPath(ShadowTrans);

          Rgba.FromValueInteger(255);
          RenderScanLinesAASolid(FRasterizerScanLineAA, FScanLine,
            RendererBaseGray8, @Rgba);

          // Calculate the bounding box and extend it by the blur radius
          BoundingRectSingle(ShadowTrans, 0, @Bbox.X1, @Bbox.Y1, @Bbox.X2,
            @Bbox.Y2);

          Bbox.X1 := Bbox.X1 - FSliderRadius.Value;
          Bbox.Y1 := Bbox.Y1 - FSliderRadius.Value;
          Bbox.X2 := Bbox.X2 + FSliderRadius.Value;
          Bbox.Y2 := Bbox.Y2 + FSliderRadius.Value;

          Cl := RectDouble(0, 0, Width, Height);

          if Bbox.Clip(@Cl) then
          begin
            // Create a new pixel Renderer and attach it to the main one as
            // a child image. It returns true if the attachment suceeded. It
            // fails if the rectangle (bbox) is fully clipped.
            PixelFormatGray8(Pixf2, FGray8RenderingBuffer[1]);
            try
              if Pixf2.Attach(PixfGray8, Trunc(Bbox.X1), Trunc(Bbox.Y1),
                Trunc(Bbox.X2), Trunc(Bbox.Y2)) then
                StackBlurGray8(Pixf2, UnsignedRound(FSliderRadius.Value),
                  UnsignedRound(FSliderRadius.Value));

              if FRadioBoxMethod.GetCurrentItem = 0 then
              begin
                Rgba.FromRgbaInteger(R, G, B);

                if FRadioBoxBlendMode.GetCurrentItem = 0 then
                  RendererBase.BlendFromColor(Pixf2, @Rgba, nil, Trunc(Bbox.X1),
                    Trunc(Bbox.Y1))
                else
                  RendererBaseBlend.BlendFromColor(Pixf2, @Rgba, nil,
                    Trunc(Bbox.X1), Trunc(Bbox.Y1));

              end
              else if FRadioBoxBlendMode.GetCurrentItem = 0 then
                RendererBase.BlendFromLUT(Pixf2, FColorLookUpTable.Data, nil,
                  Trunc(Bbox.X1), Trunc(Bbox.Y1))
              else
                RendererBaseBlend.BlendFromLUT(Pixf2, FColorLookUpTable.Data,
                  nil, Trunc(Bbox.X1), Trunc(Bbox.Y1));
            finally
              Pixf2.Free;
            end;
          end;

          Tm := GetElapsedTime;

          // Info
          Txt := TAggGsvText.Create;
          try
            Txt.SetSize(10.0);

            St := TAggConvStroke.Create(Txt);
            try
              St.Width := 1.5;

              Txt.SetStartPoint(140, 30);
              Txt.SetText(Format('%3.2f ms', [Tm]));

              FRasterizerScanLineAA.AddPath(St);
            finally
              St.Free;
            end;
          finally
            Txt.Free;
          end;

          Rgba.Black;
          RenderScanLinesAASolid(FRasterizerScanLineAA, FScanLine, RendererBase,
            @Rgba);

          // Render the controls After
          RenderControl(FRasterizerScanLineAA, FScanLine, RenScan,
            FRadioBoxBlendMode);
        finally
          ShadowTrans.Free;
        end;
      finally
        ShadowPersp.Free;
      end;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;

  // Free AGG resources
  RendererBaseGray8.Free;
  RendererBaseBlend.Free;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FShadowControl.OnMouseMove(X, Y, False) then
      ForceRedraw;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FShadowControl.OnMouseButtonDown(X, Y) then
      ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FShadowControl.OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Now you can blur rendered images rather fast! There '
      + 'two algorithms are used: Stack Blur by Mario Klingemann and Fast '
      + 'Recursive Gaussian Filter. The speed of both methods does not '
      + 'depend on the filter radius. Mario''s method works 3-5'
      + 'times faster; it doesn''t produce exactly Gaussian response,'
      + 'but pretty fair for most practical purposes. The recursive filter'
      + 'uses floating point arithmetic and works sLower. But it is true'
      + 'Gaussian filter, with theoretically infinite impulse response.'
      + 'The radius (actually 2*sigma value) can be fractional and the'
      + 'filter produces quite adequate result.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Gaussian and Stack Blur (F1-Help)';

    if Init(570, 400, []) then
      Run;
  finally
    Free;
  end;
end.
