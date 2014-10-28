program FlashRasterizer;

// AggPas 2.4 RM3 Demo application
// Note: Press FX1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  SysUtils,

  AggPlatformSupport, // please add the path to this file manually
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggArray in '..\..\Source\AggArray.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',

  AggTransViewport in '..\..\Source\AggTransViewport.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLineBin in '..\..\Source\AggScanLineBin.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerCompoundAA in '..\..\Source\AggRasterizerCompoundAA.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggMath in '..\..\Source\AggMath.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas';

const
  CFlipY = False;

type
  PPathStyle = ^TPathStyle;

  TPathStyle = record
    PathID: Cardinal;
    LeftFill, RightFill, Line: Integer;
  end;

  TCompoundShape = class(TAggVertexSource)
  private
    FPath  : TAggPathStorage;
    FAffine: TAggTransAffine;
    FCurve : TAggConvCurve;
    FTrans : TAggConvTransform;
    FStyles: TAggPodBVector;

    Ffd: TApiFile;
    function GetScale: Double; overload;
  protected
    function GetPathID(I: Cardinal): Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Open(FileName: AnsiString): Boolean;
    function ReadNext: Boolean;

    function Paths: Cardinal;
    function Style(I: Cardinal): PPathStyle;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    procedure SetScale(W, H: Double);
    procedure SetApproximationScale(S: Double);

    function HitTest(X, Y, R: Double): Integer;

    procedure ModifyVertex(I: Cardinal; X, Y: Double);

    property Scale: Double read GetScale;
  end;

  // Testing class, color provider and Span generator
  TTestStyles = class(TAggCustomStyleHandler)
  private
    FSolidColors, FGradient: PAggColor;
  public
    constructor Create(SolidColors, Gradient: PAggColor);

    function IsSolid(Style: Cardinal): Boolean; override;
    function GetColor(Style: Cardinal): PAggColor; override;

    procedure GenerateSpan(Span: PAggColor; X, Y: Integer;
      Len, Style: Cardinal); override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FShape   : TCompoundShape;
    FColors  : array [0..99] of TAggColor;
    FScale   : TAggTransAffine;
    FGamma   : TAggGammaLut8;
    FGradient: TAggPodArray;

    FPointIndex, FHitX, FHitY: Integer;
  protected
    function Open(FileName: AnsiString): Boolean;
    procedure ReadNext;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TCompoundShape }

constructor TCompoundShape.Create;
begin
  FPath := TAggPathStorage.Create;
  FAffine := TAggTransAffine.Create;
  FCurve := TAggConvCurve.Create(FPath);
  FTrans := TAggConvTransform.Create(FCurve, FAffine);
  FStyles := TAggPodBVector.Create(SizeOf(TPathStyle));
end;

destructor TCompoundShape.Destroy;
begin
  FPath.Free;
  FCurve.Free;
  FStyles.Free;
  FAffine.Free;
  FTrans.Free;

  if Ffd.IsOpened then
    ApiCloseFile(Ffd);

  inherited;
end;

function TCompoundShape.Open(FileName: AnsiString): Boolean;
begin
  Result := ApiOpenFile(Ffd, FileName);
end;

function Fgets(Buf: PAnsiChar; Max: Integer; var F: TApiFile): PAnsiChar;
var
  Read: Integer;
begin
  Result := Buf;

  while Max > 1 do
  begin
    if not ApiReadFile(F, Buf, 1, read) then
    begin
      Result := nil;

      Exit;
    end;

    if read = 0 then
      if Buf = Result then
      begin
        Result := nil;

        Exit;

      end
      else
        Break;

    case Buf^ of
      #13, #10, #9:
        Break;
    end;

    Dec(Max);
    Inc(PtrComp(Buf), read);
  end;

  if Max >= 1 then
    Buf^ := #0;
end;

var
  GBuffer: PAnsiChar;

function StrtOk(Buff: PAnsiChar): PAnsiChar;
begin
  Result := nil;

  if Buff <> nil then
    GBuffer := Buff;

  while (GBuffer <> nil) and (GBuffer^ <> #0) do
  begin
    if Result = nil then
      Result := GBuffer;

    case GBuffer^ of
      ' ', #13, #10:
        begin
          GBuffer^ := #0;

          Inc(PtrComp(GBuffer));

          Break;
        end;
    end;

    Inc(PtrComp(GBuffer));
  end;
end;

function TCompoundShape.ReadNext: Boolean;
var
  Ax, Ay, Cx, Cy: Double;
  Buf: array [0..1023] of AnsiChar;
  Ts: PAnsiChar;
  TempStyle: TPathStyle;
  Code: Integer;
begin
  FPath.RemoveAll;
  FStyles.RemoveAll;

  if Ffd.IsOpened then
  begin
    repeat
      if Fgets(@Buf[0], 1022, Ffd) = nil then
      begin
        Result := False;

        Exit;
      end;

      if Buf[0] = '=' then
        Break;

    until False;

    while Fgets(@Buf[0], 1022, Ffd) <> nil do
    begin
      if Buf[0] = '!' then
        Break;

      if Buf[0] = 'P' then
      begin
        // BeginPath
        TempStyle.PathID := FPath.StartNewPath;

        Ts := StrtOk(@Buf[0]); // Path;
        Ts := StrtOk(nil); // left_style

        Val(PAnsiChar(Ts), TempStyle.LeftFill, Code);

        Ts := StrtOk(nil); // right_style

        Val(PAnsiChar(Ts), TempStyle.RightFill, Code);

        Ts := StrtOk(nil); // line_style

        Val(PAnsiChar(Ts), TempStyle.Line, Code);

        Ts := StrtOk(nil); // ax

        Val(PAnsiChar(Ts), Ax, Code);

        Ts := StrtOk(nil); // ay

        Val(PAnsiChar(Ts), Ay, Code);

        FPath.MoveTo(Ax, Ay);
        FStyles.Add(@TempStyle);
      end;

      if Buf[0] = 'C' then
      begin
        Ts := StrtOk(@Buf[0]); // Curve;
        Ts := StrtOk(nil); // cx

        Val(PAnsiChar(Ts), Cx, Code);

        Ts := StrtOk(nil); // cy

        Val(PAnsiChar(Ts), Cy, Code);

        Ts := StrtOk(nil); // ax

        Val(PAnsiChar(Ts), Ax, Code);

        Ts := StrtOk(nil); // ay

        Val(PAnsiChar(Ts), Ay, Code);

        FPath.Curve3(Cx, Cy, Ax, Ay);
      end;

      if Buf[0] = 'L' then
      begin
        Ts := StrtOk(@Buf[0]); // Line;
        Ts := StrtOk(nil); // ax

        Val(PAnsiChar(Ts), Ax, Code);

        Ts := StrtOk(nil); // ay

        Val(PAnsiChar(Ts), Ay, Code);

        FPath.LineTo(Ax, Ay);
      end;

      if Buf[0] = '<' then
      begin
        // EndPath
      end;
    end;

    Result := True;

  end
  else
    Result := False;
end;

function TCompoundShape.GetPathID(I: Cardinal): Cardinal;
begin
  Result := PPathStyle(FStyles[I])^.PathID;
end;

function TCompoundShape.Paths: Cardinal;
begin
  Result := FStyles.Size;
end;

function TCompoundShape.Style(I: Cardinal): PPathStyle;
begin
  Result := PPathStyle(FStyles[I]);
end;

procedure TCompoundShape.Rewind(PathID: Cardinal);
begin
  FTrans.Rewind(PathID);
end;

function TCompoundShape.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FTrans.Vertex(X, Y);
end;

function TCompoundShape.GetScale: Double;
begin
  Result := FAffine.GetScale;
end;

procedure TCompoundShape.SetScale(W, H: Double);
var
  Rect: TRectDouble;
  Vp: TAggTransViewport;
begin
  FAffine.Reset;

  BoundingRectVertexSource(FPath, Self, 0, FStyles.Size, Rect);

  if (Rect.X1 < Rect.X2) and (Rect.Y1 < Rect.Y2) then
  begin
    Vp := TAggTransViewport.Create;
    try
      Vp.PreserveAspectRatio(0.5, 0.5, arMeet);

      Vp.WorldViewport(Rect);
      Vp.DeviceViewport(0, 0, W, H);
      Vp.ToAffine(FAffine);
    finally
      Vp.Free;
    end;
  end;

  FCurve.ApproximationScale := FAffine.GetScale;
end;

procedure TCompoundShape.SetApproximationScale(S: Double);
begin
  FCurve.ApproximationScale := FAffine.GetScale * S;
end;

function TCompoundShape.HitTest(X, Y, R: Double): Integer;
var
  I, Cmd: Cardinal;
  Vx, Vy: Double;
begin
  FAffine.InverseTransform(FAffine, @X, @Y);

  R := R / FAffine.GetScale;
  I := 0;

  while I < FPath.TotalVertices do
  begin
    Cmd := FPath.SetVertex(I, @Vx, @Vy);

    if IsVertex(Cmd) then
      if CalculateDistance(X, Y, Vx, Vy) <= R then
      begin
        Result := I;

        Exit;
      end;

    Inc(I);
  end;

  Result := -1;
end;

procedure TCompoundShape.ModifyVertex(I: Cardinal; X, Y: Double);
begin
  FAffine.InverseTransform(FAffine, @X, @Y);
  FPath.ModifyVertex(I, X, Y);
end;


{ TTestStyles }

constructor TTestStyles.Create(SolidColors, Gradient: PAggColor);
begin
  FSolidColors := SolidColors;
  FGradient := Gradient;
end;

// Suppose that style=1 is a Gradient
function TTestStyles.IsSolid(Style: Cardinal): Boolean;
begin
  Result := Style <> 1; // true;
end;

// Just returns a color
function TTestStyles.GetColor(Style: Cardinal): PAggColor;
begin
  Result := PAggColor(PtrComp(FSolidColors) + Style * SizeOf(TAggColor));
end;

// Generate Span. In our test case only one style (style=1)
// can be a Span generator, so that, parameter "style"
// isn't used here.
procedure TTestStyles.GenerateSpan(Span: PAggColor; X, Y: Integer;
  Len, Style: Cardinal);
begin
  Move(PAggColor(PtrComp(FGradient) + X * SizeOf(TAggColor))^, Span^,
    SizeOf(TAggColor) * Len);
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  I: Cardinal;
  C1, C2, C3: Integer;
begin
  inherited Create(PixelFormat, FlipY);

  FShape := TCompoundShape.Create;
  FScale := TAggTransAffine.Create;
  FGamma := TAggGammaLut8.Create;
  FGradient := TAggPodArray.Create(SizeOf(TAggColor));

  FGamma.Gamma := 2;

  FPointIndex := -1;
  FHitX := -1;
  FHitY := -1;

  I := 0;

  while I < 100 do
  begin
    C1 := Random($100);
    C2 := Random($100);
    C3 := Random($100);

    FColors[I].FromRgbaInteger(C3, C2, C1, 230);

    FColors[I].ApplyGammaDir(FGamma);
    FColors[I].PreMultiply;

    Inc(I);
  end;
end;

destructor TAggApplication.Destroy;
begin
  FShape.Free;
  FGamma.Free;
  FGradient.Free;
  FScale.Free;

  inherited;
end;

function TAggApplication.Open(FileName: AnsiString): Boolean;
begin
  Result := FShape.Open(FullFileName(FileName));
end;

procedure TAggApplication.ReadNext;
begin
  FShape.ReadNext;
  FShape.SetScale(Width, Height);
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  Rgba, C1, C2: TAggColor;

  RendererBase: TAggRendererBase;

  RenScan: TAggRendererScanLineAASolid;
  Ras: TAggRasterizerScanLineAA;

  Rasc: TAggRasterizerCompoundAADouble;

  ScanLn: TAggScanLineUnpacked8;
  ScanLnBin: TAggScanLineBin;

  I, W: Cardinal;

  Shape : TAggConvTransform;
  Stroke: TAggConvStroke;

  StyleHandler: TTestStyles;

  SpanAllocator: TAggSpanAllocator;

  Tfill, Tstroke: Double;

  DrawStrokes: Boolean;

  Txt : TAggGsvText;
  TxtStroke: TAggConvStroke;
begin
  // Initialize structures
  PixelFormatBgra32Pre(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Rgba.FromRgbaDouble(1, 1, 0.95);
    RendererBase.Clear(@Rgba);

    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      W := Trunc(Width);

      FGradient.Resize(W);

      C1.FromRgbaInteger(255, 0, 0, 180);
      C2.FromRgbaInteger(0, 0, 255, 180);

      I := 0;

      while I < W do
      begin
        PAggColor(FGradient[I])^ := Gradient(C1, C2, I / Width);

        PAggColor(FGradient[I]).PreMultiply;

        Inc(I);
      end;

      Ras := TAggRasterizerScanLineAA.Create;
      Rasc := TAggRasterizerCompoundAADouble.Create;

      ScanLn := TAggScanLineUnpacked8.Create;
      ScanLnBin := TAggScanLineBin.Create;

      Shape := TAggConvTransform.Create(FShape, FScale);
      Stroke := TAggConvStroke.Create(Shape);

      StyleHandler := TTestStyles.Create(@FColors[0], FGradient.Data);
      SpanAllocator := TAggSpanAllocator.Create;

      FShape.SetApproximationScale(FScale.GetScale);

      // Fill shape
      Rasc.SetClipBox(0, 0, Width, Height);
      Rasc.Reset;
      // Rasc.FillingRule := frEvenOdd;

      StartTimer;

      I := 0;

      while I < FShape.Paths do
      begin
        if (FShape.Style(I).LeftFill >= 0) or (FShape.Style(I).RightFill >= 0)
        then
        begin
          Rasc.Styles(FShape.Style(I).LeftFill, FShape.Style(I).RightFill);

          Rasc.AddPath(Shape, FShape.Style(I).PathID);
        end;

        Inc(I);
      end;

      RenderScanLinesCompound(Rasc, ScanLn, ScanLnBin, RendererBase,
        SpanAllocator, StyleHandler);

      Tfill := GetElapsedTime;

      // Hit-test test
      DrawStrokes := True;

      if (FHitX >= 0) and (FHitY >= 0) then
        if Rasc.HitTest(FHitX, FHitY) then
          DrawStrokes := False;

      // Draw strokes
      StartTimer;

      if DrawStrokes then
      begin
        Ras.SetClipBox(0, 0, Width, Height);

        Stroke.Width := Sqrt(FScale.GetScale);
        Stroke.LineJoin := ljRound;
        Stroke.LineCap := lcRound;

        I := 0;

        while I < FShape.Paths do
        begin
          Ras.Reset;

          if FShape.Style(I).Line >= 0 then
          begin
            Ras.AddPath(Stroke, FShape.Style(I).PathID);

            Rgba.FromRgbaInteger(0, 0, 0, 128);
            RenScan.SetColor(@Rgba);
            RenderScanLines(Ras, ScanLn, RenScan);
          end;

          Inc(I);
        end;
      end;

      Tstroke := GetElapsedTime;

      // Render Text
      Txt := TAggGsvText.Create;
      Txt.SetSize(8);
      Txt.Flip := True;

      TxtStroke := TAggConvStroke.Create(Txt);
      TxtStroke.Width := 1.6;
      TxtStroke.LineCap := lcRound;

      Txt.SetStartPoint(10, 20);
      Txt.SetText(Format('Fill=%.2fms (%dFPS) Stroke=%.2fms (%dFPS) '
        + 'Total=%.2fms (%dFPS) Space: Next Shape'#13#13 + '+/- : ZoomIn/'
        + 'ZoomOut (with respect to the mouse pointer)', [Tfill,
        Trunc(1000 / Tfill), Tstroke, Trunc(1000 / Tstroke),
        Tfill + Tstroke, Trunc(1000 / (Tfill + Tstroke))]));

      Ras.AddPath(TxtStroke);
      RenScan.SetColor(CRgba8Black);

      RenderScanLines(Ras, ScanLn, RenScan);

      // Gamma adjust
      if FGamma.Gamma <> 1 then
        Pixf.ApplyGammaInv(FGamma, CAggOrderBgra);

      // Free AGG resources
      Ras.Free;
      Rasc.Free;
      ScanLn.Free;
      ScanLnBin.Free;

      Shape.Free;
      Stroke.Free;

      SpanAllocator.Free;

      Txt.Free;
      TxtStroke.Free;

      StyleHandler.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  Xd, Yd: Double;
begin
  if [mkfMouseLeft, mkfMouseRight] * Flags <> [] then
    OnMouseButtonUp(X, Y, Flags)

  else if FPointIndex >= 0 then
  begin
    Xd := X;
    Yd := Y;

    FScale.InverseTransform(FScale, @Xd, @Yd);
    FShape.ModifyVertex(FPointIndex, Xd, Yd);

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  Xd, Yd, R: Double;
begin
  if mkfMouseLeft in Flags then
  begin
    Xd := X;
    Yd := Y;
    R := 4 / FScale.GetScale;

    FScale.InverseTransform(FScale, @Xd, @Yd);

    FPointIndex := FShape.HitTest(Xd, Yd, R);

    ForceRedraw;
  end;

  if mkfMouseRight in Flags then
  begin
    FHitX := X;
    FHitY := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FPointIndex := -1;
  FHitX := -1;
  FHitY := -1;

  ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Demonstration of Flash Compound shape Rasterizer. The '
      + 'Rasterizer accepts vectorial data in a form of Flash paths, that is, '
      + 'with two fill styles, fill on the left and fill on the right of the '
      + 'path. Then it produces a number of ScanLines with corresponding '
      + 'styles and requests for the colors and/or Gradients, images, etc. '
      + 'The algorithm takes care of anti-aliasing and perfect stitching '
      + 'between fill areas.'#13#13
      + 'How to play with:'#13#13
      + 'Space = Load next shape'#13
      + '+ & - Key = ZoomIn/ZoomOut (with respect to the mouse pointer)'#13
      + 'Right & Left Key = Rotate (with respect to the mouse pointer)'#13
      + 'Left click & drag to modify shape points');

  if Key = Cardinal(' ') then
  begin
    FShape.ReadNext;
    FShape.SetScale(Width, Height);
    ForceRedraw;
  end;

  if (Key = Cardinal('+')) or (Key = Cardinal(kcPadPlus)) then
  begin
    FScale.Translate(-X, -Y);
    FScale.Scale(1.1);
    FScale.Translate(X, Y);

    ForceRedraw;
  end;

  if (Key = Cardinal('-')) or (Key = Cardinal(kcPadMinus)) then
  begin
    FScale.Translate(-X, -Y);
    FScale.Scale(1 / 1.1);
    FScale.Translate(X, Y);

    ForceRedraw;
  end;

  if Key = Cardinal(kcLeft) then
  begin
    FScale.Translate(-X, -Y);
    FScale.Rotate(-Pi / 20);
    FScale.Translate(X, Y);

    ForceRedraw;
  end;

  if Key = Cardinal(kcRight) then
  begin
    FScale.Translate(-X, -Y);
    FScale.Rotate(Pi / 20);
    FScale.Translate(X, Y);

    ForceRedraw;
  end;
end;

var
  Str: AnsiString;
  FileName, P, N, X: ShortString;

begin
  FileName := 'shapes.txt';

{$IFDEF WIN32}
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    FileName := FoldName(P, N, X);
  end;
{$ENDIF }

  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'AGG Example - Flash Rasterizer (F1-Help)';

    if not Open(FileName) then
    begin
      Str := 'File not found: ' + FileName;
      if FileName = 'shapes.txt' then
        Str := Str + #13#13 + 'Download http://www.antigrain.com/' + FileName
          + #13 + 'or copy it from another directory if available.';

      DisplayMessage(Str);
    end
    else if Init(655, 520, [wfResize]) then
    begin
      ReadNext;
      Run;
    end;
  finally
    Free;
  end;
end.
