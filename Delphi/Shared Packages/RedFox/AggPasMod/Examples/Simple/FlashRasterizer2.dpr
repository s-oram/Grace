program FlashRasterizer2;

// AggPas 2.4 RM3 Demo application
// Note: Press FY1 key on run to see more info about this demo

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
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerOutlineAA in '..\..\Source\AggRasterizerOutlineAA.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggMath in '..\..\Source\AggMath.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas';

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

    FMinStyle, FMaxStyle: Integer;

    FApiFile: TApiFile;
  protected
    function GetPathID(Index: Cardinal): Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    function Open(Name: AnsiString): Boolean;
    function ReadNext: Boolean;

    function Paths: Cardinal;
    function Style(I: Cardinal): PPathStyle;

    function MinStyle: Integer;
    function MaxStyle: Integer;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    function GetScale: Double; overload;
    procedure SetScale(W, H: Double); overload;

    procedure ApproximationScale(S: Double);

    function HitTest(X, Y, R: Double): Integer;

    procedure ModifyVertex(I: Cardinal; X, Y: Double);
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FShape: TCompoundShape;
    FColors: array [0..99] of TAggColor;
    FScale: TAggTransAffine;
    FGamma: TAggGammaLut8;
    FPointIndex: Integer;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    function Open(Name: AnsiString): Boolean;
    procedure ReadNext;

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

  FMinStyle := $7FFFFFFF;
  FMaxStyle := -$7FFFFFFF;
end;

destructor TCompoundShape.Destroy;
begin
  FPath.Free;
  FCurve.Free;
  FStyles.Free;
  FAffine.Free;
  FTrans.Free;

  if FApiFile.IsOpened then
    ApiCloseFile(FApiFile);

  inherited;
end;

function TCompoundShape.Open(Name: AnsiString): Boolean;
begin
  Result := ApiOpenFile(FApiFile, Name);
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

  PathStyle: TPathStyle;

  Code: Integer;

begin
  FPath.RemoveAll;
  FStyles.RemoveAll;

  FMinStyle := $7FFFFFFF;
  FMaxStyle := -$7FFFFFFF;

  if FApiFile.IsOpened then
  begin
    repeat
      if Fgets(@Buf[0], 1022, FApiFile) = nil then
      begin
        Result := False;

        Exit;
      end;

      if Buf[0] = '=' then
        Break;

    until False;

    while Fgets(@Buf[0], 1022, FApiFile) <> nil do
    begin
      if Buf[0] = '!' then
        Break;

      if Buf[0] = 'P' then
      begin
        // BeginPath
        PathStyle.PathID := FPath.StartNewPath;

        Ts := StrtOk(@Buf[0]); // Path;
        Ts := StrtOk(nil); // LeftStyle

        Val(PAnsiChar(Ts), PathStyle.LeftFill, Code);

        Ts := StrtOk(nil); // RightStyle

        Val(PAnsiChar(Ts), PathStyle.RightFill, Code);

        Ts := StrtOk(nil); // LineStyle

        Val(PAnsiChar(Ts), PathStyle.Line, Code);

        Ts := StrtOk(nil); // ax

        Val(PAnsiChar(Ts), Ax, Code);

        Ts := StrtOk(nil); // ay

        Val(PAnsiChar(Ts), Ay, Code);

        FPath.MoveTo(Ax, Ay);
        FStyles.Add(@PathStyle);

        if PathStyle.LeftFill >= 0 then
        begin
          if PathStyle.LeftFill < FMinStyle then
            FMinStyle := PathStyle.LeftFill;

          if PathStyle.LeftFill > FMaxStyle then
            FMaxStyle := PathStyle.LeftFill;
        end;

        if PathStyle.RightFill >= 0 then
        begin
          if PathStyle.RightFill < FMinStyle then
            FMinStyle := PathStyle.RightFill;

          if PathStyle.RightFill > FMaxStyle then
            FMaxStyle := PathStyle.RightFill;
        end;
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

function TCompoundShape.GetPathID(Index: Cardinal): Cardinal;
begin
  Result := PPathStyle(FStyles[Index])^.PathID;
end;

function TCompoundShape.Paths: Cardinal;
begin
  Result := FStyles.Size;
end;

function TCompoundShape.Style(I: Cardinal): PPathStyle;
begin
  Result := PPathStyle(FStyles[I]);
end;

function TCompoundShape.MinStyle: Integer;
begin
  Result := FMinStyle;
end;

function TCompoundShape.MaxStyle: Integer;
begin
  Result := FMaxStyle;
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

procedure TCompoundShape.ApproximationScale(S: Double);
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

  FGamma.Gamma := 2.0;

  FPointIndex := -1;

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
  FScale.Free;

  inherited;
end;

function TAggApplication.Open(Name: AnsiString): Boolean;
begin
  Result := FShape.Open(FullFileName(Name));
end;

procedure TAggApplication.ReadNext;
begin
  FShape.ReadNext;
  FShape.SetScale(Width, Height);
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;

  RenScan: TAggRendererScanLineAASolid;
  Ras: TAggRasterizerScanLineAA;
  Sl : TAggScanLineUnpacked8;

  Shape : TAggConvTransform;
  Stroke: TAggConvStroke;

  I: Cardinal;
  S: Integer;

  TmpPath: TAggPathStorage;

  Style: PPathStyle;

  Tfill, Tstroke: Double;

  Txt : TAggGsvText;
  TxtStroke: TAggConvStroke;
begin
  // Initialize structures
  PixelFormatBgra32Pre(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Rgba.FromRgbaDouble(1.0, 1.0, 0.95);
    RendererBase.Clear(@Rgba);

    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      Ras := TAggRasterizerScanLineAA.Create;
      Sl := TAggScanLineUnpacked8.Create;

      Shape := TAggConvTransform.Create(FShape, FScale);
      Stroke := TAggConvStroke.Create(Shape);

      FShape.ApproximationScale(FScale.GetScale);

      TmpPath := TAggPathStorage.Create;

      { ras.ClipBox(0 ,0 ,_width ,_height ); {! }

      // This is an alternative method of Flash rasterization.
      // We decompose the Compound shape into separate paths
      // and select the ones that fit the given style (left or right).
      // So that, we form a sub-shape and draw it as a whole.
      //
      // Here the regular ScanLine Rasterizer is used, but it doesn't
      // automatically close the polygons. So that, the Rasterizer
      // actually works with a set of polylines instead of polygons.
      // Of course, the data integrity must be preserved, that is,
      // the polylines must eventually form a closed contour
      // (or a set of closed contours). So that, first we set
      // AutoClose(false);
      //
      // The second important thing is that one path can be rasterized
      // twice, if it has both, left and right fill. Sometimes the
      // path has equal left and right fill, so that, the same path
      // will be added twice even for a single sub-shape. If the
      // Rasterizer can tolerate these degenerates you can add them,
      // but it's also fine just to omit them.
      //
      // The third thing is that for one side (left or right)
      // you should invert the direction of the paths.
      //
      // The main disadvantage of this method is imperfect stitching
      // of the adjacent polygons. The problem can be solved if we use
      // compositing operation "plus" instead of alpha-blend. But
      // in this case we are forced to use an RGBA buffer, clean it with
      // zero, rasterize using "plus" operation, and then alpha-blend
      // the result over the final scene. It can be too expensive.
      // ------------------------------------------------------------
      Ras.AutoClose(False);
      Ras.FillingRule := frEvenOdd;

      StartTimer;

      S := FShape.MinStyle;

      while S <= FShape.MaxStyle do
      begin
        Ras.Reset;

        I := 0;

        while I < FShape.Paths do
        begin
          Style := FShape.Style(I);

          if Style.LeftFill <> Style.RightFill then
          begin
            if Style.LeftFill = S then
              Ras.AddPath(Shape, Style.PathID);

            if Style.RightFill = S then
            begin
              TmpPath.RemoveAll;
              TmpPath.ConcatPath(Shape, Style.PathID);
              TmpPath.InvertPolygon(0);

              Ras.AddPath(TmpPath);
            end;
          end;

          Inc(I);
        end;

        RenScan.SetColor(@FColors[S]);
        RenderScanLines(Ras, Sl, RenScan);

        Inc(S);
      end;

      Tfill := GetElapsedTime;

      Ras.AutoClose(True);
      Ras.FillingRule := frNonZero;

      // Draw strokes
      StartTimer;

      Stroke.Width := Sqrt(FScale.GetScale);
      Stroke.LineJoin := ljRound;
      Stroke.LineCap := lcRound;

      I := 0;

      while I < FShape.Paths do
      begin
        Ras.Reset;

        if PPathStyle(FShape.Style(I)).Line >= 0 then
        begin
          Ras.AddPath(Stroke, PPathStyle(FShape.Style(I)).PathID);

          Rgba.FromRgbaInteger(0, 0, 0, 128);
          RenScan.SetColor(@Rgba);

          RenderScanLines(Ras, Sl, RenScan);
        end;

        Inc(I);
      end;

      Tstroke := GetElapsedTime;

      // Render Text
      Txt := TAggGsvText.Create;
      Txt.SetSize(8.0);
      Txt.Flip := True;

      TxtStroke := TAggConvStroke.Create(Txt);
      TxtStroke.Width := 1.6;
      TxtStroke.LineCap := lcRound;

      Txt.SetStartPoint(10.0, 20.0);
      Txt.SetText(Format('Fill=%.2fms (%dFPS) Stroke=%.2fms (%dFPS) '
        + 'Total=%.2fms (%dFPS) Space: Next Shape'#13#13 + '+/- : ZoomIn/'
        + 'ZoomOut (with respect to the mouse pointer)', [Tfill,
        Trunc(1000.0 / Tfill), Tstroke, Trunc(1000.0 / Tstroke),
        Tfill + Tstroke, Trunc(1000.0 / (Tfill + Tstroke))]));

      Ras.AddPath(TxtStroke);
      RenScan.SetColor(CRgba8Black);

      RenderScanLines(Ras, Sl, RenScan);

      // Gamma adjust
      if FGamma.Gamma <> 1.0 then
        Pixf.ApplyGammaInv(FGamma, CAggOrderBgra);

      // Free AGG resources
      Ras.Free;
      Sl.Free;

      Shape.Free;
      Stroke.Free;

      TmpPath.Free;

      Txt.Free;
      TxtStroke.Free;
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
  if not (mkfMouseLeft in Flags) then
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
    R := 4.0 / FScale.GetScale;

    FScale.InverseTransform(FScale, @Xd, @Yd);

    FPointIndex := FShape.HitTest(Xd, Yd, R);

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  FPointIndex := -1;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Another possible way to render Flash Compound shapes. The '
      + 'idea behind it is prety simple. You just use the regular Rasterizer, '
      + 'but in a mode when it doesn''t automatically close the contours.'
      + ' Every Compound shape is decomposed into a number of single shapes '
      + 'that are rasterized and rendered separately.'#13#13
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
    FScale.Rotate(-Pi / 20.0);
    FScale.Translate(X, Y);

    ForceRedraw;
  end;

  if Key = Cardinal(kcRight) then
  begin
    FScale.Translate(-X, -Y);
    FScale.Rotate(Pi / 20.0);
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
{$ENDIF}

  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'AGG Example - Flash Rasterizer with separate rendering (F1-Help)';

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
