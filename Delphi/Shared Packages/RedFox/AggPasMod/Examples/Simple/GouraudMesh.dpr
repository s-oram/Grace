program GouraudMesh;

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
  AggArray in '..\..\Source\AggArray.pas',
  AggMath in '..\..\Source\AggMath.pas',
  AggMathStroke in '..\..\Source\AggMathStroke.pas',

  AggControl in '..\..\Source\AggControl.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLineBin in '..\..\Source\AggScanLineBin.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanGouraudRgba in '..\..\Source\AggSpanGouraudRgba.pas',
  AggGammaLUT in '..\..\Source\AggGammaLUT.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',
  AggRasterizerCompoundAA in '..\..\Source\AggRasterizerCompoundAA.pas',
  AggGsvText in '..\..\Source\AggGsvText.pas';

const
  CFlipY = True;

type
  PMeshPoint = ^TMeshPoint;
  TMeshPoint = record
    X, Y, Dx, Dy: Double;
    Color, DeltaColor: TAggColor;
  end;

  PMeshTriangle = ^TMeshTriangle;
  TMeshTriangle = record
    Pnt: array [0..2] of Cardinal;
  end;

  TEdgePoints = array [0..1] of Cardinal;

  PMeshEdge = ^TMeshEdge;
  TMeshEdge = record
    Pnt: TEdgePoints;
    Left, Right: Integer;
  end;

  TMeshControl = class
  private
    FCols, FRows: Cardinal;
    FDragIndex: Integer;
    FCellWidth, FCellHeight: Double;
    FDragDelta, FStart: TPointDouble;
    FVertices, FTriangles, FEdges: TAggPodBVector;
    function GetNumVertices: Cardinal;
    function GetNumEdges: Cardinal;
    function GetNumTriangles: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Generate(Cols, Rows: Cardinal; CellW, CellH, StartX,
      StartY: Double);

    procedure RandomizePoints(Delta: Double);
    procedure RotateColors;

    function OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags): Boolean;
    function OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags): Boolean;
    function OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags): Boolean;

    function Vertex(I: Cardinal): PMeshPoint; overload;
    function Vertex(X, Y: Cardinal): PMeshPoint; overload;
    function Triangle(I: Cardinal): PMeshTriangle;
    function Edge(I: Cardinal): PMeshEdge;

    property NumVertices: Cardinal read GetNumVertices;
    property NumEdges: Cardinal read GetNumEdges;
    property NumTriangles: Cardinal read GetNumTriangles;
  end;

  TStylesGouraud = class(TAggCustomStyleHandler)
  private
    FTriangles: TAggPodBVector;

    FRgba: TAggColor;
  public
    constructor Create(Mesh: TMeshControl; Gamma: TAggGamma);
    destructor Destroy; override;

    function IsSolid(Style: Cardinal): Boolean; override;
    function GetColor(Style: Cardinal): PAggColor; override;

    procedure GenerateSpan(Span: PAggColor; X, Y: Integer;
      Len, Style: Cardinal); override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FMesh: TMeshControl;
    FGamma: TAggGammaLut8;
    FStyles: TStylesGouraud;
    FPixelFormat: TAggPixelFormatProcessor;
    FRendererBase: TAggRendererBase;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;
    procedure OnResize(Width: Integer; Height: Integer); override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
    procedure OnIdle; override;
    procedure OnControlChange; override;
  end;


{ TMeshControl }

constructor TMeshControl.Create;
begin
  FVertices := TAggPodBVector.Create(SizeOf(TMeshPoint));
  FTriangles := TAggPodBVector.Create(SizeOf(TMeshTriangle));
  FEdges := TAggPodBVector.Create(SizeOf(TMeshEdge));

  FCols := 0;
  FRows := 0;

  FDragIndex := -1;
  FDragDelta.X := 0;
  FDragDelta.y := 0;
end;

destructor TMeshControl.Destroy;
begin
  FVertices.Free;
  FTriangles.Free;
  FEdges.Free;
  inherited;
end;

procedure TMeshControl.Generate(Cols, Rows: Cardinal;
  CellW, CellH, StartX, StartY: Double);
var
  I, J: Cardinal;
  X, Dx, Dy: Double;
  Pnt : array [0..3] of Integer;
  CurrentCell, LeftCell, BottomCell: Integer;
  CurrentT, LeftT, BottomT: array [1..2] of Integer;
  C, Dc: TAggColor;
  C1, C2, C3: Int8u;
  Mp: TMeshPoint;
  Mt: TMeshTriangle;
  Me: TMeshEdge;
begin
  FCols := Cols;
  FRows := Rows;
  FCellWidth := CellW;
  FCellHeight := CellH;

  FStart.X := StartX;
  FStart.Y := StartY;

  FVertices.RemoveAll;

  I := 0;

  while I < FRows do
  begin
    X := StartX;
    J := 0;

    while J < FCols do
    begin
      Dx := RandomMinMax(-0.5, 0.5);
      Dy := RandomMinMax(-0.5, 0.5);

      C1 := Random($100);
      C2 := Random($100);
      C3 := Random($100);

      C.FromRgbaInteger(C3, C2, C1);

      C1 := Random(2);
      C2 := Random(2);
      C3 := Random(2);

      Dc.FromRgbaInteger(C3, C2, C1);

      Mp.X := X;
      Mp.Y := StartY;
      Mp.Dx := Dx;
      Mp.Dy := Dy;
      Mp.Color := C;
      Mp.DeltaColor := Dc;
      FVertices.Add(@Mp);

      X := X + CellW;

      Inc(J);
    end;

    StartY := StartY + CellH;

    Inc(I);
  end;

  // 4---3
  // |t2/|
  // | / |
  // |/t1|
  // 1---2
  FTriangles.RemoveAll;
  FEdges.RemoveAll;

  I := 0;

  while I < FRows - 1 do
  begin
    J := 0;

    while J < FCols - 1 do
    begin
      Pnt[0] := I * FCols + J;
      Pnt[1] := Pnt[0] + 1;
      Pnt[2] := Pnt[1] + FCols;
      Pnt[3] := Pnt[0] + FCols;

      Mt.Pnt[0] := Pnt[0];
      Mt.Pnt[1] := Pnt[1];
      Mt.Pnt[2] := Pnt[2];
      FTriangles.Add(@Mt);

      Mt.Pnt[0] := Pnt[2];
      Mt.Pnt[1] := Pnt[3];
      Mt.Pnt[2] := Pnt[0];
      FTriangles.Add(@Mt);

      CurrentCell := I * (FCols - 1) + J;

      if J <> 0 then
        LeftCell := Integer(CurrentCell - 1)
      else
        LeftCell := -1;

      if I <> 0 then
        BottomCell := Integer(CurrentCell - (FCols - 1))
      else
        BottomCell := -1;

      CurrentT[1] := CurrentCell * 2;
      CurrentT[2] := CurrentT[1] + 1;

      if LeftCell >= 0 then
        LeftT[1] := LeftCell * 2
      else
        LeftT[1] := -1;

      if LeftCell >= 0 then
        LeftT[2] := LeftT[1] + 1
      else
        LeftT[2] := -1;

      if BottomCell >= 0 then
        BottomT[1] := BottomCell * 2
      else
        BottomT[1] := -1;

      if BottomCell >= 0 then
        BottomT[2] := BottomT[1] + 1
      else
        BottomT[2] := -1;

      Me.Pnt[0] := Pnt[0];
      Me.Pnt[1] := Pnt[1];
      Me.Left := CurrentT[1];
      Me.Right := BottomT[2];
      FEdges.Add(@Me);

      Me.Pnt[0] := Pnt[0];
      Me.Pnt[1] := Pnt[2];
      Me.Left := CurrentT[2];
      Me.Right := CurrentT[1];
      FEdges.Add(@Me);

      Me.Pnt[0] := Pnt[0];
      Me.Pnt[1] := Pnt[3];
      Me.Left := LeftT[1];
      Me.Right := CurrentT[2];
      FEdges.Add(@Me);

      if J = FCols - 2 then // Last column
      begin
        Me.Pnt[0] := Pnt[1];
        Me.Pnt[1] := Pnt[2];
        Me.Left := CurrentT[1];
        Me.Right := -1;
        FEdges.Add(@Me);
      end;

      if I = FRows - 2 then // Last row
      begin
        Me.Pnt[0] := Pnt[2];
        Me.Pnt[1] := Pnt[3];
        Me.Left := CurrentT[2];
        Me.Right := -1;
        FEdges.Add(@Me);
      end;

      Inc(J);
    end;

    Inc(I);
  end;
end;

procedure TMeshControl.RandomizePoints(Delta: Double);
var
  I, J: Cardinal;
  Xc, Yc, X1, Y1, X2, Y2: Double;
  P: PMeshPoint;
begin
  I := 0;

  while I < FRows do
  begin
    J := 0;

    while J < FCols do
    begin
      Xc := J * FCellWidth + FStart.X;
      Yc := I * FCellHeight + FStart.Y;
      X1 := Xc - FCellWidth* 0.25;
      Y1 := Yc - FCellHeight* 0.25;
      X2 := Xc + FCellWidth* 0.25;
      Y2 := Yc + FCellHeight* 0.25;

      P := Vertex(J, I);

      P.X := P.X + P.Dx;
      P.Y := P.Y + P.Dy;

      if P.X < X1 then
      begin
        P.X := X1;
        P.Dx := -P.Dx;
      end;

      if P.Y < Y1 then
      begin
        P.Y := Y1;
        P.Dy := -P.Dy;
      end;

      if P.X > X2 then
      begin
        P.X := X2;
        P.Dx := -P.Dx;
      end;

      if P.Y > Y2 then
      begin
        P.Y := Y2;
        P.Dy := -P.Dy;
      end;

      Inc(J);
    end;

    Inc(I);
  end;
end;

procedure TMeshControl.RotateColors;
var
  I: Cardinal;
  C, Dc: PAggColor;
  R, G, B: Integer;
begin
  I := 1;

  while I < FVertices.Size do
  begin
    C := @PMeshPoint(FVertices[I]).Color;
    Dc := @PMeshPoint(FVertices[I]).DeltaColor;

    if Dc.Rgba8.R <> 0 then
      R := C.Rgba8.R + 5
    else
      R := C.Rgba8.R - 5;

    if Dc.Rgba8.G <> 0 then
      G := C.Rgba8.G + 5
    else
      G := C.Rgba8.G - 5;

    if Dc.Rgba8.B <> 0 then
      B := C.Rgba8.B + 5
    else
      B := C.Rgba8.B - 5;

    if R < 0 then
    begin
      R := 0;
      Dc.Rgba8.R := Dc.Rgba8.R xor 1;
    end;

    if R > 255 then
    begin
      R := 255;
      Dc.Rgba8.R := Dc.Rgba8.R xor 1;
    end;

    if G < 0 then
    begin
      G := 0;
      Dc.Rgba8.G := Dc.Rgba8.G xor 1;
    end;

    if G > 255 then
    begin
      G := 255;
      Dc.Rgba8.G := Dc.Rgba8.G xor 1;
    end;

    if B < 0 then
    begin
      B := 0;
      Dc.Rgba8.B := Dc.Rgba8.B xor 1;
    end;

    if B > 255 then
    begin
      B := 255;
      Dc.Rgba8.B := Dc.Rgba8.B xor 1;
    end;

    C.Rgba8.R := R;
    C.Rgba8.G := G;
    C.Rgba8.B := B;

    Inc(I);
  end;
end;

function TMeshControl.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags): Boolean;
var
  I: Cardinal;
begin
  Result := False;

  if mkfMouseLeft in Flags then
  begin
    I := 0;

    while I < FVertices.Size do
    begin
      if CalculateDistance(X, Y, PMeshPoint(FVertices[I]).X,
        PMeshPoint(FVertices[I]).Y) < 5 then
      begin
        FDragIndex := I;

        FDragDelta.X := X - PMeshPoint(FVertices[I]).X;
        FDragDelta.y := Y - PMeshPoint(FVertices[I]).Y;

        Result := True;
      end;

      Inc(I);
    end;
  end;
end;

function TMeshControl.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags): Boolean;
begin
  Result := False;

  if mkfMouseLeft in Flags then
  begin
    if FDragIndex >= 0 then
    begin
      PMeshPoint(FVertices[FDragIndex]).X := X - FDragDelta.X;
      PMeshPoint(FVertices[FDragIndex]).Y := Y - FDragDelta.y;
      Result := True;
    end;
  end
  else
    Result := OnMouseButtonUp(X, Y, Flags);
end;

function TMeshControl.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags): Boolean;
begin
  Result := FDragIndex >= 0;
  FDragIndex := -1;
end;

function TMeshControl.GetNumVertices: Cardinal;
begin
  Result := FVertices.Size;
end;

function TMeshControl.Vertex(I: Cardinal): PMeshPoint;
begin
  Result := FVertices[I];
end;

function TMeshControl.Vertex(X, Y: Cardinal): PMeshPoint;
begin
  Result := FVertices[Y * FRows + X];
end;

function TMeshControl.GetNumTriangles: Cardinal;
begin
  Result := FTriangles.Size;
end;

function TMeshControl.Triangle(I: Cardinal): PMeshTriangle;
begin
  Result := FTriangles[I];
end;

function TMeshControl.GetNumEdges: Cardinal;
begin
  Result := FEdges.Size;
end;

function TMeshControl.Edge(I: Cardinal): PMeshEdge;
begin
  Result := FEdges[I];
end;


{ TStylesGouraud }

constructor TStylesGouraud.Create(Mesh: TMeshControl; Gamma: TAggGamma);
var
  I: Cardinal;
  T: PMeshTriangle;

  Pnt: array [0..2] of PMeshPoint;
  C1, C2, C3: TAggColor;

  Gouraud: TAggSpanGouraudRgba;
begin
  FTriangles := TAggPodBVector.Create(SizeOf(TAggSpanGouraudRgba));
  FRgba.FromRgbaInteger(0, 0, 0, 0);

  for I := 0 to Mesh.NumTriangles - 1 do
  begin
    T := Mesh.Triangle(I);
    Pnt[0] := Mesh.Vertex(T.Pnt[0]);
    Pnt[1] := Mesh.Vertex(T.Pnt[1]);
    Pnt[2] := Mesh.Vertex(T.Pnt[2]);

    C1 := Pnt[0].Color;
    C2 := Pnt[1].Color;
    C3 := Pnt[2].Color;

    C1.ApplyGammaDir(Gamma);
    C2.ApplyGammaDir(Gamma);
    C2.ApplyGammaDir(Gamma);

    Gouraud := TAggSpanGouraudRgba.Create(@C1, @C2, @C3, Pnt[0].X, Pnt[0].Y, Pnt[1].X, Pnt[1].Y,
      Pnt[2].X, Pnt[2].Y);

    Gouraud.Prepare;
    FTriangles.Add(@Gouraud);
  end;
end;

destructor TStylesGouraud.Destroy;
var
  Index: Cardinal;
begin
  for Index := 0 to FTriangles.Size - 1 do
    TAggSpanGouraudRgba(FTriangles[Index]^).Free;

  FTriangles.Free;
  inherited;
end;

function TStylesGouraud.IsSolid(Style: Cardinal): Boolean;
begin
  Result := False;
end;

function TStylesGouraud.GetColor(Style: Cardinal): PAggColor;
begin
  Result := @FRgba;
end;

procedure TStylesGouraud.GenerateSpan(Span: PAggColor; X, Y: Integer;
  Len, Style: Cardinal);
begin
  TAggSpanGouraudRgba(FTriangles[Style]^).Generate(Span, X, Y, Len);
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  // Initialize structures
  PixelFormatBgra32Pre(FPixelFormat, RenderingBufferWindow);
  FRendererBase := TAggRendererBase.Create(FPixelFormat, True);

  FMesh := TMeshControl.Create;
  FGamma := TAggGammaLut8.Create;

  FMesh.Generate(20, 20, 17, 17, 40, 40);
  FStyles := TStylesGouraud.Create(FMesh, FGamma);
end;

destructor TAggApplication.Destroy;
begin
  FStyles.Free;

  FGamma.Free;
  FMesh.Free;

  FRendererBase.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  Rgba: TAggColor;

  Ras: TAggRasterizerScanLineAA;

  ScanLn: TAggScanLineUnpacked8;
  ScanLnBin: TAggScanLineBin;

  Rasc : TAggRasterizerCompoundAAInteger;
  SpanAllocator: TAggSpanAllocator;

  I: Cardinal;
  E: PMeshEdge;

  Pnt: array [0..1] of PMeshPoint;

  Tm : Double;

  Txt: TAggGsvText;
  Pt: TAggConvStrokeMath;
begin
  FRendererBase.Clear(CRgba8Black);

  Ras := TAggRasterizerScanLineAA.Create;
  try
    ScanLn := TAggScanLineUnpacked8.Create;
    ScanLnBin := TAggScanLineBin.Create;

    Rasc := TAggRasterizerCompoundAAInteger.Create;
    SpanAllocator := TAggSpanAllocator.Create;

    StartTimer;
    Rasc.Reset;

    // Rasc.ClipBox(40, 40, Width - 40, Height - 40);

    I := 0;

    while I < FMesh.NumEdges do
    begin
      E := FMesh.Edge(I);
      Pnt[0] := FMesh.Vertex(E.Pnt[0]);
      Pnt[1] := FMesh.Vertex(E.Pnt[1]);

      Rasc.Styles(E.Left, E.Right);
      Rasc.MoveToDouble(Pnt[0].X, Pnt[0].Y);
      Rasc.LineToDouble(Pnt[1].X, Pnt[1].Y);

      Inc(I);
    end;

    RenderScanLinesCompound(Rasc, ScanLn, ScanLnBin, FRendererBase,
      SpanAllocator, FStyles);

    // Info
    Tm := GetElapsedTime;

    Txt := TAggGsvText.Create;
    Txt.SetSize(10.0);

    Pt := TAggConvStrokeMath.Create(Txt);
    Pt.Width := 1.5;
    Pt.LineCap := lcRound;
    Pt.LineJoin := ljRound;

    Txt.SetStartPoint(10.0, 10.0);
    Txt.SetText(Format('%3.2f ms, %d triangles, %.0f tri/sec', [Tm,
      FMesh.NumTriangles, FMesh.NumTriangles / Tm * 1000.0]));

    Ras.AddPath(Pt);
    Rgba.White;

    RenderScanLinesAASolid(Ras, ScanLn, FRendererBase, @Rgba);

    if FGamma.Gamma <> 1.0 then
      FPixelFormat.ApplyGammaInv(FGamma, CAggOrderBgra);

    // Free AGG resources
    ScanLn.Free;
    ScanLnBin.Free;

    Rasc.Free;
    SpanAllocator.Free;

    Txt.Free;
    Pt.Free;
  finally
    Ras.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FMesh.OnMouseMove(X, Y, Flags) then
    ForceRedraw;
end;

procedure TAggApplication.OnResize(Width, Height: Integer);
begin
  inherited;
  FRendererBase.SetClipBox(0, 0, Width, Height)
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FMesh.OnMouseButtonDown(X, Y, Flags) then
    ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if FMesh.OnMouseButtonUp(X, Y, Flags) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Yet another example that demonstrates the power of '
      + 'compound shape rasterization. Here we create a mesh of triangles '
      + 'and render them in one pass with multiple Gouraud shaders '
      + '(TAggSpanGouraudRgba). The example demonstrates perfect '
      + 'Anti-Aliasing and perfect triangle stitching (seamless edges) '
      + 'at the same time.'#13#13
      + 'How to play with:'#13#13
      + 'You can modify the points of the mesh by left mouse click and drag.');
end;

procedure TAggApplication.OnIdle;
begin
  FMesh.RandomizePoints(1.0);
  FMesh.RotateColors;
  ForceRedraw;
end;

procedure TAggApplication.OnControlChange;
begin
end;

begin
  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'AGG Example. (F1-Help)';

    if Init(400, 400, []) then
    begin
      WaitMode := False;
      Run;
    end;
  finally
    Free;
  end;
end.
