program GraphTest;

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

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererPrimitives in '..\..\Source\AggRendererPrimitives.pas',
  AggRasterizerScanLine in '..\..\Source\AggRasterizerScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerOutline in '..\..\Source\AggRasterizerOutline.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggArray in '..\..\Source\AggArray.pas',
  AggCurves in '..\..\Source\AggCurves.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvDash in '..\..\Source\AggConvDash.pas',
  AggConvCurve in '..\..\Source\AggConvCurve.pas',
  AggConvContour in '..\..\Source\AggConvContour.pas',
  AggConvMarker in '..\..\Source\AggConvMarker.pas',
  AggConvShortenPath in '..\..\Source\AggConvShortenPath.pas',
  AggConvMarkerAdaptor in '..\..\Source\AggConvMarkerAdaptor.pas',
  AggConvConcat in '..\..\Source\AggConvConcat.pas',
  AggArrowHead in '..\..\Source\AggArrowHead.pas',
  AggVcgenMarkersTerm in '..\..\Source\AggVcgenMarkersTerm.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanGradient in '..\..\Source\AggSpanGradient.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas',
  AggGammaFunctions in '..\..\Source\AggGammaFunctions.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas';

{$I- }

const
  CFlipY = True;

type
  TSolidRenderer = TAggRendererScanLineAASolid;
  TDraftRenderer = TAggRendererScanLineBinSolid;
  TAggGradientFunction = TAggGradientRadialDouble;
  TAggGradientRenderer = TAggRendererScanLineAA;

  PEdge = ^TEdge;
  TEdge = record
    Node: array [0..1] of Integer;
  end;

  TGraph = class
  private
    FNumNodes: Integer;
    FNumEdges: Integer;
    FNodes: PPointDouble;
    FEdges: PEdge;
  public
    constructor Create(NumNodes, NumEdges: Integer);
    destructor Destroy; override;

    function GetNode(Idx: Integer; W, H: Double): TPointDouble;
    function GetEdge(Idx: Integer): TEdge;

    property NumNodes: Integer read FNumNodes;
    property NumEdges: Integer read FNumEdges;
  end;

  TLine = class(TAggVertexSource)
  private
    FCoord: TRectDouble;
    FPointIndex: Integer;
  public
    constructor Create(X1, Y1, X2, Y2: Double); overload;
    constructor Create(Rect: TRectDouble); overload;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TCurve = class(TAggVertexSource)
  private
    FCurve: TAggCurve4;
  public
    constructor Create(X1, Y1, X2, Y2: Double; K: Double = 0.5);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TStrokeDraftSimple = class(TAggVertexSource)
  private
    FSource: TAggVertexSource;
  public
    constructor Create(Src: TAggVertexSource);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TStrokeDraftArrow = class(TAggVertexSource)
  private
    FSource: TAggConvMarkerAdaptor;
    FArrowHead: TAggArrowHead;
    FMarker: TAggConvMarker;
    FMarkerTerm: TAggVcgenMarkersTerm;
    FConcat: TAggConvConcat;
  public
    constructor Create(Src: TAggVertexSource; W: Double);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TStrokeFineSimple = class(TAggVertexSource)
  private
    FSource: TAggConvStroke;
  public
    constructor Create(Src: TAggVertexSource; W: Double);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TStrokeFineArrow = class(TAggVertexSource)
  private
    FStroke: TAggConvStroke;
    FArrowHead: TAggArrowHead;
    FMarker: TAggConvMarker;
    FMarkerTerm: TAggVcgenMarkersTerm;
    FConcat: TAggConvConcat;
  public
    constructor Create(Src: TAggVertexSource; W: Double);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TDashStrokeDraftSimple = class(TAggVertexSource)
  private
    FDash: TAggConvDash;
  public
    constructor Create(Src: TAggVertexSource; DashLength, GapLength, W: Double);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TDashStrokeDraftArrow = class(TAggVertexSource)
  private
    FDash: TAggConvDash;
    FArrowHead: TAggArrowHead;
    FMarker: TAggConvMarker;
    FMarkerTerm: TAggVcgenMarkersTerm;
    FConcat: TAggConvConcat;
  public
    constructor Create(Src: TAggVertexSource; DashLength, GapLength, W: Double);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TDashStrokeFineSimple = class(TAggVertexSource)
  private
    FDash: TAggConvDash;
    FStroke: TAggConvStroke;
  public
    constructor Create(Src: TAggVertexSource; DashLength, GapLength, W: Double);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TDashStrokeFineArrow = class(TAggVertexSource)
  private
    FDash: TAggConvDash;
    FStroke: TAggConvStroke;
    FArrowHead: TAggArrowHead;
    FMarker: TAggConvMarker;
    FMarkerTerm: TAggVcgenMarkersTerm;
    FConcat: TAggConvConcat;
  public
    constructor Create(Src: TAggVertexSource; DashLength, GapLength, W: Double);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  {
  TStrokeDraft = TStrokeDraftSimple;
  TDashStrokeDraft = TDashStrokeDraftSimple;
  TStrokeFine = TStrokeFineSimple;
  TDashStrokeFine = TDashStrokeFineSimple;
  }

  TStrokeDraft = TStrokeDraftArrow;
  TDashStrokeDraft = TDashStrokeDraftArrow;
  TStrokeFine = TStrokeFineArrow;
  TDashStrokeFine = TDashStrokeFineArrow;

  TAggApplication = class(TPlatformSupport)
  private
    FRadioBoxType: TAggControlRadioBox;
    FSliderWidth: TAggControlSlider;

    FCheckBoxBenchmark: TAggControlCheckBox;
    FCheckBoxDrawNodes: TAggControlCheckBox;
    FCheckBoxDrawEdges: TAggControlCheckBox;
    FCheckBoxDraft: TAggControlCheckBox;
    FCheckBoxTranslucent: TAggControlCheckBox;

    FGradientColors: TAggPodAutoArray;

    FGraph: TGraph;
    FDraw: Integer;
    FScanLine: TAggScanLineUnpacked8;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure DrawNodesDraft;
    procedure DrawNodesFine(Ras: TAggRasterizerScanLine);

    procedure RenderEdgeFine(Ras: TAggRasterizerScanLine;
      RenFine, RenDraft: TAggCustomRendererScanLineSolid; Src: TAggVertexSource);

    procedure DrawLinesDraft;
    procedure DrawCurvesDraft;
    procedure DrawDashesDraft;

    procedure DrawLinesFine(Ras: TAggRasterizerScanLine;
      Solid, Draft: TAggCustomRendererScanLineSolid);

    procedure DrawCurvesFine(Ras: TAggRasterizerScanLine;
      Solid, Draft: TAggCustomRendererScanLineSolid);

    procedure DrawDashesFine(Ras: TAggRasterizerScanLine;
      Solid, Draft: TAggCustomRendererScanLineSolid);

    procedure DrawPolygons(Ras: TAggRasterizerScanLine;
      Solid, Draft: TAggCustomRendererScanLineSolid);

    procedure DrawScene(Ras: TAggRasterizerScanLine;
      Solid, Draft: TAggCustomRendererScanLineSolid);

    procedure OnDraw; override;
    procedure OnControlChange; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TGraph }

constructor TGraph.Create(NumNodes, NumEdges: Integer);
var
  I: Integer;
  N: PPointDouble;
  E: PEdge;
begin
  FNumNodes := NumNodes;
  FNumEdges := NumEdges;

  AggGetMem(Pointer(FNodes), FNumNodes * SizeOf(TPointDouble));
  AggGetMem(Pointer(FEdges), FNumEdges * SizeOf(TEdge));

  RandSeed := 100;

  for I := 0 to FNumNodes - 1 do
  begin
    N := PPointDouble(PtrComp(FNodes) + I * SizeOf(TPointDouble));

    N.X := Random * 0.75 + 0.2;
    N.Y := Random * 0.85 + 0.1;
  end;

  I := 0;

  while I < FNumEdges do
  begin
    E := PEdge(PtrComp(FEdges) + I * SizeOf(TEdge));

    E.Node[0] := Random(FNumNodes);
    E.Node[1] := Random(FNumNodes);

    if E.Node[0] = E.Node[1] then
      Dec(I);

    Inc(I);
  end;
end;

destructor TGraph.Destroy;
begin
  AggFreeMem(Pointer(FNodes), FNumNodes * SizeOf(TPointDouble));
  AggFreeMem(Pointer(FEdges), FNumEdges * SizeOf(TEdge));
  inherited;
end;

function TGraph.GetNode(Idx: Integer; W, H: Double): TPointDouble;
var
  P: TPointDouble;
begin
  if Idx < FNumNodes then
  begin
    P := PPointDouble(PtrComp(FNodes) + Idx * SizeOf(TPointDouble))^;

    P.X := P.X * W;
    P.Y := P.Y * H;
  end;

  Result := P;
end;

function TGraph.GetEdge(Idx: Integer): TEdge;
var
  B: TEdge;
begin
  if Idx < FNumEdges then
    B := PEdge(PtrComp(FEdges) + Idx * SizeOf(TEdge))^;

  Result := B;
end;


{ TLine }

constructor TLine.Create(X1, Y1, X2, Y2: Double);
begin
  FCoord.X1 := X1;
  FCoord.Y1 := Y1;
  FCoord.X2 := X2;
  FCoord.Y2 := Y2;
  FPointIndex := 0;
end;

constructor TLine.Create(Rect: TRectDouble);
begin
  FCoord.X1 := Rect.X1;
  FCoord.Y1 := Rect.Y1;
  FCoord.X2 := Rect.X2;
  FCoord.Y2 := Rect.Y2;
  FPointIndex := 0;
end;

procedure TLine.Rewind(PathID: Cardinal);
begin
  FPointIndex := 0;
end;

function TLine.Vertex(X, Y: PDouble): Cardinal;
begin
  case FPointIndex of
    0:
      begin
        Inc(FPointIndex);

        X^ := FCoord.X1;
        Y^ := FCoord.Y1;

        Result := CAggPathCmdMoveTo;
      end;
    1:
      begin
        Inc(FPointIndex);

        X^ := FCoord.X2;
        Y^ := FCoord.Y2;

        Result := CAggPathCmdLineTo;
      end;
    else
      Result := CAggPathCmdStop;
  end;
end;


{ TCurve }

constructor TCurve.Create(X1, Y1, X2, Y2: Double; K: Double = 0.5);
begin
  FCurve := TAggCurve4.Create;
  FCurve.Init4(PointDouble(X1, Y1),
    PointDouble(X1 - (Y2 - Y1) * K, Y1 + (X2 - X1) * K),
    PointDouble(X2 + (Y2 - Y1) * K, Y2 - (X2 - X1) * K),
    PointDouble(X2, Y2));
end;

destructor TCurve.Destroy;
begin
  FCurve.Free;
  inherited;
end;

procedure TCurve.Rewind(PathID: Cardinal);
begin
  FCurve.Rewind(PathID);
end;

function TCurve.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FCurve.Vertex(X, Y);
end;


{ StrokeDraftSimple }

constructor TStrokeDraftSimple.Create(Src: TAggVertexSource);
begin
  FSource := Src;
end;

procedure TStrokeDraftSimple.Rewind(PathID: Cardinal);
begin
  FSource.Rewind(PathID);
end;

function TStrokeDraftSimple.Vertex(X, Y: PDouble): Cardinal;
begin
  FSource.Vertex(X, Y);
end;


{ TStrokeDraftArrow }

constructor TStrokeDraftArrow.Create;
begin
  FSource := TAggConvMarkerAdaptor.Create(Src);
  FMarkerTerm := TAggVcgenMarkersTerm.Create;
  FSource.Markers := FMarkerTerm;
  FArrowHead := TAggArrowHead.Create;
  FMarker := TAggConvMarker.Create(FSource.Markers, FArrowHead);
  FConcat := TAggConvConcat.Create(FSource, FMarker);

  FArrowHead.SetHead(0, 10, 5, 0);
  FSource.Shorten := 10.0;
end;

destructor TStrokeDraftArrow.Destroy;
begin
  FConcat.Free;
  FMarker.Free;
  FArrowHead.Free;
  FMarkerTerm.Free;
  FSource.Free;
  inherited;
end;

procedure TStrokeDraftArrow.Rewind(PathID: Cardinal);
begin
  FConcat.Rewind(PathID);
end;

function TStrokeDraftArrow.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FConcat.Vertex(X, Y);
end;


{ TStrokeFineSimple }

constructor TStrokeFineSimple.Create;
begin
  FSource := TAggConvStroke.Create(Src);
  FSource.Width := W;
end;

destructor TStrokeFineSimple.Destroy;
begin
  FSource.Free;
  inherited
end;

procedure TStrokeFineSimple.Rewind(PathID: Cardinal);
begin
  FSource.Rewind(PathID);
end;

function TStrokeFineSimple.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FSource.Vertex(X, Y);
end;


{ TStrokeFineArrow }

constructor TStrokeFineArrow.Create;
begin
  FStroke := TAggConvStroke.Create(Src);
  FMarkerTerm := TAggVcgenMarkersTerm.Create;
  FStroke.Markers := FMarkerTerm;
  FArrowHead := TAggArrowHead.Create;
  FMarker := TAggConvMarker.Create(FStroke.Markers, FArrowHead);
  FConcat := TAggConvConcat.Create(FStroke, FMarker);

  FStroke.Width := W;
  FArrowHead.SetHead(0, 10, 5, 0);
  FStroke.Shorten := W * 2.0;
end;

destructor TStrokeFineArrow.Destroy;
begin
  FConcat.Free;
  FMarker.Free;
  FArrowHead.Free;
  FMarkerTerm.Free;
  FStroke.Free;

  inherited;
end;

procedure TStrokeFineArrow.Rewind(PathID: Cardinal);
begin
  FConcat.Rewind(PathID);
end;

function TStrokeFineArrow.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FConcat.Vertex(X, Y);
end;


{ TDashStrokeDraftSimple }

constructor TDashStrokeDraftSimple.Create(Src: TAggVertexSource; DashLength,
  GapLength, W: Double);
begin
  FDash := TAggConvDash.Create(Src);
  FDash.AddDash(DashLength, GapLength);
end;

destructor TDashStrokeDraftSimple.Destroy;
begin
  FDash.Free;

  inherited;
end;

procedure TDashStrokeDraftSimple.Rewind(PathID: Cardinal);
begin
  FDash.Rewind(PathID);
end;


{ TDashStrokeDraftSimple }

function TDashStrokeDraftSimple.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FDash.Vertex(X, Y);
end;


{ TDashStrokeDraftArrow }

constructor TDashStrokeDraftArrow.Create;
begin
  FDash := TAggConvDash.Create(Src);
  FMarkerTerm := TAggVcgenMarkersTerm.Create;
  FDash.Markers := FMarkerTerm;
  FArrowHead := TAggArrowHead.Create;
  FMarker := TAggConvMarker.Create(FDash.Markers, FArrowHead);
  FConcat := TAggConvConcat.Create(FDash, FMarker);

  FDash.AddDash(DashLength, GapLength);

  FArrowHead.SetHead(0, 10, 5, 0);
  FDash.Shorten := 10.0;
end;

destructor TDashStrokeDraftArrow.Destroy;
begin
  FConcat.Free;
  FMarker.Free;
  FArrowHead.Free;
  FMarkerTerm.Free;
  FDash.Free;

  inherited;
end;

procedure TDashStrokeDraftArrow.Rewind(PathID: Cardinal);
begin
  FConcat.Rewind(PathID);
end;

function TDashStrokeDraftArrow.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FConcat.Vertex(X, Y);
end;


{ TDashStrokeFineSimple }

constructor TDashStrokeFineSimple.Create;
begin
  FDash := TAggConvDash.Create(Src);
  FStroke := TAggConvStroke.Create(FDash);

  FDash.AddDash(DashLength, GapLength);
  FStroke.Width := W;
end;

destructor TDashStrokeFineSimple.Destroy;
begin
  FDash.Free;
  FStroke.Free;

  inherited;
end;

procedure TDashStrokeFineSimple.Rewind(PathID: Cardinal);
begin
  FStroke.Rewind(PathID);
end;

function TDashStrokeFineSimple.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FStroke.Vertex(X, Y);
end;


{ TDashStrokeFineArrow }

constructor TDashStrokeFineArrow.Create;
begin
  FDash := TAggConvDash.Create(Src);
  FMarkerTerm := TAggVcgenMarkersTerm.Create;
  FDash.Markers := FMarkerTerm;
  FStroke := TAggConvStroke.Create(FDash);
  FArrowHead := TAggArrowHead.Create;
  FMarker := TAggConvMarker.Create(FDash.Markers, FArrowHead);
  FConcat := TAggConvConcat.Create(FStroke, FMarker);

  FDash.AddDash(DashLength, GapLength);
  FStroke.Width := W;
  FArrowHead.SetHead(0, 10, 5, 0);
  FDash.Shorten := W * 2.0;
end;

destructor TDashStrokeFineArrow.Destroy;
begin
  FConcat.Free;
  FMarker.Free;
  FArrowHead.Free;
  FStroke.Free;
  FMarkerTerm.Free;
  FDash.Free;

  inherited;
end;

procedure TDashStrokeFineArrow.Rewind(PathID: Cardinal);
begin
  FConcat.Rewind(PathID);
end;

function TDashStrokeFineArrow.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FConcat.Vertex(X, Y);
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
var
  I: Integer;
  C1, C2: TAggColor;
begin
  inherited Create(PixelFormat, FlipY);

  FRadioBoxType := TAggControlRadioBox.Create(-1, -1, -1, -1, not FlipY);
  FSliderWidth := TAggControlSlider.Create(190, 8, 390, 15, not FlipY);

  FCheckBoxBenchmark := TAggControlCheckBox.Create(398, 6, 'Benchmark',
    not FlipY);
  FCheckBoxDrawNodes := TAggControlCheckBox.Create(398, 21, 'Draw Nodes',
    not FlipY);
  FCheckBoxDrawEdges := TAggControlCheckBox.Create(488, 21, 'Draw Edges',
    not FlipY);
  FCheckBoxDraft := TAggControlCheckBox.Create(488, 6, 'Draft Mode', not FlipY);
  FCheckBoxTranslucent := TAggControlCheckBox.Create(190, 21,
    'Translucent Mode', not FlipY);

  FGraph := TGraph.Create(200, 100);
  FScanLine := TAggScanLineUnpacked8.Create;

  FDraw := 3;

  FGradientColors := TAggPodAutoArray.Create(256, SizeOf(TAggColor));

  AddControl(FRadioBoxType);

  FRadioBoxType.SetTextSize(8.0);
  FRadioBoxType.AddItem('Solid lines');
  FRadioBoxType.AddItem('Bezier curves');
  FRadioBoxType.AddItem('Dashed curves');
  FRadioBoxType.AddItem('Poygons AA');
  FRadioBoxType.AddItem('Poygons Bin');
  FRadioBoxType.SetCurrentItem(0);

  AddControl(FSliderWidth);

  FSliderWidth.NumSteps := 20;
  FSliderWidth.SetRange(0.0, 5.0);
  FSliderWidth.Value := 2.0;
  FSliderWidth.Caption := 'Width=%1.2f';

  FCheckBoxBenchmark.SetTextSize(8.0);
  FCheckBoxDrawNodes.SetTextSize(8.0);
  FCheckBoxDraft.SetTextSize(8.0);
  FCheckBoxDrawNodes.Status := True;
  FCheckBoxDrawEdges.Status := True;

  AddControl(FCheckBoxBenchmark);
  AddControl(FCheckBoxDrawNodes);
  AddControl(FCheckBoxDrawEdges);
  AddControl(FCheckBoxDraft);
  AddControl(FCheckBoxTranslucent);

  C1.FromRgbaDouble(1, 1, 0, 0.25);
  C2.FromRgbaDouble(0, 0, 1);

  for I := 0 to 255 do
    PAggColor(FGradientColors[I])^ :=
      Gradient(C1, C2, I / 255.0);
end;

destructor TAggApplication.Destroy;
begin
  inherited;

  FRadioBoxType.Free;
  FSliderWidth.Free;

  FCheckBoxBenchmark.Free;
  FCheckBoxDrawNodes.Free;
  FCheckBoxDrawEdges.Free;
  FCheckBoxDraft.Free;
  FCheckBoxTranslucent.Free;

  FGraph.Free;
  FScanLine.Free;
  FGradientColors.Free;
end;

procedure TAggApplication.DrawNodesDraft;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  Prim: TAggRendererPrimitives;

  I: Integer;
  N: TPointDouble;
begin
  PixelFormatBgr24(Pixf, RenderingBufferWindow);
  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Prim := TAggRendererPrimitives.Create(RendererBase);

    I := 0;

    while I < FGraph.NumNodes do
    begin
      N := FGraph.GetNode(I, Width, Height);

      Prim.FillColor := PAggColor(FGradientColors[147])^;
      Prim.LineColor := PAggColor(FGradientColors[255])^;
      Prim.OutlinedEllipse(Trunc(N.X), Trunc(N.Y), 10, 10);

      Prim.FillColor := PAggColor(FGradientColors[50])^;
      Prim.SolidEllipse(Trunc(N.X), Trunc(N.Y), 4, 4);

      Inc(I);
    end;

    Prim.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.DrawNodesFine(Ras: TAggRasterizerScanLine);
var
  Pixf: TAggPixelFormatProcessor;

  SpanAllocator: TAggSpanAllocator;
  RendererBase: TAggRendererBase;
  Gf: TAggGradientFunction;
  Sg: TAggSpanGradient;

  I: Integer;
  N: TPointDouble;

  Circle: TAggCircle;
  Mtx: TAggTransAffine;
  Ren: TAggGradientRenderer;

  SpanInterpolator: TAggSpanInterpolatorLinear;

  X, Y: Double;
begin
  SpanAllocator := TAggSpanAllocator.Create;
  PixelFormatBgr24(Pixf, RenderingBufferWindow);
  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    I := 0;

    while I < FGraph.NumNodes do
    begin
      N := FGraph.GetNode(I, Width, Height);

      Circle := TAggCircle.Create(N, 5.0 * FSliderWidth.Value);
      try
        case FDraw of
          0:
            begin
              Circle.Rewind(0);

              while not IsStop(Circle.Vertex(@X, @Y)) do;
            end;

          1:
            begin
              Ras.Reset;
              Ras.AddPath(Circle);
            end;

          2:
            begin
              Ras.Reset;
              Ras.AddPath(Circle);
              Ras.Sort;
            end;

          3:
            begin
              Gf := TAggGradientFunction.Create;
              try
                Mtx := TAggTransAffine.Create;
                try
                  Mtx.Scale(FSliderWidth.Value * 0.5);
                  Mtx.Translate(N.X, N.Y);

                  Mtx.Invert;
                  SpanInterpolator := TAggSpanInterpolatorLinear.Create(Mtx);
                  Sg := TAggSpanGradient.Create(SpanAllocator, SpanInterpolator,
                    Gf, FGradientColors, 0.0, 10.0);
                  try
                    Ren := TAggGradientRenderer.Create(RendererBase, Sg);

                    Ras.AddPath(Circle);
                    RenderScanLines(Ras, FScanLine, Ren);
                  finally
                    Sg.Free;
                  end;
                finally
                  Mtx.Free;
                end;
                SpanInterpolator.Free;
                Ren.Free;
              finally
                Gf.Free;
              end;
            end;
        end;
      finally
        Circle.Free;
      end;

      Inc(I);
    end;

    SpanAllocator.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.RenderEdgeFine(Ras: TAggRasterizerScanLine;
  RenFine, RenDraft: TAggCustomRendererScanLineSolid; Src: TAggVertexSource);
var
  Rgba: TAggColor;
  X, Y: Double;
  R, G, B, A: Integer;
begin
  case FDraw of
    0:
      begin
        Src.Rewind(0);

        while not IsStop(Src.Vertex(@X, @Y)) do;
      end;

    1:
      begin
        Ras.Reset;
        Ras.AddPath(Src);
      end;

    2:
      begin
        Ras.Reset;
        Ras.AddPath(Src);
        Ras.Sort;
      end;

    3:
      begin
        R := Random($7FFF) and $7F;
        G := Random($7FFF) and $7F;
        B := Random($7FFF) and $7F;
        A := 255;

        if FCheckBoxTranslucent.Status then
          A := 80;

        Ras.AddPath(Src);

        if FRadioBoxType.GetCurrentItem < 4 then
        begin
          Rgba.FromRgbaInteger(R, G, B, A);
          RenFine.SetColor(@Rgba);
          RenderScanLines(Ras, FScanLine, RenFine);
        end
        else
        begin
          Rgba.FromRgbaInteger(R, G, B, A);
          RenDraft.SetColor(@Rgba);
          RenderScanLines(Ras, FScanLine, RenDraft);
        end;
      end;
  end;
end;

procedure TAggApplication.DrawLinesDraft;
var
  Pixf: TAggPixelFormatProcessor;
  Prim: TAggRendererPrimitives;
  Rgba: TAggColor;

  RendererBase : TAggRendererBase;
  Ras: TAggRasterizerOutline;

  I, R, G, B, A: Integer;

  L     : TLine;
  S     : TStrokeDraft;
  E     : TEdge;
  N1, N2: TPointDouble;

begin
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Prim := TAggRendererPrimitives.Create(RendererBase);
    Ras := TAggRasterizerOutline.Create(Prim);

    I := 0;

    while I < FGraph.NumEdges do
    begin
      E := FGraph.GetEdge(I);
      N1 := FGraph.GetNode(E.Node[0], Width, Height);
      N2 := FGraph.GetNode(E.Node[1], Width, Height);

      L := TLine.Create(N1.X, N1.Y, N2.X, N2.Y);
      try
        S := TStrokeDraft.Create(L, FSliderWidth.Value);
        try
          R := Random($80);
          G := Random($80);
          B := Random($80);
          A := 255;

          if FCheckBoxTranslucent.Status then
            A := 80;

          Rgba.FromRgbaInteger(R, G, B, A);
          Prim.LineColor := Rgba;
          Ras.AddPath(S);
        finally
          S.Free;
        end;
      finally
        L.Free;
      end;

      Inc(I);
    end;

    Ras.Free;
    Prim.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.DrawCurvesDraft;
var
  Pixf: TAggPixelFormatProcessor;
  Prim: TAggRendererPrimitives;
  Rgba: TAggColor;

  RendererBase : TAggRendererBase;
  Ras: TAggRasterizerOutline;

  I, R, G, B, A: Integer;

  C     : TCurve;
  S     : TStrokeDraft;
  E     : TEdge;
  N1, N2: TPointDouble;
begin
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Prim := TAggRendererPrimitives.Create(RendererBase);
    Ras := TAggRasterizerOutline.Create(Prim);

    I := 0;

    while I < FGraph.NumEdges do
    begin
      E := FGraph.GetEdge(I);
      N1 := FGraph.GetNode(E.Node[0], Width, Height);
      N2 := FGraph.GetNode(E.Node[1], Width, Height);

      C := TCurve.Create(N1.X, N1.Y, N2.X, N2.Y);
      try
        S := TStrokeDraft.Create(C, FSliderWidth.Value);
        try
          R := Random($80);
          G := Random($80);
          B := Random($80);
          A := 255;

          if FCheckBoxTranslucent.Status then
            A := 80;

          Rgba.FromRgbaInteger(R, G, B, A);
          Prim.LineColor := Rgba;
          Ras.AddPath(S);
        finally
          S.Free;
        end;
      finally
        C.Free;
      end;

      Inc(I);
    end;

    Ras.Free;
    Prim.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.DrawDashesDraft;
var
  Pixf: TAggPixelFormatProcessor;
  Prim: TAggRendererPrimitives;
  Rgba: TAggColor;

  RendererBase : TAggRendererBase;
  Ras: TAggRasterizerOutline;

  I: Integer;
  Rgba8: TAggRgba8;

  C: TCurve;
  S: TDashStrokeDraft;
  E: TEdge;
  N1, N2: TPointDouble;
begin
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Prim := TAggRendererPrimitives.Create(RendererBase);
    Ras := TAggRasterizerOutline.Create(Prim);

    I := 0;

    while I < FGraph.NumEdges do
    begin
      E := FGraph.GetEdge(I);
      N1 := FGraph.GetNode(E.Node[0], Width, Height);
      N2 := FGraph.GetNode(E.Node[1], Width, Height);

      C := TCurve.Create(N1.X, N1.Y, N2.X, N2.Y);
      try
        S := TDashStrokeDraft.Create(C, 6.0, 3.0, FSliderWidth.Value);
        try
          Rgba8.R := Random($80);
          Rgba8.G := Random($80);
          Rgba8.B := Random($80);
          Rgba8.A := 255;

          if FCheckBoxTranslucent.Status then
            Rgba8.A := 80;

          Rgba.FromRgba8(Rgba8);
          Prim.LineColor := Rgba;
          Ras.AddPath(S);
        finally
          S.Free;
        end;
      finally
        C.Free;
      end;

      Inc(I);
    end;

    Ras.Free;
    Prim.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.DrawLinesFine(Ras: TAggRasterizerScanLine;
  Solid, Draft: TAggCustomRendererScanLineSolid);
var
  I: Integer;
  B: TEdge;
  L: TLine;
  S: TStrokeFine;

  N1, N2: TPointDouble;
begin
  I := 0;

  while I < FGraph.NumEdges do
  begin
    B := FGraph.GetEdge(I);
    N1 := FGraph.GetNode(B.Node[0], Width, Height);
    N2 := FGraph.GetNode(B.Node[1], Width, Height);

    L := TLine.Create(N1.X, N1.Y, N2.X, N2.Y);
    try
      S := TStrokeFine.Create(L, FSliderWidth.Value);
      try
        RenderEdgeFine(Ras, Solid, Draft, S);
      finally
        S.Free;
      end;
    finally
      L.Free;
    end;

    Inc(I);
  end;
end;

procedure TAggApplication.DrawCurvesFine(Ras: TAggRasterizerScanLine; Solid,
  Draft: TAggCustomRendererScanLineSolid);
var
  I: Integer;
  B: TEdge;
  C: TCurve;
  S: TStrokeFine;
  N1, N2: TPointDouble;
begin
  I := 0;

  while I < FGraph.NumEdges do
  begin
    B := FGraph.GetEdge(I);
    N1 := FGraph.GetNode(B.Node[0], Width, Height);
    N2 := FGraph.GetNode(B.Node[1], Width, Height);

    C := TCurve.Create(N1.X, N1.Y, N2.X, N2.Y);
    try
      S := TStrokeFine.Create(C, FSliderWidth.Value);
      try
        RenderEdgeFine(Ras, Solid, Draft, S);
      finally
        S.Free;
      end;
    finally
      C.Free;
    end;

    Inc(I);
  end;
end;

procedure TAggApplication.DrawDashesFine(Ras: TAggRasterizerScanLine;
  Solid, Draft: TAggCustomRendererScanLineSolid);
var
  I: Integer;
  B: TEdge;
  C: TCurve;
  S: TDashStrokeFine;
  N1, N2: TPointDouble;
begin
  I := 0;

  while I < FGraph.NumEdges do
  begin
    B := FGraph.GetEdge(I);
    N1 := FGraph.GetNode(B.Node[0], Width, Height);
    N2 := FGraph.GetNode(B.Node[1], Width, Height);

    C := TCurve.Create(N1.X, N1.Y, N2.X, N2.Y);
    try
      S := TDashStrokeFine.Create(C, 6.0, 3.0, FSliderWidth.Value);
      try
        RenderEdgeFine(Ras, Solid, Draft, S);
      finally
        S.Free;
      end;
    finally
      C.Free;
    end;

    Inc(I);
  end;
end;

procedure TAggApplication.DrawPolygons(Ras: TAggRasterizerScanLine;
  Solid, Draft: TAggCustomRendererScanLineSolid);
var
  I: Integer;
  B: TEdge;
  C: TCurve;

  N1, N2: TPointDouble;

  GammaNone: TAggGammaNone;
  GammaThreshold: TAggGammaThreshold;
begin
  if FRadioBoxType.GetCurrentItem = 4 then
  begin
    GammaThreshold := TAggGammaThreshold.Create(0.5);
    try
      Ras.Gamma(GammaThreshold);
    finally
      GammaThreshold.Free;
    end;
  end;

  I := 0;

  while I < FGraph.NumEdges do
  begin
    B := FGraph.GetEdge(I);
    N1 := FGraph.GetNode(B.Node[0], Width, Height);
    N2 := FGraph.GetNode(B.Node[1], Width, Height);

    C := TCurve.Create(N1.X, N1.Y, N2.X, N2.Y);
    try
      RenderEdgeFine(Ras, Solid, Draft, C);
    finally
      C.Free;
    end;

    Inc(I);
  end;

  GammaNone := TAggGammaNone.Create;
  try
    Ras.Gamma(GammaNone);
  finally
    GammaNone.Free;
  end;
end;

procedure TAggApplication.DrawScene(Ras: TAggRasterizerScanLine;
  Solid, Draft: TAggCustomRendererScanLineSolid);
var
  GammaNone: TAggGammaNone;
begin
  GammaNone := TAggGammaNone.Create;
  try
    Ras.Gamma(GammaNone);
  finally
    GammaNone.Free;
  end;

  RandSeed := 100;

  if FCheckBoxDrawNodes.Status then
    if FCheckBoxDraft.Status then
      DrawNodesDraft
    else
      DrawNodesFine(Ras);

  if FCheckBoxDrawEdges.Status then
    if FCheckBoxDraft.Status then
      case FRadioBoxType.GetCurrentItem of
        0:
          DrawLinesDraft;
        1:
          DrawCurvesDraft;
        2:
          DrawDashesDraft;
      end
    else
      case FRadioBoxType.GetCurrentItem of
        0:
          DrawLinesFine(Ras, Solid, Draft);

        1:
          DrawCurvesFine(Ras, Solid, Draft);

        2:
          DrawDashesFine(Ras, Solid, Draft);

        3, 4:
          DrawPolygons(Ras, Solid, Draft);
      end;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  Ras: TAggRasterizerScanLineAA;
  RendererBase : TAggRendererBase;

  Solid: TSolidRenderer;
  Draft: TDraftRenderer;
begin
  // Initialize structures
  Ras := TAggRasterizerScanLineAA.Create;

  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    Solid := TSolidRenderer.Create(RendererBase);
    Draft := TDraftRenderer.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      // Render
      DrawScene(Ras, Solid, Draft);

      // Render the controls
      Ras.FillingRule := frNonZero;

      RenderControl(Ras, FScanLine, Solid, FRadioBoxType);
      RenderControl(Ras, FScanLine, Solid, FSliderWidth);
      RenderControl(Ras, FScanLine, Solid, FCheckBoxBenchmark);
      RenderControl(Ras, FScanLine, Solid, FCheckBoxDrawNodes);
      RenderControl(Ras, FScanLine, Solid, FCheckBoxDrawEdges);
      RenderControl(Ras, FScanLine, Solid, FCheckBoxDraft);
      RenderControl(Ras, FScanLine, Solid, FCheckBoxTranslucent);

      // Free AGG resources
      Ras.Free;
    finally
      Solid.Free;
      Draft.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnControlChange;
var
  Pixf: TAggPixelFormatProcessor;

  I : Integer;
  Fd: Text;

  Ras: TAggRasterizerScanLineAA;
  RendererBase : TAggRendererBase;

  Solid: TSolidRenderer;
  Draft: TDraftRenderer;

  Text: AnsiString;
  Times: array [0..4] of Double;
begin
  if FCheckBoxBenchmark.Status then
  begin
    OnDraw;
    UpdateWindow;

    Ras := TAggRasterizerScanLineAA.Create;

    PixelFormatBgr24(Pixf, RenderingBufferWindow);

    RendererBase := TAggRendererBase.Create(Pixf, True);
    try
      Solid := TSolidRenderer.Create(RendererBase);
      Draft := TDraftRenderer.Create(RendererBase);
      try
        if FCheckBoxDraft.Status then
        begin
          StartTimer;

          for I := 0 to 9 do
            DrawScene(Ras, Solid, Draft);

          Text := Format('%3.3f milliseconds', [GetElapsedTime]);
        end
        else
        begin
          FDraw := 0;

          while FDraw < 4 do
          begin
            StartTimer;

            for I := 0 to 9 do
              DrawScene(Ras, Solid, Draft);

            Times[FDraw] := GetElapsedTime;

            Inc(FDraw);
          end;

          FDraw := 3;

          Times[4] := Times[3];
          Times[3] := Times[3] - Times[2];
          Times[2] := Times[2] - Times[1];
          Times[1] := Times[1] - Times[0];

          AssignFile(Fd, 'benchmark');
          Rewrite(Fd);

          Text := Format('%10.3f %10.3f %10.3f %10.3f %10.3f', [Times[0],
            Times[1], Times[2], Times[3], Times[4]]);

          Writeln(Fd, PAnsiChar(@Text[1]));
          Close(Fd);

          Text := Format('    pipeline        path         sort         '
            + 'render        total'#13 + '%10.3f %10.3f %10.3f %10.3f %10.3f ',
            [Times[0], Times[1], Times[2], Times[3], Times[4]]);
        end;

        DisplayMessage(Text);

        FCheckBoxBenchmark.Status := False;
        ForceRedraw;

        Ras.Free;
      finally
        Solid.Free;
        Draft.Free;
      end;
    finally
      RendererBase.Free;
    end;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Yet another example of the "general" kind. It was used '
      + 'mostly to compare the performance of different steps of rendering '
      + 'in order to see the weaknesses. The Win GDI+ analog of it looks '
      + 'worse and works sLower. Try "GDI_graph_test.zip"'
      + '(from www.antigrain.com/demo) and compare it with the AGG one. The '
      + 'most disappointing thing in GDI+ is that it cannot draw Bezier '
      + 'curves correctly. '#13#13
      + 'How to play with:'#13#13
      + 'Run the GDI+ example, choose menu Image/Bezier curves, expand '
      + 'the window to about 1000x1000 pixels, and then gradually change the '
      + 'size of the window. You will see that some curves miss the '
      + 'destination points (the centers of the node circles).'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Line Join (F1-Help)';

    if Init(600 + 100, 500 + 30, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
