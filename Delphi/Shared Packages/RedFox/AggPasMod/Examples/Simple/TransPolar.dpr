program TransPolar;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggMath in '..\..\Source\AggMath.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvSegmentator in '..\..\Source\AggConvSegmentator.pas',
  AggVertexSource in '..\..\Source\AggVertexSource.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas';

const
  CFlipY = True;

type
  TTransformedControl = class(TAggCustomAggControl)
  private
    FControl: TAggCustomAggControl;
    FPipeline: TAggVertexSource;
  protected
    function GetColorPointer(Index: Cardinal): PAggColor; override;
    function GetPathCount: Cardinal; override;
  public
    constructor Create(Ctrl: TAggCustomAggControl; Pl: TAggVertexSource);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TTransPolar = class(TAggTransAffine)
  private
    FBaseAngle, FBaseScale: Double;
    FTranslation, FBase: TPointDouble;
    FSpiral: Double;
  public
    constructor Create;

    procedure SetBaseScale(Value: Double);
    procedure SetFullCircle(Value: Double);
    procedure SetBaseOffset(Dx, Dy: Double);
    procedure SetTranslation(Dx, Dy: Double);
    procedure SetSpiral(Value: Double);
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FSlider, FSliderSpiral, FSliderBaseY: TAggControlSlider;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;
    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TTransformedControl }

constructor TTransformedControl.Create;
begin
  FControl := Ctrl;
  FPipeline := Pl;
end;

function TTransformedControl.GetPathCount: Cardinal;
begin
  Result := FControl.PathCount;
end;

procedure TTransformedControl.Rewind(PathID: Cardinal);
begin
  FPipeline.Rewind(PathID);
end;

function TTransformedControl.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FPipeline.Vertex(X, Y);
end;

function TTransformedControl.GetColorPointer(Index: Cardinal): PAggColor;
begin
  Result := FControl.ColorPointer[Index];
end;


procedure TransPolarTransform(This: TTransPolar; X, Y: PDouble);
var
  X1, Y1: Double;
begin
  X1 := (X^ + This.FBase.X) * This.FBaseAngle;
  Y1 := (Y^ + This.FBase.Y) * This.FBaseScale + (X^ * This.FSpiral);

  SinCosScale(X1, Y1, X1, Y1);
  X^ := X1 + This.FTranslation.X;
  Y^ := Y1 + This.FTranslation.Y;
end;


{ TTransPolar }

constructor TTransPolar.Create;
begin
  inherited Create;

  FBaseAngle := 1;
  FBaseScale := 1;

  FBase := PointDouble(0);

  FTranslation := PointDouble(0);
  FSpiral := 0;

  Transform := @TransPolarTransform;
end;

procedure TTransPolar.SetBaseScale(Value: Double);
begin
  FBaseScale := Value;
end;

procedure TTransPolar.SetFullCircle(Value: Double);
begin
  FBaseAngle := 2 * Pi / Value;
end;

procedure TTransPolar.SetBaseOffset(Dx, Dy: Double);
begin
  FBase.X := Dx;
  FBase.Y := Dy;
end;

procedure TTransPolar.SetTranslation(Dx, Dy: Double);
begin
  FTranslation.X := Dx;
  FTranslation.Y := Dy;
end;

procedure TTransPolar.SetSpiral(Value: Double);
begin
  FSpiral := Value;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FSlider := TAggControlSlider.Create(10, 10, 600 - 10, 17, not FlipY);
  FSliderSpiral := TAggControlSlider.Create(10, 30, 590, 37, not FlipY);
  FSliderBaseY := TAggControlSlider.Create(10, 50, 590, 57, not FlipY);

  AddControl(FSlider);

  FSlider.SetRange(0.0, 100.0);
  FSlider.NumSteps := 5;
  FSlider.Value := 32.0;
  FSlider.Caption := 'Some Value=%1.0f';

  AddControl(FSliderSpiral);

  FSliderSpiral.Caption := 'Spiral=%.3f';
  FSliderSpiral.SetRange(-0.1, 0.1);
  FSliderSpiral.Value := 0.0;

  AddControl(FSliderBaseY);

  FSliderBaseY.Caption := 'Base Y=%.3f';
  FSliderBaseY.SetRange(50.0, 200.0);
  FSliderBaseY.Value := 120.0;
end;

destructor TAggApplication.Destroy;
begin
  FSlider.Free;
  FSliderSpiral.Free;
  FSliderBaseY.Free;

  inherited;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase : TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Ras: TAggRasterizerScanLineAA;
  Sl : TAggScanLineUnpacked8;

  Trans: TTransPolar;
  Segm : TAggConvSegmentator;

  Pipeline: TAggConvTransform;

  Ctrl: TTransformedControl;
begin
  // Initialize structures
  PixelFormatBgr24(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      Ras := TAggRasterizerScanLineAA.Create;
      Sl := TAggScanLineUnpacked8.Create;

      // Render the controls
      RenderControl(Ras, Sl, RenScan, FSlider);
      RenderControl(Ras, Sl, RenScan, FSliderSpiral);
      RenderControl(Ras, Sl, RenScan, FSliderBaseY);

      // Render
      Trans := TTransPolar.Create;
      Trans.SetFullCircle(-600);
      Trans.SetBaseScale(-1);
      Trans.SetBaseOffset(0, FSliderBaseY.Value);
      Trans.SetTranslation(Width * 0.5, Height * 0.5 + 30);
      Trans.SetSpiral(-FSliderSpiral.Value);

      Segm := TAggConvSegmentator.Create(FSlider);
      Pipeline := TAggConvTransform.Create(Segm, Trans);
      try
        Ctrl := TTransformedControl.Create(FSlider, Pipeline);
        try
          RenderControl(Ras, Sl, RenScan, Ctrl);
        finally
          Ctrl.Free;
        end;
      finally
        Pipeline.Free;
      end;

      // Free AGG resources
      Segm.Free;
      Trans.Free;
      Ras.Free;
      Sl.Free;
    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Another example of non-linear transformations requested'
      + 'by one of my friends. Here we render a standard AGG control in its'
      + 'original form (the slider in the bottom) and after the transformation.'
      + 'The transformer itself is not a part of AGG and just demonstrates how'
      + 'to write custom transformers (class TTransPolar).'#13
      + 'Note that because the transformer is non-linear, we need to use'
      + 'TAggConvSegmentator first. Don''t worry much about the'
      + 'TTransformedControl class, it''s just an adaptor used to render the'
      + 'controls with additional transformations.'#13#13
      + 'How to play with:'#13#13
      + 'Try to drag the value of the slider at the bottom and watch how'
      + 'it''s being synchronized in the polar coordinates. Also change two'
      + 'other parameters (Spiral and Base Y) and the size of the window.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s'
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Polar Transformer (F1-Help)';

    if Init(600, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
