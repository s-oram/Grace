program Perspective;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

{ DEFINE AGG_GRAY8 }
{$DEFINE AGG_BGR24 }
{ DEFINE AGG_Rgb24 }
{ DEFINE AGG_BGRA32 }
{ DEFINE AGG_RgbA32 }
{ DEFINE AGG_ARGB32 }
{ DEFINE AGG_ABGR32 }
{ DEFINE AGG_Rgb565 }
{ DEFINE AGG_Rgb555 }

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',

  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',

  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggTransBilinear in '..\..\Source\AggTransBilinear.pas',
  AggTransPerspective in '..\..\Source\AggTransPerspective.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvClipPolygon in '..\..\Source\AggConvClipPolygon.pas',
  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggInteractivePolygon,
  AggParseLion
{$I Pixel_Formats.inc}

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FRasterizer: TAggRasterizerScanLineAA;
    FScanLine: TAggScanLinePacked8;

    FPath: TAggPathStorage;
    FColors: array [0..99] of TAggColor;
    FPathIndex: array [0..99] of Cardinal;

    FPathCount: Cardinal;

    FBoundingRect: TRectDouble;
    FBaseDelta: TPointDouble;
    FQuad: TInteractivePolygon;
    FRadioBoxTransType: TAggControlRadioBox;
  protected
    procedure ParseLion;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags); override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags); override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FRasterizer := TAggRasterizerScanLineAA.Create;
  FScanLine := TAggScanLinePacked8.Create;
  FPath := TAggPathStorage.Create;

  FPathCount := 0;

  FBoundingRect := RectDouble(0, 0, 0, 0);
  FBaseDelta := PointDouble(0);

  FQuad := TInteractivePolygon.Create(4, 5);
  FRadioBoxTransType := TAggControlRadioBox.Create(420, 5, 550, 55, not FlipY);

  ParseLion;

  FQuad.Point[0] := PointDouble(FBoundingRect.X1, FBoundingRect.Y1);
  FQuad.Point[1] := PointDouble(FBoundingRect.X2, FBoundingRect.Y1);
  FQuad.Point[2] := PointDouble(FBoundingRect.X2, FBoundingRect.Y2);
  FQuad.Point[3] := PointDouble(FBoundingRect.X1, FBoundingRect.Y2);

  FRadioBoxTransType.AddItem('Bilinear');
  FRadioBoxTransType.AddItem('Perspective');
  FRadioBoxTransType.SetCurrentItem(0);

  AddControl(FRadioBoxTransType);
end;

destructor TAggApplication.Destroy;
begin
  FRasterizer.Free;
  FScanLine.Free;
  FPath.Free;

  FQuad.Free;
  FRadioBoxTransType.Free;

  inherited;
end;

procedure TAggApplication.ParseLion;
begin
  FPathCount := AggParseLion.ParseLion(FPath, @FColors, @FPathIndex);

  BoundingRect(FPath, @FPathIndex, 0, FPathCount, FBoundingRect);

  FBaseDelta := PointDouble(FBoundingRect.CenterX, FBoundingRect.CenterY);

  FPath.FlipX(FBoundingRect.X1, FBoundingRect.X2);
  FPath.FlipY(FBoundingRect.Y1, FBoundingRect.Y2);
end;

procedure TAggApplication.OnInit;
var
  Delta: TPointDouble;
begin
  Delta.X := 0.5 * (Width - (FQuad.Xn[1] - FQuad.Xn[0]));
  Delta.Y := 0.5 * (Height - (FQuad.Yn[2] - FQuad.Yn[0]));

  FQuad.Xn[0] := FQuad.Xn[0] + Delta.X;
  FQuad.Yn[0] := FQuad.Yn[0] + Delta.Y;
  FQuad.Xn[1] := FQuad.Xn[1] + Delta.X;
  FQuad.Yn[1] := FQuad.Yn[1] + Delta.Y;
  FQuad.Xn[2] := FQuad.Xn[2] + Delta.X;
  FQuad.Yn[2] := FQuad.Yn[2] + Delta.Y;
  FQuad.Xn[3] := FQuad.Xn[3] + Delta.X;
  FQuad.Yn[3] := FQuad.Yn[3] + Delta.Y;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RenSl : TAggRendererScanLineAASolid;

  Rgba: TAggColor;

  TransfromBilinear: TAggTransBilinear;
  TransformPerspective: TAggTransPerspective23;

  Trans: TAggConvTransform;

  Ellipse: TAggEllipse;
  EllipseStroke: TAggConvStroke;
  TransformEllipse: TAggConvTransform;
  TransformEllipseStroke: TAggConvTransform;
begin
  // Initialize structures
  CPixelFormat(Pixf, RenderingBufferWindow);
  TransfromBilinear := nil;
  TransformPerspective := nil;

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenSl := TAggRendererScanLineAASolid.Create(RendererBase);

    RendererBase.Clear(CRgba8White);

    FRasterizer.SetClipBox(0, 0, Width, Height);

    // Perspective rendering
    if FRadioBoxTransType.GetCurrentItem = 0 then
    begin
      TransfromBilinear := TAggTransBilinear.Create(FBoundingRect,
        PQuadDouble(FQuad.Polygon));

      if TransfromBilinear.IsValid then
      begin
        // Render transformed lion
        Trans := TAggConvTransform.Create(FPath, TransfromBilinear);
        RenderAllPaths(FRasterizer, FScanLine, RenSl, Trans, @FColors,
          @FPathIndex, FPathCount);

        // Render transformed Ellipse
        Ellipse := TAggEllipse.Create(FBoundingRect.CenterX,
          FBoundingRect.CenterY, FBoundingRect.CenterX, FBoundingRect.CenterY,
          200);

        EllipseStroke := TAggConvStroke.Create(Ellipse);
        try
          EllipseStroke.Width := 3.0;
          TransformEllipse := TAggConvTransform.Create(Ellipse, TransfromBilinear);

          TransformEllipseStroke := TAggConvTransform.Create(EllipseStroke,
            TransfromBilinear);

          FRasterizer.AddPath(TransformEllipse);

          Rgba.FromRgbaDouble(0.5, 0.3, 0.0, 0.3);
          RenSl.SetColor(@Rgba);

          RenderScanLines(FRasterizer, FScanLine, RenSl);

          FRasterizer.AddPath(TransformEllipseStroke);

          Rgba.FromRgbaDouble(0.0, 0.3, 0.2, 1.0);
          RenSl.SetColor(@Rgba);

          RenderScanLines(FRasterizer, FScanLine, RenSl);
        finally
          EllipseStroke.Free;
        end;
      end;
    end
    else
    begin
      TransformPerspective := TAggTransPerspective23.Create(FBoundingRect,
        PQuadDouble(FQuad.Polygon));

      if TransformPerspective.IsValid then
      begin
        // Render transformed lion
        Trans := TAggConvTransform.Create(FPath, TransformPerspective);

        RenderAllPaths(FRasterizer, FScanLine, RenSl,  Trans, @FColors,
          @FPathIndex, FPathCount);

        // Render transformed Ellipse
        Ellipse := TAggEllipse.Create(FBoundingRect.CenterX,
          FBoundingRect.CenterY, FBoundingRect.CenterX, FBoundingRect.CenterY,
          200);

        EllipseStroke := TAggConvStroke.Create(Ellipse);
        EllipseStroke.Width := 3.0;

        TransformEllipse := TAggConvTransform.Create(Ellipse,
          TransformPerspective);
        TransformEllipseStroke := TAggConvTransform.Create(EllipseStroke,
          TransformPerspective);

        FRasterizer.AddPath(TransformEllipse);

        Rgba.FromRgbaDouble(0.5, 0.3, 0.0, 0.3);
        RenSl.SetColor(@Rgba);

        RenderScanLines(FRasterizer, FScanLine, RenSl);

        FRasterizer.AddPath(TransformEllipseStroke);

        Rgba.FromRgbaDouble(0.0, 0.3, 0.2, 1.0);
        RenSl.SetColor(@Rgba);

        RenderScanLines(FRasterizer, FScanLine, RenSl);

        // Free
        EllipseStroke.Free;
      end;
    end;

    // Render the "quad" tool and controls
    FRasterizer.AddPath(FQuad);

    Rgba.FromRgbaDouble(0, 0.3, 0.5, 0.6);
    RenSl.SetColor(@Rgba);

    RenderScanLines(FRasterizer, FScanLine, RenSl);
    RenderControl(FRasterizer, FScanLine, RenSl, FRadioBoxTransType);

    if Assigned(TransfromBilinear) then
      TransfromBilinear.Free;

    if Assigned(TransformPerspective) then
      TransformPerspective.Free;

    Trans.Free;
    TransformEllipse.Free;
    TransformEllipseStroke.Free;

    Ellipse.Free;
    RenSl.Free;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FQuad.OnMouseMove(X, Y) then
      ForceRedraw;

  if not (mkfMouseLeft in Flags) then
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
    if FQuad.OnMouseButtonDown(X, Y) then
      ForceRedraw;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
begin
  if FQuad.OnMouseButtonUp(X, Y) then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Perspective and bilinear transformations. In general, '
      + 'these classes can transform an arbitrary quadrangle to another '
      + 'arbitrary quadrangle (with some restrictions). The example '
      + 'demonstrates how to transform a rectangle to a quadrangle defined by '
      + '4 vertices. Note, that the perspective transformations don''t work '
      + 'correctly if the destination quadrangle is concave. Bilinear '
      + 'thansformations give a different result, but remain valid with any '
      + 'shape of the destination quadrangle.'#13#13
      + 'How to play with:'#13#13
      + 'You can drag the 4 corners of the quadrangle, as well as its '
      + 'boundaries.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

begin
  with TAggApplication.Create(CPixFormat, CFlipY) do
  try
    Caption := 'AGG Example. Perspective Transformations (F1-Help)';

    if Init(600, 600, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
