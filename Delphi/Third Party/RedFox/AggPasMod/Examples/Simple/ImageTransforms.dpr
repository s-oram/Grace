program ImageTransforms;

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
  AggFileUtils, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',

  AggControl in '..\..\Source\Controls\AggControl.pas',
  AggCheckBoxControl in '..\..\Source\Controls\AggCheckBoxControl.pas',
  AggRadioBoxControl in '..\..\Source\Controls\AggRadioBoxControl.pas',
  AggSliderControl in '..\..\Source\Controls\AggSliderControl.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggImageFilters in '..\..\Source\AggImageFilters.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggSpanImageFilter in '..\..\Source\AggSpanImageFilter.pas',
  AggSpanImageFilterRgba in '..\..\Source\AggSpanImageFilterRgba.pas',
  AggSpanInterpolatorLinear in '..\..\Source\AggSpanInterpolatorLinear.pas';

const
  CFlipY = True;

type
  TAggApplication = class(TPlatformSupport)
  private
    FSliderPolygonAngle, FSliderPolygonScale: TAggControlSlider;
    FSliderImageAngle, FSliderImageScale: TAggControlSlider;
    FCheckBoxRotatePolygon, FCheckBoxRotateImage: TAggControlCheckBox;
    FRadioBoxExample: TAggControlRadioBox;
    FImageCenter, FPolygonC, FImageC, FDelta: TPointDouble;
    FFlag: Integer;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure BuildStar(Ps: TAggPathStorage);

    procedure OnInit; override;
    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;
    procedure OnMouseButtonUp(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnControlChange; override;
    procedure OnIdle; override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FSliderPolygonAngle := TAggControlSlider.Create(5, 5, 145, 11, not FlipY);
  FSliderPolygonScale := TAggControlSlider.Create(5, 19, 145, 26, not FlipY);
  FSliderImageAngle := TAggControlSlider.Create(155, 5, 300, 12, not FlipY);
  FSliderImageScale := TAggControlSlider.Create(155, 19, 300, 26, not FlipY);
  FCheckBoxRotatePolygon := TAggControlCheckBox.Create(5, 33, 'Rotate Polygon',
    not FlipY);
  FCheckBoxRotateImage := TAggControlCheckBox.Create(5, 47, 'Rotate Image',
    not FlipY);
  FRadioBoxExample := TAggControlRadioBox.Create(-3, 56, -3, 56, not FlipY);

  FFlag := 0;

  AddControl(FSliderPolygonAngle);
  AddControl(FSliderPolygonScale);
  AddControl(FSliderImageAngle);
  AddControl(FSliderImageScale);
  AddControl(FCheckBoxRotatePolygon);
  AddControl(FCheckBoxRotateImage);
  AddControl(FRadioBoxExample);

  FSliderPolygonAngle.Caption := 'Polygon Angle=%3.2f';
  FSliderPolygonScale.Caption := 'Polygon Scale=%3.2f';
  FSliderPolygonAngle.SetRange(-180, 180);
  FSliderPolygonScale.SetRange(0.1, 5);
  FSliderPolygonScale.Value := 1;

  FSliderImageAngle.Caption := 'Image Angle=%3.2f';
  FSliderImageScale.Caption := 'Image Scale=%3.2f';
  FSliderImageAngle.SetRange(-180, 180);
  FSliderImageScale.SetRange(0.1, 5);
  FSliderImageScale.Value := 1;

  FRadioBoxExample.AddItem('0');
  FRadioBoxExample.AddItem('1');
  FRadioBoxExample.AddItem('2');
  FRadioBoxExample.AddItem('3');
  FRadioBoxExample.AddItem('4');
  FRadioBoxExample.AddItem('5');
  FRadioBoxExample.AddItem('6');
  FRadioBoxExample.SetCurrentItem(0);
end;

destructor TAggApplication.Destroy;
begin
  FSliderPolygonAngle.Free;
  FSliderPolygonScale.Free;
  FSliderImageAngle.Free;
  FSliderImageScale.Free;
  FCheckBoxRotatePolygon.Free;
  FCheckBoxRotateImage.Free;
  FRadioBoxExample.Free;

  inherited;
end;

procedure TAggApplication.BuildStar(Ps: TAggPathStorage);
var
  R, R1, R2, Dx, Dy: Double;
  Nr, I: Cardinal;
begin
  R := FInitialWidth;

  if FInitialHeight < R then
    R := FInitialHeight;

  R1 := R / 3 - 8;
  R2 := R1 / 1.45;
  Nr := 14;

  for I := 0 to Nr - 1 do
  begin
    SinCos(2 * Pi * I / Nr - Pi * 0.5, Dy, Dx);

    if I and 1 <> 0 then
      Ps.LineTo(FPolygonC.x + Dx * R1, FPolygonC.y + Dy * R1)
    else if I <> 0 then
      Ps.LineTo(FPolygonC.x + Dx * R2, FPolygonC.y + Dy * R2)
    else
      Ps.MoveTo(FPolygonC.x + Dx * R2, FPolygonC.y + Dy * R2);
  end;
end;

procedure TAggApplication.OnInit;
begin
  FImageCenter.X := FInitialWidth * 0.5;
  FImageCenter.Y := FInitialHeight * 0.5;

  FPolygonC.x := FInitialWidth * 0.5;
  FImageC.X := FPolygonC.x;
  FPolygonC.y := FInitialHeight * 0.5;
  FImageC.y := FPolygonC.y;
end;

procedure TAggApplication.OnDraw;
var
  Pixf: TAggPixelFormatProcessor;
  Rgba: TAggColor;

  RendererBase: TAggRendererBase;
  RenScan: TAggRendererScanLineAASolid;
  Ras: TAggRasterizerScanLineAA;
  Sl: TAggScanLineUnpacked8;
  Ps: TAggPathStorage;
  ConvTransform: TAggConvTransform;
  Circle: array [0..1] of TAggCircle;
  C1: TAggConvStroke;
  SpanAllocator: TAggSpanAllocator;
  Sg: TAggSpanImageFilter;
  Rsi: TAggRendererScanLineAA;
//  Fi: TAggCustomImageFilter;
  V: Double;

  Filter: TAggImageFilter;

  Interpolator: TAggSpanInterpolatorLinear;

  ImageMatrix, PolygonMatrix: TAggTransAffine;
begin
  Filter := nil;

  // Initialize structures
  PixelFormatBgra32(Pixf, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(Pixf, True);
  try
    RenScan := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      ImageMatrix := TAggTransAffine.Create;
      PolygonMatrix := TAggTransAffine.Create;

      PolygonMatrix.Translate(-FPolygonC.x, -FPolygonC.y);
      PolygonMatrix.Rotate(Deg2Rad(FSliderPolygonAngle.Value));
      PolygonMatrix.Scale(FSliderPolygonScale.Value);
      PolygonMatrix.Translate(FPolygonC.x, FPolygonC.y);

      case FRadioBoxExample.GetCurrentItem of
        1: // --------------(Example 1)
          begin
            ImageMatrix.Translate(-FImageCenter.X, -FImageCenter.Y);
            ImageMatrix.Rotate(Deg2Rad(FSliderPolygonAngle.Value));
            ImageMatrix.Scale(FSliderPolygonScale.Value);
            ImageMatrix.Translate(FPolygonC.x, FPolygonC.y);
            ImageMatrix.Invert;
          end;

        2: // --------------(Example 2)
          begin
            ImageMatrix.Translate(-FImageCenter.X, -FImageCenter.Y);
            ImageMatrix.Rotate(Deg2Rad(FSliderImageAngle.Value));
            ImageMatrix.Scale(FSliderImageScale.Value);
            ImageMatrix.Translate(FImageC.X, FImageC.y);
            ImageMatrix.Invert;
          end;

        3: // --------------(Example 3)
          begin
            ImageMatrix.Translate(-FImageCenter.X, -FImageCenter.Y);
            ImageMatrix.Rotate(Deg2Rad(FSliderImageAngle.Value));
            ImageMatrix.Scale(FSliderImageScale.Value);
            ImageMatrix.Translate(FPolygonC.x, FPolygonC.y);
            ImageMatrix.Invert;
          end;

        4: // --------------(Example 4)
          begin
            ImageMatrix.Translate(-FImageC.X, -FImageC.y);
            ImageMatrix.Rotate(Deg2Rad(FSliderPolygonAngle.Value));
            ImageMatrix.Scale(FSliderPolygonScale.Value);
            ImageMatrix.Translate(FPolygonC.x, FPolygonC.y);
            ImageMatrix.Invert;
          end;

        5: // --------------(Example 5)
          begin
            ImageMatrix.Translate(-FImageCenter.X, -FImageCenter.Y);
            ImageMatrix.Rotate(Deg2Rad(FSliderImageAngle.Value));
            ImageMatrix.Rotate(Deg2Rad(FSliderPolygonAngle.Value));
            ImageMatrix.Scale(FSliderImageScale.Value);
            ImageMatrix.Scale(FSliderPolygonScale.Value);
            ImageMatrix.Translate(FImageC.X, FImageC.y);
            ImageMatrix.Invert;
          end;

        6: // --------------(Example 6)
          begin
            ImageMatrix.Translate(-FImageC.X, -FImageC.Y);
            ImageMatrix.Rotate(Deg2Rad(FSliderImageAngle.Value));
            ImageMatrix.Scale(FSliderImageScale.Value);
            ImageMatrix.Translate(FImageC.x, FImageC.Y);

            ImageMatrix.Invert;
          end;

      else
        // --------------(Example 0, Identity matrix)
      end;

      Interpolator := TAggSpanInterpolatorLinear.Create(ImageMatrix);
      SpanAllocator := TAggSpanAllocator.Create;

      Rgba.FromRgbaDouble(1, 1, 1, 0);

      // nearest neighbor
      { sg := TAggSpanImageFilterRgbaNN.Create(sa, RenderingBufferImage[0],
          @rgba, interpolator, CAggOrderBgra));{ }

      // "hardcoded" bilinear filter
      Sg := TAggSpanImageFilterRgbaBilinear.Create(SpanAllocator,
        RenderingBufferImage[0], @Rgba, Interpolator, CAggOrderBgra); { }

      // arbitrary filter
      { fi     := TAggImageFilterSpline36.Create
        filter := TAggImageFilter.Create(fi);

        sg := TAggSpanImageFilterRgba.Create(sa, RenderingBufferImage[0],
          @rgba, @interpolator, filter, CAggOrderBgra));{ }

      // Render
      Rsi := TAggRendererScanLineAA.Create(RendererBase, Sg);
      try
        Ras := TAggRasterizerScanLineAA.Create;
        Sl := TAggScanLineUnpacked8.Create;
        Ps := TAggPathStorage.Create;

        BuildStar(Ps);

        ConvTransform := TAggConvTransform.Create(Ps, PolygonMatrix);
        try
          Ras.AddPath(ConvTransform);
        finally
          ConvTransform.Free;
        end;
        RenderScanLines(Ras, Sl, Rsi);
      finally
        Rsi.Free;
      end;

      Circle[0] := TAggCircle.Create(FImageC, 5, 20);
      Circle[1] := TAggCircle.Create(FImageC, 2, 20);
      C1 := TAggConvStroke.Create(Circle[0]);

      Rgba.FromRgbaDouble(0.7, 0.8, 0);
      RenScan.SetColor(@Rgba);
      Ras.AddPath(Circle[0]);
      RenderScanLines(Ras, Sl, RenScan);

      RenScan.SetColor(CRgba8Black);
      Ras.AddPath(C1);
      RenderScanLines(Ras, Sl, RenScan);

      Ras.AddPath(Circle[1]);
      RenderScanLines(Ras, Sl, RenScan);

      // Render the controls
      RenderControl(Ras, Sl, RenScan, FSliderPolygonAngle);
      RenderControl(Ras, Sl, RenScan, FSliderPolygonScale);
      RenderControl(Ras, Sl, RenScan, FSliderImageAngle);
      RenderControl(Ras, Sl, RenScan, FSliderImageScale);
      RenderControl(Ras, Sl, RenScan, FCheckBoxRotatePolygon);
      RenderControl(Ras, Sl, RenScan, FCheckBoxRotateImage);
      RenderControl(Ras, Sl, RenScan, FRadioBoxExample);

      // Free AGG resources
      Ras.Free;
      Sl.Free;
      Ps.Free;
      C1.Free;
      Circle[0].Free;
      Circle[1].Free;

      SpanAllocator.Free;
      ImageMatrix.Free;
      PolygonMatrix.Free;
      Interpolator.Free;

      if Assigned(Sg) then
        Sg.Free;

  (*
      if Assigned(Fi) then
        Fi.Free;
  *)

      if Assigned(Filter) then
        Filter.Free;

    finally
      RenScan.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    if FFlag = 1 then
    begin
      FImageC.X := X - FDelta.X;
      FImageC.Y := Y - FDelta.Y;

      ForceRedraw;
    end;

    if FFlag = 2 then
    begin
      FPolygonC.X := X - FDelta.X;
      FPolygonC.Y := Y - FDelta.Y;

      ForceRedraw;
    end;

  end
  else
    OnMouseButtonUp(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
var
  Ras: TAggRasterizerScanLineAA;
  Ps: TAggPathStorage;
  Tr: TAggConvTransform;

  PolygonMatrix: TAggTransAffine;
begin
  if mkfMouseLeft in Flags then
    if Sqrt((X - FImageC.X) * (X - FImageC.X) + (Y - FImageC.Y) *
      (Y - FImageC.Y)) < 5 then
    begin
      FDelta.X := X - FImageC.X;
      FDelta.Y := Y - FImageC.Y;

      FFlag := 1;
    end
    else
    begin
      Ras := TAggRasterizerScanLineAA.Create;
      try
        Ps := TAggPathStorage.Create;
        try
          BuildStar(Ps);

          PolygonMatrix := TAggTransAffine.Create;
          try
            PolygonMatrix.Translate(-FPolygonC.x, -FPolygonC.y);
            PolygonMatrix.Rotate(Deg2Rad(FSliderPolygonAngle.Value));

            PolygonMatrix.Scale(FSliderPolygonScale.Value);
            PolygonMatrix.Translate(FPolygonC.X, FPolygonC.Y);

            Tr := TAggConvTransform.Create(Ps, PolygonMatrix);
            try
              Ras.AddPath(Tr);
            finally
              Tr.Free;
            end;
          finally
            PolygonMatrix.Free;
          end;

          if Ras.HitTest(X, Y) then
          begin
            FDelta.X := X - FPolygonC.X;
            FDelta.Y := Y - FPolygonC.Y;

            FFlag := 2;
          end;

        finally
          Ps.Free;
        end;
      finally
        Ras.Free;
      end;
    end;
end;

procedure TAggApplication.OnMouseButtonUp(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  FFlag := 0;
end;

procedure TAggApplication.OnControlChange;
begin
  if FCheckBoxRotatePolygon.Status or FCheckBoxRotateImage.Status then
    WaitMode := False
  else
    WaitMode := True;

  ForceRedraw;
end;

procedure TAggApplication.OnIdle;
var
  Redraw: Boolean;
begin
  Redraw := False;

  if FCheckBoxRotatePolygon.Status then
  begin
    FSliderPolygonAngle.Value := FSliderPolygonAngle.Value + 0.5;

    if FSliderPolygonAngle.Value >= 180 then
      FSliderPolygonAngle.Value := FSliderPolygonAngle.Value - 360;

    Redraw := True;
  end;

  if FCheckBoxRotateImage.Status then
  begin
    FSliderImageAngle.Value := FSliderImageAngle.Value + 0.5;

    if FSliderImageAngle.Value >= 180 then
      FSliderImageAngle.Value := FSliderImageAngle.Value - 360;

    Redraw := True;
  end;

  if Redraw then
    ForceRedraw;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('Affine transformations of the images. The examples '
      + 'demonstrates how to construct the affine transformer matrix for '
      + 'different cases. See the "image_transforms.txt" file for details. '
      + 'Now there are methods in TAggTransAffine that alLow you to construct '
      + 'transformations from an arbitrary parallelogram to another '
      + 'parallelogram. It''s very convenient and easy.'#13#13
      + 'How to play with:'#13#13
      + 'Use the left mouse button change the centre of rotation or move '
      + 'the polygon shape.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s '
      + 'directory.');
end;

var
  Text: AnsiString;

  ImageName, P, N, X: ShortString;
begin
  ImageName := 'spheres';

{$IFDEF WIN32}
  if ParamCount > 0 then
  begin
    SpreadName(ParamStr(1), P, N, X);

    ImageName := FoldName(P, N, '');
  end;
{$ENDIF}

  with TAggApplication.Create(pfBgra32, CFlipY) do
  try
    Caption := 'Image Affine Transformations with filtering (F1-Help)';

    if not LoadImage(0, ImageName) then
    begin
      Text := 'File not found: ' + ImageName + ImageExtension;
      if ImageName = 'spheres' then
        Text := Text + #13#13 + 'Download http://www.antigrain.com/'
          + ImageName + ImageExtension + #13 + 'or copy it from another ' +
          'directory if available.';

      DisplayMessage(Text);
    end
    else if Init(RenderingBufferImage[0].Width, RenderingBufferImage[0].Height,
      []) then
      Run;
  finally
    Free
  end;
end.
