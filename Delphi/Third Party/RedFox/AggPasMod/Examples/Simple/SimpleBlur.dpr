program SimpleBlur;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',

  AggColor in '..\..\Source\AggColor.pas',
  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgb in '..\..\Source\AggPixelFormatRgb.pas',

  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRendererOutlineAA in '..\..\Source\AggRendererOutlineAA.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggRasterizerOutlineAA in '..\..\Source\AggRasterizerOutlineAA.pas',
  AggScanLine in '..\..\Source\AggScanLine.pas',
  AggScanlineUnpacked in '..\..\Source\AggScanlineUnpacked.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas',

  AggEllipse in '..\..\Source\AggEllipse.pas',
  AggPathStorage in '..\..\Source\AggPathStorage.pas',
  AggConvStroke in '..\..\Source\AggConvStroke.pas',
  AggConvTransform in '..\..\Source\AggConvTransform.pas',
  AggBoundingRect in '..\..\Source\AggBoundingRect.pas',
  AggSpanGenerator in '..\..\Source\AggSpanGenerator.pas',
  AggTransAffine in '..\..\Source\AggTransAffine.pas',
  AggSpanAllocator in '..\..\Source\AggSpanAllocator.pas',
  AggParseLion;

const
  CFlipY = True;

type
  TAggSpanSimpleBlurRgb24 = class(TAggSpanGenerator)
  private
    FSourceImage: TAggRenderingBuffer;
  public
    constructor Create(Alloc: TAggSpanAllocator); overload;
    constructor Create(Alloc: TAggSpanAllocator;
      Src: TAggRenderingBuffer); overload;

    procedure SetSourceImage(Src: TAggRenderingBuffer);
    function GetSourceImage: TAggRenderingBuffer;

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

  TAggApplication = class(TPlatformSupport)
  private
    FPath: TAggPathStorage;
    FColors: array [0..99] of TAggColor;
    FPathIndex: array [0..99] of Cardinal;

    FPathCount: Cardinal;

    FBoundingRect: TRectDouble;
    FAngle, FScale: Double;
    FBaseDelta, FSkew: TPointDouble;
    FCenter: TPointDouble;
  protected
    procedure ParseLion;
  public
    constructor Create(PixelFormat: TPixelFormat; FlipY: Boolean);
    destructor Destroy; override;

    procedure OnDraw; override;

    procedure OnMouseMove(X, Y: Integer; Flags: TMouseKeyboardFlags); override;
    procedure OnMouseButtonDown(X, Y: Integer; Flags: TMouseKeyboardFlags);
      override;

    procedure OnKey(X, Y: Integer; Key: Cardinal; Flags: TMouseKeyboardFlags);
      override;
  end;


{ TAggSpanSimpleBlurRgb24 }

constructor TAggSpanSimpleBlurRgb24.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);

  FSourceImage := nil;
end;

constructor TAggSpanSimpleBlurRgb24.Create(Alloc: TAggSpanAllocator;
  Src: TAggRenderingBuffer);
begin
  inherited Create(Alloc);

  FSourceImage := Src;
end;

procedure TAggSpanSimpleBlurRgb24.SetSourceImage;
begin
  FSourceImage := Src;
end;

function TAggSpanSimpleBlurRgb24.GetSourceImage;
begin
  Result := FSourceImage;
end;

function TAggSpanSimpleBlurRgb24.Generate;
var
  Span : PAggColor;
  Color: array [0..3] of Integer;

  I  : Integer;
  Ptr: PInt8u;

begin
  Span := Allocator.Span;

  if (Y < 1) or (Y >= FSourceImage.Height - 1) then
  begin
    repeat
      Span.Clear;

      Inc(PtrComp(Span), SizeOf(TAggColor));
      Dec(Len);
    until Len = 0;

    Result := Allocator.Span;

    Exit;
  end;

  repeat
    Color[0] := 0;
    Color[1] := 0;
    Color[2] := 0;
    Color[3] := 0;

    if (X > 0) and (X < FSourceImage.Width - 1) then
    begin
      I := 3;

      repeat
        Ptr := PInt8u(Integer(FSourceImage.Row(Y - I + 2)) + ((X - 1) * 3)
          * SizeOf(Int8u));

        Inc(Color[0], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[1], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[2], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[3], 255);

        Inc(Color[0], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[1], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[2], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[3], 255);

        Inc(Color[0], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[1], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[2], Ptr^);
        Inc(PtrComp(Ptr), SizeOf(Int8u));
        Inc(Color[3], 255);

        Dec(I);
      until I = 0;

      Color[0] := Color[0] div 9;
      Color[1] := Color[1] div 9;
      Color[2] := Color[2] div 9;
      Color[3] := Color[3] div 9;
    end;

    Span.FromRgbaInteger(Color[2], Color[1], Color[0], Color[3]);

    Inc(PtrComp(Span), SizeOf(TAggColor));
    Inc(X);
    Dec(Len);

  until Len = 0;

  Result := Allocator.Span;
end;


{ TAggApplication }

constructor TAggApplication.Create(PixelFormat: TPixelFormat; FlipY: Boolean);
begin
  inherited Create(PixelFormat, FlipY);

  FCenter.X := 100;
  FCenter.Y := 102;

  FPath := TAggPathStorage.Create;

  FPathCount := 0;

  FBoundingRect.X1 := 0;
  FBoundingRect.Y1 := 0;
  FBoundingRect.X2 := 0;
  FBoundingRect.Y2 := 0;

  FBaseDelta.X := 0;
  FBaseDelta.Y := 0;

  FAngle := 0;
  FScale := 1.0;

  FSkew.X := 0;
  FSkew.Y := 0;

  ParseLion;
end;

destructor TAggApplication.Destroy;
begin
  FPath.Free;
  inherited;
end;

procedure TAggApplication.ParseLion;
begin
  FPathCount := AggParseLion.ParseLion(FPath, @FColors, @FPathIndex);

  BoundingRect(FPath, @FPathIndex, 0, FPathCount, FBoundingRect);

  FBaseDelta.X := FBoundingRect.CenterX;
  FBaseDelta.Y := FBoundingRect.CenterY;
end;

procedure TAggApplication.OnDraw;
var
  PixelFormatProcessor: TAggPixelFormatProcessor;

  RendererBase: TAggRendererBase;
  RendererScanLine: TAggRendererScanLineAASolid;

  Rgba : TAggColor;
  Trans: TAggConvTransform;

  Mtx, Inv: TAggTransAffine;

  Rasterizer: TAggRasterizerOutlineAA;
  RasterizerScanLine: TAggRasterizerScanLineAA;
  ScanLinePacked: TAggScanLinePacked8;
  ScanLineUnpacked: TAggScanLineUnpacked8;

  Profile: TAggLineProfileAA;

  RendererOutline: TAggRendererOutlineAA;

  Ellipse: TAggEllipse;

  EllipseStroke: array [0..1] of TAggConvStroke;

  SpanAllocator: TAggSpanAllocator;
  SpanSimpleBlur: TAggSpanSimpleBlurRgb24;

  RendererBlur: TAggRendererScanLineAA;
begin
  // Initialize structures
  PixelFormatBgr24(PixelFormatProcessor, RenderingBufferWindow);

  RendererBase := TAggRendererBase.Create(PixelFormatProcessor, True);
  try
    RendererScanLine := TAggRendererScanLineAASolid.Create(RendererBase);
    try
      RendererBase.Clear(CRgba8White);

      RasterizerScanLine := TAggRasterizerScanLineAA.Create;
      try

        Mtx := TAggTransAffine.Create;
        try
          Trans := TAggConvTransform.Create(FPath, Mtx);

          Mtx.Translate(-FBaseDelta.X, -FBaseDelta.Y);
          Mtx.Scale(FScale, FScale);
          Mtx.Rotate(FAngle + Pi);
          Mtx.Skew(FSkew.X * 1E-3, FSkew.Y * 1E-3);
          Mtx.Translate(FInitialWidth * 0.125, FInitialHeight * 0.5);
          Mtx.Multiply(GetTransAffineResizing);

          ScanLinePacked := TAggScanLinePacked8.Create;
          ScanLineUnpacked := TAggScanLineUnpacked8.Create;

          // Full lion
          RenderAllPaths(RasterizerScanLine, ScanLinePacked, RendererScanLine,
            Trans, @FColors, @FPathIndex, FPathCount);

          // Outline Lion
          Inv := TAggTransAffine.Create;
          try
            Inv.Assign(GetTransAffineResizing);

            Inv.Invert;
            Mtx.Multiply(Inv);
          finally
            Inv.Free;
          end;

          Mtx.Translate(FInitialWidth * 0.5, 0);
          Mtx.Multiply(GetTransAffineResizing);

          Profile := TAggLineProfileAA.Create;
          Profile.SetWidth(1.0);

          RendererOutline := TAggRendererOutlineAA.Create(RendererBase, Profile);
          Rasterizer := TAggRasterizerOutlineAA.Create(RendererOutline);

          Rasterizer.RoundCap := True;
          Rasterizer.AccurateJoin := True;

          Rasterizer.RenderAllPaths(Trans, @FColors, @FPathIndex, FPathCount);
        finally
          Mtx.Free;
        end;

        // Ellipse
        Ellipse := TAggEllipse.Create(FCenter.X, FCenter.Y, 100, 100, 100);

        EllipseStroke[0] := TAggConvStroke.Create(Ellipse);
        EllipseStroke[0].Width := 6.0;
        EllipseStroke[1] := TAggConvStroke.Create(EllipseStroke[0]);
        EllipseStroke[1].Width := 2.0;

        Rgba.FromRgbaDouble(0, 0.2, 0);
        RendererScanLine.SetColor(@Rgba);

        RasterizerScanLine.AddPath(EllipseStroke[1]);
        RenderScanLines(RasterizerScanLine, ScanLinePacked, RendererScanLine);

        // Blur
        SpanAllocator := TAggSpanAllocator.Create;
        SpanSimpleBlur := TAggSpanSimpleBlurRgb24.Create(SpanAllocator);

        RendererBlur := TAggRendererScanLineAA.Create(RendererBase,
          SpanSimpleBlur);
        SpanSimpleBlur.SetSourceImage(RenderingBufferImage[0]);

        RasterizerScanLine.AddPath(Ellipse);

        CopyWindowToImage(0);
        RenderScanLines(RasterizerScanLine, ScanLineUnpacked, RendererBlur);
      finally
        RasterizerScanLine.Free;
      end;

      // More blur if desired :-)
      { CopyWindowToImage(0);
        RenderScanLines(@ras2, ScanLineUnpacked, rblur);

        CopyWindowToImage(0);
        RenderScanLines(@ras2, ScanLineUnpacked, rblur);

        CopyWindowToImage(0);
        RenderScanLines(@ras2, ScanLineUnpacked, rblur);{ }

      // Free AGG resources
      ScanLinePacked.Free;
      ScanLineUnpacked.Free;

      SpanSimpleBlur.Free;
      RendererOutline.Free;
      Rasterizer.Free;
      Profile.Free;
      Ellipse.Free;
      Trans.Free;
      RendererBlur.Free;
      EllipseStroke[0].Free;
      EllipseStroke[1].Free;
      SpanAllocator.Free;
    finally
      RendererScanLine.Free;
    end;
  finally
    RendererBase.Free;
  end;
end;

procedure TAggApplication.OnMouseMove(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  OnMouseButtonDown(X, Y, Flags);
end;

procedure TAggApplication.OnMouseButtonDown(X, Y: Integer;
  Flags: TMouseKeyboardFlags);
begin
  if mkfMouseLeft in Flags then
  begin
    FCenter.X := X;
    FCenter.Y := Y;

    ForceRedraw;
  end;
end;

procedure TAggApplication.OnKey(X, Y: Integer; Key: Cardinal;
  Flags: TMouseKeyboardFlags);
begin
  if Key = Cardinal(kcF1) then
    DisplayMessage('The example demonstrates how to write custom span '
      + 'generators. This one just applies the simplest "blur" filter 3x3 to a '
      + 'prerendered image. It calculates the average value of 9 neighbor '
      + 'pixels.'#13#13
      + 'How to play with:'#13#13
      + 'Just press the left mouse button and drag.'#13
      + 'Uncomment and recompile the part of the demo source code to get '
      + 'more blur.'#13#13
      + 'Note: F2 key saves current "screenshot" file in this demo''s'
      + 'directory.');
end;

begin
  with TAggApplication.Create(pfBgr24, CFlipY) do
  try
    Caption := 'AGG Example. Lion with Alpha-Masking (F1-Help)';
    if Init(512, 400, [wfResize]) then
      Run;
  finally
    Free;
  end;
end.
