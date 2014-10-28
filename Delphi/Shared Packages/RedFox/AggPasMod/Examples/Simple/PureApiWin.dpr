program PureApiWin;

// AggPas 2.4 RM3 Demo application
// Note: Press F1 key on run to see more info about this demo

{$I AggCompiler.inc}

uses
  {$IFDEF USE_FASTMM4}
  FastMM4,
  {$ENDIF}
  Windows,
  Messages,

  AggPlatformSupport, // please add the path to this file manually

  AggBasics in '..\..\Source\AggBasics.pas',
  AggRenderingBuffer in '..\..\Source\AggRenderingBuffer.pas',
  AggColor in '..\..\Source\AggColor.pas',

  AggPixelFormat in '..\..\Source\AggPixelFormat.pas',
  AggPixelFormatRgba in '..\..\Source\AggPixelFormatRgba.pas',
  AggRendererBase in '..\..\Source\AggRendererBase.pas',
  AggRendererScanLine in '..\..\Source\AggRendererScanLine.pas',
  AggRasterizerScanLineAA in '..\..\Source\AggRasterizerScanLineAA.pas',
  AggScanLinePacked in '..\..\Source\AggScanLinePacked.pas',
  AggRenderScanLines in '..\..\Source\AggRenderScanLines.pas';

const
  SzWindowClass = 'PURE_API';
  SzTitle = 'pure_api';

function WndProc(Wnd: HWND; Msg: UINT; WPar: WParam; LPar: LParam): LResult;
  stdcall;
var
  // Win32
  Dc: HDC;
  Ps: TPaintStruct;
  Rt: TRect;

  Width, Height: Integer;

  BmpInfo: TBitmapInfo;
  MemDC: HDC;

  Buf      : Pointer;
  Bmp, Temp: HBitmap;

  // AGG
  RenderingBuffer: TAggRenderingBuffer;
  PixelFormat: TAggPixelFormatProcessor;
  RendererBase: TAggRendererBase;
  Rgba: TAggColor;

  Renderer: TAggRendererScanLineAASolid;
  Rasterizer: TAggRasterizerScanLineAA;
  ScanLine : TAggScanLinePacked8;
begin
  Result := 0;

  case Msg of
    WM_PAINT:
      begin
        Dc := BeginPaint(Wnd, Ps);

        if Dc <> 0 then
        begin
          GetClientRect(Wnd, Rt);

          Width := Rt.Right - Rt.Left;
          Height := Rt.Bottom - Rt.Top;

          // Creating compatible DC and a bitmap to render the image
          MemDC := CreateCompatibleDC(Dc);

          BmpInfo.BmiHeader.BiSize := SizeOf(TBITMAPINFOHEADER);
          BmpInfo.BmiHeader.BiWidth := Width;
          BmpInfo.BmiHeader.BiHeight := Height;
          BmpInfo.BmiHeader.BiPlanes := 1;

          BmpInfo.BmiHeader.BiBitCount := 32;
          BmpInfo.BmiHeader.BiCompression := BI_RGB;
          BmpInfo.BmiHeader.BiSizeImage := 0;
          BmpInfo.BmiHeader.BiXPelsPerMeter := 0;
          BmpInfo.BmiHeader.BiYPelsPerMeter := 0;
          BmpInfo.BmiHeader.BiClrUsed := 0;
          BmpInfo.BmiHeader.BiClrImportant := 0;

          Buf := nil;
          Bmp := CreateDIBSection(MemDC, BmpInfo, DIB_RGB_COLORS, Buf, 0, 0);

          // Selecting the object before doing anything allows you
          // to use AGG together with native Windows GDI.
          Temp := SelectObject(MemDC, Bmp);

          // ============================================================
          // AGG Lowest level code
          RenderingBuffer := TAggRenderingBuffer.Create;
          RenderingBuffer.Attach(Buf, Width, Height, -Width * 4);
          // Use negative stride in order
          // to keep Y-axis consistent with
          // WinGDI, i.e., going down.

          // Pixel format and basic primitives Renderer
          PixelFormatBgra32(PixelFormat, RenderingBuffer);
          try
            RendererBase := TAggRendererBase.Create(PixelFormat);
            try
              Rgba.FromRgbaInteger(255, 255, 255, 127);
              RendererBase.Clear(@Rgba);

              // ScanLine Renderer for solid filling
              Renderer := TAggRendererScanLineAASolid.Create(RendererBase);
              try
                // Rasterizer & ScanLine
                Rasterizer := TAggRasterizerScanLineAA.Create;
                try
                  ScanLine := TAggScanLinePacked8.Create;
                  try
                    // Polygon (triangle)
                    Rasterizer.MoveToDouble(20.7, 34.15);
                    Rasterizer.LineToDouble(398.23, 123.43);
                    Rasterizer.LineToDouble(165.45, 401.87);

                    // Setting the attrribute (color) & Rendering
                    Rgba.FromRgbaInteger(80, 90, 60);
                    Renderer.SetColor(@Rgba);

                    RenderScanLines(Rasterizer, ScanLine, Renderer);

                    // -------------------------------------------------------
                    // Display the image. If the image is B-G-R-A (32-bits per
                    // pixel) one can use AlphaBlend instead of BitBlt. In
                    // case of AlphaBlend one also should clear the image with
                    //  zero alpha, i.e. rgba8(0,0,0,0)
                    BitBlt(Dc, Rt.Left, Rt.Top, Width, Height, MemDC, 0, 0,
                      SRCCOPY);
                  finally
                    ScanLine.Free;
                  end;
                finally
                  Rasterizer.Free;
                end;
              finally
                Renderer.Free;
              end;

              // Free resources
              SelectObject(MemDC, Temp);
              DeleteObject(Bmp);
              DeleteObject(MemDC);

              EndPaint(Wnd, Ps);
            finally
              RendererBase.Free;
            end;
          finally
            RenderingBuffer.Free;
            PixelFormat.Free;
          end;
        end;
      end;

    WM_SYSKEYDOWN, WM_KEYDOWN:
      case WPar of
        VK_F1:
          MessageBox(Wnd,
            'The AGG library is able to draw to any surface. It is achieved by '
            + 'using an offline bitmap (buffer), to which AGG primarily '
            + 'renders. Then that bitmap is blited to the GDI device context '
            + 'of the destination device.'#13#13
            + 'This example demonstrates that simple setup for a Windows app '
            + 'that paints to a GDI device context. All it needs are the '
            + 'CreateCompatibleBitmap(), CreateDIBSection() and BitBlt() '
            + 'WinAPI calls.'#0, 'AGG Message', MB_OK);

      else
        Result := DefWindowProc(Wnd, Msg, WPar, LPar);
      end;

    WM_ERASEBKGND:
      NoP;

    WM_DESTROY:
      PostQuitMessage(0);

  else
    Result := DefWindowProc(Wnd, Msg, WPar, LPar);
  end;
end;

procedure MyRegisterClass;
var
  WndClEx: TWndClassEx;
begin
  WndClEx.CbSize := SizeOf(TWndClassEx);

  WndClEx.Style := CS_HREDRAW or CS_VREDRAW;
  WndClEx.LpfnWndProc := @WndProc;
  WndClEx.CbClsExtra := 0;
  WndClEx.CbWndExtra := 0;
  WndClEx.HInstance := HInstance;
  WndClEx.HIcon := LoadIcon(HInstance, IDI_APPLICATION);
  WndClEx.HCursor := LoadCursor(0, IDC_ARROW);
  WndClEx.HbrBackground := COLOR_WINDOW + 1;
  WndClEx.LpszMenuName := nil;
  WndClEx.LpszClassName := SzWindowClass;
  WndClEx.HIconSm := 0;

  RegisterClassEx(WndClEx);
end;

function InitInstance: Boolean;
var
  Wnd: HWND;
begin
  Wnd := CreateWindow(SzWindowClass, SzTitle, WS_OVERLAPPEDWINDOW,
    Integer(CW_USEDEFAULT), 0, Integer(CW_USEDEFAULT), 0, 0, 0, HInstance, nil);

  if Wnd <> 0 then
  begin
    ShowWindow(Wnd, SW_SHOW);
    UpdateWindow(Wnd);

    Result := True;
  end
  else
    Result := False;
end;

var
  Msg: TMsg;

begin
  MyRegisterClass;

  if InitInstance then
    while GetMessage(Msg, 0, 0, 0) do
    begin
      TranslateMessage(Msg);
      DispatchMessage(Msg);
    end;
end.
