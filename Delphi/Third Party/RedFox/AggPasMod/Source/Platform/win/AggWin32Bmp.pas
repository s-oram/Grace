unit AggWin32Bmp;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012                                                      //
//                                                                            //
//  Based on:                                                                 //
//    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
//    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
//                                                                            //
//  Original License:                                                         //
//    Anti-Grain Geometry - Version 2.4 (Public License)                      //
//    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
//    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
//                                                                            //
//  Permission to copy, use, modify, sell and distribute this software        //
//  is granted provided this copyright notice appears in all copies.          //
//  This software is provided "as is" without express or implied              //
//  warranty, and with no claim as to its suitability for any purpose.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  Windows,
  SysUtils,
  AggBasics;

const
  COrgMono8 = 8;
  COrgColor16 = 16;
  COrgColor24 = 24;
  COrgColor32 = 32;
  COrgColor48 = 48;
  COrgColor64 = 64;

type
  TPixelMap = class
  private
    FBitmapInfo: PBitmapInfo;
    FBuffer: Pointer;

    function CalculateImagePointer(Bmp: PBITMAPINFO): PtrComp;
    function CalculateFullSize(Bmp: PBITMAPINFO): Cardinal;

    function CalculatePaletteSize(AColorUsed, ABitsPerPixel: Cardinal): Cardinal;
      overload;
    function CalculatePaletteSize(Bmp: PBITMAPINFO): Cardinal; overload;

    function CalculateRowLen(AWidth, ABitsPerPixel: Cardinal): Cardinal;

    function GetBuffer: Pointer;
    function GetWidth: Cardinal;
    function GetHeight: Cardinal;
    function GetStride: Integer;
    function GetBitsPerPixel: Cardinal;
    procedure SetHeight(const Value: Cardinal);
    procedure SetWidth(const Value: Cardinal);
  protected
    FBitsPerPixel: Cardinal;
    FIsInternal: Boolean;

    FImageSize, FFullSize: Cardinal;

    function CalculateHeaderSize(Bmp: PBITMAPINFO): Cardinal;

    function CreateBitmapInfo(Width, Height, BitsPerPixel: Cardinal): PBITMAPINFO;
    procedure CreateGrayScalePalette(Bmp: PBITMAPINFO);

    procedure CreateFromBitmap(Bmp: PBITMAPINFO);

    procedure FreeBitmap;

    property BitmapInfo: PBitmapInfo read FBitmapInfo;
  public
    constructor Create;
    destructor Destroy; override;



    procedure Assign(Source: TPixelMap); virtual;
    procedure Build(AWidth, AHeight, Org: Cardinal; AClearVal: Cardinal = 256);
    procedure SetSize(Width, Height: Cardinal);

    function LoadFromBitmap(var Fd: file): Boolean; overload;
    function LoadFromBitmap(Filename: TFileName): Boolean; overload;

    function SaveAsBitmap(var Fd: file): Boolean; overload;
    function SaveAsBitmap(Filename: TFileName): Boolean; overload;

    procedure Draw(HandleDC: HDC; DeviceRect: PRect = nil; BitmapRect: PRect = nil); overload;
    procedure Draw(HandleDC: HDC; X, Y: Integer; Scale: Double = 1.0); overload;

    property Buffer: Pointer read GetBuffer;
    property Width: Cardinal read GetWidth write SetWidth;
    property Height: Cardinal read GetHeight write SetHeight;
    property Stride: Integer read GetStride;
    property BitsPerPixel: Cardinal read GetBitsPerPixel;
  end;

implementation

{ TPixelMap }

constructor TPixelMap.Create;
begin
  FBitmapInfo := nil;
  FBuffer := nil;
  FBitsPerPixel := 0;

  FIsInternal := False;

  FImageSize := 0;
  FFullSize := 0;
end;

destructor TPixelMap.Destroy;
begin
  FreeBitmap;
  inherited;
end;

procedure TPixelMap.FreeBitmap;
begin
  if (FBitmapInfo <> nil) and FIsInternal then
    AggFreeMem(Pointer(FBitmapInfo), FFullSize);

  FBitmapInfo := nil;
  FBuffer := nil;

  FIsInternal := False;
end;

procedure TPixelMap.Assign(Source: TPixelMap);
begin
  with Source do
  begin
    Self.FBitmapInfo^ := FBitmapInfo^;
    Self.FBitsPerPixel := FBitsPerPixel;
    Self.FIsInternal := FIsInternal;
    Self.FImageSize := FImageSize;
    Self.FFullSize := FFullSize;

    Move(FBuffer^, Self.FBuffer^, FImageSize);
  end;
end;

procedure TPixelMap.Build(AWidth, AHeight, Org: Cardinal;
  AClearVal: Cardinal = 256);
begin
  FreeBitmap;

  if AWidth = 0 then
    AWidth := 1;

  if AHeight = 0 then
    AHeight := 1;

  FBitsPerPixel := Org;

  CreateFromBitmap(CreateBitmapInfo(AWidth, AHeight, FBitsPerPixel));

  CreateGrayScalePalette(FBitmapInfo);

  FIsInternal := True;

  Assert(Assigned(FBuffer));
  if AClearVal <= 255 then
    FillChar(FBuffer^, FImageSize, AClearVal);
end;

function TPixelMap.LoadFromBitmap(var Fd: file): Boolean;
var
  Sz: Integer;

  Bmf: BITMAPFILEHEADER;
  Bmi: PBITMAPINFO;

  BmpSize: Cardinal;
begin
  BlockRead(Fd, Bmf, SizeOf(Bmf));

  try
    if Bmf.BfType <> $4D42 then
      raise Exception.Create('Bitmap magic not found');

    BmpSize := Bmf.BfSize - SizeOf(BITMAPFILEHEADER);

    AggGetMem(Pointer(Bmi), BmpSize);
    BlockRead(Fd, Bmi^, BmpSize, Sz);

    if Sz <> BmpSize then
      raise Exception.Create('Bitmap size mismatch');

    FreeBitmap;

    FBitsPerPixel := Bmi.BmiHeader.BiBitCount;

    CreateFromBitmap(Bmi);

    FIsInternal := True;
    Result := True;
  except
    if Bmi <> nil then
      AggFreeMem(Pointer(Bmi), BmpSize);

    Result := False;
  end;
end;

function TPixelMap.LoadFromBitmap(Filename: TFileName): Boolean;
var
  Fd : file;
  Err: Integer;
  Ret: Boolean;
begin
{$I- }
  Err := IoResult;

  AssignFile(Fd, Filename);
  Reset(Fd, 1);

  Err := IoResult;
  Ret := False;

  if Err = 0 then
  begin
    Ret := LoadFromBitmap(Fd);

    Close(Fd);
  end;

  Result := Ret;
end;

function TPixelMap.SaveAsBitmap(var Fd: file): Boolean;
var
  Bmf: BITMAPFILEHEADER;
begin
  if FBitmapInfo = nil then
    Result := False
  else
  begin
    Bmf.BfType := $4D42;
    Bmf.BfOffBits := CalculateHeaderSize(FBitmapInfo) + SizeOf(Bmf);
    Bmf.BfSize := Bmf.BfOffBits + FImageSize;
    Bmf.BfReserved1 := 0;
    Bmf.BfReserved2 := 0;

    Blockwrite(Fd, Bmf, SizeOf(Bmf));
    Blockwrite(Fd, FBitmapInfo^, FFullSize);

    Result := True;
  end;
end;

function TPixelMap.SaveAsBitmap(Filename: TFileName): Boolean;
var
  Fd : file;
  Err: Integer;
  Ret: Boolean;
begin
{$I- }
  Err := IoResult;

  AssignFile(Fd, Filename);
  Rewrite(Fd, 1);

  Err := IoResult;
  Ret := False;

  if Err = 0 then
  begin
    Ret := SaveAsBitmap(Fd);

    Close(Fd);
  end;

  Result := Ret;
end;

procedure TPixelMap.SetSize(Width, Height: Cardinal);
begin
  if (Width <> Self.Width) or (Height <> Self.Height) then
    Build(Width, Height, FBitsPerPixel);
end;

procedure TPixelMap.SetHeight(const Value: Cardinal);
begin
  if Height <> Value then
    Build(Width, Value, FBitsPerPixel);
end;

procedure TPixelMap.SetWidth(const Value: Cardinal);
begin
  if Width <> Value then
    Build(Value, Height, FBitsPerPixel);
end;

procedure TPixelMap.Draw(HandleDC: HDC; DeviceRect: PRect = nil;
  BitmapRect: PRect = nil);
var
  BmpX, BmpY, BmpWidth, BmpHeight, DvcX, DvcY: Cardinal;
  DvcWidth, DvcHeight: Cardinal;

  Err: Integer;

  CompDC: HDC;
  Handle, Backup: HBITMAP;
  BmInfo: TBitmapInfo;
  Buffer: Pointer;

  Rinc, Size, Stride: Integer;
begin
  if (FBitmapInfo = nil) or (FBuffer = nil) then
    Exit;

  BmpX := 0;
  BmpY := 0;

  BmpWidth := FBitmapInfo.BmiHeader.BiWidth;
  BmpHeight := FBitmapInfo.BmiHeader.BiHeight;

  DvcX := 0;
  DvcY := 0;

  DvcWidth := FBitmapInfo.BmiHeader.BiWidth;
  DvcHeight := FBitmapInfo.BmiHeader.BiHeight;

  if BitmapRect <> nil then
  begin
    BmpX := BitmapRect.Left;
    BmpY := BitmapRect.Top;
    BmpWidth := BitmapRect.Right - BitmapRect.Left;
    BmpHeight := BitmapRect.Bottom - BitmapRect.Top;
  end;

  DvcX := BmpX;
  DvcX := BmpX;
  DvcWidth := BmpWidth;
  DvcHeight := BmpHeight;

  if DeviceRect <> nil then
  begin
    DvcX := DeviceRect.Left;
    DvcY := DeviceRect.Top;
    DvcWidth := DeviceRect.Right - DeviceRect.Left;
    DvcHeight := DeviceRect.Bottom - DeviceRect.Top;
  end;

  if (DvcWidth <> BmpWidth) or (DvcHeight <> BmpHeight) then
  begin
    SetStretchBltMode(HandleDC, COLORONCOLOR);

    StretchDIBits(HandleDC, // handle of device context
      DvcX, // x-coordinate of upper-left corner of source rect.
      DvcY, // y-coordinate of upper-left corner of source rect.
      DvcWidth, // width of source rectangle
      DvcHeight, // height of source rectangle
      BmpX, BmpY, // x, y -coordinates of upper-left corner of dest. rect.
      BmpWidth, // width of destination rectangle
      BmpHeight, // height of destination rectangle
      FBuffer, // address of bitmap bits
      FBitmapInfo^, // address of bitmap data
      DIB_RGB_COLORS, // usage
      SRCCOPY); // raster operation code
  end
  else
  begin
    Err := SetDIBitsToDevice(HandleDC, // handle to device context
      DvcX, // x-coordinate of upper-left corner of
      DvcY, // y-coordinate of upper-left corner of
      DvcWidth, // source rectangle width
      DvcHeight, // source rectangle height
      BmpX, // x-coordinate of Lower-left corner of
      BmpY, // y-coordinate of Lower-left corner of
      0, // first scan line in array
      BmpHeight, // number of scan lines
      FBuffer, // address of array with DIB bits
      FBitmapInfo^, // address of structure with bitmap info.
      DIB_RGB_COLORS); // RGB or palette indexes

    { hack }
    if Err = 0 then
    begin
      CompDC := CreateCompatibleDC(HandleDC);

      if CompDC <> 0 then
      begin
        FillChar(Bminfo, SizeOf(TBitmapInfoHeader), 0);

        Bminfo.BmiHeader.BiSize := FBitmapInfo.BmiHeader.BiSize;
        Bminfo.BmiHeader.BiCompression := FBitmapInfo.BmiHeader.BiCompression;

        Bminfo.BmiHeader.BiPlanes := FBitmapInfo.BmiHeader.BiPlanes;
        Bminfo.BmiHeader.BiBitCount := FBitmapInfo.BmiHeader.BiBitCount;

        Bminfo.BmiHeader.BiWidth := FBitmapInfo.BmiHeader.BiWidth;
        Bminfo.BmiHeader.BiHeight := FBitmapInfo.BmiHeader.BiHeight;

        Handle := CreateDIBSection(CompDC, Bminfo, DIB_RGB_COLORS,
          Buffer, 0, 0);
        Stride := GetStride;

        Rinc := ((Bminfo.BmiHeader.BiWidth * Bminfo.BmiHeader.BiBitCount + 31)
          shr 5) shl 2;
        Size := Rinc * Bminfo.BmiHeader.BiHeight;

        if Handle <> 0 then
        begin
          Backup := SelectObject(CompDC, Handle);

          if (Rinc = Stride) and (Size = FImageSize) then
          begin
            Move(FBuffer^, Buffer^, Size);

            BitBlt(HandleDC, DvcX, DvcY, DvcWidth, DvcHeight, CompDC, BmpX,
              BmpY, SRCCOPY);
          end
          else
            MessageBox(0, 'Cannot draw - different format !',
              'TPixelMap.draw message', MB_OK);

          if Backup <> 0 then
            SelectObject(CompDC, Backup);
          DeleteObject(Handle);
        end;
        DeleteDC(CompDC);
      end;
    end;
  end;
end;

procedure TPixelMap.Draw(HandleDC: HDC; X, Y: Integer; Scale: Double = 1.0);
var
  Rect: TRect;

  AWidth, AHeight: Cardinal;
begin
  if (FBitmapInfo = nil) or (FBuffer = nil) then
    Exit;

  AWidth := Trunc(FBitmapInfo.BmiHeader.BiWidth * Scale);
  AHeight := Trunc(FBitmapInfo.BmiHeader.BiHeight * Scale);

  Rect.Left := X;
  Rect.Top := Y;
  Rect.Right := X + AWidth;
  Rect.Bottom := Y + AHeight;

  Draw(HandleDC, @Rect);
end;

function TPixelMap.GetBuffer;
begin
  Result := FBuffer;
end;

function TPixelMap.GetWidth;
begin
  //NOTE: An access violation error happened here when destroying a test application  (shannon 3rd Dec 2012)
  Result := FBitmapInfo.BmiHeader.BiWidth;
end;

function TPixelMap.GetHeight;
begin
  Result := FBitmapInfo.BmiHeader.BiHeight;
end;

function TPixelMap.GetStride;
begin
  Result := CalculateRowLen(FBitmapInfo.BmiHeader.BiWidth, FBitmapInfo.BmiHeader.BiBitCount);
end;

function TPixelMap.GetBitsPerPixel;
begin
  Result := FBitsPerPixel;
end;

function TPixelMap.CalculateFullSize;
begin
  if Bmp = nil then
    Result := 0
  else
    Result := SizeOf(TBITMAPINFOHEADER) + SizeOf(RGBQUAD) *
      CalculatePaletteSize(Bmp) + Bmp.BmiHeader.BiSizeImage;
end;

function TPixelMap.CalculateHeaderSize;
begin
  if Bmp = nil then
    Result := 0
  else
    Result := SizeOf(TBITMAPINFOHEADER) + SizeOf(RGBQUAD) *
      CalculatePaletteSize(Bmp);
end;

function TPixelMap.CalculateImagePointer;
begin
  if Bmp = nil then
    Result := 0
  else
    Result := PtrComp(Bmp) + CalculateHeaderSize(Bmp);
end;

function TPixelMap.CreateBitmapInfo(Width, Height, BitsPerPixel: Cardinal)
  : PBITMAPINFO;
var
  Bmp      : PBITMAPINFO;
  LineLen : Cardinal;
  ImgSize : Cardinal;
  RgbSize : Cardinal;
  FullSize: Cardinal;
begin
  LineLen := CalculateRowLen(Width, BitsPerPixel);
  ImgSize := LineLen * Height;
  RgbSize := CalculatePaletteSize(0, BitsPerPixel) * SizeOf(RGBQUAD);
  FullSize := SizeOf(TBITMAPINFOHEADER) + RgbSize + ImgSize;

  AggGetMem(Pointer(Bmp), FullSize);
  FillChar(Bmp^, FullSize, 0);

  Bmp.BmiHeader.BiSize := SizeOf(TBITMAPINFOHEADER);
  Bmp.BmiHeader.BiWidth := Width;
  Bmp.BmiHeader.BiHeight := Height;
  Bmp.BmiHeader.BiPlanes := 1;
  Bmp.BmiHeader.BiBitCount := BitsPerPixel;
  Bmp.BmiHeader.BiCompression := 0;
  Bmp.BmiHeader.BiSizeImage := ImgSize;
  Bmp.BmiHeader.BiXPelsPerMeter := 0;
  Bmp.BmiHeader.BiYPelsPerMeter := 0;
  Bmp.BmiHeader.BiClrUsed := 0;
  Bmp.BmiHeader.BiClrImportant := 0;

  Result := Bmp;
end;

procedure TPixelMap.CreateGrayScalePalette(Bmp: PBITMAPINFO);
var
  Rgb: PRGBQUAD;
  I, RgbSize, Brightness: Cardinal;
begin
  if Bmp = nil then
    Exit;

  RgbSize := CalculatePaletteSize(Bmp);

  Rgb := PRGBQUAD(PtrComp(Bmp) + SizeOf(TBITMAPINFOHEADER));

  if RgbSize > 0 then
    for I := 0 to RgbSize - 1 do
    begin
      Brightness := Trunc((255 * I) / (RgbSize - 1));

      Rgb.RgbBlue := Brightness;
      Rgb.RgbGreen := Brightness;
      Rgb.RgbRed := Brightness;

      Rgb.RgbReserved := 0;

      Inc(PtrComp(Rgb), SizeOf(RGBQUAD));
    end;
end;

function TPixelMap.CalculatePaletteSize(AColorUsed, ABitsPerPixel: Cardinal)
  : Cardinal;
var
  PaletteSize: Integer;

begin
  PaletteSize := 0;

  if ABitsPerPixel <= 8 then
  begin
    PaletteSize := AColorUsed;

    if PaletteSize = 0 then
      PaletteSize := 1 shl ABitsPerPixel;
  end;
  Result := PaletteSize;
end;

function TPixelMap.CalculatePaletteSize(Bmp: PBITMAPINFO): Cardinal;
begin
  if Bmp = nil then
    Result := 0
  else
    Result := CalculatePaletteSize(Bmp.BmiHeader.BiClrUsed,
      Bmp.BmiHeader.BiBitCount);
end;

function TPixelMap.CalculateRowLen(AWidth, ABitsPerPixel: Cardinal): Cardinal;
var
  N, K: Cardinal;
begin
  N := AWidth;

  case ABitsPerPixel of
    1:
      begin
        K := N;
        N := N shr 3;
        if K and 7 <> 0 then
          Inc(N);
      end;

    4:
      begin
        K := N;
        N := N shr 1;

        if K and 3 <> 0 then
          Inc(N);
      end;

    8:
      NoP;
    16:
      N := N * 2;
    24:
      N := N * 3;
    32:
      N := N * 4;
    48:
      N := N * 6;
    64:
      N := N * 8;
  else
    N := 0;
  end;

  Result := ((N + 3) shr 2) shl 2;
end;

procedure TPixelMap.CreateFromBitmap(Bmp: PBITMAPINFO);
begin
  if Bmp <> nil then
  begin
    FImageSize := CalculateRowLen(Bmp.BmiHeader.BiWidth,
      Bmp.BmiHeader.BiBitCount) * Bmp.BmiHeader.BiHeight;

    FFullSize := CalculateFullSize(Bmp);

    FBitmapInfo := Bmp;
    FBuffer := Pointer(CalculateImagePointer(Bmp));
  end;
end;

end.
