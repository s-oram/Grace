unit AggMacPixelMap;

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
  QuickTimeComponents,
  ImageCompression,
  Carbon,
  AggBasics;

const
  COrgMono8 = 8;
  COrgColor16 = 16;
  COrgColor24 = 24;
  COrgColor32 = 32;

type
  TPixelMap = class
  private
    FPixelMap: ^CGrafPort;
    FBuffer: Pointer;

    FBitsPerPixel, FImageSize: Cardinal;

    function GetBuffer: Pointer;
    function GetWidth: Cardinal;
    function GetHeight: Cardinal;
    function GetRowBytes: Integer;
    function GetBitsPerPixel: Cardinal;
  public
    constructor Create;
    destructor Destroy; overload;

    procedure FreeBitmap;
    procedure Build(Width, Height, Org: Cardinal; ClearVal: Cardinal = 255);
    procedure Clear(ClearVal: Cardinal = 255);

    function LoadFromQt(Filename: ShortString): Boolean;
    function SaveAsQt(Filename: ShortString): Boolean;

    procedure Draw(Window: WindowRef; Device_rect: RectPtr = nil;
      BitmapRect: RectPtr = nil); overload;
    procedure Draw(Window: WindowRef; X, Y: Integer; Scale: Double = 1.0); overload;

    procedure Blend(Window: WindowRef; Device_rect: RectPtr = nil;
      BitmapRect: RectPtr = nil); overload;
    procedure Blend(Window: WindowRef; X, Y: Integer; Scale: Double = 1.0);
      overload;

    // Auxiliary static functions
    function CalculateRowLength(Width, BitsPerPixel: Cardinal): Cardinal;

    property Buffer: Pointer read GetBuffer;
    property Width: Cardinal read GetWidth;
    property Height: Cardinal read GetHeight;
    property RowBytes: Integer read GetRowBytes;
    property BitsPerPixel: Cardinal read GetBitsPerPixel;
  end;

implementation

{ TPixelMap }

constructor TPixelMap.Create;
begin
  FPixelMap := nil;
  FBuffer := nil;
  FBitsPerPixel := 0;

  FImageSize := 0;
end;

destructor TPixelMap.Destroy;
begin
  FreeBitmap;
  inherited;
end;

procedure TPixelMap.FreeBitmap;
begin
  AggFreeMem(FBuffer, FImageSize);

  FBuffer := nil;

  if FPixelMap <> nil then
  begin
    DisposeGWorld(GrafPtr(FPixelMap));

    FPixelMap := nil;
  end;
end;

procedure TPixelMap.Build;
var
  R: Carbon.Rect;
  Row_bytes: Integer;
begin
  FreeBitmap;

  if Width = 0 then
    Width := 1;

  if Height = 0 then
    Height := 1;

  FBitsPerPixel := Org;
  Row_bytes := CalculateRowLength(Width, FBitsPerPixel);

  SetRect(R, 0, 0, Width, Height);

  FImageSize := Row_bytes * Height;

  AggGetMem(FBuffer, FImageSize);

  // The Quicktime version for creating GWorlds is more flexible than the classical function.
  QTNewGWorldFromPtr(FPixelMap, FBitsPerPixel, ImageCompression.Rect(R), nil, nil, 0,
    FBuffer, Row_bytes);

  // create_gray_scale_palette(FPixelMap);  I didn't care about gray scale palettes so far.
  if ClearVal <= 255 then
    FillChar(FBuffer^, FImageSize, ClearVal);
end;

procedure TPixelMap.Clear;
begin
  if FBuffer <> nil then
    FillChar(FBuffer^, FImageSize, ClearVal);
end;

function TPixelMap.LoadFromQt;
var
  Fss: FSSpec;
  Err: OSErr;
  Gi : GraphicsImportComponent;
  Buf: PInt8u;

  Desc : ImageDescriptionHandle;
  Depth: Int16;
  Size : Cardinal;
begin
  // get file specification to application directory
  Err := HGetVol(nil, Fss.VRefNum, Fss.ParID);

  if Err = NoErr then
  begin
    // CopyCStringToPascal(filename ,fss.name );
    Fss.Name := Filename;

    Err := GetGraphicsImporterForFile(ImageCompression.FSSpec(Fss), Gi);

    if Err = NoErr then
    begin
      GraphicsImportGetImageDescription(Gi, Desc);

      // For simplicity, all images are currently converted to 32 bit.
      // create an empty pixelmap
      Depth := 24;

      Create(Desc^.Width, Desc^.Height, Depth, $FF);
      DisposeHandle(Handle(Desc));

      // let Quicktime draw to pixelmap
      GraphicsImportSetGWorld(Gi, FPixelMap, nil);
      GraphicsImportDraw(Gi);

    end;

  end;

  Result := Err = NoErr;
end;

function TPixelMap.SaveAsQt;
var
  Fss: FSSpec;
  Err: OSErr;
  Ge : GraphicsExportComponent;
  Cnt: UInt32;
begin
  // get file specification to application directory
  Err := HGetVol(nil, Fss.VRefNum, Fss.ParID);

  if Err = NoErr then
  begin
    // CopyCStringToPascal(filename ,fss.name );
    Fss.Name := Filename;

    // I decided to use PNG as output image file type.
    // There are a number of other available formats.
    // Should I check the file suffix to choose the image file format?
    Err := OpenADefaultComponent
      (LongWord(PInt32(@GraphicsExporterComponentType[1])^),
      LongWord(PInt32(@KQTFileTypePNG[1])^), Carbon.ComponentInstance(Ge));

    if Err = NoErr then
    begin
      Err := GraphicsExportSetInputGWorld(Ge, FPixelMap);

      if Err = NoErr then
      begin
        Err := GraphicsExportSetOutputFile(Ge, ImageCompression.FSSpec(Fss));
        Cnt := 0;

        if Err = NoErr then
          GraphicsExportDoExport(Ge, Cnt);

      end;

      CloseComponent(Carbon.ComponentInstance(Ge));

    end;

  end;

  Result := Err = NoErr;
end;

procedure TPixelMap.Draw(Window: WindowRef; Device_rect: RectPtr = nil;
  BitmapRect: RectPtr = nil);
var
  Pm  : PixMapHandle;
  Port: CGrafPtr;

  Src_rect, Dest_rect: Carbon.Rect;

  Image_description: ImageDescriptionHandle;

begin
  if (FPixelMap = nil) or (FBuffer = nil) then
    Exit;

  Pm := GetGWorldPixMap(GrafPtr(FPixelMap));
  Port := GetWindowPort(Window);

  // Again, I used the Quicktime version.
  // Good old 'CopyBits' does better interpolation when scaling
  // but does not support all pixel depths.
  SetRect(Dest_rect, 0, 0, GetWidth, GetHeight);

  MakeImageDescriptionForPixMap(ImageCompression.PixMapHandle(Pm),
    Image_description);

  if Image_description <> nil then
  begin
    SetRect(Src_rect, 0, 0, Image_description^.Width,
      Image_description^.Height);

    DecompressImage(GetPixBaseAddr(Pm), Image_description,
      ImageCompression.PixMapHandle(GetPortPixMap(Port)),
      ImageCompression.Rect(Src_rect), ImageCompression.Rect(Dest_rect),
      DitherCopy, nil);

    DisposeHandle(Handle(Image_description));
  end;
end;

procedure TPixelMap.Draw(Window: WindowRef; X, Y: Integer; Scale: Double = 1.0);
var
  Width, Height: Cardinal;
  R: Carbon.Rect;
begin
  if (FPixelMap = nil) or (FBuffer = nil) then
    Exit;

  Width := System.Trunc(_width * Scale);
  Height := System.Trunc(GetHeight * Scale);

  SetRect(R, X, Y, X + Width, Y + Height);
  Draw(Window, @R);
end;

procedure TPixelMap.Blend(Window: WindowRef; Device_rect: RectPtr = nil;
  BitmapRect: RectPtr = nil);
begin
  Draw(Window, Device_rect, BitmapRect);
  // currently just mapped to drawing method
end;

procedure TPixelMap.Blend(Window: WindowRef; X, Y: Integer; Scale: Double = 1.0);
begin
  Draw(Window, X, Y, Scale); // currently just mapped to drawing method
end;

function TPixelMap.GetBuffer;
begin
  Result := FBuffer;
end;

function TPixelMap.GetWidth;
var
  Pm: PixMapHandle;
  Bounds: Carbon.Rect;
begin
  if FPixelMap = nil then
  begin
    Result := 0;
    Exit;
  end;

  Pm := GetGWorldPixMap(GrafPtr(FPixelMap));
  GetPixBounds(Pm, Bounds);
  Result := Bounds.Right - Bounds.Left;
end;

function TPixelMap.GetHeight;
var
  Pm: PixMapHandle;
  Bounds: Carbon.Rect;
begin
  if FPixelMap = nil then
  begin
    Result := 0;
    Exit;
  end;

  Pm := GetGWorldPixMap(GrafPtr(FPixelMap));
  GetPixBounds(Pm, Bounds);
  Result := Bounds.Bottom - Bounds.Top;
end;

function TPixelMap.GetRowBytes;
var
  Pm: PixMapHandle;
begin
  if FPixelMap = nil then
  begin
    Result := 0;
    Exit;
  end;
  Pm := GetGWorldPixMap(GrafPtr(FPixelMap));
  Result := CalculateRowLength(_width, GetPixDepth(Pm));
end;

function TPixelMap.GetBitsPerPixel;
begin
  Result := FBitsPerPixel;
end;

function TPixelMap.CalculateRowLength;
var
  N, K: Cardinal;
begin
  N := Width;

  case BitsPerPixel of
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
      N := N shl 1;

    24:
      N := (N shl 1) + N;

    32:
      N := N shl 2;

  else
    N := 0;
  end;

  Result := ((N + 3) shr 2) shl 2;
end;

end.
