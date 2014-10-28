unit AggSpanGradientImage;

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
  AggBasics,
  AggColor,
  AggArray,
  AggSpanGradient,
  AggPixelFormat,
  AggPixelFormatRgba,
  AggRenderingBuffer;

type
  TAggOneColorFunction = class(TAggCustomArray)
  private
    FColor: TAggColor;
  public
    constructor Create;

    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(I: Cardinal): Pointer; override;
  end;

  TAggGradientImage = class(TAggCustomGradient)
  private
    FBuffer: Pointer;
    FAlocDeltaX, FAlocDeltaY: Integer;
    FWidth, FHeight: Integer;

    FRenderingBuffer: TAggRenderingBuffer;
    FPixelFormats: TAggPixelFormatProcessor;

    FColor: PAggColor;

    FColorFunction: TAggOneColorFunction;

    function GetImageStride: Integer;
    function GetPixelFormat: TAggPixelFormatProcessor;
  public
    constructor Create; override;
    destructor Destroy; override;

    function ImageCreate(Width, Height: Integer): Pointer;
    function GetImageBuffer: Pointer;

    function Calculate(X, Y, D: Integer): Integer; override;

    function GetColorFunction: TAggCustomArray;

    property PixelFormat: TAggPixelFormatProcessor read GetPixelFormat;
    property ImageWidth: Integer read FWidth;
    property ImageHeight: Integer read FHeight;
    property ImageStride: Integer read GetImageStride;
  end;

implementation


{ TAggOneColorFunction }

constructor TAggOneColorFunction.Create;
begin
end;

function TAggOneColorFunction.GetSize: Cardinal;
begin
  Result := 1;
end;

function TAggOneColorFunction.GetEntry: Cardinal;
begin
  Result := SizeOf(TAggColor);
end;

function TAggOneColorFunction.ArrayOperator(I: Cardinal): Pointer;
begin
  Result := @FColor;
end;


{ TAggGradientImage }

constructor TAggGradientImage.Create;
begin
  inherited Create;

  FColorFunction := TAggOneColorFunction.Create;

  FBuffer := nil;
  FAlocDeltaX := 0;
  FAlocDeltay := 0;
  FWidth := 0;
  FHeight := 0;

  FRenderingBuffer := nil;

  PixelFormatUndefined(FPixelFormats);

  FColor := FColorFunction[0];
end;

destructor TAggGradientImage.Destroy;
begin
  if FBuffer <> nil then
    AggFreeMem(FBuffer, FAlocDeltaX * FAlocDeltay * 4);

  if FRenderingBuffer <> nil then
    FRenderingBuffer.Free;

  FColorFunction.Free;
  FPixelFormats.Free;

  inherited;
end;

function TAggGradientImage.ImageCreate(Width, Height: Integer): Pointer;
var
  Row : Pointer;
  Rows: Cardinal;
begin
  Result := nil;

  if FRenderingBuffer <> nil then
    FRenderingBuffer.Free;

  FRenderingBuffer := nil;

  if (Width > FAlocDeltaX) or (Height > FAlocDeltay) then
  begin
    if FBuffer <> nil then
      AggFreeMem(FBuffer, FAlocDeltaX * FAlocDeltay * 4);

    FBuffer := nil;

    if AggGetMem(FBuffer, Width * Height * 4) then
    begin
      FAlocDeltaX := Width;
      FAlocDeltay := Height;
    end
    else
    begin
      FAlocDeltaX := 0;
      FAlocDeltay := 0;
    end;
  end;

  if FBuffer <> nil then
  begin
    FWidth := Width;
    FHeight := Height;

    Row := FBuffer;
    Rows := Height;

    while Rows > 0 do
    begin
      FillChar(Row^, FWidth * 4, 0);

      Inc(PtrComp(Row), FAlocDeltaX * 4);
      Dec(Rows);
    end;

    Result := FBuffer;
  end
  else
  begin
    FWidth := 0;
    FHeight := 0;
  end;
end;

function TAggGradientImage.GetImageBuffer: Pointer;
begin
  Result := FBuffer;
end;

function TAggGradientImage.GetImageStride: Integer;
begin
  Result := FAlocDeltaX * 4;
end;

function TAggGradientImage.Calculate(X, Y, D: Integer): Integer;
var
  Px, Py: Integer;
  Pixel: PAggRgba8;
begin
  Result := 0;

  if FBuffer <> nil then
  begin
    Px := ShrInt32(X, CAggGradientSubpixelShift);
    Py := ShrInt32(Y, CAggGradientSubpixelShift);

    Px := Px mod FWidth;

    if Px < 0 then
      Px := FWidth + Px;

    Py := Py mod FHeight;

    if Py < 0 then
      Py := FHeight + Py;

    Pixel := PAggRgba8(PtrComp(FBuffer) + Py * (FAlocDeltaX * 4) + Px * 4);

    FColor.Rgba8.R := Pixel.R;
    FColor.Rgba8.G := Pixel.G;
    FColor.Rgba8.B := Pixel.B;
    FColor.Rgba8.A := Pixel.A;
  end
  else
  begin
    FColor.Rgba8.R := 0;
    FColor.Rgba8.G := 0;
    FColor.Rgba8.B := 0;
    FColor.Rgba8.A := 0;
  end;
end;

function TAggGradientImage.GetPixelFormat: TAggPixelFormatProcessor;
begin
  if (FBuffer <> nil) and (FRenderingBuffer = nil) then
  begin
    FRenderingBuffer := TAggRenderingBuffer.Create;

    FRenderingBuffer.Attach(FBuffer, FWidth, FHeight, FAlocDeltaX * 4);
    PixelFormatRgba32(FPixelFormats, FRenderingBuffer);
  end;

  if FRenderingBuffer = nil then
    Result := nil
  else
    Result := FPixelFormats;
end;

function TAggGradientImage.GetColorFunction: TAggCustomArray;
begin
  Result := FColorFunction;
end;

end.
