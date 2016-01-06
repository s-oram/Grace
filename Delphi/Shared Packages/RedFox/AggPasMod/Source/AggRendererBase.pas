unit AggRendererBase;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012-2015                                                      //
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
  AggRenderingBuffer,
  AggPixelFormat,
  AggColor;

type
  TAggRendererBase = class
  private
    FClipBox: TRectInteger;
    procedure InitializePixelFormatProcessor;
  protected
    FPixelFormatProcessor: TAggPixelFormatProcessor;
    FOwnPixelFormatProcessor: Boolean;
    function GetWidth: Cardinal;
    function GetHeight: Cardinal;

    function GetXMin: Integer;
    function GetYMin: Integer;
    function GetXMax: Integer;
    function GetYMax: Integer;

    function GetBoundingXMin: Integer; virtual;
    function GetBoundingYMin: Integer; virtual;
    function GetBoundingXMax: Integer; virtual;
    function GetBoundingYMax: Integer; virtual;
  public
    constructor Create(PixelFormatProcessor: TAggPixelFormatProcessor;
      OwnPixelFormatProcessor: Boolean = False); overload; virtual;
    constructor Create(PixelFormatProcessorClass: TAggPixelFormatProcessorClass;
      RenderingBuffer: TAggRenderingBuffer); overload; virtual;
    destructor Destroy; override;

    function SetClipBox(X1, Y1, X2, Y2: Integer): Boolean; overload;
    function SetClipBox(Bounds: TRectInteger): Boolean; overload;
    procedure ResetClipping(Visibility: Boolean); virtual;
    procedure ClipBoxNaked(X1, Y1, X2, Y2: Integer);

    function Inbox(X, Y: Integer): Boolean; overload;
    function Inbox(Point: TPointInteger): Boolean; overload;

    procedure FirstClipBox; virtual;
    function NextClipBox: Boolean; virtual;

    function GetClipBox: PRectInteger;

    function BoundingClipBox: PRectInteger; virtual;

    procedure Clear(C: PAggColor); overload;
    procedure Clear(C: TAggRgba8); overload;

    procedure CopyPixel(X, Y: Integer; C: PAggColor); virtual;
    procedure BlendPixel(X, Y: Integer; C: PAggColor; Cover: Int8u); virtual;
    function Pixel(X, Y: Integer): TAggColor; virtual;

    procedure CopyHorizontalLine(X1, Y, X2: Integer; C: PAggColor); virtual;
    procedure CopyVerticalLine(X, Y1, Y2: Integer; C: PAggColor); virtual;

    procedure BlendHorizontalLine(X1, Y, X2: Integer; C: PAggColor; Cover: Int8u); virtual;
    procedure BlendVerticalLine(X, Y1, Y2: Integer; C: PAggColor; Cover: Int8u); virtual;

    procedure CopyBar(X1, Y1, X2, Y2: Integer; C: PAggColor); virtual;
    procedure BlendBar(X1, Y1, X2, Y2: Integer; C: PAggColor;
      Cover: Int8u); virtual;

    function Span(X, Y: Integer; Len: Cardinal): Pointer;

    procedure BlendSolidHSpan(X, Y, Len: Integer; C: PAggColor;
      Covers: PInt8u); virtual;
    procedure BlendSolidVSpan(X, Y, Len: Integer; C: PAggColor;
      Covers: PInt8u); virtual;

    procedure CopyColorHSpan(X, Y, Len: Integer; Colors: PAggColor); virtual;
    procedure BlendColorHSpan(X, Y, Len: Integer; Colors: PAggColor;
      Covers: PInt8u; Cover: Int8u = CAggCoverFull); virtual;
    procedure BlendColorVSpan(X, Y, Len: Integer; Colors: PAggColor;
      Covers: PInt8u; Cover: Int8u = CAggCoverFull); virtual;

    procedure CopyColorHSpanNoClip(X, Y, Len: Integer; Colors: PAggColor);
    procedure BlendColorHSpanNoClip(X, Y, Len: Integer; Colors: PAggColor;
      Covers: PInt8u; Cover: Int8u = CAggCoverFull);
    procedure BlendColorVSpanNoClip(X, Y, Len: Integer; Colors: PAggColor;
      Covers: PInt8u; Cover: Int8u = CAggCoverFull);

    function ClipRectArea(var Dst, Src: TRectInteger; Wsrc, Hsrc: Integer): TRectInteger;

    procedure CopyFrom(Src: TAggRenderingBuffer; RectSourcePointer: PRectInteger = nil;
      Dx: Integer = 0; Dy: Integer = 0); virtual;
    procedure BlendFrom(Src: TAggPixelFormatProcessor; RectSourcePointer: PRectInteger = nil;
      Dx: Integer = 0; Dy: Integer = 0; Cover: Int8u = CAggCoverFull); virtual;

    procedure BlendFromColor(Src: TAggPixelFormatProcessor; Color: PAggColor;
      RectSourcePointer: PRectInteger = nil; Dx: Integer = 0; Dy: Integer = 0;
      Cover: Int8u = CAggCoverFull);

    procedure BlendFromLUT(Src: TAggPixelFormatProcessor; AColorLUT: PAggColor;
      RectSourcePointer: PRectInteger = nil; Dx: Integer = 0; Dy: Integer = 0;
      Cover: Int8u = CAggCoverFull);

    property PixelFormatProcessor: TAggPixelFormatProcessor read
      FPixelFormatProcessor;

    property OwnPixelFormatProcessor: Boolean read FOwnPixelFormatProcessor
      write  FOwnPixelFormatProcessor;

    property Width: Cardinal read GetWidth;
    property Height: Cardinal read GetHeight;

    property XMin: Integer read GetXMin;
    property YMin: Integer read GetYMin;
    property XMax: Integer read GetXMax;
    property YMax: Integer read GetYMax;

    property BoundingXMin: Integer read GetBoundingXMin;
    property BoundingYMin: Integer read GetBoundingYMin;
    property BoundingXMax: Integer read GetBoundingXMax;
    property BoundingYMax: Integer read GetBoundingYMax;
  end;

implementation


{ TAggRendererBase }

constructor TAggRendererBase.Create(PixelFormatProcessor: TAggPixelFormatProcessor;
  OwnPixelFormatProcessor: Boolean = False);
begin
  FPixelFormatProcessor := PixelFormatProcessor;
  FOwnPixelFormatProcessor := OwnPixelFormatProcessor;

  InitializePixelFormatProcessor;
end;

constructor TAggRendererBase.Create(PixelFormatProcessorClass:
  TAggPixelFormatProcessorClass; RenderingBuffer: TAggRenderingBuffer);
begin
  FPixelFormatProcessor := PixelFormatProcessorClass.Create(RenderingBuffer);
  FOwnPixelFormatProcessor := True;

  InitializePixelFormatProcessor;
end;

destructor TAggRendererBase.Destroy;
begin
  if FOwnPixelFormatProcessor then
    FPixelFormatProcessor.Free;

  inherited;
end;

procedure TAggRendererBase.InitializePixelFormatProcessor;
var
  W, H: Integer;
begin
  W := 0;
  H := 0;
  if Assigned(FPixelFormatProcessor) then
  begin
    if (FPixelFormatProcessor.Width > 0) then
      W := FPixelFormatProcessor.Width - 1;
    if (FPixelFormatProcessor.Height > 0) then
      H := FPixelFormatProcessor.Height - 1;
  end;

  FClipBox := RectInteger(0, 0, W, H);
end;

function TAggRendererBase.GetWidth: Cardinal;
begin
  Result := FPixelFormatProcessor.Width;
end;

function TAggRendererBase.GetHeight: Cardinal;
begin
  Result := FPixelFormatProcessor.Height;
end;

procedure TAggRendererBase.ClipBoxNaked(X1, Y1, X2, Y2: Integer);
begin
  FClipBox.X1 := X1;
  FClipBox.Y1 := Y1;
  FClipBox.X2 := X2;
  FClipBox.Y2 := Y2;
end;

function TAggRendererBase.Inbox(X, Y: Integer): Boolean;
begin
  Result := (X >= FClipBox.X1) and (Y >= FClipBox.Y1) and
    (X <= FClipBox.X2) and (Y <= FClipBox.Y2);
end;

function TAggRendererBase.Inbox(Point: TPointInteger): Boolean;
begin
  Result := (Point.X >= FClipBox.X1) and (Point.Y >= FClipBox.Y1) and
    (Point.X <= FClipBox.X2) and (Point.Y <= FClipBox.Y2);
end;

function TAggRendererBase.SetClipBox(X1, Y1, X2, Y2: Integer): Boolean;
var
  Cb, RectClip: TRectInteger;
begin
  Cb := RectInteger(X1, Y1, X2, Y2);
  Cb.Normalize;

  RectClip := RectInteger(0, 0, Width - 1, Height - 1);
  if Cb.Clip(RectClip) then
  begin
    FClipBox := Cb;
    Result := True;
    Exit;
  end;

  FClipBox := RectInteger(1, 1, 0, 0);
  Result := False;
end;

function TAggRendererBase.SetClipBox(Bounds: TRectInteger): Boolean;
var
  RectClip: TRectInteger;
begin
  FClipBox := Bounds;
  FClipBox.Normalize;

  RectClip := RectInteger(0, 0, Width - 1, Height - 1);

  if FClipBox.Clip(RectClip) then
  begin
    Result := True;
    Exit;
  end;

  FClipBox := RectInteger(1, 1, 0, 0);

  Result := False;
end;

procedure TAggRendererBase.ResetClipping;
begin
  if Visibility then
  begin
    FClipBox := RectInteger(0, 0, Width - 1, Height - 1);
    Exit;
  end;
  FClipBox := RectInteger(1, 1, 0, 0);
end;

procedure TAggRendererBase.FirstClipBox;
begin
end;

function TAggRendererBase.NextClipBox: Boolean;
begin
  Result := False;
end;

function TAggRendererBase.GetClipBox: PRectInteger;
begin
  Result := @FClipBox;
end;

function TAggRendererBase.GetXMin;
begin
  Result := FClipBox.X1;
end;

function TAggRendererBase.GetYMin;
begin
  Result := FClipBox.Y1;
end;

function TAggRendererBase.GetXMax;
begin
  Result := FClipBox.X2;
end;

function TAggRendererBase.GetYMax;
begin
  Result := FClipBox.Y2;
end;

function TAggRendererBase.BoundingClipBox: PRectInteger;
begin
  Result := @FClipBox;
end;

function TAggRendererBase.GetBoundingXMin;
begin
  Result := FClipBox.X1;
end;

function TAggRendererBase.GetBoundingYMin;
begin
  Result := FClipBox.Y1;
end;

function TAggRendererBase.GetBoundingXMax;
begin
  Result := FClipBox.X2;
end;

function TAggRendererBase.GetBoundingYMax;
begin
  Result := FClipBox.Y2;
end;

procedure TAggRendererBase.Clear(C: PAggColor);
var
  Y: Cardinal;
begin
  if (Width > 0) and (Height > 0) then
    for Y := 0 to GetHeight - 1 do
      FPixelFormatProcessor.CopyHorizontalLine(FPixelFormatProcessor, 0, Y,
        Width, C);
end;

procedure TAggRendererBase.Clear(C: TAggRgba8);
var
  AggColor: TAggColor;
  Y: Cardinal;
begin
  AggColor.Rgba8 := C;
  if (Width > 0) and (Height > 0) then
    for Y := 0 to Height - 1 do
      FPixelFormatProcessor.CopyHorizontalLine(FPixelFormatProcessor, 0, Y,
        Width, @AggColor);
end;

procedure TAggRendererBase.CopyPixel(X, Y: Integer; C: PAggColor);
begin
  if Inbox(X, Y) then
    FPixelFormatProcessor.CopyPixel(FPixelFormatProcessor, X, Y, C);
end;

procedure TAggRendererBase.BlendPixel(X, Y: Integer; C: PAggColor; Cover: Int8u);
begin
  if Inbox(X, Y) then
    FPixelFormatProcessor.BlendPixel(FPixelFormatProcessor, X, Y, C, Cover);
end;

function TAggRendererBase.Pixel(X, Y: Integer): TAggColor;
begin
  if Inbox(X, Y) then
    Result := FPixelFormatProcessor.Pixel(FPixelFormatProcessor, X, Y);
end;

procedure TAggRendererBase.CopyHorizontalLine(X1, Y, X2: Integer; C: PAggColor);
var
  T: Integer;
begin
  if X1 > X2 then
  begin
    T := X2;
    X2 := X1;
    X1 := T;
  end;

  if Y > GetYMax then
    Exit;

  if Y < GetYMin then
    Exit;

  if X1 > GetXMax then
    Exit;

  if X2 < GetXMin then
    Exit;

  if X1 < GetXMin then
    X1 := GetXMin;

  if X2 > GetXMax then
    X2 := GetXMax;

  FPixelFormatProcessor.CopyHorizontalLine(FPixelFormatProcessor, X1, Y,
    X2 - X1 + 1, C);
end;

procedure TAggRendererBase.CopyVerticalLine(X, Y1, Y2: Integer; C: PAggColor);
var
  T: Integer;
begin
  if Y1 > Y2 then
  begin
    T := Y2;
    Y2 := Y1;
    Y1 := T;
  end;

  if X > GetXMax then
    Exit;

  if X < GetXMin then
    Exit;

  if Y1 > GetYMax then
    Exit;

  if Y2 < GetYMin then
    Exit;

  if Y1 < GetYMin then
    Y1 := GetYMin;

  if Y2 > GetYMax then
    Y2 := GetYMax;

  FPixelFormatProcessor.CopyVerticalLine(FPixelFormatProcessor, X, Y1,
    Y2 - Y1 + 1, C);
end;

procedure TAggRendererBase.BlendHorizontalLine(X1, Y, X2: Integer; C: PAggColor;
  Cover: Int8u);
var
  T: Integer;
begin
  if X1 > X2 then
  begin
    T := X2;
    X2 := X1;
    X1 := T;
  end;

  if Y > GetYMax then
    Exit;

  if Y < GetYMin then
    Exit;

  if X1 > GetXMax then
    Exit;

  if X2 < GetXMin then
    Exit;

  if X1 < GetXMin then
    X1 := GetXMin;

  if X2 > GetXMax then
    X2 := GetXMax;

  FPixelFormatProcessor.BlendHorizontalLine(FPixelFormatProcessor, X1, Y,
    X2 - X1 + 1, C, Cover);
end;

procedure TAggRendererBase.BlendVerticalLine(X, Y1, Y2: Integer; C: PAggColor;
  Cover: Int8u);
var
  T: Integer;
begin
  if Y1 > Y2 then
  begin
    T := Y2;
    Y2 := Y1;
    Y1 := T;
  end;

  if X > GetXMax then
    Exit;

  if X < GetXMin then
    Exit;

  if Y1 > GetYMax then
    Exit;

  if Y2 < GetYMin then
    Exit;

  if Y1 < GetYMin then
    Y1 := GetYMin;

  if Y2 > GetYMax then
    Y2 := GetYMax;

  FPixelFormatProcessor.BlendVerticalLine(FPixelFormatProcessor, X, Y1,
    Y2 - Y1 + 1, C, Cover);
end;

procedure TAggRendererBase.CopyBar(X1, Y1, X2, Y2: Integer; C: PAggColor);
var
  Y : Integer;
  RectClip: TRectInteger;
begin
  RectClip := RectInteger(X1, Y1, X2, Y2);
  RectClip.Normalize;

  if RectClip.Clip(GetClipBox^) then
  begin
    Y := RectClip.Y1;

    while Y <= RectClip.Y2 do
    begin
      FPixelFormatProcessor.CopyHorizontalLine(FPixelFormatProcessor,
        RectClip.X1, Y, RectClip.X2 - RectClip.X1 + 1, C);

      Inc(Y);
    end;
  end;
end;

procedure TAggRendererBase.BlendBar(X1, Y1, X2, Y2: Integer; C: PAggColor;
  Cover: Int8u);
var
  RectClip: TRectInteger;
  Y : Integer;
begin
  RectClip := RectInteger(X1, Y1, X2, Y2);
  RectClip.Normalize;

  if RectClip.Clip(GetClipBox^) then
  begin
    Y := RectClip.Y1;

    while Y <= RectClip.Y2 do
    begin
      FPixelFormatProcessor.BlendHorizontalLine(FPixelFormatProcessor,
        RectClip.X1, Y, Cardinal(RectClip.X2 - RectClip.X1 + 1), C, Cover);

      Inc(Y);
    end;
  end;
end;

function TAggRendererBase.Span(X, Y: Integer; Len: Cardinal): Pointer;
begin
end;

procedure TAggRendererBase.BlendSolidHSpan(X, Y, Len: Integer; C: PAggColor;
  Covers: PInt8u);
begin
  if Y > GetYMax then
    Exit;

  if Y < GetYMin then
    Exit;

  if X < GetXMin then
  begin
    Dec(Len, GetXMin - X);

    if Len <= 0 then
      Exit;

    Inc(PtrComp(Covers), (GetXMin - X) * SizeOf(Int8u));

    X := GetXMin;
  end;

  if X + Len > GetXMax then
  begin
    Len := GetXMax - X + 1;

    if Len <= 0 then
      Exit;
  end;

  FPixelFormatProcessor.BlendSolidHSpan(FPixelFormatProcessor, X, Y, Len, C,
    Covers);
end;

procedure TAggRendererBase.BlendSolidVSpan(X, Y, Len: Integer; C: PAggColor;
  Covers: PInt8u);
begin
  if X > GetXMax then
    Exit;

  if X < GetXMin then
    Exit;

  if Y < GetYMin then
  begin
    Dec(Len, GetYMin - Y);

    if Len <= 0 then
      Exit;

    Inc(PtrComp(Covers), (GetYMin - Y) * SizeOf(Int8u));

    Y := GetYMin;
  end;

  if Y + Len > GetYMax then
  begin
    Len := GetYMax - Y + 1;

    if Len <= 0 then
      Exit;
  end;

  FPixelFormatProcessor.BlendSolidVSpan(FPixelFormatProcessor, X, Y, Len, C,
    Covers);
end;

procedure TAggRendererBase.CopyColorHSpan(X, Y, Len: Integer;
  Colors: PAggColor);
var
  D: Integer;
begin
  if Y > GetYMax then
    Exit;

  if Y < GetYMin then
    Exit;

  if X < GetXMin then
  begin
    D := GetXMin - X;

    Dec(Len, D);

    if Len <= 0 then
      Exit;

    Inc(PtrComp(Colors), D * SizeOf(TAggColor));

    X := GetXMin;
  end;

  if X + Len > GetXMax then
  begin
    Len := GetXMax - X + 1;

    if Len <= 0 then
      Exit;
  end;

  FPixelFormatProcessor.CopyColorHSpan(FPixelFormatProcessor, X, Y, Len,
    Colors);
end;

procedure TAggRendererBase.BlendColorHSpan(X, Y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
var
  D: Integer;
begin
  if Y > GetYMax then
    Exit;

  if Y < GetYMin then
    Exit;

  if X < GetXMin then
  begin
    D := GetXMin - X;

    Dec(Len, D);

    if Len <= 0 then
      Exit;

    if Covers <> nil then
      Inc(PtrComp(Covers), D * SizeOf(Int8u));

    Inc(PtrComp(Colors), D * SizeOf(TAggColor));

    X := GetXMin;
  end;

  if X + Len > GetXMax then
  begin
    Len := GetXMax - X + 1;

    if Len <= 0 then
      Exit;
  end;

  FPixelFormatProcessor.BlendColorHSpan(FPixelFormatProcessor, X, Y, Len,
    Colors, Covers, Cover);
end;

procedure TAggRendererBase.BlendColorVSpan(X, Y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
var
  D: Integer;
begin
  if X > GetXMax then
    Exit;

  if X < GetXMin then
    Exit;

  if Y < GetYMin then
  begin
    D := GetYMin - Y;

    Dec(Len, D);

    if Len <= 0 then
      Exit;

    if Covers <> nil then
      Inc(PtrComp(Covers), D * SizeOf(Int8u));

    Inc(PtrComp(Colors), D * SizeOf(TAggColor));

    Y := GetYMin;
  end;

  if Y + Len > GetYMax then
  begin
    Len := GetYMax - Y + 1;

    if Len <= 0 then
      Exit;
  end;

  FPixelFormatProcessor.BlendColorVSpan(FPixelFormatProcessor, X, Y, Len,
    Colors, Covers, Cover);
end;

procedure TAggRendererBase.CopyColorHSpanNoClip(X, Y, Len: Integer; Colors: PAggColor);
begin
  // not implemented
end;

procedure TAggRendererBase.BlendColorHSpanNoClip(X, Y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FPixelFormatProcessor.BlendColorHSpan(FPixelFormatProcessor, X, Y, Len,
    Colors, Covers, Cover);
end;

procedure TAggRendererBase.BlendColorVSpanNoClip(X, Y, Len: Integer;
  Colors: PAggColor; Covers: PInt8u; Cover: Int8u = CAggCoverFull);
begin
  FPixelFormatProcessor.BlendColorVSpan(FPixelFormatProcessor, X, Y, Len,
    Colors, Covers, Cover);
end;

function TAggRendererBase.ClipRectArea(var Dst, Src: TRectInteger; Wsrc,
  Hsrc: Integer): TRectInteger;
var
  RectClip, Cb: TRectInteger;
begin
  RectClip := RectInteger(0, 0, 0, 0);

  Cb := GetClipBox^;

  Inc(Cb.X2);
  Inc(Cb.Y2);

  if Src.X1 < 0 then
  begin
    Dst.X1 := Dst.X1 - Src.X1;
    Src.X1 := 0;
  end;

  if Src.Y1 < 0 then
  begin
    Dst.Y1 := Dst.Y1 - Src.Y1;
    Src.Y1 := 0;
  end;

  if Src.X2 > Wsrc then
    Src.X2 := Wsrc;

  if Src.Y2 > Hsrc then
    Src.Y2 := Hsrc;

  if Dst.X1 < Cb.X1 then
  begin
    Src.X1 := Src.X1 + (Cb.X1 - Dst.X1);
    Dst.X1 := Cb.X1;
  end;

  if Dst.Y1 < Cb.Y1 then
  begin
    Src.Y1 := Src.Y1 + (Cb.Y1 - Dst.Y1);
    Dst.Y1 := Cb.Y1;
  end;

  if Dst.X2 > Cb.X2 then
    Dst.X2 := Cb.X2;

  if Dst.Y2 > Cb.Y2 then
    Dst.Y2 := Cb.Y2;

  RectClip.X2 := Dst.X2 - Dst.X1;
  RectClip.Y2 := Dst.Y2 - Dst.Y1;

  if RectClip.X2 > Src.X2 - Src.X1 then
    RectClip.X2 := Src.X2 - Src.X1;

  if RectClip.Y2 > Src.Y2 - Src.Y1 then
    RectClip.Y2 := Src.Y2 - Src.Y1;

  Result := RectClip;
end;

procedure TAggRendererBase.CopyFrom(Src: TAggRenderingBuffer; RectSourcePointer:
  PRectInteger = nil; Dx: Integer = 0; Dy: Integer = 0);
var
  RectSource, RectDest, RectClip: TRectInteger;
  IncY: Integer;
begin
  RectSource := RectInteger(0, 0, Src.Width, Src.Height);

  if RectSourcePointer <> nil then
  begin
    RectSource.X1 := RectSourcePointer.X1;
    RectSource.Y1 := RectSourcePointer.Y1;
    RectSource.X2 := RectSourcePointer.X2 + 1;
    RectSource.Y2 := RectSourcePointer.Y2 + 1;
  end;

  RectDest := RectInteger(RectSource.X1 + Dx, RectSource.Y1 + Dy,
    RectSource.X2 + Dx, RectSource.Y2 + Dy);

  RectClip := ClipRectArea(RectDest, RectSource, Src.Width, Src.Height);

  if RectClip.X2 > 0 then
  begin
    IncY := 1;

    if RectDest.Y1 > RectSource.Y1 then
    begin
      RectSource.Y1 := RectSource.Y1 + (RectClip.Y2 - 1);
      RectDest.Y1 := RectDest.Y1 + (RectClip.Y2 - 1);

      IncY := -1;
    end;

    while RectClip.Y2 > 0 do
    begin
      FPixelFormatProcessor.CopyFrom(FPixelFormatProcessor, Src, RectDest.X1,
        RectDest.Y1, RectSource.X1, RectSource.Y1, RectClip.X2);

      RectDest.Y1 := RectDest.Y1 + IncY;
      RectSource.Y1 := RectSource.Y1 + IncY;

      Dec(RectClip.Y2);
    end;
  end;
end;

procedure TAggRendererBase.BlendFrom(Src: TAggPixelFormatProcessor;
  RectSourcePointer: PRectInteger = nil; Dx: Integer = 0; Dy: Integer = 0;
  Cover: Int8u = CAggCoverFull);
var
  RectSource, RectDest, RectClip: TRectInteger;
  IncY, X1src, X1dst, Len: Integer;
  Rw: TAggRowDataType;
begin
  if RectSourcePointer <> nil then
  begin
    RectSource.X1 := RectSourcePointer.X1;
    RectSource.Y1 := RectSourcePointer.Y1;
    RectSource.X2 := RectSourcePointer.X2 + 1;
    RectSource.Y2 := RectSourcePointer.Y2 + 1;
  end
  else
  begin
    RectSource.X1 := 0;
    RectSource.Y1 := 0;
    RectSource.X2 := Src.Width;
    RectSource.Y2 := Src.Height;
  end;

  RectDest := RectInteger(RectSource.X1 + Dx, RectSource.Y1 + Dy,
    RectSource.X2 + Dx, RectSource.Y2 + Dy);

  RectClip := ClipRectArea(RectDest, RectSource, Src.Width, Src.Height);

  if RectClip.X2 > 0 then
  begin
    IncY := 1;

    if RectDest.Y1 > RectSource.Y1 then
    begin
      RectSource.Y1 := RectSource.Y1 + (RectClip.Y2 - 1);
      RectDest.Y1 := RectDest.Y1 + (RectClip.Y2 - 1);

      IncY := -1;
    end;

    while RectClip.Y2 > 0 do
    begin
      Rw := Src.Row(Src, RectSource.X1, RectSource.Y1);

      if Rw.Ptr <> nil then
      begin
        X1src := RectSource.X1;
        X1dst := RectDest.X1;
        Len := RectClip.X2;

        if Rw.X1 > X1src then
        begin
          Inc(X1dst, Rw.X1 - X1src);
          Dec(Len, Rw.X1 - X1src);

          X1src := Rw.X1;
        end;

        if Len > 0 then
        begin
          if X1src + Len - 1 > Rw.X2 then
            Dec(Len, X1src + Len - Rw.X2 - 1);

          if Len > 0 then
            FPixelFormatProcessor.BlendFrom(FPixelFormatProcessor, Src, Rw.Ptr,
              X1dst, RectDest.Y1, X1src, RectSource.Y1, Len, Cover);
        end;
      end;

      Inc(RectDest.Y1, IncY);
      Inc(RectSource.Y1, IncY);
      Dec(RectClip.Y2);
    end;
  end;
end;

procedure TAggRendererBase.BlendFromColor(Src: TAggPixelFormatProcessor;
  Color: PAggColor; RectSourcePointer: PRectInteger = nil; Dx: Integer = 0;
  Dy: Integer = 0; Cover: Int8u = CAggCoverFull);
var
  RectSource, RectDest, RectClip: TRectInteger;
  Rw: TAggRowDataType;
  IncY, X1src, X1dst, Len: Integer;
begin
  RectSource := RectInteger(0, 0, Src.Width, Src.Height);

  if RectSourcePointer <> nil then
  begin
    RectSource.X1 := RectSourcePointer.X1;
    RectSource.Y1 := RectSourcePointer.Y1;
    RectSource.X2 := RectSourcePointer.X2 + 1;
    RectSource.Y2 := RectSourcePointer.Y2 + 1;
  end;

  RectDest := RectInteger(RectSource.X1 + Dx, RectSource.Y1 + Dy,
    RectSource.X2 + Dx, RectSource.Y2 + Dy);

  RectClip := ClipRectArea(RectDest, RectSource, Src.Width, Src.Height);

  if RectClip.X2 > 0 then
  begin
    IncY := 1;

    if RectDest.Y1 > RectSource.Y1 then
    begin
      RectSource.Y1 := RectSource.Y1 + RectClip.Y2 - 1;
      RectDest.Y1 := RectDest.Y1 + RectClip.Y2 - 1;
      IncY := -1;
    end;

    while RectClip.Y2 > 0 do
    begin
      Rw := Src.Row(Src, 0, RectSource.Y1);

      if Rw.Ptr <> nil then
      begin
        X1src := RectSource.X1;
        X1dst := RectDest.X1;
        Len := RectClip.X2;

        if Rw.X1 > X1src then
        begin
          Inc(X1dst, Rw.X1 - X1src);
          Dec(Len, Rw.X1 - X1src);

          X1src := Rw.X1;
        end;

        if Len > 0 then
        begin
          if X1src + Len - 1 > Rw.X2 then
            Dec(Len, X1src + Len - Rw.X2 - 1);

          if Len > 0 then
            FPixelFormatProcessor.BlendFromColor(FPixelFormatProcessor, Src,
              Color, X1dst, RectDest.Y1, X1src, RectSource.Y1, Len, Cover);
        end;
      end;

      Inc(RectDest.Y1, IncY);
      Inc(RectSource.Y1, IncY);
      Dec(RectClip.Y2);
    end;
  end;
end;

procedure TAggRendererBase.BlendFromLUT(Src: TAggPixelFormatProcessor;
  AColorLUT: PAggColor; RectSourcePointer: PRectInteger = nil; Dx: Integer = 0;
  Dy: Integer = 0; Cover: Int8u = CAggCoverFull);
var
  RectSource, RectDest, RectClip: TRectInteger;
  Rw: TAggRowDataType;
  IncY, X1src, X1dst, Len: Integer;
begin
  RectSource := RectInteger(0, 0, Src.Width, Src.Height);

  if RectSourcePointer <> nil then
  begin
    RectSource.X1 := RectSourcePointer.X1;
    RectSource.Y1 := RectSourcePointer.Y1;
    RectSource.X2 := RectSourcePointer.X2 + 1;
    RectSource.Y2 := RectSourcePointer.Y2 + 1;
  end;

  RectDest := RectInteger(RectSource.X1 + Dx, RectSource.Y1 + Dy, RectSource.X2 + Dx,
    RectSource.Y2 + Dy);

  RectClip := ClipRectArea(RectDest, RectSource, Src.Width, Src.Height);

  if RectClip.X2 > 0 then
  begin
    IncY := 1;

    if RectDest.Y1 > RectSource.Y1 then
    begin
      RectSource.Y1 := RectSource.Y1 + RectClip.Y2 - 1;
      RectDest.Y1 := RectDest.Y1 + RectClip.Y2 - 1;
      IncY := -1;
    end;

    while RectClip.Y2 > 0 do
    begin
      Rw := Src.Row(Src, 0, RectSource.Y1);

      if Rw.Ptr <> nil then
      begin
        X1src := RectSource.X1;
        X1dst := RectDest.X1;
        Len := RectClip.X2;

        if Rw.X1 > X1src then
        begin
          Inc(X1dst, Rw.X1 - X1src);
          Dec(Len, Rw.X1 - X1src);

          X1src := Rw.X1;
        end;

        if Len > 0 then
        begin
          if X1src + Len - 1 > Rw.X2 then
            Dec(Len, X1src + Len - Rw.X2 - 1);

          if Len > 0 then
            FPixelFormatProcessor.BlendFromLUT(FPixelFormatProcessor, Src,
              AColorLUT, X1dst, RectDest.Y1, X1src, RectSource.Y1, Len, Cover);
        end;
      end;

      Inc(RectDest.Y1, IncY);
      Inc(RectSource.Y1, IncY);
      Dec(RectClip.Y2);
    end;
  end;
end;

end.
