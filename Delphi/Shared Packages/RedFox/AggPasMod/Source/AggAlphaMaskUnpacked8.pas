unit AggAlphaMaskUnpacked8;

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
  AggRenderingBuffer;

const
  CAggCoverShift = 8;
  CAggCoverNone = 0;
  CAggCoverFull = 255;

type
  TAggFuncMaskCalculate = function(P: PInt8u): Cardinal;

  TAggCustomAlphaMask = class
  public
    procedure Attach(RenderingBuffer: TAggRenderingBuffer); virtual; abstract;

    function MaskFunction: TAggFuncMaskCalculate; virtual; abstract;

    function Pixel(X, Y: Integer): Int8u; virtual; abstract;
    function CombinePixel(X, Y: Integer; Val: Int8u): Int8u; virtual; abstract;

    procedure FillHSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); virtual;
      abstract;
    procedure CombineHSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer);
      virtual; abstract;
    procedure FillVSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); virtual;
      abstract;
    procedure CombineVSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer);
      virtual; abstract;
  end;

  TAggAlphaMaskUnpacked8 = class(TAggCustomAlphaMask)
  private
    Step, Offset: Cardinal;

    FRenderingBuffer: TAggRenderingBuffer;
    FMaskFunction: TAggFuncMaskCalculate;
  public
    constructor Create(MaskF: TAggFuncMaskCalculate; AStep: Cardinal = 1;
      AOffset: Cardinal = 0); overload;
    constructor Create(RenderingBuffer: TAggRenderingBuffer;
      MaskF: TAggFuncMaskCalculate; AStep: Cardinal = 1;
      AOffset: Cardinal = 0); overload;

    procedure Attach(RenderingBuffer: TAggRenderingBuffer); override;

    function MaskFunction: TAggFuncMaskCalculate; override;

    function Pixel(X, Y: Integer): Int8u; override;
    function CombinePixel(X, Y: Integer; Val: Int8u): Int8u; override;

    procedure FillHSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); override;
    procedure CombineHSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); override;
    procedure FillVSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); override;
    procedure CombineVSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); override;
  end;

  TAggAlphaMaskGray8 = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgb24Red = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgb24Green = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgb24Blue = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgr24Red = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgr24Green = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgr24Blue = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgba32Red = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgba32Green = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgba32Blue = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgba32Alpha = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskArgb32Red = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskArgb32Green = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskArgb32Blue = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskArgb32Alpha = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgra32Red = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgra32Green = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgra32Blue = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgra32Alpha = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskAbgr32Red = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskAbgr32Green = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskAbgr32Blue = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskAbgr32Alpha = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgb24Gray = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgr24Gray = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskRgba32Gray = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskArgb32Gray = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskBgra32Gray = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskAbgr32Gray = class(TAggAlphaMaskUnpacked8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipUnpack8 = class(TAggCustomAlphaMask)
  private
    Step, Offset: Cardinal;

    FRenderingBuffer: TAggRenderingBuffer;
    FMaskFunction   : TAggFuncMaskCalculate;
  public
    constructor Create(MaskF: TAggFuncMaskCalculate; AStep: Cardinal = 1;
      AOffset: Cardinal = 0); overload;
    constructor Create(RenderingBuffer: TAggRenderingBuffer; MaskF: TAggFuncMaskCalculate;
      AStep: Cardinal = 1; AOffset: Cardinal = 0); overload;

    procedure Attach(RenderingBuffer: TAggRenderingBuffer); override;

    function MaskFunction: TAggFuncMaskCalculate; override;

    function Pixel(X, Y: Integer): Int8u; override;
    function CombinePixel(X, Y: Integer; Val: Int8u): Int8u; override;

    procedure FillHSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); override;
    procedure CombineHSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); override;
    procedure FillVSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); override;
    procedure CombineVSpan(X, Y: Integer; Dst: PInt8u; NumPixel: Integer); override;
  end;

  TAggAlphaMaskNoClipGray8 = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgb24Red = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgb24Green = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgb24Blue = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgr24Red = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgr24Green = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgr24Blue = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgba32Red = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgba32Green = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgba32Blue = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgba32Alpha = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipArgb32Red = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipArgb32Green = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipArgb32Blue = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipArgb32Alpha = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgra32Red = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgra32Green = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgra32Blue = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgra32Alpha = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipAbgr32Red = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipAbgr32Green = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipAbgr32Blue = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipAbgr32Alpha = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgb24Gray = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgr24Gray = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipRgba32Gray = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipArgb32Gray = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipBgra32Gray = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

  TAggAlphaMaskNoClipAbgr32Gray = class(TAggAlphaMaskNoClipUnpack8)
  public
    constructor Create(RenderingBuffer: TAggRenderingBuffer);
  end;

function OneComponentMaskUnpacked8(P: PInt8u): Cardinal;
function RgbToGrayMaskUnpacked8_012(P: PInt8u): Cardinal;
function RgbToGrayMaskUnpacked8_210(P: PInt8u): Cardinal;

implementation

function OneComponentMaskUnpacked8;
begin
  Result := P^;
end;

function RgbToGrayMaskUnpacked8_012;
begin
  Result := Int8u(PInt8u(P)^ * 77 +
    PInt8u(PtrComp(P) + SizeOf(Int8u))^ * 150 + PInt8u(PtrComp(P) + 2 *
    SizeOf(Int8u))^ * 29 shr 8);
end;

function RgbToGrayMaskUnpacked8_210;
begin
  Result := Int8u(PInt8u(PtrComp(P) + 2 * SizeOf(Int8u))^ * 77 +
    PInt8u(PtrComp(P) + SizeOf(Int8u))^ * 150 + PInt8u(P)^ * 29 shr 8);
end;


{ TAggAlphaMaskUnpacked8 }

constructor TAggAlphaMaskUnpacked8.Create(MaskF: TAggFuncMaskCalculate; AStep: Cardinal = 1;
  AOffset: Cardinal = 0);
begin
  Step := AStep;
  Offset := AOffset;

  FRenderingBuffer := nil;
  FMaskFunction := MaskF;
end;

constructor TAggAlphaMaskUnpacked8.Create(RenderingBuffer: TAggRenderingBuffer;
  MaskF: TAggFuncMaskCalculate; AStep: Cardinal = 1; AOffset: Cardinal = 0);
begin
  Step := AStep;
  Offset := AOffset;

  FRenderingBuffer := RenderingBuffer;
  FMaskFunction := MaskF;
end;

procedure TAggAlphaMaskUnpacked8.Attach;
begin
  FRenderingBuffer := RenderingBuffer;
end;

function TAggAlphaMaskUnpacked8.MaskFunction;
begin
  Result := @FMaskFunction;
end;

function TAggAlphaMaskUnpacked8.Pixel;
begin
  if (X >= 0) and (Y >= 0) and (X < FRenderingBuffer.Width) and
    (Y < FRenderingBuffer.Height) then
    Result := Int8u(FMaskFunction(PInt8u(PtrComp(FRenderingBuffer.Row(Y)) +
      (X * Step + Offset) * SizeOf(Int8u))))
  else
    Result := 0;
end;

function TAggAlphaMaskUnpacked8.CombinePixel;
begin
  if (X >= 0) and (Y >= 0) and (X < FRenderingBuffer.Width) and
    (Y < FRenderingBuffer.Height) then
    Result := Int8u
      ((CAggCoverFull + Val * FMaskFunction(PInt8u(PtrComp(FRenderingBuffer.Row(Y))
      + (X * Step + Offset) * SizeOf(Int8u)))) shr CAggCoverShift)
  else
    Result := 0;
end;

procedure TAggAlphaMaskUnpacked8.FillHSpan;
var
  Xmax, Ymax, Count, Rest: Integer;
  Covers, Mask           : PInt8u;
begin
  Xmax := FRenderingBuffer.Width - 1;
  Ymax := FRenderingBuffer.Height - 1;

  Count := NumPixel;
  Covers := Dst;

  if (Y < 0) or (Y > Ymax) then
  begin
    FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);

    Exit;
  end;

  if X < 0 then
  begin
    Inc(Count, X);

    if Count <= 0 then
    begin
      FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);

      Exit;
    end;

    FillChar(Covers^, -X * SizeOf(Int8u), 0);

    Dec(Covers, X);

    X := 0;
  end;

  if X + Count > Xmax then
  begin
    Rest := X + Count - Xmax - 1;

    Dec(Count, Rest);

    if Count <= 0 then
    begin
      FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);

      Exit;
    end;

    FillChar(PInt8u(PtrComp(Covers) + Count * SizeOf(Int8u))^,
      Rest * SizeOf(Int8u), 0);
  end;

  Mask := PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + (X * Step + Offset) *
    SizeOf(Int8u));

  repeat
    Covers^ := Int8u(FMaskFunction(Mask));

    Inc(PtrComp(Covers), SizeOf(Int8u));
    Inc(PtrComp(Mask), Step * SizeOf(Int8u));
    Dec(Count);

  until Count = 0;
end;

procedure TAggAlphaMaskUnpacked8.CombineHSpan;
var
  Xmax, Ymax, Count, Rest: Integer;
  Covers, Mask           : PInt8u;
begin
  Xmax := FRenderingBuffer.Width - 1;
  Ymax := FRenderingBuffer.Height - 1;

  Count := NumPixel;
  Covers := Dst;

  if (Y < 0) or (Y > Ymax) then
  begin
    FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);
    Exit;
  end;

  if X < 0 then
  begin
    Inc(Count, X);

    if Count <= 0 then
    begin
      FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);
      Exit;
    end;

    FillChar(Covers^, -X * SizeOf(Int8u), 0);
    Dec(PtrComp(Covers), X * SizeOf(Int8u));
    X := 0;
  end;

  if X + Count > Xmax then
  begin
    Rest := X + Count - Xmax - 1;
    Dec(Count, Rest);

    if Count <= 0 then
    begin
      FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);
      Exit;
    end;

    FillChar(PInt8u(PtrComp(Covers) + Count * SizeOf(Int8u))^,
      Rest * SizeOf(Int8u), 0);
  end;

  Mask := PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + (X * Step + Offset) *
    SizeOf(Int8u));

  repeat
    Covers^ := Int8u((CAggCoverFull + Covers^ * FMaskFunction(Mask))
      shr CAggCoverShift);

    Inc(PtrComp(Covers), SizeOf(Int8u));
    Inc(Mask, Step * SizeOf(Int8u));
    Dec(Count);

  until Count = 0;
end;

procedure TAggAlphaMaskUnpacked8.FillVSpan;
var
  Xmax, Ymax, Count, Rest: Integer;

  Covers, Mask: PInt8u;

begin
  Xmax := FRenderingBuffer.Width - 1;
  Ymax := FRenderingBuffer.Height - 1;

  Count := NumPixel;
  Covers := Dst;

  if (X < 0) or (X > Xmax) then
  begin
    FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);

    Exit;
  end;

  if Y < 0 then
  begin
    Inc(Count, Y);

    if Count <= 0 then
    begin
      FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);

      Exit;
    end;

    FillChar(Covers^, -Y * SizeOf(Int8u), 0);
    Dec(PtrComp(Covers), Y * SizeOf(Int8u));
    Y := 0;
  end;

  if Y + Count > Ymax then
  begin
    Rest := Y + Count - Ymax - 1;
    Dec(Count, Rest);

    if Count <= 0 then
    begin
      FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);
      Exit;
    end;

    FillChar(PInt8u(PtrComp(Covers) + Count * SizeOf(Int8u))^,
      Rest * SizeOf(Int8u), 0);
  end;

  repeat
    Covers^ := Int8u(FMaskFunction(Mask));

    Inc(PtrComp(Covers), SizeOf(Int8u));
    Inc(PtrComp(Mask), FRenderingBuffer.Stride);
    Dec(Count);
  until Count = 0;
end;

procedure TAggAlphaMaskUnpacked8.CombineVSpan;
var
  Xmax, Ymax, Count, Rest: Integer;

  Covers, Mask: PInt8u;

begin
  Xmax := FRenderingBuffer.Width - 1;
  Ymax := FRenderingBuffer.Height - 1;

  Count := NumPixel;
  Covers := Dst;

  if (X < 0) or (X > Xmax) then
  begin
    FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);
    Exit;
  end;

  if Y < 0 then
  begin
    Inc(Count, Y);

    if Count <= 0 then
    begin
      FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);

      Exit;
    end;

    FillChar(Covers^, -Y * SizeOf(Int8u), 0);
    Dec(PtrComp(Covers), Y * SizeOf(Int8u));
    Y := 0;
  end;

  if Y + Count > Ymax then
  begin
    Rest := Y + Count - Ymax - 1;
    Dec(Count, Rest);

    if Count <= 0 then
    begin
      FillChar(Dst^, NumPixel * SizeOf(Int8u), 0);
      Exit;
    end;

    FillChar(PInt8u(PtrComp(Covers) + Count * SizeOf(Int8u))^,
      Rest * SizeOf(Int8u), 0);
  end;

  Mask := PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + (X * Step + Offset) *
    SizeOf(Int8u));
  repeat
    Covers^ := Int8u((CAggCoverFull + Covers^ * FMaskFunction(Mask))
      shr CAggCoverShift);

    Inc(PtrComp(Covers), SizeOf(Int8u));
    Inc(PtrComp(Mask), FRenderingBuffer.Stride);
    Dec(Count);
  until Count = 0;
end;


{ TAggAlphaMaskGray8 }

constructor TAggAlphaMaskGray8.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 1, 0);
end;


{ TAggAlphaMaskRgb24Red }

constructor TAggAlphaMaskRgb24Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 0);
end;


{ TAggAlphaMaskRgb24Green }

constructor TAggAlphaMaskRgb24Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 1);
end;


{ TAggAlphaMaskRgb24Blue }

constructor TAggAlphaMaskRgb24Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 2);
end;


{ TAggAlphaMaskBgr24Red }

constructor TAggAlphaMaskBgr24Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 2);
end;


{ TAggAlphaMaskBgr24Green }

constructor TAggAlphaMaskBgr24Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 1);
end;


{ TAggAlphaMaskBgr24Blue }

constructor TAggAlphaMaskBgr24Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 0);
end;


{ TAggAlphaMaskRgba32Red }

constructor TAggAlphaMaskRgba32Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 0);
end;


{ TAggAlphaMaskRgba32Green }

constructor TAggAlphaMaskRgba32Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 1);
end;


{ TAggAlphaMaskRgba32Blue }

constructor TAggAlphaMaskRgba32Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 2);
end;


{ TAggAlphaMaskRgba32Alpha }

constructor TAggAlphaMaskRgba32Alpha.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 3);
end;


{ TAggAlphaMaskArgb32Red }

constructor TAggAlphaMaskArgb32Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 1);
end;


{ TAggAlphaMaskArgb32Green }

constructor TAggAlphaMaskArgb32Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 2);
end;


{ TAggAlphaMaskArgb32Blue }

constructor TAggAlphaMaskArgb32Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 3);
end;


{ TAggAlphaMaskArgb32Alpha }

constructor TAggAlphaMaskArgb32Alpha.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 0);
end;


{ TAggAlphaMaskBgra32Red }

constructor TAggAlphaMaskBgra32Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 2);
end;


{ TAggAlphaMaskBgra32Green }

constructor TAggAlphaMaskBgra32Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 1);
end;


{ TAggAlphaMaskBgra32Blue }

constructor TAggAlphaMaskBgra32Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 0);
end;


{ TAggAlphaMaskBgra32Alpha }

constructor TAggAlphaMaskBgra32Alpha.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 3);
end;


{ TAggAlphaMaskAbgr32Red }

constructor TAggAlphaMaskAbgr32Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 3);
end;


{ TAggAlphaMaskAbgr32Green }

constructor TAggAlphaMaskAbgr32Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 2);
end;


{ TAggAlphaMaskAbgr32Blue }

constructor TAggAlphaMaskAbgr32Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 1);
end;


{ TAggAlphaMaskAbgr32Alpha }

constructor TAggAlphaMaskAbgr32Alpha.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 0);
end;


{ TAggAlphaMaskRgb24Gray }

constructor TAggAlphaMaskRgb24Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_012, 3, 0);
end;


{ TAggAlphaMaskBgr24Gray }

constructor TAggAlphaMaskBgr24Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_210, 3, 0);
end;


{ TAggAlphaMaskRgba32Gray }

constructor TAggAlphaMaskRgba32Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_012, 4, 0);
end;


{ TAggAlphaMaskArgb32Gray }

constructor TAggAlphaMaskArgb32Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_012, 4, 1);
end;


{ TAggAlphaMaskBgra32Gray }

constructor TAggAlphaMaskBgra32Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_210, 4, 0);
end;


{ TAggAlphaMaskAbgr32Gray }

constructor TAggAlphaMaskAbgr32Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_210, 4, 1);
end;


{ TAggAlphaMaskNoClipUnpack8 }

constructor TAggAlphaMaskNoClipUnpack8.Create(MaskF: TAggFuncMaskCalculate;
  AStep: Cardinal = 1; AOffset: Cardinal = 0);
begin
  Step := AStep;
  Offset := AOffset;

  FRenderingBuffer := nil;
  FMaskFunction := MaskF;
end;

constructor TAggAlphaMaskNoClipUnpack8.Create(RenderingBuffer: TAggRenderingBuffer;
  MaskF: TAggFuncMaskCalculate; AStep: Cardinal = 1; AOffset: Cardinal = 0);
begin
  Step := AStep;
  Offset := AOffset;

  FRenderingBuffer := RenderingBuffer;
  FMaskFunction := MaskF;
end;

procedure TAggAlphaMaskNoClipUnpack8.Attach;
begin
  FRenderingBuffer := RenderingBuffer;
end;

function TAggAlphaMaskNoClipUnpack8.MaskFunction;
begin
  Result := @FMaskFunction;
end;

function TAggAlphaMaskNoClipUnpack8.Pixel;
begin
  Result := Int8u(FMaskFunction(PInt8u(PtrComp(FRenderingBuffer.Row(Y)) +
    (X * Step + Offset) * SizeOf(Int8u))));
end;

function TAggAlphaMaskNoClipUnpack8.CombinePixel;
begin
  Result := Int8u((CAggCoverFull + Val *
    FMaskFunction(PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + (X * Step + Offset)
    * SizeOf(Int8u)))) shr CAggCoverShift);
end;

procedure TAggAlphaMaskNoClipUnpack8.FillHSpan;
var
  Mask: PInt8u;
begin
  Mask := PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + (X * Step + Offset) *
    SizeOf(Int8u));

  repeat
    Dst^ := Int8u(FMaskFunction(Mask));

    Inc(PtrComp(Dst), SizeOf(Int8u));
    Inc(PtrComp(Mask), Step * SizeOf(Int8u));
    Dec(NumPixel);

  until NumPixel = 0;
end;

procedure TAggAlphaMaskNoClipUnpack8.CombineHSpan;
var
  Mask: PInt8u;
begin
  Mask := PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + (X * Step + Offset) *
    SizeOf(Int8u));

  repeat
    Dst^ := Int8u((CAggCoverFull + Dst^ * FMaskFunction(Mask)) shr CAggCoverShift);

    Inc(PtrComp(Dst), SizeOf(Int8u));
    Inc(PtrComp(Mask), Step * SizeOf(Int8u));
    Dec(NumPixel);

  until NumPixel = 0;
end;

procedure TAggAlphaMaskNoClipUnpack8.FillVSpan;
var
  Mask: PInt8u;
begin
  Mask := PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + (X * Step + Offset) *
    SizeOf(Int8u));

  repeat
    Dst^ := Int8u(FMaskFunction(Mask));

    Inc(PtrComp(Dst), SizeOf(Int8u));
    Inc(PtrComp(Mask), FRenderingBuffer.Stride);
    Dec(NumPixel);

  until NumPixel = 0;
end;

procedure TAggAlphaMaskNoClipUnpack8.CombineVSpan;
var
  Mask: PInt8u;
begin
  Mask := PInt8u(PtrComp(FRenderingBuffer.Row(Y)) + (X * Step + Offset) *
    SizeOf(Int8u));

  repeat
    Dst^ := Int8u((CAggCoverFull + Dst^ * FMaskFunction(Mask)) shr CAggCoverShift);

    Inc(PtrComp(Dst), SizeOf(Int8u));
    Inc(PtrComp(Mask), FRenderingBuffer.Stride);
    Dec(NumPixel);

  until NumPixel = 0;
end;


{ TAggAlphaMaskNoClipGray8 }

constructor TAggAlphaMaskNoClipGray8.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 1, 0);
end;


{ TAggAlphaMaskNoClipRgb24Red }

constructor TAggAlphaMaskNoClipRgb24Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 0);
end;


{ TAggAlphaMaskNoClipRgb24Green }

constructor TAggAlphaMaskNoClipRgb24Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 1);
end;


{ TAggAlphaMaskNoClipRgb24Blue }

constructor TAggAlphaMaskNoClipRgb24Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 2);
end;


{ TAggAlphaMaskNoClipBgr24Red }

constructor TAggAlphaMaskNoClipBgr24Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 2);
end;


{ TAggAlphaMaskNoClipBgr24Green }

constructor TAggAlphaMaskNoClipBgr24Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 1);
end;


{ TAggAlphaMaskNoClipBgr24Blue }

constructor TAggAlphaMaskNoClipBgr24Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 3, 0);
end;


{ TAggAlphaMaskNoClipRgba32Red }

constructor TAggAlphaMaskNoClipRgba32Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 0);
end;


{ TAggAlphaMaskNoClipRgba32Green }

constructor TAggAlphaMaskNoClipRgba32Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 1);
end;


{ TAggAlphaMaskNoClipRgba32Blue }

constructor TAggAlphaMaskNoClipRgba32Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 2);
end;


{ TAggAlphaMaskNoClipRgba32Alpha }

constructor TAggAlphaMaskNoClipRgba32Alpha.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 3);
end;


{ TAggAlphaMaskNoClipArgb32Red }

constructor TAggAlphaMaskNoClipArgb32Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 1);
end;


{ TAggAlphaMaskNoClipArgb32Green }

constructor TAggAlphaMaskNoClipArgb32Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 2);
end;


{ TAggAlphaMaskNoClipArgb32Blue }

constructor TAggAlphaMaskNoClipArgb32Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 3);
end;


{ TAggAlphaMaskNoClipArgb32Alpha }

constructor TAggAlphaMaskNoClipArgb32Alpha.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 0);
end;


{ TAggAlphaMaskNoClipBgra32Red }

constructor TAggAlphaMaskNoClipBgra32Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 2);
end;


{ TAggAlphaMaskNoClipBgra32Green }

constructor TAggAlphaMaskNoClipBgra32Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 1);
end;


{ TAggAlphaMaskNoClipBgra32Blue }

constructor TAggAlphaMaskNoClipBgra32Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 0);
end;


{ TAggAlphaMaskNoClipBgra32Alpha }

constructor TAggAlphaMaskNoClipBgra32Alpha.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 3);
end;


{ TAggAlphaMaskNoClipAbgr32Red }

constructor TAggAlphaMaskNoClipAbgr32Red.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 3);
end;


{ TAggAlphaMaskNoClipAbgr32Green }

constructor TAggAlphaMaskNoClipAbgr32Green.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 2);
end;


{ TAggAlphaMaskNoClipAbgr32Blue }

constructor TAggAlphaMaskNoClipAbgr32Blue.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 1);
end;


{ TAggAlphaMaskNoClipAbgr32Alpha }

constructor TAggAlphaMaskNoClipAbgr32Alpha.Create;
begin
  inherited Create(RenderingBuffer, @OneComponentMaskUnpacked8, 4, 0);
end;


{ TAggAlphaMaskNoClipRgb24Gray }

constructor TAggAlphaMaskNoClipRgb24Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_012, 3, 0);
end;


{ TAggAlphaMaskNoClipBgr24Gray }

constructor TAggAlphaMaskNoClipBgr24Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_210, 3, 0);
end;


{ TAggAlphaMaskNoClipRgba32Gray }

constructor TAggAlphaMaskNoClipRgba32Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_012, 4, 0);
end;


{ TAggAlphaMaskNoClipArgb32Gray }

constructor TAggAlphaMaskNoClipArgb32Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_012, 4, 1);
end;


{ TAggAlphaMaskNoClipBgra32Gray }

constructor TAggAlphaMaskNoClipBgra32Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_210, 4, 0);
end;


{ TAggAlphaMaskNoClipAbgr32Gray }

constructor TAggAlphaMaskNoClipAbgr32Gray.Create;
begin
  inherited Create(RenderingBuffer, @RgbToGrayMaskUnpacked8_210, 4, 1);
end;

end.
