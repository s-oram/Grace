unit AggColor;

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
{$Q-}
{$R-}

uses
  Math,
  AggBasics;

type
  PAggOrderRgb = ^TAggOrderRGB;
  TAggOrderRgb = record
    R, G, B: Int8u;
  end;

  PAggOrderBgr = ^TAggOrderBGR;
  TAggOrderBgr = record
    B, G, R: Int8u;
  end;

  PAggOrderRgba = ^TAggOrderRGBA;
  TAggOrderRgba = record
    R, G, B, A: Int8u;
  end;

  PAggOrderBgra = ^TAggOrderBGRA;
  TAggOrderBgra = record
    B, G, R, A: Int8u;
  end;

  PAggOrderArgb = ^TAggOrderARGB;
  TAggOrderArgb = record
    A, R, G, B: Int8u;
  end;

  PAggOrderAbgr = ^TAggOrderABGR;
  TAggOrderAbgr = record
    A, B, G, R: Int8u;
  end;

  TAggOrder = TAggOrderRgba;

const
  CAggBaseShift = 8;
  CAggBaseSize = 1 shl CAggBaseShift;
  CAggBaseMask = CAggBaseSize - 1;

  CAggOrderRgb: TAggOrder = (R: 0; G: 1; B: 2; A: 3);
  CAggOrderBgr: TAggOrder = (R: 2; G: 1; B: 0; A: 3);
  CAggOrderRgba: TAggOrder = (R: 0; G: 1; B: 2; A: 3);
  CAggOrderBgra: TAggOrder = (R: 2; G: 1; B: 0; A: 3);
  CAggOrderArgb: TAggOrder = (R: 1; G: 2; B: 3; A: 0);
  CAggOrderAbgr: TAggOrder = (R: 3; G: 2; B: 1; A: 0);

type
  TAggPackedRgba8 = type Cardinal;

  PPAggRgba8 = ^PAggRgba8;
  PAggRgba8 = ^TAggRgba8;
  TAggRgba8 = record
  public
    procedure Initialize(R, G, B: Int8u; A: Cardinal = CAggBaseMask);

    procedure NoColor;
    procedure Random;
    procedure Black;

    function Gradient(C: TAggRgba8; K: Double): TAggRgba8;
  case Integer of
   0: (ABGR: TAggPackedRgba8);
   1: (R, G, B, A: Byte);
   2: (Bytes: array [0..3] of Byte);
  end;

  PAggColor = ^TAggColor;

  { TAggColor }

  TAggColor = record
    Rgba8: TAggRgba8;
    V: Int8u;
  private
    function GetOpacity: Double;
    function GetBlue: Double;
    function GetGreen: Double;
    function GetRed: Double;
    procedure SetOpacity(Value: Double);
    procedure SetBlue(Value: Double);
    procedure SetGreen(Value: Double);
    procedure SetRed(Value: Double);
    procedure CalculateValue;
  public
    procedure FromRgba8(Rgba: TAggRgba8);
    procedure FromValueInteger(Value: Cardinal; Alpha: Cardinal = CAggBaseMask);
    procedure FromRgbaInteger(R, G, B: Cardinal; A: Cardinal = CAggBaseMask);
    procedure FromRgbInteger(R, G, B: Cardinal; A: Double = 1.0);
    procedure FromRgbaDouble(R, G, B: Double; A: Double = 1.0);
    procedure FromWaveLength(WaveLength, Gamma: Double);

    function Gradient8(const C: TAggColor; K: Double): TAggRgba8;

    procedure Add(C: PAggColor; Cover: Cardinal);

    procedure Clear;
    procedure Black;
    procedure White;
    procedure PreMultiply;
    procedure ApplyGammaDir(Gamma: TAggGamma);

    property Opacity: Double read GetOpacity write SetOpacity;
    property Red: Double read GetRed write SetRed;
    property Green: Double read GetGreen write SetGreen;
    property Blue: Double read GetBlue write SetBlue;
  end;

const
  // Some predefined color constants
  CRgba8Black       : TAggRgba8 = (ABGR : $FF000000);
  CRgba8DarkGray    : TAggRgba8 = (ABGR : $FF3F3F3F);
  CRgba8Gray        : TAggRgba8 = (ABGR : $FF7F7F7F);
  CRgba8LightGray   : TAggRgba8 = (ABGR : $FFBFBFBF);
  CRgba8White       : TAggRgba8 = (ABGR : $FFFFFFFF);
  CRgba8Maroon      : TAggRgba8 = (ABGR : $FF00007F);
  CRgba8Green       : TAggRgba8 = (ABGR : $FF007F00);
  CRgba8Olive       : TAggRgba8 = (ABGR : $FF007F7F);
  CRgba8Navy        : TAggRgba8 = (ABGR : $FF7F0000);
  CRgba8Purple      : TAggRgba8 = (ABGR : $FF7F007F);
  CRgba8Teal        : TAggRgba8 = (ABGR : $FF7F7F00);
  CRgba8Red         : TAggRgba8 = (ABGR : $FF0000FF);
  CRgba8Lime        : TAggRgba8 = (ABGR : $FF00FF00);
  CRgba8Yellow      : TAggRgba8 = (ABGR : $FF00FFFF);
  CRgba8Blue        : TAggRgba8 = (ABGR : $FFFF0000);
  CRgba8Fuchsia     : TAggRgba8 = (ABGR : $FFFF00FF);
  CRgba8Aqua        : TAggRgba8 = (ABGR : $FFFFFF00);

  CRgba8SemiWhite   : TAggRgba8 = (ABGR : $7FFFFFFF);
  CRgba8SemiBlack   : TAggRgba8 = (ABGR : $7F000000);
  CRgba8SemiRed     : TAggRgba8 = (ABGR : $7F0000FF);
  CRgba8SemiGreen   : TAggRgba8 = (ABGR : $7F00FF00);
  CRgba8SemiBlue    : TAggRgba8 = (ABGR : $7FFF0000);
  CRgba8SemiMaroon  : TAggRgba8 = (ABGR : $FF00007F);
  CRgba8SemiOlive   : TAggRgba8 = (ABGR : $FF007F7F);
  CRgba8SemiNavy    : TAggRgba8 = (ABGR : $FF7F0000);
  CRgba8SemiPurple  : TAggRgba8 = (ABGR : $FF7F007F);
  CRgba8SemiTeal    : TAggRgba8 = (ABGR : $FF7F7F00);
  CRgba8SemiLime    : TAggRgba8 = (ABGR : $FF00FF00);
  CRgba8SemiFuchsia : TAggRgba8 = (ABGR : $FFFF00FF);
  CRgba8SemiAqua    : TAggRgba8 = (ABGR : $FFFFFF00);

function Rgb8Packed(V: TAggPackedRgba8): TAggRgba8;
function HueSaturationLuminanceToRgb8(H, S, L: Double): TAggRgba8;
function RandomRgba8: TAggRgba8; overload; inline;
function RandomRgba8(Alpha: Int8U): TAggRgba8; overload; inline;
function Gradient(const C1, C2: TAggColor; K: Double): TAggColor;

implementation


function HueSaturationLuminanceToRgb8(H, S, L: Double): TAggRgba8;
const
  COneOverThree = 1 / 3;
var
  Temp: array [0..1] of Double;

  function HueToColor(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);
    if 6 * Hue < 1 then
      V := Temp[0] + (Temp[1] - Temp[0]) * Hue * 6
    else
    if 2 * Hue < 1 then
      V := Temp[1]
    else
    if 3 * Hue < 2 then
      V := Temp[0] + (Temp[1] - Temp[0]) * (2 * COneOverThree - Hue) * 6
    else
      V := Temp[0];
    Result := Round(255 * V);
  end;

begin
  if S = 0 then
  begin
    Result.R := Round(255 * L);
    Result.G := Result.R;
    Result.B := Result.R;
  end
  else
  begin
    if L <= 0.5 then
      Temp[1] := L * (1 + S)
    else
      Temp[1] := L + S - L * S;

    Temp[0] := 2 * L - Temp[1];

    Result.R := HueToColor(H + COneOverThree);
    Result.G := HueToColor(H);
    Result.B := HueToColor(H - COneOverThree)
  end;
  Result.A := $FF;
end;

function RandomRgba8: TAggRgba8;
begin
  Result.Abgr := System.Random($FFFFFFFF);
end;

function RandomRgba8(Alpha: Int8U): TAggRgba8; overload;
begin
  Result.Abgr := System.Random($FFFFFF) + Alpha  shl 24;
end;

function Gradient(const C1, C2: TAggColor; K: Double): TAggColor;
var
  Ik: Cardinal;
begin
  Ik := Trunc(K * CAggBaseSize);

  Result.Rgba8.R := Int8u(C1.Rgba8.R + (((C2.Rgba8.R - C1.Rgba8.R) * Ik) shr CAggBaseShift));
  Result.Rgba8.G := Int8u(C1.Rgba8.G + (((C2.Rgba8.G - C1.Rgba8.G) * Ik) shr CAggBaseShift));
  Result.Rgba8.B := Int8u(C1.Rgba8.B + (((C2.Rgba8.B - C1.Rgba8.B) * Ik) shr CAggBaseShift));
  Result.Rgba8.A := Int8u(C1.Rgba8.A + (((C2.Rgba8.A - C1.Rgba8.A) * Ik) shr CAggBaseShift));
end;

function Rgba8ToAggColor(Rgba: TAggRgba8): TAggColor;
begin
  with Result do
  begin
    Rgba8 := Rgba;
    V := (Rgba8.R * 77 + Rgba8.G * 150 + Rgba8.B * 29) shr 8;
  end;
end;

function RgbaIntegerToAggColor(R, G, B: Cardinal; A: Cardinal = CAggBaseMask):
  TAggColor;
begin
  with Result do
  begin
    Rgba8.R := Int8u(R);
    Rgba8.G := Int8u(G);
    Rgba8.B := Int8u(B);
    Rgba8.A := A;
    V := (Rgba8.R * 77 + Rgba8.G * 150 + Rgba8.B * 29) shr 8;
  end;
end;

function RgbIntegerToAggColor(R, G, B: Cardinal; A: Double = 1.0): TAggColor;
begin
  with Result do
  begin
    Rgba8.R := Int8u(R);
    Rgba8.G := Int8u(G);
    Rgba8.B := Int8u(B);
    Rgba8.A := Trunc(A * CAggBaseMask + 0.5);
    V := (Rgba8.R * 77 + Rgba8.G * 150 + Rgba8.B * 29) shr 8;
  end;
end;

function RgbaDoubleToAggColor(R, G, B, A: Double): TAggColor;
begin
  with Result do
  begin
    V := Trunc((0.299 * R + 0.587 * G + 0.114 * B) * CAggBaseMask + 0.5);
    Rgba8.R := Trunc(R * CAggBaseMask + 0.5);
    Rgba8.G := Trunc(G * CAggBaseMask + 0.5);
    Rgba8.B := Trunc(B * CAggBaseMask + 0.5);
    Rgba8.A := Trunc(A * CAggBaseMask + 0.5);
  end;
end;


{ TAggRgba8 }

procedure TAggRgba8.Initialize(R, G, B: Int8u; A: Cardinal = CAggBaseMask);
begin
  Self.B := Int8u(B);
  Self.G := Int8u(G);
  Self.R := Int8u(R);
  Self.A := Int8u(A);
end;

procedure TAggRgba8.NoColor;
begin
  R := 0;
  G := 0;
  B := 0;
  A := 0;
end;

procedure TAggRgba8.Black;
begin
  R := 0;
  G := 0;
  B := 0;
  A := $FF;
end;

procedure TAggRgba8.Random;
begin
  ABGR := System.Random($FFFFFFFF);
end;

function TAggRgba8.Gradient(C: TAggRgba8; K: Double): TAggRgba8;
var
  Ik: Int32u; // calc_type
begin
  Ik := Trunc(K * CAggBaseSize);

  Result.R := Int8u(Int32u(R) + (((Int32u(C.R) - R) * Ik) shr CAggBaseShift));
  Result.G := Int8u(Int32u(G) + (((Int32u(C.G) - G) * Ik) shr CAggBaseShift));
  Result.B := Int8u(Int32u(B) + (((Int32u(C.B) - B) * Ik) shr CAggBaseShift));
  Result.A := Int8u(Int32u(A) + (((Int32u(C.A) - A) * Ik) shr CAggBaseShift));
end;


{ TAggColor }

procedure TAggColor.FromRgba8(Rgba: TAggRgba8);
begin
  Rgba8 := Rgba;
  CalculateValue;
end;

procedure TAggColor.FromRgbaInteger(R, G, B: Cardinal; A: Cardinal = CAggBaseMask);
begin
  Rgba8.R := Int8u(R);
  Rgba8.G := Int8u(G);
  Rgba8.B := Int8u(B);
  Rgba8.A := A;
  CalculateValue;
end;

procedure TAggColor.FromRgbInteger(R, G, B: Cardinal; A: Double = 1.0);
begin
  Rgba8.R := Int8u(R);
  Rgba8.G := Int8u(G);
  Rgba8.B := Int8u(B);
  Rgba8.A := Trunc(A * CAggBaseMask + 0.5);
  CalculateValue;
end;

procedure TAggColor.FromValueInteger(Value: Cardinal; Alpha: Cardinal);
begin
  V := Value;
  Rgba8.R := 0;
  Rgba8.G := 0;
  Rgba8.B := 0;
  Rgba8.A := Alpha;
end;

procedure TAggColor.FromRgbaDouble(R, G, B: Double; A: Double);
begin
  V := Trunc((0.299 * R + 0.587 * G + 0.114 * B) * CAggBaseMask + 0.5);
  Rgba8.R := Trunc(R * CAggBaseMask + 0.5);
  Rgba8.G := Trunc(G * CAggBaseMask + 0.5);
  Rgba8.B := Trunc(B * CAggBaseMask + 0.5);
  Rgba8.A := Trunc(A * CAggBaseMask + 0.5);
end;

procedure TAggColor.FromWaveLength(WaveLength, Gamma: Double);
var
  Tr, Tg, Tb, S: Double;
begin
  Tr := 0;
  Tg := 0;
  Tb := 0;

  if (WaveLength >= 380.0) and (WaveLength <= 440.0) then
  begin
    Tr := (440.0 - WaveLength) / 60;
    Tb := 1.0;
  end
  else if (WaveLength >= 440.0) and (WaveLength <= 490.0) then
  begin
    Tg := (WaveLength - 440.0) * 0.02;
    Tb := 1.0;
  end
  else if (WaveLength >= 490.0) and (WaveLength <= 510.0) then
  begin
    Tg := 1.0;
    Tb := (510 - WaveLength) * 0.05;
  end
  else if (WaveLength >= 510.0) and (WaveLength <= 580.0) then
  begin
    Tr := (WaveLength - 510.0) / 70;
    Tg := 1.0;
  end
  else if (WaveLength >= 580.0) and (WaveLength <= 645.0) then
  begin
    Tr := 1.0;
    Tg := (645.0 - WaveLength) / 65;
  end
  else if (WaveLength >= 645.0) and (WaveLength <= 780.0) then
    Tr := 1.0;

  S := 1.0;

  if WaveLength > 700.0 then
    S := 0.3 + 0.7 * (780.0 - WaveLength) / 80
  else if WaveLength < 420.0 then
    S := 0.3 + 0.7 * (WaveLength - 380.0) / 40;

  Tr := Power(Tr * S, Gamma);
  Tg := Power(Tg * S, Gamma);
  Tb := Power(Tb * S, Gamma);

  V := Trunc((0.299 * Tr + 0.587 * Tg + 0.114 * Tb) * CAggBaseMask + 0.5);
  Rgba8.R := Trunc(Tr * CAggBaseMask + 0.5);
  Rgba8.G := Trunc(Tg * CAggBaseMask + 0.5);
  Rgba8.B := Trunc(Tb * CAggBaseMask + 0.5);
  Rgba8.A := $FF;
end;

function TAggColor.Gradient8(const C: TAggColor; K: Double): TAggRgba8;
var
  Ik: Cardinal;
begin
  Ik := Trunc(K * CAggBaseSize);

  Result.R := Int8u(Rgba8.R + (((C.Rgba8.R - Rgba8.R) * Ik) shr CAggBaseShift));
  Result.G := Int8u(Rgba8.G + (((C.Rgba8.G - Rgba8.G) * Ik) shr CAggBaseShift));
  Result.B := Int8u(Rgba8.B + (((C.Rgba8.B - Rgba8.B) * Ik) shr CAggBaseShift));
  Result.A := Int8u(Rgba8.A + (((C.Rgba8.A - Rgba8.A) * Ik) shr CAggBaseShift));
end;

procedure TAggColor.Add(C: PAggColor; Cover: Cardinal);
var
  Cv, Cr, Cg, Cb, Ca: Int32u;
begin
  if Cover = CAggCoverMask then
    if C.Rgba8.A = CAggBaseMask then
    begin
      V := C^.V;
      Rgba8 := C^.Rgba8;
    end
    else
    begin
      Cv := V + C.V;

      if Cv > Int32u(CAggBaseMask) then
        V := Int8u(CAggBaseMask)
      else
        V := Int8u(Cv);

      Cr := Rgba8.R + C.Rgba8.R;

      if Cr > Int32u(CAggBaseMask) then
        Rgba8.R := Int8u(CAggBaseMask)
      else
        Rgba8.R := Int8u(Cr);

      Cg := Rgba8.G + C.Rgba8.G;

      if Cg > Int32u(CAggBaseMask) then
        Rgba8.G := Int8u(CAggBaseMask)
      else
        Rgba8.G := Int8u(Cg);

      Cb := Rgba8.B + C.Rgba8.B;

      if Cb > Int32u(CAggBaseMask) then
        Rgba8.B := Int8u(CAggBaseMask)
      else
        Rgba8.B := Int8u(Cb);

      Ca := Rgba8.A + C.Rgba8.A;

      if Ca > Int32u(CAggBaseMask) then
        Rgba8.A := Int8u(CAggBaseMask)
      else
        Rgba8.A := Int8u(Ca);
    end
  else
  begin
    Cv := V + ((C.V * Cover + CAggCoverMask div 2) shr CAggCoverShift);
    Cr := Rgba8.R + ((C.Rgba8.R * Cover + CAggCoverMask div 2) shr CAggCoverShift);
    Cg := Rgba8.G + ((C.Rgba8.G * Cover + CAggCoverMask div 2) shr CAggCoverShift);
    Cb := Rgba8.B + ((C.Rgba8.B * Cover + CAggCoverMask div 2) shr CAggCoverShift);
    Ca := Rgba8.A + ((C.Rgba8.A * Cover + CAggCoverMask div 2) shr CAggCoverShift);

    if Cv > Int32u(CAggBaseMask) then
      V := Int8u(CAggBaseMask)
    else
      V := Int8u(Cv);

    if Cr > Int32u(CAggBaseMask) then
      Rgba8.R := Int8u(CAggBaseMask)
    else
      Rgba8.R := Int8u(Cr);

    if Cg > Int32u(CAggBaseMask) then
      Rgba8.G := Int8u(CAggBaseMask)
    else
      Rgba8.G := Int8u(Cg);

    if Cb > Int32u(CAggBaseMask) then
      Rgba8.B := Int8u(CAggBaseMask)
    else
      Rgba8.B := Int8u(Cb);

    if Ca > Int32u(CAggBaseMask) then
      Rgba8.A := Int8u(CAggBaseMask)
    else
      Rgba8.A := Int8u(Ca);
  end;
end;

procedure TAggColor.SetBlue(Value: Double);
begin
  if Value < 0.0 then
    Value := 0.0;

  if Value > 1.0 then
    Value := 1.0;

  Rgba8.B := Trunc(Value * CAggBaseMask + 0.5);
end;

procedure TAggColor.SetGreen(Value: Double);
begin
  if Value < 0.0 then
    Value := 0.0;

  if Value > 1.0 then
    Value := 1.0;

  Rgba8.G := Trunc(Value * CAggBaseMask + 0.5);
end;

procedure TAggColor.SetRed(Value: Double);
begin
  if Value < 0.0 then
    Value := 0.0;

  if Value > 1.0 then
    Value := 1.0;

  Rgba8.R := Trunc(Value * CAggBaseMask + 0.5);
end;

procedure TAggColor.SetOpacity(Value: Double);
begin
  if Value < 0.0 then
    Value := 0.0;

  if Value > 1.0 then
    Value := 1.0;

  Rgba8.A := Trunc(Value * CAggBaseMask + 0.5);
end;

function TAggColor.GetBlue: Double;
begin
  Result := Rgba8.B / CAggBaseMask;
end;

function TAggColor.GetGreen: Double;
begin
  Result := Rgba8.G / CAggBaseMask;
end;

function TAggColor.GetOpacity: Double;
begin
  Result := Rgba8.A / CAggBaseMask;
end;

function TAggColor.GetRed: Double;
begin
  Result := Rgba8.R / CAggBaseMask;
end;

procedure TAggColor.CalculateValue;
begin
  V := (Rgba8.R * 77 + Rgba8.G * 150 + Rgba8.B * 29) shr 8;
end;

procedure TAggColor.Clear;
begin
  V := 0;
  Rgba8.R := 0;
  Rgba8.G := 0;
  Rgba8.B := 0;
  Rgba8.A := 0;
end;

procedure TAggColor.PreMultiply;
begin
  if Rgba8.A = CAggBaseMask then
    Exit;

  if Rgba8.A = 0 then
  begin
    V := 0;
    Rgba8.R := 0;
    Rgba8.G := 0;
    Rgba8.B := 0;
    Exit;
  end;

  V := Int8u((V * Rgba8.A) shr CAggBaseShift);
  Rgba8.R := Int8u((Rgba8.R * Rgba8.A) shr CAggBaseShift);
  Rgba8.G := Int8u((Rgba8.G * Rgba8.A) shr CAggBaseShift);
  Rgba8.B := Int8u((Rgba8.B * Rgba8.A) shr CAggBaseShift);
end;

procedure TAggColor.ApplyGammaDir(Gamma: TAggGamma);
begin
  V := Int8u(Gamma.Dir[V]);
  Rgba8.R := Int8u(Gamma.Dir[Rgba8.R]);
  Rgba8.G := Int8u(Gamma.Dir[Rgba8.G]);
  Rgba8.B := Int8u(Gamma.Dir[Rgba8.B]);
end;

procedure TAggColor.Black;
begin
  V := 0;
  Rgba8 := CRgba8Black;
end;

procedure TAggColor.White;
begin
  V := $FF;
  Rgba8 := CRgba8White;
end;

function Rgb8Packed(V: TAggPackedRgba8): TAggRgba8;
begin
  Result.R := (V shr 16) and $FF;
  Result.G := (V shr 8) and $FF;
  Result.B := V and $FF;
  Result.A := CAggBaseMask;
end;

end.
