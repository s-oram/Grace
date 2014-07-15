unit AggSvgParser;

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

{$DEFINE EXPAT_WRAPPER}
{$I AggCompiler.inc}

uses
  SysUtils,
  AnsiStrings,
  AggBasics,
  AggColor,
  AggSvgPathTokenizer,
  AggSvgPathRenderer,
  AggSvgException,
  AggTransAffine,
  AggMathStroke,
  Expat,
  AggFileUtils;

const
  CAggBufferSize = 512;

type
  TParser = class
  private
    FPath: TPathRenderer;
    FTokenizer: TPathTokenizer;

    FBuffer, FTitle: PAnsiChar;

    FTitleLength: Cardinal;
    FTitleFlag, FPathFlag: Boolean;
    FAttrName, FAttrValue: PAnsiChar;

    FAttrNameLen, FAttrNameAloc, FAttrValueLen, FAttrValueAloc: Cardinal;
  public
    constructor Create(Path: TPathRenderer);
    destructor Destroy; override;

    procedure Parse(Fname: ShortString);
    function Title: PAnsiChar;

    // XML event handlers
    procedure ParseAttr(Attr: PPAnsiChar); overload;
    procedure ParsePath(Attr: PPAnsiChar);
    procedure ParsePoly(Attr: PPAnsiChar; Close_flag: Boolean);
    procedure ParseRect(Attr: PPAnsiChar);
    procedure ParseLine(Attr: PPAnsiChar);
    procedure ParseStyle(Str: PAnsiChar);
    procedure ParseTransform(Str: PAnsiChar);

    function ParseMatrix(Str: PAnsiChar): Cardinal;
    function ParseTranslate(Str: PAnsiChar): Cardinal;
    function ParseRotate(Str: PAnsiChar): Cardinal;
    function ParseScale(Str: PAnsiChar): Cardinal;
    function ParseSkewX(Str: PAnsiChar): Cardinal;
    function ParseSkewY(Str: PAnsiChar): Cardinal;

    function ParseAttr(Name, Value: PAnsiChar): Boolean; overload;
    function ParseNameValue(Nv_start, Nv_end: PAnsiChar): Boolean;

    procedure CopyName(Start, End_: PAnsiChar);
    procedure CopyValue(Start, End_: PAnsiChar);
  end;

procedure StartElement(Data: Pointer; El: PAnsiChar; Attr: PPAnsiChar);
{$IFDEF EXPAT_WRAPPER}cdecl; {$ENDIF}
procedure EndElement(Data: Pointer; El: PAnsiChar);
{$IFDEF EXPAT_WRAPPER}cdecl; {$ENDIF}
procedure Content(Data: Pointer; S: PAnsiChar; Len: Integer);
{$IFDEF EXPAT_WRAPPER}cdecl; {$ENDIF}

implementation

type
  PNamedColor = ^TNamedColor;
  TNamedColor = record
    Name: array [0..21] of AnsiChar;
    R, G, B, A: Int8u;
  end;

const
  CColorsNum = 148;
  Colors: array [0..CColorsNum - 1] of TNamedColor = (
    (name: 'aliceblue'; R: 240; G: 248; B: 255; A: 255),
    (name: 'antiquewhite'; R: 250; G: 235; B: 215; A: 255),
    (name: 'aqua'; R: 0; G: 255; B: 255; A: 255),
    (name: 'aquamarine'; R: 127; G: 255; B: 212; A: 255),
    (name: 'azure'; R: 240; G: 255; B: 255; A: 255),
    (name: 'beige'; R: 245; G: 245; B: 220; A: 255),
    (name: 'bisque'; R: 255; G: 228; B: 196; A: 255),
    (name: 'black'; R: 0; G: 0; B: 0; A: 255),
    (name: 'blanchedalmond'; R: 255; G: 235; B: 205; A: 255),
    (name: 'blue'; R: 0; G: 0; B: 255; A: 255),
    (name: 'blueviolet'; R: 138; G: 43; B: 226; A: 255),
    (name: 'brown'; R: 165; G: 42; B: 42; A: 255),
    (name: 'burlywood'; R: 222; G: 184; B: 135; A: 255),
    (name: 'cadetblue'; R: 95; G: 158; B: 160; A: 255),
    (name: 'chartreuse'; R: 127; G: 255; B: 0; A: 255),
    (name: 'chocolate'; R: 210; G: 105; B: 30; A: 255),
    (name: 'coral'; R: 255; G: 127; B: 80; A: 255),
    (name: 'cornfLowerblue'; R: 100; G: 149; B: 237; A: 255),
    (name: 'cornsilk'; R: 255; G: 248; B: 220; A: 255),
    (name: 'crimson'; R: 220; G: 20; B: 60; A: 255),
    (name: 'cyan'; R: 0; G: 255; B: 255; A: 255),
    (name: 'darkblue'; R: 0; G: 0; B: 139; A: 255),
    (name: 'darkcyan'; R: 0; G: 139; B: 139; A: 255),
    (name: 'darkgoldenrod'; R: 184; G: 134; B: 11; A: 255),
    (name: 'darkgray'; R: 169; G: 169; B: 169; A: 255),
    (name: 'darkgreen'; R: 0; G: 100; B: 0; A: 255),
    (name: 'darkgrey'; R: 169; G: 169; B: 169; A: 255),
    (name: 'darkkhaki'; R: 189; G: 183; B: 107; A: 255),
    (name: 'darkmagenta'; R: 139; G: 0; B: 139; A: 255),
    (name: 'darkolivegreen'; R: 85; G: 107; B: 47; A: 255),
    (name: 'darkorange'; R: 255; G: 140; B: 0; A: 255),
    (name: 'darkorchid'; R: 153; G: 50; B: 204; A: 255),
    (name: 'darkred'; R: 139; G: 0; B: 0; A: 255),
    (name: 'darksalmon'; R: 233; G: 150; B: 122; A: 255),
    (name: 'darkseagreen'; R: 143; G: 188; B: 143; A: 255),
    (name: 'darkslateblue'; R: 72; G: 61; B: 139; A: 255),
    (name: 'darkslategray'; R: 47; G: 79; B: 79; A: 255),
    (name: 'darkslategrey'; R: 47; G: 79; B: 79; A: 255),
    (name: 'darkturquoise'; R: 0; G: 206; B: 209; A: 255),
    (name: 'darkviolet'; R: 148; G: 0; B: 211; A: 255),
    (name: 'deeppink'; R: 255; G: 20; B: 147; A: 255),
    (name: 'deepskyblue'; R: 0; G: 191; B: 255; A: 255),
    (name: 'dimgray'; R: 105; G: 105; B: 105; A: 255),
    (name: 'dimgrey'; R: 105; G: 105; B: 105; A: 255),
    (name: 'dodgerblue'; R: 30; G: 144; B: 255; A: 255),
    (name: 'firebrick'; R: 178; G: 34; B: 34; A: 255),
    (name: 'floralwhite'; R: 255; G: 250; B: 240; A: 255),
    (name: 'forestgreen'; R: 34; G: 139; B: 34; A: 255),
    (name: 'fuchsia'; R: 255; G: 0; B: 255; A: 255),
    (name: 'gainsboro'; R: 220; G: 220; B: 220; A: 255),
    (name: 'ghostwhite'; R: 248; G: 248; B: 255; A: 255),
    (name: 'gold'; R: 255; G: 215; B: 0; A: 255),
    (name: 'goldenrod'; R: 218; G: 165; B: 32; A: 255),
    (name: 'gray'; R: 128; G: 128; B: 128; A: 255),
    (name: 'green'; R: 0; G: 128; B: 0; A: 255),
    (name: 'greenyelLow'; R: 173; G: 255; B: 47; A: 255),
    (name: 'grey'; R: 128; G: 128; B: 128; A: 255),
    (name: 'honeydew'; R: 240; G: 255; B: 240; A: 255),
    (name: 'hotpink'; R: 255; G: 105; B: 180; A: 255),
    (name: 'indianred'; R: 205; G: 92; B: 92; A: 255),
    (name: 'indigo'; R: 75; G: 0; B: 130; A: 255),
    (name: 'ivory'; R: 255; G: 255; B: 240; A: 255),
    (name: 'khaki'; R: 240; G: 230; B: 140; A: 255),
    (name: 'lavender'; R: 230; G: 230; B: 250; A: 255),
    (name: 'lavenderblush'; R: 255; G: 240; B: 245; A: 255),
    (name: 'lawngreen'; R: 124; G: 252; B: 0; A: 255),
    (name: 'lemonchiffon'; R: 255; G: 250; B: 205; A: 255),
    (name: 'lightblue'; R: 173; G: 216; B: 230; A: 255),
    (name: 'lightcoral'; R: 240; G: 128; B: 128; A: 255),
    (name: 'lightcyan'; R: 224; G: 255; B: 255; A: 255),
    (name: 'lightgoldenrodyelLow'; R: 250; G: 250; B: 210; A: 255),
    (name: 'lightgray'; R: 211; G: 211; B: 211; A: 255),
    (name: 'lightgreen'; R: 144; G: 238; B: 144; A: 255),
    (name: 'lightgrey'; R: 211; G: 211; B: 211; A: 255),
    (name: 'lightpink'; R: 255; G: 182; B: 193; A: 255),
    (name: 'lightsalmon'; R: 255; G: 160; B: 122; A: 255),
    (name: 'lightseagreen'; R: 32; G: 178; B: 170; A: 255),
    (name: 'lightskyblue'; R: 135; G: 206; B: 250; A: 255),
    (name: 'lightslategray'; R: 119; G: 136; B: 153; A: 255),
    (name: 'lightslategrey'; R: 119; G: 136; B: 153; A: 255),
    (name: 'lightsteelblue'; R: 176; G: 196; B: 222; A: 255),
    (name: 'lightyelLow'; R: 255; G: 255; B: 224; A: 255),
    (name: 'lime'; R: 0; G: 255; B: 0; A: 255),
    (name: 'limegreen'; R: 50; G: 205; B: 50; A: 255),
    (name: 'linen'; R: 250; G: 240; B: 230; A: 255),
    (name: 'magenta'; R: 255; G: 0; B: 255; A: 255),
    (name: 'maroon'; R: 128; G: 0; B: 0; A: 255),
    (name: 'mediumaquamarine'; R: 102; G: 205; B: 170; A: 255),
    (name: 'mediumblue'; R: 0; G: 0; B: 205; A: 255),
    (name: 'mediumorchid'; R: 186; G: 85; B: 211; A: 255),
    (name: 'mediumpurple'; R: 147; G: 112; B: 219; A: 255),
    (name: 'mediumseagreen'; R: 60; G: 179; B: 113; A: 255),
    (name: 'mediumslateblue'; R: 123; G: 104; B: 238; A: 255),
    (name: 'mediumspringgreen'; R: 0; G: 250; B: 154; A: 255),
    (name: 'mediumturquoise'; R: 72; G: 209; B: 204; A: 255),
    (name: 'mediumvioletred'; R: 199; G: 21; B: 133; A: 255),
    (name: 'midnightblue'; R: 25; G: 25; B: 112; A: 255),
    (name: 'mintcream'; R: 245; G: 255; B: 250; A: 255),
    (name: 'mistyrose'; R: 255; G: 228; B: 225; A: 255),
    (name: 'moccasin'; R: 255; G: 228; B: 181; A: 255),
    (name: 'navajowhite'; R: 255; G: 222; B: 173; A: 255),
    (name: 'navy'; R: 0; G: 0; B: 128; A: 255),
    (name: 'oldlace'; R: 253; G: 245; B: 230; A: 255),
    (name: 'olive'; R: 128; G: 128; B: 0; A: 255),
    (name: 'olivedrab'; R: 107; G: 142; B: 35; A: 255),
    (name: 'orange'; R: 255; G: 165; B: 0; A: 255),
    (name: 'orangered'; R: 255; G: 69; B: 0; A: 255),
    (name: 'orchid'; R: 218; G: 112; B: 214; A: 255),
    (name: 'palegoldenrod'; R: 238; G: 232; B: 170; A: 255),
    (name: 'palegreen'; R: 152; G: 251; B: 152; A: 255),
    (name: 'paleturquoise'; R: 175; G: 238; B: 238; A: 255),
    (name: 'palevioletred'; R: 219; G: 112; B: 147; A: 255),
    (name: 'papayawhip'; R: 255; G: 239; B: 213; A: 255),
    (name: 'peachpuff'; R: 255; G: 218; B: 185; A: 255),
    (name: 'peru'; R: 205; G: 133; B: 63; A: 255),
    (name: 'pink'; R: 255; G: 192; B: 203; A: 255),
    (name: 'plum'; R: 221; G: 160; B: 221; A: 255),
    (name: 'powderblue'; R: 176; G: 224; B: 230; A: 255),
    (name: 'purple'; R: 128; G: 0; B: 128; A: 255),
    (name: 'red'; R: 255; G: 0; B: 0; A: 255),
    (name: 'rosybrown'; R: 188; G: 143; B: 143; A: 255),
    (name: 'royalblue'; R: 65; G: 105; B: 225; A: 255),
    (name: 'saddlebrown'; R: 139; G: 69; B: 19; A: 255),
    (name: 'salmon'; R: 250; G: 128; B: 114; A: 255),
    (name: 'sandybrown'; R: 244; G: 164; B: 96; A: 255),
    (name: 'seagreen'; R: 46; G: 139; B: 87; A: 255),
    (name: 'seashell'; R: 255; G: 245; B: 238; A: 255),
    (name: 'sienna'; R: 160; G: 82; B: 45; A: 255),
    (name: 'silver'; R: 192; G: 192; B: 192; A: 255),
    (name: 'skyblue'; R: 135; G: 206; B: 235; A: 255),
    (name: 'slateblue'; R: 106; G: 90; B: 205; A: 255),
    (name: 'slategray'; R: 112; G: 128; B: 144; A: 255),
    (name: 'slategrey'; R: 112; G: 128; B: 144; A: 255),
    (name: 'snow'; R: 255; G: 250; B: 250; A: 255),
    (name: 'springgreen'; R: 0; G: 255; B: 127; A: 255),
    (name: 'steelblue'; R: 70; G: 130; B: 180; A: 255),
    (name: 'tan'; R: 210; G: 180; B: 140; A: 255),
    (name: 'teal'; R: 0; G: 128; B: 128; A: 255),
    (name: 'thistle'; R: 216; G: 191; B: 216; A: 255),
    (name: 'tomato'; R: 255; G: 99; B: 71; A: 255),
    (name: 'turquoise'; R: 64; G: 224; B: 208; A: 255),
    (name: 'violet'; R: 238; G: 130; B: 238; A: 255),
    (name: 'wheat'; R: 245; G: 222; B: 179; A: 255),
    (name: 'white'; R: 255; G: 255; B: 255; A: 255),
    (name: 'whitesmoke'; R: 245; G: 245; B: 245; A: 255),
    (name: 'yelLow'; R: 255; G: 255; B: 0; A: 255),
    (name: 'yelLowgreen'; R: 154; G: 205; B: 50; A: 255),
    (name: 'zzzzzzzzzzz'; R: 0; G: 0; B: 0; A: 0));

  PageEqHigh: ShortString = #1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16 +
    #17#18#19#20#21#22#23#24#25#26#27#28#29#30#31#32 +
    #33#34#35#36#37#38#39#40#41#42#43#44#45#46#47#48 +
    #49#50#51#52#53#54#55#56#57#58#59#60#61#62#63#64 +
    #65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80 +
    #81#82#83#84#85#86#87#88#89#90#91#92#93#94#95#96 +
    #65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80 +
    #81#82#83#84#85#86#87#88#89#90#123#124#125#126#127#128 +
    #129#130#131#132#133#134#135#136#137#138#139#140#141#142#143#144 +
    #145#146#147#148#149#150#151#152#153#154#155#156#157#158#159#160 +
    #161#162#163#164#165#166#167#168#169#170#171#172#173#174#175#176 +
    #177#178#179#180#181#182#183#184#185#186#187#188#189#190#191#192 +
    #193#194#195#196#197#198#199#200#201#202#203#204#205#206#207#208 +
    #209#210#211#212#213#214#215#216#217#218#219#220#221#222#223#224 +
    #225#226#227#228#229#230#231#232#233#234#235#236#237#238#239#240 +
    #241#242#243#244#245#246#247#248#249#250#251#252#253#254#255;

procedure StartElement(Data: Pointer; El: PAnsiChar; Attr: PPAnsiChar);
var
  This: TParser;
begin
  This := TParser(Data);

  if CompareStr(AnsiString(El), 'title') = 0 then
    This.FTitleFlag := True
  else if CompareStr(AnsiString(El), 'g') = 0 then
  begin
    This.FPath.PushAttribute;
    This.ParseAttr(Attr);
  end
  else if CompareStr(AnsiString(El), 'path') = 0 then
  begin
    if This.FPathFlag then
      raise TSvgException.Create('start_element: Nested path');

    This.FPath.BeginPath;
    This.ParsePath(Attr);
    This.FPath.EndPath;

    This.FPathFlag := True;
  end
  else if CompareStr(AnsiString(El), 'rect') = 0 then
    This.ParseRect(Attr)
  else if CompareStr(AnsiString(El), 'line') = 0 then
    This.ParseLine(Attr)
  else if CompareStr(AnsiString(El), 'polyline') = 0 then
    This.ParsePoly(Attr, False)
  else if CompareStr(AnsiString(El), 'polygon') = 0 then
    This.ParsePoly(Attr, True);

  // else
  // if StrComp(PAnsiChar(el ) ,'<OTHER_ELEMENTS>' ) = 0 then
  // begin
  // end
  // ...
end;

procedure EndElement(Data: Pointer; El: PAnsiChar);
var
  This: TParser;
begin
  This := TParser(Data);

  if CompareStr(AnsiString(El), 'title') = 0 then
    This.FTitleFlag := False
  else if CompareStr(AnsiString(El), 'g') = 0 then
    This.FPath.PopAttribute
  else if CompareStr(AnsiString(El), 'path') = 0 then
    This.FPathFlag := False;

  // else
  // if CompareStr(AnsiString(el ) ,'<OTHER_ELEMENTS>' ) = 0 then
  // begin
  // end
  // ...
end;

procedure Content(Data: Pointer; S: PAnsiChar; Len: Integer);
var
  This: TParser;
begin
  This := TParser(Data);

  // FTitleFlag signals that the <title> tag is being parsed now.
  // The following code concatenates the pieces of content of the <title> tag.
  if This.FTitleFlag then
  begin
    if Len + This.FTitleLength > 255 then
      Len := 255 - This.FTitleLength;

    if Len > 0 then
    begin
      Move(S^, PAnsiChar(PtrComp(This.FTitle) + This.FTitleLength)^, Len);

      Inc(This.FTitleLength, Len);

      PAnsiChar(PtrComp(This.FTitle) + This.FTitleLength)^ := #0;
    end;
  end;
end;

function HexCardinal(Hexstr: PAnsiChar): Cardinal;

  function Xyint(X, Y: Integer): Integer;
  var
    F: Integer;
    M: Boolean;

  begin
    M := False;

    if Y < 0 then
    begin
      Y := Y * -1;
      M := True;
    end;

    Result := X;

    if Y > 1 then
      for F := 1 to Y - 1 do
        Result := Result * X;

    if M then
      Result := Result * -1;
  end;

var
  H            : ShortString;
  Fcb          : Byte;
  Yps, Mul, Num: Cardinal;

label
  Err, Esc;

const
  Hex: string[16] = '0123456789ABCDEF';

begin
  H := '';

  while Hexstr^ <> #0 do
  begin
    H := H + PageEqHigh[Byte(Hexstr^)];

    Inc(PtrComp(Hexstr));
  end;

  if Length(H) > 0 then
  begin
    case H[Length(H)] of
      '0'..'9', 'A'..'F':
      else
        goto Err;
    end;

    Num := Pos(H[Length(H)], Hex) - 1;
    Yps := 2;
    Mul := Xyint(4, Yps);

    if Length(H) > 1 then
      for Fcb := Length(H) - 1 downto 1 do
      begin
        case H[Fcb] of
          '0'..'9', 'A'..'F':
          else
            goto Err;
        end;

        Inc(Num, (Pos(H[Fcb], Hex) - 1) * Mul);
        Inc(Yps, 2);

        Mul := Xyint(4, Yps);
      end;

    goto Esc;
  end;

Err:
  Num := 0;

Esc:
  Result := Num;
end;

function ParseColor(Str: PAnsiChar): TAggColor;
var
  U: Cardinal;
  P: PNamedColor;
  M: ShortString;

begin
  while Str^ = ' ' do
    Inc(PtrComp(Str));

  if Str^ = '#' then
  begin
    Inc(PtrComp(Str));

    U := HexCardinal(Str);

    Result.Rgba8 := Rgb8Packed(U);

  end
  else
  begin
    P := nil;

    for U := 0 to CColorsNum - 1 do
      if CompareStr(AnsiString(Str), Colors[U].Name) = 0 then
      begin
        P := @Colors[U];

        Break;
      end;

    if P = nil then
    begin
      M := 'parseColor: Invalid color name ' + AnsiString(Str) + #0;

      raise TSvgException.Create(M);
    end;

    Result.FromRgbaInteger(P.R, P.G, P.B, P.A);
  end;
end;

function ParseDouble(Str: PAnsiChar): Double;
begin
  while Str^ = ' ' do
    Inc(PtrComp(Str));

  Result := GetDouble(Pointer(PAnsiChar(Str)));
end;

function IsLower(Ch: AnsiChar): Boolean;
begin
  case Ch of
    #97..#122:
      Result := True;

  else
    Result := False;
  end;
end;

function IsNumeric(Ch: AnsiChar): Boolean;
begin
  Result := Pos(Ch, '0123456789+-.eE') <> 0;
end;

function ParseTransformArgs(Str: PAnsiChar; Args: PDouble;
  Max_na: Cardinal; Na: PCardinal): Cardinal;
var
  Ptr, End_: PAnsiChar;
  PtrDouble : PDouble;
  Value: Double;
begin
  Na^ := 0;
  Ptr := Str;

  while (Ptr^ <> #0) and (Ptr^ <> '(') do
    Inc(PtrComp(Ptr));

  if Ptr^ = #0 then
    raise TSvgException.Create('ParseTransformArgs: Invalid syntax');

  End_ := Ptr;

  while (End_^ <> #0) and (End_^ <> ')') do
    Inc(PtrComp(End_));

  if End_^ = #0 then
    raise TSvgException.Create('ParseTransformArgs: Invalid syntax');

  while PtrComp(Ptr) < PtrComp(End_) do
    if IsNumeric(Ptr^) then
    begin
      if Na^ >= Max_na then
        raise TSvgException.Create('ParseTransformArgs: Too many arguments');

      Value := GetDouble(Ptr);
      PtrDouble := PDouble(PtrComp(Args) + Na^ * SizeOf(Double));
      PtrDouble^ := Value;

      Inc(Na^);

      while (PtrComp(Ptr) < PtrComp(End_)) and IsNumeric(Ptr^) do
        Inc(PtrComp(Ptr));
    end
    else
      Inc(PtrComp(Ptr));

  Result := Cardinal(PtrComp(End_) - PtrComp(Str));
end;


{ TParser }

constructor TParser.Create;
begin
  FPath := Path;

  FTokenizer := TPathTokenizer.Create;

  AggGetMem(Pointer(FBuffer), CAggBufferSize);
  AggGetMem(Pointer(FTitle), 256);

  FTitleLength := 0;
  FTitleFlag := False;
  FPathFlag := False;

  FAttrNameAloc := 128;
  FAttrValueAloc := 1024;

  AggGetMem(Pointer(FAttrName), FAttrNameAloc);
  AggGetMem(Pointer(FAttrValue), FAttrValueAloc);

  FAttrNameLen := 127;
  FAttrValueLen := 1023;

  FTitle^ := #0;
end;

destructor TParser.Destroy;
begin
  FTokenizer.Free;
  AggFreeMem(Pointer(FAttrValue), FAttrValueAloc);
  AggFreeMem(Pointer(FAttrName), FAttrNameAloc);
  AggFreeMem(Pointer(FTitle), 256);
  AggFreeMem(Pointer(FBuffer), CAggBufferSize);
  inherited;
end;

procedure TParser.Parse(Fname: ShortString);
var
  P : XML_Parser;
  Af: TApiFile;
  Ts: PAnsiChar;
  Done: Boolean;
  Len : Integer;
begin
  P := XML_ParserCreate(nil);

  if P = nil then
    raise TSvgException.Create('Couldn''t allocate memory for parser');

  XML_SetUserData(P, @Self);
  XML_SetElementHandler(P, @StartElement, @EndElement);
  XML_SetCharacterDataHandler(P, @Content);

  Fname := Fname + #0;

  Dec(Byte(Fname[0]));

  if not ApiOpenFile(Af, Fname) then
  begin
    XML_ParserFree(P);
    raise TSvgException.Create(Format('Couldn''t open file %s', [Fname[1]]));
  end;

  Done := False;

  repeat
    ApiReadFile(Af, FBuffer, CAggBufferSize, Len);

    Done := Len < CAggBufferSize;

    if XML_Parse(P, Pointer(FBuffer), Len, Integer(Done)) = XML_STATUS_ERROR then
    begin
      ApiCloseFile(Af);
      XML_ParserFree(P);

      raise TSvgException.Create(Format('%s at line %d'#13, [Cardinal(
        XML_ErrorString(XML_Error(XML_GetErrorCode(P)))),
        XML_GetCurrentLineNumber(P)]));
    end;

  until Done;

  ApiCloseFile(Af);
  XML_ParserFree(P);

  Ts := FTitle;

  while Ts^ <> #0 do
  begin
    if Byte(Ts^) < Byte(' ') then
      Ts^ := ' ';

    Inc(PtrComp(Ts));
  end;
end;

function TParser.Title;
begin
  Result := FTitle;
end;

procedure TParser.ParseAttr(Attr: PPAnsiChar);
var
  I: Integer;

begin
  I := 0;

  while PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^ <> nil do
  begin
    if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
      'style') = 0 then
      ParseStyle(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
        SizeOf(PAnsiChar))^)
    else
      ParseAttr(AggBasics.PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^,
        AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) * SizeOf(PAnsiChar))^);

    Inc(I, 2);
  end;
end;

procedure TParser.ParsePath;
var
  I: Integer;

  Tmp: array [0..3] of PAnsiChar;

begin
  I := 0;

  while PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^ <> nil do
  begin
    // The <path> tag can consist of the path itself ("d=")
    // as well as of other parameters like "style=", "transform=", etc.
    // In the last case we simply rely on the function of parsing
    // attributes (see 'else' branch).
    if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
      'd') = 0 then
    begin
      FTokenizer.SetPathStr(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
        SizeOf(PAnsiChar))^);

      FPath.ParsePath(FTokenizer);

    end
    else
    begin
      // Create a temporary single pair "name-value" in order
      // to avoid multiple calls for the same attribute.
      Tmp[0] := AggBasics.PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^;
      Tmp[1] := AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
        SizeOf(PAnsiChar))^;
      Tmp[2] := nil;
      Tmp[3] := nil;

      ParseAttr(@Tmp);
    end;

    Inc(I, 2);
  end;
end;

procedure TParser.ParsePoly;
var
  I: Integer;
  X, Y: Double;
begin
  X := 0.0;
  Y := 0.0;

  FPath.BeginPath;

  I := 0;

  while PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^ <> nil do
  begin
    if not ParseAttr(AggBasics.PPAnsiChar(PtrComp(Attr) + I *
      SizeOf(PAnsiChar))^, AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
      SizeOf(PAnsiChar))^) then
      if CompareStr(AnsiString(AggBasics.PPAnsiChar(PtrComp(Attr) + I *
        SizeOf(PAnsiChar))^), 'points') = 0 then
      begin
        FTokenizer.SetPathStr(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1)
          * SizeOf(PAnsiChar))^);

        if not FTokenizer.Next then
          raise TSvgException.Create('parse_poly: Too few coordinates');

        X := FTokenizer.LastNumber;

        if not FTokenizer.Next then
          raise TSvgException.Create('parse_poly: Too few coordinates');

        Y := FTokenizer.LastNumber;

        FPath.MoveTo(X, Y);

        while FTokenizer.Next do
        begin
          X := FTokenizer.LastNumber;

          if not FTokenizer.Next then
            raise TSvgException.Create('parse_poly: Odd number of coordinates');

          Y := FTokenizer.LastNumber;

          FPath.LineTo(X, Y);
        end;
      end;

    Inc(I, 2);
  end;

  FPath.EndPath;
end;

procedure TParser.ParseRect;
var
  I: Integer;
  X, Y, W, H: Double;
begin
  X := 0.0;
  Y := 0.0;
  W := 0.0;
  H := 0.0;

  FPath.BeginPath;

  I := 0;

  while PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^ <> nil do
  begin
    if not ParseAttr(AggBasics.PPAnsiChar(PtrComp(Attr) + I *
      SizeOf(PAnsiChar))^, AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
      SizeOf(PAnsiChar))^) then
    begin
      if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
        'x') = 0 then
        X := ParseDouble(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
          SizeOf(PAnsiChar))^);

      if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
        'y') = 0 then
        Y := ParseDouble(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
          SizeOf(PAnsiChar))^);

      if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
        'width') = 0 then
        W := ParseDouble(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
          SizeOf(PAnsiChar))^);

      if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
        'height') = 0 then
        H := ParseDouble(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
          SizeOf(PAnsiChar))^);

      // rx - to be implemented
      // ry - to be implemented
    end;

    Inc(I, 2);
  end;

  if (W <> 0.0) and (H <> 0.0) then
  begin
    if W < 0.0 then
      raise TSvgException.Create('parse_rect: Invalid width: ');

    if H < 0.0 then
      raise TSvgException.Create('parse_rect: Invalid height: ');

    FPath.MoveTo(X, Y);
    FPath.LineTo(X + W, Y);
    FPath.LineTo(X + W, Y + H);
    FPath.LineTo(X, Y + H);
    FPath.CloseSubpath;
  end;

  FPath.EndPath;
end;

procedure TParser.ParseLine;
var
  I: Integer;
  X1, Y1, X2, Y2: Double;
begin
  X1 := 0.0;
  Y1 := 0.0;
  X2 := 0.0;
  Y2 := 0.0;

  FPath.BeginPath;

  I := 0;

  while PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^ <> nil do
  begin
    if not ParseAttr(AggBasics.PPAnsiChar(PtrComp(Attr) + I *
      SizeOf(PAnsiChar))^, AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
      SizeOf(PAnsiChar))^) then
    begin
      if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
        'x1') = 0 then
        X1 := ParseDouble(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
          SizeOf(PAnsiChar))^);

      if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
        'y1') = 0 then
        Y1 := ParseDouble(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
          SizeOf(PAnsiChar))^);

      if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
        'x2') = 0 then
        X2 := ParseDouble(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
          SizeOf(PAnsiChar))^);

      if CompareStr(AnsiString(PPAnsiChar(PtrComp(Attr) + I * SizeOf(PAnsiChar))^),
        'y2') = 0 then
        Y2 := ParseDouble(AggBasics.PPAnsiChar(PtrComp(Attr) + (I + 1) *
          SizeOf(PAnsiChar))^);
    end;

    Inc(I, 2);
  end;

  FPath.MoveTo(X1, Y1);
  FPath.LineTo(X2, Y2);
  FPath.EndPath;
end;

procedure TParser.ParseStyle;
var
  Nv_start, Nv_end: PAnsiChar;
begin
  while Str^ <> #0 do
  begin
    // Left Trim
    while (Str^ <> #0) and (Str^ = ' ') do
      Inc(PtrComp(Str));

    Nv_start := Str;

    while (Str^ <> #0) and (Str^ <> ';') do
      Inc(PtrComp(Str));

    Nv_end := Str;

    // Right Trim
    while (PtrComp(Nv_end) > PtrComp(Nv_start)) and
      ((Nv_end^ = ';') or (Nv_end^ = ' ')) do
      Dec(PtrComp(Nv_end));

    Inc(PtrComp(Nv_end));

    ParseNameValue(Nv_start, Nv_end);

    if Str^ <> #0 then
      Inc(PtrComp(Str));
  end;
end;

procedure TParser.ParseTransform;
begin
  while Str^ <> #0 do
  begin
    if IsLower(Str^) then
      if StrLComp(PAnsiChar(Str), 'matrix', 6) = 0 then
        Inc(PtrComp(Str), ParseMatrix(Str))
      else if StrLComp(PAnsiChar(Str), 'translate', 9) = 0 then
        Inc(PtrComp(Str), ParseTranslate(Str))
      else if StrLComp(PAnsiChar(Str), 'rotate', 6) = 0 then
        Inc(PtrComp(Str), ParseRotate(Str))
      else if StrLComp(PAnsiChar(Str), 'scale', 5) = 0 then
        Inc(PtrComp(Str), ParseScale(Str))
      else if StrLComp(PAnsiChar(Str), 'skewX', 5) = 0 then
        Inc(PtrComp(Str), ParseSkewX(Str))
      else if StrLComp(PAnsiChar(Str), 'skewY', 5) = 0 then
        Inc(PtrComp(Str), ParseSkewY(Str))
      else
        Inc(PtrComp(Str))

    else
      Inc(PtrComp(Str));
  end;
end;

function TParser.ParseMatrix;
var
  Args: TAggParallelogram;
  Na, Len: Cardinal;
  Ta: TAggTransAffine;
begin
  Na := 0;
  Len := ParseTransformArgs(Str, @Args, 6, @Na);

  if Na <> 6 then
    raise TSvgException.Create('parse_matrix: Invalid number of arguments');

  Ta := TAggTransAffine.Create(Args[0], Args[1], Args[2], Args[3], Args[4],
    Args[5]);
  try
    FPath.Transform.PreMultiply(Ta);
  finally
    Ta.Free;
  end;

  Result := Len;
end;

function TParser.ParseTranslate;
var
  Args: array [0..1] of Double;
  Na, Len: Cardinal;
  Tat: TAggTransAffineTranslation;
begin
  Na := 0;
  Len := ParseTransformArgs(Str, @Args, 2, @Na);

  if Na = 1 then
    Args[1] := 0.0;

  Tat := TAggTransAffineTranslation.Create(Args[0], Args[1]);
  try
    FPath.Transform.PreMultiply(Tat);
  finally
    Tat.Free;
  end;

  Result := Len;
end;

function TParser.ParseRotate;
var
  Args: array [0..2] of Double;
  Na, Len: Cardinal;
  Tar: TAggTransAffineRotation;
  Tat, T: TAggTransAffineTranslation;
begin
  Na := 0;
  Len := ParseTransformArgs(Str, @Args, 3, @Na);

  if Na = 1 then
  begin
    Tar := TAggTransAffineRotation.Create(Deg2Rad(Args[0]));
    try
      FPath.Transform.PreMultiply(Tar);
    finally
      Tar.Free;
    end;
  end
  else if Na = 3 then
  begin
    T := TAggTransAffineTranslation.Create(-Args[1], -Args[2]);
    try
      Tar := TAggTransAffineRotation.Create(Deg2Rad(Args[0]));
      try
        T.Multiply(Tar);
      finally
        Tar.Free;
      end;

      Tat := TAggTransAffineTranslation.Create(Args[1], Args[2]);
      try
        T.Multiply(Tat);
      finally
        Tat.Free;
      end;
      FPath.Transform.PreMultiply(T);
    finally
      T.Free;
    end;
  end
  else
    raise TSvgException.Create('parse_rotate: Invalid number of arguments');

  Result := Len;
end;

function TParser.ParseScale;
var
  Args: array [0..1] of Double;
  Na, Len: Cardinal;
  Tas: TAggTransAffineScaling;
begin
  Na := 0;
  Len := ParseTransformArgs(Str, @Args, 2, @Na);

  if Na = 1 then
    Args[1] := Args[0];

  Tas := TAggTransAffineScaling.Create(Args[0], Args[1]);
  try
    FPath.Transform.PreMultiply(Tas);
  finally
    Tas.Free;
  end;

  Result := Len;
end;

function TParser.ParseSkewX;
var
  Arg: Double;
  Na, Len: Cardinal;
  Tas: TAggTransAffineSkewing;
begin
  Na := 0;
  Len := ParseTransformArgs(Str, @Arg, 1, @Na);

  Tas := TAggTransAffineSkewing.Create(Deg2Rad(Arg), 0.0);
  try
    FPath.Transform.PreMultiply(Tas);
  finally
    Tas.Free;
  end;

  Result := Len;
end;

function TParser.ParseSkewY;
var
  Arg: Double;
  Na, Len: Cardinal;
  Tas: TAggTransAffineSkewing;
begin
  Na := 0;
  Len := ParseTransformArgs(Str, @Arg, 1, @Na);

  Tas := TAggTransAffineSkewing.Create(0.0, Deg2Rad(Arg));
  try
    FPath.Transform.PreMultiply(Tas);
  finally
    Tas.Free
  end;

  Result := Len;
end;

function TParser.ParseAttr(Name, Value: PAnsiChar): Boolean;
var
  Clr: TAggColor;
begin
  Result := True;

  if CompareStr(AnsiString(name), 'style') = 0 then
    ParseStyle(Value)
  else if CompareStr(AnsiString(name), 'fill') = 0 then
    if CompareStr(AnsiString(Value), 'none') = 0 then
      FPath.FillNone
    else
    begin
      Clr := ParseColor(Value);

      FPath.SetFillColor(@Clr);
    end
  else if CompareStr(AnsiString(name), 'fill-opacity') = 0 then
    FPath.SetFillOpacity(ParseDouble(Value))
  else if CompareStr(AnsiString(name), 'stroke') = 0 then
    if CompareStr(AnsiString(Value), 'none') = 0 then
      FPath.StrokeNone
    else
    begin
      Clr := ParseColor(Value);

      FPath.SetStrokeColor(@Clr);
    end
  else if CompareStr(AnsiString(name), 'stroke-width') = 0 then
    FPath.SetStrokeWidth(ParseDouble(Value))
  else if CompareStr(AnsiString(name), 'stroke-linecap') = 0 then
  begin
    if CompareStr(AnsiString(Value), 'butt') = 0 then
      FPath.SetLineCap(lcButt)
    else if CompareStr(AnsiString(Value), 'round') = 0 then
      FPath.SetLineCap(lcRound)
    else if CompareStr(AnsiString(Value), 'square') = 0 then
      FPath.SetLineCap(lcSquare);
  end
  else if CompareStr(AnsiString(name), 'stroke-linejoin') = 0 then
  begin
    if CompareStr(AnsiString(Value), 'miter') = 0 then
      FPath.SetLineJoin(ljMiter)
    else if CompareStr(AnsiString(Value), 'round') = 0 then
      FPath.SetLineJoin(ljRound)
    else if CompareStr(AnsiString(Value), 'bevel') = 0 then
      FPath.SetLineJoin(ljBevel);
  end
  else if CompareStr(AnsiString(name), 'stroke-miterlimit') = 0 then
    FPath.SetMiterLimit(ParseDouble(Value))
  else if CompareStr(AnsiString(name), 'stroke-opacity') = 0 then
    FPath.SetStrokeOpacity(ParseDouble(Value))
  else if CompareStr(AnsiString(name), 'transform') = 0 then
    ParseTransform(Value)

    // else
    // if CompareStr(AnsiString(el ) ,'<OTHER_ATTRIBUTES>' ) = 0 then
    // begin
    // end
    // ...

  else
    Result := False;
end;

function TParser.ParseNameValue;
var
  Str, Val: PAnsiChar;
begin
  Str := Nv_start;

  while (PtrComp(Str) < PtrComp(Nv_end)) and (Str^ <> ':') do
    Inc(PtrComp(Str));

  Val := Str;

  // Right Trim
  while (PtrComp(Str) > PtrComp(Nv_start)) and ((Str^ = ':') or (Str^ = ' ')) do
    Dec(PtrComp(Str));

  Inc(PtrComp(Str));

  CopyName(Nv_start, Str);

  while (PtrComp(Val) < PtrComp(Nv_end)) and ((Val^ = ':') or (Val^ = ' ')) do
    Inc(PtrComp(Val));

  CopyValue(Val, Nv_end);

  Result := ParseAttr(PAnsiChar(FAttrName),
    PAnsiChar(FAttrValue));
end;

procedure TParser.CopyName;
var
  Len: Cardinal;
begin
  Len := PtrComp(End_) - PtrComp(Start);

  if (FAttrNameLen = 0) or (Len > FAttrNameLen) then
  begin
    AggFreeMem(Pointer(FAttrName), FAttrNameAloc);

    FAttrNameAloc := Len + 1;

    AggFreeMem(Pointer(FAttrName), FAttrNameAloc);

    FAttrNameLen := Len;
  end;

  if Len <> 0 then
    Move(Start^, FAttrName^, Len);

  PAnsiChar(PtrComp(FAttrName) + Len)^ := #0;
end;

procedure TParser.CopyValue;
var
  Len: Cardinal;
begin
  Len := PtrComp(End_) - PtrComp(Start);

  if (FAttrValueLen = 0) or (Len > FAttrValueLen) then
  begin
    AggFreeMem(Pointer(FAttrValue), FAttrValueAloc);

    FAttrValueAloc := Len + 1;

    AggGetMem(Pointer(FAttrValue), FAttrValueAloc);

    FAttrValueLen := Len;
  end;

  if Len <> 0 then
    Move(Start^, FAttrValue^, Len);

  PAnsiChar(PtrComp(FAttrValue) + Len)^ := #0;
end;

end.
