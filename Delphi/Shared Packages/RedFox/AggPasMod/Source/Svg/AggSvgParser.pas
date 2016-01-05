unit AggSvgParser;

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

{-$DEFINE EXPAT_WRAPPER}
{$I AggCompiler.inc}

uses
  SysUtils,
  Expat,
  AggBasics,
  AggColor,
  AggSvgPathTokenizer,
  AggSvgPathRenderer,
  AggSvgException,
  AggTransAffine,
  AggMathStroke,
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

    procedure Parse(FileName: ShortString);
    function Title: PAnsiChar;

    // XML event handlers
    procedure ParseAttr(Attr: PPAnsiChar); overload;
    procedure ParsePath(Attr: PPAnsiChar);
    procedure ParsePoly(Attr: PPAnsiChar; CloseFlag: Boolean);
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
    function ParseNameValue(NameValueStart, NameValueStop: PAnsiChar): Boolean;

    procedure CopyName(Start, Stop: PAnsiChar);
    procedure CopyValue(Start, Stop: PAnsiChar);
  end;

procedure StartElement(Data: Pointer; El: PAnsiChar; Attr: PPAnsiChar);
{$IFDEF EXPAT_WRAPPER}cdecl; {$ENDIF}
procedure EndElement(Data: Pointer; El: PAnsiChar);
{$IFDEF EXPAT_WRAPPER}cdecl; {$ENDIF}
procedure Content(Data: Pointer; S: PAnsiChar; Len: Integer);
{$IFDEF EXPAT_WRAPPER}cdecl; {$ENDIF}

implementation

resourcestring
  RCStrInvalidCharacter = 'Invalid character (%d)';
  RCStrStartElementNestedPath = 'StartElement: Nested path';
  RCStrParseTransformArgs = 'ParseTransformArgs: Invalid syntax';
  RCStrParseTransformTooManyArgs = 'ParseTransformArgs: Too many arguments';
  RCStrCouldntAllocateMemory = 'Couldn''t allocate memory for parser';
  RCStrCouldntOpenFile = 'Couldn''t open file %s';
  RCStrSAtLineD = '%s at line %d'#13;
  RCStrParsePolyTooFewCoords = 'ParsePoly: Too few coordinates';
  RCStrParsePolyOddNumber = 'ParsePoly: Odd number of coordinates';
  RCStrParseRectInvalidWidth = 'ParseRect: Invalid width: %d';
  RCStrParseRectInvalidHeight = 'ParseRect: Invalid height: %d';
  RCStrParseMatrixInvalid = 'ParseMatrix: Invalid number of arguments';
  RCStrParseRotateInvalid = 'ParseRotate: Invalid number of arguments';
  RCStrParseColorInvalidColorName = 'ParseColor: Invalid color name %s'#0;

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
    (name: 'cornflowerblue'; R: 100; G: 149; B: 237; A: 255),
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
    (name: 'greenyellow'; R: 173; G: 255; B: 47; A: 255),
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
    (name: 'lightgoldenrodyellow'; R: 250; G: 250; B: 210; A: 255),
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
    (name: 'lightyellow'; R: 255; G: 255; B: 224; A: 255),
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
    (name: 'yellow'; R: 255; G: 255; B: 0; A: 255),
    (name: 'yellowgreen'; R: 154; G: 205; B: 50; A: 255),
    (name: 'zzzzzzzzzzz'; R: 0; G: 0; B: 0; A: 0));

  PageEqHigh: ShortString = #1#2#3#4#5#6#7#8#9#10#11#12#13#14#15#16#17#18#19 +
    #20#21#22#23#24#25#26#27#28#29#30#31#32#33#34#35#36#37#38#39#40#41#42#43 +
    #44#45#46#47#48#49#50#51#52#53#54#55#56#57#58#59#60#61#62#63#64#65#66#67 +
    #68#69#70#71#72#73#74#75#76#77#78#79#80#81#82#83#84#85#86#87#88#89#90#91 +
    #92#93#94#95#96#65#66#67#68#69#70#71#72#73#74#75#76#77#78#79#80#81#82#83 +
    #84#85#86#87#88#89#90#123#124#125#126#127#128#129#130#131#132#133#134 +
    #135#136#137#138#139#140#141#142#143#144#145#146#147#148#149#150#151#152 +
    #153#154#155#156#157#158#159#160#161#162#163#164#165#166#167#168#169#170 +
    #171#172#173#174#175#176#177#178#179#180#181#182#183#184#185#186#187#188 +
    #189#190#191#192#193#194#195#196#197#198#199#200#201#202#203#204#205#206 +
    #207#208#209#210#211#212#213#214#215#216#217#218#219#220#221#222#223#224 +
    #225#226#227#228#229#230#231#232#233#234#235#236#237#238#239#240#241#242 +
    #243#244#245#246#247#248#249#250#251#252#253#254#255;

procedure StartElement(Data: Pointer; El: PAnsiChar; Attr: PPAnsiChar);
var
  This: TParser;
begin
  This := TParser(Data^);

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
      raise TSvgException.Create(RCStrStartElementNestedPath);

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
  // if StrComp(PAnsiChar(El), '<OTHER_ELEMENTS>') = 0 then
  // begin
  // end
  // ...
end;

procedure EndElement(Data: Pointer; El: PAnsiChar);
var
  This: TParser;
begin
  This := TParser(Data^);

  if CompareStr(AnsiString(El), 'title') = 0 then
    This.FTitleFlag := False
  else if CompareStr(AnsiString(El), 'g') = 0 then
    This.FPath.PopAttribute
  else if CompareStr(AnsiString(El), 'path') = 0 then
    This.FPathFlag := False;

  // else
  // if CompareStr(AnsiString(El), '<OTHER_ELEMENTS>') = 0 then
  // begin
  // end
  // ...
end;

procedure Content(Data: Pointer; S: PAnsiChar; Len: Integer);
var
  This: TParser;
begin
  This := TParser(Data^);

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
  H: ShortString;
  Fcb: Byte;
  Yps, Mul: Cardinal;

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
    if not CharInSet(H[Length(H)], ['0'..'9', 'A'..'F']) then
      raise TSvgException.CreateFmt(RCStrInvalidCharacter, [H[Length(H)]]);

    Result := Pos(H[Length(H)], Hex) - 1;
    Yps := 2;
    Mul := Xyint(4, Yps);

    if Length(H) > 1 then
      for Fcb := Length(H) - 1 downto 1 do
      begin
        if not CharInSet(H[Fcb], ['0'..'9', 'A'..'F']) then
          raise TSvgException.CreateFmt(RCStrInvalidCharacter, [H[Fcb]]);

        Inc(Result, (Pos(H[Fcb], Hex) - 1) * Mul);
        Inc(Yps, 2);

        Mul := Xyint(4, Yps);
      end;

    Exit;
  end;
end;

function ParseColor(Str: PAnsiChar): TAggColor;
var
  U: Cardinal;
  P: PNamedColor;
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
      raise TSvgException.CreateFmt(RCStrParseColorInvalidColorName, [Str]);

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
  Result := CharInSet(Ch, [#97..#122]);
end;

function IsNumeric(Ch: AnsiChar): Boolean;
begin
  Result := Pos(Ch, '0123456789+-.eE') <> 0;
end;

function ParseTransformArgs(Str: PAnsiChar; Args: PDouble;
  Max_na: Cardinal; Na: PCardinal): Cardinal;
var
  Ptr, Stop: PAnsiChar;
  PtrDouble : PDouble;
  Value: Double;
begin
  Na^ := 0;
  Ptr := Str;

  while (Ptr^ <> #0) and (Ptr^ <> '(') do
    Inc(PtrComp(Ptr));

  if Ptr^ = #0 then
    raise TSvgException.Create(RCStrParseTransformArgs);

  Stop := Ptr;

  while (Stop^ <> #0) and (Stop^ <> ')') do
    Inc(PtrComp(Stop));

  if Stop^ = #0 then
    raise TSvgException.Create(RCStrParseTransformArgs);

  while PtrComp(Ptr) < PtrComp(Stop) do
    if IsNumeric(Ptr^) then
    begin
      if Na^ >= Max_na then
        raise TSvgException.Create(RCStrParseTransformTooManyArgs);

      Value := GetDouble(Ptr);
      PtrDouble := PDouble(PtrComp(Args) + Na^ * SizeOf(Double));
      PtrDouble^ := Value;

      Inc(Na^);

      while (PtrComp(Ptr) < PtrComp(Stop)) and IsNumeric(Ptr^) do
        Inc(PtrComp(Ptr));
    end
    else
      Inc(PtrComp(Ptr));

  Result := Cardinal(PtrComp(Stop) - PtrComp(Str));
end;


{ TParser }

constructor TParser.Create(Path: TPathRenderer);
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

procedure TParser.Parse(FileName: ShortString);
var
  P : TXmlParser;
  Af: TApiFile;
  Ts: PAnsiChar;
  Done: Boolean;
  Len : Integer;
begin
  P := XmlParserCreate(nil);

  if P = nil then
    raise TSvgException.Create(RCStrCouldntAllocateMemory);

  XmlSetUserData(P, @Self);
  XmlSetElementHandler(P, @StartElement, @EndElement);
  XmlSetCharacterDataHandler(P, @Content);

  FileName := FileName + #0;

  Dec(Byte(FileName[0]));

  if not ApiOpenFile(Af, FileName) then
  begin
    XmlParserFree(P);
    raise TSvgException.CreateFmt(RCStrCouldntOpenFile, [FileName[1]]);
  end;

  Done := False;

  repeat
    ApiReadFile(Af, FBuffer, CAggBufferSize, Len);

    Done := Len < CAggBufferSize;

    if XmlParse(P, Pointer(FBuffer), Len, Integer(Done)) = xsError then
    begin
      ApiCloseFile(Af);
      XmlParserFree(P);

      raise TSvgException.CreateFmt(RCStrSAtLineD, [Cardinal(
        XmlErrorString(TXmlError(XmlGetErrorCode(P)))),
        XmlGetCurrentLineNumber(P)]);
    end;
  until Done;

  ApiCloseFile(Af);
  XmlParserFree(P);

  Ts := FTitle;

  while Ts^ <> #0 do
  begin
    if Byte(Ts^) < Byte(' ') then
      Ts^ := ' ';

    Inc(PtrComp(Ts));
  end;
end;

function TParser.Title: PAnsiChar;
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

procedure TParser.ParsePath(Attr: PPAnsiChar);
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

procedure TParser.ParsePoly(Attr: PPAnsiChar; CloseFlag: Boolean);
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
          raise TSvgException.Create(RCStrParsePolyTooFewCoords);

        X := FTokenizer.LastNumber;

        if not FTokenizer.Next then
          raise TSvgException.Create(RCStrParsePolyTooFewCoords);

        Y := FTokenizer.LastNumber;

        FPath.MoveTo(X, Y);

        while FTokenizer.Next do
        begin
          X := FTokenizer.LastNumber;

          if not FTokenizer.Next then
            raise TSvgException.Create(RCStrParsePolyOddNumber);

          Y := FTokenizer.LastNumber;

          FPath.LineTo(X, Y);
        end;
      end;

    Inc(I, 2);
  end;

  FPath.EndPath;
end;

procedure TParser.ParseRect(Attr: PPAnsiChar);
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
      raise TSvgException.CreateFmt(RCStrParseRectInvalidWidth, [W]);

    if H < 0.0 then
      raise TSvgException.CreateFmt(RCStrParseRectInvalidHeight, [H]);

    FPath.MoveTo(X, Y);
    FPath.LineTo(X + W, Y);
    FPath.LineTo(X + W, Y + H);
    FPath.LineTo(X, Y + H);
    FPath.CloseSubpath;
  end;

  FPath.EndPath;
end;

procedure TParser.ParseLine(Attr: PPAnsiChar);
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

procedure TParser.ParseStyle(Str: PAnsiChar);
var
  NameValueStart, NameValueStop: PAnsiChar;
begin
  while Str^ <> #0 do
  begin
    // Left Trim
    while (Str^ <> #0) and (Str^ = ' ') do
      Inc(PtrComp(Str));

    NameValueStart := Str;

    while (Str^ <> #0) and (Str^ <> ';') do
      Inc(PtrComp(Str));

    NameValueStop := Str;

    // Right Trim
    while (PtrComp(NameValueStop) > PtrComp(NameValueStart)) and
      ((NameValueStop^ = ';') or (NameValueStop^ = ' ')) do
      Dec(PtrComp(NameValueStop));

    Inc(PtrComp(NameValueStop));

    ParseNameValue(NameValueStart, NameValueStop);

    if Str^ <> #0 then
      Inc(PtrComp(Str));
  end;
end;

procedure TParser.ParseTransform(Str: PAnsiChar);
begin
  while Str^ <> #0 do
  begin
    case Str^ of
      'm':
        if StrLComp(PAnsiChar(Str), 'matrix', 6) = 0 then
          Inc(PtrComp(Str), ParseMatrix(Str));
      't':
        if StrLComp(PAnsiChar(Str), 'translate', 9) = 0 then
          Inc(PtrComp(Str), ParseTranslate(Str));
      'r':
        if StrLComp(PAnsiChar(Str), 'rotate', 6) = 0 then
          Inc(PtrComp(Str), ParseRotate(Str));
      's':
        if StrLComp(PAnsiChar(Str), 'scale', 5) = 0 then
          Inc(PtrComp(Str), ParseScale(Str))
        else if StrLComp(PAnsiChar(Str), 'skewX', 5) = 0 then
          Inc(PtrComp(Str), ParseSkewX(Str))
        else if StrLComp(PAnsiChar(Str), 'skewY', 5) = 0 then
          Inc(PtrComp(Str), ParseSkewY(Str));
      else
        Inc(PtrComp(Str));
    end;
  end;
end;

function TParser.ParseMatrix(Str: PAnsiChar): Cardinal;
var
  Args: TAggParallelogram;
  Na, Len: Cardinal;
  Ta: TAggTransAffine;
begin
  Na := 0;
  Len := ParseTransformArgs(Str, @Args, 6, @Na);

  if Na <> 6 then
    raise TSvgException.Create(RCStrParseMatrixInvalid);

  Ta := TAggTransAffine.Create(Args[0], Args[1], Args[2], Args[3], Args[4],
    Args[5]);
  try
    FPath.Transform.PreMultiply(Ta);
  finally
    Ta.Free;
  end;

  Result := Len;
end;

function TParser.ParseTranslate(Str: PAnsiChar): Cardinal;
var
  Args: array [0..1] of Double;
  Na, Len: Cardinal;
  Tat: TAggTransAffineTranslation;
begin
  Na := 0;
  Len := ParseTransformArgs(Str, @Args, 2, @Na);

  if Na = 1 then
    Args[1] := 0.0;

  FPath.Transform.InitializeTransforms;
  FPath.Transform.Translate(Args[0], Args[1]);

  Result := Len;
end;

function TParser.ParseRotate(Str: PAnsiChar): Cardinal;
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
      T.Rotate(Deg2Rad(Args[0]));
      T.Translate(Args[1], Args[2]);
      FPath.Transform.PreMultiply(T);
    finally
      T.Free;
    end;
  end
  else
    raise TSvgException.Create(RCStrParseRotateInvalid);

  Result := Len;
end;

function TParser.ParseScale(Str: PAnsiChar): Cardinal;
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

function TParser.ParseSkewX(Str: PAnsiChar): Cardinal;
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

function TParser.ParseSkewY(Str: PAnsiChar): Cardinal;
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
    FPath.FillOpacity := ParseDouble(Value)
  else if CompareStr(AnsiString(name), 'stroke') = 0 then
    if CompareStr(AnsiString(Value), 'none') = 0 then
      FPath.StrokeNone
    else
    begin
      Clr := ParseColor(Value);

      FPath.SetStrokeColor(@Clr);
    end
  else if CompareStr(AnsiString(name), 'stroke-width') = 0 then
    FPath.StrokeWidth := ParseDouble(Value)
  else if CompareStr(AnsiString(name), 'stroke-linecap') = 0 then
  begin
    if CompareStr(AnsiString(Value), 'butt') = 0 then
      FPath.LineCap := lcButt
    else if CompareStr(AnsiString(Value), 'round') = 0 then
      FPath.LineCap := lcRound
    else if CompareStr(AnsiString(Value), 'square') = 0 then
      FPath.LineCap := lcSquare;
  end
  else if CompareStr(AnsiString(name), 'stroke-linejoin') = 0 then
  begin
    if CompareStr(AnsiString(Value), 'miter') = 0 then
      FPath.LineJoin := ljMiter
    else if CompareStr(AnsiString(Value), 'round') = 0 then
      FPath.LineJoin := ljRound
    else if CompareStr(AnsiString(Value), 'bevel') = 0 then
      FPath.LineJoin := ljBevel;
  end
  else if CompareStr(AnsiString(name), 'stroke-miterlimit') = 0 then
    FPath.MiterLimit := ParseDouble(Value)
  else if CompareStr(AnsiString(name), 'stroke-opacity') = 0 then
    FPath.StrokeOpacity := ParseDouble(Value)
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

function TParser.ParseNameValue(NameValueStart, NameValueStop: PAnsiChar): Boolean;
var
  Str, Val: PAnsiChar;
begin
  Str := NameValueStart;

  while (PtrComp(Str) < PtrComp(NameValueStop)) and (Str^ <> ':') do
    Inc(PtrComp(Str));

  Val := Str;

  // Right Trim
  while (PtrComp(Str) > PtrComp(NameValueStart)) and ((Str^ = ':') or (Str^ = ' ')) do
    Dec(PtrComp(Str));

  Inc(PtrComp(Str));

  CopyName(NameValueStart, Str);

  while (PtrComp(Val) < PtrComp(NameValueStop)) and ((Val^ = ':') or (Val^ = ' ')) do
    Inc(PtrComp(Val));

  CopyValue(Val, NameValueStop);

  Result := ParseAttr(PAnsiChar(FAttrName),
    PAnsiChar(FAttrValue));
end;

procedure TParser.CopyName(Start, Stop: PAnsiChar);
var
  Len: Cardinal;
begin
  Len := PtrComp(Stop) - PtrComp(Start);

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

procedure TParser.CopyValue(Start, Stop: PAnsiChar);
var
  Len: Cardinal;
begin
  Len := PtrComp(Stop) - PtrComp(Start);

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
