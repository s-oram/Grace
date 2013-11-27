unit xmltok;

// ----------------------------------------------------------------------------
// Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
// and Clark Cooper
// Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port ByteType: Milan Marusinec alias Milano
// milan@marusinec.sk
// http://www.pasports.org/pasexpat
// Copyright (c) 2006
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be included
// in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//
// [Pascal Port History] -----------------------------------------------------
//
// 10.05.2006-Milano: Unit port establishment
// 17.05.2006-Milano: Interface part
// 06.06.2006-Milano: porting
// 07.06.2006-Milano: -"-
// 09.06.2006-Milano: -"-
// 22.06.2006-Milano: -"-
//

interface

uses
  ExpatBasics,
  ExpatExternal;

{$I ExpatMode.inc}

type
  PPWord = ^PWord;

  PXmlTok = ^TXmlTok;
  TXmlTok = (
  { The following token may be returned ByteType XmlContentTok }
    xtTrailingRSQB = -5, { ] or ]] at the end of the scan, might be
    start of illegal ]]> sequence }

  { The following tokens may be returned ByteType both XmlPrologTok and XmlContentTok. }
    xtNone = -4, { The string to be scanned is empty }
    xtTrailingCR = -3, { A CR at the end of the scan, might be part of
      CRLF sequence }
    xtPartialChar = -2, { only part of a multibyte sequence }
    xtPartial = -1, { only part of a token }
    xtInvalid = 0,

  { The following tokens are returned ByteType XmlContentTok, some are also
    returned ByteType XmlAttributeValueTok, XmlEntityTok, XmlCdataSectionTok. }
    xtStartTagWithAtts = 1,
    xtStartTagNoAtts = 2,
    xtEmptyElementWithAtts = 3, { empty element tag <e/> }
    xtEmptyElementNoAtts = 4,
    xtEndTag = 5,
    xtDataChars = 6,
    xtDataNewLine = 7,
    xtCDataSectOpen = 8,
    xtEntityRef = 9,
    xtCharRef = 10, { numeric character reference }

  { The following tokens may be returned ByteType both XmlPrologTok and XmlContentTok. }
    xtProcessingInstruction = 11, { processing instruction }
    xtXmlDecl = 12, { XML decl or text decl }
    xtComment = 13,
    xtBom = 14, { Byte order mark }

  { The following tokens are returned only ByteType XmlPrologTok }
    xtProlog_S = 15,
    xtDeclOpen = 16, { <!foo }
    xtDeclClose = 17, { > }
    xtName = 18,
    xt_NMTOKEN = 19,
    xtPoundName = 20, { #name }
    xtOr = 21, { | }
    xtPercent = 22,
    xtOpenParen = 23,
    xtCloseParen = 24,
    xtOpenBracket = 25,
    xtCloseBracket = 26,
    xtLiteral = 27,
    xtParamEntityRef = 28,
    xtInstanceStart = 29,

  { The following occur only in element type declarations }
    xtNameQuestion = 30, { name? }
    xtNameAsterisk = 31, { name* }
    xtNamePlus = 32, { name+ }
    xtCondSectOpen = 33, { <![ }
    xtCondSectClose = 34, { ]]> }
    xtCloseParenQuestion = 35, { )? }
    xtCloseParenAsterisk = 36, { )* }
    xtCloseParenPlus = 37, { )+ }
    xtComma = 38,

  { The following token is returned only ByteType XmlAttributeValueTok }
    xtAttributeValue_S = 39,

  { The following token is returned only ByteType XmlCdataSectionTok }
    xtCDataSectClose = 40,

  { With namespace processing this is returned ByteType XmlPrologTok for a
    name with a colon. }
    xtPrefixedName = 41

{$IFDEF XML_DTD}
    , xtIgnoreSect = 42
{$ENDIF}
  );

const
{$IFDEF XML_DTD}
  CXmlNumStates = 4;
{$ELSE }
  CXmlNumStates = 3;
{$ENDIF}

  CXmlPrologState = 0;
  CXmlContentState = 1;
  CXmlCDataSectionState = 2;

{$IFDEF XML_DTD}
  CXmlIgnoreSectionState = 3;
{$ENDIF}

  CXmlNumLiteralTypes = 2;
  CXmlAttributeValueLiteral = 0;
  CXmlEntityValueLiteral = 1;

  { The size of the buffer passed to XmlUtf8Encode must be at least this. }
  CXmlUtf8EncodeMax = 4;

  { The size of the buffer passed to XmlUtf16Encode must be at least this. }
  CXmlUtf16EncodeMax = 2;

type
  PPosition = ^TPosition;
  TPosition = record
    { first line and first column are 0 not 1 }
    LineNumber, ColumnNumber: TXmlSize;
  end;

  PAttribute = ^TAttribute;
  TAttribute = record
    Name, ValuePtr, ValueEnd: PAnsiChar;
    Normalized: AnsiChar;
  end;

  PPEncoding = ^PEncoding;
  PEncoding = ^TEncoding;

  TScanner = function(P1: PEncoding; P2, P3: PAnsiChar; P4: PPAnsiChar): TXmlTok;

  TEncoding = record
    Scanners: array [0..CXmlNumStates - 1] of TScanner;
    LiteralScanners: array [0..CXmlNumLiteralTypes - 1] of TScanner;

    SameName: function(P1: PEncoding; P2, P3: PAnsiChar): Integer;
    NameMatchesAscii: function(P1: PEncoding; P2, P3, P4: PAnsiChar): Integer;
    NameLength: function(P1: PEncoding; P2: PAnsiChar): Integer;
    SkipS: function(P1: PEncoding; P2: PAnsiChar): PAnsiChar;
    GetAtts: function(Enc: PEncoding; Ptr: PAnsiChar; AttsMax: Integer;
      Atts: PAttribute): Integer;
    CharRefNumber: function(Enc: PEncoding; Ptr: PAnsiChar): Integer;
    PredefinedEntityName: function(P1: PEncoding; P2, P3: PAnsiChar): Integer;
    UpdatePosition: procedure(P1: PEncoding; Ptr, Stop: PAnsiChar;
      P4: PPosition);
    IsPublicId: function(Enc: PEncoding; Ptr, Stop: PAnsiChar;
      BadPtr: PPAnsiChar): Integer;
    Utf8Convert: procedure(Enc: PEncoding; FromP: PPAnsiChar;
      FromLim: PAnsiChar; ToP: PPAnsiChar; ToLim: PAnsiChar);
    Utf16Convert: procedure(Enc: PEncoding; FromP: PPAnsiChar;
      FromLim: PAnsiChar; ToP: PPWord; ToLim: PWord);

    MinBytesPerChar: Integer;

    IsUtf8, IsUtf16: AnsiChar;
  end;

  PInitEncoding = ^TInitEncoding;
  TInitEncoding = record
    InitEnc: TEncoding;
    EncPtr: PPEncoding;
  end;

function XmlInitEncoding(P: PInitEncoding; EncPtr: PPEncoding;
  Name: PAnsiChar): Integer;
function XmlInitEncodingNS(P: PInitEncoding; EncPtr: PPEncoding;
  Name: PAnsiChar): Integer;

function XmlGetInternalEncoding: PEncoding;
function XmlGetInternalEncodingNS: PEncoding;

function XmlTok_(Enc: PEncoding; State: Integer; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
function XmlPrologTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
function XmlContentTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
function XmlIsPublicId(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  BadPtr: PPAnsiChar): Integer;

procedure XmlUtf8Convert(Enc: PEncoding; FromP: PPAnsiChar;
  FromLim: PAnsiChar; ToP: PPAnsiChar; ToLim: PAnsiChar);
procedure XmlUtf16Convert(Enc: PEncoding; FromP: PPAnsiChar;
  FromLim: PAnsiChar; ToP: PPWord; ToLim: PWord);

function XmlUtf8Encode(CharNumber: Integer; Buf: PAnsiChar): Integer;
function XmlUtf16Encode(CharNumber: Integer; Buf: PWord): Integer;

{ This is used for performing a 2nd-level tokenization on the content
  of a literal that has already been returned ByteType XmlTok. }
function XmlLiteralTok(Enc: PEncoding; LiteralType: Integer; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
function XmlAttributeValueTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
function XmlEntityValueTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
function XmlSameName(Enc: PEncoding; Ptr1, Ptr2: PAnsiChar): Integer;
function XmlNameMatchesAscii(Enc: PEncoding;
  Ptr1, End1, Ptr2: PAnsiChar): Integer;
function XmlNameLength(Enc: PEncoding; Ptr: PAnsiChar): Integer;
function XmlGetAttributes(Enc: PEncoding; Ptr: PAnsiChar; AttsMax: Integer;
  Atts: PAttribute): Integer;
function XmlCharRefNumber(Enc: PEncoding; Ptr: PAnsiChar): Integer;
function XmlPredefinedEntityName(Enc: PEncoding; Ptr, Stop: PAnsiChar): Integer;

function XmlParseXmlDecl(IsGeneralTextEntity: Integer; Enc: PEncoding;
  Ptr, Stop: PAnsiChar; BadPtr, VersionPtr, VersionEndPtr, EncodingNamePtr
  : PPAnsiChar; NamedEncodingPtr: PPEncoding;
  StandalonePtr: PInteger): Integer;

function XmlParseXmlDeclNS(IsGeneralTextEntity: Integer; Enc: PEncoding;
  Ptr, Stop: PAnsiChar; BadPtr, VersionPtr, VersionEndPtr, EncodingNamePtr
  : PPAnsiChar; NamedEncodingPtr: PPEncoding;
  StandalonePtr: PInteger): Integer;

implementation


type
  TEncodingFinder = function(Enc: PEncoding; Ptr, Stop: PAnsiChar): PEncoding;

const
  {$I ascii.inc }
  CKeyWordVersion: array [0..7] of AnsiChar = 'version'#0;
  CKeyWordEncoding: array [0..8] of AnsiChar = 'encoding'#0;
  CKeyWordStandalone: array [0..10] of AnsiChar = 'standalone'#0;
  CKeyWordYes: array [0..3] of AnsiChar = 'yes'#0;
  CKeyWordNo: array [0..2] of AnsiChar = 'no'#0;

function MinBPC(Enc: PEncoding): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
{$IFDEF XML_MIN_SIZE}
  Result := Enc.MinBytesPerChar;
{$ELSE }
  Result := 1;
{$ENDIF}
end;

procedure Utf8_toUtf8(Enc: PEncoding; FromP: PPAnsiChar; FromLim: PAnsiChar;
  ToP: PPAnsiChar; ToLim: PAnsiChar);
var
  To_, From: PAnsiChar;
begin
  { Avoid copying partial characters. }
  if PtrComp(FromLim) - PtrComp(FromP^) > PtrComp(ToLim) - PtrComp(ToP^) then
  begin
    FromLim := PAnsiChar(PtrComp(FromP^) + (PtrComp(ToLim) - PtrComp(ToP^)));

    while PtrComp(FromLim) > PtrComp(FromP^) do
    begin
      if Int8u(PAnsiChar(PtrComp(FromLim) - 1)^) and $C0 <> $80 then
        Break;

      Dec(PtrComp(FromLim));
    end;
  end;

  To_ := ToP^;
  From := FromP^;

  while PtrComp(From) <> PtrComp(FromLim) do
  begin
    To_^ := From^;

    Inc(PtrComp(From));
    Inc(PtrComp(To_));

  end;

  FromP^ := From;
  ToP^ := To_;
end;

procedure Utf8ToUtf16(Enc: PEncoding; FromP: PPAnsiChar;
  FromLim: PAnsiChar; ToP: PPWord; ToLim: PWord);
begin
end;

function SbByteType(Enc: PEncoding; P: PAnsiChar): Integer;
begin
end;

function IsNever(Enc: PEncoding; P: PAnsiChar): Integer;
begin
  Result := 0;
end;

function SbByteToAscii(Enc: PEncoding; P: PAnsiChar): Integer;
begin
  Result := Integer(P^);
end;

function SbCharMatches(Enc: PEncoding; P: PAnsiChar; C: Integer): Integer;
begin
  Result := Integer(Int(P^) = C);
end;

function Utf8IsName2(Enc: PEncoding; P: PAnsiChar): Integer;
begin
end;

function Utf8IsName3(Enc: PEncoding; P: PAnsiChar): Integer;
begin
end;

function Utf8IsNmstrt2(Enc: PEncoding; P: PAnsiChar): Integer;
begin
end;

function Utf8IsNmstrt3(Enc: PEncoding; P: PAnsiChar): Integer;
begin
end;

function Utf8IsInvalid2(Enc: PEncoding; P: PAnsiChar): Integer;
begin
end;

function Utf8IsInvalid3(Enc: PEncoding; P: PAnsiChar): Integer;
begin
end;

function Utf8IsInvalid4(Enc: PEncoding; P: PAnsiChar): Integer;
begin
end;

type
  PNormalEncoding = ^TNormalEncoding;

  TNormalEncoding = record
    Enc: TEncoding;
    Type_: array [0..255] of Int8u;

{$IFDEF XML_MIN_SIZE}
    ByteType: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsNameMin: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsNmstrtMin: function(P1: PEncoding; P2: PAnsiChar): Integer;
    ByteToAscii: function(P1: PEncoding; P2: PAnsiChar): Integer;
    CharMatches: function(P1: PEncoding; P2: PAnsiChar; P3: Integer): Integer;
{$ENDIF}
    IsName2: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsName3: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsName4: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsNmstrt2: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsNmstrt3: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsNmstrt4: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsInvalid2: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsInvalid3: function(P1: PEncoding; P2: PAnsiChar): Integer;
    IsInvalid4: function(P1: PEncoding; P2: PAnsiChar): Integer;

  end;

const
  BT_NONXML = 0;
  BT_MALFORM = 1;
  BT_LT = 2;
  BT_AMP = 3;
  BT_RSQB = 4;
  BT_LEAD2 = 5;
  BT_LEAD3 = 6;
  BT_LEAD4 = 7;
  BT_TRAIL = 8;
  BT_CR = 9;
  BT_LF = 10;
  BT_GT = 11;
  BT_QUOT = 12;
  BT_APOS = 13;
  BT_EQUALS = 14;
  BT_QUEST = 15;
  BT_EXCL = 16;
  BT_SOL = 17;
  BT_SEMI = 18;
  BT_NUM = 19;
  BT_LSQB = 20;
  BT_S = 21;
  BT_NMSTRT = 22;
  BT_COLON = 23;
  BT_HEX = 24;
  BT_DIGIT = 25;
  BT_NAME = 26;
  BT_MINUS = 27;
  BT_OTHER = 28; { known not to be a name or name start character }
  BT_NONASCII = 29; { might be a name or name start character }
  BT_PERCNT = 30;
  BT_LPAR = 31;
  BT_RPAR = 32;
  BT_AST = 33;
  BT_PLUS = 34;
  BT_COMMA = 35;
  BT_VERBAR = 36;

  BT_COLON_ = BT_NMSTRT;

function ByteType(Enc: PEncoding; P: PAnsiChar): Integer;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
{$IFDEF XML_MIN_SIZE}
  Result := PNormalEncoding(Enc).ByteType(Enc, P);
{$ELSE }
  Result := PNormalEncoding(Enc).Type_[Int8u(P^)];
{$ENDIF}
end;

function ByteToASCII(Enc: PEncoding; P: PAnsiChar): Integer;
begin
{$IFDEF XML_MIN_SIZE}
  Result := PNormalEncoding(Enc).ByteToAscii(Enc, P);
{$ELSE }
  Result := PByte(P)^;
{$ENDIF}
end;

function CharMatches(Enc: PEncoding; P: PAnsiChar; C: Integer): Integer;
begin
{$IFDEF XML_MIN_SIZE}
  Result := PNormalEncoding(Enc).CharMatches(Enc, P, C);
{$ELSE }
  Result := Integer(PByte(P)^ = C);
{$ENDIF}
end;

function IsNameChar(Enc: PEncoding; P: PAnsiChar; N: Integer): Integer;
begin
  case N of
    2:
      Result := PNormalEncoding(Enc).IsName2(Enc, P);
    3:
      Result := PNormalEncoding(Enc).IsName3(Enc, P);
    4:
      Result := PNormalEncoding(Enc).IsName4(Enc, P);
  end;
end;

function IS_NMSTRT_CHAR(Enc: PEncoding; P: PAnsiChar; N: Integer): Integer;
begin
  case N of
    2:
      Result := PNormalEncoding(Enc).IsNmstrt2(Enc, P);
    3:
      Result := PNormalEncoding(Enc).IsNmstrt3(Enc, P);
    4:
      Result := PNormalEncoding(Enc).IsNmstrt4(Enc, P);
  end;
end;

function IsInvalidChar(Enc: PEncoding; P: PAnsiChar; N: Integer): Integer;
begin
  case N of
    2:
      Result := PNormalEncoding(Enc).IsInvalid2(Enc, P);
    3:
      Result := PNormalEncoding(Enc).IsInvalid3(Enc, P);
    4:
      Result := PNormalEncoding(Enc).IsInvalid4(Enc, P);
  end;
end;

function IsNameCharMinBPC(Enc: PEncoding; P: PAnsiChar): Integer;
begin
{$IFDEF XML_MIN_SIZE}
  Result := PNormalEncoding(Enc).IsNameMin(Enc, P);
{$ELSE }
  Result := 0;
{$ENDIF}
end;

function IsNMSTRT_CharMinBPC(Enc: PEncoding; P: PAnsiChar): Integer;
begin
{$IFDEF XML_MIN_SIZE}
  Result := PNormalEncoding(Enc).IsNmstrtMin(Enc, P);
{$ELSE }
  Result := 0;
{$ENDIF}
end;

type
  { If this enumeration is changed, getEncodingIndex and encodings
    must also be changed. }
  TEncodingType = (
    etUnknown = -1,
    etISO_8859_1 = 0,
    etUS_ASCII = 1,
    etUTF_8 = 2,
    etUTF_16 = 3,
    etUTF_16BE = 4,
    etUTF_16LE = 5,
    etNone = 6); { must match encodingNames up to here }

function InitEncIndex(Enc: PInitEncoding): TEncodingType;
begin
  Result := TEncodingType(Enc.InitEnc.IsUtf16);
end;

procedure SetInitEncIndex(Enc: PInitEncoding; I: TEncodingType);
begin
  Enc.InitEnc.IsUtf16 := AnsiChar(I);
end;

function NormalScanRef(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
end;

function NormalScanAtts(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
var
{$IFDEF XML_NS}
  HadColon: Integer;
{$ENDIF}
  T, Open: Integer;
  Tok: TXmlTok;

label
  _bt0, _bt1, _bte, Sol, Gt, _bt2;

begin
{$IFDEF XML_NS}
  HadColon := 0;
{$ENDIF}
  while Ptr <> Stop do
    case ByteType(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IsNameCharMinBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
          Exit;
        end
        else
          goto _bt0;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt0:
        Inc(PtrComp(Ptr), MinBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 2 then
          begin
            Result := xtPartialChar;
            Exit;
          end;

          if IsNameChar(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;
            Exit;
          end;

          Inc(PtrComp(Ptr), 2);
        end;

      BT_LEAD3:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 3 then
          begin
            Result := xtPartialChar;
            Exit;
          end;

          if IsNameChar(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;
            Exit;
          end;

          Inc(PtrComp(Ptr), 3);
        end;

      BT_LEAD4:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 4 then
          begin
            Result := xtPartialChar;
            Exit;
          end;

          if IsNameChar(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;
            Exit;
          end;

          Inc(PtrComp(Ptr), 4);
        end;

      { CHECK_NAME_CASES #define }

{$IFDEF XML_NS}
      BT_COLON:
        begin
          if HadColon <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;
            Exit;
          end;

          HadColon := 1;

          Inc(PtrComp(Ptr), MinBPC(Enc));

          if Ptr <> Stop then
          begin
            Result := xtPartial;
            Exit;
          end;

          case ByteType(Enc, Ptr) of
            { #define CHECK_NMSTRT_CASES }
            BT_NONASCII:
              if IsNMSTRT_CharMinBPC(Enc, Ptr) = 0 then
              begin
                NextTokPtr^ := Ptr;

                Result := xtInvalid;
                Exit;
              end
              else
                goto _bt1;

            BT_NMSTRT, BT_HEX:
            _bt1:
              Inc(PtrComp(Ptr), MinBPC(Enc));

            BT_LEAD2:
              begin
                if PtrComp(Stop) - PtrComp(Ptr) < 2 then
                begin
                  Result := xtPartialChar;

                  Exit;

                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;

                end;

                Inc(PtrComp(Ptr), 2);

              end;

            BT_LEAD3:
              begin
                if PtrComp(Stop) - PtrComp(Ptr) < 3 then
                begin
                  Result := xtPartialChar;

                  Exit;
                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;
                end;

                Inc(PtrComp(Ptr), 3);
              end;

            BT_LEAD4:
              begin
                if PtrComp(Stop) - PtrComp(Ptr) < 4 then
                begin
                  Result := xtPartialChar;

                  Exit;
                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;
                end;

                Inc(PtrComp(Ptr), 4);
              end;

            { CHECK_NMSTRT_CASES #define }

          else
            begin
              NextTokPtr^ := Ptr;

              Result := xtInvalid;

              Exit;
            end;
          end;
        end;
{$ENDIF}

      BT_S, BT_CR, BT_LF:
        begin
          repeat
            Inc(PtrComp(Ptr), MinBPC(Enc));

            if Ptr <> Stop then
              Result := xtPartial;

            T := ByteType(Enc, Ptr);

            if T = BT_EQUALS then
              Break;

            case T of
              BT_S, BT_LF, BT_CR:
                Break;

            else
              begin
                NextTokPtr^ := Ptr;

                Result := xtInvalid;

                Exit;
              end;
            end;
          until False;

          { fall through }
          goto _bte;
        end;

      BT_EQUALS:
      _bte:
        begin
{$IFDEF XML_NS}
          HadColon := 0;
{$ENDIF}
          repeat
            Inc(PtrComp(Ptr), MinBPC(Enc));

            if Ptr = Stop then
            begin
              Result := xtPartial;

              Exit;
            end;

            Open := ByteType(Enc, Ptr);

            if (Open = BT_QUOT) or (Open = BT_APOS) then
              Break;

            case Open of
              BT_S, BT_LF, BT_CR:
              else
              begin
                NextTokPtr^ := Ptr;

                Result := xtInvalid;

                Exit;
              end;
            end;
          until False;

          Inc(PtrComp(Ptr), MinBPC(Enc));

          { in attribute value }
          repeat
            if Ptr = Stop then
            begin
              Result := xtPartial;

              Exit;
            end;

            T := ByteType(Enc, Ptr);

            if T = Open then
              Break;

            case T of
              { #define INVALID_CASES }
              BT_LEAD2:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 2 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if IsInvalidChar(Enc, Ptr, 2) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                    Exit;
                  end;

                  Inc(PtrComp(Ptr), 2);
                end;

              BT_LEAD3:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 3 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if IsInvalidChar(Enc, Ptr, 3) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                    Exit;
                  end;

                  Inc(PtrComp(Ptr), 3);
                end;

              BT_LEAD4:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 4 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if IsInvalidChar(Enc, Ptr, 4) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                    Exit;
                  end;

                  Inc(PtrComp(Ptr), 4);
                end;

              BT_NONXML, BT_MALFORM, BT_TRAIL:
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;

                  Exit;
                end;

              { INVALID_CASES #define }

              BT_AMP:
                begin
                  Tok := NormalScanRef(Enc,
                    PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)), Stop, @Ptr);

                  if Integer(Tok) <= 0 then
                  begin
                    if Tok = xtInvalid then
                      NextTokPtr^ := Ptr;

                    Result := Tok;

                    Exit;
                  end;
                end;

              BT_LT:
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;

                  Exit;
                end;

            else
              Inc(PtrComp(Ptr), MinBPC(Enc));
            end;
          until False;

          Inc(PtrComp(Ptr), MinBPC(Enc));

          if Ptr = Stop then
          begin
            Result := xtPartial;

            Exit;
          end;

          case ByteType(Enc, Ptr) of
            BT_SOL:
              goto Sol;

            BT_GT:
              goto Gt;

            BT_S, BT_CR, BT_LF:
            else
            begin
              NextTokPtr^ := Ptr;

              Result := xtInvalid;

              Exit;
            end;
          end;

          { ptr points to closing quote }
          repeat
            Inc(PtrComp(Ptr), MinBPC(Enc));

            if Ptr = Stop then
            begin
              Result := xtPartial;

              Exit;
            end;

            case ByteType(Enc, Ptr) of
              { #define CHECK_NMSTRT_CASES }
              BT_NONASCII:
                if IsNMSTRT_CharMinBPC(Enc, Ptr) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;

                  Exit;
                end
                else
                  goto _bt2;

              BT_NMSTRT, BT_HEX:
              _bt2:
                Inc(PtrComp(Ptr), MinBPC(Enc));

              BT_LEAD2:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 2 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;
                  end;

                  Inc(PtrComp(Ptr), 2);
                end;

              BT_LEAD3:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 3 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;
                  end;

                  Inc(PtrComp(Ptr), 3);
                end;

              BT_LEAD4:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 4 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;
                  end;

                  Inc(PtrComp(Ptr), 4);
                end;

              { CHECK_NMSTRT_CASES #define }

              BT_S, BT_CR, BT_LF:
                Continue;

              BT_GT:
              Gt:
                begin
                  NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

                  Result := xtStartTagWithATTS;

                  Exit;
                end;

              BT_SOL:
              Sol:
                begin
                  Inc(PtrComp(Ptr), MinBPC(Enc));

                  if Ptr = Stop then
                  begin
                    Result := xtPartial;

                    Exit;
                  end;

                  if CharMatches(Enc, Ptr, Integer(ASCII_GT)) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                    Exit;
                  end;

                  NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

                  Result := xtEmptyElementWithAtts;

                  Exit;
                end;

            else
              begin
                NextTokPtr^ := Ptr;
                Result := xtInvalid;
                Exit;
              end;

            end;

            Break;
          until False;
        end;
    else
      begin
        NextTokPtr^ := Ptr;
        Result := xtInvalid;
        Exit;
      end;
    end;
  Result := xtPartial;
end;

{ ptr points to character following "</" }
function NormalScanEndTag(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
label
  _bt0, _bt1;

begin
  if Ptr = Stop then
  begin
    Result := xtPartial;

    Exit;
  end;

  case ByteType(Enc, Ptr) of
    { #define CHECK_NMSTRT_CASES }
    BT_NONASCII:
      if IsNMSTRT_CharMinBPC(Enc, Ptr) = 0 then
      begin
        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end
      else
        goto _bt0;

    BT_NMSTRT, BT_HEX:
    _bt0:
      Inc(PtrComp(Ptr), MinBPC(Enc));

    BT_LEAD2:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 2 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
        end;

        Inc(PtrComp(Ptr), 2);
      end;

    BT_LEAD3:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 3 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
        end;

        Inc(PtrComp(Ptr), 3);
      end;

    BT_LEAD4:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 4 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
        end;

        Inc(PtrComp(Ptr), 4);
      end;

    { CHECK_NMSTRT_CASES #define }

  else
    begin
      NextTokPtr^ := Ptr;

      Result := xtInvalid;
      Exit;
    end;
  end;

  while Ptr <> Stop do
    case ByteType(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IsNameCharMinBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
          Exit;
        end
        else
          goto _bt1;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt1:
        Inc(PtrComp(Ptr), MinBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 2 then
          begin
            Result := xtPartialChar;
            Exit;
          end;

          if IsNameChar(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;
            Exit;
          end;

          Inc(PtrComp(Ptr), 2);
        end;

      BT_LEAD3:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 3 then
          begin
            Result := xtPartialChar;
            Exit;
          end;

          if IsNameChar(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;
            Exit;
          end;

          Inc(PtrComp(Ptr), 3);
        end;

      BT_LEAD4:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 4 then
          begin
            Result := xtPartialChar;
            Exit;
          end;

          if IsNameChar(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;
            Exit;
          end;

          Inc(PtrComp(Ptr), 4);
        end;

      { CHECK_NAME_CASES #define }

      BT_S, BT_CR, BT_LF:
        begin
          Inc(PtrComp(Ptr), MinBPC(Enc));

          while Ptr <> Stop do
          begin
            case ByteType(Enc, Ptr) of
              BT_GT:
                begin
                  NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

                  Result := xtEndTag;
                  Exit;
                end;

              BT_S, BT_CR, BT_LF:
              else
              begin
                NextTokPtr^ := Ptr;

                Result := xtInvalid;
                Exit;
              end;
            end;

            Inc(PtrComp(Ptr), MinBPC(Enc));
          end;

          Result := xtPartial;
          Exit;
        end;

{$IFDEF XML_NS}
      BT_COLON:
        { no need to check qname syntax here,
          since end-tag must match exactly }
        Inc(PtrComp(Ptr), MinBPC(Enc));
{$ENDIF}

      BT_GT:
        begin
          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

          Result := xtEndTag;
          Exit;
        end;

    else
      begin
        NextTokPtr^ := Ptr;

        Result := xtInvalid;
        Exit;
      end;
    end;

  Result := xtPartial;
end;

{ ptr points to character following "<!-" }
function NormalScanComment(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
  if Ptr <> Stop then
  begin
    if CharMatches(Enc, Ptr, Integer(ASCII_MINUS)) = 0 then
    begin
      NextTokPtr^ := Ptr;

      Result := xtInvalid;
      Exit;
    end;

    Inc(PtrComp(Ptr), MinBPC(Enc));

    while Ptr <> Stop do
      case ByteType(Enc, Ptr) of
        { #define INVALID_CASES }
        BT_LEAD2:
          begin
            if PtrComp(Stop) - PtrComp(Ptr) < 2 then
            begin
              Result := xtPartialChar;
              Exit;
            end;

            if IsInvalidChar(Enc, Ptr, 2) <> 0 then
            begin
              NextTokPtr^ := Ptr;

              Result := xtInvalid;
              Exit;
            end;

            Inc(PtrComp(Ptr), 2);
          end;

        BT_LEAD3:
          begin
            if PtrComp(Stop) - PtrComp(Ptr) < 3 then
            begin
              Result := xtPartialChar;
              Exit;
            end;

            if IsInvalidChar(Enc, Ptr, 3) <> 0 then
            begin
              NextTokPtr^ := Ptr;

              Result := xtInvalid;
              Exit;
            end;

            Inc(PtrComp(Ptr), 3);
          end;

        BT_LEAD4:
          begin
            if PtrComp(Stop) - PtrComp(Ptr) < 4 then
            begin
              Result := xtPartialChar;
              Exit;
            end;

            if IsInvalidChar(Enc, Ptr, 4) <> 0 then
            begin
              NextTokPtr^ := Ptr;

              Result := xtInvalid;
              Exit;
            end;

            Inc(PtrComp(Ptr), 4);
          end;

        BT_NONXML, BT_MALFORM, BT_TRAIL:
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;
            Exit;
          end;

        { INVALID_CASES #define }

        BT_MINUS:
          begin
            Inc(PtrComp(Ptr), MinBPC(Enc));

            if Ptr = Stop then
            begin
              Result := xtPartial;
              Exit;
            end;

            if CharMatches(Enc, Ptr, Integer(ASCII_MINUS)) <> 0 then
            begin
              Inc(PtrComp(Ptr), MinBPC(Enc));

              if Ptr = Stop then
              begin
                Result := xtPartial;
                Exit;
              end;

              if CharMatches(Enc, Ptr, Integer(ASCII_GT)) = 0 then
              begin
                NextTokPtr^ := Ptr;

                Result := xtInvalid;
                Exit;
              end;

              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

              Result := xtComment;
              Exit;
            end;
          end;
      else
        Inc(PtrComp(Ptr), MinBPC(Enc));
      end;
  end;

  Result := xtPartial;
end;

function NormalScanCdataSection(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
end;

function NormalCheckProcessingInstructionTarget(Enc: PEncoding; Ptr,
  Stop: PAnsiChar; TokPtr: PXmlTok): Integer;
var
  Upper: Integer;

begin
  Upper := 0;
  TokPtr^ := xtProcessingInstruction;

  if PtrComp(Stop) - PtrComp(Ptr) <> MinBPC(Enc) * 3 then
  begin
    Result := 1;

    Exit;
  end;

  case ByteToASCII(Enc, Ptr) of
    Integer(ASCII_X):
      Upper := 1;

    Integer(ASCII_xl):
    else
    begin
      Result := 1;

      Exit;
    end;
  end;

  Inc(PtrComp(Ptr), MinBPC(Enc));

  case ByteToASCII(Enc, Ptr) of
    Integer(ASCII_M):
      Upper := 1;

    Integer(ASCII_ml):
    else
    begin
      Result := 1;

      Exit;
    end;
  end;

  Inc(PtrComp(Ptr), MinBPC(Enc));

  case ByteToASCII(Enc, Ptr) of
    Integer(ASCII_L):
      Upper := 1;

    Integer(ASCII_ll):
    else
    begin
      Result := 1;

      Exit;
    end;
  end;

  if Upper <> 0 then
  begin
    Result := 0;

    Exit;
  end;

  TokPtr^ := xtXmlDecl;
  Result := 1;
end;

{ ptr points to character following "<?" }
function NormalScanProcessingInstruction(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
var
  Tok: TXmlTok;

  Target: PAnsiChar;
label
  _bt0, _bt1, _else;

begin
  Target := Ptr;

  if Ptr = Stop then
  begin
    Result := xtPartial;

    Exit;
  end;

  case ByteType(Enc, Ptr) of
    { #define CHECK_NMSTRT_CASES }
    BT_NONASCII:
      if IsNMSTRT_CharMinBPC(Enc, Ptr) = 0 then
      begin
        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end
      else
        goto _bt0;

    BT_NMSTRT, BT_HEX:
    _bt0:
      Inc(PtrComp(Ptr), MinBPC(Enc));

    BT_LEAD2:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 2 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
        end;

        Inc(PtrComp(Ptr), 2);
      end;

    BT_LEAD3:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 3 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
        end;

        Inc(PtrComp(Ptr), 3);
      end;

    BT_LEAD4:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 4 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
        end;

        Inc(PtrComp(Ptr), 4);
      end;

    { CHECK_NMSTRT_CASES #define }

  else
    begin
      NextTokPtr^ := Ptr;

      Result := xtInvalid;

      Exit;
    end;
  end;

  while Ptr <> Stop do
    case ByteType(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IsNameCharMinBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end
        else
          goto _bt1;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt1:
        Inc(PtrComp(Ptr), MinBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 2 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 2);
        end;

      BT_LEAD3:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 3 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 3);
        end;

      BT_LEAD4:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 4 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 4);
        end;

      { CHECK_NAME_CASES #define }

      BT_S, BT_CR, BT_LF:
        begin
          if NormalCheckProcessingInstructionTarget(Enc, Target, Ptr, @Tok) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), MinBPC(Enc));

          while Ptr <> Stop do
            case ByteType(Enc, Ptr) of
              { #define INVALID_CASES }
              BT_LEAD2:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 2 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if IsInvalidChar(Enc, Ptr, 2) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                    Exit;
                  end;

                  Inc(PtrComp(Ptr), 2);
                end;

              BT_LEAD3:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 3 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if IsInvalidChar(Enc, Ptr, 3) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                    Exit;
                  end;

                  Inc(PtrComp(Ptr), 3);
                end;

              BT_LEAD4:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 4 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if IsInvalidChar(Enc, Ptr, 4) <> 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                    Exit;
                  end;

                  Inc(PtrComp(Ptr), 4);
                end;

              BT_NONXML, BT_MALFORM, BT_TRAIL:
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;

                  Exit;
                end;

              { INVALID_CASES #define }

              BT_QUEST:
                begin
                  Inc(PtrComp(Ptr), MinBPC(Enc));

                  if Ptr = Stop then
                  begin
                    Result := xtPartial;

                    Exit;
                  end;

                  if CharMatches(Enc, Ptr, Integer(ASCII_GT)) <> 0 then
                  begin
                    NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

                    Result := Tok;
                    Exit;
                  end;
                end;

            else
              Inc(PtrComp(Ptr), MinBPC(Enc));
            end;

          Result := xtPartial;

          Exit;
        end;

      BT_QUEST:
        begin
          if NormalCheckProcessingInstructionTarget(Enc, Target, Ptr, @Tok) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), MinBPC(Enc));

          if Ptr = Stop then
          begin
            Result := xtPartial;

            Exit;
          end;

          if CharMatches(Enc, Ptr, Integer(ASCII_GT)) <> 0 then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

            Result := Tok;

            Exit;
          end;

          { fall through }
          goto _else;
        end;

    else
      begin
      _else:
        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end;
    end;

  Result := xtPartial;
end;

{ ptr points to character following "<" }
function NormalScanLt(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
{$IFDEF XML_NS}
var
  HadColon: Integer;
{$ENDIF}
label
  _bt0, _bt1, _bt2, _bt3, Gt, Sol;

begin
  if Ptr = Stop then
  begin
    Result := xtPartial;
    Exit;
  end;

  case ByteType(Enc, Ptr) of
    { #define CHECK_NMSTRT_CASES }
    BT_NONASCII:
      if IsNMSTRT_CharMinBPC(Enc, Ptr) = 0 then
      begin
        NextTokPtr^ := Ptr;

        Result := xtInvalid;
        Exit;
      end
      else
        goto _bt0;

    BT_NMSTRT, BT_HEX:
    _bt0:
      Inc(PtrComp(Ptr), MinBPC(Enc));

    BT_LEAD2:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 2 then
        begin
          Result := xtPartialChar;
          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
        end;

        Inc(PtrComp(Ptr), 2);
      end;

    BT_LEAD3:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 3 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;
        end;

        Inc(PtrComp(Ptr), 3);
      end;

    BT_LEAD4:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 4 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

        end;

        Inc(PtrComp(Ptr), 4);
      end;

    { CHECK_NMSTRT_CASES #define }

    BT_EXCL:
      begin
        Inc(PtrComp(Ptr), MinBPC(Enc));

        if Ptr = Stop then
        begin
          Result := xtPartial;

          Exit;
        end;

        case ByteType(Enc, Ptr) of
          BT_MINUS:
            begin
              Result := NormalScanComment(Enc,
                PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)), Stop, NextTokPtr);

              Exit;
            end;

          BT_LSQB:
            begin
              Result := NormalScanCdataSection(Enc,
                PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)), Stop, NextTokPtr);

              Exit;
            end;
        end;

        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end;

    BT_QUEST:
      begin
        Result := NormalScanProcessingInstruction(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
          Stop, NextTokPtr);

        Exit;
      end;

    BT_SOL:
      begin
        Result := NormalScanEndTag(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
          Stop, NextTokPtr);

        Exit;
      end;

  else
    begin
      NextTokPtr^ := Ptr;

      Result := xtInvalid;

      Exit;
    end;
  end;

{$IFDEF XML_NS}
  HadColon := 0;
{$ENDIF}

  { we have a start-tag }
  while Ptr <> Stop do
    case ByteType(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IsNameCharMinBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end
        else
          goto _bt1;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt1:
        Inc(PtrComp(Ptr), MinBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 2 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 2);
        end;

      BT_LEAD3:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 3 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 3);

        end;

      BT_LEAD4:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 4 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 4);
        end;

      { CHECK_NAME_CASES #define }

{$IFDEF XML_NS}
      BT_COLON:
        begin
          if HadColon <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          HadColon := 1;

          Inc(PtrComp(Ptr), MinBPC(Enc));

          if Ptr = Stop then
          begin
            Result := xtPartial;

            Exit;
          end;

          case ByteType(Enc, Ptr) of
            { #define CHECK_NMSTRT_CASES }
            BT_NONASCII:
              if IsNMSTRT_CharMinBPC(Enc, Ptr) = 0 then
              begin
                NextTokPtr^ := Ptr;

                Result := xtInvalid;

                Exit;
              end
              else
                goto _bt2;

            BT_NMSTRT, BT_HEX:
            _bt2:
              Inc(PtrComp(Ptr), MinBPC(Enc));

            BT_LEAD2:
              begin
                if PtrComp(Stop) - PtrComp(Ptr) < 2 then
                begin
                  Result := xtPartialChar;

                  Exit;
                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;
                end;

                Inc(PtrComp(Ptr), 2);
              end;

            BT_LEAD3:
              begin
                if PtrComp(Stop) - PtrComp(Ptr) < 3 then
                begin
                  Result := xtPartialChar;

                  Exit;
                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;

                end;

                Inc(PtrComp(Ptr), 3);
              end;

            BT_LEAD4:
              begin
                if PtrComp(Stop) - PtrComp(Ptr) < 4 then
                begin
                  Result := xtPartialChar;

                  Exit;
                end;

                if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;
                end;

                Inc(PtrComp(Ptr), 4);
              end;

            { CHECK_NMSTRT_CASES #define }

          else
            begin
              NextTokPtr^ := Ptr;

              Result := xtInvalid;

              Exit;
            end;
          end;
        end;

{$ENDIF}
      BT_S, BT_CR, BT_LF:
        begin
          Inc(PtrComp(Ptr), MinBPC(Enc));

          while Ptr <> Stop do
          begin
            case ByteType(Enc, Ptr) of
              { #define CHECK_NMSTRT_CASES }
              BT_NONASCII:
                if IsNMSTRT_CharMinBPC(Enc, Ptr) = 0 then
                begin
                  NextTokPtr^ := Ptr;

                  Result := xtInvalid;

                  Exit;
                end
                else
                  goto _bt3;

              BT_NMSTRT, BT_HEX:
              _bt3:
                Inc(PtrComp(Ptr), MinBPC(Enc));

              BT_LEAD2:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 2 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 2) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                  end;

                  Inc(PtrComp(Ptr), 2);
                end;

              BT_LEAD3:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 3 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 3) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;
                  end;

                  Inc(PtrComp(Ptr), 3);
                end;

              BT_LEAD4:
                begin
                  if PtrComp(Stop) - PtrComp(Ptr) < 4 then
                  begin
                    Result := xtPartialChar;

                    Exit;
                  end;

                  if not IS_NMSTRT_CHAR(Enc, Ptr, 4) = 0 then
                  begin
                    NextTokPtr^ := Ptr;

                    Result := xtInvalid;

                  end;

                  Inc(PtrComp(Ptr), 4);

                end;

              { CHECK_NMSTRT_CASES #define }

              BT_GT:
                goto Gt;

              BT_SOL:
                goto Sol;

              BT_S, BT_CR, BT_LF:
                begin
                  Inc(PtrComp(Ptr), MinBPC(Enc));

                  Continue;
                end;
            else
              begin
                NextTokPtr^ := Ptr;

                Result := xtInvalid;

                Exit;
              end;
            end;

            Result := NormalScanAtts(Enc, Ptr, Stop, NextTokPtr);

            Exit;
          end;

          Result := xtPartial;

          Exit;
        end;

      BT_GT:
      Gt:
        begin
          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

          Result := xtStartTagNoAtts;
          Exit;
        end;

      BT_SOL:
      Sol:
        begin
          Inc(PtrComp(Ptr), MinBPC(Enc));

          if Ptr <> Stop then
          begin
            Result := xtPartial;

            Exit;
          end;

          if CharMatches(Enc, Ptr, Integer(ASCII_GT)) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

          Result := xtEmptyElementNoAtts;

          Exit;
        end;
    else
      begin
        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end;
    end;

  Result := xtPartial;
end;

{ ptr points to character following "<!" }
function NormalScanDecl(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
label
  _fall0;

begin
  if Ptr = Stop then
  begin
    Result := xtPartial;

    Exit;
  end;

  case ByteType(Enc, Ptr) of
    BT_MINUS:
      begin
        Result := NormalScanComment(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
          Stop, NextTokPtr);

        Exit;
      end;

    BT_LSQB:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

        Result := xtCondSectOpen;

        Exit;
      end;

    BT_NMSTRT, BT_HEX:
      Inc(PtrComp(Ptr), MinBPC(Enc));

  else
    begin
      NextTokPtr^ := Ptr;

      Result := xtInvalid;

      Exit;
    end;
  end;

  while Ptr <> Stop do
    case ByteType(Enc, Ptr) of
      BT_PERCNT:
        begin
          if PtrComp(Ptr) + MinBPC(Enc) = PtrComp(Stop) then
          begin
            Result := xtPartial;

            Exit;
          end;

          { don't alLow <!ENTITY% foo "whatever"> }
          case ByteType(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc))) of
            BT_S, BT_CR, BT_LF, BT_PERCNT:
              begin
                NextTokPtr^ := Ptr;

                Result := xtInvalid;

                Exit;
              end;
          end;

          { fall through }
          goto _fall0;

        end;

      BT_S, BT_CR, BT_LF:
      _fall0:
        begin
          NextTokPtr^ := Ptr;

          Result := xtDeclOpen;

          Exit;
        end;

      BT_NMSTRT, BT_HEX:
        Inc(PtrComp(Ptr), MinBPC(Enc));

    else
      begin
        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end;
    end;

  Result := xtPartial;
end;

{ ptr points to character following "%" }
function ScanPercent(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
end;

function ScanPoundName(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
end;

function NormalScanLit(Open: Integer; Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
var
  T: Integer;

label
  _break;

begin
  while Ptr <> Stop do
  begin
    T := ByteType(Enc, Ptr);

    case T of
      { #define INVALID_CASES }
      BT_LEAD2:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 2 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsInvalidChar(Enc, Ptr, 2) <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 2);
        end;

      BT_LEAD3:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 3 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsInvalidChar(Enc, Ptr, 3) <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 3);
        end;

      BT_LEAD4:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 4 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsInvalidChar(Enc, Ptr, 4) <> 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 4);
        end;

      BT_NONXML, BT_MALFORM, BT_TRAIL:
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end;

      { INVALID_CASES #define }

      BT_QUOT, BT_APOS:
        begin
          Inc(PtrComp(Ptr), MinBPC(Enc));

          if T <> Open then
            goto _break;

          if Ptr = Stop then
          begin
            Result := TXmlTok(-Integer(xtLiteral));

            Exit;
          end;

          NextTokPtr^ := Ptr;

          case ByteType(Enc, Ptr) of
            BT_S, BT_CR, BT_LF, BT_GT, BT_PERCNT, BT_LSQB:
              begin
                Result := xtLiteral;

                Exit;
              end;
          else
            begin
              Result := xtInvalid;

              Exit;
            end;
          end;
        end;
    else
      Inc(PtrComp(Ptr), MinBPC(Enc));
    end;

  _break:
  end;

  Result := xtPartial;
end;

function NormalPrologTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
var
  Tok: TXmlTok;

  N: TSize;
label
  _bt_s, _else, _else2, _bt0, _bt1;

begin
  if Ptr = Stop then
  begin
    Result := xtNone;
    Exit;
  end;

  if MinBPC(Enc) > 1 then
  begin
    N := PtrComp(Stop) - PtrComp(Ptr);

    if N and (MinBPC(Enc) - 1) <> 0 then
    begin
      N := N and not(MinBPC(Enc) - 1);

      if N = 0 then
      begin
        Result := xtPartial;

        Exit;
      end;

      Stop := PAnsiChar(PtrComp(Ptr) + N);
    end;
  end;

  case ByteType(Enc, Ptr) of
    BT_QUOT:
      begin
        Result := NormalScanLit(BT_QUOT, Enc,
          PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)), Stop, NextTokPtr);
        Exit;
      end;

    BT_APOS:
      begin
        Result := NormalScanLit(BT_APOS, Enc,
          PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)), Stop, NextTokPtr);
        Exit;
      end;

    BT_LT:
      begin
        Inc(PtrComp(Ptr), MinBPC(Enc));

        if Ptr = Stop then
        begin
          Result := xtPartial;
          Exit;
        end;

        case ByteType(Enc, Ptr) of
          BT_EXCL:
            begin
              Result := NormalScanDecl(Enc,
                PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)), Stop, NextTokPtr);
              Exit;
            end;

          BT_QUEST:
            begin
              Result := NormalScanProcessingInstruction(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)
                ), Stop, NextTokPtr);
              Exit;
            end;

          BT_NMSTRT, BT_HEX, BT_NONASCII, BT_LEAD2, BT_LEAD3, BT_LEAD4:
            begin
              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) - MinBPC(Enc));

              Result := xtInstanceStart;
              Exit;
            end;

        end;

        NextTokPtr^ := Ptr;

        Result := xtInvalid;
        Exit;
      end;

    BT_CR:
      if PtrComp(Ptr) + MinBPC(Enc) = PtrComp(Stop) then
      begin
        NextTokPtr^ := Stop;

        { indicate that this might be part of a CR/LF pair }
        Result := TXmlTok(-Integer(xtProlog_S));
        Exit;
      end
      else
        { fall through }
        goto _bt_s;

    BT_S, BT_LF:
    _bt_s:
      begin
        repeat
          Inc(PtrComp(Ptr), MinBPC(Enc));

          if Ptr = Stop then
            Break;

          case ByteType(Enc, Ptr) of
            BT_CR:
              { don't split CR/LF pair }
              if PtrComp(Ptr) + MinBPC(Enc) <> PtrComp(Stop) then
              else
                { fall through }
                goto _else;

            BT_S, BT_LF:
            else
            begin
            _else:
              NextTokPtr^ := Ptr;

              Result := xtProlog_S;
              Exit;
            end;
          end;
        until False;

        NextTokPtr^ := Ptr;

        Result := xtProlog_S;
        Exit;
      end;

    BT_PERCNT:
      begin
        Result := ScanPercent(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)), Stop,
          NextTokPtr);
        Exit;
      end;

    BT_COMMA:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

        Result := xtComma;
        Exit;
      end;

    BT_LSQB:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

        Result := xtOpenBracket;
        Exit;
      end;

    BT_RSQB:
      begin
        Inc(PtrComp(Ptr), MinBPC(Enc));

        if Ptr = Stop then
        begin
          Result := TXmlTok(-Integer(xtCloseBracket));
          Exit;
        end;

        if CharMatches(Enc, Ptr, Integer(ASCII_RSQB)) <> 0 then
        begin
          if PtrComp(Ptr) + MinBPC(Enc) = PtrComp(Stop) then
          begin
            Result := xtPartial;
            Exit;
          end;

          if CharMatches(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
            Integer(ASCII_GT)) <> 0 then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 2 * MinBPC(Enc));

            Result := xtCondSectClose;
            Exit;
          end;
        end;

        NextTokPtr^ := Ptr;

        Result := xtCloseBracket;
        Exit;
      end;

    BT_LPAR:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

        Result := xtOpenParen;
        Exit;
      end;

    BT_RPAR:
      begin
        Inc(PtrComp(Ptr), MinBPC(Enc));

        if Ptr = Stop then
        begin
          Result := TXmlTok(-Integer(xtCloseParen));
          Exit;
        end;

        case ByteType(Enc, Ptr) of
          BT_AST:
            begin
              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

              Result := xtCloseParenAsterisk;
              Exit;
            end;

          BT_QUEST:
            begin
              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

              Result := xtCloseParenQuestion;
              Exit;
            end;

          BT_PLUS:
            begin
              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

              Result := xtCloseParenPlus;
              Exit;
            end;

          BT_CR, BT_LF, BT_S, BT_GT, BT_COMMA, BT_VERBAR, BT_RPAR:
            begin
              NextTokPtr^ := Ptr;

              Result := xtCloseParen;
              Exit;
            end;
        end;

        NextTokPtr^ := Ptr;

        Result := xtInvalid;
        Exit;
      end;

    BT_VERBAR:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

        Result := xtOr;

        Exit;
      end;

    BT_GT:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

        Result := xtDeclClose;

        Exit;
      end;

    BT_NUM:
      begin
        Result := ScanPoundName(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
          Stop, NextTokPtr);

        Exit;
      end;

    BT_LEAD2:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 2 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if IS_NMSTRT_CHAR(Enc, Ptr, 2) <> 0 then
        begin
          Inc(PtrComp(Ptr), 2);

          Tok := xtName;
        end
        else if IsNameChar(Enc, Ptr, 2) <> 0 then
        begin
          Inc(PtrComp(Ptr), 2);

          Tok := xt_NMTOKEN;
        end
        else
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end;
      end;

    BT_LEAD3:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 3 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if IS_NMSTRT_CHAR(Enc, Ptr, 3) <> 0 then
        begin
          Inc(PtrComp(Ptr), 3);

          Tok := xtName;
        end
        else if IsNameChar(Enc, Ptr, 3) <> 0 then
        begin
          Inc(PtrComp(Ptr), 3);

          Tok := xt_NMTOKEN;
        end
        else
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end;
      end;

    BT_LEAD4:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 4 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if IS_NMSTRT_CHAR(Enc, Ptr, 4) <> 0 then
        begin
          Inc(PtrComp(Ptr), 4);

          Tok := xtName;
        end
        else if IsNameChar(Enc, Ptr, 4) <> 0 then
        begin
          Inc(PtrComp(Ptr), 4);

          Tok := xt_NMTOKEN;
        end
        else
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end;
      end;

    BT_NMSTRT, BT_HEX:
      begin
        Tok := xtName;

        Inc(PtrComp(Ptr), MinBPC(Enc));
      end;

    BT_DIGIT, BT_NAME, BT_MINUS {$IFDEF XML_NS} , BT_COLON: {$ELSE }: {$ENDIF}
      begin
        Tok := xt_NMTOKEN;

        Inc(PtrComp(Ptr), MinBPC(Enc));
      end;

    BT_NONASCII:
      if IsNMSTRT_CharMinBPC(Enc, Ptr) <> 0 then
      begin
        Inc(PtrComp(Ptr), MinBPC(Enc));

        Tok := xtName;
      end
      else if IsNameCharMinBPC(Enc, Ptr) <> 0 then
      begin
        Inc(PtrComp(Ptr), MinBPC(Enc));

        Tok := xt_NMTOKEN;
      end
      else
        { fall through }
        goto _else2;

  else
    begin
    _else2:
      NextTokPtr^ := Ptr;

      Result := xtInvalid;

      Exit;
    end;
  end;

  while Ptr <> Stop do
    case ByteType(Enc, Ptr) of
      { #define CHECK_NAME_CASES }
      BT_NONASCII:
        if IsNameCharMinBPC(Enc, Ptr) = 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end
        else
          goto _bt0;

      BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
      _bt0:
        Inc(PtrComp(Ptr), MinBPC(Enc));

      BT_LEAD2:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 2 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 2) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 2);
        end;

      BT_LEAD3:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 3 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 3) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 3);
        end;

      BT_LEAD4:
        begin
          if PtrComp(Stop) - PtrComp(Ptr) < 4 then
          begin
            Result := xtPartialChar;

            Exit;
          end;

          if IsNameChar(Enc, Ptr, 4) = 0 then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          Inc(PtrComp(Ptr), 4);
        end;

      { CHECK_NAME_CASES #define }

      BT_GT, BT_RPAR, BT_COMMA, BT_VERBAR, BT_LSQB, BT_PERCNT, BT_S,
        BT_CR, BT_LF:
        begin
          NextTokPtr^ := Ptr;

          Result := Tok;

          Exit;
        end;

{$IFDEF XML_NS}
      BT_COLON:
        begin
          Inc(PtrComp(Ptr), MinBPC(Enc));

          case Tok of
            xtName:
              begin
                if Ptr = Stop then
                begin
                  Result := xtPartial;

                  Exit;
                end;

                Tok := xtPrefixedName;

                case ByteType(Enc, Ptr) of
                  { #define CHECK_NAME_CASES }
                  BT_NONASCII:
                    if IsNameCharMinBPC(Enc, Ptr) = 0 then
                    begin
                      NextTokPtr^ := Ptr;

                      Result := xtInvalid;

                      Exit;
                    end
                    else
                      goto _bt1;

                  BT_NMSTRT, BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
                  _bt1:
                    Inc(PtrComp(Ptr), MinBPC(Enc));

                  BT_LEAD2:
                    begin
                      if PtrComp(Stop) - PtrComp(Ptr) < 2 then
                      begin
                        Result := xtPartialChar;

                        Exit;
                      end;

                      if IsNameChar(Enc, Ptr, 2) = 0 then
                      begin
                        NextTokPtr^ := Ptr;

                        Result := xtInvalid;

                        Exit;
                      end;

                      Inc(PtrComp(Ptr), 2);
                    end;

                  BT_LEAD3:
                    begin
                      if PtrComp(Stop) - PtrComp(Ptr) < 3 then
                      begin
                        Result := xtPartialChar;

                        Exit;
                      end;

                      if IsNameChar(Enc, Ptr, 3) = 0 then
                      begin
                        NextTokPtr^ := Ptr;

                        Result := xtInvalid;

                        Exit;
                      end;

                      Inc(PtrComp(Ptr), 3);
                    end;

                  BT_LEAD4:
                    begin
                      if PtrComp(Stop) - PtrComp(Ptr) < 4 then
                      begin
                        Result := xtPartialChar;

                        Exit;
                      end;

                      if IsNameChar(Enc, Ptr, 4) = 0 then
                      begin
                        NextTokPtr^ := Ptr;

                        Result := xtInvalid;

                        Exit;
                      end;

                      Inc(PtrComp(Ptr), 4);
                    end;

                  { CHECK_NAME_CASES #define }
                else
                  Tok := xt_NMTOKEN;
                end;
              end;

            xtPrefixedName:
              Tok := xt_NMTOKEN;
          end;
        end;
{$ENDIF}

      BT_PLUS:
        begin
          if Tok = xt_NMTOKEN then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

          Result := xtNamePlus;

          Exit;
        end;

      BT_AST:
        begin
          if Tok = xt_NMTOKEN then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

          Result := xtNameAsterisk;

          Exit;
        end;

      BT_QUEST:
        begin
          if Tok = xt_NMTOKEN then
          begin
            NextTokPtr^ := Ptr;

            Result := xtInvalid;

            Exit;
          end;

          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

          Result := xtNameQuestion;

          Exit;
        end;
    else
      begin
        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end;
    end;

  Result := TXmlTok(-Integer(Tok));
end;

function NormalContentTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
var
  N: TSize;

label
  _break, _go0, _break2;

begin
  if Ptr = Stop then
  begin
    Result := xtNone;

    Exit;
  end;

  if MinBPC(Enc) > 1 then
  begin
    N := PtrComp(Stop) - PtrComp(Ptr);

    if N and (MinBPC(Enc) - 1) <> 0 then
    begin
      N := N and not(MinBPC(Enc) - 1);

      if N = 0 then
      begin
        Result := xtPartial;

        Exit;
      end;

      Stop := PAnsiChar(PtrComp(Ptr) + N);
    end;
  end;

  case ByteType(Enc, Ptr) of
    BT_LT:
      begin
        Result := NormalScanLt(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
          Stop, NextTokPtr);

        Exit;
      end;

    BT_AMP:
      begin
        Result := NormalScanRef(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
          Stop, NextTokPtr);

        Exit;
      end;

    BT_CR:
      begin
        Inc(PtrComp(Ptr), MinBPC(Enc));

        if Ptr = Stop then
        begin
          Result := xtTrailingCR;

          Exit;
        end;

        if ByteType(Enc, Ptr) = BT_LF then
          Inc(PtrComp(Ptr), MinBPC(Enc));

        NextTokPtr^ := Ptr;

        Result := xtDataNewLine;

        Exit;
      end;

    BT_LF:
      begin
        NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

        Result := xtDataNewLine;

        Exit;
      end;

    BT_RSQB:
      begin
        Inc(PtrComp(Ptr), MinBPC(Enc));

        if Ptr = Stop then
        begin
          Result := xtTrailingRSQB;

          Exit;
        end;

        if CharMatches(Enc, Ptr, Integer(ASCII_RSQB)) = 0 then
          goto _break;

        Inc(PtrComp(Ptr), MinBPC(Enc));

        if Ptr = Stop then
        begin
          Result := xtTrailingRSQB;

          Exit;
        end;

        if CharMatches(Enc, Ptr, Integer(ASCII_GT)) = 0 then
        begin
          Dec(PtrComp(Ptr), MinBPC(Enc));

          goto _break;
        end;

        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end;

    { #define INVALID_CASES }
    BT_LEAD2:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 2 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if IsInvalidChar(Enc, Ptr, 2) <> 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end;
        Inc(PtrComp(Ptr), 2);
      end;

    BT_LEAD3:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 3 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if IsInvalidChar(Enc, Ptr, 3) <> 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end;
        Inc(PtrComp(Ptr), 3);
      end;

    BT_LEAD4:
      begin
        if PtrComp(Stop) - PtrComp(Ptr) < 4 then
        begin
          Result := xtPartialChar;

          Exit;
        end;

        if IsInvalidChar(Enc, Ptr, 4) <> 0 then
        begin
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end;

        Inc(PtrComp(Ptr), 4);
      end;

    BT_NONXML, BT_MALFORM, BT_TRAIL:
      begin
        NextTokPtr^ := Ptr;

        Result := xtInvalid;

        Exit;
      end;

    { INVALID_CASES #define }

  else
    Inc(PtrComp(Ptr), MinBPC(Enc));
  end;

_break:
  while Ptr <> Stop do
    case ByteType(Enc, Ptr) of
      BT_LEAD2:
        begin
          if (PtrComp(Stop) - PtrComp(Ptr) < 2) or
            (IsInvalidChar(Enc, Ptr, 2) <> 0) then
          begin
            NextTokPtr^ := Ptr;

            Result := xtDataChars;

            Exit;
          end;

          Inc(PtrComp(Ptr), 2);
        end;

      BT_LEAD3:
        begin
          if (PtrComp(Stop) - PtrComp(Ptr) < 3) or
            (IsInvalidChar(Enc, Ptr, 3) <> 0) then
          begin
            NextTokPtr^ := Ptr;

            Result := xtDataChars;

            Exit;
          end;

          Inc(PtrComp(Ptr), 3);
        end;

      BT_LEAD4:
        begin
          if (PtrComp(Stop) - PtrComp(Ptr) < 4) or
            (IsInvalidChar(Enc, Ptr, 4) <> 0) then
          begin
            NextTokPtr^ := Ptr;

            Result := xtDataChars;

            Exit;
          end;

          Inc(PtrComp(Ptr), 4);
        end;

      BT_RSQB:
        begin
          if PtrComp(Ptr) + MinBPC(Enc) <> PtrComp(Stop) then
          begin
            if CharMatches(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
              Integer(ASCII_RSQB)) = 0 then
            begin
              Inc(PtrComp(Ptr), MinBPC(Enc));

              goto _break2;
            end;

            if PtrComp(Ptr) + 2 * MinBPC(Enc) <> PtrComp(Stop) then
            begin
              if CharMatches(Enc, PAnsiChar(PtrComp(Ptr) + 2 * MinBPC(Enc)),
                Integer(ASCII_GT)) = 0 then
              begin
                Inc(PtrComp(Ptr), MinBPC(Enc));

                goto _break2;
              end;

              NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 2 * MinBPC(Enc));

              Result := xtInvalid;

              Exit;
            end;
          end;

          { fall through }
          goto _go0;
        end;

      BT_AMP, BT_LT, BT_NONXML, BT_MALFORM, BT_TRAIL, BT_CR, BT_LF:
      _go0:
        begin
          NextTokPtr^ := Ptr;

          Result := xtDataChars;

          Exit;
        end;

    else
      Inc(PtrComp(Ptr), MinBPC(Enc));
    end;

_break2:
  NextTokPtr^ := Ptr;

  Result := xtDataChars;
end;

function NormalCDataSectionTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
end;

function NormalIgnoreSectionTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
end;

function NormalAttributeValueTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
var
  Start: PAnsiChar;

begin
  if Ptr = Stop then
  begin
    Result := xtNone;

    Exit;
  end;

  Start := Ptr;

  while Ptr <> Stop do
    case ByteType(Enc, Ptr) of
      BT_LEAD2:
        Inc(PtrComp(Ptr), 2);

      BT_LEAD3:
        Inc(PtrComp(Ptr), 3);

      BT_LEAD4:
        Inc(PtrComp(Ptr), 4);

      BT_AMP:
        begin
          if Ptr = Start then
          begin
            Result := NormalScanRef(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)),
              Stop, NextTokPtr);

            Exit;
          end;

          NextTokPtr^ := Ptr;

          Result := xtDataChars;

          Exit;
        end;

      BT_LT:
        begin
          { this is for inside entity references }
          NextTokPtr^ := Ptr;

          Result := xtInvalid;

          Exit;
        end;

      BT_LF:
        begin
          if Ptr = Start then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

            Result := xtDataNewLine;

            Exit;
          end;

          NextTokPtr^ := Ptr;

          Result := xtDataChars;

          Exit;
        end;

      BT_CR:
        begin
          if Ptr = Start then
          begin
            Inc(PtrComp(Ptr), MinBPC(Enc));

            if Ptr = Stop then
            begin
              Result := xtTrailingCR;

              Exit;
            end;

            if ByteType(Enc, Ptr) = BT_LF then
              Inc(PtrComp(Ptr), MinBPC(Enc));

            NextTokPtr^ := Ptr;

            Result := xtDataNewLine;

            Exit;
          end;

          NextTokPtr^ := Ptr;

          Result := xtDataChars;

          Exit;
        end;

      BT_S:
        begin
          if Ptr = Start then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

            Result := xtAttributeValue_S;

            Exit;
          end;

          NextTokPtr^ := Ptr;

          Result := xtDataChars;

          Exit;
        end;
    else
      Inc(PtrComp(Ptr), MinBPC(Enc));
    end;

  NextTokPtr^ := Ptr;

  Result := xtDataChars;
end;

function NormalEntityValueTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
end;

function NormalSameName(Enc: PEncoding; Ptr1, Ptr2: PAnsiChar): Integer;
begin
end;

function NormalNameMatchesAscii(Enc: PEncoding;
  Ptr1, End1, Ptr2: PAnsiChar): Integer;
begin
  while Ptr2^ <> #0 do
  begin
    if Ptr1 = End1 then
    begin
      Result := 0;

      Exit;
    end;

    if CharMatches(Enc, Ptr1, Integer(Ptr2^)) = 0 then
    begin
      Result := 0;

      Exit;
    end;

    Inc(PtrComp(Ptr1), MinBPC(Enc));
    Inc(PtrComp(Ptr2));
  end;

  Result := Integer(Ptr1 = End1);
end;

function NormalNameLength(Enc: PEncoding; Ptr: PAnsiChar): Integer;
var
  Start: PAnsiChar;

begin
  Start := Ptr;

  repeat
    case ByteType(Enc, Ptr) of
      BT_LEAD2:
        Inc(PtrComp(Ptr), 2);

      BT_LEAD3:
        Inc(PtrComp(Ptr), 3);

      BT_LEAD4:
        Inc(PtrComp(Ptr), 4);

      BT_NONASCII, BT_NMSTRT, {$IFDEF XML_NS}BT_COLON, {$ENDIF}
      BT_HEX, BT_DIGIT, BT_NAME, BT_MINUS:
        Inc(PtrComp(Ptr), MinBPC(Enc));

    else
      begin
        Result := PtrComp(Ptr) - PtrComp(Start);

        Exit;

      end;

    end;

  until False;

end;

function NormalSkipS(Enc: PEncoding; Ptr: PAnsiChar): PAnsiChar;
begin
end;

{ This must only be called for a well-formed start-tag or empty
  element tag.  Returns the number of attributes.  Pointers to the
  first attsMax attributes are stored in atts. }
function NormalGetAtts(Enc: PEncoding; Ptr: PAnsiChar; AttsMax: Integer;
  Atts: PAttribute): Integer;
type
  TStateEnum = (Other, InName, InValue);

var
  State: TStateEnum;
  NAtts, Open: Integer;
begin
  State := InName;
  NAtts := 0;
  Open := 0; { defined when state = inValue;
    initialization just to shut up compilers }

  Inc(PtrComp(Ptr), MinBPC(Enc));

  repeat
    case ByteType(Enc, Ptr) of
      BT_LEAD2:
        begin
          if State = Other then
          begin
            if NAtts < AttsMax then
            begin
              PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
                ^.Name := Ptr;
              PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
                ^.Normalized := #1;
            end;

            State := InName;
          end;

          Inc(PtrComp(Ptr), 2 - MinBPC(Enc));
        end;

      BT_LEAD3:
        begin
          if State = Other then
          begin
            if NAtts < AttsMax then
            begin
              PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
                ^.Name := Ptr;
              PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
                ^.Normalized := #1;
            end;

            State := InName;
          end;

          Inc(PtrComp(Ptr), 3 - MinBPC(Enc));
        end;

      BT_LEAD4:
        begin
          if State = Other then
          begin
            if NAtts < AttsMax then
            begin
              PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
                ^.Name := Ptr;
              PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
                ^.Normalized := #1;
            end;

            State := InName;
          end;

          Inc(PtrComp(Ptr), 4 - MinBPC(Enc));
        end;

      BT_NONASCII, BT_NMSTRT, BT_HEX:
        if State = Other then
        begin
          if NAtts < AttsMax then
          begin
            PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
              ^.Name := Ptr;
            PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
              ^.Normalized := #1;
          end;

          State := InName;
        end;

      BT_QUOT:
        if State <> InValue then
        begin
          if NAtts < AttsMax then
            PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))^.ValuePtr
              := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

          State := InValue;
          Open := BT_QUOT;
        end
        else if Open = BT_QUOT then
        begin
          State := Other;

          if NAtts < AttsMax then
            PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
              ^.ValueEnd := Ptr;

          Inc(NAtts);
        end;

      BT_APOS:
        if State <> InValue then
        begin
          if NAtts < AttsMax then
            PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))^.ValuePtr
              := PAnsiChar(PtrComp(Ptr) + MinBPC(Enc));

          State := InValue;
          Open := BT_APOS;
        end
        else if Open = BT_APOS then
        begin
          State := Other;

          if NAtts < AttsMax then
            PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
              ^.ValueEnd := Ptr;

          Inc(NAtts);
        end;

      BT_AMP:
        if NAtts < AttsMax then
          PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
            ^.Normalized := #0;

      BT_S:
        if State = InName then
          State := Other
        else if (State = InValue) and (NAtts < AttsMax) and
          (PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))^.Normalized
          <> #0) and
          ((Ptr = PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
          ^.ValuePtr) or (ByteToASCII(Enc, Ptr) <> Integer(ASCII_SPACE)) or
          (ByteToASCII(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc)))
          = Integer(ASCII_SPACE)) or
          (ByteType(Enc, PAnsiChar(PtrComp(Ptr) + MinBPC(Enc))) = Open)) then
          PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
            ^.Normalized := #0;

      BT_CR, BT_LF:
        { This case ensures that the first attribute name is counted
          Apart from that we could just change state on the quote. }
        if State = InName then
          State := Other
        else if (State = InValue) and (NAtts < AttsMax) then
          PAttribute(PtrComp(Atts) + NAtts * SizeOf(TAttribute))
            ^.Normalized := #0;

      BT_GT, BT_SOL:
        if State <> InValue then
        begin
          Result := NAtts;

          Exit;
        end;
    end;

    Inc(PtrComp(Ptr), MinBPC(Enc));
  until False;

  { not reached }
end;

function NormalCharRefNumber(Enc: PEncoding; Ptr: PAnsiChar): Integer;
begin
end;

function NormalPredefinedEntityName(Enc: PEncoding;
  Ptr, Stop: PAnsiChar): Integer;
begin
end;

procedure NormalUpdatePosition(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  Pos: PPosition);
begin
  while Ptr <> Stop do
  begin
    case ByteType(Enc, Ptr) of
      BT_LEAD2:
        Inc(PtrComp(Ptr), 2);

      BT_LEAD3:
        Inc(PtrComp(Ptr), 3);

      BT_LEAD4:
        Inc(PtrComp(Ptr), 4);

      BT_LF:
        begin
          Inc(Pos.LineNumber);
          Inc(PtrComp(Ptr), MinBPC(Enc));

          Pos.ColumnNumber := 0;
          Continue;
        end;

      BT_CR:
        begin
          Inc(Pos.LineNumber);
          Inc(PtrComp(Ptr), MinBPC(Enc));

          if (Ptr <> Stop) and (ByteType(Enc, Ptr) = BT_LF) then
            Inc(PtrComp(Ptr), MinBPC(Enc));

          Pos.ColumnNumber := 0;
          Continue;
        end;
    else
      Inc(PtrComp(Ptr), MinBPC(Enc));
    end;

    Inc(Pos.ColumnNumber);
  end;
end;

function NormalIsPublicId(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  BadPtr: PPAnsiChar): Integer;
label
  _else;

begin
  Inc(PtrComp(Ptr), MinBPC(Enc));
  Dec(PtrComp(Stop), MinBPC(Enc));

  while Ptr <> Stop do
  begin
    case ByteType(Enc, Ptr) of
      BT_S:
        if CharMatches(Enc, Ptr, Integer(ASCII_TAB)) <> 0 then
        begin
          BadPtr^ := Ptr;
          Result := 0;

          Exit;
        end;

      BT_NAME, BT_NMSTRT:
        if ByteToASCII(Enc, Ptr) and not $7F = 0 then
        else
          goto _else;

      BT_DIGIT, BT_HEX, BT_MINUS, BT_APOS, BT_LPAR, BT_RPAR, BT_PLUS, BT_COMMA,
        BT_SOL, BT_EQUALS, BT_QUEST, BT_CR, BT_LF, BT_SEMI, BT_EXCL, BT_AST,
        BT_PERCNT, BT_NUM {$IFDEF XML_NS} , BT_COLON: {$ELSE} : {$ENDIF}
      else
      _else:
        case ByteToASCII(Enc, Ptr) of
          $24, { $ }
          $40: { @ }
          else
          begin
            BadPtr^ := Ptr;
            Result := 0;

            Exit;
          end;
        end;
    end;

    Inc(PtrComp(Ptr), MinBPC(Enc));
  end;

  Result := 1;
end;

const
{$IFDEF XML_NS}
  Utf8_encoding_ns: TNormalEncoding =
    (Enc: (Scanners: (NormalPrologTok, NormalContentTok,
    NormalCDataSectionTok {$IFDEF XML_DTD}, NormalIgnoreSectionTok
    {$ENDIF} ); LiteralScanners: (NormalAttributeValueTok,
    NormalEntityValueTok);

    SameName: NormalSameName;
    NameMatchesAscii: NormalNameMatchesAscii;
    NameLength: NormalNameLength;
    SkipS: NormalSkipS;
    GetAtts: NormalGetAtts;
    CharRefNumber: NormalCharRefNumber;
    PredefinedEntityName: NormalPredefinedEntityName;
    UpdatePosition: NormalUpdatePosition;
    IsPublicId: NormalIsPublicId;
    Utf8Convert: Utf8_toUtf8;
    Utf16Convert: Utf8ToUtf16;

    MinBytesPerChar: 1;

    IsUtf8: #1; IsUtf16: #0); Type_: (
      {$I asciitab.inc}
      {$I utf8tab.inc}
    );

{$IFDEF XML_MIN_SIZE}
    ByteType: SbByteType; IsNameMin: IsNever; IsNmstrtMin: IsNever;
    ByteToAscii: SbByteToAscii; CharMatches: SbCharMatches;
{$ENDIF}
    IsName2: Utf8IsName2; IsName3: Utf8IsName3; IsName4: IsNever;
    IsNmstrt2: Utf8IsNmstrt2; IsNmstrt3: Utf8IsNmstrt3; IsNmstrt4: IsNever;
    IsInvalid2: Utf8IsInvalid2; IsInvalid3: Utf8IsInvalid3;
    IsInvalid4: Utf8IsInvalid4);
{$ENDIF}

  Utf8_encoding: TNormalEncoding = (Enc: (Scanners: (NormalPrologTok,
    NormalContentTok, NormalCDataSectionTok
    {$IFDEF XML_DTD}, NormalIgnoreSectionTok
    {$ENDIF} ); LiteralScanners: (NormalAttributeValueTok,
    NormalEntityValueTok);

    SameName: NormalSameName; NameMatchesAscii: NormalNameMatchesAscii;
    NameLength: NormalNameLength; SkipS: NormalSkipS; GetAtts: NormalGetAtts;
    CharRefNumber: NormalCharRefNumber;
    PredefinedEntityName: NormalPredefinedEntityName;
    UpdatePosition: NormalUpdatePosition; IsPublicId: NormalIsPublicId;
    Utf8Convert: Utf8_toUtf8; Utf16Convert: Utf8ToUtf16;

    MinBytesPerChar: 1;

    IsUtf8: #1; IsUtf16: #0); Type_: (
      {$I asciitab_bt_colon_.inc}
      {$I utf8tab.inc}
    );

{$IFDEF XML_MIN_SIZE}
    ByteType: SbByteType; IsNameMin: IsNever; IsNmstrtMin: IsNever;
    ByteToAscii: SbByteToAscii; CharMatches: SbCharMatches;
{$ENDIF}
    IsName2: Utf8IsName2; IsName3: Utf8IsName3; IsName4: IsNever;
    IsNmstrt2: Utf8IsNmstrt2; IsNmstrt3: Utf8IsNmstrt3; IsNmstrt4: IsNever;
    IsInvalid2: Utf8IsInvalid2; IsInvalid3: Utf8IsInvalid3;
    IsInvalid4: Utf8IsInvalid4);

{$IFDEF XML_NS}
  Internal_utf8_encoding_ns: TNormalEncoding =
    (Enc: (Scanners: (NormalPrologTok, NormalContentTok,
    NormalCDataSectionTok {$IFDEF XML_DTD}, NormalIgnoreSectionTok
    {$ENDIF} ); LiteralScanners: (NormalAttributeValueTok,
    NormalEntityValueTok);

    SameName: NormalSameName; NameMatchesAscii: NormalNameMatchesAscii;
    NameLength: NormalNameLength; SkipS: NormalSkipS; GetAtts: NormalGetAtts;
    CharRefNumber: NormalCharRefNumber;
    PredefinedEntityName: NormalPredefinedEntityName;
    UpdatePosition: NormalUpdatePosition; IsPublicId: NormalIsPublicId;
    Utf8Convert: Utf8_toUtf8; Utf16Convert: Utf8ToUtf16;

    MinBytesPerChar: 1;

    IsUtf8: #1; IsUtf16: #0); Type_: (
      {$I iasciitab.inc}
      {$I utf8tab.inc}
    );

{$IFDEF XML_MIN_SIZE}
    ByteType: SbByteType; IsNameMin: IsNever; IsNmstrtMin: IsNever;
    ByteToAscii: SbByteToAscii; CharMatches: SbCharMatches;
{$ENDIF}
    IsName2: Utf8IsName2; IsName3: Utf8IsName3; IsName4: IsNever;
    IsNmstrt2: Utf8IsNmstrt2; IsNmstrt3: Utf8IsNmstrt3; IsNmstrt4: IsNever;
    IsInvalid2: Utf8IsInvalid2; IsInvalid3: Utf8IsInvalid3;
    IsInvalid4: Utf8IsInvalid4);
{$ENDIF}

  Internal_utf8_encoding: TNormalEncoding =
    (Enc: (Scanners: (NormalPrologTok, NormalContentTok,
    NormalCDataSectionTok {$IFDEF XML_DTD}, NormalIgnoreSectionTok
    {$ENDIF} ); LiteralScanners: (NormalAttributeValueTok,
    NormalEntityValueTok);

    SameName: NormalSameName; NameMatchesAscii: NormalNameMatchesAscii;
    NameLength: NormalNameLength; SkipS: NormalSkipS; GetAtts: NormalGetAtts;
    CharRefNumber: NormalCharRefNumber;
    PredefinedEntityName: NormalPredefinedEntityName;
    UpdatePosition: NormalUpdatePosition; IsPublicId: NormalIsPublicId;
    Utf8Convert: Utf8_toUtf8; Utf16Convert: Utf8ToUtf16;

    MinBytesPerChar: 1;

    IsUtf8: #1; IsUtf16: #0); Type_: (
      {$I iasciitab_bt_colon_.inc}
      {$I utf8tab.inc}
    );

{$IFDEF XML_MIN_SIZE}
    ByteType: SbByteType; IsNameMin: IsNever; IsNmstrtMin: IsNever;
    ByteToAscii: SbByteToAscii; CharMatches: SbCharMatches;
{$ENDIF}
    IsName2: Utf8IsName2; IsName3: Utf8IsName3; IsName4: IsNever;
    IsNmstrt2: Utf8IsNmstrt2; IsNmstrt3: Utf8IsNmstrt3; IsNmstrt4: IsNever;
    IsInvalid2: Utf8IsInvalid2; IsInvalid3: Utf8IsInvalid3;
    IsInvalid4: Utf8IsInvalid4);

{$IFDEF XML_NS}
  Latin1_encoding_ns: TNormalEncoding = (); {..}
{$ENDIF}
  Latin1_encoding: TNormalEncoding = (); {..}

{$IFDEF XML_NS}
  Ascii_encoding_ns: TNormalEncoding = (); {..}
{$ENDIF}
  Ascii_encoding: TNormalEncoding = (); {..}

{$IFDEF XML_NS}
  Little2_encoding_ns: TNormalEncoding = (); {..}
{$ENDIF}
  Little2_encoding: TNormalEncoding = (); {..}

{$IFDEF XML_NS}
  Big2_encoding_ns: TNormalEncoding = (); {..}
{$ENDIF}
  Big2_encoding: TNormalEncoding = (); {..}

  CKeywordISO_8859_1: array [0..10] of AnsiChar = (ASCII_I, ASCII_S, ASCII_O,
    ASCII_MINUS, ASCII_8, ASCII_8, ASCII_5, ASCII_9, ASCII_MINUS, ASCII_1, #0);

  CKeywordUS_ASCII: array [0..8] of AnsiChar = (ASCII_U, ASCII_S, ASCII_MINUS, ASCII_A,
    ASCII_S, ASCII_C, ASCII_I, ASCII_I, #0);

  CKeywordUtf8: array [0..5] of AnsiChar = (ASCII_U, ASCII_T, ASCII_F, ASCII_MINUS,
    ASCII_8, #0);

  CKeywordUtf16: array [0..6] of AnsiChar = (ASCII_U, ASCII_T, ASCII_F, ASCII_MINUS,
    ASCII_1, ASCII_6, #0);

  CKeywordUtf16BE: array [0..8] of AnsiChar = (ASCII_U, ASCII_T, ASCII_F, ASCII_MINUS,
    ASCII_1, ASCII_6, ASCII_B, ASCII_E, #0);

  CKeywordUtf16LE: array [0..8] of AnsiChar = (ASCII_U, ASCII_T, ASCII_F, ASCII_MINUS,
    ASCII_1, ASCII_6, ASCII_L, ASCII_E, #0);

function Streqci(S1, S2: PAnsiChar): Integer;
var
  C1, C2: AnsiChar;

begin
  repeat
    C1 := S1^;
    C2 := S2^;

    Inc(PtrComp(S1));
    Inc(PtrComp(S2));

    if (ASCII_al <= C1) and (C1 <= ASCII_zl) then
      Inc(Byte(C1), Byte(ASCII_A) - Byte(ASCII_al));

    if (ASCII_al <= C2) and (C2 <= ASCII_zl) then
      Inc(Byte(C2), Byte(ASCII_A) - Byte(ASCII_al));

    if C1 <> C2 then
    begin
      Result := 0;

      Exit;
    end;

    if (C1 = #0) or (C2 = #0) then
      Break;

  until False;

  Result := 1;
end;

procedure InitUpdatePosition(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  Pos: PPosition);
begin
end;

function GetEncodingIndex(Name: PAnsiChar): TEncodingType;
const
  EncodingNames: array [0..5] of PAnsiChar = (@CKeywordISO_8859_1, @CKeywordUS_ASCII,
    @CKeywordUtf8, @CKeywordUtf16, @CKeywordUtf16BE, @CKeywordUtf16LE);

var
  I: Integer;

begin
  if name = nil then
    Result := etNone
  else
  begin
    I := 0;

    while I < SizeOf(EncodingNames) div SizeOf(EncodingNames[0]) do
    begin
      if Streqci(Name, EncodingNames[I]) <> 0 then
      begin
        Result := TEncodingType(I);

        Exit;
      end;

      Inc(I);
    end;

    Result := etUnknown;
  end;
end;

{ This is what detects the TEncoding.  encodingTable maps from
  TEncoding indices to encodings; int8u(enc.initEnc.isUtf16 ) is the index of
  the external (protocol) specified TEncoding; state is
  CXmlContentState if we're parsing an external text entity, and
  CXmlPrologState otherwise. }
function InitScan(EncodingTable: PPEncoding; Enc: PInitEncoding;
  State: Integer; Ptr, Stop: PAnsiChar; NextTokPtr: PPAnsiChar): TXmlTok;
var
  EncPtr: PPEncoding;

  E: TEncodingType;

label
  _003C, _esc;

begin
  if Ptr = Stop then
  begin
    Result := xtNone;

    Exit;
  end;

  EncPtr := Enc.EncPtr;

  { only a single byte available for auto-detection }
  if PtrComp(Ptr) + 1 = PtrComp(Stop) then
  begin
{$IFNDEF XML_DTD} { FIXME }
    { a well-formed document entity must have more than one byte }
    if State <> CXmlContentState then
    begin
      Result := xtPartial;

      Exit;
    end;
{$ENDIF}

    { so we're parsing an external text entity... }
    { if UTF-16 was externally specified, then we need at least 2 bytes }
    case InitEncIndex(Enc) of
      etUTF_16, etUTF_16LE, etUTF_16BE:
        begin
          Result := xtPartial;

          Exit;
        end;
    end;

    case Int8u(Ptr^) of
      $FE, $FF, $EF: { possibly first byte of UTF-8 BOM }
        if (InitEncIndex(Enc) = etISO_8859_1) and (State = CXmlContentState)
        then
        else
          goto _003C;

      $00, $3C: { fall through }
      _003C:
        begin
          Result := xtPartial;

          Exit;
        end;
    end;

  end
  else
    case (PtrComp(Ptr^) shl 8) or PByte(PtrComp(Ptr) + 1)^ of
      $FEFF:
        if (InitEncIndex(Enc) = etISO_8859_1) and (State = CXmlContentState)
        then
        else
        begin
          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 2);
          EncPtr^ := PPEncoding(PtrComp(EncodingTable) + Integer(etUTF_16BE) *
            SizeOf(PEncoding))^;

          Result := xtBom;

          Exit;
        end;

      { 00 3C is handled in the default case }
      $3C00:
        if ((InitEncIndex(Enc) = etUTF_16BE) or
          (InitEncIndex(Enc) = etUTF_16)) and (State = CXmlContentState)
        then
        else
        begin
          EncPtr^ := PPEncoding(PtrComp(EncodingTable) + Integer(etUTF_16LE) *
            SizeOf(PEncoding))^;
          Result := XmlTok_(EncPtr^, State, Ptr, Stop, NextTokPtr);

          Exit;
        end;

      $FFFE:
        if (InitEncIndex(Enc) = etISO_8859_1) and (State = CXmlContentState)
        then
        else
        begin
          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 2);
          EncPtr^ := PPEncoding(PtrComp(EncodingTable) + Integer(etUTF_16LE) *
            SizeOf(PEncoding))^;

          Result := xtBom;

          Exit;
        end;

      { Maybe a UTF-8 BOM (EF BB BF) }
      { If there's an explicitly specified (external) encoding
        of ISO-8859-1 or some flavour of UTF-16
        and this is an external text entity,
        don't look for the BOM,
        because it might be a legal data. }
      $EFBB:
        begin
          if State = CXmlContentState then
          begin
            E := InitEncIndex(Enc);

            if (E = etISO_8859_1) or (E = etUTF_16BE) or (E = etUTF_16LE)
              or (E = etUTF_16) then
              goto _esc;
          end;

          if PtrComp(Ptr) + 2 = PtrComp(Stop) then
          begin
            Result := xtPartial;

            Exit;
          end;

          if PByte(PtrComp(Ptr) + 2)^ = $BF then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 3);
            EncPtr^ := PPEncoding(PtrComp(EncodingTable) + Integer(etUTF_8) *
              SizeOf(PEncoding))^;

            Result := xtBom;

            Exit;
          end;
        end;

    else
      { 0 isn't a legal data character. Furthermore a document
        entity can only start with ASCII characters.  So the only
        way this can fail to be big-endian UTF-16 if it it's an
        external parsed general entity that's labelled as
        UTF-16LE. }
      if Ptr^ = #0 then
      begin
        if (State = CXmlContentState) and (InitEncIndex(Enc) = etUTF_16LE)
        then
          goto _esc;

        EncPtr^ := PPEncoding(PtrComp(EncodingTable) + Integer(etUTF_16BE) *
          SizeOf(PEncoding))^;
        Result := XmlTok_(EncPtr^, State, Ptr, Stop, NextTokPtr);

        Exit;
      end
      else
        { We could recover here in the case:
          - parsing an external entity
          - second byte is 0
          - no externally specified TEncoding
          - no TEncoding declaration
          ByteType assuming UTF-16LE.  But we don't, because this would mean when
          presented just with a single byte, we couldn't reliably determine
          whether we needed further bytes. }
        if PByte(PtrComp(Ptr) + 1)^ = 0 then
        begin
          if State = CXmlContentState then
            goto _esc;

          EncPtr^ := PPEncoding(PtrComp(EncodingTable) + Integer(etUTF_16LE) *
            SizeOf(PEncoding))^;
          Result := XmlTok_(EncPtr^, State, Ptr, Stop, NextTokPtr);
        end;
    end;

_esc:
  EncPtr^ := PPEncoding(PtrComp(EncodingTable) + IntegeR(InitEncIndex(Enc)) *
    SizeOf(PEncoding))^;
  Result := XmlTok_(EncPtr^, State, Ptr, Stop, NextTokPtr);
end;

function ToAscii(Enc: PEncoding; Ptr, Stop: PAnsiChar): Integer;
var
  Buf: array [0..0] of AnsiChar;
  P: PAnsiChar;
begin
  P := @Buf[0];

  XmlUtf8Convert(Enc, @Ptr, Stop, @P, PAnsiChar(PtrComp(P) + 1));

  if P = @Buf[0] then
    Result := -1
  else
    Result := Integer(Buf[0]);
end;

function IsSpace(C: Integer): Integer;
begin
  case C of
    $20, $D, $A, $9:
      Result := 1;

  else
    Result := 0;
  end;
end;

{ Return 1 if there's just optional white space or there's an S
  folLowed ByteType name=val. }
function ParsePseudoAttribute(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NamePtr, NameEndPtr, ValPtr, NextTokPtr: PPAnsiChar): Integer;
var
  C: Integer;

  Open: AnsiChar;
begin
  if Ptr = Stop then
  begin
    NamePtr^ := nil;
    Result := 1;

    Exit;
  end;

  if IsSpace(ToAscii(Enc, Ptr, Stop)) = 0 then
  begin
    NextTokPtr^ := Ptr;

    Result := 0;

    Exit;
  end;

  repeat
    Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

  until IsSpace(ToAscii(Enc, Ptr, Stop)) = 0;

  if Ptr = Stop then
  begin
    NamePtr^ := nil;
    Result := 1;

    Exit;
  end;

  NamePtr^ := Ptr;

  repeat
    C := ToAscii(Enc, Ptr, Stop);

    if C = -1 then
    begin
      NextTokPtr^ := Ptr;

      Result := 0;

      Exit;
    end;

    if C = Integer(ASCII_EQUALS) then
    begin
      NameEndPtr^ := Ptr;

      Break;
    end;

    if IsSpace(C) <> 0 then
    begin
      NameEndPtr^ := Ptr;

      repeat
        Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

        C := ToAscii(Enc, Ptr, Stop);

      until IsSpace(C) = 0;

      if C <> Integer(ASCII_EQUALS) then
      begin
        NextTokPtr^ := Ptr;

        Result := 0;

        Exit;
      end;

      Break;
    end;

    Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

  until False;

  if Ptr = NamePtr^ then
  begin
    NextTokPtr^ := Ptr;

    Result := 0;

    Exit;
  end;

  Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

  C := ToAscii(Enc, Ptr, Stop);

  while IsSpace(C) <> 0 do
  begin
    Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

    C := ToAscii(Enc, Ptr, Stop);
  end;

  if (C <> Integer(ASCII_QUOT)) and (C <> Integer(ASCII_APOS)) then
  begin
    NextTokPtr^ := Ptr;

    Result := 0;

    Exit;
  end;

  Open := AnsiChar(C);

  Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

  ValPtr^ := Ptr;

  repeat
    C := ToAscii(Enc, Ptr, Stop);

    if C = Integer(Open) then
      Break;

    if not((Int(ASCII_al) <= C) and (C <= Integer(ASCII_zl))) and
      not((Int(ASCII_A) <= C) and (C <= Integer(ASCII_Z))) and
      not((Int(ASCII_0) <= C) and (C <= Integer(ASCII_9))) and
      (C <> Integer(ASCII_PERIOD)) and (C <> Integer(ASCII_MINUS)) and
      (C <> Integer(ASCII_UNDERSCORE)) then
    begin
      NextTokPtr^ := Ptr;

      Result := 0;

      Exit;
    end;

    Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

  until False;

  NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + Enc.MinBytesPerChar);

  Result := 1;
end;

function DoParseXmlDecl(EncodingFinder: TEncodingFinder;
  IsGeneralTextEntity: Integer; Enc: PEncoding; Ptr, Stop: PAnsiChar;
  BadPtr, VersionPtr, VersionEndPtr, EncodingName: PPAnsiChar;
  TEncoding: PPEncoding; Standalone: PInteger): Integer;
var
  Val, Name, NameEnd: PAnsiChar;

  C: Integer;

begin
  Val := nil;
  name := nil;
  NameEnd := nil;

  Inc(PtrComp(Ptr), 5 * Enc.MinBytesPerChar);
  Dec(PtrComp(Stop), 2 * Enc.MinBytesPerChar);

  if (ParsePseudoAttribute(Enc, Ptr, Stop, @name, @NameEnd, @Val, @Ptr) = 0) or
    (name = nil) then
  begin
    BadPtr^ := Ptr;
    Result := 0;

    Exit;
  end;

  if XmlNameMatchesAscii(Enc, name, NameEnd, @CKeyWordVersion[0]) = 0 then
    if IsGeneralTextEntity = 0 then
    begin
      BadPtr^ := name;
      Result := 0;

      Exit;
    end
    else
  else
  begin
    if VersionPtr <> nil then
      VersionPtr^ := Val;

    if VersionEndPtr <> nil then
      VersionEndPtr^ := Ptr;

    if ParsePseudoAttribute(Enc, Ptr, Stop, @name, @NameEnd, @Val, @Ptr) = 0
    then
    begin
      BadPtr^ := Ptr;
      Result := 0;

      Exit;
    end;

    if name = nil then
    begin
      if IsGeneralTextEntity <> 0 then
      begin
        { a TextDecl must have an EncodingDecl }

        BadPtr^ := Ptr;
        Result := 0;

        Exit;
      end;

      Result := 1;

      Exit;
    end;
  end;

  if XmlNameMatchesAscii(Enc, name, NameEnd, @CKeyWordEncoding[0]) <> 0 then
  begin
    C := ToAscii(Enc, Val, Stop);

    if not((Int(ASCII_al) <= C) and (C <= Integer(ASCII_zl))) and
      not((Int(ASCII_A) <= C) and (C <= Integer(ASCII_Z))) then
    begin
      BadPtr^ := Val;
      Result := 0;

      Exit;
    end;

    if EncodingName <> nil then
      EncodingName^ := Val;

    if TEncoding <> nil then
      TEncoding^ := EncodingFinder(Enc, Val,
        PAnsiChar(PtrComp(Ptr) - Enc.MinBytesPerChar));

    if ParsePseudoAttribute(Enc, Ptr, Stop, @name, @NameEnd, @Val, @Ptr) = 0
    then
    begin
      BadPtr^ := Ptr;
      Result := 0;

      Exit;
    end;

    if name <> nil then
    begin
      Result := 1;

      Exit;
    end;
  end;

  if (XmlNameMatchesAscii(Enc, name, NameEnd, @CKeyWordStandalone[0]) = 0) or
    (IsGeneralTextEntity <> 0) then
  begin
    BadPtr^ := name;
    Result := 0;

    Exit;
  end;

  if XmlNameMatchesAscii(Enc, Val, PAnsiChar(PtrComp(Ptr) - Enc.MinBytesPerChar),
    @CKeyWordYes[0]) <> 0 then
    if Standalone <> nil then
      Standalone^ := 1
    else
  else if XmlNameMatchesAscii(Enc, Val,
    PAnsiChar(PtrComp(Ptr) - Enc.MinBytesPerChar), @CKeyWordNo[0]) <> 0 then
    if Standalone <> nil then
      Standalone^ := 0
    else
  else
  begin
    BadPtr^ := Val;
    Result := 0;

    Exit;
  end;

  while IsSpace(ToAscii(Enc, Ptr, Stop)) <> 0 do
    Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

  if Ptr <> Stop then
  begin
    BadPtr^ := Ptr;
    Result := 0;

    Exit;
  end;

  Result := 1;
end;

const
{$IFDEF XML_NS}
  EncodingsNS: array [0 .. 6] of PEncoding = (@Latin1_encoding_ns.Enc,
    @Ascii_encoding_ns.Enc, @Utf8_encoding_ns.Enc, @Big2_encoding_ns.Enc,
    @Big2_encoding_ns.Enc, @Little2_encoding_ns.Enc, @Utf8_encoding_ns.Enc);
  { etNone }
{$ENDIF}
  Encodings: array [0 .. 6] of PEncoding = (@Latin1_encoding.Enc,
    @Ascii_encoding.Enc, @Utf8_encoding.Enc, @Big2_encoding.Enc,
    @Big2_encoding.Enc, @Little2_encoding.Enc, @Utf8_encoding.Enc); { CEncodingNone }

function InitScanProlog(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
  Result := InitScan(@Encodings, PInitEncoding(Enc), CXmlPrologState, Ptr,
    Stop, NextTokPtr);
end;

function InitScanContent(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
begin
end;

function XmlInitEncoding(P: PInitEncoding; EncPtr: PPEncoding;
  Name: PAnsiChar): Integer;
var
  I: TEncodingType;
begin
  I := GetEncodingIndex(Name);

  if I = etUnknown then
  begin
    Result := 0;
    Exit;
  end;

  SetInitEncIndex(P, I);

  P.InitEnc.Scanners[CXmlPrologState] := @InitScanProlog;
  P.InitEnc.Scanners[CXmlContentState] := @InitScanContent;

  P.InitEnc.UpdatePosition := @InitUpdatePosition;

  P.EncPtr := EncPtr;
  EncPtr^ := @P.InitEnc;

  Result := 1;
end;

function XmlInitEncodingNS(P: PInitEncoding; EncPtr: PPEncoding;
  Name: PAnsiChar): Integer;
begin
end;

function XmlGetUtf8InternalEncoding: PEncoding;
begin
  Result := @Internal_utf8_encoding.Enc;
end;

function XmlGetUtf16InternalEncoding: PEncoding;
begin
end;

function XmlGetInternalEncoding: PEncoding;
begin
{$IFDEF XML_UNICODE}
  Result := XmlGetUtf16InternalEncoding;
{$ELSE}
  Result := XmlGetUtf8InternalEncoding;
{$ENDIF}
end;

function XmlGetUtf8InternalEncodingNS: PEncoding;
begin
end;

function XmlGetUtf16InternalEncodingNS: PEncoding;
begin
end;

function XmlGetInternalEncodingNS: PEncoding;
begin
{$IFDEF XML_UNICODE}
  Result := XmlGetUtf16InternalEncodingNS;
{$ELSE}
  Result := XmlGetUtf8InternalEncodingNS;
{$ENDIF}
end;

function FindEncoding(Enc: PEncoding; Ptr, Stop: PAnsiChar): PEncoding;
begin
end;

function XmlParseXmlDecl(IsGeneralTextEntity: Integer; Enc: PEncoding;
  Ptr, Stop: PAnsiChar; BadPtr, VersionPtr, VersionEndPtr,
  EncodingNamePtr: PPAnsiChar; NamedEncodingPtr: PPEncoding;
  StandalonePtr: PInteger): Integer;
begin
  Result := DoParseXmlDecl(@FindEncoding, IsGeneralTextEntity, Enc, Ptr, Stop,
    BadPtr, VersionPtr, VersionEndPtr, EncodingNamePtr, NamedEncodingPtr,
    StandalonePtr);
end;

function XmlTok_(Enc: PEncoding; State: Integer; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
  Result := Enc.Scanners[State](Enc, Ptr, Stop, NextTokPtr);
end;

function XmlPrologTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
  Result := XmlTok_(Enc, CXmlPrologState, Ptr, Stop, NextTokPtr);
end;

function XmlContentTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
  Result := XmlTok_(Enc, CXmlContentState, Ptr, Stop, NextTokPtr);
end;

function XmlIsPublicId(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  BadPtr: PPAnsiChar): Integer;
begin
  Result := Enc.IsPublicId(Enc, Ptr, Stop, BadPtr);
end;

procedure XmlUtf8Convert(Enc: PEncoding; FromP: PPAnsiChar;
  FromLim: PAnsiChar; ToP: PPAnsiChar; ToLim: PAnsiChar);
begin
  Enc.Utf8Convert(Enc, FromP, FromLim, ToP, ToLim);
end;

procedure XmlUtf16Convert(Enc: PEncoding; FromP: PPAnsiChar;
  FromLim: PAnsiChar; ToP: PPWord; ToLim: PWord);
begin
  Enc.Utf16Convert(Enc, FromP, FromLim, ToP, ToLim);
end;

function XmlUtf8Encode(CharNumber: Integer; Buf: PAnsiChar): Integer;
begin
end;

function XmlUtf16Encode(CharNumber: Integer; Buf: PWord): Integer;
begin
end;

function XmlLiteralTok(Enc: PEncoding; LiteralType: Integer; Ptr,
  Stop: PAnsiChar; NextTokPtr: PPAnsiChar): TXmlTok;
begin
  Result := Enc.LiteralScanners[LiteralType](Enc, Ptr, Stop, NextTokPtr);
end;

function XmlAttributeValueTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
  Result := XmlLiteralTok(Enc, CXmlAttributeValueLiteral, Ptr, Stop,
    NextTokPtr);
end;

function XmlEntityValueTok(Enc: PEncoding; Ptr, Stop: PAnsiChar;
  NextTokPtr: PPAnsiChar): TXmlTok;
begin
  Result := XmlLiteralTok(Enc, CXmlEntityValueLiteral, Ptr, Stop, NextTokPtr);
end;

function XmlSameName(Enc: PEncoding; Ptr1, Ptr2: PAnsiChar): Integer;
begin
  Result := Enc.SameName(Enc, Ptr1, Ptr2);
end;

function XmlNameMatchesAscii(Enc: PEncoding;
  Ptr1, End1, Ptr2: PAnsiChar): Integer;
begin
  Result := Enc.NameMatchesAscii(Enc, Ptr1, End1, Ptr2);
end;

function XmlNameLength(Enc: PEncoding; Ptr: PAnsiChar): Integer;
begin
  Result := Enc.NameLength(Enc, Ptr);
end;

function XmlGetAttributes(Enc: PEncoding; Ptr: PAnsiChar; AttsMax: Integer;
  Atts: PAttribute): Integer;
begin
  Result := Enc.GetAtts(Enc, Ptr, AttsMax, Atts);
end;

function XmlCharRefNumber(Enc: PEncoding; Ptr: PAnsiChar): Integer;
begin
  Result := Enc.CharRefNumber(Enc, Ptr);
end;

function XmlPredefinedEntityName(Enc: PEncoding; Ptr, Stop: PAnsiChar): Integer;
begin
  Result := Enc.PredefinedEntityName(Enc, Ptr, Stop);
end;

function XmlParseXmlDeclNS(IsGeneralTextEntity: Integer; Enc: PEncoding;
  Ptr, Stop: PAnsiChar; BadPtr, VersionPtr, VersionEndPtr, EncodingNamePtr
  : PPAnsiChar; NamedEncodingPtr: PPEncoding;
  StandalonePtr: PInteger): Integer;
begin
end;

end.
