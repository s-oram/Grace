unit xmltok;

// ----------------------------------------------------------------------------
// Copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
// and Clark Cooper
// Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port By: Milan Marusinec alias Milano
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
{ xmltok.pas }

interface

uses
  Expat_basics,
  Expat_external;

{$I expat_mode.inc}

type
  PPWord = ^PWord;

{ CONSTANTS DEFINITION }
const
  { The following token may be returned by XmlContentTok }
  XML_TOK_TRAILING_RSQB = -5; { ] or ]] at the end of the scan; might be
    start of illegal ]]> sequence }

  { The following tokens may be returned by both XmlPrologTok and XmlContentTok. }
  XML_TOK_NONE = -4; { The string to be scanned is empty }
  XML_TOK_TRAILING_CR = -3; { A CR at the end of the scan;
    might be part of CRLF sequence }
  XML_TOK_PARTIAL_CHAR = -2; { only part of a multibyte sequence }
  XML_TOK_PARTIAL = -1; { only part of a token }
  XML_TOK_INVALID = 0;

  { The following tokens are returned by XmlContentTok; some are also
    returned by XmlAttributeValueTok, XmlEntityTok, XmlCdataSectionTok. }
  XML_TOK_START_TAG_WITH_ATTS = 1;
  XML_TOK_START_TAG_NO_ATTS = 2;
  XML_TOK_EMPTY_ELEMENT_WITH_ATTS = 3; { empty element tag <e/> }
  XML_TOK_EMPTY_ELEMENT_NO_ATTS = 4;
  XML_TOK_END_TAG = 5;
  XML_TOK_DATA_CHARS = 6;
  XML_TOK_DATA_NEWLINE = 7;
  XML_TOK_CDATA_SECT_OPEN = 8;
  XML_TOK_ENTITY_REF = 9;
  XML_TOK_CHAR_REF = 10; { numeric character reference }

  { The following tokens may be returned by both XmlPrologTok and XmlContentTok. }
  XML_TOK_PI = 11; { processing instruction }
  XML_TOK_XML_DECL = 12; { XML decl or text decl }
  XML_TOK_COMMENT = 13;
  XML_TOK_BOM = 14; { Byte order mark }

  { The following tokens are returned only by XmlPrologTok }
  XML_TOK_PROLOG_S = 15;
  XML_TOK_DECL_OPEN = 16; { <!foo }
  XML_TOK_DECL_CLOSE = 17; { > }
  XML_TOK_NAME = 18;
  XML_TOK_NMTOKEN = 19;
  XML_TOK_POUND_NAME = 20; { #name }
  XML_TOK_OR = 21; { | }
  XML_TOK_PERCENT = 22;
  XML_TOK_OPEN_PAREN = 23;
  XML_TOK_CLOSE_PAREN = 24;
  XML_TOK_OPEN_BRACKET = 25;
  XML_TOK_CLOSE_BRACKET = 26;
  XML_TOK_LITERAL = 27;
  XML_TOK_PARAM_ENTITY_REF = 28;
  XML_TOK_INSTANCE_START = 29;

  { The following occur only in element type declarations }
  XML_TOK_NAME_QUESTION = 30; { name? }
  XML_TOK_NAME_ASTERISK = 31; { name* }
  XML_TOK_NAME_PLUS = 32; { name+ }
  XML_TOK_COND_SECT_OPEN = 33; { <![ }
  XML_TOK_COND_SECT_CLOSE = 34; { ]]> }
  XML_TOK_CLOSE_PAREN_QUESTION = 35; { )? }
  XML_TOK_CLOSE_PAREN_ASTERISK = 36; { )* }
  XML_TOK_CLOSE_PAREN_PLUS = 37; { )+ }
  XML_TOK_COMMA = 38;

  { The following token is returned only by XmlAttributeValueTok }
  XML_TOK_ATTRIBUTE_VALUE_S = 39;

  { The following token is returned only by XmlCdataSectionTok }
  XML_TOK_CDATA_SECT_CLOSE = 40;

  { With namespace processing this is returned by XmlPrologTok for a
    name with a colon. }
  XML_TOK_PREFIXED_NAME = 41;

{$IFDEF XML_DTD }
  XML_TOK_IGNORE_SECT = 42;

{$ENDIF }
{$IFDEF XML_DTD }
  XML_N_STATES = 4;

{$ELSE }
  XML_N_STATES = 3;

{$ENDIF }
  XML_PROLOG_STATE = 0;
  XML_CONTENT_STATE = 1;
  XML_CDATA_SECTION_STATE = 2;

{$IFDEF XML_DTD }
  XML_IGNORE_SECTION_STATE = 3;

{$ENDIF }
  XML_N_LITERAL_TYPES = 2;
  XML_ATTRIBUTE_VALUE_LITERAL = 0;
  XML_ENTITY_VALUE_LITERAL = 1;

  { The size of the buffer passed to XmlUtf8Encode must be at least this. }
  XML_UTF8_ENCODE_MAX = 4;

  { The size of the buffer passed to XmlUtf16Encode must be at least this. }
  XML_UTF16_ENCODE_MAX = 2;

  
type
  POSITION_ptr = ^POSITION;

  POSITION = record
    { first line and first column are 0 not 1 }
    LineNumber, ColumnNumber: XML_Size;

  end;

  ATTRIBUTE_ptr = ^ATTRIBUTE;

  ATTRIBUTE = record
    Name, ValuePtr, ValueEnd: PAnsiChar;
    Normalized: AnsiChar;
  end;

  ENCODING_ptr_ptr = ^ENCODING_ptr;
  ENCODING_ptr = ^ENCODING;

  SCANNER = function(P1: ENCODING_ptr; P2, P3: PAnsiChar; P4: PPAnsiChar): Integer;

  ENCODING = record
    Scanners: array [0..XML_N_STATES - 1] of SCANNER;
    LiteralScanners: array [0..XML_N_LITERAL_TYPES - 1] of SCANNER;

    SameName: function(P1: ENCODING_ptr; P2, P3: PAnsiChar): Integer;
    NameMatchesAscii: function(P1: ENCODING_ptr; P2, P3, P4: PAnsiChar): Integer;
    NameLength: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    SkipS: function(P1: ENCODING_ptr; P2: PAnsiChar): PAnsiChar;
    GetAtts: function(Enc: ENCODING_ptr; Ptr: PAnsiChar; AttsMax: Integer;
      Atts: ATTRIBUTE_ptr): Integer;
    CharRefNumber: function(Enc: ENCODING_ptr; Ptr: PAnsiChar): Integer;
    PredefinedEntityName: function(P1: ENCODING_ptr; P2, P3: PAnsiChar): Integer;
    UpdatePosition: procedure(P1: ENCODING_ptr; Ptr, End_: PAnsiChar;
      P4: POSITION_ptr);
    IsPublicId: function(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
      BadPtr: PPAnsiChar): Integer;
    Utf8Convert: procedure(Enc: ENCODING_ptr; FromP: PPAnsiChar;
      FromLim: PAnsiChar; ToP: PPAnsiChar; ToLim: PAnsiChar);
    Utf16Convert: procedure(Enc: ENCODING_ptr; FromP: PPAnsiChar;
      FromLim: PAnsiChar; ToP: PPWord; ToLim: PWord);

    MinBytesPerChar: Integer;

    IsUtf8, IsUtf16: AnsiChar;
  end;

  INIT_ENCODING_ptr = ^INIT_ENCODING;

  INIT_ENCODING = record
    InitEnc: ENCODING;
    EncPtr: ENCODING_ptr_ptr;
  end;

  { GLOBAL PROCEDURES }
function XmlInitEncoding(P: INIT_ENCODING_ptr; EncPtr: ENCODING_ptr_ptr;
  Name: PAnsiChar): Integer;
function XmlInitEncodingNS(P: INIT_ENCODING_ptr; EncPtr: ENCODING_ptr_ptr;
  Name: PAnsiChar): Integer;

function XmlGetInternalEncoding: ENCODING_ptr;
function XmlGetInternalEncodingNS: ENCODING_ptr;

function XmlTok_(Enc: ENCODING_ptr; State: Integer; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
function XmlPrologTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
function XmlContentTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
function XmlIsPublicId(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  BadPtr: PPAnsiChar): Integer;

procedure XmlUtf8Convert(Enc: ENCODING_ptr; FromP: PPAnsiChar;
  FromLim: PAnsiChar; ToP: PPAnsiChar; ToLim: PAnsiChar);
procedure XmlUtf16Convert(Enc: ENCODING_ptr; FromP: PPAnsiChar;
  FromLim: PAnsiChar; ToP: PPWord; ToLim: PWord);

function XmlUtf8Encode(CharNumber: Integer; Buf: PAnsiChar): Integer;
function XmlUtf16Encode(CharNumber: Integer; Buf: PWord): Integer;

{ This is used for performing a 2nd-level tokenization on the content
  of a literal that has already been returned by XmlTok. }
function XmlLiteralTok(Enc: ENCODING_ptr; LiteralType: Integer; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
function XmlAttributeValueTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
function XmlEntityValueTok(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NextTokPtr: PPAnsiChar): Integer;
function XmlSameName(Enc: ENCODING_ptr; Ptr1, Ptr2: PAnsiChar): Integer;
function XmlNameMatchesAscii(Enc: ENCODING_ptr;
  Ptr1, End1, Ptr2: PAnsiChar): Integer;
function XmlNameLength(Enc: ENCODING_ptr; Ptr: PAnsiChar): Integer;
function XmlGetAttributes(Enc: ENCODING_ptr; Ptr: PAnsiChar; AttsMax: Integer;
  Atts: ATTRIBUTE_ptr): Integer;
function XmlCharRefNumber(Enc: ENCODING_ptr; Ptr: PAnsiChar): Integer;
function XmlPredefinedEntityName(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar): Integer;

function XmlParseXmlDecl(IsGeneralTextEntity: Integer; Enc: ENCODING_ptr;
  Ptr, End_: PAnsiChar; BadPtr, VersionPtr, VersionEndPtr, EncodingNamePtr
  : PPAnsiChar; NamedEncodingPtr: ENCODING_ptr_ptr;
  StandalonePtr: PInteger): Integer;

function XmlParseXmlDeclNS(IsGeneralTextEntity: Integer; Enc: ENCODING_ptr;
  Ptr, End_: PAnsiChar; BadPtr, VersionPtr, VersionEndPtr, EncodingNamePtr
  : PPAnsiChar; NamedEncodingPtr: ENCODING_ptr_ptr;
  StandalonePtr: PInteger): Integer;

implementation


type
  EncodingFinder_func = function(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar)
    : ENCODING_ptr;

const
{$I ascii.inc }
  KW_version: array [0..7] of AnsiChar = (ASCII_vl, ASCII_el, ASCII_rl, ASCII_sl,
    ASCII_il, ASCII_ol, ASCII_nl, #0);

  KW_encoding: array [0..8] of AnsiChar = (ASCII_el, ASCII_nl, ASCII_cl, ASCII_ol,
    ASCII_dl, ASCII_il, ASCII_nl, ASCII_gl, #0);

  KW_standalone: array [0..10] of AnsiChar = (ASCII_sl, ASCII_tl, ASCII_al,
    ASCII_nl, ASCII_dl, ASCII_al, ASCII_ll, ASCII_ol, ASCII_nl, ASCII_el, #0);

  KW_yes: array [0..3] of AnsiChar = (ASCII_yl, ASCII_el, ASCII_sl, #0);

  KW_no: array [0..2] of AnsiChar = (ASCII_nl, ASCII_ol, #0);

  { MINBPC }
function MINBPC(Enc: ENCODING_ptr): Integer;
begin
{$IFDEF XML_MIN_SIZE }
  Result := Enc.MinBytesPerChar;

{$ELSE }
  Result := 1;

{$ENDIF }
end;

{ utf8_toUtf8 }{ unicode }
procedure Utf8_toUtf8(Enc: ENCODING_ptr; FromP: PPAnsiChar; FromLim: PAnsiChar;
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

{ utf8_toUtf16 {.. }{ unicode }
procedure Utf8_toUtf16(Enc: ENCODING_ptr; FromP: PPAnsiChar;
  FromLim: PAnsiChar; ToP: PPWord; ToLim: PWord);
begin
end;

{ sb_byteType {.. }
function Sb_byteType(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
end;

{ isNever }
function IsNever(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
  Result := 0;
end;

{ sb_byteToAscii }
function Sb_byteToAscii(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
  Result := Integer(P^);
end;

{ sb_charMatches }
function Sb_charMatches(Enc: ENCODING_ptr; P: PAnsiChar; C: Integer): Integer;
begin
  Result := Integer(Int(P^) = C);

end;

{ utf8_isName2 {.. }
function Utf8_isName2(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
end;

{ utf8_isName3 {.. }
function Utf8_isName3(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
end;

{ utf8_isNmstrt2 {.. }
function Utf8_isNmstrt2(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
end;

{ utf8_isNmstrt3 {.. }
function Utf8_isNmstrt3(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
end;

{ utf8_isInvalid2 {.. }
function Utf8_isInvalid2(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
end;

{ utf8_isInvalid3 {.. }
function Utf8_isInvalid3(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
end;

{ utf8_isInvalid4 {.. }
function Utf8_isInvalid4(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
end;


type
  Normal_encoding_ptr = ^Normal_encoding;

  Normal_encoding = record
    Enc: ENCODING;
    Type_: array [0..255] of Int8u;

{$IFDEF XML_MIN_SIZE }
    ByteType: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsNameMin: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsNmstrtMin: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    ByteToAscii: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    CharMatches: function(P1: ENCODING_ptr; P2: PAnsiChar; P3: Integer): Integer;

{$ENDIF }
    IsName2: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsName3: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsName4: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsNmstrt2: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsNmstrt3: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsNmstrt4: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsInvalid2: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsInvalid3: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;
    IsInvalid4: function(P1: ENCODING_ptr; P2: PAnsiChar): Integer;

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

  { BYTE_TYPE }
function BYTE_TYPE(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
{$IFDEF XML_MIN_SIZE }
  Result := Normal_encoding_ptr(Enc).ByteType(Enc, P);

{$ELSE }
  Result := Normal_encoding_ptr(Enc).Type_[Int8u(P^)];

{$ENDIF }
end;

{ BYTETo_ASCII }
function BYTETo_ASCII(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
{$IFDEF XML_MIN_SIZE }
  Result := Normal_encoding_ptr(Enc).ByteToAscii(Enc, P);

{$ELSE }
  Result := PByte(P)^;

{$ENDIF }
end;

{ CHAR_MATCHES }
function CHAR_MATCHES(Enc: ENCODING_ptr; P: PAnsiChar; C: Integer): Integer;
begin
{$IFDEF XML_MIN_SIZE }
  Result := Normal_encoding_ptr(Enc).CharMatches(Enc, P, C);

{$ELSE }
  Result := Integer(PByte(P)^ = C);

{$ENDIF }
end;

{ IS_NAME_CHAR }
function IS_NAME_CHAR(Enc: ENCODING_ptr; P: PAnsiChar; N: Integer): Integer;
begin
  case N of
    2:
      Result := Normal_encoding_ptr(Enc).IsName2(Enc, P);
    3:
      Result := Normal_encoding_ptr(Enc).IsName3(Enc, P);
    4:
      Result := Normal_encoding_ptr(Enc).IsName4(Enc, P);

  end;

end;

{ IS_NMSTRT_CHAR }
function IS_NMSTRT_CHAR(Enc: ENCODING_ptr; P: PAnsiChar; N: Integer): Integer;
begin
  case N of
    2:
      Result := Normal_encoding_ptr(Enc).IsNmstrt2(Enc, P);
    3:
      Result := Normal_encoding_ptr(Enc).IsNmstrt3(Enc, P);
    4:
      Result := Normal_encoding_ptr(Enc).IsNmstrt4(Enc, P);
  end;
end;

{ IS_INVALID_CHAR }
function IS_INVALID_CHAR(Enc: ENCODING_ptr; P: PAnsiChar; N: Integer): Integer;
begin
  case N of
    2:
      Result := Normal_encoding_ptr(Enc).IsInvalid2(Enc, P);
    3:
      Result := Normal_encoding_ptr(Enc).IsInvalid3(Enc, P);
    4:
      Result := Normal_encoding_ptr(Enc).IsInvalid4(Enc, P);
  end;
end;

{ IS_NAME_CHAR_MINBPC }
function IS_NAME_CHAR_MINBPC(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
{$IFDEF XML_MIN_SIZE }
  Result := Normal_encoding_ptr(Enc).IsNameMin(Enc, P);

{$ELSE }
  Result := 0;

{$ENDIF }
end;

{ IS_NMSTRT_CHAR_MINBPC }
function IS_NMSTRT_CHAR_MINBPC(Enc: ENCODING_ptr; P: PAnsiChar): Integer;
begin
{$IFDEF XML_MIN_SIZE }
  Result := Normal_encoding_ptr(Enc).IsNmstrtMin(Enc, P);

{$ELSE }
  Result := 0;

{$ENDIF }
end;

{ INIT_ENC_INDEX }
function INIT_ENC_INDEX(Enc: INIT_ENCODING_ptr): Integer;
begin
  Result := Integer(Enc.InitEnc.IsUtf16);
end;

{ SET_INIT_ENC_INDEX }
procedure SET_INIT_ENC_INDEX(Enc: INIT_ENCODING_ptr; I: Integer);
begin
  Enc.InitEnc.IsUtf16 := AnsiChar(I);
end;

{$I xmltok_impl.inc }

const
{$IFDEF XML_NS }
  Utf8_encoding_ns: Normal_encoding =
    (Enc: (Scanners: (Normal_prologTok, Normal_contentTok,
    Normal_cdataSectionTok {$IFDEF XML_DTD }, Normal_ignoreSectionTok
    {$ENDIF } ); LiteralScanners: (Normal_attributeValueTok,
    Normal_entityValueTok);

    SameName: Normal_sameName; NameMatchesAscii: Normal_nameMatchesAscii;
    NameLength: Normal_nameLength; SkipS: Normal_skipS; GetAtts: Normal_getAtts;
    CharRefNumber: Normal_charRefNumber;
    PredefinedEntityName: Normal_predefinedEntityName;
    UpdatePosition: Normal_updatePosition; IsPublicId: Normal_isPublicId;
    Utf8Convert: Utf8_toUtf8; Utf16Convert: Utf8_toUtf16;

    MinBytesPerChar: 1;

    IsUtf8: #1; IsUtf16: #0); Type_: ({$I asciitab.inc}
{$I utf8tab.inc});

{$IFDEF XML_MIN_SIZE }
    ByteType: Sb_byteType; IsNameMin: IsNever; IsNmstrtMin: IsNever;
    ByteToAscii: Sb_byteToAscii; CharMatches: Sb_charMatches;

{$ENDIF }
    IsName2: Utf8_isName2; IsName3: Utf8_isName3; IsName4: IsNever;
    IsNmstrt2: Utf8_isNmstrt2; IsNmstrt3: Utf8_isNmstrt3; IsNmstrt4: IsNever;
    IsInvalid2: Utf8_isInvalid2; IsInvalid3: Utf8_isInvalid3;
    IsInvalid4: Utf8_isInvalid4);

{$ENDIF }
  Utf8_encoding: Normal_encoding = (Enc: (Scanners: (Normal_prologTok,
    Normal_contentTok, Normal_cdataSectionTok
    {$IFDEF XML_DTD }, Normal_ignoreSectionTok
    {$ENDIF } ); LiteralScanners: (Normal_attributeValueTok,
    Normal_entityValueTok);

    SameName: Normal_sameName; NameMatchesAscii: Normal_nameMatchesAscii;
    NameLength: Normal_nameLength; SkipS: Normal_skipS; GetAtts: Normal_getAtts;
    CharRefNumber: Normal_charRefNumber;
    PredefinedEntityName: Normal_predefinedEntityName;
    UpdatePosition: Normal_updatePosition; IsPublicId: Normal_isPublicId;
    Utf8Convert: Utf8_toUtf8; Utf16Convert: Utf8_toUtf16;

    MinBytesPerChar: 1;

    IsUtf8: #1; IsUtf16: #0); Type_: ({$I asciitab_bt_colon_.inc}
{$I utf8tab.inc});

{$IFDEF XML_MIN_SIZE }
    ByteType: Sb_byteType; IsNameMin: IsNever; IsNmstrtMin: IsNever;
    ByteToAscii: Sb_byteToAscii; CharMatches: Sb_charMatches;

{$ENDIF }
    IsName2: Utf8_isName2; IsName3: Utf8_isName3; IsName4: IsNever;
    IsNmstrt2: Utf8_isNmstrt2; IsNmstrt3: Utf8_isNmstrt3; IsNmstrt4: IsNever;
    IsInvalid2: Utf8_isInvalid2; IsInvalid3: Utf8_isInvalid3;
    IsInvalid4: Utf8_isInvalid4);

{$IFDEF XML_NS }
  Internal_utf8_encoding_ns: Normal_encoding =
    (Enc: (Scanners: (Normal_prologTok, Normal_contentTok,
    Normal_cdataSectionTok {$IFDEF XML_DTD }, Normal_ignoreSectionTok
    {$ENDIF } ); LiteralScanners: (Normal_attributeValueTok,
    Normal_entityValueTok);

    SameName: Normal_sameName; NameMatchesAscii: Normal_nameMatchesAscii;
    NameLength: Normal_nameLength; SkipS: Normal_skipS; GetAtts: Normal_getAtts;
    CharRefNumber: Normal_charRefNumber;
    PredefinedEntityName: Normal_predefinedEntityName;
    UpdatePosition: Normal_updatePosition; IsPublicId: Normal_isPublicId;
    Utf8Convert: Utf8_toUtf8; Utf16Convert: Utf8_toUtf16;

    MinBytesPerChar: 1;

    IsUtf8: #1; IsUtf16: #0); Type_: ({$I iasciitab.inc}
{$I utf8tab.inc});

{$IFDEF XML_MIN_SIZE }
    ByteType: Sb_byteType; IsNameMin: IsNever; IsNmstrtMin: IsNever;
    ByteToAscii: Sb_byteToAscii; CharMatches: Sb_charMatches;

{$ENDIF }
    IsName2: Utf8_isName2; IsName3: Utf8_isName3; IsName4: IsNever;
    IsNmstrt2: Utf8_isNmstrt2; IsNmstrt3: Utf8_isNmstrt3; IsNmstrt4: IsNever;
    IsInvalid2: Utf8_isInvalid2; IsInvalid3: Utf8_isInvalid3;
    IsInvalid4: Utf8_isInvalid4);

{$ENDIF }
  Internal_utf8_encoding: Normal_encoding =
    (Enc: (Scanners: (Normal_prologTok, Normal_contentTok,
    Normal_cdataSectionTok {$IFDEF XML_DTD }, Normal_ignoreSectionTok
    {$ENDIF } ); LiteralScanners: (Normal_attributeValueTok,
    Normal_entityValueTok);

    SameName: Normal_sameName; NameMatchesAscii: Normal_nameMatchesAscii;
    NameLength: Normal_nameLength; SkipS: Normal_skipS; GetAtts: Normal_getAtts;
    CharRefNumber: Normal_charRefNumber;
    PredefinedEntityName: Normal_predefinedEntityName;
    UpdatePosition: Normal_updatePosition; IsPublicId: Normal_isPublicId;
    Utf8Convert: Utf8_toUtf8; Utf16Convert: Utf8_toUtf16;

    MinBytesPerChar: 1;

    IsUtf8: #1; IsUtf16: #0); Type_: ({$I iasciitab_bt_colon_.inc}
{$I utf8tab.inc});

{$IFDEF XML_MIN_SIZE }
    ByteType: Sb_byteType; IsNameMin: IsNever; IsNmstrtMin: IsNever;
    ByteToAscii: Sb_byteToAscii; CharMatches: Sb_charMatches;

{$ENDIF }
    IsName2: Utf8_isName2; IsName3: Utf8_isName3; IsName4: IsNever;
    IsNmstrt2: Utf8_isNmstrt2; IsNmstrt3: Utf8_isNmstrt3; IsNmstrt4: IsNever;
    IsInvalid2: Utf8_isInvalid2; IsInvalid3: Utf8_isInvalid3;
    IsInvalid4: Utf8_isInvalid4);

{$IFDEF XML_NS }
  Latin1_encoding_ns: Normal_encoding = (); {..}

{$ENDIF }
  Latin1_encoding: Normal_encoding = (); {..}

{$IFDEF XML_NS }
  Ascii_encoding_ns: Normal_encoding = (); {..}

{$ENDIF }
  Ascii_encoding: Normal_encoding = (); {..}

{$IFDEF XML_NS }
  Little2_encoding_ns: Normal_encoding = (); {..}

{$ENDIF }
  Little2_encoding: Normal_encoding = (); {..}

{$IFDEF XML_NS }
  Big2_encoding_ns: Normal_encoding = (); {..}

{$ENDIF }
  Big2_encoding: Normal_encoding = (); {..}

  { If this enumeration is changed, getEncodingIndex and encodings
    must also be changed. }
  UNKNOWN_ENC = -1;
  ISO_8859_1_ENC = 0;
  US_ASCII_ENC = 1;
  UTF_8_ENC = 2;
  UTF_16_ENC = 3;
  UTF_16BE_ENC = 4;
  UTF_16LE_ENC = 5;
  NO_ENC = 6; { must match encodingNames up to here }

  KW_ISO_8859_1: array [0..10] of AnsiChar = (ASCII_I, ASCII_S, ASCII_O,
    ASCII_MINUS, ASCII_8, ASCII_8, ASCII_5, ASCII_9, ASCII_MINUS, ASCII_1, #0);

  KW_US_ASCII: array [0..8] of AnsiChar = (ASCII_U, ASCII_S, ASCII_MINUS, ASCII_A,
    ASCII_S, ASCII_C, ASCII_I, ASCII_I, #0);

  KW_UTF_8: array [0..5] of AnsiChar = (ASCII_U, ASCII_T, ASCII_F, ASCII_MINUS,
    ASCII_8, #0);

  KW_UTF_16: array [0..6] of AnsiChar = (ASCII_U, ASCII_T, ASCII_F, ASCII_MINUS,
    ASCII_1, ASCII_6, #0);

  KW_UTF_16BE: array [0..8] of AnsiChar = (ASCII_U, ASCII_T, ASCII_F, ASCII_MINUS,
    ASCII_1, ASCII_6, ASCII_B, ASCII_E, #0);

  KW_UTF_16LE: array [0..8] of AnsiChar = (ASCII_U, ASCII_T, ASCII_F, ASCII_MINUS,
    ASCII_1, ASCII_6, ASCII_L, ASCII_E, #0);

  
  { streqci }
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

{ initUpdatePosition {.. }
procedure InitUpdatePosition(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  Pos: POSITION_ptr);
begin
end;

{ getEncodingIndex }
function GetEncodingIndex(Name: PAnsiChar): Integer;
const
  EncodingNames: array [0..5] of PAnsiChar = (@KW_ISO_8859_1, @KW_US_ASCII,
    @KW_UTF_8, @KW_UTF_16, @KW_UTF_16BE, @KW_UTF_16LE);

var
  I: Integer;

begin
  if name = nil then
    Result := NO_ENC
  else
  begin
    I := 0;

    while I < SizeOf(EncodingNames) div SizeOf(EncodingNames[0]) do
    begin
      if Streqci(name, EncodingNames[I]) <> 0 then
      begin
        Result := I;

        Exit;
      end;

      Inc(I);

    end;

    Result := UNKNOWN_ENC;

  end;
end;

{ initScan }
{ This is what detects the encoding.  encodingTable maps from
  encoding indices to encodings; int8u(enc.initEnc.isUtf16 ) is the index of
  the external (protocol) specified encoding; state is
  XML_CONTENT_STATE if we're parsing an external text entity, and
  XML_PROLOG_STATE otherwise. }
function InitScan(EncodingTable: ENCODING_ptr_ptr; Enc: INIT_ENCODING_ptr;
  State: Integer; Ptr, End_: PAnsiChar; NextTokPtr: PPAnsiChar): Integer;
var
  EncPtr: ENCODING_ptr_ptr;

  E: Integer;

label
  _003C, _esc;

begin
  if Ptr = End_ then
  begin
    Result := XML_TOK_NONE;

    Exit;
  end;

  EncPtr := Enc.EncPtr;

  { only a single byte available for auto-detection }
  if PtrComp(Ptr) + 1 = PtrComp(End_) then
  begin
{$IFNDEF XML_DTD } { FIXME }
    { a well-formed document entity must have more than one byte }
    if State <> XML_CONTENT_STATE then
    begin
      Result := XML_TOK_PARTIAL;

      Exit;
    end;

{$ENDIF }
    { so we're parsing an external text entity... }
    { if UTF-16 was externally specified, then we need at least 2 bytes }
    case INIT_ENC_INDEX(Enc) of
      UTF_16_ENC, UTF_16LE_ENC, UTF_16BE_ENC:
        begin
          Result := XML_TOK_PARTIAL;

          Exit;
        end;
    end;

    case Int8u(Ptr^) of
      $FE, $FF, $EF: { possibly first byte of UTF-8 BOM }
        if (INIT_ENC_INDEX(Enc) = ISO_8859_1_ENC) and (State = XML_CONTENT_STATE)
        then
        else
          goto _003C;

      $00, $3C: { fall through }
      _003C:
        begin
          Result := XML_TOK_PARTIAL;

          Exit;
        end;
    end;

  end
  else
    case (PtrComp(Ptr^) shl 8) or PByte(PtrComp(Ptr) + 1)^ of
      $FEFF:
        if (INIT_ENC_INDEX(Enc) = ISO_8859_1_ENC) and (State = XML_CONTENT_STATE)
        then
        else
        begin
          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 2);
          EncPtr^ := ENCODING_ptr_ptr(PtrComp(EncodingTable) + UTF_16BE_ENC *
            SizeOf(ENCODING_ptr))^;

          Result := XML_TOK_BOM;

          Exit;
        end;

      { 00 3C is handled in the default case }
      $3C00:
        if ((INIT_ENC_INDEX(Enc) = UTF_16BE_ENC) or
          (INIT_ENC_INDEX(Enc) = UTF_16_ENC)) and (State = XML_CONTENT_STATE)
        then
        else
        begin
          EncPtr^ := ENCODING_ptr_ptr(PtrComp(EncodingTable) + UTF_16LE_ENC *
            SizeOf(ENCODING_ptr))^;
          Result := XmlTok_(EncPtr^, State, Ptr, End_, NextTokPtr);

          Exit;
        end;

      $FFFE:
        if (INIT_ENC_INDEX(Enc) = ISO_8859_1_ENC) and (State = XML_CONTENT_STATE)
        then
        else
        begin
          NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 2);
          EncPtr^ := ENCODING_ptr_ptr(PtrComp(EncodingTable) + UTF_16LE_ENC *
            SizeOf(ENCODING_ptr))^;

          Result := XML_TOK_BOM;

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
          if State = XML_CONTENT_STATE then
          begin
            E := INIT_ENC_INDEX(Enc);

            if (E = ISO_8859_1_ENC) or (E = UTF_16BE_ENC) or (E = UTF_16LE_ENC)
              or (E = UTF_16_ENC) then
              goto _esc;
          end;

          if PtrComp(Ptr) + 2 = PtrComp(End_) then
          begin
            Result := XML_TOK_PARTIAL;

            Exit;
          end;

          if PByte(PtrComp(Ptr) + 2)^ = $BF then
          begin
            NextTokPtr^ := PAnsiChar(PtrComp(Ptr) + 3);
            EncPtr^ := ENCODING_ptr_ptr(PtrComp(EncodingTable) + UTF_8_ENC *
              SizeOf(ENCODING_ptr))^;

            Result := XML_TOK_BOM;

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
        if (State = XML_CONTENT_STATE) and (INIT_ENC_INDEX(Enc) = UTF_16LE_ENC)
        then
          goto _esc;

        EncPtr^ := ENCODING_ptr_ptr(PtrComp(EncodingTable) + UTF_16BE_ENC *
          SizeOf(ENCODING_ptr))^;
        Result := XmlTok_(EncPtr^, State, Ptr, End_, NextTokPtr);

        Exit;

      end
      else
        { We could recover here in the case:
          - parsing an external entity
          - second byte is 0
          - no externally specified encoding
          - no encoding declaration
          by assuming UTF-16LE.  But we don't, because this would mean when
          presented just with a single byte, we couldn't reliably determine
          whether we needed further bytes. }
        if PByte(PtrComp(Ptr) + 1)^ = 0 then
        begin
          if State = XML_CONTENT_STATE then
            goto _esc;

          EncPtr^ := ENCODING_ptr_ptr(PtrComp(EncodingTable) + UTF_16LE_ENC *
            SizeOf(ENCODING_ptr))^;
          Result := XmlTok_(EncPtr^, State, Ptr, End_, NextTokPtr);
        end;
    end;

_esc:
  EncPtr^ := ENCODING_ptr_ptr(PtrComp(EncodingTable) + INIT_ENC_INDEX(Enc) *
    SizeOf(ENCODING_ptr))^;
  Result := XmlTok_(EncPtr^, State, Ptr, End_, NextTokPtr);
end;

{ toAscii }
function ToAscii(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar): Integer;
var
  Buf: array [0..0] of AnsiChar;

  P: PAnsiChar;

begin
  P := @Buf[0];

  XmlUtf8Convert(Enc, @Ptr, End_, @P, PAnsiChar(PtrComp(P) + 1));

  if P = @Buf[0] then
    Result := -1
  else
    Result := Integer(Buf[0]);
end;

{ isSpace }
function IsSpace(C: Integer): Integer;
begin
  case C of
    $20, $D, $A, $9:
      Result := 1;

  else
    Result := 0;
  end;
end;

{ parsePseudoAttribute }
{ Return 1 if there's just optional white space or there's an S
  folLowed by name=val. }
function ParsePseudoAttribute(Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  NamePtr, NameEndPtr, ValPtr, NextTokPtr: PPAnsiChar): Integer;
var
  C: Integer;

  Open: AnsiChar;

begin
  if Ptr = End_ then
  begin
    NamePtr^ := nil;
    Result := 1;

    Exit;
  end;

  if IsSpace(ToAscii(Enc, Ptr, End_)) = 0 then
  begin
    NextTokPtr^ := Ptr;

    Result := 0;

    Exit;
  end;

  repeat
    Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

  until IsSpace(ToAscii(Enc, Ptr, End_)) = 0;

  if Ptr = End_ then
  begin
    NamePtr^ := nil;
    Result := 1;

    Exit;
  end;

  NamePtr^ := Ptr;

  repeat
    C := ToAscii(Enc, Ptr, End_);

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

        C := ToAscii(Enc, Ptr, End_);

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

  C := ToAscii(Enc, Ptr, End_);

  while IsSpace(C) <> 0 do
  begin
    Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

    C := ToAscii(Enc, Ptr, End_);
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
    C := ToAscii(Enc, Ptr, End_);

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

{ doParseXmlDecl }
function DoParseXmlDecl(EncodingFinder: EncodingFinder_func;
  IsGeneralTextEntity: Integer; Enc: ENCODING_ptr; Ptr, End_: PAnsiChar;
  BadPtr, VersionPtr, VersionEndPtr, EncodingName: PPAnsiChar;
  Encoding: ENCODING_ptr_ptr; Standalone: PInteger): Integer;
var
  Val, Name, NameEnd: PAnsiChar;

  C: Integer;

begin
  Val := nil;
  name := nil;
  NameEnd := nil;

  Inc(PtrComp(Ptr), 5 * Enc.MinBytesPerChar);
  Dec(PtrComp(End_), 2 * Enc.MinBytesPerChar);

  if (ParsePseudoAttribute(Enc, Ptr, End_, @name, @NameEnd, @Val, @Ptr) = 0) or
    (name = nil) then
  begin
    BadPtr^ := Ptr;
    Result := 0;

    Exit;
  end;

  if XmlNameMatchesAscii(Enc, name, NameEnd, @KW_version[0]) = 0 then
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

    if ParsePseudoAttribute(Enc, Ptr, End_, @name, @NameEnd, @Val, @Ptr) = 0
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

  if XmlNameMatchesAscii(Enc, name, NameEnd, @KW_encoding[0]) <> 0 then
  begin
    C := ToAscii(Enc, Val, End_);

    if not((Int(ASCII_al) <= C) and (C <= Integer(ASCII_zl))) and
      not((Int(ASCII_A) <= C) and (C <= Integer(ASCII_Z))) then
    begin
      BadPtr^ := Val;
      Result := 0;

      Exit;
    end;

    if EncodingName <> nil then
      EncodingName^ := Val;

    if Encoding <> nil then
      Encoding^ := EncodingFinder(Enc, Val,
        PAnsiChar(PtrComp(Ptr) - Enc.MinBytesPerChar));

    if ParsePseudoAttribute(Enc, Ptr, End_, @name, @NameEnd, @Val, @Ptr) = 0
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

  if (XmlNameMatchesAscii(Enc, name, NameEnd, @KW_standalone[0]) = 0) or
    (IsGeneralTextEntity <> 0) then
  begin
    BadPtr^ := name;
    Result := 0;

    Exit;
  end;

  if XmlNameMatchesAscii(Enc, Val, PAnsiChar(PtrComp(Ptr) - Enc.MinBytesPerChar),
    @KW_yes[0]) <> 0 then
    if Standalone <> nil then
      Standalone^ := 1
    else
  else if XmlNameMatchesAscii(Enc, Val,
    PAnsiChar(PtrComp(Ptr) - Enc.MinBytesPerChar), @KW_no[0]) <> 0 then
    if Standalone <> nil then
      Standalone^ := 0
    else
  else
  begin
    BadPtr^ := Val;
    Result := 0;

    Exit;
  end;

  while IsSpace(ToAscii(Enc, Ptr, End_)) <> 0 do
    Inc(PtrComp(Ptr), Enc.MinBytesPerChar);

  if Ptr <> End_ then
  begin
    BadPtr^ := Ptr;
    Result := 0;

    Exit;
  end;

  Result := 1;
end;

{$I xmltok_ns.inc }

{ XMLTOK_ }
function XmlTok_;
begin
  Result := Enc.Scanners[State](Enc, Ptr, End_, NextTokPtr);
end;

{ XMLPROLOGTOK }
function XmlPrologTok;
begin
  Result := XmlTok_(Enc, XML_PROLOG_STATE, Ptr, End_, NextTokPtr);
end;

{ XMLCONTENTTOK }
function XmlContentTok;
begin
  Result := XmlTok_(Enc, XML_CONTENT_STATE, Ptr, End_, NextTokPtr);
end;

{ XMLISPUBLICID }
function XmlIsPublicId;
begin
  Result := Enc.IsPublicId(Enc, Ptr, End_, BadPtr);
end;

{ XMLUTF8CONVERT }
procedure XmlUtf8Convert;
begin
  Enc.Utf8Convert(Enc, FromP, FromLim, ToP, ToLim);
end;

{ XMLUTF16CONVERT }
procedure XmlUtf16Convert;
begin
  Enc.Utf16Convert(Enc, FromP, FromLim, ToP, ToLim);
end;

{ XMLUTF8ENCODE {.. }{ unicode }
function XmlUtf8Encode;
begin
end;

{ XMLUTF16ENCODE {.. }{ unicode }
function XmlUtf16Encode;
begin
end;

{ XMLLITERALTOK }
function XmlLiteralTok;
begin
  Result := Enc.LiteralScanners[LiteralType](Enc, Ptr, End_, NextTokPtr);
end;

{ XMLATTRIBUTEVALUETOK }
function XmlAttributeValueTok;
begin
  Result := XmlLiteralTok(Enc, XML_ATTRIBUTE_VALUE_LITERAL, Ptr, End_,
    NextTokPtr);
end;

{ XMLENTITYVALUETOK }
function XmlEntityValueTok;
begin
  Result := XmlLiteralTok(Enc, XML_ENTITY_VALUE_LITERAL, Ptr, End_, NextTokPtr);
end;

{ XMLSAMENAME }
function XmlSameName;
begin
  Result := Enc.SameName(Enc, Ptr1, Ptr2);
end;

{ XMLNAMEMATCHESASCII }
function XmlNameMatchesAscii;
begin
  Result := Enc.NameMatchesAscii(Enc, Ptr1, End1, Ptr2);
end;

{ XMLNAMELENGTH }
function XmlNameLength;
begin
  Result := Enc.NameLength(Enc, Ptr);
end;

{ XMLGETATTRIBUTES }
function XmlGetAttributes;
begin
  Result := Enc.GetAtts(Enc, Ptr, AttsMax, Atts);
end;

{ XMLCHARREFNUMBER }
function XmlCharRefNumber;
begin
  Result := Enc.CharRefNumber(Enc, Ptr);
end;

{ XMLPREDEFINEDENTITYNAME }
function XmlPredefinedEntityName;
begin
  Result := Enc.PredefinedEntityName(Enc, Ptr, End_);
end;

{ XMLPARSEXMLDECLNS {.. }
function XmlParseXmlDeclNS;
begin
end;

end.
