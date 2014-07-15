//----------------------------------------------------------------------------
// copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
//                                and Clark Cooper
// copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port By: Milan Marusinec alias Milano
//                 milan@marusinec.sk
//                 http://www.pasports.org/pasexpat
// copyright (c) 2006
//
// Permission is hereby granted, free of charge, to any person obtaining
// a Copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, Copy, modify, merge, publish,
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
// IN NO EVENT SHALL THE AUTHORS OR copyright HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

{ xmltok_ns.inc }
const
{$IFDEF XML_NS}
 encodingsNS : array[0..6 ] of ENCODING_ptr = (
  @latin1_encoding_ns.enc  ,
  @ascii_encoding_ns.enc   ,
  @utf8_encoding_ns.enc    ,
  @big2_encoding_ns.enc    ,
  @big2_encoding_ns.enc    ,
  @little2_encoding_ns.enc ,
  @utf8_encoding_ns.enc    ); { NO_ENC }
{$ENDIF}

 encodings : array[0..6 ] of ENCODING_ptr = (
  @latin1_encoding.enc  ,
  @ascii_encoding.enc   ,
  @utf8_encoding.enc    ,
  @big2_encoding.enc    ,
  @big2_encoding.enc    ,
  @little2_encoding.enc ,
  @utf8_encoding.enc    ); { NO_ENC }

{ initScanProlog }
function initScanProlog(enc : ENCODING_ptr; ptr ,end_ : PAnsiChar; nextTokPtr : PPAnsiChar ) : Integer;
begin
  Result := InitScan(@encodings ,INIT_ENCODING_ptr(enc ) ,XML_PROLOG_STATE ,ptr ,end_ ,nextTokPtr );
end;

{ initScanContent {..}
function initScanContent(enc : ENCODING_ptr; ptr ,end_ : PAnsiChar; nextTokPtr : PPAnsiChar ) : Integer;
begin
end;

{ XMLINITENCODING }
function XmlInitEncoding;
var
  i : Integer;
begin
  i := GetEncodingIndex(name );

  if i = UNKNOWN_ENC then
  begin
    Result := 0;
    Exit;
   end;

  SET_INIT_ENC_INDEX(p ,i );

  p.initEnc.scanners[XML_PROLOG_STATE  ]:=@initScanProlog;
  p.initEnc.scanners[XML_CONTENT_STATE ]:=@initScanContent;

  p.initEnc.updatePosition:=@initUpdatePosition;

  p.encPtr:=encPtr;
  encPtr^ :=@p.initEnc;

  Result := 1;
end;

{ XMLINITENCODINGNS }
function XmlInitEncodingNS;
begin
end;

{ XmlGetUtf8InternalEncoding }
function XmlGetUtf8InternalEncoding : ENCODING_ptr;
begin
  Result := @internal_utf8_encoding.enc;
end;

{ XmlGetUtf16InternalEncoding {..}
function XmlGetUtf16InternalEncoding : ENCODING_ptr;
begin
end;

{ XMLGETINTERNALENCODING }
function XmlGetInternalEncoding;
begin
{$IFDEF XML_UNICODE }
  Result := XmlGetUtf16InternalEncoding;
{$ELSE}
  Result := XmlGetUtf8InternalEncoding;
{$ENDIF}
end;

{ XmlGetUtf8InternalEncodingNS {..}
function XmlGetUtf8InternalEncodingNS : ENCODING_ptr;
begin
end;

{ XmlGetUtf16InternalEncodingNS {..}
function XmlGetUtf16InternalEncodingNS : ENCODING_ptr;
begin
end;

{ XMLGETINTERNALENCODINGNS }
function XmlGetInternalEncodingNS;
begin
{$IFDEF XML_UNICODE}
  Result := XmlGetUtf16InternalEncodingNS;
{$ELSE}
  Result := XmlGetUtf8InternalEncodingNS;
{$ENDIF}
end;

{ findEncoding {..}
function findEncoding(enc : ENCODING_ptr; ptr ,end_ : PAnsiChar ) : ENCODING_ptr;
begin
end;

{ XMLPARSEXMLDECL }
function XmlParseXmlDecl;
begin
  Result := doParseXmlDecl(
   @findEncoding ,
   isGeneralTextEntity ,
   enc ,ptr ,end_ ,
   badPtr ,
   versionPtr ,
   versionEndPtr ,
   encodingNamePtr ,
   namedEncodingPtr ,
   standalonePtr );

end;

