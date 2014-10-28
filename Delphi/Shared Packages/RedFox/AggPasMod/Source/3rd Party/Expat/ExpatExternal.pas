unit ExpatExternal;

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
// 03.05.2006-Milano: Unit port establishment
//

interface

{$I ExpatMode.inc }

type
  (* General Integer Types *)
  Int8 = Shortint;
  Int8u = Byte;
  Int16 = Smallint;
  Int16u = Word;
  Int32 = Longint;
  Int32u = Longword;
  Int64 = System.Int64;

  {$IFDEF FPC}
  Int64u = Qword;
  {$ELSE }
  Int64u = System.Int64;
  {$ENDIF}

  (* General Character Types *)
  Char8 = AnsiChar;
  Char16 = Int16u;
  Char32 = Int32u;

  (* C/C++ compatibility Types *)
  Int = Int32;
  Cardinal = Int32u;
  TSize = Int32u;

  (* Pascal Pointer Computation Type *)
  {$IFDEF CPU64}
  PtrComp = System.Int64;
  {$ELSE }
  PtrComp = Integer;
  {$ENDIF}

  (* Type Pointers *)
  PInt8 = ^Int8;
  PPInt8 = ^PInt8;

  PInt8u = ^Int8u;
  PPInt8u = ^PInt8u;

  PInt16 = ^Int16;
  PPInt16 = ^PInt16;

  PInt16uAccess = ^Int16u;
  PPInt16uAccess = ^PInt16uAccess;

  PInt32Access = ^Int32;
  PPInt32Accessptr = ^PInt32Access;

  PInt32uAccess = ^Int32u;
  PPInt32uAccess = ^PInt32uAccess;

  PInt64 = ^Int64;
  PPInt64 = ^PInt64;

  PInt64uAccess = ^Int64u;
  PPInt64uAccess = ^PInt64uAccess;

  PChar8 = ^Char8;
  PPChar8 = ^PChar8;

  PChar16 = ^Char16;
  PPChar16 = ^PChar16;

  PChar32 = ^Char32;
  PPChar32 = ^PChar32;

  PInt = ^Int;
  PPInt = ^PInt;

  PCardinal = ^Cardinal;
  PPCardinal = ^PCardinal;

  PPAnsiChar = ^PAnsiChar;

  (* Expat Types *)
  {$IFDEF XML_UNICODE}
    {$IFDEF XML_UNICODE_WCHAR_T}
    // Information is UTF-16 encoded.
    TXmlChar = Int16u;
    TXmlLChar = Int16u;
    {$ELSE}
    // Information is UTF-8 encoded.
    TXmlChar = Word;
    TXmlLChar = AnsiChar;
    {$ENDIF}
  {$ELSE}
  TXmlChar = AnsiChar;
  TXmlLChar = AnsiChar;
  {$ENDIF}

  PXmlChar = ^TXmlChar;
  PXmlLChar = ^TXmlLChar;
  PPXmlChar = ^PXmlChar;

  {$IFDEF XML_LARGE_SIZE} // Use large integers for file/stream positions.
  TXmlIndex = Int64;
  TXmlSize = Int64u;
  {$ELSE }
  TXmlIndex = LongInt;
  TXmlSize = LongWord;
  {$ENDIF}

implementation

end.
