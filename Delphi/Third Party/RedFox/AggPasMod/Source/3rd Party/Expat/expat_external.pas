unit expat_external;

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

{$I expat_mode.inc }


type
  (* General Integer Types *)
  Int8 = Shortint;
  Int8u = Byte;
  Int16 = Smallint;
  Int16u = Word;
  Int32 = Longint;
  Int32u = Longword;
  Int64 = System.Int64;

  {$IFDEF FPC }
  Int64u = Qword;

  {$ELSE }
  Int64u = System.Int64;

  {$ENDIF }
  (* General Character Types *)
  Char8 = Char;
  Char16 = Int16u;
  Char32 = Int32u;

  (* C/C++ compatibility Types *)
  Int = Int32;
  Cardinal = Int32u;
  Size_t = Int32u;

  (* Pascal Pointer Computation Type *)
  {$IFDEF CPU64 }
  PtrComp = System.Int64;

  {$ELSE }
  PtrComp = Integer;

  {$ENDIF }
  (* Type Pointers *)
  Int8_ptr = ^Int8;
  Int8_ptr_ptr = ^Int8_ptr;

  Int8u_ptr = ^Int8u;
  Int8u_ptr_ptr = ^Int8u_ptr;

  Int16_ptr = ^Int16;
  Int16_ptr_ptr = ^Int16_ptr;

  TInt16uAccessptr = ^Int16u;
  TInt16uAccessptr_ptr = ^TInt16uAccessptr;

  TInt32Accessptr = ^Int32;
  TInt32Accessptr_ptr = ^TInt32Accessptr;

  TInt32uAccessptr = ^Int32u;
  TInt32uAccessptr_ptr = ^TInt32uAccessptr;

  Int64_ptr = ^Int64;
  Int64_ptr_ptr = ^Int64_ptr;

  TInt64uAccessptr = ^Int64u;
  TInt64uAccessptr_ptr = ^TInt64uAccessptr;

  Char8_ptr = ^Char8;
  Char8_ptr_ptr = ^Char8_ptr;

  Char16_ptr = ^Char16;
  Char16_ptr_ptr = ^Char16_ptr;

  Char32_ptr = ^Char32;
  Char32_ptr_ptr = ^Char32_ptr;

  PInt = ^Int;
  PInt_ptr = ^PInt;

  PCardinal = ^Cardinal;
  PCardinal_ptr = ^PCardinal;

  PPAnsiChar = ^PAnsiChar;

  (* Expat Types *)
  {$IFDEF XML_UNICODE } // Information is UTF-16 encoded.
  {$IFDEF XML_UNICODE_WCHAR_T }
  XML_Char = Int16u;
  XML_LChar = Int16u;

  {$ELSE }
  XML_Char = Word;
  XML_LChar = AnsiChar;

  {$ENDIF }
  {$ELSE }              // Information is UTF-8 encoded.
  XML_Char = AnsiChar;
  XML_LChar = AnsiChar;

  {$ENDIF }
  XML_PAnsiChar = ^XML_Char;
  XML_LPAnsiChar = ^XML_LChar;

  XML_PPAnsiChar = ^XML_PAnsiChar;

  {$IFDEF XML_LARGE_SIZE } // Use large integers for file/stream positions.
  XML_Index = Int64;
  XML_Size = Int64u;

  {$ELSE }
  XML_Index = Longint;
  XML_Size = Longword;

  {$ENDIF }

implementation




end.

