unit ExpatBasics;

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
//

interface

{$I ExpatMode.inc }

function ExpatGetMem(var Ptr: Pointer; Size: Integer): Boolean;
function ExpatRealloc(var Ptr: Pointer; Old, Size: Integer): Boolean;
function ExpatFreemem(var Ptr: Pointer; Size: Integer): Boolean;

procedure NoP;

// SHR for signed integers is differently implemented in pascal compilers
// than in c++ compilers. On the assembler level, c++ is using the SAR and
// pascal is using SHR. That gives completely different result, when the
// number is negative. We have to be compatible with c++ implementation,
// thus instead of directly using SHR we emulate c++ solution.
function ShrInt8(I, Shift: Shortint): Shortint;
function ShrInt16(I, Shift: SmallInt): SmallInt;
function ShrInt32(I, Shift: Longint): Longint;

implementation

function ExpatGetMem(var Ptr: Pointer; Size: Integer): Boolean;
begin
  Result := False;
  try
    Getmem(Ptr, Size);
    Result := True;
  except
    Ptr := nil;
  end;
end;

function ExpatRealloc(var Ptr: Pointer; Old, Size: Integer): Boolean;
var
  Nb : Pointer;
  Max: Integer;
begin
  if ExpatGetMem(Nb, Size) then
  begin
    Max := Old;

    if Max > Size then
      Max := Size;

    Move(Ptr^, Nb^, Max);

    ExpatFreemem(Ptr, Old);

    Ptr := Nb;
    Result := True;
  end
  else
    Result := False;
end;

function ExpatFreemem(var Ptr: Pointer; Size: Integer): Boolean;
begin
  if Ptr = nil then
    Result := True

  else
    try
      Freemem(Ptr, Size);

      Ptr := nil;
      Result := True;

    except
      Result := False;
    end;
end;

procedure NoP;
begin
end;

function ShrInt8(I, Shift: Shortint): Shortint;
begin
{$IFDEF EXPAT_CPU_386}
  asm
    mov     al ,byte ptr [i ]
    mov     cl ,byte ptr [shift ]
    sar     al ,cl
    mov     byte ptr [result ] ,al
  end;
{$ENDIF}
{$IFDEF EXPAT_CPU_PPC }
  asm
    lbz     r2,i
    extsb   r2,r2
    lbz     r3,shift
    extsb   r3,r3
    sraw    r2,r2,r3
    extsb   r2,r2
    stb     r2,result
  end;
{$ENDIF}
end;

function ShrInt16(I, Shift: SmallInt): SmallInt;
begin
{$IFDEF EXPAT_CPU_386}
  asm
    mov     ax ,word ptr [i ]
    mov     cx ,word ptr [shift ]
    sar     ax ,cl
    mov     word ptr [result ] ,ax
  end;

{$ENDIF}
{$IFDEF EXPAT_CPU_PPC}
  asm
    lha     r2,i
    lha     r3,shift
    sraw    r2,r2,r3
    extsh   r2,r2
    sth     r2,result

  end;
{$ENDIF}
end;

function ShrInt32(I, Shift: Longint): Longint;
begin
{$IFDEF EXPAT_CPU_386}
  asm
    mov     eax, dword ptr [i ]
    mov     ecx, dword ptr [shift ]
    sar     eax, cl
    mov     dword ptr [result ] ,eax
  end;

{$ENDIF}
{$IFDEF EXPAT_CPU_PPC}
  asm
    lwz     r3, i
    lwz     r2, shift
    sraw    r3, r3,r2
    stw     r3, result
  end;
{$ENDIF}
end;

end.
