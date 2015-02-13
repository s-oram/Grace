{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

{$I Spring.inc}

unit Spring.Cryptography.MD5;

interface

{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

uses
  Spring,
  Spring.Cryptography,
  Spring.Cryptography.Base;

type
  TMD5Count = array[0..1] of LongWord;
  TMD5State = array[0..3] of LongWord;
  TMD5Block = array[0..15] of LongWord;
  TMD5CBits = array[0..7] of byte;
  TMD5Digest = array[0..15] of byte;
  TMD5Buffer = array[0..63] of byte;

  TMD5Context = record
    State   : TMD5State;
    Count   : TMD5Count;
    Buffer  : TMD5Buffer;
  end;

  ///	<summary>
  ///	  MD5 Hash
  ///	</summary>
  TMD5 = class(THashAlgorithmBase, IMD5)
  private
    const fCHashSize = 16 * 8; // 256 bits
  private
    fContext: TMD5Context;
    fDigest: TMD5Digest;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  end;

procedure MD5Init(var Context: TMD5Context);
procedure MD5Update(var Context: TMD5Context; Input: PByte; Length: longword);
procedure MD5Final(var Context: TMD5Context; var Digest: TMD5Digest);

implementation

var
  PADDING: TMD5Buffer = (
    $80, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00
  );


{$IFDEF SUPPORTS_REGION}{$REGION 'TMD5'}{$ENDIF}

function TMD5.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TMD5.HashUpdate(const buffer: Pointer; count: Integer);
begin
  MD5Update(fContext, buffer, count);
end;

function TMD5.HashFinal: TBuffer;
begin
  MD5Final(fContext, fDigest);
  Result := TBuffer.Create(fDigest);
end;

procedure TMD5.HashInit;
var
  i: Byte;
begin
  for i := Low(fDigest) to High(fDigest) do
  begin
    fDigest[i] := i + 1;
  end;
  MD5Init(fContext);
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}

procedure CopyMemory(Destination: Pointer; Source: Pointer; Length: LongWord);
begin
  Move(Source^, Destination^, Length);
end;

procedure ZeroMemory(Destination: Pointer; Length: LongWord);
begin
  FillChar(Destination^, Length, 0);
end;

function F(x, y, z: LongWord): LongWord;
{$IFDEF CPUX86}
asm
  // Result := (x and y) or ((not x) and z);
  AND EDX, EAX
  NOT EAX
  AND EAX, ECX
  OR EAX, EDX
end;
{$ELSE}
begin
  Result := (x and y) or ((not x) and z);
end;
{$ENDIF}

function G(x, y, z: LongWord): LongWord;
{$IFDEF CPUX86}
asm
  //Result := (x and z) or (y and (not z));
  AND EAX, ECX
  NOT ECX
  AND EDX, ECX
  OR EAX, EDX
end;
{$ELSE}
begin
  Result := (x and z) or (y and (not z));
end;
{$ENDIF}

function H(x, y, z: LongWord): LongWord;
{$IFDEF CPUX86}
asm
  //Result := x xor y xor z;
  XOR EAX, EDX
  XOR EAX, ECX
end;
{$ELSE}
begin
  Result := x xor y xor z;
end;
{$ENDIF}

function I(x, y, z: LongWord): LongWord;
{$IFDEF CPUX86}
asm
  //Result := y xor (x or (not z));
  NOT ECX
  OR EAX, ECX
  XOR EAX, EDX
end;
{$ELSE}
begin
  Result := y xor (x or (not z));
end;
{$ENDIF}

procedure rot(var x: LongWord; n: BYTE);
{$IFDEF CPUX86}
asm
  //x := (x shl n) or (x shr (32 - n));
  PUSH EBX
  MOV CL, $20
  SUB CL, DL
  MOV EBX, [EAX]
  SHR EBX, CL
  MOV ECX, EDX
  MOV EDX, [EAX]
  SHL EDX, CL
  OR EBX, EDX
  MOV [EAX], EBX
  POP EBX
end;
{$ELSE}
begin
  x := (x shl n) or (x shr (32 - n));
end;
{$ENDIF}

procedure FF(var a: LongWord; b, c, d, x: LongWord; s: BYTE; ac: LongWord);
begin
  inc(a, F(b, c, d) + x + ac);
  rot(a, s);
  inc(a, b);
end;

procedure GG(var a: LongWord; b, c, d, x: LongWord; s: BYTE; ac: LongWord);
begin
  inc(a, G(b, c, d) + x + ac);
  rot(a, s);
  inc(a, b);
end;

procedure HH(var a: LongWord; b, c, d, x: LongWord; s: BYTE; ac: LongWord);
begin
  inc(a, H(b, c, d) + x + ac);
  rot(a, s);
  inc(a, b);
end;

procedure II(var a: LongWord; b, c, d, x: LongWord; s: BYTE; ac: LongWord);
begin
  inc(a, I(b, c, d) + x + ac);
  rot(a, s);
  inc(a, b);
end;

// Encode Count bytes at Source into (Count / 4) DWORDs at Target
procedure Encode(Source, Target: pointer; Count: longword);
var
  S: PByte;
  T: PLongWord;
  I: longword;
begin
  S := Source;
  T := Target;
  for I := 1 to Count div 4 do
  begin
    T^ := S^;
    inc(S);
    T^ := T^ or (S^ shl 8);
    inc(S);
    T^ := T^ or (S^ shl 16);
    inc(S);
    T^ := T^ or (S^ shl 24);
    inc(S);
    inc(T);
  end;
end;

// Decode Count DWORDs at Source into (Count * 4) Bytes at Target
procedure Decode(Source, Target: pointer; Count: longword);
var
  S: PLongWord;
  T: PByte;
  I: longword;
begin
  S := Source;
  T := Target;
  for I := 1 to Count do
  begin
    T^ := S^ and $ff;
    inc(T);
    T^ := (S^ shr 8) and $ff;
    inc(T);
    T^ := (S^ shr 16) and $ff;
    inc(T);
    T^ := (S^ shr 24) and $ff;
    inc(T);
    inc(S);
  end;
end;

// Transform State according to first 64 bytes at Buffer
procedure Transform(Buffer: pointer; var State: TMD5State);
var
  a, b, c, d: LongWord;
  Block: TMD5Block;
begin
  Encode(Buffer, @Block, 64);
  a := State[0];
  b := State[1];
  c := State[2];
  d := State[3];
  FF (a, b, c, d, Block[ 0],  7, $d76aa478);
  FF (d, a, b, c, Block[ 1], 12, $e8c7b756);
  FF (c, d, a, b, Block[ 2], 17, $242070db);
  FF (b, c, d, a, Block[ 3], 22, $c1bdceee);
  FF (a, b, c, d, Block[ 4],  7, $f57c0faf);
  FF (d, a, b, c, Block[ 5], 12, $4787c62a);
  FF (c, d, a, b, Block[ 6], 17, $a8304613);
  FF (b, c, d, a, Block[ 7], 22, $fd469501);
  FF (a, b, c, d, Block[ 8],  7, $698098d8);
  FF (d, a, b, c, Block[ 9], 12, $8b44f7af);
  FF (c, d, a, b, Block[10], 17, $ffff5bb1);
  FF (b, c, d, a, Block[11], 22, $895cd7be);
  FF (a, b, c, d, Block[12],  7, $6b901122);
  FF (d, a, b, c, Block[13], 12, $fd987193);
  FF (c, d, a, b, Block[14], 17, $a679438e);
  FF (b, c, d, a, Block[15], 22, $49b40821);
  GG (a, b, c, d, Block[ 1],  5, $f61e2562);
  GG (d, a, b, c, Block[ 6],  9, $c040b340);
  GG (c, d, a, b, Block[11], 14, $265e5a51);
  GG (b, c, d, a, Block[ 0], 20, $e9b6c7aa);
  GG (a, b, c, d, Block[ 5],  5, $d62f105d);
  GG (d, a, b, c, Block[10],  9,  $2441453);
  GG (c, d, a, b, Block[15], 14, $d8a1e681);
  GG (b, c, d, a, Block[ 4], 20, $e7d3fbc8);
  GG (a, b, c, d, Block[ 9],  5, $21e1cde6);
  GG (d, a, b, c, Block[14],  9, $c33707d6);
  GG (c, d, a, b, Block[ 3], 14, $f4d50d87);
  GG (b, c, d, a, Block[ 8], 20, $455a14ed);
  GG (a, b, c, d, Block[13],  5, $a9e3e905);
  GG (d, a, b, c, Block[ 2],  9, $fcefa3f8);
  GG (c, d, a, b, Block[ 7], 14, $676f02d9);
  GG (b, c, d, a, Block[12], 20, $8d2a4c8a);
  HH (a, b, c, d, Block[ 5],  4, $fffa3942);
  HH (d, a, b, c, Block[ 8], 11, $8771f681);
  HH (c, d, a, b, Block[11], 16, $6d9d6122);
  HH (b, c, d, a, Block[14], 23, $fde5380c);
  HH (a, b, c, d, Block[ 1],  4, $a4beea44);
  HH (d, a, b, c, Block[ 4], 11, $4bdecfa9);
  HH (c, d, a, b, Block[ 7], 16, $f6bb4b60);
  HH (b, c, d, a, Block[10], 23, $bebfbc70);
  HH (a, b, c, d, Block[13],  4, $289b7ec6);
  HH (d, a, b, c, Block[ 0], 11, $eaa127fa);
  HH (c, d, a, b, Block[ 3], 16, $d4ef3085);
  HH (b, c, d, a, Block[ 6], 23,  $4881d05);
  HH (a, b, c, d, Block[ 9],  4, $d9d4d039);
  HH (d, a, b, c, Block[12], 11, $e6db99e5);
  HH (c, d, a, b, Block[15], 16, $1fa27cf8);
  HH (b, c, d, a, Block[ 2], 23, $c4ac5665);
  II (a, b, c, d, Block[ 0],  6, $f4292244);
  II (d, a, b, c, Block[ 7], 10, $432aff97);
  II (c, d, a, b, Block[14], 15, $ab9423a7);
  II (b, c, d, a, Block[ 5], 21, $fc93a039);
  II (a, b, c, d, Block[12],  6, $655b59c3);
  II (d, a, b, c, Block[ 3], 10, $8f0ccc92);
  II (c, d, a, b, Block[10], 15, $ffeff47d);
  II (b, c, d, a, Block[ 1], 21, $85845dd1);
  II (a, b, c, d, Block[ 8],  6, $6fa87e4f);
  II (d, a, b, c, Block[15], 10, $fe2ce6e0);
  II (c, d, a, b, Block[ 6], 15, $a3014314);
  II (b, c, d, a, Block[13], 21, $4e0811a1);
  II (a, b, c, d, Block[ 4],  6, $f7537e82);
  II (d, a, b, c, Block[11], 10, $bd3af235);
  II (c, d, a, b, Block[ 2], 15, $2ad7d2bb);
  II (b, c, d, a, Block[ 9], 21, $eb86d391);
  inc(State[0], a);
  inc(State[1], b);
  inc(State[2], c);
  inc(State[3], d);
end;

// Initialize given Context
procedure MD5Init(var Context: TMD5Context);
begin
  with Context do
  begin
    State[0] := $67452301;
    State[1] := $efcdab89;
    State[2] := $98badcfe;
    State[3] := $10325476;
    Count[0] := 0;
    Count[1] := 0;
    ZeroMemory(@Buffer, SizeOf(TMD5Buffer));
  end;
end;

// Update given Context to include Length bytes of Input
procedure MD5Update(var Context: TMD5Context; Input: PByte; Length: longword);
var
  Index: longword;
  PartLen: longword;
  I: longword;
begin
  with Context do
  begin
    Index := (Count[0] shr 3) and $3f;
    inc(Count[0], Length shl 3);
    if Count[0] < (Length shl 3) then inc(Count[1]);
    inc(Count[1], Length shr 29);
  end;

  PartLen := 64 - Index;
  if Length >= PartLen then
  begin
    CopyMemory(@Context.Buffer[Index], Input, PartLen);
    Transform(@Context.Buffer, Context.State);
    I := PartLen;
    while I + 63 < Length do
    begin
      Transform(@Input[I], Context.State);
      inc(I, 64);
    end;
    Index := 0;
  end
  else I := 0;
  CopyMemory(@Context.Buffer[Index], @Input[I], Length - I);
end;

// Finalize given Context, create Digest and zeroize Context
procedure MD5Final(var Context: TMD5Context; var Digest: TMD5Digest);
var
  Bits: TMD5CBits;
  Index: longword;
  PadLen: longword;
begin
  Decode(@Context.Count, @Bits, 2);
  Index := (Context.Count[0] shr 3) and $3f;
  if Index < 56 then
    PadLen := 56 - Index
  else
    PadLen := 120 - Index;
  MD5Update(Context, @PADDING, PadLen);
  MD5Update(Context, @Bits, 8);
  Decode(@Context.State, @Digest, 4);
  ZeroMemory(@Context, SizeOf(TMD5Context));
end;

end.

