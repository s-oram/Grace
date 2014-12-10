unit SiEncryption;

{ Copyright (c) 2004, Henrick Wibell Hellström, StreamSec
  All rights reserved.

  Redistributed by Gurock Software for use as part of the SmartInspect
  product with permission from the copyright owner.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form to the extent necessary to obtain
      certain functionality from the SmartInspect product by Gurock Software,
      is allowed under the explicit license terms imposed by Gurock Software
      for this product.
    * Neither the name of StreamSec nor the names of its contributors may be
      used to endorse or promote products derived from this software without
      specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE. }

interface

uses
  Classes, SysUtils, Windows;

const
  MAXROUNDS = 14;

type
  TCipherAlg = (caUnknown,caRijndael);
  TCipherMode = (cmECB,cmCFB,cmCBC,cmOFB,cmABC,cmCTR);

  OctetString = AnsiString;

  TCipher = class
  private
    FMode: TCipherMode;
  protected
    procedure CleanUp; virtual; abstract;
    function GetVector: OctetString; virtual; abstract;
    procedure SetVector(const Value: OctetString); virtual; abstract;
    procedure VirtualLock; virtual;
    procedure VirtualUnlock; virtual;
  public
    constructor Create(const AKey; Count: Integer); virtual;
    destructor Destroy; override;
    class function BlockSize: Integer; virtual;
    class function Algorithm: TCipherAlg; virtual;
    class function AlgorithmName: PAnsiChar; virtual;
    procedure Decrypt(var Buf; Count: Integer); virtual;
    procedure DecryptPtr(Buf: Pointer; Count: Integer);
    procedure DecryptToBuf(const Src; var Dst; Count: Integer); virtual;
    procedure DecryptToPtr(Src, Dst: Pointer; Count: Integer);
    procedure Encrypt(var Buf; Count: Integer); virtual;
    procedure EncryptPtr(Buf: Pointer; Count: Integer);
    procedure EncryptToBuf(const Src; var Dst; Count: Integer); virtual;
    procedure EncryptToPtr(Src, Dst: Pointer; Count: Integer);
    procedure GetVectorBuf(var IV; Count: Integer); virtual;
    procedure SetUp(const AKey; Count: Integer); virtual; abstract;
    procedure SetVectorBuf(const IV; Count: Integer); virtual;
    property IVector: OctetString read GetVector write SetVector;
    property Mode: TCipherMode read FMode write FMode;
  end;

  TBlockCipher = class(TCipher)
  private
    FModeRatio: Integer;
  protected
    FIV: PByteArray;
    FFBIndex: Integer;
    function AllocBuffer: PByteArray;
    procedure CleanUp; override;
    procedure DeallocBuffer(var T: PByteArray);
    procedure DecryptABC(var Buf; Count: Integer);
    procedure DecryptCBC(var Buf; Count: Integer);
    procedure DecryptCFB(var Buf; Count: Integer);
    procedure DecryptCTR(var Buf; Count: Integer);
    procedure DecryptOFB(var Buf; Count: Integer);
    procedure DecryptBlock(var Buf); virtual;
    procedure DecryptBlockToDst(var Dst; const Src); virtual;
    procedure EncryptABC(var Buf; Count: Integer);
    procedure EncryptCBC(var Buf; Count: Integer);
    procedure EncryptCFB(var Buf; Count: Integer);
    procedure EncryptCTR(var Buf; Count: Integer);
    procedure EncryptOFB(var Buf; Count: Integer);
    procedure EncryptBlock(var Buf); virtual;
    procedure EncryptBlockToDst(var Dst; const Src); virtual;
    function GetVector: OctetString; override;
    function GetBlockVectorSize: Integer;
    procedure SetModeRatio(Value: Integer); virtual;
    procedure SetVector(const Value: OctetString); override;
    property ModeRatio: Integer read FModeRatio write SetModeRatio;
    property FBIndex: Integer read FFBIndex;
  public
    procedure Decrypt(var Buf; Count: Integer); override;
    procedure Encrypt(var Buf; Count: Integer); override;
    procedure GetVectorBuf(var IV; Count: Integer); override;
    class function MaxKeySize: Integer; virtual;
    class function MinKeySize: Integer; virtual;
    procedure SetVectorBuf(const IV; Count: Integer); override;
    property BlockVectorSize: Integer read GetBlockVectorSize;
  end;

  TRijndael = class(TBlockCipher)
  protected
    IV: array[0..47] of byte;
    numrounds: longint;
    rk, drk: array[0..MAXROUNDS,0..7] of LongWord;
    procedure CleanUp; override;
    procedure DecryptBlock(var Buf); override;
    procedure DecryptBlockToDst(var Dst; const Src); override;
    procedure EncryptBlock(var Buf); override;
    procedure EncryptBlockToDst(var Dst; const Src); override;
  public
    constructor Create(const AKey; Count: Integer); override;
    class function BlockSize: Integer; override;
    class function Algorithm: TCipherAlg; override;
    class function AlgorithmName: PAnsiChar; override;
    class function MaxKeySize: Integer; override;
    class function MinKeySize: Integer; override;
    procedure SetUp(const AKey; Count: Integer); override;
  end;

  TEncryptStream = class(TStream)
  private
    FBlock: Pointer;
    FBlockCount: Integer;
    FBlockSize: Integer;
    FCipher: TCipher;
    FDataStream: TStream;
    procedure SetCipher(const Value: TCipher);
    procedure SetDataStream(const Value: TStream);
  public
    constructor Create(ADataStream: TStream; ACipher: TCipher);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Done: Integer;
    property Cipher: TCipher read FCipher write SetCipher;
    property DataStream: TStream read FDataStream write SetDataStream;
  end;

  TDecryptStream = class(TStream)
  private
    FBlock: PByteArray;
    FBlockCount: Integer;
    FBlockSize: Integer;
    FCipher: TCipher;
    FDataStream: TStream;
    FOK: Boolean;
    FDecBuffer: array [0..255] of Byte;
    FDecLen: Integer;
    procedure SetCipher(const Value: TCipher);
    procedure SetDataStream(const Value: TStream);
  public
    constructor Create(ADataStream: TStream; ACipher: TCipher);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Done: Integer; virtual;
    property Cipher: TCipher read FCipher write SetCipher;
    property DataStream: TStream read FDataStream write SetDataStream;
    property OK: Boolean read FOK;
  end;

  THashAlgorithm = (haMD5, haNull, haDefault);

  THash = class
  protected
    function GetDigest: OctetString; virtual; abstract;
    procedure Calc; virtual; abstract;
    procedure CleanUp; virtual; abstract;
    procedure Init; virtual; abstract;
  public
    constructor Create(const Data; Count: Int64); virtual;
    destructor Destroy; override;
    class function Algorithm: THashAlgorithm; virtual;
    class function DERPrefix: Pointer; virtual;
    class function DERPrefixLen: Integer; virtual;
    class function DigestSize: Integer; virtual;
    procedure Done(Digest: Pointer); virtual; abstract;
    procedure GetContext(Context: Pointer); virtual; abstract;
    procedure HashData(const Data; Count: Int64); virtual; abstract;
    procedure SetUp; virtual;
    procedure SetUpContext(Context: Pointer); virtual; abstract;
    property Digest: OctetString read GetDigest;
  end;

  TBaseHash = class(THash)
  protected
    FCount: Int64;
    FDataBuffer: packed array [0..127] of Byte;
    FDigest: packed array [0..63] of Byte;
    procedure CleanUp; override;
    function GetDigest: OctetString; override;
  public
    procedure GetContext(Context: Pointer); override;
    procedure HashData(const Data; Count: Int64); override;
    procedure SetUpContext(Context: Pointer); override;
  end;

  TMD5 = class(TBaseHash)
  protected
    procedure Calc; override;
    procedure Init; override;
  public
    procedure Done(Digest: Pointer); override;
    class function Algorithm: THashAlgorithm; override;
    class function DERPrefix: Pointer; override;
    class function DERPrefixLen: Integer; override;
    class function DigestSize: Integer; override;
  end;

implementation

{$I SiRijndael.Inc}

{$DEFINE NO_GUI}
{$DEFINE CBC}
{$DEFINE RIJNDAEL}
{$Q-,R-}

{$IFDEF VER120}
  {$DEFINE D4}
  {$DEFINE D4UP}
  {$DEFINE MSWINDOWS}
{$ENDIF}
{$IFDEF VER130}
  {$DEFINE D5}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE MSWINDOWS}
{$ENDIF}
{$IFDEF VER140}
  {$DEFINE D6}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF VER150}
  {$DEFINE D7}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{$IFNDEF D4UP}
  {$DEFINE D9}
  {$DEFINE D4UP}
  {$DEFINE D5UP}
  {$DEFINE D6UP}
  {$DEFINE D7UP}
{$ENDIF}

{$IFDEF D4UP}
  {$DEFINE INT64}
  {$DEFINE LONGWORD}
{$ENDIF}

const
  BC = 4;
  MAXKC = 8;

const
  DERPrefixMD5: array [0..17] of Byte =
    ($30,$20,$30,$0c,$06,$08,$2a,$86,$48,$86,$f7,$0d,$02,$05,$05,$00,$04,$10);

procedure XORBytes(var Dest; const Source; Count: Integer);
asm
{$IFDEF WIN64}
  mov r9,r8
  shr r9,3
  jz @@2

  lea rdx,[rdx + r9*8]
  lea rcx,[rcx + r9*8]
  neg r9
@@1:
  mov rax,[rdx + r9*8]
  xor [rcx + r9*8],rax
  inc r9
  jnz @@1
@@2:
  and r8,7
  jz @@4
@@3:
  mov al,byte [rdx]
  xor byte [rcx],al
  inc rcx
  inc rdx
  dec r8
  jnz @@3
@@4:
  {$ELSE}
  push EBX
  push EDI

  mov EDI,ECX
  shr EDI,2
  jz @@2

  lea EDX,[EDX + EDI*4]
  lea EAX,[EAX + EDI*4]
  neg EDI
@@1:
  mov EBX,dword [EDX + EDI*4]
  xor dword [EAX + EDI*4],EBX
  inc EDI
  jnz @@1
@@2:
  and ECX,3
  jz @@4
@@3:
  mov BL,byte [EDX]
  xor byte [EAX],BL
  inc EAX
  inc EDX
  dec ECX
  jnz @@3
@@4:
  pop EDI
  pop EBX
  {$ENDIF}
end;

procedure XORBytesBackup(var Dest; const Source; var Backup; Count: Integer);
asm
  {$IFDEF WIN64}
  mov r10,r9
  shr r10,3
  jz @@2

  lea r8,[r8 + r10*8]
  lea rdx,[rdx + r10*8]
  lea rcx,[rcx + r10*8]
  neg r10
@@1:
  mov rax,[rcx + r10*8]
  mov r11,[rdx + r10*8]
  mov [r8 + r10*8],rax
  xor [rcx + r10*8],r11
  inc r10
  jnz @@1
@@2:
  and r9,7
  jz @@4
@@3:
  mov al,byte [rcx + r9 - 1]
  mov r11b,byte [rdx + r9 - 1]
  mov byte [r8 + r9 - 1],al
  xor byte [rcx + r9 - 1],r11b
  dec r9
  jnz @@3
@@4:
  {$ELSE}
  push EBX
  push EDI
  push ESI
  push EBP

  mov EDI,ECX

  mov ECX,Count
  mov ESI,ECX
  shr ESI,2
  jz @@2

  lea EDI,[EDI + ESI*4]
  lea EDX,[EDX + ESI*4]
  lea EAX,[EAX + ESI*4]
  neg ESI
@@1:
  mov EBX,dword [EAX + ESI*4]
  mov EBP,dword [EDX + ESI*4]
  mov dword [EDI + ESI*4],EBX
  xor dword [EAX + ESI*4],EBP
  inc ESI
  jnz @@1
@@2:
  and ECX,3
  jz @@4
@@3:
  mov BL,byte [EAX + ECX - 1]
  mov BH,byte [EDX + ECX - 1]
  mov byte [EDI + ECX - 1],BL
  xor byte [EAX + ECX - 1],BH
  dec ECX
  jnz @@3
@@4:
  pop EBP
  pop ESI
  pop EDI
  pop EBX
  {$ENDIF}
end;

procedure XORBytesABCDcr(var MsgText; var H; const PrevH; var PrevC;
  Count: Integer);
asm
  {$IFDEF WIN64}
  push rbx

  mov r10d,Count
  mov r11,r10
  shr r11,3
  jz @@2

  lea r8,[r8 + r11*8]
  lea r9,[r9 + r11*8]
  lea rdx,[rdx + r11*8]
  lea rcx,[rcx + r11*8]
  neg r11
@@1:
  mov rax,[rcx + r11*8]
  mov rbx,[r9 + r11*8]
  xor rbx,[rdx + r11*8]
  mov [rdx + r11*8],rbx
  xor rbx,[r8 + r11*8]
  mov [r9 + r11*8],rax
  mov [rcx + r11*8],rbx
  inc r11
  jnz @@1
@@2:
  and r10,7
  jz @@4
@@3:
  xor r11,r11
  mov al,byte [rcx + r10 - 1]
  mov r11b,byte [rdx + r10 - 1]
  xor r11b,byte [r9 + r10 - 1]
  mov byte [rdx + r10 - 1],r11b
  xor r11b,byte [r8 + r10 - 1]
  mov byte [r9 + r10 - 1],al
  mov byte [rcx + r10 - 1],r11b
  dec r10
  jnz @@3
@@4:
  pop rbx
  {$ELSE}
  push EBX
  push EDI
  push ESI
  push EBP

  mov EDI,PrevC

  mov ESI,Count
  push ESI
  shr ESI,2
  jz @@2

  lea ECX,[ECX + ESI*4]
  lea EDI,[EDI + ESI*4]
  lea EDX,[EDX + ESI*4]
  lea EAX,[EAX + ESI*4]
  neg ESI
@@1:
  mov EBP,dword [EAX + ESI*4]
  mov EBX,dword [EDI + ESI*4]
  xor EBX,dword [EDX + ESI*4]
  mov dword [EDX + ESI*4],EBX
  xor EBX,dword [ECX + ESI*4]
  mov dword [EDI + ESI*4],EBP
  mov dword [EAX + ESI*4],EBX
  inc ESI
  jnz @@1
@@2:
  pop ESI
  and ESI,3
  jz @@4
@@3:
  mov BL,byte [EAX + ESI - 1]
  mov BH,byte [EDX + ESI - 1]
  xor BH,byte [EDI + ESI - 1]
  mov byte [EDX + ESI - 1],BH
  xor BH,byte [ECX + ESI - 1]
  mov byte [EDI + ESI - 1],BL
  mov byte [EAX + ESI - 1],BH
  dec ESI
  jnz @@3
@@4:
  pop EBP
  pop ESI
  pop EDI
  pop EBX
  {$ENDIF}
end;

procedure XORBlock(var Dest; const Source; LWCount: Integer);
asm
  {$IFDEF WIN64}
  shr r8,1
  lea rcx,[rcx + r8*8]
  lea rdx,[rdx + r8*8]
  neg r8
@@Main:
  mov rax,[rdx + r8*8]
  xor [rcx + r8*8],rax
  inc r8
  jnz @@Main
  {$ELSE}
  push EBX
  push EDI
  shr ECX,1
  lea EDI,[EAX + ECX*8]
  lea EDX,[EDX + ECX*8]
  neg ECX
@@Main:
  mov EBX,dword [EDX + ECX*8]
  mov EAX,dword [EDX + ECX*8 + 4]
  xor dword [EDI + ECX*8],EBX
  xor dword [EDI + ECX*8 + 4],EAX
  inc ECX
  jnz @@Main
  pop EDI
  pop EBX
  {$ENDIF}
end;

procedure XORBytesCopy(var Dest; const Source; var Copy; Count: Integer);
asm
  {$IFDEF WIN64}
  mov r10,r9
  shr r9,3
  jz @@2

  lea r8,[r8 + r9*8]
  lea rdx,[rdx + r9*8]
  lea rcx,[rcx + r9*8]
  neg r9
@@1:
  mov rax,[rdx + r9*8]
  xor rax,[rcx + r9*8]
  mov [r8 + r9*8],rax
  mov [rcx + r9*8],rax
  inc r9
  jnz @@1
@@2:
  and r10,7
  jz @@4
@@3:
  mov al,byte [rdx + r10 - 1]
  xor al,byte [rcx + r10 - 1]
  mov byte [r8 + r10 - 1],al
  mov byte [rcx + r10 - 1],al
  dec r10
  jnz @@3
@@4:
  {$ELSE}
  push EBX
  push EDI
  push ESI

  mov EDI,ECX

  mov ECX,Count
  mov ESI,ECX
  shr ESI,2
  jz @@2

  lea EDI,[EDI + ESI*4]
  lea EDX,[EDX + ESI*4]
  lea EAX,[EAX + ESI*4]
  neg ESI
@@1:
  mov EBX,dword [EDX + ESI*4]
  xor EBX,dword [EAX + ESI*4]
  mov dword [EDI + ESI*4],EBX
  mov dword [EAX + ESI*4],EBX
  inc ESI
  jnz @@1
@@2:
  and ECX,3
  jz @@4
@@3:
  mov BL,byte [EDX + ECX - 1]
  xor BL,byte [EAX + ECX - 1]
  mov byte [EDI + ECX - 1],BL
  mov byte [EAX + ECX - 1],BL
  dec ECX
  jnz @@3
@@4:
  pop ESI
  pop EDI
  pop EBX
  {$ENDIF}
end;

procedure IncBlock(var Buf; LWCount: Integer);
asm
  {$IFDEF WIN64}
  lea rcx,[rcx + rdx*4 - 8]
  mov rax,1
  shr rdx,1
  neg rdx
@@Main:
  mov r8,[rcx]
  bswap r8
  add r8,rax
  mov al,ah
  setc al
  bswap r8
  mov [rcx],r8
  sub rcx,8
  inc rdx
  jnz @@Main
  {$ELSE}
  push EBX
  lea EAX,[EAX + EDX*4 - 4]
  mov ECX,1
  neg EDX
@@Main:
  mov EBX,dword [EAX]
  bswap EBX
  add EBX,ECX
  mov CL,CH
  setc CL
  bswap EBX
  mov dword [EAX],EBX
  sub EAX,4
  inc EDX
  jnz @@Main
  pop EBX
  {$ENDIF}
end;

procedure ProtectClear(var ABuf; Count: Integer);
begin
  if Count <= 0 then Exit;
  FillChar(ABuf,Count,$FF);
  FillChar(ABuf,Count,$AA);
  FillChar(ABuf,Count,$55);
  FillChar(ABuf,Count,$00);
end;

{ THash }

{$WARNINGS OFF}
class function THash.Algorithm: THashAlgorithm;
begin
  // Undefined
end;
{$WARNINGS ON}

constructor THash.Create(const Data; Count: Int64);
begin
  SetUp;
  HashData(Data,Count);
end;

class function THash.DERPrefix: Pointer;
begin
  Result := nil;
end;

class function THash.DERPrefixLen: Integer;
begin
  Result := 0;
end;

destructor THash.Destroy;
begin
  CleanUp;
  inherited;
end;

class function THash.DigestSize: Integer;
begin
  Result := -1;
end;

procedure THash.SetUp;
begin
  CleanUp;
  Init;
end;

{ TBaseHash }

procedure TBaseHash.CleanUp;
begin
  FCount := 0;
  ProtectClear(FDigest,SizeOf(FDigest));
  ProtectClear(FDataBuffer,SizeOf(FDataBuffer));
end;

procedure TBaseHash.GetContext(Context: Pointer);
begin
  Move(FDigest,Context^,DigestSize);
end;

function TBaseHash.GetDigest: OctetString;
begin
  SetLength(Result,DigestSize);
  Move(FDigest,Result[1],DigestSize);
end;

procedure TBaseHash.HashData(const Data; Count: Int64);
type
  PByte = ^Byte;
var
  I: Integer;
  P: PByte;
begin
  P := @Data;
  I := FCount and $3F;
  if I > 0 then begin
    if Count >= $40 - I then begin
      Move(P^,FDataBuffer[I],$40 - I);
      Calc;
      Inc(P,$40 - I);
      Inc(FCount,$40 - I);
      Dec(Count,$40 - I);
    end else begin
      Move(P^,FDataBuffer[I],Count);
      Inc(FCount,Count);
      Count := 0;
    end;
  end;
  while Count >= $40 do begin
    Move(P^,FDataBuffer,$40);
    Calc;
    Inc(P,$40);
    Inc(FCount,$40);
    Dec(Count,$40);
  end;
  if Count > 0 then begin
    Move(P^,FDataBuffer,Count);
    Inc(FCount,Count);
  end;
end;

procedure TBaseHash.SetUpContext(Context: Pointer);
begin
  Move(Context^,FDigest,DigestSize);
end;

{ TMD5 }

procedure TMD5.Done(Digest: Pointer);
var
  I: Integer;
  S: Int64;
begin
  I := (FCount and $3F);
  FDataBuffer[I] := $80;
  Inc(I);
  if I > $38 then begin
    FillChar(FDataBuffer[I],$40 - I,0);
    Calc;
    I := 0;
  end;
  FillChar(FDataBuffer[I],$40 - I,0);
  S := FCount * 8;
  Move(S,FDataBuffer[$38],8);
  Calc;
  if Assigned(Digest) then
    Move(FDigest,Digest^,DigestSize);
  FCount := 0;
end;

class function TMD5.Algorithm: THashAlgorithm;
begin
  Result := haMD5;
end;

procedure TMD5.Calc;
asm
{$IFDEF WIN64}
  push rbx
  push rsi
  push rdi
  push rbp

  lea rbp,dword ptr [rcx].TBaseHash.FDataBuffer

// Copy FDigest to state:
  lea rsi,dword ptr [rcx].TBaseHash.FDigest
  push rsi
  mov EAX,[rsi]
  mov EBX,[rsi + 4]
  mov ECX,[rsi + 8]
  mov EDX,[rsi + 12]

// Compress:
{ The F function is:

   (B and C) or ((not B) and D)
Truth table:
    1  1  1 _1_    0  1   0  1
    0  0  1 _1_    1  0   1  1
    1  0  0 _0_    0  1   0  1
    0  0  0 _1_    1  1   1  1
    1  1  1 _1_    0  1   0  0
    0  0  1 _0_    1  0   0  0
    1  0  0 _0_    0  1   0  0
    0  0  0 _0_    1  0   0  0

or equivalently:

   (((D xor C) and B) xor D)
Truth table:
      1  0  1   0  1  _1_ 1
      1  0  1   0  0  _1_ 1
      1  1  0   1  1  _0_ 1
      1  1  0   0  0  _1_ 1
      0  1  1   1  1  _1_ 0
      0  1  1   0  0  _0_ 0
      0  0  0   0  1  _0_ 0
      0  0  0   0  0  _0_ 0
}

//  Inc(A, Buffer[ 0] + $D76AA478 + (D xor (B and (C xor D)))); A := A rol  7 + B;
  mov ESI,[rbp]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea rAX,[rAX + rdi + $D76AA478]
  ror EAX,25
  add EAX,EBX
//  Inc(D, Buffer[ 1] + $E8C7B756 + (C xor (A and (B xor C)))); D := D rol 12 + A;
  mov ESI,[rbp + 4]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea rDX,[rDX + rdi + $E8C7B756]
  ror EDX,20
  add EDX,EAX
//  Inc(C, Buffer[ 2] + $242070DB + (B xor (D and (A xor B)))); C := C rol 17 + D;
  mov ESI,[rbp + 8]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea rCX,[rCX + rdi + $242070DB]
  ror ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 3] + $C1BDCEEE + (A xor (C and (D xor A)))); B := B rol 22 + C;
  mov ESI,[rbp + 12]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea rBX,[rBX + rdi + $C1BDCEEE]
  ror EBX,10
  add EBX,ECX
//  Inc(A, Buffer[ 4] + $F57C0FAF + (D xor (B and (C xor D)))); A := A rol  7 + B;
  mov ESI,[rbp + 16]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea rAX,[rAX + rdi + $F57C0FAF]
  ror EAX,25
  add EAX,EBX
//  Inc(D, Buffer[ 5] + $4787C62A + (C xor (A and (B xor C)))); D := D rol 12 + A;
  mov ESI,[rbp + 20]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea rDX,[rDX + rdi + $4787C62A]
  ror EDX,20
  add EDX,EAX
//  Inc(C, Buffer[ 6] + $A8304613 + (B xor (D and (A xor B)))); C := C rol 17 + D;
  mov ESI,[rbp + 24]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea rCX,[rCX + rdi + $A8304613]
  ror ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 7] + $FD469501 + (A xor (C and (D xor A)))); B := B rol 22 + C;
  mov ESI,[rbp + 28]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea rBX,[rBX + rdi + $FD469501]
  ror EBX,10
  add EBX,ECX
//  Inc(A, Buffer[ 8] + $698098D8 + (D xor (B and (C xor D)))); A := A rol  7 + B;
  mov ESI,[rbp + 32]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea rAX,[rDI + rAX + $698098D8]
  ror EAX,25
  add EAX,EBX
//  Inc(D, Buffer[ 9] + $8B44F7AF + (C xor (A and (B xor C)))); D := D rol 12 + A;
  mov ESI,[rbp + 36]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea rDX,[rDI + rDX + $8B44F7AF]
  ror EDX,20
  add EDX,EAX
//  Inc(C, Buffer[10] + $FFFF5BB1 + (B xor (D and (A xor B)))); C := C rol 17 + D;
  mov ESI,[rbp + 40]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea rCX,[rDI + rCX + $FFFF5BB1]
  ror ECX,15
  add ECX,EDX
//  Inc(B, Buffer[11] + $895CD7BE + (A xor (C and (D xor A)))); B := B rol 22 + C;
  mov ESI,[rbp + 44]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea rBX,[rDI + rBX + $895CD7BE]
  ror EBX,10
  add EBX,ECX
//  Inc(A, Buffer[12] + $6B901122 + (D xor (B and (C xor D)))); A := A rol  7 + B;
  mov ESI,[rbp + 48]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea rAX,[rDI + rAX + $6B901122]
  ror EAX,25
  add EAX,EBX
//  Inc(D, Buffer[13] + $FD987193 + (C xor (A and (B xor C)))); D := D rol 12 + A;
  mov ESI,[rbp + 52]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea rDX,[rDI + rDX + $FD987193]
  ror EDX,20
  add EDX,EAX
//  Inc(C, Buffer[14] + $A679438E + (B xor (D and (A xor B)))); C := C rol 17 + D;
  mov ESI,[rbp + 56]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea rCX,[rDI + rCX + $A679438E]
  ror ECX,15
  add ECX,EDX
//  Inc(B, Buffer[15] + $49B40821 + (A xor (C and (D xor A)))); B := B rol 22 + C;
  mov ESI,[rbp + 60]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea rBX,[rDI + rBX + $49B40821]
  ror EBX,10
  add EBX,ECX

{ The G function is:

   (D and B) or ((not D) and C)
Truth table:
    1  1  1 _1_    0  1   0  1
    0  0  1 _1_    1  0   1  1
    1  0  0 _0_    0  1   0  1
    0  0  0 _1_    1  1   1  1
    1  1  1 _1_    0  1   0  0
    0  0  1 _0_    1  0   0  0
    1  0  0 _0_    0  1   0  0
    0  0  0 _0_    1  0   0  0

or equivalently:

   (((C xor B) and D) xor C)
Truth table:
      1  0  1   0  1  _1_ 1
      1  0  1   0  0  _1_ 1
      1  1  0   1  1  _0_ 1
      1  1  0   0  0  _1_ 1
      0  1  1   1  1  _1_ 0
      0  1  1   0  0  _0_ 0
      0  0  0   0  1  _0_ 0
      0  0  0   0  0  _0_ 0
}

//  Inc(A, Buffer[ 1] + $F61E2562 + (C xor (D and (B xor C)))); A := A rol  5 + B;
  mov ESI,[rbp + 4]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea rAX,[rdi + rAX + $F61E2562]
  rol EAX,5
  add EAX,EBX
//  Inc(D, Buffer[ 6] + $C040B340 + (B xor (C and (A xor B)))); D := D rol  9 + A;
  mov ESI,[rbp + 24]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea rDX,[rdi + rDX + $C040B340]
  rol EDX,9
  add EDX,EAX
//  Inc(C, Buffer[11] + $265E5A51 + (A xor (B and (D xor A)))); C := C rol 14 + D;
  mov ESI,[rbp + 44]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea rCX,[rdi + rCX + $265E5A51]
  rol ECX,14
  add ECX,EDX
//  Inc(B, Buffer[ 0] + $E9B6C7AA + (D xor (A and (C xor D)))); B := B rol 20 + C;
  mov ESI,[rbp]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea rBX,[rdi + rBX + $E9B6C7AA]
  rol EBX,20
  add EBX,ECX
//  Inc(A, Buffer[ 5] + $D62F105D + (C xor (D and (B xor C)))); A := A rol  5 + B;
  mov ESI,[rbp + 20]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea rAX,[rdi + rAX + $D62F105D]
  rol EAX,5
  add EAX,EBX
//  Inc(D, Buffer[10] + $02441453 + (B xor (C and (A xor B)))); D := D rol  9 + A;
  mov ESI,[rbp + 40]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea rDX,[rdi + rDX + $02441453]
  rol EDX,9
  add EDX,EAX
//  Inc(C, Buffer[15] + $D8A1E681 + (A xor (B and (D xor A)))); C := C rol 14 + D;
  mov ESI,[rbp + 60]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea rCX,[rdi + rCX + $D8A1E681]
  rol ECX,14
  add ECX,EDX
//  Inc(B, Buffer[ 4] + $E7D3FBC8 + (D xor (A and (C xor D)))); B := B rol 20 + C;
  mov ESI,[rbp + 16]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea rBX,[rdi + rBX + $E7D3FBC8]
  rol EBX,20
  add EBX,ECX
//  Inc(A, Buffer[ 9] + $21E1CDE6 + (C xor (D and (B xor C)))); A := A rol  5 + B;
  mov ESI,[rbp + 36]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea rAX,[rdi + rAX + $21E1CDE6]
  rol EAX,5
  add EAX,EBX
//  Inc(D, Buffer[14] + $C33707D6 + (B xor (C and (A xor B)))); D := D rol  9 + A;
  mov ESI,[rbp + 56]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea rDX,[rdi + rDX + $C33707D6]
  rol EDX,9
  add EDX,EAX
//  Inc(C, Buffer[ 3] + $F4D50D87 + (A xor (B and (D xor A)))); C := C rol 14 + D;
  mov ESI,[rbp + 12]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea rCX,[rdi + rCX + $F4D50D87]
  rol ECX,14
  add ECX,EDX
//  Inc(B, Buffer[ 8] + $455A14ED + (D xor (A and (C xor D)))); B := B rol 20 + C;
  mov ESI,[rbp + 32]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea rBX,[rdi + rBX + $455A14ED]
  rol EBX,20
  add EBX,ECX
//  Inc(A, Buffer[13] + $A9E3E905 + (C xor (D and (B xor C)))); A := A rol  5 + B;
  mov ESI,[rbp + 52]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea rAX,[rdi + rAX + $A9E3E905]
  rol EAX,5
  add EAX,EBX
//  Inc(D, Buffer[ 2] + $FCEFA3F8 + (B xor (C and (A xor B)))); D := D rol  9 + A;
  mov ESI,[rbp + 8]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea rDX,[rdi + rDX + $FCEFA3F8]
  rol EDX,9
  add EDX,EAX
//  Inc(C, Buffer[ 7] + $676F02D9 + (A xor (B and (D xor A)))); C := C rol 14 + D;
  mov ESI,[rbp + 28]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea rCX,[rdi + rCX + $676F02D9]
  rol ECX,14
  add ECX,EDX
//  Inc(B, Buffer[12] + $8D2A4C8A + (D xor (A and (C xor D)))); B := B rol 20 + C;
  mov ESI,[rbp + 48]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea rBX,[rdi + rBX + $8D2A4C8A]
  rol EBX,20
  add EBX,ECX

//  Inc(A, Buffer[ 5] + $FFFA3942 + (B xor C xor D)); A := A rol  4 + B;
  mov ESI,[rbp + 20]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea rAX,[rAX + rdi + $FFFA3942]
  rol EAX,4
  add EAX,EBX
//  Inc(D, Buffer[ 8] + $8771F681 + (A xor B xor C)); D := D rol 11 + A;
  mov ESI,[rbp + 32]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea rDX,[rDX + rdi + $8771F681]
  rol EDX,11
  add EDX,EAX
//  Inc(C, Buffer[11] + $6D9D6122 + (D xor A xor B)); C := C rol 16 + D;
  mov ESI,[rbp + 44]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea rCX,[rCX + rdi + $6D9D6122]
  rol ECX,16
  add ECX,EDX
//  Inc(B, Buffer[14] + $FDE5380C + (C xor D xor A)); B := B rol 23 + C;
  mov ESI,[rbp + 56]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea rBX,[rBX + rdi + $FDE5380C]
  rol EBX,23
  add EBX,ECX
//  Inc(A, Buffer[ 1] + $A4BEEA44 + (B xor C xor D)); A := A rol  4 + B;
  mov ESI,[rbp + 4]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea rAX,[rAX + rdi + $A4BEEA44]
  rol EAX,4
  add EAX,EBX
//  Inc(D, Buffer[ 4] + $4BDECFA9 + (A xor B xor C)); D := D rol 11 + A;
  mov ESI,[rbp + 16]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea rDX,[rDX + rdi + $4BDECFA9]
  rol EDX,11
  add EDX,EAX
//  Inc(C, Buffer[ 7] + $F6BB4B60 + (D xor A xor B)); C := C rol 16 + D;
  mov ESI,[rbp + 28]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea rCX,[rCX + rdi + $F6BB4B60]
  rol ECX,16
  add ECX,EDX
//  Inc(B, Buffer[10] + $BEBFBC70 + (C xor D xor A)); B := B rol 23 + C;
  mov ESI,[rbp + 40]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea rBX,[rBX + rdi + $BEBFBC70]
  rol EBX,23
  add EBX,ECX
//  Inc(A, Buffer[13] + $289B7EC6 + (B xor C xor D)); A := A rol  4 + B;
  mov ESI,[rbp + 52]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea rAX,[rAX + rdi + $289B7EC6]
  rol EAX,4
  add EAX,EBX
//  Inc(D, Buffer[ 0] + $EAA127FA + (A xor B xor C)); D := D rol 11 + A;
  mov ESI,[rbp]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea rDX,[rDX + rdi + $EAA127FA]
  rol EDX,11
  add EDX,EAX
//  Inc(C, Buffer[ 3] + $D4EF3085 + (D xor A xor B)); C := C rol 16 + D;
  mov ESI,[rbp + 12]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea rCX,[rCX + rdi + $D4EF3085]
  rol ECX,16
  add ECX,EDX
//  Inc(B, Buffer[ 6] + $04881D05 + (C xor D xor A)); B := B rol 23 + C;
  mov ESI,[rbp + 24]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea rBX,[rBX + rdi + $04881D05]
  rol EBX,23
  add EBX,ECX
//  Inc(A, Buffer[ 9] + $D9D4D039 + (B xor C xor D)); A := A rol  4 + B;
  mov ESI,[rbp + 36]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea rAX,[rAX + rdi + $D9D4D039]
  rol EAX,4
  add EAX,EBX
//  Inc(D, Buffer[12] + $E6DB99E5 + (A xor B xor C)); D := D rol 11 + A;
  mov ESI,[rbp + 48]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea rDX,[rDX + rdi + $E6DB99E5]
  rol EDX,11
  add EDX,EAX
//  Inc(C, Buffer[15] + $1FA27CF8 + (D xor A xor B)); C := C rol 16 + D;
  mov ESI,[rbp + 60]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea rCX,[rCX + rdi + $1FA27CF8]
  rol ECX,16
  add ECX,EDX
//  Inc(B, Buffer[ 2] + $C4AC5665 + (C xor D xor A)); B := B rol 23 + C;
  mov ESI,[rbp + 8]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea rBX,[rBX + rdi + $C4AC5665]
  rol EBX,23
  add EBX,ECX

//  Inc(A, Buffer[ 0] + $F4292244 + (C xor (B or not D))); A := A rol  6 + B;
  mov ESI,[rbp]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea rAX,[rAX + rdi + $F4292244]
  rol EAX,6
  add EAX,EBX
//  Inc(D, Buffer[ 7] + $432AFF97 + (B xor (A or not C))); D := D rol 10 + A;
  mov ESI,[rbp + 28]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea rDX,[rDX + rdi + $432AFF97]
  rol EDX,10
  add EDX,EAX
//  Inc(C, Buffer[14] + $AB9423A7 + (A xor (D or not B))); C := C rol 15 + D;
  mov ESI,[rbp + 56]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea rCX,[rCX + rdi + $AB9423A7]
  rol ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 5] + $FC93A039 + (D xor (C or not A))); B := B rol 21 + C;
  mov ESI,[rbp + 20]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea rBX,[rBX + rdi + $FC93A039]
  rol EBX,21
  add EBX,ECX
//  Inc(A, Buffer[12] + $655B59C3 + (C xor (B or not D))); A := A rol  6 + B;
  mov ESI,[rbp + 48]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea rAX,[rAX + rdi + $655B59C3]
  rol EAX,6
  add EAX,EBX
//  Inc(D, Buffer[ 3] + $8F0CCC92 + (B xor (A or not C))); D := D rol 10 + A;
  mov ESI,[rbp + 12]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea rDX,[rDX + rdi + $8F0CCC92]
  rol EDX,10
  add EDX,EAX
//  Inc(C, Buffer[10] + $FFEFF47D + (A xor (D or not B))); C := C rol 15 + D;
  mov ESI,[rbp + 40]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea rCX,[rCX + rdi + $FFEFF47D]
  rol ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 1] + $85845DD1 + (D xor (C or not A))); B := B rol 21 + C;
  mov ESI,[rbp + 4]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea rBX,[rBX + rdi + $85845DD1]
  rol EBX,21
  add EBX,ECX
//  Inc(A, Buffer[ 8] + $6FA87E4F + (C xor (B or not D))); A := A rol  6 + B;
  mov ESI,[rbp + 32]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea rAX,[rAX + rdi + $6FA87E4F]
  rol EAX,6
  add EAX,EBX
//  Inc(D, Buffer[15] + $FE2CE6E0 + (B xor (A or not C))); D := D rol 10 + A;
  mov ESI,[rbp + 60]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea rDX,[rDX + rdi + $FE2CE6E0]
  rol EDX,10
  add EDX,EAX
//  Inc(C, Buffer[ 6] + $A3014314 + (A xor (D or not B))); C := C rol 15 + D;
  mov ESI,[rbp + 24]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea rCX,[rCX + rdi + $A3014314]
  rol ECX,15
  add ECX,EDX
//  Inc(B, Buffer[13] + $4E0811A1 + (D xor (C or not A))); B := B rol 21 + C;
  mov ESI,[rbp + 52]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea rBX,[rBX + rdi + $4E0811A1]
  rol EBX,21
  add EBX,ECX
//  Inc(A, Buffer[ 4] + $F7537E82 + (C xor (B or not D))); A := A rol  6 + B;
  mov ESI,[rbp + 16]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea rAX,[rAX + rdi + $F7537E82]
  rol EAX,6
  add EAX,EBX
//  Inc(D, Buffer[11] + $BD3AF235 + (B xor (A or not C))); D := D rol 10 + A;
  mov ESI,[rbp + 44]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea rDX,[rDX + rdi + $BD3AF235]
  rol EDX,10
  add EDX,EAX
//  Inc(C, Buffer[ 2] + $2AD7D2BB + (A xor (D or not B))); C := C rol 15 + D;
  mov ESI,[rbp + 8]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea rCX,[rCX + rdi + $2AD7D2BB]
  rol ECX,15
  add ECX,EDX
//  Inc(B, Buffer[ 9] + $EB86D391 + (D xor (C or not A))); B := B rol 21 + C;
  mov ESI,[rbp + 36]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea rBX,[rBX + rdi + $EB86D391]
  rol EBX,21
  add EBX,ECX

// Add state to FDigest:
  pop rsi
  mov r8d,[rsi]
  add r8d,EAX
  mov [rsi],r8d
  mov r8d,[rsi + 4]
  add r8d,EBX
  mov [rsi + 4],r8d
  mov r8d,[rsi + 8]
  add r8d,ECX
  mov [rsi + 8],r8d
  mov r8d,[rsi + 12]
  add r8d,EDX
  mov [rsi + 12],r8d

  pop rbp
  pop rdi
  pop rsi
  pop rbx
{$ELSE}
  push EBX
  push ESI
  push EDI
  push EBP

  lea EBP,dword ptr [EAX].TBaseHash.FDataBuffer

// Copy FDigest to state:
  push EAX
  lea ESI,dword ptr [EAX].TBaseHash.FDigest
  mov EAX,[ESI]
  mov EBX,[ESI + 4]
  mov ECX,[ESI + 8]
  mov EDX,[ESI + 12]

// Compress:
{ The F function is:

   (B and C) or ((not B) and D)
Truth table:
    1  1  1 _1_    0  1   0  1
    0  0  1 _1_    1  0   1  1
    1  0  0 _0_    0  1   0  1
    0  0  0 _1_    1  1   1  1
    1  1  1 _1_    0  1   0  0
    0  0  1 _0_    1  0   0  0
    1  0  0 _0_    0  1   0  0
    0  0  0 _0_    1  0   0  0

or equivalently:

   (((D xor C) and B) xor D)
Truth table:
      1  0  1   0  1  _1_ 1
      1  0  1   0  0  _1_ 1
      1  1  0   1  1  _0_ 1
      1  1  0   0  0  _1_ 1
      0  1  1   1  1  _1_ 0
      0  1  1   0  0  _0_ 0
      0  0  0   0  1  _0_ 0
      0  0  0   0  0  _0_ 0
}

  mov ESI,[EBP]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea EAX,[EAX + EDI + $D76AA478]
  ror EAX,25
  add EAX,EBX

  mov ESI,[EBP + 4]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea EDX,[EDX + EDI + $E8C7B756]
  ror EDX,20
  add EDX,EAX

  mov ESI,[EBP + 8]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea ECX,[ECX + EDI + $242070DB]
  ror ECX,15
  add ECX,EDX

  mov ESI,[EBP + 12]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea EBX,[EBX + EDI + $C1BDCEEE]
  ror EBX,10
  add EBX,ECX

  mov ESI,[EBP + 16]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea EAX,[EAX + EDI + $F57C0FAF]
  ror EAX,25
  add EAX,EBX

  mov ESI,[EBP + 20]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea EDX,[EDX + EDI + $4787C62A]
  ror EDX,20
  add EDX,EAX

  mov ESI,[EBP + 24]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea ECX,[ECX + EDI + $A8304613]
  ror ECX,15
  add ECX,EDX

  mov ESI,[EBP + 28]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea EBX,[EBX + EDI + $FD469501]
  ror EBX,10
  add EBX,ECX

  mov ESI,[EBP + 32]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea EAX,[EDI + EAX + $698098D8]
  ror EAX,25
  add EAX,EBX

  mov ESI,[EBP + 36]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea EDX,[EDI + EDX + $8B44F7AF]
  ror EDX,20
  add EDX,EAX

  mov ESI,[EBP + 40]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea ECX,[EDI + ECX + $FFFF5BB1]
  ror ECX,15
  add ECX,EDX

  mov ESI,[EBP + 44]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea EBX,[EDI + EBX + $895CD7BE]
  ror EBX,10
  add EBX,ECX

  mov ESI,[EBP + 48]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EBX
  xor EDI,EDX
  add EAX,ESI
  lea EAX,[EDI + EAX + $6B901122]
  ror EAX,25
  add EAX,EBX

  mov ESI,[EBP + 52]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EAX
  xor EDI,ECX
  add EDX,ESI
  lea EDX,[EDI + EDX + $FD987193]
  ror EDX,20
  add EDX,EAX

  mov ESI,[EBP + 56]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,EDX
  xor EDI,EBX
  add ECX,ESI
  lea ECX,[EDI + ECX + $A679438E]
  ror ECX,15
  add ECX,EDX

  mov ESI,[EBP + 60]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,ECX
  xor EDI,EAX
  add EBX,ESI
  lea EBX,[EDI + EBX + $49B40821]
  ror EBX,10
  add EBX,ECX

{ The G function is:

   (D and B) or ((not D) and C)
Truth table:
    1  1  1 _1_    0  1   0  1
    0  0  1 _1_    1  0   1  1
    1  0  0 _0_    0  1   0  1
    0  0  0 _1_    1  1   1  1
    1  1  1 _1_    0  1   0  0
    0  0  1 _0_    1  0   0  0
    1  0  0 _0_    0  1   0  0
    0  0  0 _0_    1  0   0  0

or equivalently:

   (((C xor B) and D) xor C)
Truth table:
      1  0  1   0  1  _1_ 1
      1  0  1   0  0  _1_ 1
      1  1  0   1  1  _0_ 1
      1  1  0   0  0  _1_ 1
      0  1  1   1  1  _1_ 0
      0  1  1   0  0  _0_ 0
      0  0  0   0  1  _0_ 0
      0  0  0   0  0  _0_ 0
}

  mov ESI,[EBP + 4]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea EAX,[EDI + EAX + $F61E2562]
  rol EAX,5
  add EAX,EBX

  mov ESI,[EBP + 24]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea EDX,[EDI + EDX + $C040B340]
  rol EDX,9
  add EDX,EAX

  mov ESI,[EBP + 44]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea ECX,[EDI + ECX + $265E5A51]
  rol ECX,14
  add ECX,EDX

  mov ESI,[EBP]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea EBX,[EDI + EBX + $E9B6C7AA]
  rol EBX,20
  add EBX,ECX

  mov ESI,[EBP + 20]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea EAX,[EDI + EAX + $D62F105D]
  rol EAX,5
  add EAX,EBX

  mov ESI,[EBP + 40]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea EDX,[EDI + EDX + $02441453]
  rol EDX,9
  add EDX,EAX

  mov ESI,[EBP + 60]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea ECX,[EDI + ECX + $D8A1E681]
  rol ECX,14
  add ECX,EDX

  mov ESI,[EBP + 16]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea EBX,[EDI + EBX + $E7D3FBC8]
  rol EBX,20
  add EBX,ECX

  mov ESI,[EBP + 36]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea EAX,[EDI + EAX + $21E1CDE6]
  rol EAX,5
  add EAX,EBX

  mov ESI,[EBP + 56]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea EDX,[EDI + EDX + $C33707D6]
  rol EDX,9
  add EDX,EAX

  mov ESI,[EBP + 12]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea ECX,[EDI + ECX + $F4D50D87]
  rol ECX,14
  add ECX,EDX

  mov ESI,[EBP + 32]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea EBX,[EDI + EBX + $455A14ED]
  rol EBX,20
  add EBX,ECX

  mov ESI,[EBP + 52]
  mov EDI,ECX
  xor EDI,EBX
  and EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  lea EAX,[EDI + EAX + $A9E3E905]
  rol EAX,5
  add EAX,EBX

  mov ESI,[EBP + 8]
  mov EDI,EBX
  xor EDI,EAX
  and EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  lea EDX,[EDI + EDX + $FCEFA3F8]
  rol EDX,9
  add EDX,EAX

  mov ESI,[EBP + 28]
  mov EDI,EAX
  xor EDI,EDX
  and EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  lea ECX,[EDI + ECX + $676F02D9]
  rol ECX,14
  add ECX,EDX

  mov ESI,[EBP + 48]
  mov EDI,EDX
  xor EDI,ECX
  and EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  lea EBX,[EDI + EBX + $8D2A4C8A]
  rol EBX,20
  add EBX,ECX

  mov ESI,[EBP + 20]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea EAX,[EAX + EDI + $FFFA3942]
  rol EAX,4
  add EAX,EBX

  mov ESI,[EBP + 32]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea EDX,[EDX + EDI + $8771F681]
  rol EDX,11
  add EDX,EAX

  mov ESI,[EBP + 44]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea ECX,[ECX + EDI + $6D9D6122]
  rol ECX,16
  add ECX,EDX

  mov ESI,[EBP + 56]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea EBX,[EBX + EDI + $FDE5380C]
  rol EBX,23
  add EBX,ECX

  mov ESI,[EBP + 4]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea EAX,[EAX + EDI + $A4BEEA44]
  rol EAX,4
  add EAX,EBX

  mov ESI,[EBP + 16]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea EDX,[EDX + EDI + $4BDECFA9]
  rol EDX,11
  add EDX,EAX

  mov ESI,[EBP + 28]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea ECX,[ECX + EDI + $F6BB4B60]
  rol ECX,16
  add ECX,EDX

  mov ESI,[EBP + 40]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea EBX,[EBX + EDI + $BEBFBC70]
  rol EBX,23
  add EBX,ECX

  mov ESI,[EBP + 52]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea EAX,[EAX + EDI + $289B7EC6]
  rol EAX,4
  add EAX,EBX

  mov ESI,[EBP]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea EDX,[EDX + EDI + $EAA127FA]
  rol EDX,11
  add EDX,EAX

  mov ESI,[EBP + 12]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea ECX,[ECX + EDI + $D4EF3085]
  rol ECX,16
  add ECX,EDX

  mov ESI,[EBP + 24]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea EBX,[EBX + EDI + $04881D05]
  rol EBX,23
  add EBX,ECX

  mov ESI,[EBP + 36]
  mov EDI,EDX
  xor EDI,ECX
  add EAX,ESI
  xor EDI,EBX
  lea EAX,[EAX + EDI + $D9D4D039]
  rol EAX,4
  add EAX,EBX

  mov ESI,[EBP + 48]
  mov EDI,ECX
  xor EDI,EBX
  add EDX,ESI
  xor EDI,EAX
  lea EDX,[EDX + EDI + $E6DB99E5]
  rol EDX,11
  add EDX,EAX

  mov ESI,[EBP + 60]
  mov EDI,EBX
  xor EDI,EAX
  add ECX,ESI
  xor EDI,EDX
  lea ECX,[ECX + EDI + $1FA27CF8]
  rol ECX,16
  add ECX,EDX

  mov ESI,[EBP + 8]
  mov EDI,EAX
  xor EDI,EDX
  add EBX,ESI
  xor EDI,ECX
  lea EBX,[EBX + EDI + $C4AC5665]
  rol EBX,23
  add EBX,ECX

  mov ESI,[EBP]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea EAX,[EAX + EDI + $F4292244]
  rol EAX,6
  add EAX,EBX

  mov ESI,[EBP + 28]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea EDX,[EDX + EDI + $432AFF97]
  rol EDX,10
  add EDX,EAX

  mov ESI,[EBP + 56]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea ECX,[ECX + EDI + $AB9423A7]
  rol ECX,15
  add ECX,EDX

  mov ESI,[EBP + 20]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea EBX,[EBX + EDI + $FC93A039]
  rol EBX,21
  add EBX,ECX

  mov ESI,[EBP + 48]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea EAX,[EAX + EDI + $655B59C3]
  rol EAX,6
  add EAX,EBX

  mov ESI,[EBP + 12]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea EDX,[EDX + EDI + $8F0CCC92]
  rol EDX,10
  add EDX,EAX

  mov ESI,[EBP + 40]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea ECX,[ECX + EDI + $FFEFF47D]
  rol ECX,15
  add ECX,EDX

  mov ESI,[EBP + 4]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea EBX,[EBX + EDI + $85845DD1]
  rol EBX,21
  add EBX,ECX

  mov ESI,[EBP + 32]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea EAX,[EAX + EDI + $6FA87E4F]
  rol EAX,6
  add EAX,EBX

  mov ESI,[EBP + 60]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea EDX,[EDX + EDI + $FE2CE6E0]
  rol EDX,10
  add EDX,EAX

  mov ESI,[EBP + 24]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea ECX,[ECX + EDI + $A3014314]
  rol ECX,15
  add ECX,EDX

  mov ESI,[EBP + 52]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea EBX,[EBX + EDI + $4E0811A1]
  rol EBX,21
  add EBX,ECX

  mov ESI,[EBP + 16]
  mov EDI,EDX
  not EDI
  or  EDI,EBX
  add EAX,ESI
  xor EDI,ECX
  lea EAX,[EAX + EDI + $F7537E82]
  rol EAX,6
  add EAX,EBX

  mov ESI,[EBP + 44]
  mov EDI,ECX
  not EDI
  or  EDI,EAX
  add EDX,ESI
  xor EDI,EBX
  lea EDX,[EDX + EDI + $BD3AF235]
  rol EDX,10
  add EDX,EAX

  mov ESI,[EBP + 8]
  mov EDI,EBX
  not EDI
  or  EDI,EDX
  add ECX,ESI
  xor EDI,EAX
  lea ECX,[ECX + EDI + $2AD7D2BB]
  rol ECX,15
  add ECX,EDX

  mov ESI,[EBP + 36]
  mov EDI,EAX
  not EDI
  or  EDI,ECX
  add EBX,ESI
  xor EDI,EDX
  lea EBX,[EBX + EDI + $EB86D391]
  rol EBX,21
  add EBX,ECX

// Add state to FDigest:
  mov EBP,EAX
  pop EAX
  lea ESI,dword ptr [EAX].TBaseHash.FDigest
  mov EAX,[ESI]
  add EAX,EBP
  mov [ESI],EAX
  mov EAX,[ESI + 4]
  add EAX,EBX
  mov [ESI + 4],EAX
  mov EAX,[ESI + 8]
  add EAX,ECX
  mov [ESI + 8],EAX
  mov EAX,[ESI + 12]
  add EAX,EDX
  mov [ESI + 12],EAX

  pop EBP
  pop EDI
  pop ESI
  pop EBX
  {$ENDIF}
end;

class function TMD5.DERPrefix: Pointer;
begin
  Result := @DERPrefixMD5;
end;

class function TMD5.DERPrefixLen: Integer;
begin
  Result := SizeOf(DERPrefixMD5);
end;

class function TMD5.DigestSize: Integer;
begin
  Result := 16;
end;

procedure TMD5.Init;
asm
  // Initialize FDigest.
  {$IFDEF WIN64}
  lea rdx,dword ptr [rcx].TBaseHash.FDigest
  mov dword [rdx     ],$67452301;
  mov dword [rdx +  4],$EFCDAB89;
  mov dword [rdx +  8],$98BADCFE;
  mov dword [rdx + 12],$10325476;
  {$ELSE}
  lea EDX,dword ptr [EAX].TBaseHash.FDigest
  mov dword [EDX     ],$67452301;
  mov dword [EDX +  4],$EFCDAB89;
  mov dword [EDX +  8],$98BADCFE;
  mov dword [EDX + 12],$10325476;
  {$ENDIF}
end;        

{ TEncryptStream }

constructor TEncryptStream.Create(ADataStream: TStream; ACipher: TCipher);
begin
  inherited Create;
  FCipher := ACipher;
  if ACipher is TBlockCipher then begin
    FBlockSize := ACipher.BlockSize;
    GetMem(FBlock,FBlockSize);
  end;
  FDataStream := ADataStream;
end;

destructor TEncryptStream.Destroy;
begin
  ProtectClear(FBlock^,FBlockSize);
  FreeMem(FBlock);
  FBlock := nil;
  inherited;
end;

function TEncryptStream.Done: Integer;
var
  P: PAnsiChar;
  I, PadSize: Integer;
begin
  if Assigned(FBlock) then begin
    PadSize := FBlockSize - FBlockCount;
    P := FBlock;
    for I := FBlockSize - 1 downto FBlockCount do
      P[I] := AnsiChar(PadSize);
    FCipher.Encrypt(FBlock^,FBlockSize);
    FDataStream.Write(FBlock^,FBlockSize);
    Result := PadSize;
    FBlockCount := 0;
  end else
    Result := 0;
end;

function TEncryptStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

function TEncryptStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(FDataStream) then
    Result := FDataStream.Seek(Offset,Origin)
  else
    Result := 0;
end;

procedure TEncryptStream.SetCipher(const Value: TCipher);
begin
  FCipher := Value;
end;

procedure TEncryptStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

function TEncryptStream.Write(const Buffer; Count: Integer): Longint;
var
  P: PAnsiChar;
  InternalBuffer: array [0..1023] of Byte;
  InBuffer: Integer;
begin
  if Assigned(FDataStream) then begin
    if Assigned(FCipher) then begin
      Result := 0;
      P := @Buffer;
      Count := Count + FBlockCount;
      while Count > 0 do begin
        if FBlockCount > 0 then
          Move(FBlock^,InternalBuffer,FBlockCount);
        if Count >= 1024 then begin
          Move(P^,InternalBuffer[FBlockCount],1024 - FBlockCount);
          P := P + 1024 - FBlockCount;
          Result := Result + 1024 - FBlockCount;
          InBuffer := 1024;
          Count := Count - 1024;
        end else begin
          Move(P^,InternalBuffer[FBlockCount],Count - FBlockCount);
          P := P + Count - FBlockCount;
          Result := Result + Count - FBlockCount;
          InBuffer := Count;
          Count := 0;
        end;
        if FBlockSize > 0 then begin
          FBlockCount := InBuffer mod FBlockSize;
          InBuffer := InBuffer - FBlockCount;
          Count := Count + FBlockCount;
          Move(InternalBuffer[InBuffer],FBlock^,FBlockCount);
        end else
          FBlockCount := 0;
        if InBuffer = 0 then
          Break;
        FCipher.Encrypt(InternalBuffer,InBuffer);
        FDataStream.Write(InternalBuffer,InBuffer);
      end;
    end else
      Result := FDataStream.Write(Buffer,Count);
  end else
    Result := 0;
end;

{ TDecryptStream }

constructor TDecryptStream.Create(ADataStream: TStream; ACipher: TCipher);
begin    
  inherited Create;
  FCipher := ACipher;
  if ACipher is TBlockCipher then begin
    FBlockSize := ACipher.BlockSize;
    GetMem(FBlock,FBlockSize);
  end;
  FDataStream := ADataStream;
end;

destructor TDecryptStream.Destroy;
begin
  inherited;
  ProtectClear(FBlock^,FBlockCount);
  FreeMem(FBlock);
  ProtectClear(FDecBuffer,FDecLen);
end;

function TDecryptStream.Done: Integer;
var
  I: Integer;
begin
  Result := 0;
  FOK := FDecLen <= FBlockSize;
  if FOK and (FDecLen > 0) then begin
    Result := FDecBuffer[FDecLen - 1];
    if Result > FBlockSize then
      FOK := False
    else if Result <> FDecLen then
      FOK := False
    else
      for I := 0 to Result - 2 do
        if FDecBuffer[I] <> Result then
          FOK := False;
  end;
end;

function TDecryptStream.Read(var Buffer; Count: Integer): Longint;
var
  P: PAnsiChar;
  InBuffer, PLen, DLen: Integer;
begin
  if Assigned(FDataStream) then begin
    if Assigned(FCipher) then begin
      Result := 0;
      InBuffer := 0;
      P := @Buffer;
      if FDecLen > 0 then begin
        if FDecLen > Count then begin
          Move(FDecBuffer,Buffer,Count);
          Move(FDecBuffer[Count],FDecBuffer,FDecLen - Count);
          FDecLen := FDecLen - Count;
          Result := Count;
          Count := 0;
        end else begin
          Move(FDecBuffer,Buffer,FDecLen);
          P := P + FDecLen;
          Result := FDecLen;
          Count := Count - FDecLen;
          FDecLen := 0;
        end;
      end else if FBlockCount > 0 then begin
        Move(FBlock^,P^,FBlockCount);
        InBuffer := FBlockCount;
        P := P + InBuffer;
        FBlockCount := 0;
      end;
      if Count > 0 then begin
        InBuffer := InBuffer + FDataStream.Read(P^,Count);
        Result := Result + InBuffer;
        DLen := InBuffer mod FBlockSize;
        PLen := InBuffer - DLen;
        if PLen > 0 then FCipher.Decrypt(P^,PLen);
        Move(P[PLen],FBlock^,DLen);
        FBlockCount := FDataStream.Read(FBlock^[DLen],FBlockSize - DLen) + DLen;
        if FBlockCount = FBlockSize then begin
          FCipher.DecryptToPtr(FBlock,@FDecBuffer,FBlockCount);
          Move(FDecBuffer,P[PLen],DLen);
          FBlockCount := 0;
          FDecLen := FBlockSize - DLen;
          Move(FDecBuffer[DLen],FDecBuffer,FDecLen);
        end else
          Result := Result - DLen;
      end;
    end else
      Result := FDataStream.Read(Buffer,Count);
  end else
    Result := 0;
end;

function TDecryptStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  if Assigned(FDataStream) then begin
    Result := FDataStream.Seek(Offset,Origin);
  end else
    Result := 0;
end;

procedure TDecryptStream.SetCipher(const Value: TCipher);
begin
  FCipher := Value;
end;

procedure TDecryptStream.SetDataStream(const Value: TStream);
begin
  FDataStream := Value;
end;

function TDecryptStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := 0;
end;

{ TRijndael }

procedure TRijndael.CleanUp;
begin
  ProtectClear(rk,Sizeof(rk));
  ProtectClear(drk,Sizeof(drk));
  ProtectClear(IV,Sizeof(IV));
  NumRounds:= 0;
end;

procedure TRijndael.DecryptBlock(var Buf);
type
  TBlock = array [0..3] of LongWord;
var
  P: ^TBlock;
  A, B, C, D, E, F, G, H: LongWord;
  I: Integer;
begin
  P := @Buf;
  A := P[0] xor drk[0,0];
  B := P[1] xor drk[0,1];
  C := P[2] xor drk[0,2];
  D := P[3] xor drk[0,3];
  for I := 1 to numrounds-1 do begin
    E := A;
    G := B;
    H := D;

    A := drk[I,0];
    B := drk[I,1];
    F := Lo(E);
    A := A xor LongWord(T4[0,F]);
    D := drk[I,3];
    F := Hi(E);
    E := E shr 16;
    B := B xor LongWord(T4[1,F]);
    F := Lo(E);
    E := Hi(E);
    D := D xor LongWord(T4[3,E]);
    E := C;
    C := drk[I,2];
    C := C xor LongWord(T4[2,F]);

    F := Lo(E);
    C := C xor LongWord(T4[0,F]);
    F := Hi(E);
    E := E shr 16;
    D := D xor LongWord(T4[1,F]);
    F := Lo(E);
    E := Hi(E);
    A := A xor LongWord(T4[2,F]);
    B := B xor LongWord(T4[3,E]);

    F := Lo(G);
    B := B xor LongWord(T4[0,F]);
    F := Hi(G);
    G := G shr 16;
    C := C xor LongWord(T4[1,F]);
    F := Lo(G);
    E := Hi(G);
    D := D xor LongWord(T4[2,F]);
    A := A xor LongWord(T4[3,E]);

    F := Lo(H);
    D := D xor LongWord(T4[0,F]);
    F := Hi(H);
    H := H shr 16;
    A := A xor LongWord(T4[1,F]);
    F := Lo(H);
    E := Hi(H);
    B := B xor LongWord(T4[2,F]);
    C := C xor LongWord(T4[3,E]);
  end;
  E := A;
  G := B;
  H := D;

  I := numrounds;

  A := drk[I,0];
  B := drk[I,1];
  F := Lo(E);
  A := A xor LongWord(S4[0,F]);
  D := drk[I,3];
  F := Hi(E);
  E := E shr 16;
  B := B xor LongWord(S4[1,F]);
  F := Lo(E);
  E := Hi(E);
  D := D xor LongWord(S4[3,E]);
  E := C;
  C := drk[I,2];
  C := C xor LongWord(S4[2,F]);

  F := Lo(E);
  C := C xor LongWord(S4[0,F]);
  F := Hi(E);
  E := E shr 16;
  D := D xor LongWord(S4[1,F]);
  F := Lo(E);
  E := Hi(E);
  A := A xor LongWord(S4[2,F]);
  B := B xor LongWord(S4[3,E]);

  F := Lo(G);
  B := B xor LongWord(S4[0,F]);
  F := Hi(G);
  G := G shr 16;
  C := C xor LongWord(S4[1,F]);
  F := Lo(G);
  E := Hi(G);
  D := D xor LongWord(S4[2,F]);
  A := A xor LongWord(S4[3,E]);

  F := Lo(H);
  D := D xor LongWord(S4[0,F]);
  F := Hi(H);
  H := H shr 16;
  A := A xor LongWord(S4[1,F]);
  F := Lo(H);
  E := Hi(H);
  B := B xor LongWord(S4[2,F]);
  C := C xor LongWord(S4[3,E]);

  P[0] := A;
  P[1] := B;
  P[2] := C;
  P[3] := D;
{$WARNINGS ON}
end;

procedure TRijndael.EncryptBlock(var Buf);
type
  TBlock = array [0..3] of LongWord;
var
  P: ^TBlock;
  A, B, C, D, E, F, G, H: LongWord;
  I: Integer;
begin
  P := @Buf;
  A := P[0] xor rk[0,0];
  B := P[1] xor rk[0,1];
  C := P[2] xor rk[0,2];
  D := P[3] xor rk[0,3];
  for I := 1 to numrounds-1 do begin
    E := A;
    G := B;
    H := D;

    A := rk[I,0];
    D := rk[I,3];
    F := Lo(E);
    A := A xor LongWord(T0[0,F]);
    B := rk[I,1];
    F := Hi(E);
    E := E shr 16;
    D := D xor LongWord(T0[1,F]);
    F := Lo(E);
    E := Hi(E);
    B := B xor LongWord(T0[3,E]);
    E := C;
    C := rk[I,2];
    C := C xor LongWord(T0[2,F]);

    F := Lo(E);
    C := C xor LongWord(T0[0,F]);
    F := Hi(E);
    E := E shr 16;
    B := B xor LongWord(T0[1,F]);
    F := Lo(E);
    E := Hi(E);
    A := A xor LongWord(T0[2,F]);
    D := D xor LongWord(T0[3,E]);

    F := Lo(G);
    B := B xor LongWord(T0[0,F]);
    F := Hi(G);
    G := G shr 16;
    A := A xor LongWord(T0[1,F]);
    F := Lo(G);
    E := Hi(G);
    D := D xor LongWord(T0[2,F]);
    C := C xor LongWord(T0[3,E]);

    F := Lo(H);
    D := D xor LongWord(T0[0,F]);
    F := Hi(H);
    H := H shr 16;
    C := C xor LongWord(T0[1,F]);
    F := Lo(H);
    E := Hi(H);
    B := B xor LongWord(T0[2,F]);
    A := A xor LongWord(T0[3,E]);
  end;
  E := A;
  G := B;
  H := D;

  I := numrounds;

  A := rk[I,0];
  D := rk[I,3];
  F := Lo(E);
  A := A xor LongWord(S0[0,F]);
  B := rk[I,1];
  F := Hi(E);
  E := E shr 16;
  D := D xor LongWord(S0[1,F]);
  F := Lo(E);
  E := Hi(E);
  B := B xor LongWord(S0[3,E]);
  E := C;
  C := rk[I,2];
  C := C xor LongWord(S0[2,F]);

  F := Lo(E);
  C := C xor LongWord(S0[0,F]);
  F := Hi(E);
  E := E shr 16;
  B := B xor LongWord(S0[1,F]);
  F := Lo(E);
  E := Hi(E);
  A := A xor LongWord(S0[2,F]);
  D := D xor LongWord(S0[3,E]);

  F := Lo(G);
  B := B xor LongWord(S0[0,F]);
  F := Hi(G);
  G := G shr 16;
  A := A xor LongWord(S0[1,F]);
  F := Lo(G);
  E := Hi(G);
  D := D xor LongWord(S0[2,F]);
  C := C xor LongWord(S0[3,E]);

  F := Lo(H);
  D := D xor LongWord(S0[0,F]);
  F := Hi(H);
  H := H shr 16;
  C := C xor LongWord(S0[1,F]);
  F := Lo(H);
  E := Hi(H);
  B := B xor LongWord(S0[2,F]);
  A := A xor LongWord(S0[3,E]);

  P[0] := A;
  P[1] := B;
  P[2] := C;
  P[3] := D;
{$WARNINGS ON}
end;

procedure InvMixColumn(a: PByteArray; BC: byte);
var
  j: longint;
begin
  for j:= 0 to (BC-1) do
    PDWord(@(a^[j*4]))^ := PDWord(@U1[a^[j*4+0]])^ xor
                           PDWord(@U2[a^[j*4+1]])^ xor
                           PDWord(@U3[a^[j*4+2]])^ xor
                           PDWord(@U4[a^[j*4+3]])^;
end;

procedure TRijndael.SetUp(const AKey; Count: Integer);
var
  KC, ROUNDS, j, r, t, rconpointer: longint;
  tk: array[0..MAXKC-1,0..3] of byte;
  Size: LongInt;
begin
  CleanUp;
  if (Count <= 0) then
    raise Exception.Create(Format('Rijndael: Invalid key size - %d',[Count]));

  Size := Count;

  FillChar(tk,Sizeof(tk),0);
  Move(AKey,tk,Size);
  if Size <= 16 then begin
    KC := 4;
    Rounds := 10;
  end else if Size <= 24 then begin
    KC := 6;
    Rounds := 12;
  end else begin
    KC := 8;
    Rounds := 14;
  end;
  numrounds := rounds;
  r := 0;
  t := 0;
  j := 0;
  while (j < KC) and (r < rounds+1) do begin
    while (j < KC) and (t < BC) do begin
      rk[r,t] := PDWord(@tk[j])^;
      Inc(j);
      Inc(t);
    end;
    if t = BC then begin
      t := 0;
      Inc(r);
    end;
  end;
  rconpointer := 0;
  while r < rounds+1 do begin
    tk[0,0] := tk[0,0] xor S[tk[KC-1,1]];
    tk[0,1] := tk[0,1] xor S[tk[KC-1,2]];
    tk[0,2] := tk[0,2] xor S[tk[KC-1,3]];
    tk[0,3] := tk[0,3] xor S[tk[KC-1,0]];
    tk[0,0] := tk[0,0] xor rcon[rconpointer];
    Inc(rconpointer);
    if KC <> 8 then begin
      for j := 1 to KC-1 do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end else begin
      for j:= 1 to ((KC div 2)-1) do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
      tk[KC shr 1,0]:= tk[KC shr 1,0] xor S[tk[KC shr 1 - 1,0]];
      tk[KC shr 1,1]:= tk[KC shr 1,1] xor S[tk[KC shr 1 - 1,1]];
      tk[KC shr 1,2]:= tk[KC shr 1,2] xor S[tk[KC shr 1 - 1,2]];
      tk[KC shr 1,3]:= tk[KC shr 1,3] xor S[tk[KC shr 1 - 1,3]];
      for j:= (KC shr 1) + 1 to KC-1 do
        PDWord(@tk[j])^:= PDWord(@tk[j])^ xor PDWord(@tk[j-1])^;
    end;
    j:= 0;
    while (j < KC) and (r < rounds+1) do begin
      while (j < KC) and (t < BC) do begin
        rk[r,t] := PDWord(@tk[j])^;
        Inc(j);
        Inc(t);
      end;
      if t = BC then begin
        Inc(r);
        t := 0;
      end;
    end;
  end;
  ProtectClear(tk,SizeOf(tk));
  drk[0] := rk[numrounds];
  drk[numrounds] := rk[0];
  for r:= 1 to (numrounds-1) do begin
    drk[r] := rk[numrounds-r];
    InvMixColumn(@drk[r],BC);
  end;
end;

class function TRijndael.BlockSize: Integer;
begin
  Result := 16;
end;

constructor TRijndael.Create(const AKey; Count: Integer);
begin
  FIV := @IV;
  inherited;
end;

class function TRijndael.MaxKeySize: Integer;
begin
  Result := 32;
end;

class function TRijndael.MinKeySize: Integer;
begin
  Result := 16;
end;

class function TRijndael.Algorithm: TCipherAlg;
begin
  Result := caRijndael;
end;

class function TRijndael.AlgorithmName: PAnsiChar;
begin
  Result := 'Rijndael';
end;

procedure TRijndael.DecryptBlockToDst(var Dst; const Src);
type
  TBlock = array [0..3] of LongWord;
var
  P: ^TBlock;
  A, B, C, D, E, F, G, H: LongWord;
  I: Integer;
begin
  P := @Src;
  A := P[0] xor drk[0,0];
  B := P[1] xor drk[0,1];
  C := P[2] xor drk[0,2];
  D := P[3] xor drk[0,3];
  for I := 1 to numrounds-1 do begin
    E := A;
    G := B;
    H := D;

    A := drk[I,0];
    B := drk[I,1];
    F := Lo(E);
    A := A xor LongWord(T4[0,F]);
    D := drk[I,3];
    F := Hi(E);
    E := E shr 16;
    B := B xor LongWord(T4[1,F]);
    F := Lo(E);
    E := Hi(E);
    D := D xor LongWord(T4[3,E]);
    E := C;
    C := drk[I,2];
    C := C xor LongWord(T4[2,F]);

    F := Lo(E);
    C := C xor LongWord(T4[0,F]);
    F := Hi(E);
    E := E shr 16;
    D := D xor LongWord(T4[1,F]);
    F := Lo(E);
    E := Hi(E);
    A := A xor LongWord(T4[2,F]);
    B := B xor LongWord(T4[3,E]);

    F := Lo(G);
    B := B xor LongWord(T4[0,F]);
    F := Hi(G);
    G := G shr 16;
    C := C xor LongWord(T4[1,F]);
    F := Lo(G);
    E := Hi(G);
    D := D xor LongWord(T4[2,F]);
    A := A xor LongWord(T4[3,E]);

    F := Lo(H);
    D := D xor LongWord(T4[0,F]);
    F := Hi(H);
    H := H shr 16;
    A := A xor LongWord(T4[1,F]);
    F := Lo(H);
    E := Hi(H);
    B := B xor LongWord(T4[2,F]);
    C := C xor LongWord(T4[3,E]);
  end;
  E := A;
  G := B;
  H := D;

  I := numrounds;

  A := drk[I,0];
  B := drk[I,1];
  F := Lo(E);
  A := A xor LongWord(S4[0,F]);
  D := drk[I,3];
  F := Hi(E);
  E := E shr 16;
  B := B xor LongWord(S4[1,F]);
  F := Lo(E);
  E := Hi(E);
  D := D xor LongWord(S4[3,E]);
  E := C;
  C := drk[I,2];
  C := C xor LongWord(S4[2,F]);

  F := Lo(E);
  C := C xor LongWord(S4[0,F]);
  F := Hi(E);
  E := E shr 16;
  D := D xor LongWord(S4[1,F]);
  F := Lo(E);
  E := Hi(E);
  A := A xor LongWord(S4[2,F]);
  B := B xor LongWord(S4[3,E]);

  F := Lo(G);
  B := B xor LongWord(S4[0,F]);
  F := Hi(G);
  G := G shr 16;
  C := C xor LongWord(S4[1,F]);
  F := Lo(G);
  E := Hi(G);
  D := D xor LongWord(S4[2,F]);
  A := A xor LongWord(S4[3,E]);

  F := Lo(H);
  D := D xor LongWord(S4[0,F]);
  F := Hi(H);
  H := H shr 16;
  A := A xor LongWord(S4[1,F]);
  F := Lo(H);
  E := Hi(H);
  B := B xor LongWord(S4[2,F]);
  C := C xor LongWord(S4[3,E]);

  P := @Dst;
  P[0] := A;
  P[1] := B;
  P[2] := C;
  P[3] := D;
end;

procedure TRijndael.EncryptBlockToDst(var Dst; const Src);
type
  TBlock = array [0..3] of LongWord;
var
  P: ^TBlock;
  A, B, C, D, E, F, G, H: LongWord;
  I: Integer;
begin
  P := @Src;
  A := P[0] xor rk[0,0];
  B := P[1] xor rk[0,1];
  C := P[2] xor rk[0,2];
  D := P[3] xor rk[0,3];
  for I := 1 to numrounds-1 do begin
    E := A;
    G := B;
    H := D;

    A := rk[I,0];
    D := rk[I,3];
    F := Lo(E);
    A := A xor LongWord(T0[0,F]);
    B := rk[I,1];
    F := Hi(E);
    E := E shr 16;
    D := D xor LongWord(T0[1,F]);
    F := Lo(E);
    E := Hi(E);
    B := B xor LongWord(T0[3,E]);
    E := C;
    C := rk[I,2];
    C := C xor LongWord(T0[2,F]);

    F := Lo(E);
    C := C xor LongWord(T0[0,F]);
    F := Hi(E);
    E := E shr 16;
    B := B xor LongWord(T0[1,F]);
    F := Lo(E);
    E := Hi(E);
    A := A xor LongWord(T0[2,F]);
    D := D xor LongWord(T0[3,E]);

    F := Lo(G);
    B := B xor LongWord(T0[0,F]);
    F := Hi(G);
    G := G shr 16;
    A := A xor LongWord(T0[1,F]);
    F := Lo(G);
    E := Hi(G);
    D := D xor LongWord(T0[2,F]);
    C := C xor LongWord(T0[3,E]);

    F := Lo(H);
    D := D xor LongWord(T0[0,F]);
    F := Hi(H);
    H := H shr 16;
    C := C xor LongWord(T0[1,F]);
    F := Lo(H);
    E := Hi(H);
    B := B xor LongWord(T0[2,F]);
    A := A xor LongWord(T0[3,E]);
  end;
  E := A;
  G := B;
  H := D;

  I := numrounds;

  A := rk[I,0];
  D := rk[I,3];
  F := Lo(E);
  A := A xor LongWord(S0[0,F]);
  B := rk[I,1];
  F := Hi(E);
  E := E shr 16;
  D := D xor LongWord(S0[1,F]);
  F := Lo(E);
  E := Hi(E);
  B := B xor LongWord(S0[3,E]);
  E := C;
  C := rk[I,2];
  C := C xor LongWord(S0[2,F]);

  F := Lo(E);
  C := C xor LongWord(S0[0,F]);
  F := Hi(E);
  E := E shr 16;
  B := B xor LongWord(S0[1,F]);
  F := Lo(E);
  E := Hi(E);
  A := A xor LongWord(S0[2,F]);
  D := D xor LongWord(S0[3,E]);

  F := Lo(G);
  B := B xor LongWord(S0[0,F]);
  F := Hi(G);
  G := G shr 16;
  A := A xor LongWord(S0[1,F]);
  F := Lo(G);
  E := Hi(G);
  D := D xor LongWord(S0[2,F]);
  C := C xor LongWord(S0[3,E]);

  F := Lo(H);
  D := D xor LongWord(S0[0,F]);
  F := Hi(H);
  H := H shr 16;
  C := C xor LongWord(S0[1,F]);
  F := Lo(H);
  E := Hi(H);
  B := B xor LongWord(S0[2,F]);
  A := A xor LongWord(S0[3,E]);

  P := @Dst;
  P[0] := A;
  P[1] := B;
  P[2] := C;
  P[3] := D;
{$WARNINGS ON}
end;

{ TBlockCipher }

function TBlockCipher.AllocBuffer: PByteArray;
begin
  GetMem(Result,BlockSize);
end;

procedure TBlockCipher.CleanUp;
begin
  ProtectClear(FIV^,BlockSize * BlockVectorSize);
end;

procedure TBlockCipher.DeallocBuffer(var T: PByteArray);
begin
  FreeMem(T);
end;

procedure TBlockCipher.Decrypt(var Buf; Count: Integer);
var
  I, BlockLen, C: Integer;
  PT: PByteArray;
begin
  case Mode of
    cmECB:
      begin
        BlockLen := BlockSize;
        PT := Addr(Buf);
        C := Count div BlockLen;
        for I := 0 to C-1 do begin
          DecryptBlock(PT[0]);
          PT := Addr(PT[BlockLen]);
        end;
      end;
    cmCBC: DecryptCBC(Buf,Count);
    cmCFB: DecryptCFB(Buf,Count);
    cmCTR: DecryptCTR(Buf,Count);
    cmOFB: DecryptOFB(Buf,Count);
    cmABC: DecryptABC(Buf,Count);
  else
    Assert(False,'Cipher mode not supported');
  end;
end;

procedure TBlockCipher.DecryptBlock(var Buf);
begin
  DecryptBlockToDst(Buf,Buf);
end;

procedure TBlockCipher.DecryptBlockToDst(var Dst; const Src);
begin
  Move(Src,Dst,BlockSize);
  DecryptBlock(Dst);
end;

procedure TBlockCipher.DecryptABC(var Buf; Count: Integer);
var
  C, I, BlockLen, BlockLWLen: Integer;
  CT, PT, HT: PByteArray;
begin
  BlockLen := BlockSize;
  BlockLWLen := BlockLen shr 2;
  PT := Addr(Buf);
  CT := Addr(FIV[BlockLen]);
  HT := Addr(FIV[BlockLen*2]);
  C := Count div BlockLen;
  for I := 0 to C - 1 do begin
    // A[i] := D(C[i] xor H[i-1])
    XORBytesBackup(FIV[0],PT[0],HT[0],BlockLen);
    DecryptBlock(FIV[0]);
    // H[i] := A[i] xor C[i-1]
    // P[i] := H[i] xor H[i-1]
    XORBytesABCDcr(PT[0],FIV[0],HT[0],CT[0],BlockLen);
    PT := Addr(PT[BlockLen]);
  end;
  C := Count mod BlockLen;
  if C > 0 then begin
    XORBlock(FIV[0],CT[0],BlockLWLen);
    EncryptBlock(FIV[0]);
    for I := 0 to C - 1 do
      PT[I] := PT[I] xor FIV[I];
  end;
end;

procedure TBlockCipher.DecryptCBC(var Buf; Count: Integer);
var
  C, I, BlockLen, BlockLWLen: Integer;
  CT, PT, PrevCT: PByteArray;
begin
  BlockLen := BlockSize;
  BlockLWLen := BlockLen shr 2;
  PT := Addr(Buf);
  CT := AllocBuffer;
  try
    C := Count div (2*BlockLen);
    PrevCT := Addr(FIV[0]);
    for I := 0 to C - 1 do begin
      Move(PT[0],CT[0],BlockLen);
      DecryptBlock(PT[0]);
      XORBlock(PT[0],PrevCT[0],BlockLWLen);
      PT := Addr(PT[BlockLen]);
      Move(PT[0],PrevCT[0],BlockLen);
      DecryptBlock(PT[0]);
      XORBlock(PT[0],CT[0],BlockLWLen);
      PT := Addr(PT[BlockLen]);
    end;
    C := Count mod (2*BlockLen);
    if C >= BlockLen then begin
      Move(PT[0],CT[0],BlockLen);
      DecryptBlock(PT[0]);
      XORBlock(PT[0],PrevCT[0],BlockLWLen);
      Move(CT[0],FIV[0],BlockLen);
      PT := Addr(PT[BlockLen]);
      C := C - BlockLen;
    end;
    if C > 0 then begin
      EncryptBlock(FIV[0]);
      for I := 0 to C - 1 do
        PT[I] := PT[I] xor FIV[I];
    end;
  finally
    DeallocBuffer(CT);
  end;
end;

procedure TBlockCipher.DecryptCFB(var Buf; Count: Integer);
var
  I, K, BlockLen, C: Integer;
  PT, CT: PByteArray;
begin
  BlockLen := BlockSize;
  PT := Addr(Buf);
  CT := AllocBuffer;
  try
    if ModeRatio = BlockSize then begin
      if FBIndex > 0 then begin
        C := BlockLen;
        if C > Count + FBIndex then
          C := Count + FBIndex;
        XORBytesBackup(PT[0],FIV[FBIndex],FIV[FBIndex],C - FBIndex);
        PT := Addr(PT[C - FBIndex]);
        Count := Count - C + FBIndex;
        FFBIndex := C mod BlockLen;
      end;
      C := Count div BlockLen;
      for I := 0 to C - 1 do begin
        EncryptBlockToDst(CT[0],FIV[0]);
        XORBytesBackUp(PT[0],CT[0],FIV[0],BlockLen);
        PT := Addr(PT[BlockLen]);
        FFBIndex := 0;
      end;
      C := Count mod BlockLen;
      if C > 0 then begin
        EncryptBlockToDst(CT[0],FIV[0]);
        XORBytesBackup(PT[0],CT[0],FIV[0],C);
        Move(CT[C],FIV[C],BlockLen - C);
        FFBIndex := C;
      end;
    end else begin
      if FBIndex > 0 then begin
        C := ModeRatio;
        if C > Count + FBIndex then
          C := Count + FBIndex;
        K := BlockLen - ModeRatio + FBIndex;
        XORBytesBackup(PT[0],FIV[K],FIV[K],C - FBIndex);
        PT := Addr(PT[C - FBIndex]);
        Count := Count - C + FBIndex;
        FFBIndex := C mod ModeRatio;
      end;
      C := Count div ModeRatio;
      for I := 0 to C - 1 do begin
        EncryptBlockToDst(CT[0],FIV[0]);
        Move(FIV[ModeRatio],FIV[0],BlockLen - ModeRatio);
        XORBytesBackUp(PT[0],CT[0],FIV[BlockLen - ModeRatio],ModeRatio);
        PT := Addr(PT[ModeRatio]);
        FFBIndex := 0;
      end;
      C := Count mod ModeRatio;
      if C > 0 then begin
        EncryptBlockToDst(CT[0],FIV[0]);
        Move(FIV[ModeRatio],FIV[0],BlockLen - ModeRatio);
        XORBytesBackUp(PT[0],CT[0],FIV[BlockLen - ModeRatio],C);
        Move(CT[C],FIV[BlockLen - ModeRatio + C],ModeRatio - C);
        FFBIndex := C;
      end;
    end;
  finally
    DeallocBuffer(CT);
  end;
end;

procedure TBlockCipher.DecryptCTR(var Buf; Count: Integer);
begin
  // Encrypt_CTR = Decrypt_CTR
  EncryptCTR(Buf, Count);
end;

procedure TBlockCipher.DecryptOFB(var Buf; Count: Integer);
begin
  // Encrypt_OFB = Decrypt_OFB
  EncryptOFB(Buf, Count);
end;

procedure TBlockCipher.Encrypt(var Buf; Count: Integer);
var
  I, BlockLen, C: Integer;
  PT: PByteArray;
begin
  case Mode of
    cmECB:
      begin
        BlockLen := BlockSize;
        PT := Addr(Buf);
        C := Count div BlockLen;
        for I := 0 to C-1 do begin
          EncryptBlock(PT[0]);
          PT := Addr(PT[BlockLen]);
        end;
      end;
    cmCBC: EncryptCBC(Buf,Count);
    cmCFB: EncryptCFB(Buf,Count);
    cmCTR: EncryptCTR(Buf,Count);
    cmOFB: EncryptOFB(Buf,Count);
    cmABC: EncryptABC(Buf,Count);
  else
    Assert(False,'Cipher mode not supported');
  end;
end;

procedure TBlockCipher.EncryptBlock(var Buf);
begin
  EncryptBlockToDst(Buf,Buf);
end;

procedure TBlockCipher.EncryptBlockToDst(var Dst; const Src);
begin
  Move(Src,Dst,BlockSize);
  EncryptBlock(Dst);
end;

procedure TBlockCipher.EncryptABC(var Buf; Count: Integer);
var
  C, I, BlockLen, BlockLWLen: Integer;
  CT, PT, HT: PByteArray;
begin
  BlockLen := BlockSize;
  BlockLWLen := BlockLen shr 2;
  PT := Addr(Buf);
  CT := Addr(FIV[BlockLen]);
  HT := Addr(FIV[BlockLen*2]);
  C := Count div BlockLen;
  for I := 0 to C - 1 do begin
    // H[i] := P[i] xor H[i-1]
    XORBytesBackup(FIV[0],PT[0],HT[0],BlockLen);
    // C[i] := E(C[i-1] xor H[i]) xor H[i-1]
    XORBytes(CT[0],FIV[0],BlockLen);
    EncryptBlock(CT[0]);
    XORBytesCopy(CT[0],HT[0],PT[0],BlockLen);
    PT := Addr(PT[BlockLen]);
  end;
  C := Count mod BlockLen;
  if C > 0 then begin
    XORBlock(FIV[0],CT[0],BlockLWLen);
    EncryptBlock(FIV[0]);
    for I := 0 to C - 1 do
      PT[I] := PT[I] xor FIV[I];
  end;
end;

procedure TBlockCipher.EncryptCBC(var Buf; Count: Integer);
var
  C, I, BlockLen, BlockLWLen: Integer;
  PT, PrevCT: PByteArray;
begin
  BlockLen := BlockSize;
  BlockLWLen := BlockLen shr 2;
  PT := Addr(Buf);
  C := Count div BlockLen;
  PrevCT := Addr(FIV[0]);
  for I := 0 to C - 1 do begin
    XorBlock(PT[0],PrevCT[0],BlockLWLen);
    EncryptBlock(PT[0]);
    PrevCT := PT;
    PT := Addr(PT[BlockLen]);
  end;
  Move(PrevCT[0],FIV[0],BlockLen);
  C := Count mod BlockLen;
  if C > 0 then begin
    EncryptBlock(FIV[0]);
    for I := 0 to C - 1 do
      PT[I] := PT[I] xor FIV[I];
  end;
end;

procedure TBlockCipher.EncryptCFB(var Buf; Count: Integer);
var
  I, K, BlockLen, C: Integer;
  PT, CT: PByteArray;
begin
  BlockLen := BlockSize;
  PT := Addr(Buf);
  CT := AllocBuffer;
  try
    if ModeRatio = BlockSize then begin
      if FBIndex > 0 then begin
        C := BlockLen;
        if C > Count + FBIndex then
          C := Count + FBIndex;
        XORBytesCopy(PT[0],FIV[FBIndex],FIV[FBIndex],C - FBIndex);
        PT := Addr(PT[C - FBIndex]);
        Count := Count - C + FBIndex;
        FFBIndex := C mod BlockLen;
      end;
      C := Count div BlockLen;
      for I := 0 to C - 1 do begin
        EncryptBlockToDst(CT[0],FIV[0]);
        XORBytesCopy(PT[0],CT[0],FIV[0],BlockLen);
        PT := Addr(PT[BlockLen]);
        FFBIndex := 0;
      end;
      C := Count mod BlockLen;
      if C > 0 then begin
        EncryptBlockToDst(CT[0],FIV[0]);
        XORBytesCopy(PT[0],CT[0],FIV[0],C);
        Move(CT[C],FIV[C],BlockLen - C);
        FFBIndex := C;
      end;
    end else begin
      if FBIndex > 0 then begin
        C := ModeRatio;
        if C > Count + FBIndex then
          C := Count + FBIndex;
        K := BlockLen - ModeRatio + FBIndex;
        XORBytesCopy(PT[0],FIV[K],FIV[K],C - FBIndex);
        PT := Addr(PT[C - FBIndex]);
        Count := Count - C + FBIndex;
        FFBIndex := C mod ModeRatio;
      end;
      C := Count div ModeRatio;
      for I := 0 to C - 1 do begin
        EncryptBlockToDst(CT[0],FIV[0]);
        Move(FIV[ModeRatio],FIV[0],BlockLen - ModeRatio);
        XORBytesCopy(PT[0],CT[0],FIV[BlockLen - ModeRatio],ModeRatio);
        PT := Ptr(Integer(PT) + ModeRatio);
        FFBIndex := 0;
      end;
      C := Count mod ModeRatio;
      if C > 0 then begin
        EncryptBlockToDst(CT[0],FIV[0]);
        Move(FIV[ModeRatio],FIV[0],BlockLen - ModeRatio);
        XORBytesCopy(PT[0],CT[0],FIV[BlockLen - ModeRatio],C);
        Move(CT[C],FIV[BlockLen - ModeRatio + C],ModeRatio - C);
        FFBIndex := C;
      end;
    end;
  finally
    DeallocBuffer(CT);
  end;
end;

procedure TBlockCipher.EncryptCTR(var Buf; Count: Integer);
var
  I, BlockLen, BlockLWLen, C: Integer;
  PT, CT: PByteArray;
begin
  // IMPORTANT: Classes that use this method MUST allocate two blocks for FIV,
  // despite that BlockVectorSize MUST return 1 for CTR classes.
  BlockLen := ModeRatio;
  if BlockLen = 0 then
    BlockLen := BlockSize;
  BlockLWLen := BlockSize shr 2;
  PT := Addr(Buf);
  CT := Addr(FIV[BlockSize]);
  if FBIndex > 0 then begin
    C := BlockLen;
    if C > Count + FBIndex then
      C := Count + FBIndex;
    XORBytes(PT[0],CT[FBIndex],C - FBIndex);
    PT := Addr(PT[C - FBIndex]);
    Count := Count - C + FBIndex;
    FFBIndex := C mod BlockLen;
  end;
  C := Count div BlockLen;
  for I := 0 to C - 1 do begin
    IncBlock(FIV[0],BlockLWLen);
    EncryptBlockToDst(CT[0],FIV[0]);
    XORBytes(PT[0],CT[0],BlockLen);
    PT := Addr(PT[BlockLen]);
    FFBIndex := 0;
  end;
  C := Count mod BlockLen;
  if C > 0 then begin
    IncBlock(FIV[0],BlockLWLen);
    EncryptBlockToDst(CT[0],FIV[0]);
    XORBytes(PT[0],CT[0],C);
    FFBIndex := C;
  end;
end;

procedure TBlockCipher.EncryptOFB(var Buf; Count: Integer);
var
  I, BlockLen, C: Integer;
  PT: PByteArray;
begin
  BlockLen := ModeRatio;
  PT := Addr(Buf);
  if FBIndex > 0 then begin
    C := BlockLen - FBIndex;
    if C > Count then
      C := Count;
    XORBytes(PT[0],FIV[FBIndex],C);
    PT := Addr(PT[C]);
    Count := Count - C;
    FFBIndex := (C + FBIndex) mod BlockLen;
  end;
  C := Count div BlockLen;
  for I := 0 to C - 1 do begin
    EncryptBlock(FIV[0]);
    XORBytes(PT[0],FIV[0],BlockLen);
    PT := Addr(PT[BlockLen]);
    FFBIndex := 0;
  end;
  C := Count mod BlockLen;
  if C > 0 then begin
    EncryptBlock(FIV[0]);
    XORBytes(PT[0],FIV[0],C);
    FFBIndex := C;
  end;
end;

function TBlockCipher.GetBlockVectorSize: Integer;
begin
  case Mode of
    cmCFB: Result := 1;
    cmCBC: Result := 1;
    cmABC: Result := 2;
    cmCTR: Result := 1;
    cmOFB: Result := 1;
  else
    Result := 0;
  end;
end;

function TBlockCipher.GetVector: OctetString;
begin
  SetLength(Result,BlockSize * BlockVectorSize);
  Move(FIV^,Result[1],Length(Result));
end;

procedure TBlockCipher.GetVectorBuf(var IV; Count: Integer);
begin
  if Count > BlockSize * BlockVectorSize then
    Move(FIV^,IV,BlockSize * BlockVectorSize)
  else
    Move(FIV^,IV,Count);
end;

class function TBlockCipher.MaxKeySize: Integer;
begin
  Result := -1;
end;

class function TBlockCipher.MinKeySize: Integer;
begin
  Result := -1;
end;

procedure TBlockCipher.SetModeRatio(Value: Integer);
begin
  if (Value < 1) or (Value >= BlockSize) then
    Value := BlockSize;
  FModeRatio := Value;
end;

procedure TBlockCipher.SetVector(const Value: OctetString);
begin
  if Length(Value) > BlockSize * BlockVectorSize then
    Move(Pointer(Value)^,FIV^,BlockSize * BlockVectorSize)
  else if Length(Value) = BlockSize * BlockVectorSize then
    Move(Pointer(Value)^,FIV^,Length(Value))
  else begin
    ProtectClear(FIV^,BlockSize * BlockVectorSize);
    Move(Pointer(Value)^,FIV^,Length(Value));
  end;
  FFBIndex := 0;
end;

procedure TBlockCipher.SetVectorBuf(const IV; Count: Integer);
begin
  if Count > BlockSize * BlockVectorSize then
    Move(IV,FIV^,BlockSize * BlockVectorSize)
  else if Count = BlockSize * BlockVectorSize then
    Move(IV,FIV^,Count)
  else begin
    ProtectClear(FIV^,BlockSize * BlockVectorSize);
    Move(IV,FIV^,Count);
  end;
  FFBIndex := 0;
end;

{ TCipher }

class function TCipher.Algorithm: TCipherAlg;
begin
  Result := caUnknown;
end;

class function TCipher.AlgorithmName: PAnsiChar;
begin
  Result := nil;
end;

class function TCipher.BlockSize: Integer;
begin
  Result := -1;
end;

constructor TCipher.Create(const AKey; Count: Integer);
begin
  VirtualLock;
  SetUp(AKey, Count);
end;

procedure TCipher.Decrypt(var Buf; Count: Integer);
begin
  DecryptToBuf(Buf,Buf,Count);
end;

procedure TCipher.DecryptPtr(Buf: Pointer; Count: Integer);
begin
  Decrypt(Buf^,Count);
end;

procedure TCipher.DecryptToBuf(const Src; var Dst; Count: Integer);
begin
  Move(Src,Dst,Count);
  Decrypt(Dst,Count);
end;

procedure TCipher.DecryptToPtr(Src, Dst: Pointer; Count: Integer);
begin
  Move(Src^,Dst^,Count);
  Decrypt(Dst^,Count);
end;

destructor TCipher.Destroy;
begin
  CleanUp;
  VirtualUnlock;
  inherited;
end;

procedure TCipher.Encrypt(var Buf; Count: Integer);
begin
  EncryptToBuf(Buf,Buf,Count);
end;

procedure TCipher.EncryptPtr(Buf: Pointer; Count: Integer);
begin
  Encrypt(Buf^,Count);
end;

procedure TCipher.EncryptToBuf(const Src; var Dst; Count: Integer);
begin
  Move(Src,Dst,Count);
  Encrypt(Dst,Count);
end;

procedure TCipher.EncryptToPtr(Src, Dst: Pointer; Count: Integer);
begin
  Move(Src^,Dst^,Count);
  Encrypt(Dst^,Count);
end;

procedure TCipher.GetVectorBuf(var IV; Count: Integer);
var
  Str: AnsiString;
begin
  Str := IVector;
  if Length(Str) < Count then
    Move(Pointer(Str)^,IV,Length(Str))
  else
    Move(Pointer(Str)^,IV,Count);
  ProtectClear(Pointer(Str)^,Length(Str));
end;

procedure TCipher.SetVectorBuf(const IV; Count: Integer);
var
  Str: AnsiString;
begin
  SetLength(Str,Count);
  Move(IV,Pointer(Str)^,Count);
  IVector := Str;
  ProtectClear(Pointer(Str)^,Count);
end;

procedure TCipher.VirtualLock;
begin
  {$IFDEF WIN32}
  Windows.VirtualLock(Self,InstanceSize);
  {$ENDIF}
end;

procedure TCipher.VirtualUnlock;
begin
  {$IFDEF WIN32}
  Windows.VirtualUnlock(Self,InstanceSize);
  {$ENDIF}
end;

end.
