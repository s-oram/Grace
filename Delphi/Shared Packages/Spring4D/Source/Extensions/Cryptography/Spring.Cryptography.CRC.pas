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

unit Spring.Cryptography.CRC;

interface

uses
  Classes,
  SysUtils,
  Spring,
  Spring.Cryptography,
  Spring.Cryptography.Base;

type
  ///	<summary>
  ///	  CRC16 Hash (CheckSum)
  ///	</summary>
  TCRC16 = class(THashAlgorithmBase, ICRC16)
  private
    const fCHashSize = 2 * 8;  // 16 bits
  private
    fCRCValue: Word;
    function GetCrcValue: UInt16;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  public
    property CrcValue: UInt16 read GetCrcValue;
  end;

  ///	<summary>
  ///	  CRC32 Hash (CheckSum)
  ///	</summary>
  TCRC32 = class(THashAlgorithmBase, ICRC32)
  private
    const fCHashSize = 4 * 8;  // 32 bits
  private
    fCRCValue: LongWord;
    function GetCrcValue: UInt32;
  protected
    function GetHashSize: Integer; override;
    procedure HashInit; override;
    procedure HashUpdate(const buffer: Pointer; count: Integer); override;
    function HashFinal: TBuffer; override;
  public
    property CrcValue: UInt32 read GetCrcValue;
  end;

procedure CRC32Init(var crc: LongWord);
procedure CRC32Update(var crc: LongWord; const buf: Pointer; len: LongWord);
procedure CRC32BUpdate(var crc: LongWord; const buf: Pointer; len: LongWord);
function CRC32Final(var crc: LongWord): TBuffer;

procedure CRC16Init(var crc: Word);
procedure CRC16Update(var crc: Word; const buffer: Pointer; len: Integer);
function CRC16Final(var crc: Word): TBuffer;

implementation

uses
  Spring.Cryptography.Utils;

const
  //This polynomial ( 0xEDB88320L) DOES generate the same CRC values as ZMODEM and PKZIP
  crc32_table_b: array[0..255] of LongWord =
  (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419,
    $706AF48F, $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07,
    $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856,
    $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9,
    $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3,
    $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC, $51DE003A,
    $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599,
    $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190,
    $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E,
    $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED,
    $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950,
    $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3,
    $FBD44C65, $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5,
    $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010,
    $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17,
    $2EB40D81, $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6,
    $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615,
    $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344,
    $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A,
    $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1,
    $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C,
    $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE,
    $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31,
    $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C,
    $026D930A, $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B,
    $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1,
    $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278,
    $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7,
    $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66,
    $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8,
    $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B,
    $2D02EF8D
  );


  //This polynomial ($04c11db7) is used at: AUTODIN II, Ethernet, & FDDI
  crc32_table: array[0..255] of LongWord = (
    $00000000, $04c11db7, $09823b6e, $0d4326d9,
    $130476dc, $17c56b6b, $1a864db2, $1e475005,
    $2608edb8, $22c9f00f, $2f8ad6d6, $2b4bcb61,
    $350c9b64, $31cd86d3, $3c8ea00a, $384fbdbd,
    $4c11db70, $48d0c6c7, $4593e01e, $4152fda9,
    $5f15adac, $5bd4b01b, $569796c2, $52568b75,
    $6a1936c8, $6ed82b7f, $639b0da6, $675a1011,
    $791d4014, $7ddc5da3, $709f7b7a, $745e66cd,
    $9823b6e0, $9ce2ab57, $91a18d8e, $95609039,
    $8b27c03c, $8fe6dd8b, $82a5fb52, $8664e6e5,
    $be2b5b58, $baea46ef, $b7a96036, $b3687d81,
    $ad2f2d84, $a9ee3033, $a4ad16ea, $a06c0b5d,
    $d4326d90, $d0f37027, $ddb056fe, $d9714b49,
    $c7361b4c, $c3f706fb, $ceb42022, $ca753d95,
    $f23a8028, $f6fb9d9f, $fbb8bb46, $ff79a6f1,
    $e13ef6f4, $e5ffeb43, $e8bccd9a, $ec7dd02d,
    $34867077, $30476dc0, $3d044b19, $39c556ae,
    $278206ab, $23431b1c, $2e003dc5, $2ac12072,
    $128e9dcf, $164f8078, $1b0ca6a1, $1fcdbb16,
    $018aeb13, $054bf6a4, $0808d07d, $0cc9cdca,
    $7897ab07, $7c56b6b0, $71159069, $75d48dde,
    $6b93dddb, $6f52c06c, $6211e6b5, $66d0fb02,
    $5e9f46bf, $5a5e5b08, $571d7dd1, $53dc6066,
    $4d9b3063, $495a2dd4, $44190b0d, $40d816ba,
    $aca5c697, $a864db20, $a527fdf9, $a1e6e04e,
    $bfa1b04b, $bb60adfc, $b6238b25, $b2e29692,
    $8aad2b2f, $8e6c3698, $832f1041, $87ee0df6,
    $99a95df3, $9d684044, $902b669d, $94ea7b2a,
    $e0b41de7, $e4750050, $e9362689, $edf73b3e,
    $f3b06b3b, $f771768c, $fa325055, $fef34de2,
    $c6bcf05f, $c27dede8, $cf3ecb31, $cbffd686,
    $d5b88683, $d1799b34, $dc3abded, $d8fba05a,
    $690ce0ee, $6dcdfd59, $608edb80, $644fc637,
    $7a089632, $7ec98b85, $738aad5c, $774bb0eb,
    $4f040d56, $4bc510e1, $46863638, $42472b8f,
    $5c007b8a, $58c1663d, $558240e4, $51435d53,
    $251d3b9e, $21dc2629, $2c9f00f0, $285e1d47,
    $36194d42, $32d850f5, $3f9b762c, $3b5a6b9b,
    $0315d626, $07d4cb91, $0a97ed48, $0e56f0ff,
    $1011a0fa, $14d0bd4d, $19939b94, $1d528623,
    $f12f560e, $f5ee4bb9, $f8ad6d60, $fc6c70d7,
    $e22b20d2, $e6ea3d65, $eba91bbc, $ef68060b,
    $d727bbb6, $d3e6a601, $dea580d8, $da649d6f,
    $c423cd6a, $c0e2d0dd, $cda1f604, $c960ebb3,
    $bd3e8d7e, $b9ff90c9, $b4bcb610, $b07daba7,
    $ae3afba2, $aafbe615, $a7b8c0cc, $a379dd7b,
    $9b3660c6, $9ff77d71, $92b45ba8, $9675461f,
    $8832161a, $8cf30bad, $81b02d74, $857130c3,
    $5d8a9099, $594b8d2e, $5408abf7, $50c9b640,
    $4e8ee645, $4a4ffbf2, $470cdd2b, $43cdc09c,
    $7b827d21, $7f436096, $7200464f, $76c15bf8,
    $68860bfd, $6c47164a, $61043093, $65c52d24,
    $119b4be9, $155a565e, $18197087, $1cd86d30,
    $029f3d35, $065e2082, $0b1d065b, $0fdc1bec,
    $3793a651, $3352bbe6, $3e119d3f, $3ad08088,
    $2497d08d, $2056cd3a, $2d15ebe3, $29d4f654,
    $c5a92679, $c1683bce, $cc2b1d17, $c8ea00a0,
    $d6ad50a5, $d26c4d12, $df2f6bcb, $dbee767c,
    $e3a1cbc1, $e760d676, $ea23f0af, $eee2ed18,
    $f0a5bd1d, $f464a0aa, $f9278673, $fde69bc4,
    $89b8fd09, $8d79e0be, $803ac667, $84fbdbd0,
    $9abc8bd5, $9e7d9662, $933eb0bb, $97ffad0c,
    $afb010b1, $ab710d06, $a6322bdf, $a2f33668,
    $bcb4666d, $b8757bda, $b5365d03, $b1f740b4
  );

{$IFDEF CPUX86}
  {$DEFINE USE_ASM}
{$ENDIF}

{$IFDEF USE_ASM}
procedure CRC32Init(var crc: LongWord); assembler;
asm
  mov   dword ptr [eax],0ffffffffh
end;
{$ELSE}
procedure CRC32Init(var crc: LongWord);
begin
  crc := $ffffffff;
end;
{$ENDIF}

{$IFDEF USE_ASM}
procedure CRC32Update(var crc: LongWord; const buf: Pointer; len: LongWord); assembler;
asm
  push  esi
  push  ebx
  push  eax
  mov   eax,dword ptr[eax]
  mov   esi,edx

  or    ecx,ecx
  jz    @@end
@@loop:
  mov   edx,eax
  shl   edx,8
  mov   bl,[esi]
  shr   eax,24
  xor   al,bl
  mov   eax,dword ptr [crc32_table + eax * 4]
  xor   eax,edx

  inc   esi
  dec   ecx
  jnz   @@loop

@@end:
  mov   ecx,eax
  pop   eax
  mov   [eax],ecx

  pop   ebx
  pop   esi
end;
{$ELSE}
procedure CRC32Update(var crc: LongWord; const buf: Pointer; len: LongWord);
var
  p: PByte;
begin
  P := buf;
  while len > 0 do
  begin
    crc := (crc shl 8) xor crc32_table[(crc shr 24) xor p^];
    Inc(p);
    Dec(len);
  end;
end;
{$ENDIF}

{$IFDEF USE_ASM}
procedure CRC32BUpdate(var crc: LongWord; const buf: Pointer; len: LongWord); assembler;
asm
  push  esi
  push  ebx
  push  eax
  mov   eax,dword ptr [eax]
  mov   esi,edx
  xor   ebx,ebx

  or    ecx,ecx
  jz    @@end
@@loop:
  mov   edx,eax
  shr   edx,8
  and   edx,00ffffffh
  mov   bl,[esi]
  xor   eax,ebx
  and   eax,0ffh
  mov   eax,dword ptr [crc32_table_b + eax * 4]
  xor   eax,edx

  inc   esi
  dec   ecx
  jnz   @@loop

@@end:
  mov   ecx,eax
  pop   eax
  mov   [eax],ecx

  pop   ebx
  pop   esi
end;
{$ELSE}
procedure CRC32BUpdate(var crc: LongWord; const buf: Pointer; len: LongWord);
var
  p: PByte;
begin
  P := buf;
  while len > 0 do
  begin
    crc := ((crc shr 8) and $00FFFFFF) xor crc32_table_b[(crc xor p^) and $ff];
    Inc(p);
    Dec(len);
  end;
end;
{$ENDIF}

function CRC32Final(var crc: LongWord): TBuffer;
begin
  crc := not crc;
  with LongRec(crc) do
  begin
    Result := TBuffer.Create([Bytes[3], Bytes[2], Bytes[1], Bytes[0]]);
  end;
end;

const
  Crc16_Table: array [0 .. 255] of Word = (
    $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241, $C601, $06C0,
    $0780, $C741, $0500, $C5C1, $C481, $0440, $CC01, $0CC0, $0D80, $CD41,
    $0F00, $CFC1, $CE81, $0E40, $0A00, $CAC1, $CB81, $0B40, $C901, $09C0,
    $0880, $C841, $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
    $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41, $1400, $D4C1,
    $D581, $1540, $D701, $17C0, $1680, $D641, $D201, $12C0, $1380, $D341,
    $1100, $D1C1, $D081, $1040, $F001, $30C0, $3180, $F141, $3300, $F3C1,
    $F281, $3240, $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
    $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41, $FA01, $3AC0,
    $3B80, $FB41, $3900, $F9C1, $F881, $3840, $2800, $E8C1, $E981, $2940,
    $EB01, $2BC0, $2A80, $EA41, $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1,
    $EC81, $2C40, $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
    $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041, $A001, $60C0,
    $6180, $A141, $6300, $A3C1, $A281, $6240, $6600, $A6C1, $A781, $6740,
    $A501, $65C0, $6480, $A441, $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0,
    $6E80, $AE41, $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
    $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41, $BE01, $7EC0,
    $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40, $B401, $74C0, $7580, $B541,
    $7700, $B7C1, $B681, $7640, $7200, $B2C1, $B381, $7340, $B101, $71C0,
    $7080, $B041, $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
    $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440, $9C01, $5CC0,
    $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40, $5A00, $9AC1, $9B81, $5B40,
    $9901, $59C0, $5880, $9841, $8801, $48C0, $4980, $8941, $4B00, $8BC1,
    $8A81, $4A40, $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
    $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641, $8201, $42C0,
    $4380, $8341, $4100, $81C1, $8081, $4040
  );

procedure CRC16Init(var crc: Word);
begin
  crc := $0000;
end;

procedure CRC16Update(var crc: Word; const buffer: Pointer; len: Integer);
var
  p: pByte;
  i: Integer;
begin
  p := buffer;
  for i := 1 to len do
  begin
    crc := Hi(crc) xor Crc16_Table[p^ xor Lo(crc)];
    Inc(p);
  end;
end;

function CRC16Final(var crc: Word): TBuffer;
begin
  Result := TBuffer.Create([Hi(crc), Lo(crc)]);
end;


{$IFDEF SUPPORTS_REGION}{$REGION 'TCRC16'}{$ENDIF}

function TCRC16.GetCrcValue: UInt16;
begin
  Result := fCRCValue;
end;

function TCRC16.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TCRC16.HashInit;
begin
  CRC16Init(fCRCValue);
end;

procedure TCRC16.HashUpdate(const buffer: Pointer; count: Integer);
begin
  CRC16Update(fCRCValue, buffer, count);
end;

function TCRC16.HashFinal: TBuffer;
begin
  Result := CRC16Final(fCRCValue);
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{$IFDEF SUPPORTS_REGION}{$REGION 'TCRC32'}{$ENDIF}

function TCRC32.GetCrcValue: UInt32;
begin
  Result := fCRCValue;
end;

function TCRC32.GetHashSize: Integer;
begin
  Result := fCHashSize;
end;

procedure TCRC32.HashInit;
begin
  CRC32Init(fCRCValue);
end;

procedure TCRC32.HashUpdate(const buffer: Pointer; count: Integer);
begin
  CRC32BUpdate(fCRCValue, buffer, count);
end;

function TCRC32.HashFinal: TBuffer;
begin
  Result := CRC32Final(fCRCValue);
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}

end.
