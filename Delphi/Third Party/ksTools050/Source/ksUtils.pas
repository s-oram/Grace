{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksUtils;

interface

uses SysUtils, Classes;

{ CRC32Table used to calculate CRC32 [function CRC32OfByte] }
const CRC32Table: array[0..255] of LongWord = (
  $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535,
  $9e6495a3, $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd,
  $e7b82d07, $90bf1d91, $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d,
  $6ddde4eb, $f4d4b551, $83d385c7, $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
  $14015c4f, $63066cd9, $fa0f3d63, $8d080df5, $3b6e20c8, $4c69105e, $d56041e4,
  $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b, $35b5a8fa, $42b2986c,
  $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59, $26d930ac,
  $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
  $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab,
  $b6662d3d, $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f,
  $9fbfe4a5, $e8b8d433, $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb,
  $086d3d2d, $91646c97, $e6635c01, $6b6b51f4, $1c6c6162, $856530d8, $f262004e,
  $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457, $65b0d9c6, $12b7e950, $8bbeb8ea,
  $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65, $4db26158, $3ab551ce,
  $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb, $4369e96a,
  $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
  $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409,
  $ce61e49f, $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81,
  $b7bd5c3b, $c0ba6cad, $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739,
  $9dd277af, $04db2615, $73dc1683, $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8,
  $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1, $f00f9344, $8708a3d2, $1e01f268,
  $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7, $fed41b76, $89d32be0,
  $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5, $d6d6a3e8,
  $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
  $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef,
  $4669be79, $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703,
  $220216b9, $5505262f, $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7,
  $b5d0cf31, $2cd99e8b, $5bdeae1d, $9b64c2b0, $ec63f226, $756aa39c, $026d930a,
  $9c0906a9, $eb0e363f, $72076785, $05005713, $95bf4a82, $e2b87a14, $7bb12bae,
  $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21, $86d3d2d4, $f1d4e242,
  $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777, $88085ae6,
  $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
  $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d,
  $3e6e77db, $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5,
  $47b2cf7f, $30b5ffe9, $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605,
  $cdd70693, $54de5729, $23d967bf, $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94,
  $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
  );

{ SwapTable swaps bits in a byte }
const SwapTable: array[Byte] of Byte = (
 $00, $80, $40, $C0, $20, $A0, $60, $E0, $10, $90, $50, $D0, $30, $B0, $70, $F0,
 $08, $88, $48, $C8, $28, $A8, $68, $E8, $18, $98, $58, $D8, $38, $B8, $78, $F8,
 $04, $84, $44, $C4, $24, $A4, $64, $E4, $14, $94, $54, $D4, $34, $B4, $74, $F4,
 $0C, $8C, $4C, $CC, $2C, $AC, $6C, $EC, $1C, $9C, $5C, $DC, $3C, $BC, $7C, $FC,
 $02, $82, $42, $C2, $22, $A2, $62, $E2, $12, $92, $52, $D2, $32, $B2, $72, $F2,
 $0A, $8A, $4A, $CA, $2A, $AA, $6A, $EA, $1A, $9A, $5A, $DA, $3A, $BA, $7A, $FA,
 $06, $86, $46, $C6, $26, $A6, $66, $E6, $16, $96, $56, $D6, $36, $B6, $76, $F6,
 $0E, $8E, $4E, $CE, $2E, $AE, $6E, $EE, $1E, $9E, $5E, $DE, $3E, $BE, $7E, $FE,
 $01, $81, $41, $C1, $21, $A1, $61, $E1, $11, $91, $51, $D1, $31, $B1, $71, $F1,
 $09, $89, $49, $C9, $29, $A9, $69, $E9, $19, $99, $59, $D9, $39, $B9, $79, $F9,
 $05, $85, $45, $C5, $25, $A5, $65, $E5, $15, $95, $55, $D5, $35, $B5, $75, $F5,
 $0D, $8D, $4D, $CD, $2D, $AD, $6D, $ED, $1D, $9D, $5D, $DD, $3D, $BD, $7D, $FD,
 $03, $83, $43, $C3, $23, $A3, $63, $E3, $13, $93, $53, $D3, $33, $B3, $73, $F3,
 $0B, $8B, $4B, $CB, $2B, $AB, $6B, $EB, $1B, $9B, $5B, $DB, $3B, $BB, $7B, $FB,
 $07, $87, $47, $C7, $27, $A7, $67, $E7, $17, $97, $57, $D7, $37, $B7, $77, $F7,
 $0F, $8F, $4F, $CF, $2F, $AF, $6F, $EF, $1F, $9F, $5F, $DF, $3F, $BF, $7F, $FF
 );

{ Bit Masks for 32-bit values }
const
  BitMask: array[0..32] of LongWord = (
    $00000000, $00000001, $00000003, $00000007, $0000000F, $0000001F,
    $0000003F, $0000007F, $000000FF, $000001FF, $000003FF, $000007FF,
    $00000FFF, $00001FFF, $00003FFF, $00007FFF, $0000FFFF, $0001FFFF,
    $0003FFFF, $0007FFFF, $000FFFFF, $001FFFFF, $003FFFFF, $007FFFFF,
    $00FFFFFF, $01FFFFFF, $03FFFFFF, $07FFFFFF, $0FFFFFFF, $1FFFFFFF,
    $3FFFFFFF, $7FFFFFFF, $FFFFFFFF);

type
  TksGetByte = function: Byte of object;
  TksPutByte = procedure(Value: Byte) of object;
  TksGetBits = function(BitCount: Integer): LongWord of object;
  TksPutBits = procedure(Value: LongWord; BitCount: Integer) of object;

{ CompareBytes performs a binary compare of Len bytes of memory referenced
  by P1 to that of P2. The return value is less than 0 if P1^[N] < P2^[N],
  0 if all Len bytes are equal, or greater then 0 if P1^[N] > P2^[N]. }
function CompareBytes(P1, P2: Pointer; Len: Cardinal): Integer;

{ EqualBytesLen returns the number of identical bytes
  referenced by P1 and P2. }
function EqualBytesLen(P1, P2: Pointer; MaxLen: Integer): Integer;

{ ScanBytes perfoms a binary scan of BufSize bytes referenced by Buf for DataSize
  bytes referenced by Data. Returns number of bytes scanned to find Data,
  -1 if Data not found. }
function ScanBytes(Buf, Data: Pointer; BufSize, DataSize: Integer): Integer;

{ FindByte perfoms a binary scan of BufSize bytes referenced by Buf
  for byte B. Returns pointer to the first B entry, nil if B not found. }
function FindByte(Buf: Pointer; B: Byte; BufSize: Integer): Pointer;

{ FindByte perfoms a binary scan of BufSize bytes referenced by Buf
  for byte B. Scanning begins from Buf[BufSize-1] and goes backwards.
  Returns pointer to the first B entry, nil if B not found. }
function FindByteBack(Buf: Pointer; B: Byte; BufSize: Integer): Pointer;

{ Percentage returns the ratio of V1 to V2 * 100. }
function Percentage(V1, V2: Integer): Integer;

{ CRC32OfByte returns updated CRC32 value after handling AByte }
function CRC32OfByte(AByte: Byte; CurCrc: LongWord): LongWord;

{ CRC32OfData returns updated CRC32 value after handling DataSize bytes }
function Crc32OfData(var Data; DataSize: LongWord;
  CurCrc: LongWord = $FFFFFFFF): LongWord;

{ SwapBits swaps Len<=16 bits in Code }
function SwapBits16(Code, Len: Word): Word;
function SwapBits32(Code, Len: LongWord): LongWord;

{ Rol returns the Value parameter rotated left by the number of bit positions
  specified in the Shift parameter }
function Rol32(Value: LongWord; Shift: Byte = 1): LongWord;

{ Ror returns the Value parameter rotated right by the number of bit positions
  specified in the Shift parameter }
function Ror32(Value: LongWord; Shift: Byte = 1): LongWord;

implementation

function CompareBytes(P1, P2: Pointer; Len: Cardinal): Integer;
asm
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,EDX
        MOV     ESI,EAX
        XOR     EAX,EAX
        XOR     EDX,EDX
        REPE    CMPSB
        JE      @@1             // because ECX can be zero
        MOV     AL,[ESI-1]
        MOV     DL,[EDI-1]
        SUB     EAX,EDX
@@1:    POP     ESI
        POP     EDI
end;

function EqualBytesLen(P1, P2: Pointer; MaxLen: Integer): Integer;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        XOR     EAX,EAX         // set Z flag cause MaxLen can be zero
        MOV     EAX,ECX
        REPE    CMPSB
        JE      @@2
        DEC     EAX
@@2:    SUB     EAX,ECX
        POP     EDI
        POP     ESI
end;

function ScanBytes(Buf,                       // Buffer to scan
                   Data: Pointer;             // Data to find
                   BufSize,                   // Size of Buffer
                   DataSize: Integer          // Size of Data
                   ): Integer;                // Buffer Position if Data found;
                                              //   -1 if Data not found.
asm
{     ->EAX     Buf                             }
{       EDX     Data                            }
{       ECX     BufSize                         }
{     <-EAX     Position of Data in Buf or -1   }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EDX         { Data }
        MOV     EDI,EAX         { Buf  }

        PUSH    EDI             { remember Buf to calculate index       }

        MOV     EDX,DataSize
        DEC     EDX             { EDX = DataSize - 1                    }
        JS      @@fail          { < 0 ? return -1                       }

        MOV     AL,[ESI]        { AL = first byte of Data               }
        INC     ESI             { Point ESI to 2'nd byte of Data        }

        SUB     ECX,EDX         { #positions in Buf to look at          }
                                { = BufSize - DataSize + 1              }
        JLE     @@fail
@@loop:
        REPNE   SCASB
        JNE     @@fail
        MOV     EBX,ECX         { save outer loop counter               }
        PUSH    ESI             { save outer loop Data pointer          }
        PUSH    EDI             { save outer loop Buf pointer           }

        MOV     ECX,EDX
        REPE    CMPSB
        POP     EDI             { restore outer loop Buf pointer        }
        POP     ESI             { restore outer loop Data pointer       }
        JE      @@found
        MOV     ECX,EBX         { restore outer loop counter            }
        JMP     @@loop

@@fail:
        POP     EDX             { get rid of saved Buf origin           }
        XOR     EAX,EAX
        JMP     @@exit

@@found:
        POP     EDX             { restore Buf origin                    }
        MOV     EAX,EDI         { EDI points of byte after match        }
        SUB     EAX,EDX         { the difference is the index + 1       }
@@exit:
        DEC     EAX
        POP     EDI
        POP     ESI
        POP     EBX
end;

function FindByte(Buf: Pointer;          // Buffer to scan
                  B: Byte;               // Byte to find
                  BufSize: Integer       // Size of Buffer
                  ): Pointer;            // Pointer to B if byte found
                                         //   nil if byte not found
asm
{     ->EAX     Buf                             }
{        DL     B                               }
{       ECX     BufSize                         }
{     <-EAX     Position of B in Buf or nil     }

        PUSH    EDI
        MOV     EDI,EAX         { Buf }
        OR      EAX,EAX         { clear Z flag (needed if BufSize = 0) }
        MOV     AL,DL           { B }
        REPNE   SCASB
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        DEC     EAX
@@1:    POP     EDI
end;

function FindByteBack(Buf: Pointer;          // Buffer to scan
                      B: Byte;               // Byte to find
                      BufSize: Integer       // Size of Buffer
                      ): Pointer;            // Pointer to B if byte found
                                             //   nil if byte not found
asm
{     ->EAX     Buf                             }
{        DL     B                               }
{       ECX     BufSize                         }
{     <-EAX     Position of B in Buf or nil     }

        PUSH    EDI
        ADD     EAX,ECX         { Buf + BufSize }
        DEC     EAX             { EAX points to last byte in Buf }
        MOV     EDI,EAX
//        OR      EAX,EAX         { clear Z flag (needed if BufSize = 0) }
        MOV     AL,DL           { B }
        STD
        REPNE   SCASB
        CLD
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        INC     EAX
@@1:    POP     EDI
end;

function Percentage(V1, V2: LongInt): LongInt;
begin
  if V2 > $FA0000 then begin
    V1:= (V1 + $80) shr 8;
    V2:= (V2 + $80) shr 8;
  end;
  if V2 <= 0 then Result := 0
  else if V1 >= V2 then Result:= 100
  else Result:= (V1 * 100) div V2;
end;

function CRC32OfByte(AByte: Byte; CurCrc: LongWord): LongWord;
begin
  Result:= Crc32Table[Byte(CurCrc xor AByte)] xor LongWord(CurCrc shr 8);
end;

function Crc32OfData(var Data; DataSize, CurCrc: LongWord): LongWord;
type
  CRCByteArray = array[0..Pred(High(LongInt))] of Byte;

var
  I: Integer;

begin
  Result:= CurCrc;
  for I:= 0 to (DataSize-1) do
    Result:= Crc32Table[Byte(Result xor CRCByteArray(Data)[I])] xor
      LongWord(Result shr 8);
end;

{
function SwapBits16(Code, Len: Word): Word;
asm
        PUSH    EBX
        MOVZX   ECX,AL          // prepare for table lookup
        LEA     EBX,SwapTable   // get address to table
        MOV     DH,AH           // store high byte for later
        XOR     EAX,EAX
        CMP     DL,8            // do we need high byte?
        MOV     AL,[EBX+ECX]    // table lookup for low byte
        MOV     CL,8            // prepare for fixup shift
        JLE     @@1             // jump if not

        MOV     CL,DH           // prepare high byte for table lookup
        MOV     AH,AL           // reverse bytes
        MOV     AL,[EBX+ECX]    // table lookup for high (now low) byte
        MOV     CL,16           // prepare for fixup shift

@@1:
        SUB     CL,DL           // how much to shift?
        SHR     EAX,CL
        POP     EBX
end;
}

function SwapBits16(Code, Len: Word): Word;
asm
        PUSH    EBX
        MOVZX   ECX,AL          // prepare for table lookup
        LEA     EBX,SwapTable   // get address to table
        MOV     AL,AH           // store high byte for later
        MOV     AH,[EBX+ECX]    // table lookup for low byte
        MOV     CL,AL           // prepare high byte for table lookup
        MOV     AL,[EBX+ECX]    // table lookup for high byte
        MOV     CL,16           // prepare for fixup shift
        SUB     CL,DL           // how much to shift?
        SHR     AX,CL
        POP     EBX
end;

function SwapBits32(Code, Len: LongWord): LongWord;
asm
        PUSH    EBX
        MOVZX   ECX,AL          // prepare for table lookup
        LEA     EBX,SwapTable   // get address to table
        MOV     AL,AH
        MOV     AH,[EBX+ECX]    // table lookup for low byte
        MOV     CL,AL           // prepare high byte for table lookup
        MOV     AL,[EBX+ECX]    // table lookup for next byte
        MOV     CL,16
        ROL     EAX,CL
        MOV     CL,AL
        MOV     AL,AH
        MOV     AH,[EBX+ECX]
        MOV     CL,AL
        MOV     AL,[EBX+ECX]
        MOV     CL,32           // prepare for fixup shift
        SUB     CL,DL           // how much to shift?
        SHR     EAX,CL
        POP     EBX
end;

function Rol32(Value: LongWord; Shift: Byte): LongWord;
asm
        MOV     CL,DL
        ROL     EAX,CL
end;

function Ror32(Value: LongWord; Shift: Byte): LongWord;
asm
        MOV     CL,DL
        ROR     EAX,CL
end;

end.

