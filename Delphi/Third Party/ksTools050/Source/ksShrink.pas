{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksShrink;

interface

uses SysUtils, Classes, ksUtils, ksClasses;

procedure Shrink(GetByte: TksGetByte; PutBits: TksPutBits; Count: LongWord);
procedure Unshrink(GetBits: TksGetBits; PutByte: TksPutByte; Count: LongWord);

implementation

{ History

*********************** from PKWARE ************************

Shrinking is a Dynamic Ziv-Lempel-Welch compression algorithm
with partial clearing.  The initial code size is 9 bits, and
the maximum code size is 13 bits.  Shrinking differs from
conventional Dynamic Ziv-Lempel-Welch implementations in several
respects:

1)  The code size is controlled by the compressor, and is not
    automatically increased when codes larger than the current
    code size are created (but not necessarily used).  When
    the decompressor encounters the code sequence 256
    (decimal) followed by 1, it should increase the code size
    read from the input stream to the next bit size.  No
    blocking of the codes is performed, so the next code at
    the increased size should be read from the input stream
    immediately after where the previous code at the smaller
    bit size was read.  Again, the decompressor should not
    increase the code size used until the sequence 256,1 is
    encountered.

2)  When the table becomes full, total clearing is not
    performed.  Rather, when the compresser emits the code
    sequence 256,2 (decimal), the decompressor should clear
    all leaf nodes from the Ziv-Lempel tree, and continue to
    use the current code size.  The nodes that are cleared
    from the Ziv-Lempel tree are then re-used, with the lowest
    code value re-used first, and the highest code value
    re-used last.  The compressor can emit the sequence 256,2
    at any time.

************************* from InfoZip ****************************

   Shrink.Pas version 1.2 by R. P. Byrne, 1989 (in Pascal and assembler).
   We here heartily acknowledge R. P. Byrne's contribution to this project.
   The existence of this program really triggered our efforts to write a
   portable Unix zip.  What little remains of Byrne's program lies in this
   source file, and those remnants are mostly in the variable and routine
   names, since it has been translated and extensively modified and rewritten.
   Stolen, translated into C, and modified by Mark Adler, 11 October 1990.
   Severely modified again by Mark Adler, 11 July 1991, to remove the
   unnecessary FreeList and ClearTable arrays, and to replace the recursive
   Prune() routine with a non-recursive method.
   As Stravinsky once said: "Mediocre composers plagiarize.
                             Great composers steal."

*******************************************************************

   Translated back into Pascal (Delphi)
     & modified by Sergey Kasandrov, October 2001
   Minor modications (advanced records, etc) April 2010

}

const
  InitCodeSize = 9;         // Initial Code Size = 9 bits
  MaxCodeSize = 13;         // Max Code Size = 13 bits
  EscapeCode = 256;         // Special Code
  FirstEntry = 257;         // First String Code
  IncSizeCode = 1;          // Code for a jump in code size
  ClearCode   = 2;          // Code for code table has been cleared

const
  ShrinkTableSize = 8 * 1024;

{ Shrink Encoder }

type
  PShrinkCodeRec = ^TShrinkCodeRec;
  TShrinkCodeRec = record
    Child: SmallInt;
    Sibling: SmallInt;
    Suffix: Byte;
  end;

  TShrinkCodeTable = array[0..ShrinkTableSize - 1] of TShrinkCodeRec;

type
  TEncoder = record
    FCodeTable: TShrinkCodeTable;
    FNextFree: SmallInt;
    procedure InitCodeTable;
    procedure ClearTable;
    procedure AddCode(PrefixCode: SmallInt; SuffixCode: Byte);
    procedure Execute(GetByte: TksGetByte; PutBits: TksPutBits;
                      Count: LongWord);
  end;

procedure TEncoder.InitCodeTable;
var
  I: Integer;               // counter for table entries
  P: PShrinkCodeRec;        // pointer to current table entry

begin
// Initialize parent symbols
  P:= @FCodeTable;
  for I:= 0 to 255 do begin
    P.Child:= -1;
    P.Suffix:= Byte(I);
    Inc(P);
  end;
// Build free list
  FNextFree:= FirstEntry;
  P:= @FCodeTable[FirstEntry];
  I:= FirstEntry;
  repeat
    P.Child:= I + 1;
    Inc(P);
    Inc(I);
  until I = ShrinkTableSize - 1;
  P.Child:= -1;
end;

procedure TEncoder.ClearTable;
var
  N: Integer;
  P: PShrinkCodeRec;
  Q: ^SmallInt;
begin
// Mark leaf nodes
  P:= @FCodeTable[ShrinkTableSize-1];
  N:= ShrinkTableSize - FirstEntry;
  repeat
    if P.Child = -1 then P.Child:= -2;
    Dec(P);
    Dec(N);
  until N = 0;
// Shake leaves from tree
  P:= @FCodeTable;
  N:= 256;
  repeat
    Q:= @P.Child;
    while (Q^ <> -1) and (FCodeTable[Q^].Child = - 2) do
      Q^:= FCodeTable[Q^].Sibling;
    Inc(P);
    Dec(N);
  until N = 0;
  P:= @FCodeTable[FirstEntry];
  N:= ShrinkTableSize - FirstEntry;
  repeat
    if (P.Child <> -2) then begin
      Q:= @P.Child;
      while (Q^ <> -1) and (FCodeTable[Q^].Child = -2) do
        Q^:= FCodeTable[Q^].Sibling;
      Q:= @P.Sibling;
      while (Q^ <> -1) and (FCodeTable[Q^].Child = -2) do
        Q^:= FCodeTable[Q^].Sibling;
    end;
    Inc(P);
    Dec(N);
  until N = 0;
// Build the list of free table entries
  FNextFree:= -1;
  P:= @FCodeTable[ShrinkTableSize-1];
  N:= ShrinkTableSize - FirstEntry;
  repeat
    if P.Child = -2 then begin
      P.Child:= FNextFree;
      FNextFree:= N + FirstEntry - 1;
    end;
    Dec(P);
    Dec(N);
  until N = 0;
end;

procedure TEncoder.AddCode(PrefixCode: SmallInt; SuffixCode: Byte);
var
  I: Integer;

begin
  I:= FNextFree;
  if I <> -1 then begin
    FNextFree:= FCodeTable[I].Child;
    FCodeTable[I].Child:= -1;
    FCodeTable[I].Sibling:= -1;
    FCodeTable[I].Suffix:= SuffixCode;
    if FCodeTable[PrefixCode].Child = -1 then
      FCodeTable[PrefixCode].Child:= I
    else begin
      PrefixCode:= FCodeTable[PrefixCode].Child;
      while (FCodeTable[PrefixCode].Sibling <> -1) do
        PrefixCode:= FCodeTable[PrefixCode].Sibling;
      FCodeTable[PrefixCode].Sibling:= I;
    end;
  end;
end;

procedure TEncoder.Execute(GetByte: TksGetByte; PutBits: TksPutBits;
                           Count: LongWord);
var
  N: LongWord;
  Index: Integer;
  LastCode, MaxCode: SmallInt;
  CodeSize: SmallInt;
  CurrCode: Byte;

begin
  InitCodeTable;
  CodeSize:= InitCodeSize;
  MaxCode:= (1 shl InitCodeSize) - 1;
  if (Count = 0) then Exit;
  LastCode:= GetByte;
  N:= 1;
  while N < Count do begin
    CurrCode:= GetByte;
    Inc(N);
    Index:= FCodeTable[LastCode].Child;
    while (Index <> -1) and (FCodeTable[Index].Suffix <> CurrCode) do
      Index:= FCodeTable[Index].Sibling;
    if (Index <> -1)
// If LastCode:CurrCode pair is found in the code table,
//   then LastCode to the entry where the pair is located
      then LastCode:= Index
    else begin
// Not in table
      PutBits(LastCode, CodeSize);
      AddCode(LastCode, CurrCode);     // Attempt to add to code table
      LastCode:= CurrCode;             // Reset lastcode for new char
      if (FNextFree >= MaxCode) and (CodeSize < MaxCodeSize) then begin
// Time to increase the code size and change the max. code
        PutBits(EscapeCode, CodeSize);
        PutBits(IncSizeCode, CodeSize);
        Inc(CodeSize);
        MaxCode:= (1 shl CodeSize) - 1;
//        MaxCode = MaxCode shl 1;
      end;
      while (FNextFree = -1) and (N < Count) do begin
// Ok, lets clear the code table (adaptive reset)
        PutBits(LastCode, CodeSize);
        PutBits(EscapeCode, CodeSize);
        PutBits(ClearCode, CodeSize);
        ClearTable;
        CurrCode:= GetByte;
        AddCode(LastCode, CurrCode);
        Inc(N);
        LastCode:= CurrCode;
      end;
    end;
  end;
  PutBits(LastCode, CodeSize);   // Write last prefix code
end;

procedure Shrink(GetByte: TksGetByte; PutBits: TksPutBits; Count: LongWord);
var
  Coder: TEncoder;

begin
  Coder.Execute(GetByte, PutBits, Count);
end;

{ Shrink Decoder }

procedure Unshrink(GetBits: TksGetBits; PutByte: TksPutByte; Count: LongWord);
const
  MaxCodeMax = 8192;        // = 1 shl MaxCodeSize
  Unused = -1;

var
  PrefixCode: packed array[0..8 * 1024 - 1] of SmallInt;
  SuffixChar: packed array[0..8 * 1024 - 1] of Byte;
  Stack: packed array[0..8 * 1024 - 1] of Byte;
  CodeSize: SmallInt;
{  MaxCode : SmallInt;}
  NextFree: SmallInt;
  BaseChar: SmallInt;
  NewCode: SmallInt;
  OldCode: SmallInt;
  SaveCode: SmallInt;
  M, R : SmallInt;
  I: Integer;
  N: LongWord;
  StackIndex: Integer;

begin
  CodeSize:= InitCodeSize;
  NextFree:= FirstEntry;
  FillChar(PrefixCode, SizeOf(PrefixCode), $FF); // All entries unused
  FillChar(SuffixChar, SizeOf(SuffixChar), 0);

  for NewCode:= 255 downto 0 do begin
    PrefixCode[NewCode]:= 0;
    SuffixChar[NewCode]:= NewCode;
  end;

  OldCode:= GetBits(CodeSize);
  BaseChar:= OldCode;
  PutByte(BaseChar);

  StackIndex := 0;
  N:= 1;
  while N < Count do begin
    NewCode:= GetBits(CodeSize);
    if (NewCode = EscapeCode) then begin
      case GetBits(CodeSize) of
        1: Inc(CodeSize);
        2: begin
  {mark all nodes as potentially unused}
             for I:= FirstEntry to NextFree - 1 do
               Word(PrefixCode[I]):= Word(PrefixCode[I]) or $8000;
  {unmark those used by other nodes}
             for M:= FirstEntry to NextFree - 1 do begin
  {reference to another node}
               R:= PrefixCode[M] and $7FFF;
  {flag node as referenced}
               if R >= FirstEntry then
                 PrefixCode[R]:= PrefixCode[R] and $7FFF;
             end;
  {clear the ones that are still marked}
             for I:= FirstEntry to NextFree - 1 do
               if PrefixCode[I] < 0 then PrefixCode[I]:= -1;
  {recalculate NextFree}
             NextFree:= FirstEntry;
             while (NextFree < MaxCodeMax) and
               (PrefixCode[NextFree] <> -1) do Inc(NextFree);
        end;
      end;
    end
    else begin
  {save current code}
      SaveCode:= NewCode;
  {special case}
      if PrefixCode[NewCode] = Unused then begin
        Stack[StackIndex]:= BaseChar;
        Inc(StackIndex);
        NewCode:= OldCode;
      end;
  {generate output characters in reverse order}
      while (NewCode >= FirstEntry) do begin
        if PrefixCode[NewCode] = Unused then begin
          Stack[StackIndex]:= BaseChar;
          Inc(StackIndex);
          NewCode:= OldCode;
        end
        else begin
          Stack[StackIndex]:= SuffixChar[NewCode];
          Inc(StackIndex);
          NewCode:= PrefixCode[NewCode];
        end;
      end;
      BaseChar:= SuffixChar[NewCode];
      PutByte(BaseChar);
      Inc(N);
  {put them out in forward order}
      while (StackIndex > 0) do begin
        Dec(StackIndex);
        PutByte(Stack[StackIndex]);
        Inc(N);
      end;
  {add new entry to tables}
      NewCode:= NextFree;
      if NewCode < MaxCodeMax then begin
        PrefixCode[NewCode]:= OldCode;
        SuffixChar[NewCode]:= BaseChar;
        while (NextFree < MaxCodeMax) and
              (PrefixCode[NextFree] <> Unused) do
          Inc(NextFree);
      end;
  {remember previous code}
      OldCode:= SaveCode;
    end;
  end;
end;

end.
