{ unit sdStringTable

  Author: Nils Haeck M.Sc. (n.haeck@simdesign.nl)
  Original Date: 28 May 2007
  Version: 1.1
  Copyright (c) 2007 - 2010 Simdesign BV

  It is NOT allowed under ANY circumstances to publish or copy this code
  without accepting the license conditions in accompanying LICENSE.txt
  first!

  This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
  ANY KIND, either express or implied.

  Please visit http://www.simdesign.nl/xml.html for more information.
}
unit sdStringTable;

interface

{$i NativeXml.inc}

uses
  Classes, SysUtils, Contnrs;

type

  // A record describing a string by its first position and length (Count)
  TsdStringRec = record
    First: Pbyte;
    Count: integer;
  end;

  // A string reference item used in string reference lists (do not use directly)
  TsdRefString = class
  private
    FID: integer;
    FFrequency: integer;
    FFirst: Pbyte;
    FCharCount: integer;
  protected
    procedure SetString(const SR: TsdStringRec);
    function CompareToSR(const SR: TsdStringRec): integer;
    function StringRec: TsdStringRec;
  public
    destructor Destroy; override;
    function AsString: UTF8String;
    property CharCount: integer read FCharCount;
    property Frequency: integer read FFrequency;
  end;

  // A list of string reference items (do not use directly)
  TsdRefStringList = class(TObjectList)
  private
    function GetItems(Index: integer): TsdRefString;
  protected
    // Assumes list is sorted by StringID
    function IndexOfID(AID: integer; var Index: integer): boolean;
    // Assumes list is sorted by string rec
    function IndexOfSR(const AStringRec: TsdStringRec; var Index: integer): boolean;
  public
    property Items[Index: integer]: TsdRefString read GetItems; default;
  end;

  // A string table, holding a collection of unique strings, sorted in 2 ways
  // for fast access. Strings can be added with AddString or AddStringRec,
  // and should be updated with SetString. When a string is added or updated,
  // an ID is returned which the application can use to retrieve the string,
  // using GetString.
  TsdStringTable = class(TPersistent)
  private
    FByID: TsdRefStringList;
    FBySR: TsdRefStringList;
  protected
    procedure DecFrequency(AItem: TsdRefString; ByIdIndex: integer);
    function NextUniqueID: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    // Add a new string rec, return fresh ID or ID of existing item, and increase
    // the existing item's ref count
    function AddStringRec(const SR: TsdStringRec): integer;
    // Add a new string S to the table, the function returns its ID.
    function AddString(const S: UTF8String): integer;
    // Get the refstring by ID
    function ById(index: integer): TsdRefString;
    // Delete refstring by ID
    procedure Delete(ByIdIndex: integer);
    // determine if the stringrec exists
    function ExistStringRec(const SR: TsdStringRec): boolean;
    // Get the string of refstring with ID
    function GetString(ID: integer): UTF8String;
    // Set the string value of refstring with ID.
    procedure SetString(var ID: integer; const S: UTF8String);
    // Number of refstrings
    function StringCount: integer;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(S: TStream);
  end;

{utility functions}

// convert a string into a string rec
function sdStringToSR(const S: Utf8String): TsdStringRec;

// convert a string rec into a string
function sdSRToString(const SR: TsdStringRec): Utf8String;

// compare two string recs. This is NOT an alphabetic compare. SRs are first
// compared by length, then by first byte, then last byte then second, then
// N-1, until all bytes are compared.
function sdCompareSR(const SR1,  SR2: TsdStringRec): integer;

// compare 2 bytes
function sdCompareByte(Byte1, Byte2: byte): integer;

// compare 2 integers
function sdCompareInteger(Int1, Int2: integer): integer;

function sdUtf16ToUtf8Mem(Src: Pword; Dst: Pbyte; Count: integer): integer;
function sdUtf8ToUtf16Mem(var Src: Pbyte; Dst: Pword; Count: integer): integer;
procedure sdStreamWrite(S: TStream; const AString: AnsiString);
procedure sdStreamWriteStringRec(S: TStream; const AStringRec: TsdStringRec);
procedure sdStreamWriteRefString(S: TStream; ARefString: TsdRefString);

implementation

{ TsdRefString }

function TsdRefString.AsString: UTF8String;
begin
  Result := sdSRToString(StringRec);
end;

function TsdRefString.CompareToSR(const SR: TsdStringRec): integer;
begin
  if SR.Count = 0 then
  begin
    // shortcut
    Result := 1;
    exit;
  end;
  Result := sdCompareSR(StringRec, SR);
end;

destructor TsdRefString.Destroy;
begin
  FreeMem(FFirst);
  inherited;
end;

procedure TsdRefString.SetString(const SR: TsdStringRec);
begin
  FCharCount := SR.Count;
  ReallocMem(FFirst, FCharCount);
  Move(SR.First^, FFirst^, FCharCount);
end;

function TsdRefString.StringRec: TsdStringRec;
begin
  Result.First := FFirst;
  Result.Count := FCharCount;
end;

{ TsdRefStringList }

function TsdRefStringList.GetItems(Index: integer): TsdRefString;
begin
  Result := Get(Index);
end;

function TsdRefStringList.IndexOfID(AID: integer; var Index: integer): boolean;
var
  Min, Max: integer;
begin
  Result := False;

  // Find position - binary method
  Index := 0;
  Min := 0;
  Max := Count;
  while Min < Max do
  begin
    Index := (Min + Max) div 2;
    case sdCompareInteger(Items[Index].FID, AID) of
    -1: Min := Index + 1;
     0: begin
          Result := True;
          exit;
        end;
     1: Max := Index;
    end;
  end;

  Index := Min;
end;

function TsdRefStringList.IndexOfSR(const AStringRec: TsdStringRec; var Index: integer): boolean;
var
  Min, Max: integer;
  SR: TsdStringRec;
begin
  Result := False;

  // Find position - binary method
  Index := 0;
  Min := 0;
  Max := Count;
  while Min < Max do
  begin
    Index := (Min + Max) div 2;
    SR := TsdRefString(Get(Index)).StringRec;
    case sdCompareSR(SR, AStringRec) of
    -1: Min := Index + 1;
     0: begin
          Result := True;
          exit;
        end;
     1: Max := Index;
    end;
  end;

  Index := Min;
end;

{ TsdStringTable }

function TsdStringTable.AddString(const S: UTF8String): integer;
var
  SR: TsdStringRec;
begin
  SR := sdStringToSR(S);
  Result := AddStringRec(SR);
end;

function TsdStringTable.AddStringRec(const SR: TsdStringRec): integer;
var
  BySRIndex: integer;
  Item: TsdRefString;
  NewSR: TsdStringRec;
  Res: boolean;
begin
  // zero-length string
  if SR.Count = 0 then
  begin
    Result := 0;
    exit;
  end;

  // Try to find the new string
  if FBySR.IndexOfSR(SR, BySRIndex) then
  begin
    Item := FBySR.Items[BySRIndex];
    inc(Item.FFrequency);
    Result := Item.FID;
    exit;
  end;

  // Not found.. must make new item
  Item := TsdRefString.Create;
  Item.SetString(SR);
  NewSR := Item.StringRec;
  Item.FID := NextUniqueID;
  FById.Add(Item);
  Item.FFrequency := 1;

  // debug:
  //SetLength(Item.FValue, Item.FCount);
  //Move(Item.FirstPtr(FBase)^, Item.FValue[1], Item.FCount);

  // Insert in BySR lists
  Res := FBySR.IndexOfSR(NewSR, BySRIndex);
  assert(Res = False);
  FBySR.Insert(BySRIndex, Item);
  Result := Item.FID;
end;

function TsdStringTable.ById(index: integer): TsdRefString;
begin
  Result := FById[Index];
end;

procedure TsdStringTable.Clear;
begin
  FByID.Clear;
  FBySR.Clear;
end;

constructor TsdStringTable.Create;
begin
  inherited Create;
  FByID := TsdRefStringList.Create(False);
  FBySR := TsdRefStringList.Create(True);
end;

procedure TsdStringTable.DecFrequency(AItem: TsdRefString; ByIdIndex: integer);
var
  BySRIndex: integer;
  Res: boolean;
begin
  dec(AItem.FFrequency);
  assert(AItem.FFrequency >= 0);

  if AItem.FFrequency = 0 then
  begin
    // We must remove it
    FById.Delete(ByIdIndex);
    Res := FBySR.IndexOfSR(AItem.StringRec, BySRIndex);
    assert(Res = True);
    FBySR.Delete(BySRIndex);
  end;
end;

procedure TsdStringTable.Delete(ByIdIndex: integer);
var
  Item: TsdRefString;
  BySRIndex: integer;
  Res: boolean;
begin
  Item := FById[ByIdIndex];
  if Item = nil then
    exit;
  FById.Delete(ByIdIndex);
  Res := FBySR.IndexOfSR(Item.StringRec, BySRIndex);
  assert(Res = True);
  FBySR.Delete(BySRIndex);
end;

destructor TsdStringTable.Destroy;
begin
  FreeAndNil(FByID);
  FreeAndNil(FBySR);
  inherited;
end;

function TsdStringTable.ExistStringRec(const SR: TsdStringRec): boolean;
var
  BySRIndex: integer;
begin
  // zero-length string
  if SR.Count = 0 then
  begin
    Result := False;
    exit;
  end;

  // Try to find the new string
  Result := FBySR.IndexOfSR(SR, BySRIndex);

end;

function TsdStringTable.GetString(ID: integer): UTF8String;
var
  Index, Count: integer;
  Item: TsdRefString;
begin
  if ID = 0 then
  begin
    Result := '';
    exit;
  end;

  // Find the ID
  if FByID.IndexOfID(ID, Index) then
  begin
    Item := FById[Index];
    Count := Item.FCharCount;
    SetLength(Result, Count);
    Move(Item.FFirst^, Result[1], Count);
    exit;
  end;

  Result := '';
end;

function TsdStringTable.NextUniqueID: integer;
begin
  if FById.Count = 0 then
    Result := 1
  else
    Result := FByID[FByID.Count - 1].FID + 1;
end;

procedure TsdStringTable.SaveToFile(const AFileName: string);
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TsdStringTable.SaveToStream(S: TStream);
var
  i: integer;
  R: UTF8String;
begin
  for i := 0 to FBySR.Count - 1 do
  begin
    R := FBySR[i].AsString + #13#10;
    S.Write(R[1], length(R));
  end;
end;

procedure TsdStringTable.SetString(var ID: integer; const S: UTF8String);
var
  ByIdIndex: integer;
  Item: TsdRefString;
  SR: TsdStringRec;
begin
  // Make temp string record
  SR := sdStringtoSR(S);

  // Do we have a ref string with this ID?
  if (ID > 0) and FByID.IndexOfID(ID, ByIdIndex) then
  begin
    // Is the string still the same?
    Item := FById[ByIdIndex];
    if Item.CompareToSR(SR) = 0 then
      exit;
    // The string changed..
    DecFrequency(Item, ByIdIndex);
  end;

  ID := AddStringRec(SR);
end;

{utility functions}

function TsdStringTable.StringCount: integer;
begin
  Result := FBySR.Count;
end;

function sdStringToSR(const S: UTF8String): TsdStringRec;
begin
  Result.Count := length(S);
  if Result.Count = 0 then
    Result.First := nil
  else
    Result.First := @S[1];
end;

function sdSRToString(const SR: TsdStringRec): UTF8String;
begin
  SetLength(Result, SR.Count);
  if SR.Count > 0 then
    Move(SR.First^, Result[1], SR.Count);
end;

function sdCompareByte(Byte1, Byte2: byte): integer;
begin
  if Byte1 < Byte2 then
    Result := -1
  else
    if Byte1 > Byte2 then
      Result := 1
    else
      Result := 0;
end;

function sdCompareInteger(Int1, Int2: integer): integer;
begin
  if Int1 < Int2 then
    Result := -1
  else
    if Int1 > Int2 then
      Result := 1
    else
      Result := 0;
end;

function sdCompareSR(const SR1,  SR2: TsdStringRec): integer;
var
  Count: integer;
  First1, First2, Last1, Last2: Pbyte;
begin
  // Compare string length first
  Result := sdCompareInteger(SR1.Count, SR2.Count);
  if Result <> 0 then
    exit;

  // Compare first
  Result := sdCompareByte(SR1.First^, SR2.First^);
  if Result <> 0 then
    exit;
  Count := SR1.Count;

  // Setup First & Last pointers
  First1 := SR1.First;
  First2 := SR2.First;
  Last1 := First1; inc(Last1, Count);
  Last2 := First2; inc(Last2, Count);

  // Compare each time last ptrs then first ptrs, until they meet in the middle
  repeat
    dec(Last1);
    dec(Last2);
    if First1 = Last1 then
      exit;
    Result := sdCompareByte(Last1^, Last2^);
    if Result <> 0 then
      exit;
    inc(First1); inc(First2);
    if First1 = Last1 then
      exit;
    Result := sdCompareByte(First1^, First2^);
    if Result <> 0 then
      exit;
  until False;
end;

function sdUtf16ToUtf8Mem(Src: Pword; Dst: Pbyte; Count: integer): integer;
// Convert an Unicode (UTF16 LE) memory block to UTF8. This routine will process
// Count wide characters (2 bytes size) to Count UTF8 characters (1-3 bytes).
// Therefore, the block at Dst must be at least 1.5 the size of the source block.
// The function returns the number of *bytes* written.
var
  W: word;
  DStart: Pbyte;
begin
  DStart := Dst;
  while Count > 0 do
  begin
    W := Src^;
    inc(Src);
    if W <= $7F then
    begin
      Dst^ := byte(W);
      inc(Dst);
    end else
    begin
      if W > $7FF then
      begin
        Dst^ := byte($E0 or (W shr 12));
        inc(Dst);
        Dst^ := byte($80 or ((W shr 6) and $3F));
        inc(Dst);
        Dst^ := byte($80 or (W and $3F));
        inc(Dst);
      end else
      begin //  $7F < W <= $7FF
        Dst^ := byte($C0 or (W shr 6));
        inc(Dst);
        Dst^ := byte($80 or (W and $3F));
        inc(Dst);
      end;
    end;
    dec(Count);
  end;
  Result := integer(Dst) - integer(DStart);
end;

function sdUtf8ToUtf16Mem(var Src: Pbyte; Dst: Pword; Count: integer): integer;
// Convert an UTF8 memory block to Unicode (UTF16 LE). This routine will process
// Count *bytes* of UTF8 (each character 1-3 bytes) into UTF16 (each char 2 bytes).
// Therefore, the block at Dst must be at least 2 times the size of Count, since
// many UTF8 characters consist of just one byte, and are mapped to 2 bytes. The
// function returns the number of *wide chars* written. Note that the Src block must
// have an exact number of UTF8 characters in it, if Count doesn't match then
// the last character will be converted anyway (going past the block boundary!)
var
  W: word;
  C: byte;
  DStart: Pword;
  SClose: Pbyte;
begin
  DStart := Dst;
  SClose := Src;
  inc(SClose, Count);
  while integer(Src) < integer(SClose) do
  begin
    // 1st byte
    W := Src^;
    inc(Src);
    if W and $80 <> 0 then
    begin
      W := W and $3F;
      if W and $20 <> 0 then
      begin
        // 2nd byte
        C := Src^;
        inc(Src);
        if C and $C0 <> $80 then
          // malformed trail byte or out of range char
          Continue;
        W := (W shl 6) or (C and $3F);
      end;
      // 2nd or 3rd byte
      C := Src^;
      inc(Src);
      if C and $C0 <> $80 then
        // malformed trail byte
        Continue;
      Dst^ := (W shl 6) or (C and $3F);
      inc(Dst);
    end else
    begin
      Dst^ := W;
      inc(Dst);
    end;
  end;
  Result := (integer(Dst) - integer(DStart)) div 2;
end;

procedure sdStreamWrite(S: TStream; const AString: AnsiString);
var
  L: integer;
begin
  L := Length(AString);
  if L > 0 then
  begin
    S.Write(AString[1], L);
  end;
end;

procedure sdStreamWriteStringRec(S: TStream; const AStringRec: TsdStringRec);
begin
  S.Write(PAnsiChar(AStringRec.First)^, AStringRec.Count);
end;

procedure sdStreamWriteRefString(S: TStream; ARefString: TsdRefString);
begin
  if ARefString = nil then
    exit;
  S.Write(PAnsiChar(ARefString.FFirst)^, ARefString.FCharCount);
end;

end.
