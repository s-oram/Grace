// ----------------------------------------------------------------------------
// copyright (c) 1998, 1999, 2000 Thai Open Source Software Center Ltd
// and Clark Cooper
// copyright (c) 2001, 2002, 2003, 2004, 2005, 2006 Expat maintainers.
//
// Expat - Version 2.0.0 Release Milano 0.83 (PasExpat 2.0.0 RM0.83)
// Pascal Port By: Milan Marusinec alias Milano
// milan@marusinec.sk
// http://www.pasports.org/pasexpat
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
//
// [Pascal Port History] -----------------------------------------------------
//
// 03.05.2006-Milano: Unit port establishment
// 02.06.2006-Milano: porting
// 06.06.2006-Milano: -"-
// 07.06.2006-Milano: doProlog
// 08.06.2006-Milano: doProlog finished, porting
// 09.06.2006-Milano: porting
// 12.06.2006-Milano: -"-
// 14.06.2006-Milano: -"-
// 15.06.2006-Milano: doContent
// 16.06.2006-Milano: porting, storeAtts
// 17.06.2006-Milano: -"-
// 20.06.2006-Milano: epilogProcessor, porting
// 22.06.2006-Milano: appendAttributeValue
//
{ xmlparse.inc }
{$Q-}
{$R-}
function PoolStoreString(Pool: STRING_POOL_ptr; Enc: ENCODING_ptr;
  Ptr, End_: PAnsiChar): XML_PAnsiChar; forward;
procedure PoolFinish(Pool: STRING_POOL_ptr); forward;
procedure PoolClear(Pool: STRING_POOL_ptr); forward;
procedure PoolDestroy(Pool: STRING_POOL_ptr); forward;
function PoolAppendChar(Pool: STRING_POOL_ptr; C: AnsiChar): Integer; forward;

function ReportProcessingInstruction(Parser: XML_Parser; Enc: ENCODING_ptr;
  Start, End_: PAnsiChar): Integer; forward;
function ReportComment(Parser: XML_Parser; Enc: ENCODING_ptr;
  Start, End_: PAnsiChar): Integer; forward;

function GetAttributeId(Parser: XML_Parser; Enc: ENCODING_ptr;
  Start, End_: PAnsiChar): ATTRIBUTE_ID_ptr; forward;

function StoreAttributeValue(Parser: XML_Parser; Enc: ENCODING_ptr;
  IsCdata: XML_Bool; Ptr, End_: PAnsiChar; Pool: STRING_POOL_ptr)
  : XML_Error; forward;

const
  ImplicitContext: array [0 .. 40] of XML_Char = ('x', 'm', 'l', '=', 'h', 't',
    't', 'p', ':', '/', '/', 'w', 'w', 'w', '.', 'w', '3', '.', 'o', 'r', 'g',
    '/', 'X', 'M', 'L', '/', '1', '9', '9', '8', '/', 'n', 'a', 'm', 'e', 's',
    'p', 'a', 'c', 'e', #0);

  INIT_TAG_BUF_SIZE = 32; { must be a multiple of SizeOf(XML_Char) }
  INIT_DATA_BUF_SIZE = 1024;
  INIT_ATTS_SIZE = 16;
  INIT_ATTS_VERSION = $FFFFFFFF;
  INIT_BLOCK_SIZE = 1024;
  INIT_BUFFER_SIZE = 1024;

  EXPAND_SPARE = 24;

  INIT_SCAFFOLD_ELEMENTS = 32;

  INIT_POWER = 6;

type
  IPPAnsiChar = ^IPAnsiChar;
  IPAnsiChar = ^ICHAR;

{$IFDEF XML_UNICODE }
  ICHAR = Int16u;

{$ELSE }
  ICHAR = Char;

{$ENDIF }
  HASH_TABLE_ITER_ptr = ^HASH_TABLE_ITER;

  HASH_TABLE_ITER = record
    P, End_: NAMED_ptr_ptr;

  end;

const
{$IFDEF XML_UNICODE }
  XML_ENCODE_MAX = XML_UTF16_ENCODE_MAX;

{$ELSE }
  XML_ENCODE_MAX = XML_UTF8_ENCODE_MAX;

{$ENDIF }

  { memcmp }
function Memcmp(P1, P2: Int8u_ptr; L: Integer): Integer;
begin
  while L > 0 do
  begin
    if P1^ <> P2^ then
    begin
      Result := P1^ - P2^;

      Exit;

    end;

    Dec(L);
    Inc(PtrComp(P1));
    Inc(PtrComp(P2));

  end;

  Result := 0;

end;

{ CHAR_HASH }
{ Basic character hash algorithm, taken from Python's string hash:
  h = h * 1000003 ^ character, the constant being a prime number. }
function CHAR_HASH(H: Int32u; C: XML_Char): Int32u;
begin
{$IFDEF XML_UNICODE }
  Result := (H * $F4243) xor Int16u(C);

{$ELSE }
  Result := (H * $F4243) xor Int8u(C);

{$ENDIF }
end;

{ MUSRasterizerConverterERT }
function MUSRasterizerConverterERT(Enc: ENCODING_ptr; S: PAnsiChar): Integer;
begin
{$IFDEF XML_UNICODE }
  Result := Integer(not Boolean(Enc.IsUtf16) or Boolean(Int32u(S) and 1));

{$ELSE }
  Result := Integer(not Boolean(Enc.IsUtf8));

{$ENDIF }
end;

{ For probing (after a collision) we need a step size relative prime
  to the hash table size, which is a power of 2. We use double-hashing,
  since we can calculate a second hash value cheaply by taking those bits
  of the first hash value that were discarded (masked out) when the table
  index was calculated: index:=hash and mask, where mask:=table.size - 1.
  We limit the maximum step size to table.size div 4 (mask shr 2 ) and make
  it odd, since odd numbers are always relative prime to a power of 2. }
{ SECOND_HASH }
function SECOND_HASH(Hash, Mask: Int32u; Power: Int8u): Int8u;
begin
  Result := ((Hash and not Mask) shr (Power - 1)) and (Mask shr 2);
end;

{ PROBE_STEP }
function PROBE_STEP(Hash, Mask: Int32u; Power: Int8u): Int8u;
begin
  Result := SECOND_HASH(Hash, Mask, Power) or 1;
end;

{ XML_T }
function XML_T(X: AnsiChar): XML_Char;
begin
  Result := X;
end;

{ XML_L }
function XML_L(X: AnsiChar): XML_Char;
begin
  Result := X;
end;

{ ROUND_UP }
{ Round up n to be a multiple of sz, where sz is a power of 2. }
function ROUND_UP(N, Sz: Integer): Integer;
begin
  Result := (N + (Sz - 1)) and not(Sz - 1);
end;

{ XmlConvert }
procedure XmlConvert(Enc: ENCODING_ptr; FromP, FromLim, ToP, ToLim: Pointer);
begin
{$IFDEF XML_UNICODE }
  XmlUtf16Convert(Enc, FromP, FromLim, ToP, ToLim);

{$ELSE }
  XmlUtf8Convert(Enc, FromP, FromLim, ToP, ToLim);

{$ENDIF }
end;

{ XmlEncode }
function XmlEncode(CharNumber: Integer; Buf: Pointer): Integer;
begin
{$IFDEF XML_UNICODE }
  Result := XmlUtf16Encode(CharNumber, Buf);

{$ELSE }
  Result := XmlUtf8Encode(CharNumber, Buf);

{$ENDIF }
end;

{ poolInit }
procedure PoolInit(Pool: STRING_POOL_ptr; Ms: XML_Memory_Handling_Suite_ptr);
begin
  Pool.Blocks := nil;
  Pool.FreeBlocks := nil;
  Pool.Start := nil;
  Pool.Ptr := nil;
  Pool.End_ := nil;
  Pool.Mem := Ms;
end;

{ hashTableDestroy }
procedure HashTableDestroy(Table: HASH_TABLE_ptr);
var
  I: Size_t;
begin
  I := 0;

  while I < Table.Size do
  begin
    if NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^ <> nil then
      Table.Mem.Free_fcn(Pointer(NAMED_ptr_ptr(PtrComp(Table.V) + I *
        SizeOf(NAMED_ptr))^), NAMED_ptr_ptr(PtrComp(Table.V) + I *
        SizeOf(NAMED_ptr))^^.Alloc);

    Inc(I);

  end;

  Table.Mem.Free_fcn(Pointer(Table.V), Table.A);
end;

{ hashTableInit }
procedure HashTableInit(P: HASH_TABLE_ptr; Ms: XML_Memory_Handling_Suite_ptr);
begin
  P.Power := 0;
  P.Size := 0;
  P.Used := 0;
  P.V := nil;
  P.Mem := Ms;
end;

{ hashTableIterInit }
procedure HashTableIterInit(Iter: HASH_TABLE_ITER_ptr; Table: HASH_TABLE_ptr);
begin
  Iter.P := Table.V;
  Iter.End_ := NAMED_ptr_ptr(PtrComp(Iter.P) + Table.Size * SizeOf(NAMED_ptr));
end;

{ hashTableIterNext }
function HashTableIterNext(Iter: HASH_TABLE_ITER_ptr): NAMED_ptr;
var
  Tem: NAMED_ptr;
begin
  while Iter.P <> Iter.End_ do
  begin
    Tem := Iter.P^;

    Inc(PtrComp(Iter.P), SizeOf(NAMED_ptr));

    if Tem <> nil then
    begin
      Result := Tem;
      Exit;
    end;
  end;
  Result := nil;
end;

{ dtdCreate }
function DtdCreate(Ms: XML_Memory_Handling_Suite_ptr): DTD_ptr;
var
  P: DTD_ptr;

begin
  Ms.Malloc_fcn(Pointer(P), SizeOf(DTD));

  if P = nil then
  begin
    Result := P;

    Exit;
  end;

  PoolInit(@P.Pool, Ms);
  PoolInit(@P.EntityValuePool, Ms);

  HashTableInit(@P.GeneralEntities, Ms);
  HashTableInit(@P.ElementTypes, Ms);
  HashTableInit(@P.AttributeIds, Ms);
  HashTableInit(@P.Prefixes, Ms);

{$IFDEF XML_DTD }
  P.ParamEntityRead := XML_FALSE;

  HashTableInit(@P.ParamEntities, Ms);

{$ENDIF }
  P.DefaultPrefix.Name := nil;
  P.DefaultPrefix.Binding := nil;

  P.In_eldecl := XML_FALSE;
  P.ScaffIndex := nil;
  P.ScaffAlloc := 0;
  P.Scaffold := nil;
  P.ScaffLevel := 0;
  P.ScaffSize := 0;
  P.ScaffCount := 0;
  P.ContentStringLen := 0;

  P.KeepProcessing := XML_TRUE;
  P.HasParamEntityRefs := XML_FALSE;
  P.Standalone := XML_FALSE;

  Result := P;
end;

{ dtdDestroy }
procedure DtdDestroy(P: DTD_ptr; IsDocEntity: XML_Bool;
  Ms: XML_Memory_Handling_Suite_ptr);
var
  Iter: HASH_TABLE_ITER;
  E: ELEMENT_TYPE_ptr;
begin
  HashTableIterInit(@Iter, @P.ElementTypes);

  repeat
    E := ELEMENT_TYPE_ptr(HashTableIterNext(@Iter));

    if E = nil then
      Break;

    if E.AllocDefaultAtts <> 0 then
      Ms.Free_fcn(Pointer(E.DefaultAtts), E.DefaultAttsAlloc);

  until False;

  HashTableDestroy(@P.GeneralEntities);

{$IFDEF XML_DTD }
  HashTableDestroy(@P.ParamEntities);

{$ENDIF }
  HashTableDestroy(@P.ElementTypes);
  HashTableDestroy(@P.AttributeIds);
  HashTableDestroy(@P.Prefixes);

  PoolDestroy(@P.Pool);
  PoolDestroy(@P.EntityValuePool);

  if IsDocEntity <> 0 then
  begin
    Ms.Free_fcn(Pointer(P.ScaffIndex), P.ScaffAlloc);
    Ms.Free_fcn(Pointer(P.Scaffold), SizeOf(CONTENT_SCAFFOLD));
  end;

  Ms.Free_fcn(Pointer(P), SizeOf(DTD));
end;

{ handleUnknownEncoding {.. }
function HandleUnknownEncoding(Parser: XML_Parser; EncodingName: XML_PAnsiChar)
  : XML_Error;
begin
end;

{ initializeEncoding }
function InitializeEncoding(Parser: XML_Parser): XML_Error;
var
  S : PAnsiChar;
  Ok: Integer;

begin
{$IFDEF XML_UNICODE {..}
{$ELSE }
  S := Pointer(Parser.M_protocolEncodingName);

{$ENDIF }
  if Parser.M_ns <> 0 then
    Ok := XmlInitEncodingNS(@Parser.MInitEncoding, @Parser.M_encoding,
      Pointer(S))
  else
    Ok := XmlInitEncoding(@Parser.MInitEncoding, @Parser.M_encoding,
      Pointer(S));

  if Ok <> 0 then
    Result := XML_ERROR_NONE
  else
    Result := HandleUnknownEncoding(Parser, Parser.M_protocolEncodingName);
end;

{ reportDefault {.. }
procedure ReportDefault(Parser: XML_Parser; Enc: ENCODING_ptr;
  Start, End_: PAnsiChar);
begin
end;

{ getContext {.. }
function GetContext(Parser: XML_Parser): XML_PAnsiChar;
begin
end;

{ processXmlDecl }
function ProcessXmlDecl(Parser: XML_Parser; IsGeneralTextEntity: Integer;
  S, Next: PAnsiChar): XML_Error;
var
  EncodingName, Version, Versionend: PAnsiChar;
  StoredEncName, Storedversion: XML_PAnsiChar;
  NewEncoding: ENCODING_ptr;
  Standalone, Ok: Integer;
  Result_: XML_Error;
begin
  EncodingName := nil;
  StoredEncName := nil;
  NewEncoding := nil;
  Version := nil;
  Storedversion := nil;
  Standalone := -1;

  if Parser.M_ns <> 0 then
    Ok := XmlParseXmlDeclNS(IsGeneralTextEntity, Parser.M_encoding, Pointer(S),
      Pointer(Next), @Parser.M_eventPtr, @Version, @Versionend, @EncodingName,
      @NewEncoding, @Standalone)
  else
    Ok := XmlParseXmlDecl(IsGeneralTextEntity, Parser.M_encoding, Pointer(S),
      Pointer(Next), @Parser.M_eventPtr, @Version, @Versionend, @EncodingName,
      @NewEncoding, @Standalone);

  if Ok = 0 then
    if IsGeneralTextEntity <> 0 then
    begin
      Result := XML_ERROR_TEXT_DECL;

      Exit;
    end
    else
    begin
      Result := XML_ERROR_XML_DECL;

      Exit;
    end;

  if (IsGeneralTextEntity = 0) and (Standalone = 1) then
  begin
    Parser.M_dtd.Standalone := XML_TRUE;

{$IFDEF XML_DTD }
    if Parser.M_paramEntityParsing = XML_PARAM_ENTITY_PARSING_UNLESS_STANDALONE
    then
      Parser.M_paramEntityParsing := XML_PARAM_ENTITY_PARSING_NEVER;

{$ENDIF }
  end;

  if @Parser.M_xmlDeclHandler <> nil then
  begin
    if EncodingName <> nil then
    begin
      StoredEncName := PoolStoreString(@Parser.M_temp2Pool, Parser.M_encoding,
        EncodingName, PAnsiChar(PtrComp(EncodingName) +
        XmlNameLength(Parser.M_encoding, Pointer(EncodingName))));

      if StoredEncName = nil then
      begin
        Result := XML_ERROR_NO_MEMORY;

        Exit;
      end;

      PoolFinish(@Parser.M_temp2Pool);
    end;

    if Version <> nil then
    begin
      Storedversion := PoolStoreString(@Parser.M_temp2Pool, Parser.M_encoding,
        Version, PAnsiChar(PtrComp(Versionend) -
        Parser.M_encoding.MinBytesPerChar));

      if Storedversion = nil then
      begin
        Result := XML_ERROR_NO_MEMORY;

        Exit;
      end;
    end;

    Parser.M_xmlDeclHandler(Parser.M_handlerArg, Storedversion, StoredEncName,
      Standalone);
  end
  else if @Parser.M_defaultHandler <> nil then
    ReportDefault(Parser, Parser.M_encoding, S, Next);

  if Parser.M_protocolEncodingName = nil then
  begin
    if NewEncoding <> nil then
    begin
      if NewEncoding.MinBytesPerChar <> Parser.M_encoding.MinBytesPerChar then
      begin
        Parser.M_eventPtr := EncodingName;

        Result := XML_ERROR_INCORRECT_ENCODING;

        Exit;
      end;

      Parser.M_encoding := NewEncoding;
    end
    else if EncodingName <> nil then
    begin
      if StoredEncName = nil then
      begin
        StoredEncName := PoolStoreString(@Parser.M_temp2Pool, Parser.M_encoding,
          EncodingName, PAnsiChar(PtrComp(EncodingName) +
          XmlNameLength(Parser.M_encoding, Pointer(EncodingName))));

        if StoredEncName = nil then
        begin
          Result := XML_ERROR_NO_MEMORY;

          Exit;
        end;
      end;

      Result_ := HandleUnknownEncoding(Parser, StoredEncName);

      PoolClear(@Parser.M_temp2Pool);

      if Result_ = XML_ERROR_UNKNOWN_ENCODING then
        Parser.M_eventPtr := EncodingName;

      Result := Result_;
      Exit;
    end;
  end;

  if (StoredEncName <> nil) or (Storedversion <> nil) then
    PoolClear(@Parser.M_temp2Pool);

  Result := XML_ERROR_NONE;
end;

{ poolClear }
procedure PoolClear(Pool: STRING_POOL_ptr);
var
  P, Tem: BLOCK_ptr;
begin
  if Pool.FreeBlocks = nil then
    Pool.FreeBlocks := Pool.Blocks

  else
  begin
    P := Pool.Blocks;

    while P <> nil do
    begin
      Tem := P.Next;
      P.Next := Pool.FreeBlocks;
      Pool.FreeBlocks := P;
      P := Tem;
    end;
  end;

  Pool.Blocks := nil;
  Pool.Start := nil;
  Pool.Ptr := nil;
  Pool.End_ := nil;
end;

{ poolDestroy }
procedure PoolDestroy(Pool: STRING_POOL_ptr);
var
  P, Tem: BLOCK_ptr;
begin
  P := Pool.Blocks;

  while P <> nil do
  begin
    Tem := P.Next;

    Pool.Mem.Free_fcn(Pointer(P), P.Alloc);

    P := Tem;
  end;

  P := Pool.FreeBlocks;

  while P <> nil do
  begin
    Tem := P.Next;

    Pool.Mem.Free_fcn(Pointer(P), P.Alloc);

    P := Tem;
  end;
end;

{ poolGrow }
function PoolGrow(Pool: STRING_POOL_ptr): XML_Bool;
var
  Tem: BLOCK_ptr;
  BlockSize: Integer;
begin
  if Pool.FreeBlocks <> nil then
  begin
    if Pool.Start = nil then
    begin
      Pool.Blocks := Pool.FreeBlocks;
      Pool.FreeBlocks := Pool.FreeBlocks.Next;
      Pool.Blocks.Next := nil;

      Pool.Start := @Pool.Blocks.S;
      Pool.End_ := XML_PAnsiChar(PtrComp(Pool.Start) + Pool.Blocks.Size *
        SizeOf(XML_Char));
      Pool.Ptr := Pool.Start;

      Result := XML_TRUE;

      Exit;
    end;

    if PtrComp(Pool.End_) - PtrComp(Pool.Start) < Pool.FreeBlocks.Size then
    begin
      Tem := Pool.FreeBlocks.Next;

      Pool.FreeBlocks.Next := Pool.Blocks;
      Pool.Blocks := Pool.FreeBlocks;
      Pool.FreeBlocks := Tem;

      Move(Pool.Start^, Pointer(@Pool.Blocks.S)^, PtrComp(Pool.End_) -
        PtrComp(Pool.Start));

      Pool.Ptr := XML_PAnsiChar(PtrComp(@Pool.Blocks.S) + PtrComp(Pool.Ptr) -
        PtrComp(Pool.Start));
      Pool.Start := @Pool.Blocks.S;
      Pool.End_ := XML_PAnsiChar(PtrComp(Pool.Start) + Pool.Blocks.Size *
        SizeOf(XML_Char));

      Result := XML_TRUE;

      Exit;
    end;
  end;

  if (Pool.Blocks <> nil) and (Pool.Start = @Pool.Blocks.S) then
  begin
    BlockSize := (PtrComp(Pool.End_) - PtrComp(Pool.Start)) *
      2 div SizeOf(XML_Char);

    Pool.Mem.Realloc_fcn(Pointer(Pool.Blocks), Pool.Blocks.Alloc,
      (SizeOf(BLOCK_ptr) + SizeOf(Integer) * 2) + BlockSize * SizeOf(XML_Char));

    if Pool.Blocks = nil then
    begin
      Result := XML_FALSE;

      Exit;
    end
    else
      Pool.Blocks.Alloc := (SizeOf(BLOCK_ptr) + SizeOf(Integer) * 2) + BlockSize *
        SizeOf(XML_Char);

    Pool.Blocks.Size := BlockSize;

    Pool.Ptr := XML_PAnsiChar(PtrComp(@Pool.Blocks.S) +
      (PtrComp(Pool.Ptr) - PtrComp(Pool.Start)));
    Pool.Start := @Pool.Blocks.S;
    Pool.End_ := XML_PAnsiChar(PtrComp(Pool.Start) + BlockSize *
      SizeOf(XML_Char));
  end
  else
  begin
    BlockSize := (PtrComp(Pool.End_) - PtrComp(Pool.Start))
      div SizeOf(XML_Char);

    if BlockSize < INIT_BLOCK_SIZE then
      BlockSize := INIT_BLOCK_SIZE
    else
      BlockSize := BlockSize * 2;

    Pool.Mem.Malloc_fcn(Pointer(Tem), (SizeOf(BLOCK_ptr) + SizeOf(Integer) * 2) +
      BlockSize * SizeOf(XML_Char));

    if Tem = nil then
    begin
      Result := XML_FALSE;

      Exit;
    end;

    Tem.Size := BlockSize;
    Tem.Alloc := (SizeOf(BLOCK_ptr) + SizeOf(Integer) * 2) + BlockSize *
      SizeOf(XML_Char);
    Tem.Next := Pool.Blocks;

    Pool.Blocks := Tem;

    if Pool.Ptr <> Pool.Start then
      Move(Pool.Start^, Pointer(@Tem.S)^, PtrComp(Pool.Ptr) -
        PtrComp(Pool.Start));

    Pool.Ptr := XML_PAnsiChar(PtrComp(@Tem.S) +
      (PtrComp(Pool.Ptr) - PtrComp(Pool.Start)) * SizeOf(XML_Char));
    Pool.Start := @Tem.S;
    Pool.End_ := XML_PAnsiChar(PtrComp(@Tem.S) + BlockSize * SizeOf(XML_Char));
  end;

  Result := XML_TRUE;
end;

{ poolAppend }
function PoolAppend(Pool: STRING_POOL_ptr; Enc: ENCODING_ptr;
  Ptr, End_: PAnsiChar): XML_PAnsiChar;
begin
  if (Pool.Ptr = nil) and (PoolGrow(Pool) = 0) then
  begin
    Result := nil;

    Exit;
  end;

  repeat
    XmlConvert(Enc, @Ptr, End_, IPPAnsiChar(@Pool.Ptr), IPAnsiChar(Pool.End_));

    if Ptr = End_ then
      Break;

    if PoolGrow(Pool) = 0 then
      Result := nil;
  until False;

  Result := Pool.Start;
end;

{ poolStoreString }
function PoolStoreString(Pool: STRING_POOL_ptr; Enc: ENCODING_ptr;
  Ptr, End_: PAnsiChar): XML_PAnsiChar;
begin
  if PoolAppend(Pool, Enc, Ptr, End_) = nil then
  begin
    Result := nil;

    Exit;
  end;

  if (Pool.Ptr = Pool.End_) and (PoolGrow(Pool) = 0) then
  begin
    Result := nil;

    Exit;
  end;

  Pool.Ptr^ := XML_Char(0);
  Inc(PtrComp(Pool.Ptr));
  Result := Pool.Start;
end;

{ poolCopyString }
function PoolCopyString(Pool: STRING_POOL_ptr; S: XML_PAnsiChar): XML_PAnsiChar;
label
  _w0;

begin
  goto _w0;

  while S^ <> XML_Char(0) do
  begin
  _w0:
    if PoolAppendChar(Pool, S^) = 0 then
    begin
      Result := nil;

      Exit;
    end;

    Inc(PtrComp(S), SizeOf(XML_Char));
  end;

  S := Pool.Start;
  PoolFinish(Pool);
  Result := S;
end;

{ poolAppendString {.. }
function PoolAppendString(Pool: STRING_POOL_ptr; S: XML_PAnsiChar)
  : XML_PAnsiChar;
begin
end;

{ poolStart }
function PoolStart(Pool: STRING_POOL_ptr): XML_PAnsiChar;
begin
  Result := Pool.Start;
end;

{ poolLength }
function PoolLength(Pool: STRING_POOL_ptr): Integer;
begin
  Result := PtrComp(Pool.Ptr) - PtrComp(Pool.Start);
end;

{ poolChop }
procedure PoolChop(Pool: STRING_POOL_ptr);
begin
  Dec(PtrComp(Pool.Ptr), SizeOf(XML_Char));
end;

{ poolLastChar }
function PoolLastChar(Pool: STRING_POOL_ptr): XML_Char;
begin
  Result := XML_PAnsiChar(PtrComp(Pool.Ptr) - 1 * SizeOf(XML_Char))^;
end;

{ poolDiscard }
procedure PoolDiscard(Pool: STRING_POOL_ptr);
begin
  Pool.Ptr := Pool.Start;
end;

{ poolFinish }
procedure PoolFinish(Pool: STRING_POOL_ptr);
begin
  Pool.Start := Pool.Ptr;
end;

{ poolAppendChar }
function PoolAppendChar(Pool: STRING_POOL_ptr; C: AnsiChar): Integer;
begin
  if (Pool.Ptr = Pool.End_) and (PoolGrow(Pool) = 0) then
    Result := 0
  else
  begin
    Pool.Ptr^ := C;

    Inc(PtrComp(Pool.Ptr));

    Result := 1;
  end;
end;

{ keyeq }
function Keyeq(S1, S2: KEY): XML_Bool;
begin
  while S1^ = S2^ do
  begin
    if S1^ = #0 then
    begin
      Result := XML_TRUE;

      Exit;
    end;

    Inc(PtrComp(S1), SizeOf(XML_Char));
    Inc(PtrComp(S2), SizeOf(XML_Char));
  end;

  Result := XML_FALSE;
end;

{ hash }
function Hash(S: KEY): Int32u;
var
  H: Int32u;
begin
  H := 0;

  while S^ <> XML_Char(0) do
  begin
    H := CHAR_HASH(H, S^);

    Inc(PtrComp(S), SizeOf(XML_Char));
  end;

  Result := H;
end;

{ lookup }
function Lookup(Table: HASH_TABLE_ptr; Name: KEY; CreateSize: Size_t)
  : NAMED_ptr;
var
  I, Tsize, NewSize, J: Size_t;
  H, Mask, NewMask, NewHash: Int32u;
  Step, NewPower: Int8u;
  NewV: NAMED_ptr_ptr;
begin
  if Table.Size = 0 then
  begin
    if CreateSize = 0 then
    begin
      Result := nil;

      Exit;
    end;

    Table.Power := INIT_POWER;

    { table->size is a power of 2 }
    Table.Size := Size_t(1 shl INIT_POWER);

    Tsize := Table.Size * SizeOf(NAMED_ptr);

    Table.Mem.Malloc_fcn(Pointer(Table.V), Tsize);

    if Table.V = nil then
    begin
      Table.Size := 0;

      Result := nil;

      Exit;
    end
    else
      Table.A := Tsize;

    FillChar(Table.V^, Tsize, 0);

    I := Hash(name) and (Table.Size - 1);
  end
  else
  begin
    H := Hash(name);
    Mask := Table.Size - 1;
    Step := 0;
    I := H and Mask;

    while NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^ <> nil do
    begin
      if Keyeq(name, NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))
        ^^.Name) <> 0 then
      begin
        Result := NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^;

        Exit;
      end;

      if Step = 0 then
        Step := PROBE_STEP(H, Mask, Table.Power);

      if I < Step then
        Inc(I, Table.Size - Step)
      else
        Dec(I, Step);
    end;

    if CreateSize = 0 then
    begin
      Result := nil;

      Exit;
    end;

    { check for overfLow (table is half full) }
    if Table.Used shr (Table.Power - 1) <> 0 then
    begin
      NewPower := Table.Power + 1;
      NewSize := Size_t(1 shl NewPower);
      NewMask := NewSize - 1;
      Tsize := NewSize * SizeOf(NAMED_ptr);

      Table.Mem.Malloc_fcn(Pointer(NewV), Tsize);

      if NewV = nil then
      begin
        Result := nil;

        Exit;
      end;

      FillChar(NewV^, Tsize, 0);

      I := 0;

      while I < Table.Size do
      begin
        if NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^ <> nil then
        begin
          NewHash := Hash(NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr)
            )^^.Name);
          J := NewHash and NewMask;
          Step := 0;

          while NAMED_ptr_ptr(PtrComp(NewV) + J * SizeOf(NAMED_ptr))^ <> nil do
          begin
            if Step = 0 then
              Step := PROBE_STEP(NewHash, NewMask, NewPower);

            if J < Step then
              Inc(J, NewSize - Step)
            else
              Dec(J, Step);
          end;

          NAMED_ptr_ptr(PtrComp(NewV) + J * SizeOf(NAMED_ptr))^ :=
            NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^;
        end;

        Inc(I);
      end;

      Table.Mem.Free_fcn(Pointer(Table.V), Table.A);

      Table.V := NewV;
      Table.A := Tsize;
      Table.Power := NewPower;
      Table.Size := NewSize;

      I := H and NewMask;
      Step := 0;

      while NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^ <> nil do
      begin
        if Step = 0 then
          Step := PROBE_STEP(H, NewMask, NewPower);

        if I < Step then
          Inc(I, NewSize - Step)
        else
          Dec(I, Step);
      end;
    end;
  end;

  Table.Mem.Malloc_fcn(Pointer(NAMED_ptr_ptr(PtrComp(Table.V) + I *
    SizeOf(NAMED_ptr))^), CreateSize);

  if NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^ = nil then
  begin
    Result := nil;

    Exit;
  end;

  FillChar(NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^^,
    CreateSize, 0);

  NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^^.Name := name;
  NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^^.Alloc := CreateSize;

  Inc(Table.Used);

  Result := NAMED_ptr_ptr(PtrComp(Table.V) + I * SizeOf(NAMED_ptr))^;
end;

{ normalizePublicId }
procedure NormalizePublicId(PublicId: XML_PAnsiChar);
var
  P, S: XML_PAnsiChar;

begin
  P := PublicId;
  S := PublicId;

  while S^ <> XML_Char(0) do
  begin
    case S^ of
      XML_Char($20), XML_Char($D), XML_Char($A):
        if (P <> PublicId) and (XML_PAnsiChar(PtrComp(P) - 1 * SizeOf(XML_Char))
          ^ <> XML_Char($20)) then
        begin
          P^ := XML_Char($20);

          Inc(PtrComp(P), SizeOf(XML_Char));
        end;

    else
      begin
        P^ := S^;

        Inc(PtrComp(P), SizeOf(XML_Char));
      end;
    end;

    Inc(PtrComp(S), SizeOf(XML_Char));
  end;

  if (P <> PublicId) and (XML_PAnsiChar(PtrComp(P) - 1 * SizeOf(XML_Char))
    ^ = XML_Char($20)) then
    Dec(PtrComp(P), SizeOf(XML_Char));

  P^ := XML_T(#0);
end;

{ setElementTypePrefix {.. }
function SetElementTypePrefix(Parser: XML_Parser;
  ElementType: ELEMENT_TYPE_ptr): Integer;
begin
end;

{ addBinding {.. }
{ addBinding overwrites the value of prefix.binding without checking.
  Therefore one must keep track of the old value outside of addBinding. }
function AddBinding(Parser: XML_Parser; Prefix: PREFIX_ptr;
  AttId: ATTRIBUTE_ID_ptr; Uri: XML_PAnsiChar; BindingsPtr: BINDING_ptr_ptr)
  : XML_Error;
begin
end;

{ storeRawNames }
{ Initially tag.rawName always points into the parse buffer;
  for those TAG instances opened while the current parse buffer was
  processed, and not yet closed, we need to store tag.rawName in a more
  permanent location, since the parse buffer is about to be discarded. }
function StoreRawNames(Parser: XML_Parser): XML_Bool;
var
  Tag: TAG_ptr;
  BufSize, NameLen: Integer;
  RawNameBuf, Temp: PAnsiChar;
begin
  Tag := Parser.M_tagStack;

  while Tag <> nil do
  begin
    NameLen := SizeOf(XML_Char) * (Tag.Name.StrLen + 1);
    RawNameBuf := PAnsiChar(PtrComp(Tag.Buf) + NameLen);

    { Stop if already stored. Since tagStack is a stack, we can stop
      at the first entry that has already been copied; everything
      below it in the stack is already been accounted for in a
      previous call to this function. }
    if Tag.RawName = RawNameBuf then
      Break;

    { For re-use purposes we need to ensure that the
      size of tag.buf is a multiple of SizeOf(XML_Char ). }
    BufSize := NameLen + ROUND_UP(Tag.RawNameLength, SizeOf(XML_Char));

    if BufSize > PtrComp(Tag.BufEnd) - PtrComp(Tag.Buf) then
    begin
      if Parser.M_mem.Realloc_fcn(Pointer(Tag.Buf), Tag.Alloc, BufSize) then
        Temp := Tag.Buf
      else
        Temp := nil;

      if Temp = nil then
      begin
        Result := XML_FALSE;

        Exit;

      end;

      Tag.Alloc := BufSize;

      { if tag.name.str points to tag.buf (only when namespace
        processing is off) then we have to update it }
      if Tag.Name.Str = XML_PAnsiChar(Tag.Buf) then
        Tag.Name.Str := XML_PAnsiChar(Temp);

      { if tag->name.localPart is set (when namespace processing is on)
        then update it as well, since it will always point into tag->buf }
      if Tag.Name.LocalPart <> nil then
        Tag.Name.LocalPart :=
          XML_PAnsiChar(PtrComp(Temp) + (PtrComp(Tag.Name.LocalPart) -
          PtrComp(Tag.Buf)));

      Tag.Buf := Temp;
      Tag.BufEnd := PAnsiChar(PtrComp(Temp) + BufSize);
      RawNameBuf := PAnsiChar(PtrComp(Temp) + NameLen);

    end;

    Move(Tag.RawName^, RawNameBuf^, Tag.RawNameLength);

    Tag.RawName := RawNameBuf;
    Tag := Tag.Parent;

  end;

  Result := XML_TRUE;

end;

{ storeAtts }
{ Precondition: all arguments must be non-NULL;
  Purpose:
  - normalize attributes
  - check attributes for well-formedness
  - generate namespace aware attribute names (URI, prefix)
  - build list of attributes for startElementHandler
  - default attributes
  - process namespace declarations (check and report them)
  - generate namespace aware element name (URI, prefix) }
function StoreAtts(Parser: XML_Parser; Enc: ENCODING_ptr; AttStr: PAnsiChar;
  TagNamePtr: TAG_NAME_ptr; BindingsPtr: BINDING_ptr_ptr): XML_Error;
var
  Dtd: DTD_ptr;

  ElementType: ELEMENT_TYPE_ptr;

  NDefaultAtts, AttIndex, PrefixLen, I, N, NPrefixes, OldAttsSize, J,
    NsAttsSize: Integer;

  Version, UriHash, Mask: Int32u;

  Step: Int8u;

  AppAtts: XML_PPAnsiChar; { the attribute list for the application }

  Uri, LocalPart, Name, S, S1, S2: XML_PAnsiChar;

  C: XML_Char;

  Binding, B: BINDING_ptr;

  AttId, Id: ATTRIBUTE_ID_ptr;

  Result_: XML_Error;

  IsCdata: XML_Bool;

  Da: DEFAULT_ATTRIBUTE_ptr;

  P: TAG_ptr;

label
  _w0, _w1;

begin
  Dtd := Parser.M_dtd; { save one level of indirection }

  AttIndex := 0;
  NPrefixes := 0;

  { lookup the element type name }
  ElementType := ELEMENT_TYPE_ptr(Lookup(@Dtd.ElementTypes, TagNamePtr.Str, 0));

  if ElementType = nil then
  begin
    name := PoolCopyString(@Dtd.Pool, TagNamePtr.Str);

    if name = nil then
    begin
      Result := XML_ERROR_NO_MEMORY;

      Exit;

    end;

    ElementType := ELEMENT_TYPE_ptr(Lookup(@Dtd.ElementTypes, name,
      SizeOf(ELEMENT_TYPE)));

    if ElementType = nil then
    begin
      Result := XML_ERROR_NO_MEMORY;

      Exit;

    end;

    if (Parser.M_ns <> 0) and (SetElementTypePrefix(Parser, ElementType) = 0)
    then
    begin
      Result := XML_ERROR_NO_MEMORY;

      Exit;

    end;

  end;

  NDefaultAtts := ElementType.NDefaultAtts;

  { get the attributes from the tokenizer }
  N := XmlGetAttributes(Enc, Pointer(AttStr), Parser.M_attsSize, Parser.M_atts);

  if N + NDefaultAtts > Parser.M_attsSize then
  begin
    OldAttsSize := Parser.M_attsSize;
    Parser.M_attsSize := N + NDefaultAtts + INIT_ATTS_SIZE;

    if not Parser.M_mem.Realloc_fcn(Pointer(Parser.M_atts), Parser.M_attsAlloc,
      Parser.M_attsSize * SizeOf(ATTRIBUTE)) then
    begin
      Result := XML_ERROR_NO_MEMORY;

      Exit;

    end
    else
      Parser.M_attsAlloc := Parser.M_attsSize * SizeOf(ATTRIBUTE);

    if N > OldAttsSize then
      XmlGetAttributes(Enc, Pointer(AttStr), N, Parser.M_atts);

  end;

  AppAtts := XML_PPAnsiChar(Parser.M_atts);

  I := 0;

  while I < N do
  begin
    { add the name and value to the attribute list }
    AttId := GetAttributeId(Parser, Enc,
      Pointer(ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I * SizeOf(ATTRIBUTE))
      ^.Name), Pointer(PtrComp(ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I *
      SizeOf(ATTRIBUTE))^.Name) + XmlNameLength(Enc,
      ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I * SizeOf(ATTRIBUTE))^.Name)));

    if AttId = nil then
    begin
      Result := XML_ERROR_NO_MEMORY;

      Exit;

    end;

    { Detect duplicate attributes by their QNames. This does not work when
      namespace processing is turned on and different prefixes for the same
      namespace are used. For this case we have a check further down. }
    if XML_PAnsiChar(PtrComp(AttId.Name) - 1 * SizeOf(XML_Char))^ <> XML_Char(0)
    then
    begin
      if Enc = Parser.M_encoding then
        Parser.M_eventPtr :=
          Pointer(ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I *
          SizeOf(ATTRIBUTE))^.Name);

      Result := XML_ERROR_DUPLICATE_ATTRIBUTE;

      Exit;

    end;

    XML_PAnsiChar(PtrComp(AttId.Name) - 1 * SizeOf(XML_Char))^ := XML_Char(1);

    XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^ :=
      AttId.Name;

    Inc(AttIndex);

    if ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I * SizeOf(ATTRIBUTE))
      ^.Normalized = #0 then
    begin
      IsCdata := XML_TRUE;

      { figure out whether declared as other than CDATA }
      if AttId.MaybeTokenized <> 0 then
      begin
        J := 0;

        while J < NDefaultAtts do
        begin
          if AttId = DEFAULT_ATTRIBUTE_ptr(PtrComp(ElementType.DefaultAtts) + J
            * SizeOf(DEFAULT_ATTRIBUTE))^.Id then
          begin
            IsCdata := DEFAULT_ATTRIBUTE_ptr(PtrComp(ElementType.DefaultAtts) +
              J * SizeOf(DEFAULT_ATTRIBUTE))^.IsCdata;

            Break;

          end;

          Inc(J);

        end;

      end;

      { normalize the attribute value }
      Result_ := StoreAttributeValue(Parser, Enc, IsCdata,
        Pointer(ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I * SizeOf(ATTRIBUTE))
        ^.ValuePtr), Pointer(ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I *
        SizeOf(ATTRIBUTE))^.ValueEnd), @Parser.M_tempPool);

      if Result_ <> XML_Error(0) then
      begin
        Result := Result_;

        Exit;

      end;

      XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^ :=
        PoolStart(@Parser.M_tempPool);

      PoolFinish(@Parser.M_tempPool);

    end
    else
    begin
      { the value did not need normalizing }
      XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^ :=
        PoolStoreString(@Parser.M_tempPool, Enc,
        Pointer(ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I * SizeOf(ATTRIBUTE))
        ^.ValuePtr), Pointer(ATTRIBUTE_ptr(PtrComp(Parser.M_atts) + I *
        SizeOf(ATTRIBUTE))^.ValueEnd));

      if XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^ = nil
      then
      begin
        Result := XML_ERROR_NO_MEMORY;

        Exit;

      end;

      PoolFinish(@Parser.M_tempPool);

    end;

    { handle prefixed attribute names }
    if AttId.Prefix <> nil then
      if AttId.Xmlns <> 0 then
      begin
        { deal with namespace declarations here }
        Result_ := AddBinding(Parser, AttId.Prefix, AttId,
          XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^,
          BindingsPtr);

        if Result_ <> XML_Error(0) then
        begin
          Result := Result_;

          Exit;

        end;

        Dec(AttIndex);

      end
      else
      begin
        { deal with other prefixed names later }
        Inc(AttIndex);
        Inc(NPrefixes);

        XML_PAnsiChar(PtrComp(AttId.Name) - 1 * SizeOf(XML_Char))^ :=
          XML_Char(2);

      end
    else
      Inc(AttIndex);

    Inc(I);

  end;

  { set-up for XML_GetSpecifiedAttributeCount and XML_GetIdAttributeIndex }
  Parser.M_nSpecifiedAtts := AttIndex;

  if (ElementType.IdAtt <> nil) and
    (XML_PAnsiChar(PtrComp(ElementType.IdAtt.Name) - 1 * SizeOf(XML_Char))^ <>
    XML_Char(0)) then
  begin
    I := 0;

    while I < AttIndex do
    begin
      if XML_PPAnsiChar(PtrComp(AppAtts) + I * SizeOf(XML_PAnsiChar))
        ^ = ElementType.IdAtt.Name then
      begin
        Parser.M_idAttIndex := I;

        Break;

      end;

      Inc(I, 2);

    end;

  end
  else
    Parser.M_idAttIndex := -1;

  { do attribute defaulting }
  I := 0;

  while I < NDefaultAtts do
  begin
    Da := DEFAULT_ATTRIBUTE_ptr(PtrComp(ElementType.DefaultAtts) + I *
      SizeOf(DEFAULT_ATTRIBUTE));

    if (XML_PAnsiChar(PtrComp(Da.Id.Name) - 1 * SizeOf(XML_Char))^ = XML_Char(0)
      ) and (Da.Value <> nil) then
      if Da.Id.Prefix <> nil then
        if Da.Id.Xmlns <> 0 then
        begin
          Result_ := AddBinding(Parser, Da.Id.Prefix, Da.Id, Da.Value,
            BindingsPtr);

          if Result_ <> XML_Error(0) then
          begin
            Result := Result_;

            Exit;

          end;

        end
        else
        begin
          XML_PAnsiChar(PtrComp(Da.Id.Name) - 1 * SizeOf(XML_Char))^ :=
            XML_Char(2);

          Inc(NPrefixes);

          XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^
            := Da.Id.Name;

          Inc(AttIndex);

          XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^
            := Da.Value;

          Inc(AttIndex);

        end
      else
      begin
        XML_PAnsiChar(PtrComp(Da.Id.Name) - 1 * SizeOf(XML_Char))^ :=
          XML_Char(1);

        XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^ :=
          Da.Id.Name;

        Inc(AttIndex);

        XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^
          := Da.Value;

        Inc(AttIndex);

      end;

    Inc(I);

  end;

  XML_PPAnsiChar(PtrComp(AppAtts) + AttIndex * SizeOf(XML_PAnsiChar))^ := nil;

  { expand prefixed attribute names, check for duplicates,
    and clear flags that say whether attributes were specified }
  I := 0;

  if NPrefixes <> 0 then
  begin
    { j = hash table index }
    Version := Parser.M_nsAttsVersion;
    NsAttsSize := 1 shl Parser.M_nsAttsPower;

    { size of hash table must be at least 2 * (# of prefixed attributes) }
    if ShrInt32(NPrefixes shl 1, Parser.M_nsAttsPower) <> 0
    then { true for nsAttsPower = 0 }
    begin
      { hash table size must also be a power of 2 and >= 8 }
      while ShrInt32(NPrefixes, Parser.M_nsAttsPower) <> 0 do
        Inc(Parser.M_nsAttsPower);

      if Parser.M_nsAttsPower < 3 then
        Parser.M_nsAttsPower := 3;

      NsAttsSize := 1 shl Parser.M_nsAttsPower;

      if not Parser.M_mem.Realloc_fcn(Pointer(Parser.M_nsAtts),
        Parser.M_nsAttsAlloc, NsAttsSize * SizeOf(NS_ATT)) then
      begin
        Result := XML_ERROR_NO_MEMORY;

        Exit;

      end
      else
        Parser.M_nsAttsAlloc := NsAttsSize * SizeOf(NS_ATT);

      Version := 0; { force re-initialization of nsAtts hash table }

    end;

    { using a version flag saves us from initializing nsAtts every time }
    if Version = 0 then { initialize version flags when version wraps around }
    begin
      Version := INIT_ATTS_VERSION;

      J := NsAttsSize;

      while J <> 0 do
      begin
        Dec(J);

        NS_ATT_ptr(PtrComp(Parser.M_nsAtts) + J * SizeOf(NS_ATT))^.Version
          := Version;

      end;

    end;

    Dec(Version);

    Parser.M_nsAttsVersion := Version;

    { expand prefixed names and check for duplicates }
    while I < AttIndex do
    begin
      S := XML_PPAnsiChar(PtrComp(AppAtts) + I * SizeOf(XML_PAnsiChar))^;

      if XML_PAnsiChar(PtrComp(S) - 1 * SizeOf(XML_Char))^ = XML_Char(2)
      then { prefixed }
      begin
        UriHash := 0;

        XML_PAnsiChar(PtrComp(S) - 1 * SizeOf(XML_Char))^ := XML_Char(0);
        { clear flag }

        Id := ATTRIBUTE_ID_ptr(Lookup(@Dtd.AttributeIds, S, 0));
        B := Id.Prefix.Binding;

        if B = nil then
        begin
          Result := XML_ERROR_UNBOUND_PREFIX;

          Exit;

        end;

        { as we expand the name we also calculate its hash value }
        J := 0;

        while J < B.UriLen do
        begin
          C := XML_PAnsiChar(PtrComp(B.Uri) + J * SizeOf(XML_Char))^;

          if PoolAppendChar(@Parser.M_tempPool, C) = 0 then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          UriHash := CHAR_HASH(UriHash, C);

          Inc(J);

        end;

        while S^ <> XML_T(':') do
          Inc(PtrComp(S), SizeOf(XML_Char));

        goto _w0;

        while S^ <> XML_Char(0) do { copies null terminator }
        begin
        _w0:
          C := S^;

          if PoolAppendChar(@Parser.M_tempPool, S^) = 0 then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          UriHash := CHAR_HASH(UriHash, C);

          Inc(PtrComp(S), SizeOf(XML_Char));

        end;

        { Check hash table for duplicate of expanded name (uriName).
          Derived from code in lookup(HASH_TABLE *table, ...). }
        Step := 0;
        Mask := NsAttsSize - 1;
        J := UriHash and Mask; { index into hash table }

        while NS_ATT_ptr(PtrComp(Parser.M_nsAtts) + J * SizeOf(NS_ATT))
          ^.Version = Version do
        begin
          { for speed we compare stored hash values first }
          if UriHash = NS_ATT_ptr(PtrComp(Parser.M_nsAtts) + J * SizeOf(NS_ATT))
            ^.Hash then
          begin
            S1 := PoolStart(@Parser.M_tempPool);
            S2 := NS_ATT_ptr(PtrComp(Parser.M_nsAtts) + J * SizeOf(NS_ATT)
              )^.UriName;

            { s1 is null terminated, but not s2 }
            while (S1^ = S2^) and (S1^ <> XML_Char(0)) do
            begin
              Inc(PtrComp(S1), SizeOf(XML_Char));
              Inc(PtrComp(S2), SizeOf(XML_Char));

            end;

            if S1^ = XML_Char(0) then
            begin
              Result := XML_ERROR_DUPLICATE_ATTRIBUTE;

              Exit;

            end;

          end;

          if Step = 0 then
            Step := PROBE_STEP(UriHash, Mask, Parser.M_nsAttsPower);

          if J < Step then
            Inc(J, NsAttsSize - Step)
          else
            Dec(J, Step);

        end;

        if Parser.M_ns_triplets <> 0
        then { append namespace separator and prefix }
        begin
          XML_PAnsiChar(PtrComp(Parser.M_tempPool.Ptr) - 1 * SizeOf(XML_Char))^
            := Parser.M_namespaceSeparator;

          S := B.Prefix.Name;

          goto _w1;

          while S^ <> XML_Char(0) do
          begin
          _w1:
            if PoolAppendChar(@Parser.M_tempPool, S^) = 0 then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            Inc(PtrComp(S), SizeOf(XML_Char));

          end;

        end;

        { store expanded name in attribute list }
        S := PoolStart(@Parser.M_tempPool);

        PoolFinish(@Parser.M_tempPool);

        XML_PPAnsiChar(PtrComp(AppAtts) + I * SizeOf(XML_PAnsiChar))^ := S;

        { fill empty slot with new version, uriName and hash value }
        NS_ATT_ptr(PtrComp(Parser.M_nsAtts) + J * SizeOf(NS_ATT))^.Version
          := Version;
        NS_ATT_ptr(PtrComp(Parser.M_nsAtts) + J * SizeOf(NS_ATT))^.Hash
          := UriHash;
        NS_ATT_ptr(PtrComp(Parser.M_nsAtts) + J * SizeOf(NS_ATT))^.UriName := S;

        Dec(NPrefixes);

        if NPrefixes = 0 then
        begin
          Inc(I, 2);

          Break;

        end;

      end
      else { not prefixed }
        XML_PAnsiChar(PtrComp(S) - 1 * SizeOf(XML_Char))^ := XML_Char(0);
      { clear flag }

      Inc(I, 2);

    end;

  end;

  { clear flags for the remaining attributes }
  while I < AttIndex do
  begin
    XML_PAnsiChar(PtrComp(XML_PPAnsiChar(PtrComp(AppAtts) + I *
      SizeOf(XML_PAnsiChar))^) - 1 * SizeOf(XML_Char))^ := XML_Char(0);

    Inc(I, 2);

  end;

  Binding := BindingsPtr^;

  while Binding <> nil do
  begin
    XML_PAnsiChar(PtrComp(Binding.AttId.Name) - 1 * SizeOf(XML_Char))^ :=
      XML_Char(0);

    Binding := Binding.NextTagBinding;

  end;

  if Parser.M_ns = 0 then
  begin
    Result := XML_ERROR_NONE;

    Exit;

  end;

  { expand the element type name }
  if ElementType.Prefix <> nil then
  begin
    Binding := ElementType.Prefix.Binding;

    if Binding = nil then
    begin
      Result := XML_ERROR_UNBOUND_PREFIX;

      Exit;

    end;

    LocalPart := TagNamePtr.Str;

    while LocalPart^ <> XML_T(':') do
      Inc(PtrComp(LocalPart), SizeOf(XML_Char));

  end
  else if Dtd.DefaultPrefix.Binding <> nil then
  begin
    Binding := Dtd.DefaultPrefix.Binding;
    LocalPart := TagNamePtr.Str;

  end
  else
  begin
    Result := XML_ERROR_NONE;

    Exit;

  end;

  PrefixLen := 0;

  if (Parser.M_ns_triplets <> 0) and (Binding.Prefix.Name <> nil) then
  begin
    while XML_PAnsiChar(PtrComp(Binding.Prefix.Name) + PrefixLen *
      SizeOf(XML_Char))^ <> XML_Char(0) do
      Inc(PrefixLen);

    Inc(PrefixLen); { prefixLen includes null terminator }

  end;

  TagNamePtr.LocalPart := LocalPart;
  TagNamePtr.UriLen := Binding.UriLen;
  TagNamePtr.Prefix := Binding.Prefix.Name;
  TagNamePtr.PrefixLen := PrefixLen;

  I := 0;

  while XML_PAnsiChar(PtrComp(LocalPart) + I * SizeOf(XML_Char))^ <>
    XML_Char(0) do
    Inc(I);

  Inc(I); { i includes null terminator }

  N := I + Binding.UriLen + PrefixLen;

  if N > Binding.UriAlloc then
  begin
    Parser.M_mem.Malloc_fcn(Pointer(Uri), (N + EXPAND_SPARE) *
      SizeOf(XML_Char));

    if Uri = nil then
    begin
      Result := XML_ERROR_NO_MEMORY;

      Exit;

    end;

    J := Binding.UriAlloc;

    Binding.UriAlloc := N + EXPAND_SPARE;

    Move(Binding.Uri^, Uri^, Binding.UriLen * SizeOf(XML_Char));

    P := Parser.M_tagStack;

    while P <> nil do
    begin
      if P.Name.Str = Binding.Uri then
        P.Name.Str := Uri;

      P := P.Parent;

    end;

    Parser.M_mem.Free_fcn(Pointer(Binding.Uri), J * SizeOf(XML_Char));

    Binding.Uri := Uri;

  end;

  { if namespaceSeparator != '\0' then uri includes it already }
  Uri := XML_PAnsiChar(PtrComp(Binding.Uri) + Binding.UriLen *
    SizeOf(XML_Char));

  Move(LocalPart^, Uri^, I * SizeOf(XML_Char));

  { we always have a namespace separator between localPart and prefix }
  if PrefixLen <> 0 then
  begin
    Inc(PtrComp(Uri), (I - 1) * SizeOf(XML_Char));

    Uri^ := Parser.M_namespaceSeparator; { replace null terminator }

    Move(Binding.Prefix.Name^, XML_PAnsiChar(PtrComp(Uri) + 1 * SizeOf(XML_Char)
      )^, PrefixLen * SizeOf(XML_Char));

  end;

  TagNamePtr.Str := Binding.Uri;

  Result := XML_ERROR_NONE;

end;

{ processInternalEntity {.. }
function ProcessInternalEntity(Parser: XML_Parser; Entity: ENTITY_ptr;
  BetweenDecl: XML_Bool): XML_Error;
begin
end;

{ epilogProcessor }
function EpilogProcessor(Parser: XML_Parser; S, End_: PAnsiChar;
  NextPtr: PPAnsiChar): XML_Error;
var
  Next: PAnsiChar;

  Tok: Integer;

begin
  Parser.M_processor := @EpilogProcessor;
  Parser.M_eventPtr := S;

  repeat
    Next := nil;
    Tok := XmlPrologTok(Parser.M_encoding, Pointer(S), Pointer(End_), @Next);

    Parser.M_eventEndPtr := Next;

    case Tok of
      - XML_TOK_PROLOG_S:
        begin
          if @Parser.M_defaultHandler <> nil then
          begin
            ReportDefault(Parser, Parser.M_encoding, S, Next);

            if Parser.M_parsingStatus.Parsing = XML_FINISHED then
            begin
              Result := XML_ERROR_ABORTED;

              Exit;

            end;

          end;

          NextPtr^ := Next;
          Result := XML_ERROR_NONE;

          Exit;

        end;

      XML_TOK_NONE:
        begin
          NextPtr^ := S;
          Result := XML_ERROR_NONE;

          Exit;

        end;

      XML_TOK_PROLOG_S:
        if @Parser.M_defaultHandler <> nil then
          ReportDefault(Parser, Parser.M_encoding, S, Next);

      XML_TOK_PI:
        if ReportProcessingInstruction(Parser, Parser.M_encoding, S, Next) = 0
        then
        begin
          Result := XML_ERROR_NO_MEMORY;

          Exit;

        end;

      XML_TOK_COMMENT:
        if ReportComment(Parser, Parser.M_encoding, S, Next) = 0 then
        begin
          Result := XML_ERROR_NO_MEMORY;

          Exit;

        end;

      XML_TOK_INVALID:
        begin
          Parser.M_eventPtr := Next;

          Result := XML_ERROR_INVALID_TOKEN;

          Exit;

        end;

      XML_TOK_PARTIAL:
        begin
          if Parser.M_parsingStatus.FinalBuffer = 0 then
          begin
            NextPtr^ := S;
            Result := XML_ERROR_NONE;

            Exit;

          end;

          Result := XML_ERROR_UNCLOSED_TOKEN;

          Exit;

        end;

      XML_TOK_PARTIAL_CHAR:
        begin
          if Parser.M_parsingStatus.FinalBuffer = 0 then
          begin
            NextPtr^ := S;
            Result := XML_ERROR_NONE;

            Exit;

          end;

          Result := XML_ERROR_PARTIAL_CHAR;

          Exit;

        end;

    else
      begin
        Result := XML_ERROR_JUNK_AFTER_DOC_ELEMENT;

        Exit;

      end;

    end;

    Parser.M_eventPtr := Next;

    S := Next;

    case Parser.M_parsingStatus.Parsing of
      XML_SUSPENDED:
        begin
          NextPtr^ := Next;
          Result := XML_ERROR_NONE;

          Exit;

        end;

      XML_FINISHED:
        begin
          Result := XML_ERROR_ABORTED;

          Exit;

        end;

    end;

  until False;

end;

{ doCdataSection {.. }
{ startPtr gets set to non-null if the section is closed, and to null if
  the section is not yet closed. }
function DoCdataSection(Parser: XML_Parser; Enc: ENCODING_ptr;
  StartPtr: PPAnsiChar; End_: PAnsiChar; NextPtr: PPAnsiChar;
  HaveMore: XML_Bool): XML_Error;
begin
end;

{ cdataSectionProcessor {.. }
{ The idea here is to avoid using stack for each CDATA section when
  the whole file is parsed with one call. }
function CdataSectionProcessor(Parser: XML_Parser; Start, End_: PAnsiChar;
  EndPtr: PPAnsiChar): XML_Error;
begin
end;

{ doContent }
function DoContent(Parser: XML_Parser; StartTagLevel: Integer; Enc: ENCODING_ptr;
  S, End_: PAnsiChar; NextPtr: PPAnsiChar; HaveMore: XML_Bool): XML_Error;
var
  Dtd: DTD_ptr;

  EventPP, EventEndPP: PPAnsiChar;

  Next, RawNameEnd, FromPtr, Temp, RawName: PAnsiChar;

  Tok, BufSize, ConvLen, Len, N: Integer;

  C, Ch: XML_Char;

  Name, Context, ToPtr, LocalPart, Prefix, Uri: XML_PAnsiChar;

  Entity: ENTITY_ptr;

  Result_: XML_Error;

  Tag: TAG_ptr;

  Bindings, B: BINDING_ptr;

  NoElmHandlers: XML_Bool;

  Name_: TAG_NAME;

  Buf: array [0 .. XML_ENCODE_MAX - 1] of XML_Char;

  DataPtr: IPAnsiChar;

label
  _break;

begin
  { save one level of indirection }
  Dtd := Parser.M_dtd;

  if Enc = Parser.M_encoding then
  begin
    EventPP := @Parser.M_eventPtr;
    EventEndPP := @Parser.M_eventEndPtr;

  end
  else
  begin
    EventPP := @Parser.M_openInternalEntities.InternalEventPtr;
    EventEndPP := @Parser.M_openInternalEntities.InternalEventEndPtr;

  end;

  EventPP^ := S;

  repeat
    Next := S; { XmlContentTok doesn't always set the last arg }

    Tok := XmlContentTok(Enc, Pointer(S), Pointer(End_), @Next);

    EventEndPP^ := Next;

    case Tok of
      XML_TOK_TRAILING_CR:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := XML_ERROR_NONE;

            Exit;

          end;

          EventEndPP^ := End_;

          if @Parser.M_characterDataHandler <> nil then
          begin
            C := XML_Char($A);

            Parser.M_characterDataHandler(Parser.M_handlerArg, @C, 1);

          end
          else if @Parser.M_defaultHandler <> nil then
            ReportDefault(Parser, Enc, S, End_);

          { We are at the end of the final buffer, should we check for
            XML_SUSPENDED, XML_FINISHED? }
          if StartTagLevel = 0 then
          begin
            Result := XML_ERROR_NO_ELEMENTS;

            Exit;

          end;

          if Parser.M_tagLevel <> StartTagLevel then
          begin
            Result := XML_ERROR_ASYNC_ENTITY;

            Exit;

          end;

          NextPtr^ := End_;
          Result := XML_ERROR_NONE;

          Exit;

        end;

      XML_TOK_NONE:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := XML_ERROR_NONE;

            Exit;

          end;

          if StartTagLevel > 0 then
          begin
            if Parser.M_tagLevel <> StartTagLevel then
            begin
              Result := XML_ERROR_ASYNC_ENTITY;

              Exit;

            end;

            NextPtr^ := S;
            Result := XML_ERROR_NONE;

            Exit;

          end;

          Result := XML_ERROR_NO_ELEMENTS;

          Exit;

        end;

      XML_TOK_INVALID:
        begin
          EventPP^ := Next;
          Result := XML_ERROR_INVALID_TOKEN;

          Exit;

        end;

      XML_TOK_PARTIAL:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := XML_ERROR_NONE;

            Exit;

          end;

          Result := XML_ERROR_UNCLOSED_TOKEN;

          Exit;

        end;

      XML_TOK_PARTIAL_CHAR:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := XML_ERROR_NONE;

            Exit;

          end;

          Result := XML_ERROR_PARTIAL_CHAR;

          Exit;

        end;

      XML_TOK_ENTITY_REF:
        begin
          Ch := XML_Char(XmlPredefinedEntityName(Enc,
            Pointer(PtrComp(S) + Enc.MinBytesPerChar),
            Pointer(PtrComp(Next) - Enc.MinBytesPerChar)));

          if Ch <> XML_Char(0) then
          begin
            if @Parser.M_characterDataHandler <> nil then
              Parser.M_characterDataHandler(Parser.M_handlerArg, @Ch, 1)
            else if @Parser.M_defaultHandler <> nil then
              ReportDefault(Parser, Enc, S, Next);

            goto _break;

          end;

          name := PoolStoreString(@Dtd.Pool, Enc,
            PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

          if name = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          Entity := ENTITY_ptr(Lookup(@Dtd.GeneralEntities, name, 0));

          PoolDiscard(@Dtd.Pool);

          { First, determine if a check for an existing declaration is needed;
            if yes, check that the entity exists, and that it is internal,
            otherwise call the skipped entity or default handler. }
          if (Dtd.HasParamEntityRefs = 0) or (Dtd.Standalone <> 0) then
            if Entity = nil then
            begin
              Result := XML_ERROR_UNDEFINED_ENTITY;

              Exit;

            end
            else if Entity.IsInternal = 0 then
            begin
              Result := XML_ERROR_ENTITY_DECLARED_IN_PE;

              Exit;

            end
            else
          else if Entity = nil then
          begin
            if @Parser.M_skippedEntityHandler <> nil then
              Parser.M_skippedEntityHandler(Parser.M_handlerArg, name, 0)
            else if @Parser.M_defaultHandler <> nil then
              ReportDefault(Parser, Enc, S, Next);

            goto _break;

          end;

          if Entity.Open <> 0 then
          begin
            Result := XML_ERROR_RECURSIVE_ENTITY_REF;

            Exit;

          end;

          if Entity.Notation <> nil then
          begin
            Result := XML_ERROR_BINARY_ENTITY_REF;

            Exit;

          end;

          if Entity.TextPtr <> nil then
          begin
            if Parser.M_defaultExpandInternalEntities <> 0 then
            begin
              if @Parser.M_skippedEntityHandler <> nil then
                Parser.M_skippedEntityHandler(Parser.M_handlerArg,
                  Entity.Name, 0)
              else if @Parser.M_defaultHandler <> nil then
                ReportDefault(Parser, Enc, S, Next);

              goto _break;

            end;

            Result_ := ProcessInternalEntity(Parser, Entity, XML_FALSE);

            if Result_ <> XML_ERROR_NONE then
            begin
              Result := Result_;

              Exit;

            end;

          end
          else if @Parser.M_externalEntityRefHandler <> nil then
          begin
            Entity.Open := XML_TRUE;
            Context := GetContext(Parser);
            Entity.Open := XML_FALSE;

            if Context = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            if Parser.M_externalEntityRefHandler
              (Parser.M_externalEntityRefHandlerArg, Context, Entity.Base,
              Entity.SystemId, Entity.PublicId) = 0 then
            begin
              Result := XML_ERROR_EXTERNAL_ENTITY_HANDLING;

              Exit;

            end;

            PoolDiscard(@Parser.M_tempPool);

          end
          else if @Parser.M_defaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

        end;

      XML_TOK_START_TAG_NO_ATTS, XML_TOK_START_TAG_WITH_ATTS:
        begin
          if Parser.M_freeTagList <> nil then
          begin
            Tag := Parser.M_freeTagList;

            Parser.M_freeTagList := Parser.M_freeTagList.Parent;

          end
          else
          begin
            Parser.M_mem.Malloc_fcn(Pointer(Tag), SizeOf(Expat.TAG));

            if Tag = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            Parser.M_mem.Malloc_fcn(Pointer(Tag.Buf), INIT_TAG_BUF_SIZE);

            if Tag.Buf = nil then
            begin
              Parser.M_mem.Free_fcn(Pointer(Tag), SizeOf(Expat.TAG));

              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end
            else
              Tag.Alloc := INIT_TAG_BUF_SIZE;

            Tag.BufEnd := PAnsiChar(PtrComp(Tag.Buf) + INIT_TAG_BUF_SIZE);

          end;

          Tag.Bindings := nil;
          Tag.Parent := Parser.M_tagStack;
          Parser.M_tagStack := Tag;
          Tag.Name.LocalPart := nil;
          Tag.Name.Prefix := nil;
          Tag.RawName := PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar);
          Tag.RawNameLength := XmlNameLength(Enc, Pointer(Tag.RawName));

          Inc(Parser.M_tagLevel);

          RawNameEnd := PAnsiChar(PtrComp(Tag.RawName) + Tag.RawNameLength);
          FromPtr := Tag.RawName;
          ToPtr := XML_PAnsiChar(Tag.Buf);

          repeat
            XmlConvert(Enc, @FromPtr, RawNameEnd, IPPAnsiChar(@ToPtr),
              IPAnsiChar(PtrComp(Tag.BufEnd) - 1));

            ConvLen := (PtrComp(ToPtr) - PtrComp(Tag.Buf)) div SizeOf(XML_Char);

            if FromPtr = RawNameEnd then
            begin
              Tag.Name.StrLen := ConvLen;

              Break;

            end;

            BufSize := (PtrComp(Tag.BufEnd) - PtrComp(Tag.Buf)) shl 1;

            Parser.M_mem.Realloc_fcn(Pointer(Tag.Buf), Tag.Alloc, BufSize);

            if Temp = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end
            else
              Tag.Alloc := BufSize;

            Tag.Buf := Temp;
            Tag.BufEnd := PAnsiChar(PtrComp(Temp) + BufSize);

            ToPtr := XML_PAnsiChar(PtrComp(Temp) + ConvLen);

          until False;

          Tag.Name.Str := XML_PAnsiChar(Tag.Buf);

          ToPtr^ := XML_T(#0);
          Result_ := StoreAtts(Parser, Enc, S, @Tag.Name, @Tag.Bindings);

          if Result_ <> XML_Error(0) then
          begin
            Result := Result_;

            Exit;

          end;

          if @Parser.FStartElementHandler <> nil then
            Parser.FStartElementHandler(Parser.M_handlerArg, Tag.Name.Str,
              XML_PPAnsiChar(Parser.M_atts))
          else if @Parser.M_defaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

          PoolClear(@Parser.M_tempPool);

        end;

      XML_TOK_EMPTY_ELEMENT_NO_ATTS, XML_TOK_EMPTY_ELEMENT_WITH_ATTS:
        begin
          RawName := PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar);
          Bindings := nil;
          NoElmHandlers := XML_TRUE;

          Name_.Str := PoolStoreString(@Parser.M_tempPool, Enc, RawName,
            PAnsiChar(PtrComp(RawName) + XmlNameLength(Enc, Pointer(RawName))));

          if Name_.Str = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          PoolFinish(@Parser.M_tempPool);

          Result_ := StoreAtts(Parser, Enc, S, @Name_, @Bindings);

          if Result_ <> XML_Error(0) then
          begin
            Result := Result_;

            Exit;

          end;

          PoolFinish(@Parser.M_tempPool);

          if @Parser.FStartElementHandler <> nil then
          begin
            Parser.FStartElementHandler(Parser.M_handlerArg, Name_.Str,
              XML_PPAnsiChar(Parser.M_atts));

            NoElmHandlers := XML_FALSE;

          end;

          if @Parser.M_endElementHandler <> nil then
          begin
            if @Parser.FStartElementHandler <> nil then
              EventPP^ := EventEndPP^;

            Parser.M_endElementHandler(Parser.M_handlerArg, Name_.Str);

            NoElmHandlers := XML_FALSE;

          end;

          if (NoElmHandlers <> 0) and (@Parser.M_defaultHandler <> nil) then
            ReportDefault(Parser, Enc, S, Next);

          PoolClear(@Parser.M_tempPool);

          while Bindings <> nil do
          begin
            B := Bindings;

            if @Parser.M_endNamespaceDeclHandler <> nil then
              Parser.M_endNamespaceDeclHandler(Parser.M_handlerArg,
                B.Prefix.Name);

            Bindings := Bindings.NextTagBinding;
            B.NextTagBinding := Parser.M_freeBindingList;

            Parser.M_freeBindingList := B;
            B.Prefix.Binding := B.PrevPrefixBinding;

          end;

          if Parser.M_tagLevel = 0 then
          begin
            Result := EpilogProcessor(Parser, Next, End_, NextPtr);

            Exit;

          end;

        end;

      XML_TOK_END_TAG:
        if Parser.M_tagLevel = StartTagLevel then
        begin
          Result := XML_ERROR_ASYNC_ENTITY;

          Exit;

        end
        else
        begin
          Tag := Parser.M_tagStack;
          Parser.M_tagStack := Tag.Parent;
          Tag.Parent := Parser.M_freeTagList;
          Parser.M_freeTagList := Tag;

          RawName := PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar * 2);
          Len := XmlNameLength(Enc, Pointer(RawName));

          if (Len <> Tag.RawNameLength) or
            (Memcmp(Pointer(Tag.RawName), Pointer(RawName), Len) <> 0) then
          begin
            EventPP^ := RawName;
            Result := XML_ERROR_TAG_MISMATCH;

            Exit;

          end;

          Dec(Parser.M_tagLevel);

          if @Parser.M_endElementHandler <> nil then
          begin
            LocalPart := Tag.Name.LocalPart;

            if (Parser.M_ns <> 0) and (LocalPart <> nil) then
            begin
              { localPart and prefix may have been overwritten in
                tag->name.str, since this points to the binding->uri
                buffer which gets re-used; so we have to add them again }
              Uri := XML_PAnsiChar(PtrComp(Tag.Name.Str) + Tag.Name.UriLen);

              { don't need to check for space - already done in storeAtts() }
              while LocalPart^ <> XML_Char(0) do
              begin
                Uri^ := LocalPart^;

                Inc(PtrComp(Uri), SizeOf(XML_Char));
                Inc(PtrComp(LocalPart), SizeOf(XML_Char));

              end;

              Prefix := XML_PAnsiChar(Tag.Name.Prefix);

              if (Parser.M_ns_triplets <> 0) and (Prefix <> nil) then
              begin
                Uri^ := Parser.M_namespaceSeparator;

                Inc(PtrComp(Uri), SizeOf(XML_Char));

                while Prefix^ <> XML_Char(0) do
                begin
                  Uri^ := Prefix^;

                  Inc(PtrComp(Uri), SizeOf(XML_Char));
                  Inc(PtrComp(Prefix), SizeOf(XML_Char));

                end;

              end;

              Uri^ := XML_T(#0);

            end;

            Parser.M_endElementHandler(Parser.M_handlerArg, Tag.Name.Str);

          end
          else if @Parser.M_defaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

          while Tag.Bindings <> nil do
          begin
            B := Tag.Bindings;

            if @Parser.M_endNamespaceDeclHandler <> nil then
              Parser.M_endNamespaceDeclHandler(Parser.M_handlerArg,
                B.Prefix.Name);

            Tag.Bindings := Tag.Bindings.NextTagBinding;
            B.NextTagBinding := Parser.M_freeBindingList;
            Parser.M_freeBindingList := B;
            B.Prefix.Binding := B.PrevPrefixBinding;

          end;

          if Parser.M_tagLevel = 0 then
          begin
            Result := EpilogProcessor(Parser, Next, End_, NextPtr);

            Exit;

          end;

        end;

      XML_TOK_CHAR_REF:
        begin
          N := XmlCharRefNumber(Enc, Pointer(S));

          if N < 0 then
          begin
            Result := XML_ERROR_BAD_CHAR_REF;

            Exit;

          end;

          if @Parser.M_characterDataHandler <> nil then
            Parser.M_characterDataHandler(Parser.M_handlerArg, @Buf[0],
              XmlEncode(N, IPAnsiChar(@Buf)))
          else if @Parser.M_defaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

        end;

      XML_TOK_XML_DECL:
        begin
          Result := XML_ERROR_MISPLACED_XML_PI;

          Exit;

        end;

      XML_TOK_DATA_NEWLINE:
        if @Parser.M_characterDataHandler <> nil then
        begin
          C := XML_Char($A);

          Parser.M_characterDataHandler(Parser.M_handlerArg, @C, 1);

        end
        else if @Parser.M_defaultHandler <> nil then
          ReportDefault(Parser, Enc, S, Next);

      XML_TOK_CDATA_SECT_OPEN:
        begin
          if @Parser.FStartCdataSectionHandler <> nil then
            Parser.FStartCdataSectionHandler(Parser.M_handlerArg)
{$IFDEF 0 }
            { Suppose you doing a transformation on a document that involves
              changing only the AnsiCharacter data.  You set up a defaultHandler
              and a AnsiCharacterDataHandler.  The defaultHandler simply copies
              AnsiCharacters through.  The AnsiCharacterDataHandler does the
              transformation and writes the AnsiCharacters out escaping them as
              necessary.  This case will fail to work if we leave out the
              following two lines (because & and < inside CDATA sections will
              be incorrectly escaped).

              However, now we have a start/endCdataSectionHandler, so it seems
              easier to let the user deal with this. }
          else if @Parser.M_characterDataHandler <> nil then
            Parser.M_characterDataHandler(Parser.M_handlerArg,
              Parser.M_dataBuf, 0)
{$ENDIF }
          else if @Parser.M_defaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

          Result_ := DoCdataSection(Parser, Enc, @Next, End_, NextPtr,
            HaveMore);

          if Result_ <> XML_ERROR_NONE then
          begin
            Result := Result_;

            Exit;

          end
          else if Next = nil then
          begin
            Parser.M_processor := @CdataSectionProcessor;

            Result := Result_;

            Exit;

          end;

        end;

      XML_TOK_TRAILING_RSQB:
        begin
          if HaveMore <> 0 then
          begin
            NextPtr^ := S;
            Result := XML_ERROR_NONE;

            Exit;

          end;

          if @Parser.M_characterDataHandler <> nil then
            if MUSRasterizerConverterERT(Enc, S) <> 0 then
            begin
              DataPtr := IPAnsiChar(Parser.M_dataBuf);

              XmlConvert(Enc, @S, End_, @DataPtr,
                IPAnsiChar(Parser.M_dataBufEnd));

              Parser.M_characterDataHandler(Parser.M_handlerArg,
                Parser.M_dataBuf, (PtrComp(DataPtr) - PtrComp(Parser.M_dataBuf))
                div SizeOf(ICHAR));

            end
            else
              Parser.M_characterDataHandler(Parser.M_handlerArg,
                XML_PAnsiChar(S), (PtrComp(End_) - PtrComp(S))
                div SizeOf(XML_Char))
          else if @Parser.M_defaultHandler <> nil then
            ReportDefault(Parser, Enc, S, End_);

          { We are at the end of the final buffer, should we check for
            XML_SUSPENDED, XML_FINISHED? }
          if StartTagLevel = 0 then
          begin
            EventPP^ := End_;
            Result := XML_ERROR_NO_ELEMENTS;

            Exit;

          end;

          if Parser.M_tagLevel <> StartTagLevel then
          begin
            EventPP^ := End_;
            Result := XML_ERROR_ASYNC_ENTITY;

            Exit;

          end;

          NextPtr^ := End_;
          Result := XML_ERROR_NONE;

          Exit;

        end;

      XML_TOK_DATA_CHARS:
        if @Parser.M_characterDataHandler <> nil then
          if MUSRasterizerConverterERT(Enc, S) <> 0 then
            repeat
              DataPtr := IPAnsiChar(Parser.M_dataBuf);

              XmlConvert(Enc, @S, Next, @DataPtr,
                IPAnsiChar(Parser.M_dataBufEnd));

              EventEndPP^ := S;

              Parser.M_characterDataHandler(Parser.M_handlerArg,
                Parser.M_dataBuf, (PtrComp(DataPtr) - PtrComp(Parser.M_dataBuf))
                div SizeOf(ICHAR));

              if S = Next then
                Break;

              EventPP^ := S;

            until False
          else
            Parser.M_characterDataHandler(Parser.M_handlerArg, XML_PAnsiChar(S),
              (PtrComp(Next) - PtrComp(S)) div SizeOf(XML_Char))

        else if @Parser.M_defaultHandler <> nil then
          ReportDefault(Parser, Enc, S, Next);

      XML_TOK_PI:
        if ReportProcessingInstruction(Parser, Enc, S, Next) = 0 then
        begin
          Result := XML_ERROR_NO_MEMORY;

          Exit;

        end;

      XML_TOK_COMMENT:
        if ReportComment(Parser, Enc, S, Next) = 0 then
        begin
          Result := XML_ERROR_NO_MEMORY;

          Exit;

        end;

    else
      if @Parser.M_defaultHandler <> nil then
        ReportDefault(Parser, Enc, S, Next);

    end;

  _break:
    EventPP^ := Next;
    S := Next;

    case Parser.M_parsingStatus.Parsing of
      XML_SUSPENDED:
        begin
          NextPtr^ := Next;
          Result := XML_ERROR_NONE;

          Exit;

        end;

      XML_FINISHED:
        begin
          Result := XML_ERROR_ABORTED;

          Exit;

        end;

    end;

  until False;

  { not reached }

end;

{ contentProcessor }
function ContentProcessor(Parser: XML_Parser; Start, End_: PAnsiChar;
  EndPtr: PPAnsiChar): XML_Error;
var
  Result_: XML_Error;

begin
  Result_ := DoContent(Parser, 0, Parser.M_encoding, Start, End_, EndPtr,
    XML_Bool(not Parser.M_parsingStatus.FinalBuffer));

  if Result_ = XML_ERROR_NONE then
    if StoreRawNames(Parser) = 0 then
    begin
      Result := XML_ERROR_NO_MEMORY;

      Exit;

    end;

  Result := Result_;

end;

{ getElementType {.. }
function GetElementType(Parser: XML_Parser; Enc: ENCODING_ptr;
  Ptr, End_: PAnsiChar): ELEMENT_TYPE_ptr;
begin
end;

{ getAttributeId }
function GetAttributeId(Parser: XML_Parser; Enc: ENCODING_ptr;
  Start, End_: PAnsiChar): ATTRIBUTE_ID_ptr;
var
  Dtd: DTD_ptr;

  Id: ATTRIBUTE_ID_ptr;

  Name: XML_PAnsiChar;

  I, J: Integer;

begin
  { save one level of indirection }
  Dtd := Parser.M_dtd;

  if PoolAppendChar(@Dtd.Pool, XML_T(#0)) = 0 then
  begin
    Result := nil;

    Exit;

  end;

  name := PoolStoreString(@Dtd.Pool, Enc, Start, End_);

  if name = nil then
  begin
    Result := nil;

    Exit;

  end;

  { skip quotation mark - its storage will be re-used (like in name[-1]) }
  Inc(PtrComp(name), SizeOf(XML_Char));

  Id := ATTRIBUTE_ID_ptr(Lookup(@Dtd.AttributeIds, name, SizeOf(ATTRIBUTE_ID)));

  if Id = nil then
  begin
    Result := nil;

    Exit;

  end;

  if Id.Name <> name then
    PoolDiscard(@Dtd.Pool)

  else
  begin
    PoolFinish(@Dtd.Pool);

    if Parser.M_ns = 0 then
    else if (XML_PAnsiChar(PtrComp(name) + 0 * SizeOf(XML_Char))^ = XML_T('x'))
      and (XML_PAnsiChar(PtrComp(name) + 1 * SizeOf(XML_Char))^ = XML_T('m'))
      and (XML_PAnsiChar(PtrComp(name) + 2 * SizeOf(XML_Char))^ = XML_T('l'))
      and (XML_PAnsiChar(PtrComp(name) + 3 * SizeOf(XML_Char))^ = XML_T('n'))
      and (XML_PAnsiChar(PtrComp(name) + 4 * SizeOf(XML_Char))^ = XML_T('s'))
      and ((XML_PAnsiChar(PtrComp(name) + 5 * SizeOf(XML_Char))^ = XML_T(#0)) or
      (XML_PAnsiChar(PtrComp(name) + 5 * SizeOf(XML_Char))^ = XML_T(':'))) then
    begin
      if XML_PAnsiChar(PtrComp(name) + 5 * SizeOf(XML_Char))^ = XML_T(#0) then
        Id.Prefix := @Dtd.DefaultPrefix
      else
        Id.Prefix := PREFIX_ptr(Lookup(@Dtd.Prefixes,
          XML_PAnsiChar(PtrComp(name) + 6 * SizeOf(XML_Char)), SizeOf(PREFIX)));

      Id.Xmlns := XML_TRUE;

    end
    else
    begin
      I := 0;

      while XML_PAnsiChar(PtrComp(name) + I * SizeOf(XML_Char))^ <>
        XML_Char(0) do
      begin
        { attributes without prefix are *not* in the default namespace }
        if XML_PAnsiChar(PtrComp(name) + I * SizeOf(XML_Char))^ = XML_T(':')
        then
        begin
          J := 0;

          while J < I do
          begin
            if PoolAppendChar(@Dtd.Pool,
              XML_PAnsiChar(PtrComp(name) + J * SizeOf(XML_Char))^) = 0 then
            begin
              Result := nil;

              Exit;

            end;

            Inc(J);

          end;

          if PoolAppendChar(@Dtd.Pool, XML_T(#0)) = 0 then
          begin
            Result := nil;

            Exit;

          end;

          Id.Prefix := PREFIX_ptr(Lookup(@Dtd.Prefixes, PoolStart(@Dtd.Pool),
            SizeOf(PREFIX)));

          if Id.Prefix.Name = PoolStart(@Dtd.Pool) then
            PoolFinish(@Dtd.Pool)
          else
            PoolDiscard(@Dtd.Pool);

          Break;

        end;

        Inc(I);

      end;

    end;

  end;

  Result := Id;

end;

{ defineAttribute {.. }
function DefineAttribute(Type_: ELEMENT_TYPE_ptr; AttId: ATTRIBUTE_ID_ptr;
  IsCdata, IsId: XML_Bool; Value: XML_PAnsiChar; Parser: XML_Parser): Integer;
begin
end;

{ appendAttributeValue }
function AppendAttributeValue(Parser: XML_Parser; Enc: ENCODING_ptr;
  IsCdata: XML_Bool; Ptr, End_: PAnsiChar; Pool: STRING_POOL_ptr): XML_Error;
var
  Dtd: DTD_ptr;

  Next: PAnsiChar;

  Tok, I, N: Integer;

  Buf: array [0 .. XML_ENCODE_MAX - 1] of XML_Char;

  Name, TextEnd: XML_PAnsiChar;

  Entity: ENTITY_ptr;

  CheckEntityDecl: AnsiChar;

  Ch: XML_Char;

  Result_: XML_Error;

label
  _break, _go0;

begin
  { save one level of indirection }
  Dtd := Parser.M_dtd;

  repeat
    Tok := XmlAttributeValueTok(Enc, Pointer(Ptr), Pointer(End_), @Next);

    case Tok of
      XML_TOK_NONE:
        begin
          Result := XML_ERROR_NONE;

          Exit;

        end;

      XML_TOK_INVALID:
        begin
          if Enc = Parser.M_encoding then
            Parser.M_eventPtr := Next;

          Result := XML_ERROR_INVALID_TOKEN;

        end;

      XML_TOK_PARTIAL:
        begin
          if Enc = Parser.M_encoding then
            Parser.M_eventPtr := Ptr;

          Result := XML_ERROR_INVALID_TOKEN;

        end;

      XML_TOK_CHAR_REF:
        begin
          N := XmlCharRefNumber(Enc, Pointer(Ptr));

          if N < 0 then
          begin
            if Enc = Parser.M_encoding then
              Parser.M_eventPtr := Ptr;

            Result := XML_ERROR_BAD_CHAR_REF;

          end;

          if (IsCdata = 0) and (N = $20) and { space }
            ((PoolLength(Pool) = 0) or (PoolLastChar(Pool) = XML_Char($20)))
          then
            goto _break;

          N := XmlEncode(N, IPAnsiChar(Buf));

          if N = 0 then
          begin
            if Enc = Parser.M_encoding then
              Parser.M_eventPtr := Ptr;

            Result := XML_ERROR_BAD_CHAR_REF;

            Exit;

          end;

          I := 0;

          while I < N do
          begin
            if PoolAppendChar(Pool, Buf[I]) = 0 then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            Inc(I);

          end;

        end;

      XML_TOK_DATA_CHARS:
        if PoolAppend(Pool, Enc, Ptr, Next) = nil then
        begin
          Result := XML_ERROR_NO_MEMORY;

          Exit;

        end;

      XML_TOK_TRAILING_CR:
        begin
          Next := PAnsiChar(PtrComp(Ptr) + Enc.MinBytesPerChar);

          goto _go0;

        end;

      XML_TOK_ATTRIBUTE_VALUE_S, XML_TOK_DATA_NEWLINE:
      _go0:
        begin
          if (IsCdata = 0) and
            ((PoolLength(Pool) = 0) or (PoolLastChar(Pool) = XML_Char($20)))
          then
            goto _break;

          if PoolAppendChar(Pool, AnsiChar($20)) = 0 then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

        end;

      XML_TOK_ENTITY_REF:
        begin
          Ch := XML_Char(XmlPredefinedEntityName(Enc,
            Pointer(PAnsiChar(PtrComp(Ptr) + Enc.MinBytesPerChar)),
            Pointer(PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar))));

          if Ch <> XML_Char(0) then
          begin
            if PoolAppendChar(Pool, Ch) = 0 then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            goto _break;

          end;

          name := PoolStoreString(@Parser.M_temp2Pool, Enc,
            PAnsiChar(PtrComp(Ptr) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

          if name = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          Entity := ENTITY_ptr(Lookup(@Parser.M_dtd.GeneralEntities, name, 0));

          PoolDiscard(@Parser.M_temp2Pool);

          { First, determine if a check for an existing declaration is needed;
            if yes, check that the entity exists, and that it is internal. }
          if Pool = @Parser.M_dtd.Pool then { are we called from prolog? }
          begin
            if Dtd.Standalone <> 0 then
              CheckEntityDecl := AnsiChar(Parser.M_openInternalEntities = nil)
            else
              CheckEntityDecl := AnsiChar(Dtd.HasParamEntityRefs = 0);

{$IFDEF XML_DTD }
            CheckEntityDecl := AnsiChar((CheckEntityDecl <> #0) and
              (Parser.M_prologState.DocumentEntity <> 0))

{$ENDIF }
          end
          else { if pool = @tempPool: we are called from content }
            CheckEntityDecl := AnsiChar((Dtd.HasParamEntityRefs = 0) or
              (Dtd.Standalone <> 0));

          if CheckEntityDecl <> #0 then
            if Entity = nil then
            begin
              Result := XML_ERROR_UNDEFINED_ENTITY;

              Exit;

            end
            else if Entity.IsInternal = 0 then
            begin
              Result := XML_ERROR_ENTITY_DECLARED_IN_PE;

              Exit;

            end
            else
          else if Entity = nil then
            { Cannot report skipped entity here - see comments on
              skippedEntityHandler.
              if @parser.m_skippedEntityHandler <> NIL then
              parser.m_skippedEntityHandler(parser.m_handlerArg ,name ,0 ); }
            { Cannot call the default handler because this would be
              out of sync with the call to the startElementHandler.
              if (pool = @parser.m_tempPool ) and
              (@parser.m_defaultHandler <> NIL ) then
              reportDefault(parser ,enc ,ptr ,next ); }
            goto _break;

          if Entity.Open <> 0 then
          begin
            if Enc = Parser.M_encoding then
              Parser.M_eventPtr := Ptr;

            Result := XML_ERROR_RECURSIVE_ENTITY_REF;

            Exit;

          end;

          if Entity.Notation <> nil then
          begin
            if Enc = Parser.M_encoding then
              Parser.M_eventPtr := Ptr;

            Result := XML_ERROR_BINARY_ENTITY_REF;

            Exit;

          end;

          if Entity.TextPtr = nil then
          begin
            if Enc = Parser.M_encoding then
              Parser.M_eventPtr := Ptr;

            Result := XML_ERROR_ATTRIBUTE_EXTERNAL_ENTITY_REF;

            Exit;

          end
          else
          begin
            TextEnd := XML_PAnsiChar(PtrComp(Entity.TextPtr) + Entity.TextLen *
              SizeOf(XML_Char));

            Entity.Open := XML_TRUE;

            Result_ := AppendAttributeValue(Parser, Parser.MInternalEncoding,
              IsCdata, PAnsiChar(Entity.TextPtr), PAnsiChar(TextEnd), Pool);

            Entity.Open := XML_FALSE;

            if Result_ <> XML_Error(0) then
            begin
              Result := Result_;

              Exit;

            end;

          end;

        end;

    else
      begin
        if Enc = Parser.M_encoding then
          Parser.M_eventPtr := Ptr;

        Result := XML_ERROR_UNEXPECTED_STATE;

        Exit;

      end;

    end;

  _break:
    Ptr := Next;

  until False;

  { not reached }

end;

{ storeAttributeValue }
function StoreAttributeValue(Parser: XML_Parser; Enc: ENCODING_ptr;
  IsCdata: XML_Bool; Ptr, End_: PAnsiChar; Pool: STRING_POOL_ptr): XML_Error;
var
  Result_: XML_Error;

begin
  Result_ := AppendAttributeValue(Parser, Enc, IsCdata, Ptr, End_, Pool);

  if Result_ <> XML_Error(0) then
  begin
    Result := Result_;

    Exit;

  end;

  if (IsCdata = 0) and (PoolLength(Pool) <> 0) and
    (PoolLastChar(Pool) = XML_Char($20)) then
    PoolChop(Pool);

  if PoolAppendChar(Pool, XML_T(#0)) = 0 then
  begin
    Result := XML_ERROR_NO_MEMORY;

    Exit;

  end;

  Result := XML_ERROR_NONE;

end;

{ storeEntityValue {.. }
function StoreEntityValue(Parser: XML_Parser; Enc: ENCODING_ptr;
  Start, End_: PAnsiChar): XML_Error;
begin
end;

{ doIgnoreSection {.. }
{ startPtr gets set to non-null is the section is closed, and to null
  if the section is not yet closed. }
function DoIgnoreSection(Parser: XML_Parser; Enc: ENCODING_ptr;
  StartPtr: PPAnsiChar; End_: PAnsiChar; NextPtr: PPAnsiChar;
  HaveMore: XML_Bool): XML_Error;
begin
end;

{ ignoreSectionProcessor {.. }
{ The idea here is to avoid using stack for each IGNORE section when
  the whole file is parsed with one call. }
function IgnoreSectionProcessor(Parser: XML_Parser; Start, End_: PAnsiChar;
  EndPtr: PPAnsiChar): XML_Error;
begin
end;

{ nextScaffoldPart {.. }
function NextScaffoldPart(Parser: XML_Parser): Integer;
begin
end;

{ build_model {.. }
function Build_model(Parser: XML_Parser): XML_Content_ptr;
begin
end;

{ reportProcessingInstruction {.. }
function ReportProcessingInstruction(Parser: XML_Parser; Enc: ENCODING_ptr;
  Start, End_: PAnsiChar): Integer;
begin
end;

{ normalizeLines {.. }
procedure NormalizeLines(S: XML_PAnsiChar);
begin
end;

{ reportComment }
function ReportComment(Parser: XML_Parser; Enc: ENCODING_ptr;
  Start, End_: PAnsiChar): Integer;
var
  Data: XML_PAnsiChar;

begin
  if @Parser.M_commentHandler = nil then
  begin
    if @Parser.M_defaultHandler <> nil then
      ReportDefault(Parser, Enc, Start, End_);

    Result := 1;

    Exit;

  end;

  Data := PoolStoreString(@Parser.M_tempPool, Enc,
    PAnsiChar(PtrComp(Start) + Enc.MinBytesPerChar * 4),
    PAnsiChar(PtrComp(End_) - Enc.MinBytesPerChar * 3));

  if Data = nil then
  begin
    Result := 0;

    Exit;

  end;

  NormalizeLines(Data);

  Parser.M_commentHandler(Parser.M_handlerArg, Data);

  PoolClear(@Parser.M_tempPool);

  Result := 1;

end;

{ doProlog }
function DoProlog(Parser: XML_Parser; Enc: ENCODING_ptr; S, End_: PAnsiChar;
  Tok: Integer; Next: PAnsiChar; NextPtr: PPAnsiChar; HaveMore: XML_Bool)
  : XML_Error;
const
{$IFDEF XML_DTD }
  ExternalSubsetName: array [0 .. 1] of XML_Char = ('#', #0);

{$ENDIF }
  AtypeCDATA: array [0 .. 5] of XML_Char = ('C', 'D', 'A', 'T', 'A', #0);
  AtypeID: array [0 .. 2] of XML_Char = ('I', 'D', #0);
  AtypeIDREF: array [0 .. 5] of XML_Char = ('I', 'D', 'R', 'E', 'F', #0);
  AtypeIDREFS: array [0 .. 6] of XML_Char = ('I', 'D', 'R', 'E', 'F', 'S', #0);
  AtypeENTITY: array [0 .. 6] of XML_Char = ('E', 'N', 'T', 'I', 'T', 'Y', #0);
  AtypeENTITIES: array [0 .. 8] of XML_Char = ('E', 'N', 'T', 'I', 'T', 'I',
    'E', 'S', #0);
  AtypeNMTOKEN: array [0 .. 7] of XML_Char = ('N', 'M', 'T', 'O', 'K',
    'E', 'N', #0);
  AtypeNMTOKENS: array [0 .. 8] of XML_Char = ('N', 'M', 'T', 'O', 'K', 'E',
    'N', 'S', #0);
  NotationPrefix: array [0 .. 8] of XML_Char = ('N', 'O', 'T', 'A', 'T', 'I',
    'O', 'N', #0);
  EnumValueSep: array [0 .. 1] of XML_Char = ('|', #0);
  EnumValueStart: array [0 .. 1] of XML_Char = ('(', #0);

var
  Dtd: DTD_ptr;

  EventPP, EventEndPP: PPAnsiChar;

  Quant: XML_Content_Quant;

  Role, Myindex, NameLen: Integer;

  HandleDefault, HadParamEntityRefs, Ok, BetweenDecl: XML_Bool;

  Result_: XML_Error;

  Tem, Prefix, AttVal, Name, SystemId: XML_PAnsiChar;

  Entity: ENTITY_ptr;

  Ctemp, Nxt: PAnsiChar;

  Itemp: PInteger;

  Content, Model: XML_Content_ptr;

  El: ELEMENT_TYPE_ptr;

label
  _break, _go0, _go1,
  AlreadyChecked, CheckAttListDeclHandler, ElementContent, CloseGroup;

begin
  { save one level of indirection }
  Dtd := Parser.M_dtd;

  if Enc = Parser.M_encoding then
  begin
    EventPP := @Parser.M_eventPtr;
    EventEndPP := @Parser.M_eventEndPtr;

  end
  else
  begin
    EventPP := @Parser.M_openInternalEntities.InternalEventPtr;
    EventEndPP := @Parser.M_openInternalEntities.InternalEventEndPtr;

  end;

  repeat
    HandleDefault := XML_TRUE;
    EventPP^ := S;
    EventEndPP^ := Next;

    if Tok <= 0 then
    begin
      if (HaveMore <> 0) and (Tok <> XML_TOK_INVALID) then
      begin
        NextPtr^ := S;
        Result := XML_ERROR_NONE;

        Exit;

      end;

      case Tok of
        XML_TOK_INVALID:
          begin
            EventPP^ := Next;
            Result := XML_ERROR_INVALID_TOKEN;

            Exit;

          end;

        XML_TOK_PARTIAL:
          begin
            Result := XML_ERROR_UNCLOSED_TOKEN;

            Exit;

          end;

        XML_TOK_PARTIAL_CHAR:
          begin
            Result := XML_ERROR_PARTIAL_CHAR;

            Exit;

          end;

        XML_TOK_NONE:
          begin
{$IFDEF XML_DTD }
            { for internal PE NOT referenced between declarations }
            if (Enc <> Parser.M_encoding) and
              (Parser.M_openInternalEntities.BetweenDecl = 0) then
            begin
              NextPtr^ := S;
              Result := XML_ERROR_NONE;

              Exit;

            end;

            { WFC: PE Between Declarations - must check that PE contains
              complete markup, not only for external PEs, but also for
              internal PEs if the reference occurs between declarations. }
            if (Parser.M_isParamEntity <> 0) or (Enc <> Parser.M_encoding) then
            begin
              if XmlTokenRole(@Parser.M_prologState, XML_TOK_NONE,
                Pointer(End_), Pointer(End_), Enc) = XML_ROLE_ERROR then
              begin
                Result := XML_ERROR_INCOMPLETE_PE;

                Exit;

              end;

              NextPtr^ := S;
              Result := XML_ERROR_NONE;

              Exit;

            end;

{$ENDIF }
            Result := XML_ERROR_NO_ELEMENTS;

            Exit;

          end;

      else
        begin
          Tok := -Tok;
          Next := End_;

        end;

      end;

    end;

    Role := XmlTokenRole(@Parser.M_prologState, Tok, Pointer(S),
      Pointer(Next), Enc);

    case Role of
      XML_ROLE_XML_DECL:
        begin
          Result_ := ProcessXmlDecl(Parser, 0, S, Next);

          if Result_ <> XML_ERROR_NONE then
          begin
            Result := Result_;

            Exit;

          end;

          Enc := Parser.M_encoding;

          HandleDefault := XML_FALSE;

        end;

      XML_ROLE_DOCTYPE_NAME:
        begin
          if @Parser.FStartDoctypeDeclHandler <> nil then
          begin
            Parser.M_doctypeName := PoolStoreString(@Parser.M_tempPool,
              Enc, S, Next);

            if Parser.M_doctypeName = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            PoolFinish(@Parser.M_tempPool);

            Parser.M_doctypePubid := nil;
            HandleDefault := XML_FALSE;

          end;

          Parser.M_doctypeSysid := nil; { always initialize to NULL }

        end;

      XML_ROLE_DOCTYPE_INTERNAL_SUBSET:
        if @Parser.FStartDoctypeDeclHandler <> nil then
        begin
          Parser.FStartDoctypeDeclHandler(Parser.M_handlerArg,
            Parser.M_doctypeName, Parser.M_doctypeSysid,
            Parser.M_doctypePubid, 1);

          Parser.M_doctypeName := nil;

          PoolClear(@Parser.M_tempPool);

          HandleDefault := XML_FALSE;

        end;

{$IFDEF XML_DTD }
      XML_ROLE_TEXT_DECL:
        begin
          Result_ := ProcessXmlDecl(Parser, 1, S, Next);

          if Result_ <> XML_ERROR_NONE then
          begin
            Result := Result_;

            Exit;

          end;

          Enc := Parser.M_encoding;
          HandleDefault := XML_FALSE;

        end;

{$ENDIF }
      XML_ROLE_DOCTYPE_PUBLIC_ID:
        begin
{$IFDEF XML_DTD }
          Parser.M_useForeignDTD := XML_FALSE;
          Parser.M_declEntity :=
            ENTITY_ptr(Lookup(@Dtd.ParamEntities, @ExternalSubsetName[0],
            SizeOf(Expat.ENTITY)));

          if Parser.M_declEntity = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

{$ENDIF }
          Dtd.HasParamEntityRefs := XML_TRUE;

          if @Parser.FStartDoctypeDeclHandler <> nil then
          begin
            if XmlIsPublicId(Enc, Pointer(S), Pointer(Next), Pointer(EventPP)) = 0
            then
            begin
              Result := XML_ERROR_PUBLICID;

              Exit;

            end;

            Parser.M_doctypePubid := PoolStoreString(@Parser.M_tempPool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if Parser.M_doctypePubid = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            NormalizePublicId(XML_PAnsiChar(Parser.M_doctypePubid));
            PoolFinish(@Parser.M_tempPool);

            HandleDefault := XML_FALSE;

            goto AlreadyChecked;

          end;

          { fall through }
          goto _go0;

        end;

      XML_ROLE_ENTITY_PUBLIC_ID:
      _go0:
        begin
          if XmlIsPublicId(Enc, Pointer(S), Pointer(Next), Pointer(EventPP)) = 0
          then
          begin
            Result := XML_ERROR_PUBLICID;

            Exit;

          end;

        AlreadyChecked:
          if (Dtd.KeepProcessing <> 0) and (Parser.M_declEntity <> nil) then
          begin
            Tem := PoolStoreString(@Dtd.Pool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if Tem = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            NormalizePublicId(Tem);

            Parser.M_declEntity.PublicId := Tem;

            PoolFinish(@Dtd.Pool);

            if @Parser.M_entityDeclHandler <> nil then
              HandleDefault := XML_FALSE;

          end;

        end;

      XML_ROLE_DOCTYPE_CLOSE:
        begin
          if Parser.M_doctypeName <> nil then
          begin
            Parser.FStartDoctypeDeclHandler(Parser.M_handlerArg,
              Parser.M_doctypeName, Parser.M_doctypeSysid,
              Parser.M_doctypePubid, 0);

            PoolClear(@Parser.M_tempPool);

            HandleDefault := XML_FALSE;

          end;

          { doctypeSysid will be non-NULL in the case of a previous
            XML_ROLE_DOCTYPE_SYSTEM_ID, even if startDoctypeDeclHandler
            was not set, indicating an external subset }
{$IFDEF XML_DTD }
          if (Parser.M_doctypeSysid <> nil) or (Parser.M_useForeignDTD <> 0)
          then
          begin
            HadParamEntityRefs := Dtd.HasParamEntityRefs;
            Dtd.HasParamEntityRefs := XML_TRUE;

            if (Parser.M_paramEntityParsing <> XML_ParamEntityParsing(0)) and
              (@Parser.M_externalEntityRefHandler <> nil) then
            begin
              Entity := ENTITY_ptr(Lookup(@Dtd.ParamEntities,
                @ExternalSubsetName[0], SizeOf(Expat.ENTITY)));

              if Entity = nil then
              begin
                Result := XML_ERROR_NO_MEMORY;

                Exit;

              end;

              if Parser.M_useForeignDTD <> 0 then
                Entity.Base := Parser.FCurBase;

              Dtd.ParamEntityRead := XML_FALSE;

              if Parser.M_externalEntityRefHandler
                (Parser.M_externalEntityRefHandlerArg, 0, Entity.Base,
                Entity.SystemId, Entity.PublicId) = 0 then
              begin
                Result := XML_ERROR_EXTERNAL_ENTITY_HANDLING;

                Exit;

              end;

              if Dtd.ParamEntityRead <> 0 then
                if (Dtd.Standalone = 0) and
                  (@Parser.M_notStandaloneHandler <> nil) and
                  (Parser.M_notStandaloneHandler(Parser.M_handlerArg) = 0) then
                begin
                  Result := XML_ERROR_NOT_STANDALONE;

                  Exit;

                end
                else
              else
                { if we didn't read the foreign DTD then this means that there
                  is no external subset and we must reset dtd.hasParamEntityRefs }
                if Parser.M_doctypeSysid = nil then
                  Dtd.HasParamEntityRefs := HadParamEntityRefs;

              { end of DTD - no need to update dtd.keepProcessing }

            end;

            Parser.M_useForeignDTD := XML_FALSE;

          end;

{$ENDIF }
          if @Parser.M_endDoctypeDeclHandler <> nil then
          begin
            Parser.M_endDoctypeDeclHandler(Parser.M_handlerArg);

            HandleDefault := XML_FALSE;

          end;

        end;

      XML_ROLE_INSTANCE_START:
        begin
{$IFDEF XML_DTD }
          { if there is no DOCTYPE declaration then now is the
            last chance to read the foreign DTD }
          if Parser.M_useForeignDTD <> 0 then
          begin
            HadParamEntityRefs := Dtd.HasParamEntityRefs;
            Dtd.HasParamEntityRefs := XML_TRUE;

            if (Parser.M_paramEntityParsing <> XML_ParamEntityParsing(0)) and
              (@Parser.M_externalEntityRefHandler <> nil) then
            begin
              Entity := ENTITY_ptr(Lookup(@Dtd.ParamEntities,
                @ExternalSubsetName[0], SizeOf(Expat.ENTITY)));

              if Entity = nil then
              begin
                Result := XML_ERROR_NO_MEMORY;

                Exit;

              end;

              Entity.Base := Parser.FCurBase;
              Dtd.ParamEntityRead := XML_FALSE;

              if Parser.M_externalEntityRefHandler
                (Parser.M_externalEntityRefHandlerArg, 0, Entity.Base,
                Entity.SystemId, Entity.PublicId) = 0 then
              begin
                Result := XML_ERROR_EXTERNAL_ENTITY_HANDLING;

                Exit;

              end;

              if Dtd.ParamEntityRead <> 0 then
                if (Dtd.Standalone = 0) and
                  (@Parser.M_notStandaloneHandler <> nil) and
                  (Parser.M_notStandaloneHandler(Parser.M_handlerArg) = 0) then
                begin
                  Result := XML_ERROR_NOT_STANDALONE;

                  Exit;

                end
                else
              else
                { if we didn't read the foreign DTD then this means that there
                  is no external subset and we must reset dtd.hasParamEntityRefs }
                Dtd.HasParamEntityRefs := HadParamEntityRefs;

              { end of DTD - no need to update dtd.keepProcessing }

            end;

          end;

{$ENDIF }
          Parser.M_processor := @ContentProcessor;

          Result := ContentProcessor(Parser, S, End_, NextPtr);

          Exit;

        end;

      XML_ROLE_ATTLIST_ELEMENT_NAME:
        begin
          Parser.M_declElementType := GetElementType(Parser, Enc, S, Next);

          if Parser.M_declElementType = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_NAME:
        begin
          Parser.M_declAttributeId := GetAttributeId(Parser, Enc, S, Next);

          if Parser.M_declAttributeId = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          Parser.M_declAttributeIsCdata := XML_FALSE;
          Parser.M_declAttributeType := nil;
          Parser.M_declAttributeIsId := XML_FALSE;

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_TYPE_CDATA:
        begin
          Parser.M_declAttributeIsCdata := XML_TRUE;
          Parser.M_declAttributeType := @AtypeCDATA[0];

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_TYPE_ID:
        begin
          Parser.M_declAttributeIsId := XML_TRUE;
          Parser.M_declAttributeType := @AtypeID[0];

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_TYPE_IDREF:
        begin
          Parser.M_declAttributeType := @AtypeIDREF[0];

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_TYPE_IDREFS:
        begin
          Parser.M_declAttributeType := @AtypeIDREFS[0];

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_TYPE_ENTITY:
        begin
          Parser.M_declAttributeType := @AtypeENTITY[0];

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_TYPE_ENTITIES:
        begin
          Parser.M_declAttributeType := @AtypeENTITIES[0];

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_TYPE_NMTOKEN:
        begin
          Parser.M_declAttributeType := @AtypeNMTOKEN[0];

          goto CheckAttListDeclHandler;

        end;

      XML_ROLE_ATTRIBUTE_TYPE_NMTOKENS:
        begin
          Parser.M_declAttributeType := @AtypeNMTOKENS[0];

        CheckAttListDeclHandler:
          if (Dtd.KeepProcessing <> 0) and (@Parser.M_attlistDeclHandler <> nil)
          then
            HandleDefault := XML_FALSE;

        end;

      XML_ROLE_ATTRIBUTE_ENUM_VALUE, XML_ROLE_ATTRIBUTE_NOTATION_VALUE:
        if (Dtd.KeepProcessing <> 0) and (@Parser.M_attlistDeclHandler <> nil)
        then
        begin
          if Parser.M_declAttributeType <> nil then
            Prefix := @EnumValueSep[0]

          else if Role = XML_ROLE_ATTRIBUTE_NOTATION_VALUE then
            Prefix := @NotationPrefix[0]
          else
            Prefix := @EnumValueStart[0];

          if PoolAppendString(@Parser.M_tempPool, Prefix) = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          if PoolAppend(@Parser.M_tempPool, Enc, S, Next) = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          Parser.M_declAttributeType := Parser.M_tempPool.Start;

          HandleDefault := XML_FALSE;

        end;

      XML_ROLE_IMPLIED_ATTRIBUTE_VALUE, XML_ROLE_REQUIRED_ATTRIBUTE_VALUE:
        if Dtd.KeepProcessing <> 0 then
        begin
          if DefineAttribute(Parser.M_declElementType, Parser.M_declAttributeId,
            Parser.M_declAttributeIsCdata, Parser.M_declAttributeIsId, 0,
            Parser) = 0 then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          if (@Parser.M_attlistDeclHandler <> nil) and
            (Parser.M_declAttributeType <> nil) then
          begin
            if (Parser.M_declAttributeType^ = XML_T('(')) or
              ((Parser.M_declAttributeType^ = XML_T('N')) and
              (XML_PAnsiChar(PtrComp(Parser.M_declAttributeType) + 1)
              ^ = XML_T('O'))) then
            begin
              { Enumerated or Notation type }
              if (PoolAppendChar(@Parser.M_tempPool, XML_T(')')) = 0) or
                (PoolAppendChar(@Parser.M_tempPool, XML_T(#0)) = 0) then
              begin
                Result := XML_ERROR_NO_MEMORY;

                Exit;

              end;

              Parser.M_declAttributeType := Parser.M_tempPool.Start;

              PoolFinish(@Parser.M_tempPool);

            end;

            EventEndPP^ := S;

            Parser.M_attlistDeclHandler(Parser.M_handlerArg,
              Parser.M_declElementType.Name, Parser.M_declAttributeId.Name,
              Parser.M_declAttributeType, 0,
              Integer(Role = XML_ROLE_REQUIRED_ATTRIBUTE_VALUE));

            PoolClear(@Parser.M_tempPool);

            HandleDefault := XML_FALSE;

          end;

        end;

      XML_ROLE_DEFAULT_ATTRIBUTE_VALUE, XML_ROLE_FIXED_ATTRIBUTE_VALUE:
        if Dtd.KeepProcessing <> 0 then
        begin
          Result_ := StoreAttributeValue(Parser, Enc,
            Parser.M_declAttributeIsCdata,
            PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar), @Dtd.Pool);

          if Result_ <> XML_Error(0) then
          begin
            Result := Result_;

            Exit;

          end;

          AttVal := PoolStart(@Dtd.Pool);

          PoolFinish(@Dtd.Pool);

          { ID attributes aren't alLowed to have a default }
          if DefineAttribute(Parser.M_declElementType, Parser.M_declAttributeId,
            Parser.M_declAttributeIsCdata, XML_FALSE, AttVal, Parser) = 0 then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          if (@Parser.M_attlistDeclHandler <> nil) and
            (Parser.M_declAttributeType <> nil) then
          begin
            if (Parser.M_declAttributeType^ = XML_T('(')) or
              ((Parser.M_declAttributeType^ = XML_T('N')) and
              (XML_PAnsiChar(PtrComp(Parser.M_declAttributeType) + 1)
              ^ = XML_T('O'))) then
            begin
              { Enumerated or Notation type }
              if (PoolAppendChar(@Parser.M_tempPool, XML_T(')')) = 0) or
                (PoolAppendChar(@Parser.M_tempPool, XML_T(#0)) = 0) then
              begin
                Result := XML_ERROR_NO_MEMORY;

                Exit;

              end;

              Parser.M_declAttributeType := Parser.M_tempPool.Start;

              PoolFinish(@Parser.M_tempPool);

            end;

            EventEndPP^ := S;

            Parser.M_attlistDeclHandler(Parser.M_handlerArg,
              Parser.M_declElementType.Name, Parser.M_declAttributeId.Name,
              Parser.M_declAttributeType, AttVal,
              Integer(Role = XML_ROLE_FIXED_ATTRIBUTE_VALUE));

            PoolClear(@Parser.M_tempPool);

            HandleDefault := XML_FALSE;
          end;
        end;

      XML_ROLE_ENTITY_VALUE:
        if Dtd.KeepProcessing <> 0 then
        begin
          Result_ := StoreEntityValue(Parser, Enc,
            PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

          if Parser.M_declEntity <> nil then
          begin
            Parser.M_declEntity.TextPtr := PoolStart(@Dtd.EntityValuePool);
            Parser.M_declEntity.TextLen := PoolLength(@Dtd.EntityValuePool);

            PoolFinish(@Dtd.EntityValuePool);

            if @Parser.M_entityDeclHandler <> nil then
            begin
              EventEndPP^ := S;

              Parser.M_entityDeclHandler(Parser.M_handlerArg,
                Parser.M_declEntity.Name, Parser.M_declEntity.Is_param,
                Parser.M_declEntity.TextPtr, Parser.M_declEntity.TextLen,
                Parser.FCurBase, 0, 0, 0);

              HandleDefault := XML_FALSE;
            end;
          end
          else
            PoolDiscard(@Dtd.EntityValuePool);

          if Result_ <> XML_ERROR_NONE then
          begin
            Result := Result_;

            Exit;
          end;
        end;

      XML_ROLE_DOCTYPE_SYSTEM_ID:
        begin
{$IFDEF XML_DTD }
          Parser.M_useForeignDTD := XML_FALSE;

{$ENDIF }
          Dtd.HasParamEntityRefs := XML_TRUE;

          if @Parser.FStartDoctypeDeclHandler <> nil then
          begin
            Parser.M_doctypeSysid := PoolStoreString(@Parser.M_tempPool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if Parser.M_doctypeSysid = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;
            end;

            PoolFinish(@Parser.M_tempPool);

            HandleDefault := XML_FALSE;
          end
{$IFDEF XML_DTD }
          else
            { use externalSubsetName to make doctypeSysid non-NULL
              for the case where no startDoctypeDeclHandler is set }
            Parser.M_doctypeSysid := @ExternalSubsetName[0];

{$ELSE }; {$ENDIF }
          if (Dtd.Standalone = 0) and
{$IFDEF XML_DTD }
            (Parser.M_paramEntityParsing = XML_ParamEntityParsing(0)) and
{$ENDIF }
            (@Parser.M_notStandaloneHandler <> nil) and
            (Parser.M_notStandaloneHandler(Parser.M_handlerArg) = 0) then
          begin
            Result := XML_ERROR_NOT_STANDALONE;

            Exit;
          end;

{$IFNDEF XML_DTD }
{$ELSE }
          if Parser.M_declEntity = nil then
          begin
            Parser.M_declEntity :=
              ENTITY_ptr(Lookup(@Dtd.ParamEntities, @ExternalSubsetName[0],
              SizeOf(Expat.ENTITY)));

            if Parser.M_declEntity = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            Parser.M_declEntity.PublicId := nil;

          end;

{$ENDIF }
          { fall through }
          goto _go1;

        end;

      XML_ROLE_ENTITY_SYSTEM_ID:
      _go1:
        if (Dtd.KeepProcessing <> 0) and (Parser.M_declEntity <> nil) then
        begin
          Parser.M_declEntity.SystemId := PoolStoreString(@Dtd.Pool, Enc,
            PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
            PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

          if Parser.M_declEntity.SystemId = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          Parser.M_declEntity.Base := Parser.FCurBase;

          PoolFinish(@Dtd.Pool);

          if @Parser.M_entityDeclHandler <> nil then
            HandleDefault := XML_FALSE;

        end;

      XML_ROLE_ENTITY_COMPLETE:
        if (Dtd.KeepProcessing <> 0) and (Parser.M_declEntity <> nil) and
          (@Parser.M_entityDeclHandler <> nil) then
        begin
          EventEndPP^ := S;

          Parser.M_entityDeclHandler(Parser.M_handlerArg,
            Parser.M_declEntity.Name, Parser.M_declEntity.Is_param, 0, 0,
            Parser.M_declEntity.Base, Parser.M_declEntity.SystemId,
            Parser.M_declEntity.PublicId, 0);

          HandleDefault := XML_FALSE;

        end;

      XML_ROLE_ENTITY_NOTATION_NAME:
        if (Dtd.KeepProcessing <> 0) and (Parser.M_declEntity <> nil) then
        begin
          Parser.M_declEntity.Notation := PoolStoreString(@Dtd.Pool,
            Enc, S, Next);

          if Parser.M_declEntity.Notation = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          PoolFinish(@Dtd.Pool);

          if @Parser.M_unparsedEntityDeclHandler <> nil then
          begin
            EventEndPP^ := S;

            Parser.M_unparsedEntityDeclHandler(Parser.M_handlerArg,
              Parser.M_declEntity.Name, Parser.M_declEntity.Base,
              Parser.M_declEntity.SystemId, Parser.M_declEntity.PublicId,
              Parser.M_declEntity.Notation);

            HandleDefault := XML_FALSE;

          end
          else if @Parser.M_entityDeclHandler <> nil then
          begin
            EventEndPP^ := S;

            Parser.M_entityDeclHandler(Parser.M_handlerArg,
              Parser.M_declEntity.Name, 0, 0, 0, Parser.M_declEntity.Base,
              Parser.M_declEntity.SystemId, Parser.M_declEntity.PublicId,
              Parser.M_declEntity.Notation);

            HandleDefault := XML_FALSE;

          end;

        end;

      XML_ROLE_GENERAL_ENTITY_NAME:
        begin
          if XmlPredefinedEntityName(Enc, Pointer(S), Pointer(Next)) <> 0 then
          begin
            Parser.M_declEntity := nil;

            goto _break;

          end;

          if Dtd.KeepProcessing <> 0 then
          begin
            name := PoolStoreString(@Dtd.Pool, Enc, S, Next);

            if name = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            Parser.M_declEntity :=
              ENTITY_ptr(Lookup(@Dtd.GeneralEntities, name,
              SizeOf(Expat.ENTITY)));

            if Parser.M_declEntity = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            if Parser.M_declEntity.Name <> name then
            begin
              PoolDiscard(@Dtd.Pool);

              Parser.M_declEntity := nil;

            end
            else
            begin
              PoolFinish(@Dtd.Pool);

              Parser.M_declEntity.PublicId := nil;
              Parser.M_declEntity.Is_param := XML_FALSE;

              { if we have a parent parser or are reading an internal parameter
                entity, then the entity declaration is not considered "internal" }
              Parser.M_declEntity.IsInternal :=
                XML_Bool(not((Parser.M_parentParser <> nil) or
                (Parser.M_openInternalEntities <> nil)));

              if @Parser.M_entityDeclHandler <> nil then
                HandleDefault := XML_FALSE;

            end;

          end
          else
          begin
            PoolDiscard(@Dtd.Pool);

            Parser.M_declEntity := nil;

          end;

        end;

      XML_ROLE_PARAM_ENTITY_NAME:
{$IFDEF XML_DTD }
        if Dtd.KeepProcessing <> 0 then
        begin
          name := PoolStoreString(@Dtd.Pool, Enc, S, Next);

          if name <> nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          Parser.M_declEntity :=
            ENTITY_ptr(Lookup(@Dtd.ParamEntities, name, SizeOf(Expat.ENTITY)));

          if Parser.M_declEntity = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          if Parser.M_declEntity.Name <> name then
          begin
            PoolDiscard(@Dtd.Pool);

            Parser.M_declEntity := nil;

          end
          else
          begin
            PoolFinish(@Dtd.Pool);

            Parser.M_declEntity.PublicId := nil;
            Parser.M_declEntity.Is_param := XML_TRUE;

            { if we have a parent parser or are reading an internal parameter
              entity, then the entity declaration is not considered "internal" }
            Parser.M_declEntity.IsInternal :=
              XML_Bool(not((Parser.M_parentParser <> nil) or
              (Parser.M_openInternalEntities <> nil)));

            if @Parser.M_entityDeclHandler <> nil then
              HandleDefault := XML_FALSE;

          end;

        end
        else
        begin
          PoolDiscard(@Dtd.Pool);

          Parser.M_declEntity := nil;

        end;

{$ELSE }
        Parser.M_declEntity := nil;

{$ENDIF }
      XML_ROLE_NOTATION_NAME:
        begin
          Parser.M_declNotationPublicId := nil;
          Parser.M_declNotationName := nil;

          if @Parser.M_notationDeclHandler <> nil then
          begin
            Parser.M_declNotationName := PoolStoreString(@Parser.M_tempPool,
              Enc, S, Next);

            if Parser.M_declNotationName = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            PoolFinish(@Parser.M_tempPool);

            HandleDefault := XML_FALSE;

          end;

        end;

      XML_ROLE_NOTATION_PUBLIC_ID:
        begin
          if XmlIsPublicId(Enc, Pointer(S), Pointer(Next), Pointer(EventPP)) = 0
          then
          begin
            Result := XML_ERROR_PUBLICID;

            Exit;

          end;

          if Parser.M_declNotationName <> nil
          then { means notationDeclHandler <> NIL }
          begin
            Tem := PoolStoreString(@Parser.M_tempPool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if Tem = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            NormalizePublicId(Tem);

            Parser.M_declNotationPublicId := Tem;

            PoolFinish(@Parser.M_tempPool);

            HandleDefault := XML_FALSE;

          end;

        end;

      XML_ROLE_NOTATION_SYSTEM_ID:
        begin
          if (Parser.M_declNotationName <> nil) and
            (@Parser.M_notationDeclHandler <> nil) then
          begin
            SystemId := PoolStoreString(@Parser.M_tempPool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if SystemId = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            EventEndPP^ := S;

            Parser.M_notationDeclHandler(Parser.M_handlerArg,
              Parser.M_declNotationName, Parser.FCurBase, SystemId,
              Parser.M_declNotationPublicId);

            HandleDefault := XML_FALSE;

          end;

          PoolClear(@Parser.M_tempPool);

        end;

      XML_ROLE_NOTATION_NO_SYSTEM_ID:
        begin
          if (Parser.M_declNotationPublicId <> nil) and
            (@Parser.M_notationDeclHandler <> nil) then
          begin
            EventEndPP^ := S;

            Parser.M_notationDeclHandler(Parser.M_handlerArg,
              Parser.M_declNotationName, Parser.FCurBase, 0,
              Parser.M_declNotationPublicId);

            HandleDefault := XML_FALSE;

          end;

          PoolClear(@Parser.M_tempPool);

        end;

      XML_ROLE_ERROR:
        case Tok of
          XML_TOK_PARAM_ENTITY_REF:
            { PE references in internal subset are
              not alLowed within declarations. }
            begin
              Result := XML_ERROR_PARAM_ENTITY_REF;

              Exit;

            end;

          XML_TOK_XML_DECL:
            begin
              Result := XML_ERROR_MISPLACED_XML_PI;

              Exit;

            end;

        else
          begin
            Result := XML_ERROR_SYNTAX;

            Exit;

          end;

        end;

{$IFDEF XML_DTD }
      XML_ROLE_IGNORE_SECT:
        begin
          if @Parser.M_defaultHandler <> nil then
            ReportDefault(Parser, Enc, S, Next);

          HandleDefault := XML_FALSE;

          Result_ := DoIgnoreSection(Parser, Enc, @Next, End_, NextPtr,
            HaveMore);

          if Result_ <> XML_ERROR_NONE then
          begin
            Result := Result_;

            Exit;

          end
          else if Next = nil then
          begin
            Parser.M_processor := @IgnoreSectionProcessor;

            Result := Result_;

            Exit;

          end;

        end;

{$ENDIF }
      XML_ROLE_GROUP_OPEN:
        begin
          if Parser.M_prologState.Level >= Parser.M_groupSize then
            if Parser.M_groupSize <> 0 then
            begin
              Parser.M_groupSize := Parser.M_groupSize * 2;

              if Parser.M_mem.Realloc_fcn(Pointer(Parser.M_groupConnector),
                Parser.M_groupAlloc, Parser.M_groupSize) then
                Parser.M_groupAlloc := Parser.M_groupSize

              else
              begin
                Result := XML_ERROR_NO_MEMORY;

                Exit;

              end;

              if Dtd.ScaffIndex <> nil then
                if Parser.M_mem.Realloc_fcn(Pointer(Dtd.ScaffIndex),
                  Dtd.ScaffAlloc, Parser.M_groupSize * SizeOf(Integer)) then
                  Dtd.ScaffAlloc := Parser.M_groupSize * SizeOf(Integer)

                else
                begin
                  Result := XML_ERROR_NO_MEMORY;

                  Exit;

                end;

            end
            else
            begin
              Parser.M_groupSize := 32;

              if Parser.M_mem.Malloc_fcn(Pointer(Parser.M_groupConnector),
                Parser.M_groupSize) then
                Parser.M_groupAlloc := Parser.M_groupSize

              else
              begin
                Result := XML_ERROR_NO_MEMORY;

                Exit;

              end;

            end;

          PAnsiChar(PtrComp(Parser.M_groupConnector) +
            Parser.M_prologState.Level)^ := #0;

          if Dtd.In_eldecl <> 0 then
          begin
            Myindex := NextScaffoldPart(Parser);

            if Myindex < 0 then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            PInteger(PtrComp(Dtd.ScaffIndex) + Dtd.ScaffLevel * SizeOf(Integer))^
              := Myindex;

            Inc(Dtd.ScaffLevel);

            CONTENT_SCAFFOLD_ptr(PtrComp(Dtd.Scaffold) + Myindex *
              SizeOf(CONTENT_SCAFFOLD))^.Type_ := XML_CTYPE_SEQ;

            if @Parser.M_elementDeclHandler <> nil then
              HandleDefault := XML_FALSE;

          end;

        end;

      XML_ROLE_GROUP_SEQUENCE:
        begin
          if PAnsiChar(PtrComp(Parser.M_groupConnector) +
            Parser.M_prologState.Level)^ = '|' then
          begin
            Result := XML_ERROR_SYNTAX;

            Exit;

          end;

          PAnsiChar(PtrComp(Parser.M_groupConnector) +
            Parser.M_prologState.Level)^ := ',';

          if (Dtd.In_eldecl <> 0) and (@Parser.M_elementDeclHandler <> nil) then
            HandleDefault := XML_FALSE;

        end;

      XML_ROLE_GROUP_CHOICE:
        begin
          if PAnsiChar(PtrComp(Parser.M_groupConnector) +
            Parser.M_prologState.Level)^ = ',' then
          begin
            Result := XML_ERROR_SYNTAX;

            Exit;

          end;

          if (Dtd.In_eldecl <> 0) and
            (PAnsiChar(PtrComp(Parser.M_groupConnector) +
            Parser.M_prologState.Level)^ <> #0) and
            (CONTENT_SCAFFOLD_ptr(PtrComp(Dtd.Scaffold) +
            PInteger(PtrComp(Dtd.ScaffIndex) + (Dtd.ScaffLevel - 1) * SizeOf(Integer))^
            * SizeOf(CONTENT_SCAFFOLD))^.Type_ <> XML_CTYPE_MIXED) then
          begin
            CONTENT_SCAFFOLD_ptr(PtrComp(Dtd.Scaffold) +
              PInteger(PtrComp(Dtd.ScaffIndex) + (Dtd.ScaffLevel - 1) * SizeOf(Integer))
              ^ * SizeOf(CONTENT_SCAFFOLD))^.Type_ := XML_CTYPE_CHOICE;

            if @Parser.M_elementDeclHandler <> nil then
              HandleDefault := XML_FALSE;

          end;

          PAnsiChar(PtrComp(Parser.M_groupConnector) +
            Parser.M_prologState.Level)^ := '|';

        end;

      XML_ROLE_PARAM_ENTITY_REF
      {$IFDEF XML_DTD } , XML_ROLE_INNER_PARAM_ENTITY_REF: {$ELSE }: {$ENDIF }
        begin
{$IFDEF XML_DTD }
          Dtd.HasParamEntityRefs := XML_TRUE;

          if Parser.M_paramEntityParsing = XML_ParamEntityParsing(0) then
            Dtd.KeepProcessing := Dtd.Standalone

          else
          begin
            name := PoolStoreString(@Dtd.Pool, Enc,
              PAnsiChar(PtrComp(S) + Enc.MinBytesPerChar),
              PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar));

            if name = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            Entity := ENTITY_ptr(Lookup(@Dtd.ParamEntities, name, 0));

            PoolDiscard(@Dtd.Pool);

            { first, determine if a check for an existing declaration is needed;
              if yes, check that the entity exists, and that it is internal,
              otherwise call the skipped entity handler }
            if Dtd.Standalone <> 0 then
              Ok := XML_Bool(Parser.M_openInternalEntities = nil)
            else
              Ok := XML_Bool(Dtd.HasParamEntityRefs = 0);

            if (Parser.M_prologState.DocumentEntity <> 0) and (Ok <> 0) then
              if Entity = nil then
              begin
                Result := XML_ERROR_UNDEFINED_ENTITY;

                Exit;

              end
              else if Entity.IsInternal = 0 then
              begin
                Result := XML_ERROR_ENTITY_DECLARED_IN_PE;

                Exit;

              end
              else
            else if Entity = nil then
            begin
              Dtd.KeepProcessing := Dtd.Standalone;

              { cannot report skipped entities in declarations }
              if (Role = XML_ROLE_PARAM_ENTITY_REF) and
                (@Parser.M_skippedEntityHandler <> nil) then
              begin
                Parser.M_skippedEntityHandler(Parser.M_handlerArg, name, 1);

                HandleDefault := XML_FALSE;

              end;

              goto _break;

            end;

            if Entity.Open <> 0 then
            begin
              Result := XML_ERROR_RECURSIVE_ENTITY_REF;

              Exit;

            end;

            if Entity.TextPtr <> nil then
            begin
              if Role = XML_ROLE_PARAM_ENTITY_REF then
                BetweenDecl := XML_TRUE
              else
                BetweenDecl := XML_FALSE;

              Result_ := ProcessInternalEntity(Parser, Entity, BetweenDecl);

              if Result_ <> XML_ERROR_NONE then
              begin
                Result := Result_;

                Exit;

              end;

              HandleDefault := XML_FALSE;

              goto _break;

            end;

            if @Parser.M_externalEntityRefHandler <> nil then
            begin
              Dtd.ParamEntityRead := XML_FALSE;
              Entity.Open := XML_TRUE;

              if Parser.M_externalEntityRefHandler
                (Parser.M_externalEntityRefHandlerArg, 0, Entity.Base,
                Entity.SystemId, Entity.PublicId) = 0 then
              begin
                Entity.Open := XML_FALSE;

                Result := XML_ERROR_EXTERNAL_ENTITY_HANDLING;

                Exit;

              end;

              Entity.Open := XML_FALSE;
              HandleDefault := XML_FALSE;

              if Dtd.ParamEntityRead = 0 then
              begin
                Dtd.KeepProcessing := Dtd.Standalone;

                goto _break;

              end;

            end
            else
            begin
              Dtd.KeepProcessing := Dtd.Standalone;

              goto _break;

            end;

          end;

{$ENDIF }
          if (Dtd.Standalone = 0) and (@Parser.M_notStandaloneHandler <> nil)
            and (Parser.M_notStandaloneHandler(Parser.M_handlerArg) = 0) then
          begin
            Result := XML_ERROR_NOT_STANDALONE;

            Exit;

          end;

        end;

      { Element declaration stuff }
      XML_ROLE_ELEMENT_NAME:
        if @Parser.M_elementDeclHandler <> nil then
        begin
          Parser.M_declElementType := GetElementType(Parser, Enc, S, Next);

          if Parser.M_declElementType = nil then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          Dtd.ScaffLevel := 0;
          Dtd.ScaffCount := 0;
          Dtd.In_eldecl := XML_TRUE;
          HandleDefault := XML_FALSE;

        end;

      XML_ROLE_CONTENT_ANY, XML_ROLE_CONTENT_EMPTY:
        if Dtd.In_eldecl <> 0 then
        begin
          if @Parser.M_elementDeclHandler <> nil then
          begin
            Parser.M_mem.Malloc_fcn(Pointer(Content), SizeOf(XML_Content));

            if Content = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            Content.Quant := XML_CQUANT_NONE;
            Content.Name := nil;
            Content.Numchildren := 0;
            Content.Children := nil;

            if Role = XML_ROLE_CONTENT_ANY then
              Content.Type_ := XML_CTYPE_ANY
            else
              Content.Type_ := XML_CTYPE_EMPTY;

            EventEndPP^ := S;

            Parser.M_elementDeclHandler(Parser.M_handlerArg,
              Parser.M_declElementType.Name, Content);

            HandleDefault := XML_FALSE;

          end;

          Dtd.In_eldecl := XML_FALSE;

        end;

      XML_ROLE_CONTENT_PCDATA:
        if Dtd.In_eldecl <> 0 then
        begin
          CONTENT_SCAFFOLD_ptr(PtrComp(Dtd.Scaffold) +
            PInteger(PtrComp(Dtd.ScaffIndex) + (Dtd.ScaffLevel - 1) * SizeOf(Integer))^
            * SizeOf(CONTENT_SCAFFOLD))^.Type_ := XML_CTYPE_MIXED;

          if @Parser.M_elementDeclHandler <> nil then
            HandleDefault := XML_FALSE;

        end;

      XML_ROLE_CONTENT_ELEMENT:
        begin
          Quant := XML_CQUANT_NONE;

          goto ElementContent;

        end;

      XML_ROLE_CONTENT_ELEMENT_OPT:
        begin
          Quant := XML_CQUANT_OPT;

          goto ElementContent;

        end;

      XML_ROLE_CONTENT_ELEMENT_REP:
        begin
          Quant := XML_CQUANT_REP;

          goto ElementContent;

        end;

      XML_ROLE_CONTENT_ELEMENT_PLUS:
        begin
          Quant := XML_CQUANT_PLUS;

        ElementContent:
          if Dtd.In_eldecl <> 0 then
          begin
            if Quant = XML_CQUANT_NONE then
              Nxt := Next
            else
              Nxt := PAnsiChar(PtrComp(Next) - Enc.MinBytesPerChar);

            Myindex := NextScaffoldPart(Parser);

            if Myindex < 0 then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            CONTENT_SCAFFOLD_ptr(PtrComp(Dtd.Scaffold) + Myindex *
              SizeOf(CONTENT_SCAFFOLD))^.Type_ := XML_CTYPE_NAME;

            CONTENT_SCAFFOLD_ptr(PtrComp(Dtd.Scaffold) + Myindex *
              SizeOf(CONTENT_SCAFFOLD))^.Quant := Quant;

            El := GetElementType(Parser, Enc, S, Nxt);

            if El = nil then
            begin
              Result := XML_ERROR_NO_MEMORY;

              Exit;

            end;

            name := El.Name;

            CONTENT_SCAFFOLD_ptr(PtrComp(Dtd.Scaffold) + Myindex *
              SizeOf(CONTENT_SCAFFOLD))^.Name := name;

            NameLen := 0;

            while XML_PAnsiChar(PtrComp(name) + NameLen)^ <> XML_Char(0) do
              Inc(NameLen);

            Inc(Dtd.ContentStringLen, NameLen);

            if @Parser.M_elementDeclHandler <> nil then
              HandleDefault := XML_FALSE;

          end;

        end;

      XML_ROLE_GROUP_CLOSE:
        begin
          Quant := XML_CQUANT_NONE;

          goto CloseGroup;

        end;

      XML_ROLE_GROUP_CLOSE_OPT:
        begin
          Quant := XML_CQUANT_OPT;

          goto CloseGroup;

        end;

      XML_ROLE_GROUP_CLOSE_REP:
        begin
          Quant := XML_CQUANT_REP;

          goto CloseGroup;

        end;

      XML_ROLE_GROUP_CLOSE_PLUS:
        begin
          Quant := XML_CQUANT_PLUS;

        CloseGroup:
          if Dtd.In_eldecl <> 0 then
          begin
            if @Parser.M_elementDeclHandler <> nil then
              HandleDefault := XML_FALSE;

            Dec(Dtd.ScaffLevel);

            CONTENT_SCAFFOLD_ptr(PtrComp(Dtd.Scaffold) +
              PInteger(PtrComp(Dtd.ScaffIndex) + Dtd.ScaffLevel * SizeOf(Integer))^ *
              SizeOf(CONTENT_SCAFFOLD))^.Quant := Quant;

            if Dtd.ScaffLevel = 0 then
            begin
              if HandleDefault = 0 then
              begin
                Model := Build_model(Parser);

                if Model = nil then
                begin
                  Result := XML_ERROR_NO_MEMORY;

                  Exit;

                end;

                EventEndPP^ := S;

                Parser.M_elementDeclHandler(Parser.M_handlerArg,
                  Parser.M_declElementType.Name, Model);

              end;

              Dtd.In_eldecl := XML_FALSE;
              Dtd.ContentStringLen := 0;

            end;

          end;

        end; { End element declaration stuff }

      XML_ROLE_PI:
        begin
          if ReportProcessingInstruction(Parser, Enc, S, Next) = 0 then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          HandleDefault := XML_FALSE;

        end;

      XML_ROLE_COMMENT:
        begin
          if ReportComment(Parser, Enc, S, Next) = 0 then
          begin
            Result := XML_ERROR_NO_MEMORY;

            Exit;

          end;

          HandleDefault := XML_FALSE;

        end;

      XML_ROLE_NONE:
        case Tok of
          XML_TOK_BOM:
            HandleDefault := XML_FALSE;

        end;

      XML_ROLE_DOCTYPE_NONE:
        if @Parser.FStartDoctypeDeclHandler <> nil then
          HandleDefault := XML_FALSE;

      XML_ROLE_ENTITY_NONE:
        if (Dtd.KeepProcessing <> 0) and (@Parser.M_entityDeclHandler <> nil)
        then
          HandleDefault := XML_FALSE;

      XML_ROLE_NOTATION_NONE:
        if @Parser.M_notationDeclHandler <> nil then
          HandleDefault := XML_FALSE;

      XML_ROLE_ATTLIST_NONE:
        if (Dtd.KeepProcessing <> 0) and (@Parser.M_attlistDeclHandler <> nil)
        then
          HandleDefault := XML_FALSE;

      XML_ROLE_ELEMENT_NONE:
        if @Parser.M_elementDeclHandler <> nil then
          HandleDefault := XML_FALSE;

    end; { end of big case }

  _break:
    if (HandleDefault = XML_TRUE) and (@Parser.M_defaultHandler <> nil) then
      ReportDefault(Parser, Enc, S, Next);

    case Parser.M_parsingStatus.Parsing of
      XML_SUSPENDED:
        begin
          NextPtr^ := Next;
          Result := XML_ERROR_NONE;

          Exit;

        end;

      XML_FINISHED:
        begin
          Result := XML_ERROR_ABORTED;

          Exit;

        end;

    else
      begin
        S := Next;
        Tok := XmlPrologTok(Enc, Pointer(S), Pointer(End_), @Next);

      end;

    end;

  until False;

  { not reached }

end;

{ prologProcessor }
function PrologProcessor(Parser: XML_Parser; S, End_: PAnsiChar;
  NextPtr: PPAnsiChar): XML_Error;
var
  Next: PAnsiChar;
  Tok : Integer;

begin
  Next := S;
  Tok := XmlPrologTok(Parser.M_encoding, Pointer(S), Pointer(End_), @Next);

  Result := DoProlog(Parser, Parser.M_encoding, S, End_, Tok, Next, NextPtr,
    XML_Bool(not Parser.M_parsingStatus.FinalBuffer));

end;

{ prologInitProcessor }
function PrologInitProcessor(Parser: XML_Parser; S, End_: PAnsiChar;
  NextPtr: PPAnsiChar): XML_Error;
var
  Result_: XML_Error;

begin
  Result_ := InitializeEncoding(Parser);

  if Result_ <> XML_ERROR_NONE then
  begin
    Result := Result_;

    Exit;

  end;

  Parser.M_processor := @PrologProcessor;

  Result := PrologProcessor(Parser, S, End_, NextPtr);

end;

{ parserInit }
procedure ParserInit(Parser: XML_Parser; EncodingName: XML_PAnsiChar);
begin
  Parser.M_processor := @PrologInitProcessor;

  XmlPrologStateInit(@Parser.M_prologState);

  if EncodingName <> nil then
    Parser.M_protocolEncodingName := PoolCopyString(@Parser.M_tempPool,
      EncodingName)
  else
    Parser.M_protocolEncodingName := nil;

  Parser.FCurBase := nil;

  XmlInitEncoding(@Parser.MInitEncoding, @Parser.M_encoding, 0);

  Parser.M_userData := nil;
  Parser.M_handlerArg := nil;

  Parser.FStartElementHandler := nil;
  Parser.M_endElementHandler := nil;
  Parser.M_characterDataHandler := nil;
  Parser.M_processingInstructionHandler := nil;
  Parser.M_commentHandler := nil;
  Parser.FStartCdataSectionHandler := nil;
  Parser.M_endCdataSectionHandler := nil;
  Parser.M_defaultHandler := nil;
  Parser.FStartDoctypeDeclHandler := nil;
  Parser.M_endDoctypeDeclHandler := nil;
  Parser.M_unparsedEntityDeclHandler := nil;
  Parser.M_notationDeclHandler := nil;
  Parser.FStartNamespaceDeclHandler := nil;
  Parser.M_endNamespaceDeclHandler := nil;
  Parser.M_notStandaloneHandler := nil;
  Parser.M_externalEntityRefHandler := nil;
  Parser.M_externalEntityRefHandlerArg := Parser;
  Parser.M_skippedEntityHandler := nil;
  Parser.M_elementDeclHandler := nil;
  Parser.M_attlistDeclHandler := nil;
  Parser.M_entityDeclHandler := nil;
  Parser.M_xmlDeclHandler := nil;

  Parser.M_bufferPtr := Parser.M_buffer;
  Parser.M_bufferEnd := Parser.M_buffer;

  Parser.M_parseEndByteIndex := 0;
  Parser.M_parseEndPtr := nil;

  Parser.M_declElementType := nil;
  Parser.M_declAttributeId := nil;
  Parser.M_declEntity := nil;

  Parser.M_doctypeName := nil;
  Parser.M_doctypeSysid := nil;
  Parser.M_doctypePubid := nil;

  Parser.M_declAttributeType := nil;
  Parser.M_declNotationName := nil;
  Parser.M_declNotationPublicId := nil;
  Parser.M_declAttributeIsCdata := XML_FALSE;
  Parser.M_declAttributeIsId := XML_FALSE;

  FillChar(Parser.M_position, SizeOf(POSITION), 0);

  Parser.M_errorCode := XML_ERROR_NONE;

  Parser.M_eventPtr := nil;
  Parser.M_eventEndPtr := nil;
  Parser.M_positionPtr := nil;

  Parser.M_openInternalEntities := nil;
  Parser.M_defaultExpandInternalEntities := XML_TRUE;

  Parser.M_tagLevel := 0;
  Parser.M_tagStack := nil;
  Parser.MInheritedBindings := nil;
  Parser.M_nSpecifiedAtts := 0;

  Parser.M_unknownEncodingMem := nil;
  Parser.M_unknownEncodingRelease := nil;
  Parser.M_unknownEncodingData := nil;
  Parser.M_unknownEncodingAlloc := 0;

  Parser.M_parentParser := nil;
  Parser.M_parsingStatus.Parsing := XML_INITIALIZED;

{$IFDEF XML_DTD }
  Parser.M_isParamEntity := XML_FALSE;
  Parser.M_useForeignDTD := XML_FALSE;

  Parser.M_paramEntityParsing := XML_PARAM_ENTITY_PARSING_NEVER;

{$ENDIF }
end;

{ parserCreate }
function ParserCreate(EncodingName: XML_PAnsiChar;
  Memsuite: XML_Memory_Handling_Suite_ptr; NameSep: XML_PAnsiChar; Dtd: DTD_ptr)
  : XML_Parser;
var
  Parser: XML_Parser;
  Mtemp : XML_Memory_Handling_Suite_ptr;

begin
  Parser := nil;

  if Memsuite <> nil then
  begin
    Memsuite.Malloc_fcn(Pointer(Parser), SizeOf(XML_ParserStruct));

    if Parser <> nil then
    begin
      Mtemp := @Parser.M_mem;

      Mtemp.Malloc_fcn := Memsuite.Malloc_fcn;
      Mtemp.Realloc_fcn := Memsuite.Realloc_fcn;
      Mtemp.Free_fcn := Memsuite.Free_fcn;

    end;

  end
  else
  begin
    Expat_getmem(Pointer(Parser), SizeOf(XML_ParserStruct));

    if Parser <> nil then
    begin
      Mtemp := @Parser.M_mem;

      Mtemp.Malloc_fcn := @Expat_getmem;
      Mtemp.Realloc_fcn := @Expat_realloc;
      Mtemp.Free_fcn := @Expat_freemem;

    end;

  end;

  if Parser = nil then
  begin
    Result := nil;

    Exit;

  end;

  Parser.M_buffer := nil;
  Parser.M_bufferLim := nil;
  Parser.M_attsSize := INIT_ATTS_SIZE;

  Parser.M_attsAlloc := 0;
  Parser.M_nsAttsAlloc := 0;

  Parser.M_mem.Malloc_fcn(Pointer(Parser.M_atts),
    Parser.M_attsSize * SizeOf(ATTRIBUTE));

  if Parser.M_atts = nil then
  begin
    Parser.M_mem.Free_fcn(Pointer(Parser), SizeOf(XML_ParserStruct));

    Result := nil;

    Exit;

  end
  else
    Parser.M_attsAlloc := Parser.M_attsSize * SizeOf(ATTRIBUTE);

  Parser.M_mem.Malloc_fcn(Pointer(Parser.M_dataBuf),
    INIT_DATA_BUF_SIZE * SizeOf(XML_Char));

  if Parser.M_dataBuf = nil then
  begin
    Parser.M_mem.Free_fcn(Pointer(Parser.M_atts), Parser.M_attsAlloc);
    Parser.M_mem.Free_fcn(Pointer(Parser), SizeOf(XML_ParserStruct));

    Result := nil;

    Exit;

  end;

  Parser.M_dataBufEnd := XML_PAnsiChar(PtrComp(Parser.M_dataBuf) +
    INIT_DATA_BUF_SIZE);

  if Dtd <> nil then
    Parser.M_dtd := Dtd
  else
  begin
    Parser.M_dtd := DtdCreate(@Parser.M_mem);

    if Parser.M_dtd = nil then
    begin
      Parser.M_mem.Free_fcn(Pointer(Parser.M_dataBuf),
        INIT_DATA_BUF_SIZE * SizeOf(XML_Char));
      Parser.M_mem.Free_fcn(Pointer(Parser.M_atts), Parser.M_attsAlloc);
      Parser.M_mem.Free_fcn(Pointer(Parser), SizeOf(XML_ParserStruct));

      Result := nil;

      Exit;

    end;

  end;

  Parser.M_freeBindingList := nil;
  Parser.M_freeTagList := nil;
  Parser.M_freeInternalEntities := nil;

  Parser.M_groupSize := 0;
  Parser.M_groupAlloc := 0;
  Parser.M_groupConnector := nil;

  Parser.M_unknownEncodingHandler := nil;
  Parser.M_unknownEncodingHandlerData := nil;

  Parser.M_namespaceSeparator := '!';

  Parser.M_ns := XML_FALSE;
  Parser.M_ns_triplets := XML_FALSE;

  Parser.M_nsAtts := nil;
  Parser.M_nsAttsVersion := 0;
  Parser.M_nsAttsPower := 0;

  PoolInit(@Parser.M_tempPool, @Parser.M_mem);
  PoolInit(@Parser.M_temp2Pool, @Parser.M_mem);
  ParserInit(Parser, EncodingName);

  if (EncodingName <> nil) and (Parser.M_protocolEncodingName = nil) then
  begin
    XML_ParserFree(Parser);

    Result := nil;

    Exit;

  end;

  if NameSep <> nil then
  begin
    Parser.M_ns := XML_TRUE;

    Parser.MInternalEncoding := XmlGetInternalEncodingNS;
    Parser.M_namespaceSeparator := NameSep^;

  end
  else
    Parser.MInternalEncoding := XmlGetInternalEncoding;

  Result := Parser;

end;

{ setContext {.. }
function SetContext(Parser: XML_Parser; Context: XML_PAnsiChar): XML_Bool;
begin
end;

{ XML_PARSERCREATE }
function XML_ParserCreate;
begin
  Result := XML_ParserCreate_MM(Encoding, nil, nil);

end;

{ XML_PARSER_CREATE_MM }
function XML_ParserCreate_MM;
var
  Parser: XML_Parser;

begin
  Parser := ParserCreate(Encoding, Memsuite, NamespaceSeparator, nil);

  if (Parser <> nil) and (Parser.M_ns <> 0) then
    { implicit context only set for root parser, since child
      parsers (i.e. external entity parsers) will inherit it }
    if not SetContext(Parser, @ImplicitContext[0]) <> 0 then
    begin
      XML_ParserFree(Parser);

      Result := nil;

      Exit;

    end;

  Result := Parser;

end;

{ XML_SETUSERDATA }
procedure XML_SetUserData;
begin
  if Parser.M_handlerArg = Parser.M_userData then
  begin
    Parser.M_handlerArg := UserData;
    Parser.M_userData := UserData;

  end
  else
    Parser.M_userData := UserData;

end;

{ XML_SETELEMENTHANDLER }
procedure XML_SetElementHandler;
begin
  Parser.FStartElementHandler := Start;
  Parser.M_endElementHandler := End_;

end;

{ XML_SETCHARACTERDATAHANDLER }
procedure XML_SetCharacterDataHandler;
begin
  Parser.M_characterDataHandler := Handler;

end;

{ XML_GetBuffer }
function XML_GetBuffer(Parser: XML_Parser; Len: Integer): Pointer;
var
  NeededSize, Keep, Offset, BufferSize: Integer;

  NewBuf: PAnsiChar;

begin
  case Parser.M_parsingStatus.Parsing of
    XML_SUSPENDED:
      begin
        Parser.M_errorCode := XML_ERROR_SUSPENDED;

        Result := nil;

        Exit;

      end;

    XML_FINISHED:
      begin
        Parser.M_errorCode := XML_ERROR_FINISHED;

        Result := nil;

        Exit;

      end;

  end;

  if Len > PtrComp(Parser.M_bufferLim) - PtrComp(Parser.M_bufferEnd) then
  begin
    { FIXME avoid integer overfLow }
    NeededSize := Len + (PtrComp(Parser.M_bufferEnd) -
      PtrComp(Parser.M_bufferPtr));

{$IFDEF XML_CONTEXT_BYTES }
    Keep := PtrComp(Parser.M_bufferPtr) - PtrComp(Parser.M_buffer);

    if Keep > _XML_CONTEXT_BYTES then
      Keep := _XML_CONTEXT_BYTES;

    Inc(NeededSize, Keep);

{$ENDIF }
    if NeededSize <= PtrComp(Parser.M_bufferLim) - PtrComp(Parser.M_buffer) then
    begin
{$IFDEF XML_CONTEXT_BYTES }
      if Keep < PtrComp(Parser.M_bufferPtr) - PtrComp(Parser.M_buffer) then
      begin
        Offset := PtrComp(Parser.M_bufferPtr) - PtrComp(Parser.M_buffer) - Keep;

        Move(PAnsiChar(PtrComp(Parser.M_buffer) + Offset)^, Parser.M_buffer^,
          PtrComp(Parser.M_bufferEnd) - PtrComp(Parser.M_bufferPtr) + Keep);

        Dec(PtrComp(Parser.M_bufferEnd), Offset);
        Dec(PtrComp(Parser.M_bufferPtr), Offset);

      end;

{$ELSE }
      Move(Parser.M_bufferPtr^, Parser.M_buffer^, PtrComp(Parser.M_bufferEnd) -
        PtrComp(Parser.M_bufferPtr));

      Parser.M_bufferEnd := PAnsiChar(PtrComp(Parser.M_buffer) +
        (PtrComp(Parser.M_bufferEnd) - PtrComp(Parser.M_bufferPtr)));
      Parser.M_bufferPtr := Parser.M_buffer;

{$ENDIF }
    end
    else
    begin
      BufferSize := PtrComp(Parser.M_bufferLim) - PtrComp(Parser.M_bufferPtr);

      if BufferSize = 0 then
        BufferSize := INIT_BUFFER_SIZE;

      repeat
        BufferSize := BufferSize * 2;

      until BufferSize >= NeededSize;

      Parser.M_mem.Malloc_fcn(Pointer(NewBuf), BufferSize);

      if NewBuf = nil then
      begin
        Parser.M_errorCode := XML_ERROR_NO_MEMORY;

        Result := nil;

        Exit;

      end;

      Parser.M_bufferLim := PAnsiChar(PtrComp(NewBuf) + BufferSize);

{$IFDEF XML_CONTEXT_BYTES }
      if Parser.M_bufferPtr <> nil then
      begin
        Keep := PtrComp(Parser.M_bufferPtr) - PtrComp(Parser.M_buffer);

        if Keep > _XML_CONTEXT_BYTES then
          Keep := _XML_CONTEXT_BYTES;

        Move(PAnsiChar(PtrComp(Parser.M_bufferPtr) - Keep)^, NewBuf^,
          PtrComp(Parser.M_bufferEnd) - PtrComp(Parser.M_bufferPtr) + Keep);

        Expat_freemem(Pointer(Parser.M_buffer), Parser.M_bufferAloc);

        Parser.M_buffer := NewBuf;
        Parser.M_bufferAloc := BufferSize;

        Parser.M_bufferEnd :=
          PAnsiChar(PtrComp(Parser.M_buffer) + (PtrComp(Parser.M_bufferEnd) -
          PtrComp(Parser.M_bufferPtr)) + Keep);

        Parser.M_bufferPtr := PAnsiChar(PtrComp(Parser.M_buffer) + Keep);

      end
      else
      begin
        Parser.M_bufferEnd :=
          PAnsiChar(PtrComp(NewBuf) + (PtrComp(Parser.M_bufferEnd) -
          PtrComp(Parser.M_bufferPtr)));
        Parser.M_buffer := NewBuf;
        Parser.M_bufferPtr := NewBuf;
        Parser.M_bufferAloc := BufferSize;

      end;

{$ELSE }
      if Parser.M_bufferPtr <> nil then
      begin
        Move(Parser.M_bufferPtr^, NewBuf^, PtrComp(Parser.M_bufferEnd) -
          PtrComp(Parser.M_bufferPtr));

        Expat_freemem(Pointer(Parser.M_buffer), Parser.M_bufferAloc);

      end;

      Parser.M_bufferEnd :=
        PAnsiChar(PtrComp(NewBuf) + (PtrComp(Parser.M_bufferEnd) -
        PtrComp(Parser.M_bufferPtr)));
      Parser.M_buffer := NewBuf;
      Parser.M_bufferPtr := NewBuf;
      Parser.M_bufferAloc := BufferSize;

{$ENDIF }
    end;

  end;

  Result := Parser.M_bufferEnd;

end;

{ errorProcessor }
function ErrorProcessor(Parser: XML_Parser; S, End_: PAnsiChar;
  NextPtr: PPAnsiChar): XML_Error;
begin
  Result := Parser.M_errorCode;

end;

{ XML_ParseBuffer }
function XML_ParseBuffer(Parser: XML_Parser; Len, IsFinal: Integer): XML_Status;
var
  Start  : PAnsiChar;
  Result_: XML_Status;

begin
  Result_ := XML_STATUS_OK;

  case Parser.M_parsingStatus.Parsing of
    XML_SUSPENDED:
      begin
        Parser.M_errorCode := XML_ERROR_SUSPENDED;

        Result := XML_STATUS_ERROR;

        Exit;

      end;

    XML_FINISHED:
      begin
        Parser.M_errorCode := XML_ERROR_FINISHED;

        Result := XML_STATUS_ERROR;

        Exit;

      end;

  else
    Parser.M_parsingStatus.Parsing := XML_PARSING_;

  end;

  Start := Parser.M_bufferPtr;
  Parser.M_positionPtr := Start;

  Inc(PtrComp(Parser.M_bufferEnd), Len);

  Parser.M_parseEndPtr := Parser.M_bufferEnd;

  Inc(PtrComp(Parser.M_parseEndByteIndex), Len);

  Parser.M_parsingStatus.FinalBuffer := XML_Bool(IsFinal);

  Parser.M_errorCode := Parser.M_processor(Parser, Start, Parser.M_parseEndPtr,
    @Parser.M_bufferPtr);

  if Parser.M_errorCode <> XML_ERROR_NONE then
  begin
    Parser.M_eventEndPtr := Parser.M_eventPtr;
    Parser.M_processor := @ErrorProcessor;

    Result := XML_STATUS_ERROR;

    Exit;

  end
  else
    case Parser.M_parsingStatus.Parsing of
      XML_SUSPENDED:
        Result_ := XML_STATUS_SUSPENDED;

      XML_INITIALIZED, XML_PARSING_:
        if IsFinal <> 0 then
        begin
          Parser.M_parsingStatus.Parsing := XML_FINISHED;

          Result := Result_;

          Exit;

        end;

    else
      { should not happen }
      NoP;

    end;

  Parser.M_encoding.UpdatePosition(Parser.M_encoding,
    Pointer(Parser.M_positionPtr), Pointer(Parser.M_bufferPtr),
    @Parser.M_position);

  Parser.M_positionPtr := Parser.M_bufferPtr;

  Result := Result_;

end;

{ XML_PARSE {.. }
function XML_Parse;
var
  Buff: Pointer;

begin
  case Parser.M_parsingStatus.Parsing of
    XML_SUSPENDED:
      begin
        Parser.M_errorCode := XML_ERROR_SUSPENDED;

        Result := XML_STATUS_ERROR;

        Exit;

      end;

    XML_FINISHED:
      begin
        Parser.M_errorCode := XML_ERROR_FINISHED;

        Result := XML_STATUS_ERROR;

        Exit;

      end;

  else
    Parser.M_parsingStatus.Parsing := XML_PARSING_;

  end;

  if Len = 0 then
  begin
    NoP;

  end
{$IFNDEF XML_CONTEXT_BYTES }
  else if Parser.M_bufferPtr = Parser.M_bufferEnd then
  begin
    NoP;

  end

{$ENDIF }
  else
  begin
    Buff := XML_GetBuffer(Parser, Len);

    if Buff = nil then
      Result := XML_STATUS_ERROR

    else
    begin
      Move(S^, Buff^, Len);

      Result := XML_ParseBuffer(Parser, Len, IsFinal);

    end;

  end;

end;

{ XML_GETERRORCODE {.. }
function XML_GetErrorCode;
begin
end;

{ XML_ERRORSTRING {.. }
function XML_ErrorString;
begin
end;

{ XML_GETCURRENTLINENUMBER {.. }
function XML_GetCurrentLineNumber;
begin
end;

{ destroyBindings }
procedure DestroyBindings(Bindings: BINDING_ptr; Parser: XML_Parser);
var
  B: BINDING_ptr;

begin
  repeat
    B := Bindings;

    if B = nil then
      Break;

    Bindings := B.NextTagBinding;

    Parser.M_mem.Free_fcn(Pointer(B.Uri), B.UriAlloc);
    Parser.M_mem.Free_fcn(Pointer(B), SizeOf(Expat.BINDING));

  until False;

end;

{ XML_PARSERFREE }
procedure XML_ParserFree;
var
  TagList, P: TAG_ptr;

  EntityList, OpenEntity: OPEN_INTERNAL_ENTITY_ptr;

begin
  if Parser = nil then
    Exit;

  { free tagStack and freeTagList }
  TagList := Parser.M_tagStack;

  repeat
    if TagList = nil then
    begin
      if Parser.M_freeTagList = nil then
        Break;

      TagList := Parser.M_freeTagList;

      Parser.M_freeTagList := nil;

    end;

    P := TagList;
    TagList := TagList.Parent;

    Parser.M_mem.Free_fcn(Pointer(P.Buf), P.Alloc);
    DestroyBindings(P.Bindings, Parser);
    Parser.M_mem.Free_fcn(Pointer(P), SizeOf(Expat.TAG));

  until False;

  { free openInternalEntities and freeInternalEntities }
  EntityList := Parser.M_openInternalEntities;

  repeat
    if EntityList = nil then
    begin
      if Parser.M_freeInternalEntities = nil then
        Break;

      EntityList := Parser.M_freeInternalEntities;

      Parser.M_freeInternalEntities := nil;

    end;

    OpenEntity := EntityList;
    EntityList := EntityList.Next;

    Parser.M_mem.Free_fcn(Pointer(OpenEntity), SizeOf(OPEN_INTERNAL_ENTITY));

  until False;

  DestroyBindings(Parser.M_freeBindingList, Parser);
  DestroyBindings(Parser.MInheritedBindings, Parser);

  PoolDestroy(@Parser.M_tempPool);
  PoolDestroy(@Parser.M_temp2Pool);

{$IFDEF XML_DTD }
  { external parameter entity parsers share the DTD structure
    parser->m_dtd with the root parser, so we must not destroy it }
  if (Parser.M_isParamEntity = 0) and (Parser.M_dtd <> nil) then
{$ELSE }
  if Parser.M_dtd <> nil then {$ENDIF }
    DtdDestroy(Parser.M_dtd, XML_Bool(Parser.M_parentParser = nil),
      @Parser.M_mem);

  Parser.M_mem.Free_fcn(Pointer(Parser.M_atts), Parser.M_attsAlloc);
  Parser.M_mem.Free_fcn(Pointer(Parser.M_groupConnector), Parser.M_groupAlloc);
  Parser.M_mem.Free_fcn(Pointer(Parser.M_buffer), Parser.M_bufferAloc);
  Parser.M_mem.Free_fcn(Pointer(Parser.M_dataBuf),
    INIT_DATA_BUF_SIZE * SizeOf(XML_Char));
  Parser.M_mem.Free_fcn(Pointer(Parser.M_nsAtts), Parser.M_nsAttsAlloc);
  Parser.M_mem.Free_fcn(Pointer(Parser.M_unknownEncodingMem),
    Parser.M_unknownEncodingAlloc);

  if @Parser.M_unknownEncodingRelease <> nil then
    Parser.M_unknownEncodingRelease(Parser.M_unknownEncodingData);

  Parser.M_mem.Free_fcn(Pointer(Parser), SizeOf(XML_ParserStruct));

end;
