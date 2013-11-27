unit AggArray;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012                                                      //
//                                                                            //
//  Based on:                                                                 //
//    Pascal port by Milan Marusinec alias Milano (milan@marusinec.sk)        //
//    Copyright (c) 2005-2006, see http://www.aggpas.org                      //
//                                                                            //
//  Original License:                                                         //
//    Anti-Grain Geometry - Version 2.4 (Public License)                      //
//    Copyright (C) 2002-2005 Maxim Shemanarev (http://www.antigrain.com)     //
//    Contact: McSeem@antigrain.com / McSeemAgg@yahoo.com                     //
//                                                                            //
//  Permission to copy, use, modify, sell and distribute this software        //
//  is granted provided this copyright notice appears in all copies.          //
//  This software is provided "as is" without express or implied              //
//  warranty, and with no claim as to its suitability for any purpose.        //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  AggBasics;

type
  TAggFuncLess = function(E1, E2: Pointer) : Boolean;
  TAggFuncEqual = function(E1, E2: Pointer): Boolean;

  TAggCustomArray = class
  protected
    function GetSize: Cardinal; virtual; abstract;
    function GetEntry: Cardinal; virtual; abstract;
    function ArrayOperator(Index: Cardinal): Pointer; virtual; abstract;
  public
    function At(Index: Cardinal): Pointer; virtual;

    property Size: Cardinal read GetSize;
    property Entry: Cardinal read GetEntry;
    property ItemPointer[Index: Cardinal]: Pointer read ArrayOperator; default;
  end;

  TAggRangeAdaptor = class(TAggCustomArray)
  private
    FStart: Cardinal;
  protected
    FSize: Cardinal;
    FArray: TAggCustomArray;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(Index: Cardinal): Pointer; override;
  public
    constructor Create(AArray: TAggCustomArray; Start, ASize: Cardinal);
  end;

  TAggPodArrayAdaptor = class(TAggCustomArray)
  private
    FEntry: Cardinal;
  protected
    FSize: Cardinal;
    FArray: Pointer;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(Index: Cardinal): Pointer; override;
  public
    constructor Create(AArray: Pointer; ASize, Entry: Cardinal);

    function At(Index: Cardinal): Pointer; override;
  end;

  TAggPodAutoArray = class(TAggCustomArray)
  protected
    FSize: Cardinal;
    FArray: Pointer;
    FEntrySize: Cardinal;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(Index: Cardinal): Pointer; override;
  public
    constructor Create(ASize, EntrySize: Cardinal);
    destructor Destroy; override;

    property EntrySize: Cardinal read FEntrySize;
  end;

  // ------------------------------------------------------------------------
  // A simple class template to store Plain Old Data, a vector
  // of a fixed size. The data is continous in memory
  // ------------------------------------------------------------------------
  TAggPodArray = class(TAggCustomArray)
  private
    FCapacity: Cardinal;
  protected
    FSize: Cardinal;
    FArray: Pointer;
    FEntrySize: Cardinal;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(Index: Cardinal): Pointer; override;
  public
    constructor Create(EntrySize: Cardinal); overload;
    constructor Create(EntrySize, ASize: Cardinal); overload;
    destructor Destroy; override;

    procedure Allocate(ASize: Cardinal; ExtraTail: Cardinal = 0);
    procedure Resize(NewSize: Cardinal);
    procedure Capacity(Cap, ExtraTail: Cardinal);

    procedure Zero;
    procedure Add(Value: Pointer);
    function Data: Pointer;

    property ArrayPointer: Pointer read FArray;
    property EntrySize: Cardinal read FEntrySize;
  end;

  TAggPodVector = TAggPodArray;

  // ------------------------------------------------------------------------
  // A simple class template to store Plain Old Data, similar to std::deque
  // It doesn't reallocate memory but instead, uses blocks of data of size
  // of (1 << S), that is, power of two. The data is NOT contiguous in memory,
  // so the only valid access method is operator [] or curr(), prev(), next()
  //
  // There reallocs occure only when the pool of pointers to blocks needs
  // to be extended (it happens very rarely). You can control the value
  // of increment to reallocate the pointer buffer. See the second constructor.
  // By default, the incremeent value equals (1 << S), i.e., the block size.
  // ------------------------------------------------------------------------
  TAggPodDeque = class(TAggCustomArray)
  private
    FBlockShift, FBlockSize, FBlockMask: Cardinal;

    FNumBlocks, FMaxBlocks, FBlockPtrInc: Cardinal;

    FBlocks: PPointer;
  protected
    FSize: Cardinal;
    FEntrySize: Cardinal;
    function GetDataPointer: Pointer;
    procedure FreeBlocks;
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;
    function ArrayOperator(Index: Cardinal): Pointer; override;
  public
    constructor Create(EntrySize: Cardinal; AShift: Cardinal = 6); overload;
    constructor Create(BlockPointerInc, EntrySize: Cardinal;
      AShift: Cardinal); overload;
    destructor Destroy; override;

    procedure Clear;
    procedure RemoveAll;
    procedure RemoveLast;

    procedure Add(Val: Pointer); virtual;
    procedure ModifyLast(Val: Pointer); virtual;

    procedure CutAt(ASize: Cardinal);
    procedure AssignOperator(V: TAggPodDeque);

    function Curr(Idx: Cardinal): Pointer;
    function Prev(Idx: Cardinal): Pointer;
    function Next(Idx: Cardinal): Pointer;
    function Last: Pointer;

    function AllocateContinuousBlock(NumElements: Cardinal): Integer;
    procedure AllocateBlock(Nb: Cardinal);

    property EntrySize: Cardinal read FEntrySize;
  end;

  TAggPodBVector = TAggPodDeque;

  // ------------------------------------------------------------------------
  // Allocator for arbitrary POD data. Most usable in different cache
  // systems for efficient memory allocations.
  // Memory is allocated with blocks of fixed size ("block_size" in
  // the constructor). If required size exceeds the block size the allocator
  // creates a new block of the required size. However, the most efficient
  // use is when the average reqired size is much less than the block size.
  // ------------------------------------------------------------------------
  PAggPodAlloc = ^TAggPodAlloc;
  TAggPodAlloc = record
    Ptr: PInt8u;
    Size: Cardinal;
  end;

  TAggPodAllocator = class
  private
    FBlockSize, FBlockPtrInc, FNumBlocks, FMaxBlocks: Cardinal;

    FBlocks: PAggPodAlloc;
    FBufPtr: PInt8u;

    FRest: Cardinal;
  public
    constructor Create(FBlockSize: Cardinal; BlockPointerInc: Cardinal = 256 - 8);
    destructor Destroy; override;

    procedure RemoveAll;

    function Allocate(Size: Cardinal; Alignment: Cardinal = 1): PInt8u;

    procedure AllocateBlock(Size: Cardinal);
  end;

procedure QuickSort(Arr: TAggCustomArray; Less: TAggFuncLess);
function RemoveDuplicates(Arr: TAggCustomArray; Equal: TAggFuncEqual): Cardinal;

function IntLess(A, B: Pointer): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function IntGreater(A, B: Pointer): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function CardinalLess(A, B: Pointer): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function CardinalGreater(A, B: Pointer): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

implementation


procedure QuickSort(Arr: TAggCustomArray; Less: TAggFuncLess);
const
  QuickSortThreshold = 9;
type
  Int80_ptr = ^Int80;
  Int80 = array [0..79] of Integer;

var
  Temp, E1, E2: Pointer;
  Swap: Cardinal;
  Stack: Int80;
  Top: Int80_ptr;
  Limit, Base, Len, I, J, Pivot: Integer;
begin
  if Arr.Size < 2 then
    Exit;

  AggGetMem(Temp, Arr.Entry);

  Swap := Arr.Entry;
  Top := @Stack;
  Limit := Arr.Size;
  Base := 0;

  repeat
    Len := Limit - Base;

    if Len > QuickSortThreshold then
    begin
      // we use base + len/2 as the pivot
      Pivot := Base + Len div 2;

      // SwapElements(arr[base], arr[pivot]);
      Move(Arr[Base]^, Temp^, Swap);
      Move(Arr[Pivot]^, Arr[Base]^, Swap);
      Move(Temp^, Arr[Pivot]^, Swap);

      I := Base + 1;
      J := Limit - 1;

      // now ensure that *i <= *base <= *j
      E1 := Arr[J];
      E2 := Arr[I];

      if Less(E1, E2) then
      begin
        // SwapElements(*e1, *e2);
        Move(E1^, Temp^, Swap);
        Move(E2^, E1^, Swap);
        Move(Temp^, E2^, Swap);
      end;

      E1 := Arr[Base];
      E2 := Arr[I];

      if Less(E1, E2) then
      begin
        // SwapElements(*e1, *e2);
        Move(E1^, Temp^, Swap);
        Move(E2^, E1^, Swap);
        Move(Temp^, E2^, Swap);
      end;

      E1 := Arr[J];
      E2 := Arr[Base];

      if Less(E1, E2) then
      begin
        // SwapElements(*e1, *e2);
        Move(E1^, Temp^, Swap);
        Move(E2^, E1^, Swap);
        Move(Temp^, E2^, Swap);
      end;

      repeat
        repeat
          Inc(I)
        until not Less(Arr[I], Arr[Base]);

        repeat
          Dec(J);
        until not Less(Arr[Base], Arr[J]);

        if I > J then
          Break;

        // SwapElements(arr[i], arr[j]);
        Move(Arr[I]^, Temp^, Swap);
        Move(Arr[J]^, Arr[I]^, Swap);
        Move(Temp^, Arr[J]^, Swap);
      until False;

      // SwapElements(arr[base], arr[j]);
      Move(Arr[Base]^, Temp^, Swap);
      Move(Arr[J]^, Arr[Base]^, Swap);
      Move(Temp^, Arr[J]^, Swap);

      // now, push the largest sub-array
      if J - Base > Limit - I then
      begin
        Top^[0] := Base;
        Top^[1] := J;
        Base := I;
      end
      else
      begin
        Top^[0] := I;
        Top^[1] := Limit;
        Limit := J;
      end;

      Inc(PtrComp(Top), 2 * SizeOf(Integer));
    end
    else
    begin
      // the sub-array is small, perform insertion sort
      J := Base;
      I := J + 1;

      while I < Limit do
      begin
        E1 := Arr[J + 1];
        E2 := Arr[J];

        while Less(E1, E2) do
        begin
          // SwapElements(*e1, *e2);
          Move(E1^, Temp^, Swap);
          Move(E2^, E1^, Swap);
          Move(Temp^, E2^, Swap);

          if J = Base then
            Break;

          Dec(J);

          E1 := Arr[J + 1];
          E2 := Arr[J];
        end;

        J := I;

        Inc(I);
      end;

      if PtrComp(Top) > PtrComp(@Stack) then
      begin
        Dec(PtrComp(Top), 2 * SizeOf(Integer));

        Base := Top^[0];
        Limit := Top^[1];
      end
      else
        Break;
    end;
  until False;

  AggFreeMem(Temp, Arr.Entry);
end;

// Remove duplicates from a sorted array. It doesn't cut the
// tail of the array, it just returns the number of remaining elements.
function RemoveDuplicates(Arr: TAggCustomArray; Equal: TAggFuncEqual): Cardinal;
var
  I, J: Cardinal;
  E: Pointer;
begin
  if Arr.Size < 2 then
  begin
    Result := Arr.Size;

    Exit;
  end;

  I := 1;
  J := 1;

  while I < Arr.Size do
  begin
    E := Arr[I];

    if not Equal(E, Arr.ArrayOperator(I - 1)) then
    begin
      Move(E^, Arr.ArrayOperator(J)^, Arr.Entry);
      Inc(J);
    end;

    Inc(I);
  end;

  Result := J;
end;

function IntLess(A, B: Pointer): Boolean;
begin
  Result := PInteger(A)^ < PInteger(B)^;
end;

function IntGreater(A, B: Pointer): Boolean;
begin
  Result := PInteger(A)^ > PInteger(B)^;
end;

function CardinalLess(A, B: Pointer): Boolean;
begin
  Result := PCardinal(A)^ < PCardinal(B)^;
end;

function CardinalGreater(A, B: Pointer): Boolean;
begin
  Result := PCardinal(A)^ > PCardinal(B)^;
end;


{ TAggCustomArray }

function TAggCustomArray.At(Index: Cardinal): Pointer;
begin
  At := ArrayOperator(Index);
end;


{ TAggRangeAdaptor }

constructor TAggRangeAdaptor.Create(AArray: TAggCustomArray;
  Start, ASize: Cardinal);
begin
  FArray := AArray;
  FStart := Start;
  FSize := ASize;
end;

function TAggRangeAdaptor.GetSize: Cardinal;
begin
  Result := FSize;
end;

function TAggRangeAdaptor.GetEntry: Cardinal;
begin
  Result := FArray.Entry;
end;

function TAggRangeAdaptor.ArrayOperator(Index: Cardinal): Pointer;
begin
  Result := FArray.ArrayOperator(FStart + Index);
end;


{ TAggPodArrayAdaptor }

constructor TAggPodArrayAdaptor.Create(AArray: Pointer; ASize, Entry: Cardinal);
begin
  FArray := AArray;
  FSize := ASize;
  FEntry := Entry;
end;

function TAggPodArrayAdaptor.GetSize;
begin
  Result := FSize;
end;

function TAggPodArrayAdaptor.GetEntry;
begin
  Result := FEntry;
end;

function TAggPodArrayAdaptor.ArrayOperator(Index: Cardinal): Pointer;
begin
  Result := Pointer(PtrComp(FArray) + Index * SizeOf(FEntry));
end;

function TAggPodArrayAdaptor.At(Index: Cardinal): Pointer;
begin
  Result := Pointer(PtrComp(FArray) + Index * FEntry);
end;


{ TAggPodAutoArray }

constructor TAggPodAutoArray.Create;
begin
  FSize := ASize;
  FEntrySize := EntrySize;

  AggGetMem(FArray, FSize * FEntrySize);
end;

destructor TAggPodAutoArray.Destroy;
begin
  AggFreeMem(FArray, FSize * FEntrySize);
  inherited
end;

function TAggPodAutoArray.GetSize;
begin
  Result := FSize;
end;

function TAggPodAutoArray.GetEntry;
begin
  Result := FEntrySize;
end;

function TAggPodAutoArray.ArrayOperator(Index: Cardinal): Pointer;
begin
  Result := Pointer(PtrComp(FArray) + Index * FEntrySize);
end;


{ TAggPodArray }

constructor TAggPodArray.Create(EntrySize: Cardinal);
begin
  FEntrySize := EntrySize;
  FSize := 0;
  FCapacity := 0;

  FArray := nil;
end;

constructor TAggPodArray.Create(EntrySize, ASize: Cardinal);
begin
  Create(EntrySize);
  Allocate(ASize);
  FSize := 0;
end;

destructor TAggPodArray.Destroy;
begin
  if FArray <> nil then
    AggFreeMem(FArray, FCapacity * FEntrySize);

  inherited;
end;

// Allocate n elements. All data is lost,
// but elements can be accessed in range 0...size-1.
procedure TAggPodArray.Allocate(ASize: Cardinal; ExtraTail: Cardinal = 0);
begin
  Capacity(ASize, ExtraTail);

  FSize := ASize;
end;

// Resize keeping the content.
procedure TAggPodArray.Resize;
var
  Buff: Pointer;
begin
  if NewSize > FSize then
    if NewSize > FCapacity then
    begin
      AggGetMem(Buff, NewSize * FEntrySize);

      if FArray <> nil then
      begin
        Move(FArray^, Buff^, FSize * FEntrySize);

        AggFreeMem(FArray, FCapacity * FEntrySize);
      end;

      FArray := Buff;
      FCapacity := NewSize;

    end
    else
  else
    FSize := NewSize;
end;

procedure TAggPodArray.Capacity(Cap, ExtraTail: Cardinal);
begin
  FSize := 0;

  if Cap > FCapacity then
  begin
    AggFreeMem(FArray, FCapacity * FEntrySize);

    FCapacity := Cap + ExtraTail;

    if FCapacity > 0 then
      AggGetMem(FArray, FCapacity * FEntrySize)
    else
      FArray := 0;
  end;
end;

procedure TAggPodArray.Zero;
begin
  FillChar(FArray^, FEntrySize * FSize, 0);
end;

procedure TAggPodArray.Add(Value: Pointer);
begin
  Move(Value^, Pointer(PtrComp(FArray) + FSize * FEntrySize)^, FEntrySize);
  Inc(FSize);
end;

function TAggPodArray.Data;
begin
  Result := FArray;
end;

function TAggPodArray.GetSize;
begin
  Result := FSize;
end;

function TAggPodArray.GetEntry;
begin
  Result := FEntrySize;
end;

function TAggPodArray.ArrayOperator(Index: Cardinal): Pointer;
begin
  Result := Pointer(PtrComp(FArray) + Index * FEntrySize);
end;


{ TAggPodDeque }

constructor TAggPodDeque.Create(EntrySize: Cardinal; AShift: Cardinal = 6);
begin
  FBlockShift := AShift;
  FBlockSize := 1 shl FBlockShift;
  FBlockMask := FBlockSize - 1;

  FSize := 0;
  FNumBlocks := 0;
  FMaxBlocks := 0;
  FBlocks := 0;
  FBlockPtrInc := FBlockSize;

  FEntrySize := EntrySize;
end;

constructor TAggPodDeque.Create(BlockPointerInc, EntrySize: Cardinal;
  AShift: Cardinal);
begin
  Create(EntrySize, AShift);

  FBlockPtrInc := BlockPointerInc;
end;

destructor TAggPodDeque.Destroy;
begin
  FreeBlocks;

  inherited;
end;

procedure TAggPodDeque.FreeBlocks;
var
  Blk: Pointer;
begin
  if FNumBlocks <> 0 then
  begin
    Blk := Pointer(PtrComp(FBlocks) + (FNumBlocks - 1) * SizeOf(Pointer));

    while FNumBlocks <> 0 do
    begin
      AggFreeMem(PPointer(Blk)^, FBlockSize * FEntrySize);

      Dec(PtrComp(Blk), SizeOf(Pointer));
      Dec(FNumBlocks);
    end;

    AggFreeMem(Pointer(FBlocks), FMaxBlocks * SizeOf(Pointer));
  end;
end;

procedure TAggPodDeque.Clear;
begin
  FSize := 0;
end;

procedure TAggPodDeque.RemoveAll;
begin
  FSize := 0;
end;

procedure TAggPodDeque.RemoveLast;
begin
  if FSize <> 0 then
    Dec(FSize);
end;

procedure TAggPodDeque.Add(Val: Pointer);
var
  P: Pointer;
begin
  P := GetDataPointer;

  Move(Val^, P^, FEntrySize);
  Inc(FSize);
end;

procedure TAggPodDeque.ModifyLast(Val: Pointer);
begin
  RemoveLast;
  Add(Val);
end;

procedure TAggPodDeque.CutAt(ASize: Cardinal);
begin
  if ASize < FSize then
    FSize := ASize;
end;

function TAggPodDeque.GetSize;
begin
  Result := FSize;
end;

function TAggPodDeque.GetEntry;
begin
  Result := FEntrySize;
end;

function TAggPodDeque.ArrayOperator(Index: Cardinal): Pointer;
var
  P: PPointer;
begin
  P := FBlocks;
  Inc(P, (Index shr FBlockShift));
  Result := P^;
  Inc(PByte(Result), (Index and FBlockMask) * FEntrySize);
end;

procedure TAggPodDeque.AssignOperator;
var
  I       : Cardinal;
  Src, Dst: Pointer;
begin
  FreeBlocks;

  FBlockShift := V.FBlockShift;
  FBlockSize := V.FBlockSize;
  FBlockMask := V.FBlockMask;

  FSize := V.FSize;
  FEntrySize := V.FEntrySize;

  FNumBlocks := V.FNumBlocks;
  FMaxBlocks := V.FMaxBlocks;

  FBlockPtrInc := V.FBlockPtrInc;

  if FMaxBlocks <> 0 then
    AggGetMem(Pointer(FBlocks), FMaxBlocks * SizeOf(Pointer))
  else
    FBlocks := nil;

  Src := V.FBlocks;
  Dst := FBlocks;
  I := 0;

  while I < FNumBlocks do
  begin
    AggGetMem(PPointer(Dst)^, FBlockSize * FEntrySize);

    Move(PPointer(Src)^^, PPointer(Dst)^^, FBlockSize * FEntrySize);

    Inc(PtrComp(Src), SizeOf(Pointer));
    Inc(PtrComp(Dst), SizeOf(Pointer));
    Inc(I);
  end;
end;

function TAggPodDeque.Curr;
begin
  Result := ArrayOperator(Idx);
end;

function TAggPodDeque.Prev;
begin
  Result := ArrayOperator((Idx + FSize - 1) mod FSize);
end;

function TAggPodDeque.Next;
begin
  Result := ArrayOperator((Idx + 1) mod FSize);
end;

function TAggPodDeque.Last: Pointer;
begin
  Result := ArrayOperator(FSize - 1);
end;

function TAggPodDeque.AllocateContinuousBlock;
var
  Rest, Index: Cardinal;

begin
  if NumElements < FBlockSize then
  begin
    GetDataPointer; // Allocate initial block if necessary

    Rest := FBlockSize - (FSize and FBlockMask);

    if NumElements <= Rest then
    begin
      // The rest of the block is good, we can use it
      index := FSize;

      Inc(FSize, NumElements);

      Result := index;

      Exit;
    end;

    // New block
    Inc(FSize, Rest);

    GetDataPointer;

    index := FSize;

    Inc(FSize, NumElements);

    Result := index;

    Exit;
  end;

  Result := -1; // Impossible to allocate
end;

procedure TAggPodDeque.AllocateBlock(Nb: Cardinal);
var
  NewBlocks: Pointer;
  Blocks: PPointer;
begin
  if Nb >= FMaxBlocks then
  begin
    AggGetMem(NewBlocks, (FMaxBlocks + FBlockPtrInc) * SizeOf(Pointer));

    if FBlocks <> nil then
    begin
      Move(FBlocks^, NewBlocks^, FNumBlocks * SizeOf(Pointer));

      AggFreeMem(Pointer(FBlocks), FMaxBlocks * SizeOf(Pointer));
    end;

    FBlocks := NewBlocks;

    Inc(FMaxBlocks, FBlockPtrInc);
  end;

  Blocks := FBlocks;
  Inc(Blocks, Nb);
  AggGetMem(Blocks^, FBlockSize * FEntrySize);

  Inc(FNumBlocks);
end;

function TAggPodDeque.GetDataPointer: Pointer;
var
  Nb: Cardinal;
  Block: PPointer;
begin
  Nb := FSize shr FBlockShift;

  if Nb >= FNumBlocks then
    AllocateBlock(Nb);

  Block := FBlocks;
  Inc(Block, Nb);

  Result := Block^;
  Inc(PInt8U(Result), (FSize and FBlockMask) * FEntrySize);
end;


{ TAggPodAllocator }

constructor TAggPodAllocator.Create;
begin
  FBlockSize := FBlockSize;
  FBlockPtrInc := BlockPointerInc;

  FNumBlocks := 0;
  FMaxBlocks := 0;

  FBlocks := nil;
  FBufPtr := nil;
  FRest := 0;
end;

destructor TAggPodAllocator.Destroy;
begin
  RemoveAll;
  inherited;
end;

procedure TAggPodAllocator.RemoveAll;
var
  Blk: PAggPodAlloc;
begin
  if FNumBlocks <> 0 then
  begin
    Blk := PAggPodAlloc(PtrComp(FBlocks) + (FNumBlocks - 1) *
      SizeOf(TAggPodAlloc));

    while FNumBlocks <> 0 do
    begin
      AggFreeMem(Pointer(Blk.Ptr), Blk.Size);

      Dec(PtrComp(Blk), SizeOf(TAggPodAlloc));
      Dec(FNumBlocks);
    end;

    AggFreeMem(Pointer(FBlocks), FMaxBlocks * SizeOf(TAggPodAlloc));
  end;

  FNumBlocks := 0;
  FMaxBlocks := 0;

  FBlocks := nil;
  FBufPtr := nil;
  FRest := 0;
end;

function TAggPodAllocator.Allocate;
var
  Ptr  : PInt8u;
  Align: Cardinal;
begin
  if Size = 0 then
  begin
    Result := 0;

    Exit;
  end;

  if Size <= FRest then
  begin
    Ptr := FBufPtr;

    if Alignment > 1 then
    begin
      Align := (Alignment - Cardinal(Int32u(Ptr)) mod Alignment) mod Alignment;

      Inc(Size, Align);
      Inc(PtrComp(Ptr), Align);

      if Size <= FRest then
      begin
        Dec(FRest, Size);
        Inc(PtrComp(FBufPtr), Size);

        Result := Ptr;

        Exit;
      end;

      AllocateBlock(Size);

      Result := Allocate(Size - Align, Alignment);

      Exit;
    end;

    Dec(FRest, Size);
    Inc(PtrComp(FBufPtr), Size);

    Result := Ptr;

    Exit;
  end;

  AllocateBlock(Size + Alignment - 1);

  Result := Allocate(Size, Alignment);
end;

procedure TAggPodAllocator.AllocateBlock(Size: Cardinal);
var
  NewBlocks: PAggPodAlloc;
begin
  if Size < FBlockSize then
    Size := FBlockSize;

  if FNumBlocks >= FMaxBlocks then
  begin
    AggGetMem(Pointer(NewBlocks), (FMaxBlocks + FBlockPtrInc) *
      SizeOf(TAggPodAlloc));

    if FBlocks <> nil then
    begin
      Move(FBlocks^, NewBlocks^, FNumBlocks * SizeOf(TAggPodAlloc));

      AggFreeMem(Pointer(FBlocks), FMaxBlocks * SizeOf(TAggPodAlloc));
    end;

    FBlocks := NewBlocks;

    Inc(FMaxBlocks, FBlockPtrInc);
  end;

  AggGetMem(Pointer(FBufPtr), Size * SizeOf(Int8u));

  PAggPodAlloc(PtrComp(FBlocks) + FNumBlocks * SizeOf(TAggPodAlloc)).Ptr := FBufPtr;
  PAggPodAlloc(PtrComp(FBlocks) + FNumBlocks * SizeOf(TAggPodAlloc)).Size := Size;

  Inc(FNumBlocks);

  FRest := Size;
end;

end.
