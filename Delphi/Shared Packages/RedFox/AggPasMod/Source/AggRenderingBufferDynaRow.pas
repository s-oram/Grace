unit AggRenderingBufferDynaRow;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012-2015                                                      //
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
  AggBasics,
  AggRenderingBuffer;

type
  // Rendering buffer class with dynamic allocation of the rows.
  // The rows are allocated as needed when requesting for PSpan().
  // The class automatically calculates min_x and max_x for each row.
  // Generally it's more efficient to use this class as a temporary buffer
  // for rendering a few lines and then to blend it with another buffer.
  TAggRenderingBufferDynaRow = class(TAggRenderingBuffer)
  private
    FBuffer: PAggRowDataType; // Pointers to each row of the buffer
    FAlloc, FByteWidth: Cardinal; // Width in bytes
  public
    constructor Create(Width, Height, ByteWidth: Cardinal);
    destructor Destroy; override;

    procedure Init(Width, Height, ByteWidth: Cardinal);

    function GetByteWidth: Cardinal;

    function RowXY(X, Y: Integer; Len: Cardinal): PInt8u; override;
    function Row(Y: Cardinal): PInt8u; override;
  end;

implementation


{ TAggRenderingBufferDynaRow }

// Allocate and clear the buffer
constructor TAggRenderingBufferDynaRow.Create(Width, Height,
  ByteWidth: Cardinal);
begin
  FAlloc := SizeOf(TAggRowDataType) * Height;

  AggGetMem(Pointer(FBuffer), FAlloc);

  FWidth := Width;
  FHeight := Height;

  FByteWidth := ByteWidth;

  FillChar(FBuffer^, FAlloc, 0);
end;

destructor TAggRenderingBufferDynaRow.Destroy;
begin
  Init(0, 0, 0);
  inherited;
end;

// Allocate and clear the buffer
procedure TAggRenderingBufferDynaRow.Init(Width, Height, ByteWidth: Cardinal);
var
  I: Cardinal;
begin
  I := 0;

  while I < FHeight do
  begin
    AggFreeMem(Pointer(PAggRowDataType(PtrComp(FBuffer) + I *
      SizeOf(TAggRowDataType)).Ptr), FByteWidth);

    Inc(I);
  end;

  AggFreeMem(Pointer(FBuffer), FAlloc);

  FBuffer := nil;

  if (Width <> 0) and (Height <> 0) then
  begin
    FWidth := Width;
    FHeight := Height;

    FByteWidth := ByteWidth;

    FAlloc := SizeOf(TAggRowDataType) * Height;

    AggGetMem(Pointer(FBuffer), FAlloc);
    FillChar(FBuffer^, FAlloc, 0);
  end;
end;

function TAggRenderingBufferDynaRow.GetByteWidth: Cardinal;
begin
  Result := FByteWidth;
end;

// The main function used for rendering. Returns pointer to the
// pre-allocated Span. Memory for the row is allocated as needed.
function TAggRenderingBufferDynaRow.RowXY(X, Y: Integer; Len: Cardinal): PInt8u;
var
  R: PAggRowDataType;
  P: PInt8u;

  X2: Integer;

begin
  R := PAggRowDataType(PtrComp(FBuffer) + Y * SizeOf(TAggRowDataType));
  X2 := X + Len - 1;

  if R.Ptr <> nil then
  begin
    if X < R.X1 then
      R.X1 := X;

    if X2 > R.X2 then
      R.X2 := X2;

  end
  else
  begin
    AggGetMem(Pointer(P), FByteWidth);

    R.Ptr := P;
    R.X1 := X;
    R.X2 := X2;

    FillChar(P^, FByteWidth, 0);
  end;

  Result := R.Ptr;
end;

function TAggRenderingBufferDynaRow.Row(Y: Cardinal): PInt8u;
begin
  Result := RowXY(0, Y, FWidth);
end;

end.
