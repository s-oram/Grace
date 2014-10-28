unit AggRenderingBuffer;

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
  PAggRowDataType = ^TAggRowDataType;
  TAggRowDataType = record
    X1, X2: Integer;
    Ptr: PInt8u;
  public
    procedure Initialize(X1, X2: Integer; Ptr: PInt8u); overload;
  end;

  TAggSpanData = record
    X: Integer;
    Len: Byte;
    Ptr: PInt8u;
  public
    procedure Initialize(X: Integer; Len: Byte; Ptr: PInt8u); overload;
  end;

  TAggRenderingBuffer = class
  private
    FBuffer: PInt8u; // Pointer to rendering buffer
    FRows: PPInt8u; // Pointers to each row of the buffer
    FStride: Integer; // Number of bytes per row. Can be < 0
    FMaxHeight: Cardinal; // The maximal height (currently allocated)
    function GetPixelPointer(X, Y: Cardinal): PInt8u;
  protected
    FWidth: Cardinal;  // Width in pixels
    FHeight: Cardinal; // Height in pixels
    function GetStrideAbs: Cardinal;
  public
    constructor Create; overload;
    constructor Create(ABuffer: PInt8u; AWidth, AHeight: Cardinal;
      AStride: Integer); overload;
    destructor Destroy; override;

    procedure Attach(ABuffer: PInt8u; AWidth, AHeight: Cardinal;
      AStride: Integer);

    function RowXY(X, Y: Integer; Len: Cardinal): PInt8u; virtual;
    function Row(Y: Cardinal): PInt8u; virtual;
    function NextRow(P: PInt8u): PInt8u; virtual;
    function Rows: PInt8u;

    procedure CopyFrom(RenderingBuffer: TAggRenderingBuffer);
    procedure Clear(Value: Int8u);

    property Buffer: PInt8u read FBuffer;
    property Height: Cardinal read FHeight;
    property Width: Cardinal read FWidth;
    property Stride: Integer read FStride;
    property StrideAbs: Cardinal read GetStrideAbs;

    property ScanLine[Index: Cardinal]: PInt8u read Row;
    property PixelPointer[X, Y: Cardinal]: PInt8u read GetPixelPointer;
  end;

implementation


{ TAggRowDataType }

procedure TAggRowDataType.Initialize(X1, X2: Integer; Ptr: PInt8u);
begin
  Self.X1 := X1;
  Self.X2 := X2;
  Self.Ptr := Ptr;
end;


{ TAggSpanData }

procedure TAggSpanData.Initialize(X: Integer; Len: Byte; Ptr: PInt8u);
begin
  Self.X := X;
  Self.Len := Len;
  Self.Ptr := Ptr;
end;


{ TAggRenderingBuffer }

constructor TAggRenderingBuffer.Create;
begin
  FBuffer := nil;
  FRows := nil;
  FWidth := 0;
  FHeight := 0;
  FStride := 0;

  FMaxHeight := 0;
  inherited;
end;

constructor TAggRenderingBuffer.Create(ABuffer: PInt8u;
  AWidth, AHeight: Cardinal; AStride: Integer);
begin
  Create;
  Attach(ABuffer, AWidth, AHeight, AStride);
end;

destructor TAggRenderingBuffer.Destroy;
begin
  AggFreeMem(Pointer(FRows), FMaxHeight * SizeOf(PInt8u));
  inherited;
end;

procedure TAggRenderingBuffer.Attach(ABuffer: PInt8u; AWidth, AHeight: Cardinal;
  AStride: Integer);
var
  RowsPointer: PPInt8u;
  RowPointer: PInt8u;
begin
  FBuffer := ABuffer;
  FWidth := AWidth;
  FHeight := AHeight;
  FStride := AStride;

  if AHeight > FMaxHeight then
  begin
    AggFreeMem(Pointer(FRows), FMaxHeight * SizeOf(PInt8u));
    AggGetMem(Pointer(FRows), AHeight * SizeOf(PInt8u));

    FMaxHeight := AHeight;
  end;

  if AStride < 0 then
    if AHeight > 0 then
    begin
      RowPointer := FBuffer;
      Dec(RowPointer, (AHeight - 1) * AStride);
    end
    else
      RowPointer := nil
  else
    RowPointer := FBuffer;

  RowsPointer := Pointer(FRows);

  while AHeight > 0 do
  begin
    RowsPointer^ := RowPointer;

    Inc(PtrComp(RowPointer), AStride);
    Inc(PtrComp(RowsPointer), SizeOf(PInt8u));

    Dec(AHeight);
  end;
end;

function TAggRenderingBuffer.GetPixelPointer(X, Y: Cardinal): PInt8u;
begin
  Result := RowXY(X, Y, Abs(FStride) div FWidth);
end;

function TAggRenderingBuffer.GetStrideAbs;
begin
  if FStride < 0 then
    Result := -FStride
  else
    Result := FStride;
end;

function TAggRenderingBuffer.RowXY(X, Y: Integer; Len: Cardinal): PInt8u;
var
  RowPointer: PPInt8u;
begin
  RowPointer := FRows;
  Inc(RowPointer, Y);
  Result := RowPointer^;
end;

function TAggRenderingBuffer.Row(Y: Cardinal): PInt8u;
var
  RowPointer: PPInt8u;
begin
  RowPointer := FRows;
  Inc(RowPointer, Y);
  Result := RowPointer^;
end;

function TAggRenderingBuffer.NextRow(P: PInt8u): PInt8u;
begin
  Result := P;
  Inc(Result, FStride);
end;

function TAggRenderingBuffer.Rows;
begin
  Result := Pointer(FRows);
end;

procedure TAggRenderingBuffer.CopyFrom(RenderingBuffer: TAggRenderingBuffer);
var
  H, L, Y: Cardinal;
begin
  H := Height;

  if RenderingBuffer.Height < H then
    H := RenderingBuffer.Height;

  L := StrideAbs;

  if RenderingBuffer.StrideAbs < L then
    L := RenderingBuffer.StrideAbs;

  L := L * SizeOf(Int8u);

  if H > 0 then
    for Y := 0 to H - 1 do
      Move(RenderingBuffer.Row(Y)^, Row(Y)^, L);
end;

procedure TAggRenderingBuffer.Clear(Value: Int8u);
var
  Y, X: Cardinal;
  P   : PInt8u;
begin
  if Height > 0 then
    for Y := 0 to Height - 1 do
    begin
      P := Row(Y);

      if StrideAbs > 0 then
        for X := 0 to StrideAbs - 1 do
        begin
          P^ := Value;

          Inc(PtrComp(P), SizeOf(Int8u));
        end;
    end;
end;

end.
