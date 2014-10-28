unit AggScanLinePacked;

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
  AggBasics,
  AggScanLine;

type
  PAggSpanPacked8 = ^TAggSpanPacked8;
  TAggSpanPacked8 = record
    X, Len: Int16; // If negative, it's a solid Span, covers is valid

    Covers: PInt8u;
  end;

  TAggScanLinePacked8 = class(TAggCustomScanLine)
  private
    FMaxLength: Cardinal;
    FLastX, FY: Integer;
    FCovers, FCoverPtr: PInt8u;
    FSpans, FCurrentSpan: PAggSpanPacked8;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
    function GetSizeOfSpan: Cardinal; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;
    procedure ResetSpans; override;

    procedure Finalize(Y: Integer); override;
    procedure AddCell(X: Integer; Cover: Cardinal); override;
    procedure AddCells(X: Integer; Len: Cardinal; Covers: PInt8u); override;
    procedure AddSpan(X: Integer; Len, Cover: Cardinal); override;

    function GetBegin: Pointer; override;
  end;

implementation


{ TAggScanLinePacked8 }

constructor TAggScanLinePacked8.Create;
begin
  FMaxLength := 0;
  FLastX := $7FFFFFF0;

  FY := 0;

  FCovers := nil;
  FCoverPtr := nil;

  FSpans := nil;
  FCurrentSpan := nil;
end;

destructor TAggScanLinePacked8.Destroy;
begin
  AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanPacked8));
  AggFreeMem(Pointer(FCovers), FMaxLength * SizeOf(Int8u));
  inherited;
end;

procedure TAggScanLinePacked8.Reset(MinX, MaxX: Integer);
var
  MaxLen: Cardinal;
begin
  MaxLen := MaxX - MinX + 3;

  if MaxLen > FMaxLength then
  begin
    AggFreeMem(Pointer(FCovers), FMaxLength);
    AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanPacked8));

    AggGetMem(Pointer(FCovers), MaxLen * SizeOf(Int8u));
    AggGetMem(Pointer(FSpans), MaxLen * SizeOf(TAggSpanPacked8));

    FMaxLength := MaxLen;
  end;

  FLastX := $7FFFFFF0;

  FCoverPtr := FCovers;
  FCurrentSpan := FSpans;

  FCurrentSpan.Len := 0;
end;

procedure TAggScanLinePacked8.ResetSpans;
begin
  FLastX := $7FFFFFF0;

  FCoverPtr := FCovers;
  FCurrentSpan := FSpans;

  FCurrentSpan.Len := 0;
end;

procedure TAggScanLinePacked8.Finalize(Y: Integer);
begin
  FY := Y;
end;

procedure TAggScanLinePacked8.AddCell(X: Integer; Cover: Cardinal);
begin
  FCoverPtr^ := Int8u(Cover);

  if (X = FLastX + 1) and (FCurrentSpan.Len > 0) then
    Inc(FCurrentSpan.Len)
  else
  begin
    Inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanPacked8));

    FCurrentSpan.Covers := FCoverPtr;

    FCurrentSpan.X := Int16(X);
    FCurrentSpan.Len := 1;
  end;

  FLastX := X;

  Inc(PtrComp(FCoverPtr), SizeOf(Int8u));
end;

procedure TAggScanLinePacked8.AddCells(X: Integer; Len: Cardinal; Covers: PInt8u);
begin
  Move(Covers^, FCoverPtr^, Len * SizeOf(Int8u));

  if (X = FLastX + 1) and (FCurrentSpan.Len > 0) then
    Inc(FCurrentSpan.Len, Int16(Len))
  else
  begin
    Inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanPacked8));

    FCurrentSpan.Covers := FCoverPtr;
    FCurrentSpan.X := Int16(X);
    FCurrentSpan.Len := Int16(Len);
  end;

  Inc(PtrComp(FCoverPtr), Len * SizeOf(Int8u));

  FLastX := X + Len - 1;
end;

procedure TAggScanLinePacked8.AddSpan(X: Integer; Len, Cover: Cardinal);
begin
  if (X = FLastX + 1) and (FCurrentSpan.Len < 0) and (Cover = FCurrentSpan.Covers^)
  then
    Dec(FCurrentSpan.Len, Int16(Len))
  else
  begin
    FCoverPtr^ := Int8u(Cover);

    Inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanPacked8));

    FCurrentSpan.Covers := FCoverPtr;
    FCurrentSpan.X := Int16(X);
    FCurrentSpan.Len := Int16(Len);
    FCurrentSpan.Len := -FCurrentSpan.Len;

    Inc(PtrComp(FCoverPtr), SizeOf(Int8u));
  end;

  FLastX := X + Len - 1;
end;

function TAggScanLinePacked8.GetY;
begin
  Result := FY;
end;

function TAggScanLinePacked8.GetNumSpans: Cardinal;
begin
  Result := (PtrComp(FCurrentSpan) - PtrComp(FSpans)) div SizeOf(TAggSpanPacked8);
end;

function TAggScanLinePacked8.GetBegin: Pointer;
begin
  Result := PAggSpanPacked8(PtrComp(FSpans) + SizeOf(TAggSpanPacked8));
end;

function TAggScanLinePacked8.GetSizeOfSpan: Cardinal;
begin
  Result := SizeOf(TAggSpanPacked8);
end;

end.
