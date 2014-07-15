unit AggScanLineBin;

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
  PAggSpanBin = ^TAggSpanBin;
  TAggSpanBin = record
    X, Len: Int16;
  end;

  // This is binary scaline container which supports the interface
  // used in the Rasterizer::render(). See description of AggScanlineUnpacked8
  // for details.

  TAggScanLineBin = class(TAggCustomScanLine)
  private
    FMaxLength: Cardinal;
    FLastX, FY: Integer;

    FSpans, FCurrentSpan: PAggSpanBin;
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
    procedure AddSpan(X: Integer; Len, Cover: Cardinal); override;

    function GetBegin: Pointer; override;
  end;

implementation


{ TAggScanLineBin }

constructor TAggScanLineBin.Create;
begin
  FMaxLength := 0;
  FLastX := $7FFFFFF0;

  FSpans := nil;
  FCurrentSpan := nil;
end;

destructor TAggScanLineBin.Destroy;
begin
  AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanBin));
  inherited;
end;

procedure TAggScanLineBin.Reset(MinX, MaxX: Integer);
var
  MaxLength: Cardinal;
begin
  MaxLength := MaxX - MinX + 3;

  if MaxLength > FMaxLength then
  begin
    AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanBin));
    AggGetMem(Pointer(FSpans), MaxLength * SizeOf(TAggSpanBin));

    FMaxLength := MaxLength;
  end;

  FLastX := $7FFFFFF0;
  FCurrentSpan := FSpans;
end;

procedure TAggScanLineBin.ResetSpans;
begin
  FLastX := $7FFFFFF0;
  FCurrentSpan := FSpans;
end;

procedure TAggScanLineBin.Finalize;
begin
  FY := Y;
end;

procedure TAggScanLineBin.AddCell(X: Integer; Cover: Cardinal);
begin
  if X = FLastX + 1 then
    Inc(FCurrentSpan.Len)
  else
  begin
    Inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanBin));

    FCurrentSpan.X := Int16(X);
    FCurrentSpan.Len := 1;
  end;

  FLastX := X;
end;

procedure TAggScanLineBin.AddSpan(X: Integer; Len, Cover: Cardinal);
begin
  if X = FLastX + 1 then
    FCurrentSpan.Len := Int16(FCurrentSpan.Len + Len)
  else
  begin
    Inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanBin));

    FCurrentSpan.X := Int16(X);
    FCurrentSpan.Len := Int16(Len);
  end;

  FLastX := X + Len - 1;
end;

function TAggScanLineBin.GetY: Integer;
begin
  Result := FY
end;

function TAggScanLineBin.GetNumSpans: Cardinal;
begin
  Result := (PtrComp(FCurrentSpan) - PtrComp(FSpans)) div SizeOf(TAggSpanBin);
end;

function TAggScanLineBin.GetBegin: Pointer;
begin
  Result := PAggSpanBin(PtrComp(FSpans) + SizeOf(TAggSpanBin));
end;

function TAggScanLineBin.GetSizeOfSpan: Cardinal;
begin
  Result := SizeOf(TAggSpanBin);
end;

end.
