unit AggScanlineUnpacked;

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
  AggScanLine,
  AggAlphaMaskUnpacked8;

type
  // Unpacked ScanLine container class
  //
  // This class is used to transfer data from a ScanLine Rasterizer
  // to the rendering buffer. It's organized very simple. The class stores
  // information of horizontal Spans to render it into a pixel-map buffer.
  // Each Span has staring X, length, and an array of bytes that determine the
  // cover-values for each pixel.
  // Before using this class you should know the minimal and maximal pixel
  // coordinates of your ScanLine. The protocol of using is:
  // 1. reset(MinX, MaxX)
  // 2. AddCell() / AddSpan() - accumulate ScanLine.
  // When forming one ScanLine the next X coordinate must be always greater
  // than the last stored one, i.e. it works only with ordered coordinates.
  // 3. Call finalize(y) and render the ScanLine.
  // 3. Call ResetSpans() to prepare for the new ScanLine.
  //
  // 4. Rendering:
  //
  // ScanLine provides an iterator class that allows you to extract
  // the Spans and the cover values for each pixel. Be aware that clipping
  // has not been done yet, so you should perform it yourself.
  // Use ScanLineU8::iterator to render Spans:
  // -------------------------------------------------------------------------
  //
  // int y = sl.y();                    // Y-coordinate of the ScanLine
  //
  // ************************************
  // ...Perform vertical clipping here...
  // ************************************
  //
  // ScanLineU8::const_iterator Span = sl.begin();
  //
  // Cardinal char* row = m_rbuf->row(y); // The the address of the beginning
  // // of the current row
  //
  // Cardinal NumSpans = sl.NumSpans(); // Number of Spans. It's guaranteed that
  // // NumSpans is always greater than 0.
  //
  // do
  // {
  // const ScanLineU8::TCover* covers =
  // Span->covers;                     // The array of the cover values
  //
  // int num_pix = Span->len;              // Number of pixels of the Span.
  // // Always greater than 0, still it's
  // // better to use "int" instead of
  // // "Cardinal" because it's more
  // // convenient for clipping
  // int x = Span->x;
  //
  // **************************************
  // ...Perform horizontal clipping here...
  // ...you have x, covers, and pix_count..
  // **************************************
  //
  // Cardinal char* dst = row + x;  // Calculate the start address of the row.
  // // In this case we assume a simple
  // // grayscale image 1-byte per pixel.
  // do
  // {
  // *dst++ = *covers++;        // Hypotetical rendering.
  // }
  // while(--num_pix);
  //
  // ++Span;
  // }
  // while(--NumSpans);  // NumSpans cannot be 0, so this loop is quite safe
  // ------------------------------------------------------------------------
  //
  // The question is: why should we accumulate the whole ScanLine when we
  // could render just separate Spans when they're ready?
  // That's because using the ScanLine is generally faster. When is consists
  // of more than one Span the conditions for the processor cash system
  // are better, because switching between two different areas of memory
  // (that can be very large) occurs less frequently.
  // ------------------------------------------------------------------------

  PAggSpanUnpacked8 = ^TAggSpanUnpacked8;
  TAggSpanUnpacked8 = record
    X, Len: Int16;
    Covers: PInt8u;
  end;

  TAggScanLineUnpacked8 = class(TAggCustomScanLine)
  private
    FMinX: Integer;
    FMaxLength: Cardinal;
    FLastX, FY: Integer;

    FCovers: PInt8u;
    FSpans, FCurrentSpan: PAggSpanUnpacked8;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;

    function GetSizeOfSpan: Cardinal; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Reset(MinX, MaxX: Integer); override;
    procedure ResetSpans; override;
    function GetBegin: Pointer; override;

    procedure Finalize(Y: Integer); override;
    procedure AddCell(X: Integer; Cover: Cardinal); override;
    procedure AddCells(X: Integer; Len: Cardinal; Covers: PInt8u); override;
    procedure AddSpan(X: Integer; Len, Cover: Cardinal); override;
  end;

  TAggScanLineUnpacked8AlphaMask = class(TAggScanLineUnpacked8)
  private
    FAlphaMask: TAggCustomAlphaMask;
  public
    constructor Create; overload; override;
    constructor Create(AlphaMask: TAggCustomAlphaMask); overload;

    procedure Finalize(Y: Integer); override;
  end;

implementation


{ TAggScanLineUnpacked8 }

constructor TAggScanLineUnpacked8.Create;
begin
  FMinX := 0;
  FMaxLength := 0;
  FLastX := $7FFFFFF0;

  FCovers := nil;
  FSpans := nil;
  FCurrentSpan := nil;
  inherited;
end;

destructor TAggScanLineUnpacked8.Destroy;
begin
  AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanUnpacked8));
  AggFreeMem(Pointer(FCovers), FMaxLength * SizeOf(Int8u));
  inherited;
end;

procedure TAggScanLineUnpacked8.Reset(MinX, MaxX: Integer);
var
  MaxLength: Cardinal;
begin
  MaxLength := MaxX - MinX + 2;

  if MaxLength > FMaxLength then
  begin
    AggFreeMem(Pointer(FSpans), FMaxLength * SizeOf(TAggSpanUnpacked8));
    AggFreeMem(Pointer(FCovers), FMaxLength * SizeOf(Int8u));

    AggGetMem(Pointer(FCovers), MaxLength * SizeOf(Int8u));
    AggGetMem(Pointer(FSpans), MaxLength * SizeOf(TAggSpanUnpacked8));

    FMaxLength := MaxLength;
  end;

  FLastX := $7FFFFFF0;
  FMinX := MinX;
  FCurrentSpan := FSpans;
end;

procedure TAggScanLineUnpacked8.ResetSpans;
begin
  FLastX := $7FFFFFF0;
  FCurrentSpan := FSpans;
end;

procedure TAggScanLineUnpacked8.Finalize(Y: Integer);
begin
  FY := Y;
end;

procedure TAggScanLineUnpacked8.AddCell(X: Integer; Cover: Cardinal);
begin
  Dec(X, FMinX);

  PInt8u(PtrComp(FCovers) + X * SizeOf(Int8u))^ := Int8u(Cover);

  if X = FLastX + 1 then
    Inc(FCurrentSpan.Len)
  else
  begin
    Inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanUnpacked8));

    FCurrentSpan.X := Int16(X + FMinX);
    FCurrentSpan.Len := 1;

    FCurrentSpan.Covers := PInt8u(PtrComp(FCovers) + X * SizeOf(Int8u));
  end;

  FLastX := X;
end;

procedure TAggScanLineUnpacked8.AddCells(X: Integer; Len: Cardinal;
  Covers: PInt8u);
begin
  Dec(X, FMinX);
  Move(Covers^, PInt8u(PtrComp(FCovers) + X)^, Len * SizeOf(Int8u));

  if X = FLastX + 1 then
    Inc(FCurrentSpan.Len, Int16(Len))
  else
  begin
    Inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanUnpacked8));

    FCurrentSpan.X := Int16(X + FMinX);
    FCurrentSpan.Len := Int16(Len);
    FCurrentSpan.Covers := PInt8u(PtrComp(FCovers) + X * SizeOf(Int8u));
  end;

  FLastX := X + Len - 1;
end;

procedure TAggScanLineUnpacked8.AddSpan(X: Integer; Len, Cover: Cardinal);
begin
  Dec(X, FMinX);

  FillChar(PInt8u(PtrComp(FCovers) + X * SizeOf(Int8u))^, Len, Cover);

  if X = FLastX + 1 then
    Inc(FCurrentSpan.Len, Int16(Len))
  else
  begin
    Inc(PtrComp(FCurrentSpan), SizeOf(TAggSpanUnpacked8));

    FCurrentSpan.X := Int16(X + FMinX);
    FCurrentSpan.Len := Int16(Len);
    FCurrentSpan.Covers := PInt8u(PtrComp(FCovers) + X * SizeOf(Int8u));
  end;

  FLastX := X + Len - 1;
end;

function TAggScanLineUnpacked8.GetY: Integer;
begin
  Result := FY;
end;

function TAggScanLineUnpacked8.GetNumSpans: Cardinal;
begin
  Result := (PtrComp(FCurrentSpan) - PtrComp(FSpans)) div SizeOf(TAggSpanUnpacked8);
end;

function TAggScanLineUnpacked8.GetBegin: Pointer;
begin
  Result := PAggSpanUnpacked8(PtrComp(FSpans) + SizeOf(TAggSpanUnpacked8));
end;

function TAggScanLineUnpacked8.GetSizeOfSpan;
begin
  Result := SizeOf(TAggSpanUnpacked8);
end;


{ TAggScanLineUnpacked8AlphaMask }

constructor TAggScanLineUnpacked8AlphaMask.Create;
begin
  inherited Create;

  FAlphaMask := nil;
end;

constructor TAggScanLineUnpacked8AlphaMask.Create(AlphaMask: TAggCustomAlphaMask);
begin
  inherited Create;

  FAlphaMask := AlphaMask;
end;

procedure TAggScanLineUnpacked8AlphaMask.Finalize(Y: Integer);
var
  Span: PAggSpanUnpacked8;
  Ss, Count: Cardinal;
begin
  inherited Finalize(Y);

  if FAlphaMask <> nil then
  begin
    Span := GetBegin;
    Ss := SizeOfSpan;
    Count := NumSpans;

    repeat
      FAlphaMask.CombineHSpan(Span.X, Y, Span.Covers, Span.Len);

      Inc(PtrComp(Span), Ss);
      Dec(Count);

    until Count = 0;
  end;
end;

end.
