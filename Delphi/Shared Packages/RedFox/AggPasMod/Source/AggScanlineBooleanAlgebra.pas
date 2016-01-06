unit AggScanLineBooleanAlgebra;

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
{$Q-}
{$R-}

uses
  AggBasics,
  AggRasterizerScanLine,
  AggScanLine,
  AggRendererScanLine;

type
  TAggBoolScanLineOp = (bsoOr, bsoAnd, bsoXor, bsoXorSaddle, bsoXorAbsDiff,
    bsoAMinusB, bsoBMinusA);

  TAggBoolScanLineFunctor = class;

  TAggBoolScanLineFunctor1 = procedure(This: TAggBoolScanLineFunctor;
    Span: PAggSpanRecord; X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
  TAggBoolScanLineFunctor2 = procedure(This: TAggBoolScanLineFunctor; Span1,
    Span2: PAggSpanRecord; X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
  TAggBoolScanLineFormula = function(This: TAggBoolScanLineFunctor;
    A, B: Cardinal): Cardinal;

  TAggBoolScanLineFunctor = class
  private
    FCoverShift, FCoverSize, FCoverMask, FCoverFull: Cardinal;
  public
    Functor1: TAggBoolScanLineFunctor1;
    Functor2: TAggBoolScanLineFunctor2;
    Formula: TAggBoolScanLineFormula;

    constructor Create1(F1: TAggBoolScanLineFunctor1;
      CoverShift: Cardinal = AggBasics.CAggCoverShift);
    constructor Create2(F2: TAggBoolScanLineFunctor2;
      CoverShift: Cardinal = AggBasics.CAggCoverShift);
  end;

procedure BoolScanLineSubtractShapesAA(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineXORShapesAbsDiffAA(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineXORShapesSaddleAA(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineXORShapesAA(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineIntersectShapesAA(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineUniteShapesAA(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineCombineShapesAA(Op: TAggBoolScanLineOp;
  Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, Sl: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine);

procedure BoolScanLineSubtractShapesBin(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineXORShapesBin(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineIntersectShapesBin(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineUniteShapesBin(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);

procedure BoolScanLineCombineShapesBin(Op: TAggBoolScanLineOp;
  Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, Sl: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine);

implementation


{ TAggBoolScanLineFunctor }

constructor TAggBoolScanLineFunctor.Create1(F1: TAggBoolScanLineFunctor1;
  CoverShift: Cardinal = AggBasics.CAggCoverShift);
begin
  FCoverShift := CoverShift;
  FCoverSize := 1 shl FCoverShift;
  FCoverMask := FCoverSize - 1;
  FCoverFull := FCoverMask;

  Functor1 := F1;
  Functor2 := nil;
  Formula := nil;
end;

constructor TAggBoolScanLineFunctor.Create2(F2: TAggBoolScanLineFunctor2;
  CoverShift: Cardinal = AggBasics.CAggCoverShift);
begin
  FCoverShift := CoverShift;
  FCoverSize := 1 shl FCoverShift;
  FCoverMask := FCoverSize - 1;
  FCoverFull := FCoverMask;

  Functor1 := nil;
  Functor2 := F2;
  Formula := nil;
end;

// Functor.
// Add nothing. Used in conbine_shapes_sub
procedure BoolScanLineAddSpanEmpty(This: TAggBoolScanLineFunctor; Span: PAggSpanRecord;
  X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
begin
end;

// Functor.
// Combine two Spans as empty ones. The functor does nothing
// and is used to XOR binary Spans.
procedure BoolScanLineCombineSpansEmpty(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
begin
end;

// Functor.
// Add an anti-aliased Span
// anti-aliasing information, but only X and Length. The function
// is compatible with any type of ScanLines.
procedure BoolScanLineAddSpanAA(This: TAggBoolScanLineFunctor; Span: PAggSpanRecord;
  X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
var
  Covers: PInt8u;
begin
  if Span.Len < 0 then
    Sl.AddSpan(X, Len, Span.Covers^)
  else if Span.Len > 0 then
  begin
    Covers := Span.Covers;

    if Span.X < X then
      Inc(PtrComp(Covers), X - Span.X);

    Sl.AddCells(X, Len, Covers);
  end;
end;

// Functor.
// Unite two Spans preserving the anti-aliasing information.
// The result is added to the "sl" ScanLine.
procedure BoolScanLineUniteSpansAA(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
var
  Cover           : Cardinal;
  Covers1, Covers2: PInt8u;
begin
  // Calculate the operation code and choose the
  // proper combination algorithm.
  // 0 = Both Spans are of AA type
  // 1 = Span1 is solid, Span2 is AA
  // 2 = Span1 is AA, Span2 is solid
  // 3 = Both Spans are of solid type
  case Cardinal(Span1.Len < 0) or (Cardinal(Span2.Len < 0) shl 1) of
    0: // Both are AA Spans
      begin
        Covers1 := Span1.Covers;
        Covers2 := Span2.Covers;

        if Span1.X < X then
          Inc(PtrComp(Covers1), (X - Span1.X) * SizeOf(Int8u));

        if Span2.X < X then
          Inc(PtrComp(Covers2), (X - Span2.X) * SizeOf(Int8u));

        repeat
          Cover := This.FCoverMask * This.FCoverMask -
            (This.FCoverMask - Covers1^) * (This.FCoverMask - Covers2^);

          Inc(PtrComp(Covers1), SizeOf(Int8u));
          Inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover = This.FCoverFull * This.FCoverFull then
            Sl.AddCell(X, This.FCoverFull)
          else
            Sl.AddCell(X, Cover shr This.FCoverShift);

          Inc(X);
          Dec(Len);

        until Len = 0;
      end;

    1: // Span1 is solid, Span2 is AA
      begin
        Covers2 := Span2.Covers;

        if Span2.X < X then
          Inc(PtrComp(Covers2), (X - Span2.X) * SizeOf(Int8u));

        if Span1.Covers^ = This.FCoverFull then
          Sl.AddSpan(X, Len, This.FCoverFull)
        else
          repeat
            Cover := This.FCoverMask * This.FCoverMask -
              (This.FCoverMask - Span1.Covers^) * (This.FCoverMask - Covers2^);

            Inc(PtrComp(Covers2), SizeOf(Int8u));

            if Cover = This.FCoverFull * This.FCoverFull then
              Sl.AddCell(X, This.FCoverFull)
            else
              Sl.AddCell(X, Cover shr This.FCoverShift);

            Inc(X);
            Dec(Len);

          until Len = 0;
      end;

    2: // Span1 is AA, Span2 is solid
      begin
        Covers1 := Span1.Covers;

        if Span1.X < X then
          Inc(PtrComp(Covers1), (X - Span1.X) * SizeOf(Int8u));

        if Span2.Covers^ = This.FCoverFull then
          Sl.AddSpan(X, Len, This.FCoverFull)
        else
          repeat
            Cover := This.FCoverMask * This.FCoverMask -
              (This.FCoverMask - Covers1^) * (This.FCoverMask - Span2.Covers^);

            Inc(PtrComp(Covers1), SizeOf(Int8u));

            if Cover = This.FCoverFull * This.FCoverFull then
              Sl.AddCell(X, This.FCoverFull)
            else
              Sl.AddCell(X, Cover shr This.FCoverShift);

            Inc(X);
            Dec(Len);

          until Len = 0;
      end;

    3: // Both are solid Spans
      begin
        Cover := This.FCoverMask * This.FCoverMask -
          (This.FCoverMask - Span1.Covers^) * (This.FCoverMask - Span2.Covers^);

        if Cover = This.FCoverFull * This.FCoverFull then
          Sl.AddSpan(X, Len, This.FCoverFull)
        else
          Sl.AddSpan(X, Len, Cover shr This.FCoverShift);
      end;
  end;
end;

// Functor.
// Combine two binary encoded Spans, i.e., when we don't have any
// anti-aliasing information, but only X and Length. The function
// is compatible with any type of ScanLines.
procedure BoolScanLineCombineSpansBin(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
begin
  Sl.AddSpan(X, Len, This.FCoverFull);
end;

// Functor.
// Add a binary Span
procedure BoolScanLineAddSpanBin(This: TAggBoolScanLineFunctor; Span: PAggSpanRecord;
  X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
begin
  Sl.AddSpan(X, Len, This.FCoverFull);
end;

// Functor.
// Intersect two Spans preserving the anti-aliasing information.
// The result is added to the "sl" ScanLine.
procedure SboolIntersecTAggSpansAA(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
var
  Cover           : Cardinal;
  Covers1, Covers2: PInt8u;
begin
  // Calculate the operation code and choose the
  // proper combination algorithm.
  // 0 = Both Spans are of AA type
  // 1 = Span1 is solid, Span2 is AA
  // 2 = Span1 is AA, Span2 is solid
  // 3 = Both Spans are of solid type
  case Cardinal(Span1.Len < 0) or (Cardinal(Span2.Len < 0) shl 1) of
    0: // Both are AA Spans
      begin
        Covers1 := Span1.Covers;
        Covers2 := Span2.Covers;

        if Span1.X < X then
          Inc(PtrComp(Covers1), (X - Span1.X) * SizeOf(Int8u));

        if Span2.X < X then
          Inc(PtrComp(Covers2), (X - Span2.X) * SizeOf(Int8u));

        repeat
          Cover := Covers1^ * Covers2^;

          Inc(PtrComp(Covers1), SizeOf(Int8u));
          Inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover = This.FCoverFull * This.FCoverFull then
            Sl.AddCell(X, This.FCoverFull)
          else
            Sl.AddCell(X, Cover shr This.FCoverShift);

          Inc(X);
          Dec(Len);

        until Len = 0;
      end;

    1: // Span1 is solid, Span2 is AA
      begin
        Covers2 := Span2.Covers;

        if Span2.X < X then
          Inc(PtrComp(Covers2), (X - Span2.X));

        if Span1.Covers^ = This.FCoverFull then
          Sl.AddCells(X, Len, Covers2)
        else
          repeat
            Cover := Span1.Covers^ * Covers2^;

            Inc(PtrComp(Covers2), SizeOf(Int8u));

            if Cover = This.FCoverFull * This.FCoverFull then
              Sl.AddCell(X, This.FCoverFull)
            else
              Sl.AddCell(X, Cover shr This.FCoverShift);

            Inc(X);
            Dec(Len);

          until Len = 0;
      end;

    2: // Span1 is AA, Span2 is solid
      begin
        Covers1 := Span1.Covers;

        if Span1.X < X then
          Inc(PtrComp(Covers1), (X - Span1.X) * SizeOf(Int8u));

        if Span2.Covers^ = This.FCoverFull then
          Sl.AddCells(X, Len, Covers1)
        else
          repeat
            Cover := Covers1^ * Span2.Covers^;

            Inc(PtrComp(Covers1), SizeOf(Int8u));

            if Cover = This.FCoverFull * This.FCoverFull then
              Sl.AddCell(X, This.FCoverFull)
            else
              Sl.AddCell(X, Cover shr This.FCoverShift);

            Inc(X);
            Dec(Len);

          until Len = 0;
      end;

    3: // Both are solid Spans
      begin
        Cover := Span1.Covers^ * Span2.Covers^;

        if Cover = This.FCoverFull * This.FCoverFull then
          Sl.AddSpan(X, Len, This.FCoverFull)
        else
          Sl.AddSpan(X, Len, Cover shr This.FCoverShift);
      end;
  end;
end;

// Functor.
// XOR two Spans preserving the anti-aliasing information.
// The result is added to the "sl" ScanLine.
procedure BoolScanLineXORSpansAA(This: TAggBoolScanLineFunctor; Span1,
  Span2: PAggSpanRecord; X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
var
  Cover           : Cardinal;
  Covers1, Covers2: PInt8u;
begin
  // Calculate the operation code and choose the
  // proper combination algorithm.
  // 0 = Both Spans are of AA type
  // 1 = Span1 is solid, Span2 is AA
  // 2 = Span1 is AA, Span2 is solid
  // 3 = Both Spans are of solid type
  case Cardinal(Span1.Len < 0) or (Cardinal(Span2.Len < 0) shl 1) of
    0: // Both are AA Spans
      begin
        Covers1 := Span1.Covers;
        Covers2 := Span2.Covers;

        if Span1.X < X then
          Inc(PtrComp(Covers1), (X - Span1.X) * SizeOf(Int8u));

        if Span2.X < X then
          Inc(PtrComp(Covers2), (X - Span2.X) * SizeOf(Int8u));

        repeat
          Cover := This.Formula(This, Covers1^, Covers2^);

          Inc(PtrComp(Covers1), SizeOf(Int8u));
          Inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover <> 0 then
            Sl.AddCell(X, Cover);

          Inc(X);
          Dec(Len);

        until Len = 0;
      end;

    1: // Span1 is solid, Span2 is AA
      begin
        Covers2 := Span2.Covers;

        if Span2.X < X then
          Inc(PtrComp(Covers2), (X - Span2.X) * SizeOf(Int8u));

        repeat
          Cover := This.Formula(This, Span1.Covers^, Covers2^);

          Inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover <> 0 then
            Sl.AddCell(X, Cover);

          Inc(X);
          Dec(Len);

        until Len = 0;
      end;

    2: // Span1 is AA, Span2 is solid
      begin
        Covers1 := Span1.Covers;

        if Span1.X < X then
          Inc(PtrComp(Covers1), (X - Span1.X) * SizeOf(Int8u));

        repeat
          Cover := This.Formula(This, Covers1^, Span2.Covers^);

          Inc(PtrComp(Covers1), SizeOf(Int8u));

          if Cover <> 0 then
            Sl.AddCell(X, Cover);

          Inc(X);
          Dec(Len);

        until Len = 0;
      end;

    3: // Both are solid Spans
      begin
        Cover := This.Formula(This, Span1.Covers^, Span2.Covers^);

        if Cover <> 0 then
          Sl.AddSpan(X, Len, Cover);
      end;
  end;
end;

// Functor.
// Unite two Spans preserving the anti-aliasing information.
// The result is added to the "sl" ScanLine.
procedure BoolScanLineSubtracTAggSpansAA(This: TAggBoolScanLineFunctor;
  Span1, Span2: PAggSpanRecord; X: Integer; Len: Cardinal; Sl: TAggCustomScanLine);
var
  Cover           : Cardinal;
  Covers1, Covers2: PInt8u;
begin
  // Calculate the operation code and choose the
  // proper combination algorithm.
  // 0 = Both Spans are of AA type
  // 1 = Span1 is solid, Span2 is AA
  // 2 = Span1 is AA, Span2 is solid
  // 3 = Both Spans are of solid type
  case Cardinal(Span1.Len < 0) or (Cardinal(Span2.Len < 0) shl 1) of
    0: // Both are AA Spans
      begin
        Covers1 := Span1.Covers;
        Covers2 := Span2.Covers;

        if Span1.X < X then
          Inc(PtrComp(Covers1), (X - Span1.X) * SizeOf(Int8u));

        if Span2.X < X then
          Inc(PtrComp(Covers2), (X - Span2.X) * SizeOf(Int8u));

        repeat
          Cover := Covers1^ * (This.FCoverMask - Covers2^);

          Inc(PtrComp(Covers1), SizeOf(Int8u));
          Inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover <> 0 then
            if Cover = This.FCoverFull * This.FCoverFull then
              Sl.AddCell(X, This.FCoverFull)
            else
              Sl.AddCell(X, Cover shr This.FCoverShift);

          Inc(X);
          Dec(Len);

        until Len = 0;
      end;

    1: // Span1 is solid, Span2 is AA
      begin
        Covers2 := Span2.Covers;

        if Span2.X < X then
          Inc(PtrComp(Covers2), (X - Span2.X) * SizeOf(Int8u));

        repeat
          Cover := Span1.Covers^ * (This.FCoverMask - Covers2^);

          Inc(PtrComp(Covers2), SizeOf(Int8u));

          if Cover <> 0 then
            if Cover = This.FCoverFull * This.FCoverFull then
              Sl.AddCell(X, This.FCoverFull)
            else
              Sl.AddCell(X, Cover shr This.FCoverShift);

          Inc(X);
          Dec(Len);

        until Len = 0;
      end;

    2: // Span1 is AA, Span2 is solid
      begin
        Covers1 := Span1.Covers;

        if Span1.X < X then
          Inc(PtrComp(Covers1), (X - Span1.X) * SizeOf(Int8u));

        if Span2.Covers^ <> This.FCoverFull then
          repeat
            Cover := Covers1^ * (This.FCoverMask - Span2.Covers^);

            Inc(PtrComp(Covers1), SizeOf(Int8u));

            if Cover <> 0 then
              if Cover = This.FCoverFull * This.FCoverFull then
                Sl.AddCell(X, This.FCoverFull)
              else
                Sl.AddCell(X, Cover shr This.FCoverShift);

            Inc(X);
            Dec(Len);

          until Len = 0;
      end;

    3: // Both are solid Spans
      begin
        Cover := Span1.Covers^ * (This.FCoverMask - Span2.Covers^);

        if Cover <> 0 then
          if Cover = This.FCoverFull * This.FCoverFull then
            Sl.AddSpan(X, Len, This.FCoverFull)
          else
            Sl.AddSpan(X, Len, Cover shr This.FCoverShift);
      end;
  end;
end;

function BoolScanLineXORFormulaLinear(This: TAggBoolScanLineFunctor; A, B: Cardinal)
  : Cardinal;
var
  Cover: Cardinal;
begin
  Cover := A + B;

  if Cover > This.FCoverMask then
    Cover := This.FCoverMask + This.FCoverMask - Cover;

  Result := Cover;
end;

function BoolScanLineXORFormulaSaddle(This: TAggBoolScanLineFunctor; A, B: Integer): Cardinal;
var
  K: Cardinal;
begin
  K := A * B;

  if K = This.FCoverMask * This.FCoverMask then
    Result := 0
  else
  begin
    A := (This.FCoverMask * This.FCoverMask - (A shl This.FCoverShift) + K)
      shr This.FCoverShift;
    B := (This.FCoverMask * This.FCoverMask - (B shl This.FCoverShift) + K)
      shr This.FCoverShift;

    Result := This.FCoverMask - ((A * B) shr This.FCoverShift);
  end;
end;

function BoolScanLineXORFormulaAbsDiff(This: TAggBoolScanLineFunctor; A, B: Integer)
  : Cardinal;
begin
  Result := Abs(A - B);
end;

procedure BoolScanLineAddSpansAndRender(Sl1, Sl: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine; AddSpan: TAggBoolScanLineFunctor);
var
  Ss, NumSpans: Cardinal;
  Span: PAggSpanRecord;
begin
  Sl.ResetSpans;

  Ss := Sl1.SizeOfSpan;
  Span := Sl1.GetBegin;
  NumSpans := Sl1.NumSpans;

  repeat
    AddSpan.Functor1(AddSpan, Span, Span.X, Abs(Span.Len), Sl);

    Dec(NumSpans);

    if NumSpans = 0 then
      Break;

    Inc(PtrComp(Span), Ss);
  until False;

  Sl.Finalize(Sl1.Y);
  Ren.Render(Sl);
end;

// Unite two ScanLines, "sl1" and "sl2" and generate a new "sl" one.
// The CombineSpans functor can be of type BoolScanLineCombineSpansBin or
// SboolIntersecTAggSpansAA. First is a general functor to combine
// two Spans without Anti-Aliasing, the second preserves the AA
// information, but works sLower
procedure BoolScanLineUniteScanLines(Sl1, Sl2, Sl: TAggCustomScanLine;
  AddSpan1, AddSpan2, CombineSpans: TAggBoolScanLineFunctor);
const
  CInvalidB = $FFFFFFF;
  CInvalidE = CInvalidB - 1;
var
  Num1, Num2, Ss1, Ss2: Cardinal;
  Xb1, Xb2, Xe1, Xe2, Xb, Xe, Len: Integer;
  Span1, Span2: PAggSpanRecord;
begin
  Sl.ResetSpans;

  Num1 := Sl1.NumSpans;
  Num2 := Sl2.NumSpans;

  // Initialize the Spans as invalid
  Xb1 := CInvalidB;
  Xb2 := CInvalidB;
  Xe1 := CInvalidE;
  Xe2 := CInvalidE;

  // Initialize Span1 if there are Spans
  if Num1 <> 0 then
  begin
    Span1 := Sl1.GetBegin;
    Ss1 := Sl1.SizeOfSpan;
    Xb1 := Span1.X;
    Xe1 := Xb1 + Abs(Span1.Len) - 1;

    Dec(Num1);
  end;

  // Initialize Span2 if there are Spans
  if Num2 <> 0 then
  begin
    Span2 := Sl2.GetBegin;
    Ss2 := Sl2.SizeOfSpan;
    Xb2 := Span2.X;
    Xe2 := Xb2 + Abs(Span2.Len) - 1;

    Dec(Num2);
  end;

  repeat
    // Retrieve a new Span1 if it's invalid
    if (Num1 <> 0) and (Xb1 > Xe1) then
    begin
      Dec(Num1);
      Inc(PtrComp(Span1), Ss1);

      Xb1 := Span1.X;
      Xe1 := Xb1 + Abs(Span1.Len) - 1;
    end;

    // Retrieve a new Span2 if it's invalid
    if (Num2 <> 0) and (Xb2 > Xe2) then
    begin
      Dec(Num2);
      Inc(PtrComp(Span2), Ss2);

      Xb2 := Span2.X;
      Xe2 := Xb2 + Abs(Span2.Len) - 1;
    end;

    if (Xb1 > Xe1) and (Xb2 > Xe2) then
      Break;

    // Calculate the intersection
    Xb := Xb1;
    Xe := Xe1;

    if Xb < Xb2 then
      Xb := Xb2;

    if Xe > Xe2 then
      Xe := Xe2;

    Len := Xe - Xb + 1; // The length of the intersection

    if Len > 0 then
    begin
      // The Spans intersect,
      // add the beginning of the Span
      if Xb1 < Xb2 then
      begin
        AddSpan1.Functor1(AddSpan1, Span1, Xb1, Xb2 - Xb1, Sl);

        Xb1 := Xb2;
      end
      else if Xb2 < Xb1 then
      begin
        AddSpan2.Functor1(AddSpan2, Span2, Xb2, Xb1 - Xb2, Sl);

        Xb2 := Xb1;
      end;

      // Add the combination part of the Spans
      CombineSpans.Functor2(CombineSpans, Span1, Span2, Xb, Len, Sl);

      // Invalidate the fully processed Span or both
      if Xe1 < Xe2 then
      begin
        // Invalidate Span1 and eat
        // the processed part of Span2
        Xb1 := CInvalidB;
        Xe1 := CInvalidE;

        Inc(Xb2, Len);
      end
      else if Xe2 < Xe1 then
      begin
        // Invalidate Span2 and eat
        // the processed part of Span1
        Xb2 := CInvalidB;
        Xe2 := CInvalidE;

        Inc(Xb1, Len);
      end
      else
      begin
        Xb1 := CInvalidB; // Invalidate both
        Xb2 := CInvalidB;
        Xe1 := CInvalidE;
        Xe2 := CInvalidE;
      end;
    end
    else
      // The Spans do not intersect
      if Xb1 < Xb2 then
      begin
        // Advance Span1
        if Xb1 <= Xe1 then
          AddSpan1.Functor1(AddSpan1, Span1, Xb1, Xe1 - Xb1 + 1, Sl);

        Xb1 := CInvalidB; // Invalidate
        Xe1 := CInvalidE;
      end
      else
      begin
        // Advance Span2
        if Xb2 <= Xe2 then
          AddSpan2.Functor1(AddSpan2, Span2, Xb2, Xe2 - Xb2 + 1, Sl);

        Xb2 := CInvalidB; // Invalidate
        Xe2 := CInvalidE;
      end;

  until False;
end;

// Unite the ScanLine shapes. Here the "ScanLine Generator"
// abstraction is used. ScanLineGen1 and ScanLineGen2 are
// the generators, and can be of type TAggRasterizerScanLineAA<>.
// There function requires three ScanLine containers that can be
// of different type.
// "sl1" and "sl2" are used to retrieve ScanLines from the generators,
// "sl" is ised as the resulting ScanLine to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls BoolScanLineUniteScanLines with CombineSpansFunctor
// as the last argument. See BoolScanLineUniteScanLines for details.
procedure BoolScanLineUniteShapes(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine;
  AddSpan1, AddSpan2, CombineSpans: TAggBoolScanLineFunctor);
var
  Flag1, Flag2: Boolean;
  R1, R2, Ur: TRectInteger;
begin
  // Prepare the ScanLine generators.
  // If anyone of them doesn't contain
  // any ScanLines, then return.
  Flag1 := Sg1.RewindScanLines;
  Flag2 := Sg2.RewindScanLines;

  if not Flag1 and not Flag2 then
    Exit;

  // Get the bounding boxes
  R1 := RectInteger(Sg1.MinimumX, Sg1.MinimumY, Sg1.MaximumX, Sg1.MaximumY);
  R2 := RectInteger(Sg2.MinimumX, Sg2.MinimumY, Sg2.MaximumX, Sg2.MaximumY);

  // Calculate the union of the bounding boxes
  Ur := UniteRectangles(R1, R2);

  if not Ur.IsValid then
    Exit;

  Ren.Prepare(Cardinal(Ur.X2 - Ur.X2 + 2));

  // Reset the ScanLines and get two first ones
  Sl.Reset(Ur.X1, Ur.X2);

  if Flag1 then
  begin
    Sl1.Reset(Sg1.MinimumX, Sg1.MaximumX);

    Flag1 := Sg1.SweepScanLine(Sl1);
  end;

  if Flag2 then
  begin
    Sl2.Reset(Sg2.MinimumX, Sg2.MaximumX);

    Flag2 := Sg2.SweepScanLine(Sl2);
  end;

  // The main loop
  // Here we synchronize the ScanLines with
  // the same Y coordinate.
  while Flag1 or Flag2 do
    if Flag1 and Flag2 then
      if Sl1.Y = Sl2.Y then
      begin
        // The Y coordinates are the same.
        // Combine the ScanLines, render if they contain any Spans,
        // and advance both generators to the next ScanLines
        BoolScanLineUniteScanLines(Sl1, Sl2, Sl, AddSpan1, AddSpan2,
          CombineSpans);

        if Sl.NumSpans <> 0 then
        begin
          Sl.Finalize(Sl1.Y);
          Ren.Render(Sl);
        end;

        Flag1 := Sg1.SweepScanLine(Sl1);
        Flag2 := Sg2.SweepScanLine(Sl2);
      end
      else if Sl1.Y < Sl2.Y then
      begin
        BoolScanLineAddSpansAndRender(Sl1, Sl, Ren, AddSpan1);

        Flag1 := Sg1.SweepScanLine(Sl1);
      end
      else
      begin
        BoolScanLineAddSpansAndRender(Sl2, Sl, Ren, AddSpan2);

        Flag2 := Sg2.SweepScanLine(Sl2);
      end
    else
    begin
      if Flag1 then
      begin
        BoolScanLineAddSpansAndRender(Sl1, Sl, Ren, AddSpan1);

        Flag1 := Sg1.SweepScanLine(Sl1);
      end;

      if Flag2 then
      begin
        BoolScanLineAddSpansAndRender(Sl2, Sl, Ren, AddSpan2);

        Flag2 := Sg2.SweepScanLine(Sl2);
      end;
    end;
end;

// Intersect two ScanLines, "sl1" and "sl2" and generate a new "sl" one.
// The CombineSpans functor can be of type BoolScanLineCombineSpansBin or
// SboolIntersecTAggSpansAA. First is a general functor to combine
// two Spans without Anti-Aliasing, the second preserves the AA
// information, but works sLower
procedure BoolScanLineIntersecTAggScanLines(Sl1, Sl2, Sl: TAggCustomScanLine;
  CombineSpans: TAggBoolScanLineFunctor);
var
  Num1, Num2  : Cardinal;
  Span1, Span2: PAggSpanRecord;

  Xb1, Xb2, Xe1, Xe2, Ss1, Ss2: Integer;

  Advance_Span1, Advance_both: Boolean;

begin
  Sl.ResetSpans;

  Num1 := Sl1.NumSpans;

  if Num1 = 0 then
    Exit;

  Num2 := Sl2.NumSpans;

  if Num2 = 0 then
    Exit;

  Span1 := Sl1.GetBegin;
  Ss1 := Sl1.SizeOfSpan;
  Span2 := Sl2.GetBegin;
  Ss2 := Sl2.SizeOfSpan;

  while (Num1 <> 0) and (Num2 <> 0) do
  begin
    Xb1 := Span1.X;
    Xb2 := Span2.X;
    Xe1 := Xb1 + Abs(Span1.Len) - 1;
    Xe2 := Xb2 + Abs(Span2.Len) - 1;

    // Determine what Spans we should advance in the next step
    // The Span with the least ending X should be advanced
    // advance_both is just an optimization when we ending
    // coordinates are the same and we can advance both
    Advance_Span1 := Xe1 < Xe2;
    Advance_both := Xe1 = Xe2;

    // Find the intersection of the Spans
    // and check if they intersect
    if Xb1 < Xb2 then
      Xb1 := Xb2;

    if Xe1 > Xe2 then
      Xe1 := Xe2;

    if Xb1 <= Xe1 then
      CombineSpans.Functor2(CombineSpans, Span1, Span2, Xb1,
        Xe1 - Xb1 + 1, Sl);

    // Advance the Spans
    if Advance_both then
    begin
      Dec(Num1);
      Dec(Num2);

      if Num1 <> 0 then
        Inc(PtrComp(Span1), Ss1);

      if Num2 <> 0 then
        Inc(PtrComp(Span2), Ss2);
    end
    else if Advance_Span1 then
    begin
      Dec(Num1);

      if Num1 <> 0 then
        Inc(PtrComp(Span1), Ss1);
    end
    else
    begin
      Dec(Num2);

      if Num2 <> 0 then
        Inc(PtrComp(Span2), Ss2);
    end;
  end;
end;

// Intersect the ScanLine shapes. Here the "ScanLine Generator"
// abstraction is used. ScanLineGen1 and ScanLineGen2 are
// the generators, and can be of type TAggRasterizerScanLineAA<>.
// There function requires three ScanLine containers that can be of
// different types.
// "sl1" and "sl2" are used to retrieve ScanLines from the generators,
// "sl" is ised as the resulting ScanLine to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls BoolScanLineIntersecTAggScanLines with CombineSpansFunctor
// as the last argument. See BoolScanLineIntersecTAggScanLines for details.
procedure BoolScanLineIntersectShapes(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine;
  CombineSpans: TAggBoolScanLineFunctor);
var
  R1, R2, Ir: TRectInteger;
begin
  // Prepare the ScanLine generators.
  // If anyone of them doesn't contain
  // any ScanLines, then return.
  if not Sg1.RewindScanLines then
    Exit;

  if not Sg2.RewindScanLines then
    Exit;

  // Get the bounding boxes
  R1 := RectInteger(Sg1.MinimumX, Sg1.MinimumY, Sg1.MaximumX, Sg1.MaximumY);
  R2 := RectInteger(Sg2.MinimumX, Sg2.MinimumY, Sg2.MaximumX, Sg2.MaximumY);

  // Calculate the intersection of the bounding
  // boxes and return if they don't intersect.
  Ir := IntersectRectangles(R1, R2);

  if not Ir.IsValid then
    Exit;

  // Reset the ScanLines and get two first ones
  Sl.Reset(Ir.X1, Ir.X2);
  Sl1.Reset(Sg1.MinimumX, Sg1.MaximumX);
  Sl2.Reset(Sg2.MinimumX, Sg2.MaximumX);

  if not Sg1.SweepScanLine(Sl1) then
    Exit;

  if not Sg2.SweepScanLine(Sl2) then
    Exit;

  Ren.Prepare(Cardinal(Ir.X2 - Ir.X1 + 2));

  // The main loop
  // Here we synchronize the ScanLines with
  // the same Y coordinate, ignoring all other ones.
  // Only ScanLines having the same Y-coordinate
  // are to be combined.
  repeat
    while Sl1.Y < Sl2.Y do
      if not Sg1.SweepScanLine(Sl1) then
        Exit;

    while Sl2.Y < Sl1.Y do
      if not Sg2.SweepScanLine(Sl2) then
        Exit;

    if Sl1.Y = Sl2.Y then
    begin
      // The Y coordinates are the same.
      // Combine the ScanLines, render if they contain any Spans,
      // and advance both generators to the next ScanLines
      BoolScanLineIntersecTAggScanLines(Sl1, Sl2, Sl, CombineSpans);

      if Sl.NumSpans <> 0 then
      begin
        Sl.Finalize(Sl1.Y);
        Ren.Render(Sl);
      end;

      if not Sg1.SweepScanLine(Sl1) then
        Exit;

      if not Sg2.SweepScanLine(Sl2) then
        Exit;
    end;
  until False;
end;

// Subtract the ScanLine shapes, "sg1-sg2". Here the "ScanLine Generator"
// abstraction is used. ScanLineGen1 and ScanLineGen2 are
// the generators, and can be of type TAggRasterizerScanLineAA<>.
// There function requires three ScanLine containers that can be of
// different types.
// "sl1" and "sl2" are used to retrieve ScanLines from the generators,
// "sl" is ised as the resulting ScanLine to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
// the function calls BoolScanLineIntersecTAggScanLines with CombineSpansFunctor
// as the last argument. See combine_ScanLines_sub for details.
procedure BoolScanLineSubtractShapes(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine;
  AddSpan1, CombineSpans: TAggBoolScanLineFunctor);
var
  R1: TRectInteger;
  Flag1, Flag2: Boolean;
  AddSpan2: TAggBoolScanLineFunctor;
begin
  // Prepare the ScanLine generators.
  // Here "sg1" is master, "sg2" is slave.
  if not Sg1.RewindScanLines then
    Exit;

  Flag2 := Sg2.RewindScanLines;

  // Get the bounding box
  R1 := RectInteger(Sg1.MinimumX, Sg1.MinimumY, Sg1.MaximumX, Sg1.MaximumY);

  // Reset the ScanLines and get two first ones
  Sl.Reset(Sg1.MinimumX, Sg1.MaximumX);
  Sl1.Reset(Sg1.MinimumX, Sg1.MaximumX);
  Sl2.Reset(Sg2.MinimumX, Sg2.MaximumX);

  if not Sg1.SweepScanLine(Sl1) then
    Exit;

  if Flag2 then
    Flag2 := Sg2.SweepScanLine(Sl2);

  Ren.Prepare(Cardinal(Sg1.MaximumX - Sg1.MinimumX + 2));

  // A fake Span2 processor
  AddSpan2 := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanEmpty);
  try
    // The main loop
    // Here we synchronize the ScanLines with
    // the same Y coordinate, ignoring all other ones.
    // Only ScanLines having the same Y-coordinate
    // are to be combined.
    Flag1 := True;

    repeat
      // Synchronize "slave" with "master"
      while Flag2 and (Sl2.Y < Sl1.Y) do
        Flag2 := Sg2.SweepScanLine(Sl2);

      if Flag2 and (Sl2.Y = Sl1.Y) then
      begin
        // The Y coordinates are the same.
        // Combine the ScanLines and render if they contain any Spans.
        BoolScanLineUniteScanLines(Sl1, Sl2, Sl, AddSpan1, AddSpan2, CombineSpans);

        if Sl.NumSpans <> 0 then
        begin
          Sl.Finalize(Sl1.Y);
          Ren.Render(Sl);
        end;
      end
      else
        BoolScanLineAddSpansAndRender(Sl1, Sl, Ren, AddSpan1);

      // Advance the "master"
      Flag1 := Sg1.SweepScanLine(Sl1);
    until not Flag1;
  finally
    AddSpan2.Free;
  end;
end;


// Subtract shapes "sg1-sg2" with anti-aliasing
// See IntersectShapes_aa for more comments
procedure BoolScanLineSubtractShapesAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineSubtracTAggSpansAA);
  try
    BoolScanLineSubtractShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, AddFunctor,
      CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

// Apply eXclusive OR to two anti-aliased ScanLine shapes.
// There's the absolute difference used to calculate
// Anti-Aliasing values, that is:
// a XOR b : abs(a-b)
// See IntersectShapes_aa for more comments
procedure BoolScanLineXORShapesAbsDiffAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineXORSpansAA);
  try
    CombineFunctor.Formula := @BoolScanLineXORFormulaAbsDiff;

    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, AddFunctor, AddFunctor,
      CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

// Apply eXclusive OR to two anti-aliased ScanLine shapes.
// There's the classical "Saddle" used to calculate the
// Anti-Aliasing values, that is:
// a XOR b : 1-((1-a+a*b)*(1-b+a*b))
// See IntersectShapes_aa for more comments
procedure BoolScanLineXORShapesSaddleAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineXORSpansAA);

  try
    CombineFunctor.Formula := @BoolScanLineXORFormulaSaddle;

    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, AddFunctor, AddFunctor,
      CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

// Apply eXclusive OR to two anti-aliased ScanLine shapes. There's
// a modified "Linear" XOR used instead of classical "Saddle" one.
// The reason is to have the result absolutely conststent with what
// the ScanLine Rasterizer produces.
// See IntersectShapes_aa for more comments
procedure BoolScanLineXORShapesAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineXORSpansAA);

  try
    CombineFunctor.Formula := @BoolScanLineXORFormulaLinear;

    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, AddFunctor,
      AddFunctor, CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

// Intersect two anti-aliased ScanLine shapes.
// Here the "ScanLine Generator" abstraction is used.
// ScanLineGen1 and ScanLineGen2 are the generators, and can be of
// type TAggRasterizerScanLineAA<>. There function requires three
// ScanLine containers that can be of different types.
// "sl1" and "sl2" are used to retrieve ScanLines from the generators,
// "sl" is ised as the resulting ScanLine to render it.
// The external "sl1" and "sl2" are used only for the sake of
// optimization and reusing of the scanline objects.
procedure BoolScanLineIntersectShapesAA(Sg1, Sg2: TAggRasterizerScanLine;
  Sl1, Sl2, Sl: TAggCustomScanLine; Ren: TAggCustomRendererScanLine);
var
  CombineFunctor: TAggBoolScanLineFunctor;
begin
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@SboolIntersecTAggSpansAA);
  try
    BoolScanLineIntersectShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, CombineFunctor);
  finally
    CombineFunctor.Free;
  end;
end;

// Unite two anti-aliased ScanLine shapes
// See IntersectShapes_aa for more comments
procedure BoolScanLineUniteShapesAA;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanAA);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineUniteSpansAA);
  try
    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, AddFunctor,
      AddFunctor, CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineCombineShapesAA(Op: TAggBoolScanLineOp;
  Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, Sl: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine);
begin
  case Op of
    bsoOr:
      BoolScanLineUniteShapesAA(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoAnd:
      BoolScanLineIntersectShapesAA(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoXor:
      BoolScanLineXORShapesAA(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoXorSaddle:
      BoolScanLineXORShapesSaddleAA(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoXorAbsDiff:
      BoolScanLineXORShapesAbsDiffAA(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoAMinusB:
      BoolScanLineSubtractShapesAA(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoBMinusA:
      BoolScanLineSubtractShapesAA(Sg2, Sg1, Sl2, Sl1, Sl, Ren);
  end;
end;

procedure BoolScanLineSubtractShapesBin;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanBin);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineCombineSpansEmpty);
  try
    BoolScanLineSubtractShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, AddFunctor,
      CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineXORShapesBin;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;

begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanBin);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineCombineSpansEmpty);

  try
    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, AddFunctor,
      AddFunctor, CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineIntersectShapesBin;
var
  CombineFunctor: TAggBoolScanLineFunctor;
begin
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineCombineSpansBin);

  try
    BoolScanLineIntersectShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, CombineFunctor);
  finally
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineUniteShapesBin;
var
  AddFunctor, CombineFunctor: TAggBoolScanLineFunctor;
begin
  AddFunctor := TAggBoolScanLineFunctor.Create1(@BoolScanLineAddSpanBin);
  CombineFunctor := TAggBoolScanLineFunctor.Create2(@BoolScanLineCombineSpansBin);

  try
    BoolScanLineUniteShapes(Sg1, Sg2, Sl1, Sl2, Sl, Ren, AddFunctor,
      AddFunctor, CombineFunctor);
  finally
    AddFunctor.Free;
    CombineFunctor.Free;
  end;
end;

procedure BoolScanLineCombineShapesBin(Op: TAggBoolScanLineOp;
  Sg1, Sg2: TAggRasterizerScanLine; Sl1, Sl2, Sl: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine);
begin
  case Op of
    bsoOr:
      BoolScanLineUniteShapesBin(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoAnd:
      BoolScanLineIntersectShapesBin(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoXor, bsoXorSaddle, bsoXorAbsDiff:
      BoolScanLineXORShapesBin(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoAMinusB:
      BoolScanLineSubtractShapesBin(Sg1, Sg2, Sl1, Sl2, Sl, Ren);

    bsoBMinusA:
      BoolScanLineSubtractShapesBin(Sg2, Sg1, Sl2, Sl1, Sl, Ren);
  end;
end;

end.
