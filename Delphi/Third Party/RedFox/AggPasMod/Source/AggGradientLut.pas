unit AggGradientLut;

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
  AggArray,
  AggDdaLine,
  AggColor;

type
  TAggGradientLut = class(TAggCustomArray)
  private
    FColorLutSize: Cardinal;
    FColorProfile: TAggPodBVector;
    FColorLut: TAggPodArray;
  protected
    function GetSize: Cardinal; override;
    function GetEntry: Cardinal; override;

    // Size-index Interface. This class can be used directly as the
    // ColorF in SpanGradient. All it needs is two access methods
    // size() and operator [].
    function ArrayOperator(Index: Cardinal): Pointer; override;
    property ItemPointer[Index: Cardinal]: Pointer read ArrayOperator; default;
  public
    constructor Create(ASize: Cardinal = 256);
    destructor Destroy; override;

    // Build Gradient Lut
    // First, call RemoveAll(), then addColor() at least twice,
    // then BuildLut(). Argument "offset" in addColor must be
    // in range [0...1] and defines a color stop as it is described
    // in SVG specification, section Gradients and Patterns.
    // The simplest linear Gradient is:
    // TAggGradientLut.addColor(0.0, startColor);
    // TAggGradientLut.addColor(1.0, endColor);
    procedure RemoveAll;
    procedure AddColor(Offset: Double; Color: PAggColor);

    procedure BuildLut;
  end;

implementation

type
  PAggColorPoint = ^TAggColorPoint;
  TAggColorPoint = record
    Offset: Double;
    Color: TAggColor;
  end;

  TAggColorInterpolator = record
    FC1, FC2: TAggColor;
    FLength, FCount: Cardinal;
    V, R, G, B, A: TAggDdaLineInterpolator;
    FIsGray: Boolean;
  public
    procedure Initialize(C1, C2: PAggColor; Len: Cardinal;
      IsGray: Boolean = False);

    procedure OperatorInc;

    function Color: TAggColor;
  end;


{ TAggColorInterpolator }

procedure TAggColorInterpolator.Initialize(C1, C2: PAggColor; Len: Cardinal;
  IsGray: Boolean = False);
begin
  FC1 := C1^;
  FC2 := C2^;

  FLength := Len;
  FCount := 0;

  FIsGray := IsGray;

  if FIsGray then
    V.Initialize(C1.V, C2.V, Len, 14)

  else
  begin
    R.Initialize(C1.Rgba8.R, C2.Rgba8.R, Len, 14);
    G.Initialize(C1.Rgba8.G, C2.Rgba8.G, Len, 14);
    B.Initialize(C1.Rgba8.B, C2.Rgba8.B, Len, 14);
  end;

  A.Initialize(C1.Rgba8.A, C2.Rgba8.A, Len, 14);
end;

procedure TAggColorInterpolator.OperatorInc;
begin
  Inc(FCount);

  if FIsGray then
    V.PlusOperator
  else
  begin
    R.PlusOperator;
    G.PlusOperator;
    B.PlusOperator;
  end;

  A.PlusOperator;
end;

function TAggColorInterpolator.Color: TAggColor;
begin
  if FIsGray then
    Result.FromValueInteger(R.Y, A.Y)
  else
    Result.FromRgbaInteger(R.Y, G.Y, B.Y, A.Y)
end;


{ TAggGradientLut }

constructor TAggGradientLut.Create(ASize: Cardinal = 256);
begin
  FColorLutSize := ASize;

  FColorProfile := TAggPodBVector.Create(SizeOf(TAggColorPoint), 4);
  FColorLut := TAggPodArray.Create(SizeOf(TAggColor), FColorLutSize);
end;

destructor TAggGradientLut.Destroy;
begin
  FColorProfile.Free;
  FColorLut.Free;
  inherited
end;

procedure TAggGradientLut.RemoveAll;
begin
  FColorProfile.RemoveAll;
end;

procedure TAggGradientLut.AddColor(Offset: Double; Color: PAggColor);
var
  Cp: TAggColorPoint;
begin
  if Offset < 0.0 then
    Offset := 0.0;

  if Offset > 1.0 then
    Offset := 1.0;

  Cp.Color := Color^;
  Cp.Offset := Offset;


  FColorProfile.Add(@Cp);
end;

function OffsetLess(A, B: PAggColorPoint): Boolean;
begin
  Result := A.Offset < B.Offset;
end;

function OffsetEqual(A, B: PAggColorPoint): Boolean;
begin
  Result := A.Offset = B.Offset;
end;

procedure TAggGradientLut.BuildLut;
var
  I, Start, Stop: Cardinal;
  C: TAggColor;
  Ci: TAggColorInterpolator;
begin
  QuickSort(FColorProfile, @OffsetLess);
  FColorProfile.CutAt(RemoveDuplicates(FColorProfile, @OffsetEqual));

  if FColorProfile.Size >= 2 then
  begin
    Start := UnsignedRound(PAggColorPoint(FColorProfile[0]).Offset *
      FColorLutSize);

    C := PAggColorPoint(FColorProfile[0]).Color;
    I := 0;

    while I < Start do
    begin
      PAggColor(FColorLut[I])^ := C;

      Inc(I);
    end;

    I := 1;

    while I < FColorProfile.Size do
    begin
      Stop := UnsignedRound(PAggColorPoint(FColorProfile[I]).Offset *
        FColorLutSize);

      Ci.Initialize(@PAggColorPoint(FColorProfile[I - 1]).Color,
        @PAggColorPoint(FColorProfile[I]).Color, Stop - Start + 1);

      while Start < Stop do
      begin
        PAggColor(FColorLut[Start])^ := Ci.Color;

        Ci.OperatorInc;

        Inc(Start);
      end;

      Inc(I);
    end;

    C := PAggColorPoint(FColorProfile.Last).Color;

    while Stop < FColorLut.Size do
    begin
      PAggColor(FColorLut[Stop])^ := C;

      Inc(Stop);
    end;
  end;
end;

function TAggGradientLut.GetSize: Cardinal;
begin
  Result := FColorLutSize;
end;

function TAggGradientLut.GetEntry: Cardinal;
begin
  Result := FColorLut.EntrySize;
end;

function TAggGradientLut.ArrayOperator(Index: Cardinal): Pointer;
begin
  Result := FColorLut[Index];
end;

end.
