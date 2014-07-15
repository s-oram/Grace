unit AggSpanSubdivAdaptor;

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
  AggSpanInterpolatorLinear,
  AggTransAffine;

type
  TAggSpanSubdivAdaptor = class(TAggSpanInterpolator)
  private
    FSubdivShift, FSubdivSize, FSubdivMask: Cardinal;

    FInterpolator: TAggSpanInterpolator;

    FSourceX: Integer;
    FSourceY: Double;
    FPos, FLength: Cardinal;

    procedure SetSubdivShift(Shift: Cardinal);
    procedure SetInterpolator(Value: TAggSpanInterpolator);
  protected
    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;
  public
    constructor Create(SS: Cardinal = 8); overload;
    constructor Create(Interpolator: TAggSpanInterpolator;
      ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;
    constructor Create(Interpolator: TAggSpanInterpolator; X, Y: Double;
      Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;

    procedure SetBegin(X, Y: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;

    procedure LocalScale(X, Y: PInteger); override;

    property Interpolator: TAggSpanInterpolator read FInterpolator write
      SetInterpolator;
    property SubdivShift: Cardinal read FSubdivShift write SetSubdivShift;
    property Transformer: TAggTransAffine read GetTransformer write
      SetTransformer;
  end;

implementation


{ TAggSpanSubdivAdaptor }

constructor TAggSpanSubdivAdaptor.Create(SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;

  FSubdivShift := 4;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FInterpolator := nil;
end;

constructor TAggSpanSubdivAdaptor.Create(Interpolator: TAggSpanInterpolator;
  ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;

  FSubdivShift := ASubdivShift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FInterpolator := Interpolator;
end;

constructor TAggSpanSubdivAdaptor.Create(Interpolator: TAggSpanInterpolator;
  X, Y: Double; Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;

  FSubdivShift := ASubdivShift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FInterpolator := Interpolator;

  SetBegin(X, Y, Len);
end;

procedure TAggSpanSubdivAdaptor.SetInterpolator(Value: TAggSpanInterpolator);
begin
  FInterpolator := Value;
end;

function TAggSpanSubdivAdaptor.GetTransformer;
begin
  Result := FInterpolator.Transformer;
end;

procedure TAggSpanSubdivAdaptor.SetTransformer;
begin
  FInterpolator.Transformer := Trans;
end;

procedure TAggSpanSubdivAdaptor.SetSubdivShift;
begin
  FSubdivShift := Shift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;
end;

procedure TAggSpanSubdivAdaptor.SetBegin(X, Y: Double; Len: Cardinal);
begin
  FPos := 1;
  FSourceX := Trunc(X * FSubpixelSize) + FSubpixelSize;
  FSourceY := Y;
  FLength := Len;

  if Len > FSubdivSize then
    Len := FSubdivSize;

  FInterpolator.SetBegin(X, Y, Len);
end;

procedure TAggSpanSubdivAdaptor.IncOperator;
var
  Len: Cardinal;

begin
  FInterpolator.IncOperator;

  if FPos >= FSubdivSize then
  begin
    Len := FLength;

    if Len > FSubdivSize then
      Len := FSubdivSize;

    FInterpolator.Resynchronize(FSourceX / FSubpixelSize + Len, FSourceY, Len);

    FPos := 0;
  end;

  Inc(FSourceX, FSubpixelSize);
  Inc(FPos);
  Dec(FLength);
end;

procedure TAggSpanSubdivAdaptor.Coordinates(X, Y: PInteger);
begin
  FInterpolator.Coordinates(X, Y);
end;

procedure TAggSpanSubdivAdaptor.Coordinates(var X, Y: Integer);
begin
  FInterpolator.Coordinates(X, Y);
end;

procedure TAggSpanSubdivAdaptor.LocalScale;
begin
  FInterpolator.LocalScale(X, Y);
end;

end.
