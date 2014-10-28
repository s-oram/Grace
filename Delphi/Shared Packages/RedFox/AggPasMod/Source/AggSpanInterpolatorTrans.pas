unit AggSpanInterpolatorTrans;

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
//                                                                            //
//  Horizontal Span interpolator for use with an arbitrary transformer.       //
//  The efficiency highly depends on the operations done in the transformer   //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}
{$Q-}
{$R-}

uses
  AggBasics,
  AggTransAffine,
  AggSpanInterpolatorLinear;

type
  TAggSpanInterpolatorTrans = class(TAggSpanInterpolator)
  private
    FTrans: TAggTransAffine;

    FX, FY: Double;
    FIntX, FIntY: Integer;
  public
    constructor Create(SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; X, Y, Z: Cardinal;
      SS: Cardinal = 8); overload;

    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;

    procedure SetBegin(X, Y: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;
  end;

implementation


{ TAggSpanInterpolatorTrans }

constructor TAggSpanInterpolatorTrans.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTrans := nil;
end;

constructor TAggSpanInterpolatorTrans.Create(Trans: TAggTransAffine;
  SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTrans := Trans;
end;

constructor TAggSpanInterpolatorTrans.Create(Trans: TAggTransAffine;
  X, Y, Z: Cardinal; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FTrans := Trans;

  SetBegin(X, Y, 0);
end;

function TAggSpanInterpolatorTrans.GetTransformer;
begin
  Result := FTrans;
end;

procedure TAggSpanInterpolatorTrans.SetTransformer;
begin
  FTrans := Trans;
end;

procedure TAggSpanInterpolatorTrans.SetBegin(X, Y: Double; Len: Cardinal);
begin
  FX := X;
  FY := Y;

  FTrans.Transform(FTrans, @X, @Y);

  FIntX := IntegerRound(X * FSubpixelSize);
  FIntY := IntegerRound(Y * FSubpixelSize);
end;

procedure TAggSpanInterpolatorTrans.IncOperator;
var
  X, Y: Double;

begin
  FX := FX + 1.0;

  X := FX;
  Y := FY;

  FTrans.Transform(FTrans, @X, @Y);

  FIntX := IntegerRound(X * FSubpixelSize);
  FIntY := IntegerRound(Y * FSubpixelSize);
end;

procedure TAggSpanInterpolatorTrans.Coordinates(X, Y: PInteger);
begin
  X^ := FIntX;
  Y^ := FIntY;
end;

procedure TAggSpanInterpolatorTrans.Coordinates(var X, Y: Integer);
begin
  X := FIntX;
  Y := FIntY;
end;

end.
