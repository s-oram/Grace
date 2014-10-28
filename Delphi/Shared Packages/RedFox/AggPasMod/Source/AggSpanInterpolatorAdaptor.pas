unit AggSpanInterpolatorAdaptor;

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
  TAggDistortion = class
  public
    procedure Calculate(var X, Y: Integer); virtual; abstract;
  end;

  TAggSpanInterpolatorAdaptor = class(TAggSpanInterpolatorLinear)
  private
    FDistortion: TAggDistortion;
    procedure SetDistortion(Dist: TAggDistortion);
  public
    constructor Create; overload;
    constructor Create(Trans: TAggTransAffine; Dist: TAggDistortion); overload;
    constructor Create(Trans: TAggTransAffine; Dist: TAggDistortion;
      X, Y: Double; Len: Cardinal); overload;

    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;

    property Distortion: TAggDistortion read FDistortion write SetDistortion;
  end;

implementation


{ TAggSpanInterpolatorAdaptor }

constructor TAggSpanInterpolatorAdaptor.Create;
begin
  inherited Create;

  FDistortion := nil;
end;

constructor TAggSpanInterpolatorAdaptor.Create(Trans: TAggTransAffine;
  Dist: TAggDistortion);
begin
  inherited Create(Trans);

  FDistortion := Dist;
end;

constructor TAggSpanInterpolatorAdaptor.Create(Trans: TAggTransAffine;
  Dist: TAggDistortion; X, Y: Double; Len: Cardinal);
begin
  inherited Create(Trans, X, Y, Len);

  FDistortion := Dist;
end;

procedure TAggSpanInterpolatorAdaptor.SetDistortion;
begin
  FDistortion := Dist;
end;

procedure TAggSpanInterpolatorAdaptor.Coordinates(X, Y: PInteger);
begin
  inherited Coordinates(X, Y);

  FDistortion.Calculate(X^, Y^);
end;

procedure TAggSpanInterpolatorAdaptor.Coordinates(var X, Y: Integer);
begin
  inherited Coordinates(X, Y);

  FDistortion.Calculate(X, Y);
end;

end.
