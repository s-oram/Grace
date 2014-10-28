unit AggTransWarpMagnifier;

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
  AggTransAffine;

type
  TAggTransWarpMagnifier = class(TAggTransAffine)
  private
    FCenter: TPointDouble;
    FMagnification, FRadius: Double;
    procedure SetMagnification(M: Double);
    procedure SetRadius(R: Double);
  public
    constructor Create;

    procedure SetCenter(X, Y: Double);

    property Magnification: Double read FMagnification write SetMagnification;
    property Radius: Double read FRadius write SetRadius;
  end;

implementation

procedure WarpMagnifierTransform(This: TAggTransWarpMagnifier; X, Y: PDouble);
var
  Dx, Dy, R, M: Double;

begin
  Dx := X^ - This.FCenter.X;
  Dy := Y^ - This.FCenter.Y;
  R := Sqrt(Dx * Dx + Dy * Dy);

  if R < This.FRadius then
  begin
    X^ := This.FCenter.X + Dx * This.FMagnification;
    Y^ := This.FCenter.Y + Dy * This.FMagnification;

    Exit;
  end;

  M := (R + This.FRadius * (This.FMagnification - 1.0)) / R;

  X^ := This.FCenter.X + Dx * M;
  Y^ := This.FCenter.Y + Dy * M;
end;

procedure WarpMagnifierTransformInverseTransform(This: TAggTransWarpMagnifier;
  X, Y: PDouble);
var
  T: TAggTransWarpMagnifier;
begin
  T := TAggTransWarpMagnifier.Create;
  try
    T := This;

    T.SetMagnification(1.0 / This.FMagnification);
    T.SetRadius(This.FRadius * This.FMagnification);
    T.Transform(@T, X, Y);
  finally
    T.Free;
  end;
end;


{ TAggTransWarpMagnifier }

constructor TAggTransWarpMagnifier.Create;
begin
  inherited Create;

  FCenter.X := 0.0;
  FCenter.Y := 0.0;

  FMagnification := 1.0;
  FRadius := 1.0;

  Transform := @WarpMagnifierTransform;
  InverseTransform := @WarpMagnifierTransformInverseTransform;
end;

procedure TAggTransWarpMagnifier.SetCenter(X, Y: Double);
begin
  FCenter.X := X;
  FCenter.Y := Y;
end;

procedure TAggTransWarpMagnifier.SetMagnification(M: Double);
begin
  FMagnification := M;
end;

procedure TAggTransWarpMagnifier.SetRadius(R: Double);
begin
  FRadius := R;
end;

end.
