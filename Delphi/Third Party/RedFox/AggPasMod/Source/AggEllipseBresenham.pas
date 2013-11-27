unit AggEllipseBresenham;

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
  TAggEllipseBresenhamInterpolator = record
  private
    FRadiusSquared, FTwoRadiusSquared: TPointInteger;
    FDelta, FInc: TPointInteger;
    FCurF: Integer;
  public
    procedure Initialize(Radius: Integer); overload;
    procedure Initialize(Rx, Ry: Integer); overload;

    procedure IncOperator;

    property DeltaX: Integer read FDelta.X;
    property DeltaY: Integer read FDelta.Y;
  end;

implementation


{ TAggEllipseBresenhamInterpolator }

procedure TAggEllipseBresenhamInterpolator.Initialize(Radius: Integer);
begin
  FRadiusSquared := PointInteger(Radius * Radius, Radius * Radius);

  FTwoRadiusSquared.X := FRadiusSquared.X shl 1;
  FTwoRadiusSquared.Y := FRadiusSquared.Y shl 1;

  FDelta := PointInteger(0);

  FInc.X := 0;
  FInc.Y := -Radius * FTwoRadiusSquared.X;
  FCurF := 0;
end;

procedure TAggEllipseBresenhamInterpolator.Initialize(Rx, Ry: Integer);
begin
  FRadiusSquared := PointInteger(Rx * Rx, Ry * Ry);

  FTwoRadiusSquared.X := FRadiusSquared.X shl 1;
  FTwoRadiusSquared.Y := FRadiusSquared.Y shl 1;

  FDelta := PointInteger(0);

  FInc.X := 0;
  FInc.Y := -Ry * FTwoRadiusSquared.X;
  FCurF := 0;
end;

procedure TAggEllipseBresenhamInterpolator.IncOperator;
var
  Mx, My, Mxy, Minimum, Fx, Fy, Fxy: Integer;
  Flag: Boolean;
begin
  Mx := FCurF + FInc.X + FRadiusSquared.Y;
  Fx := Mx;

  if Mx < 0 then
    Mx := -Mx;

  My := FCurF + FInc.Y + FRadiusSquared.X;
  Fy := My;

  if My < 0 then
    My := -My;

  Mxy := FCurF + FInc.X + FRadiusSquared.Y + FInc.Y + FRadiusSquared.X;
  Fxy := Mxy;

  if Mxy < 0 then
    Mxy := -Mxy;

  Minimum := Mx;
  Flag := True;

  if Minimum > My then
  begin
    Minimum := My;
    Flag := False;
  end;

  FDelta := PointInteger(0);

  if Minimum > Mxy then
  begin
    Inc(FInc.X, FTwoRadiusSquared.Y);
    Inc(FInc.Y, FTwoRadiusSquared.X);

    FCurF := Fxy;

    FDelta.X := 1;
    FDelta.Y := 1;

    Exit;
  end;

  if Flag then
  begin
    Inc(FInc.X, FTwoRadiusSquared.Y);

    FCurF := Fx;
    FDelta.X := 1;

    Exit;
  end;

  Inc(FInc.Y, FTwoRadiusSquared.X);

  FCurF := Fy;
  FDelta.Y := 1;
end;

end.
