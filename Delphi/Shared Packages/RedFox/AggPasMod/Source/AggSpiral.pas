unit AggSpiral;

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
  SysUtils,
  AggMath,
  AggBasics,
  AggVertexSource;

type
  TSpiral = class(TAggVertexSource)
  private
    FX, FY, FR1, FR2, FStep, FStartAngle, FAngle: Double;
    FCurrentRadius, FDa, FDr: Double;
    FStart: Boolean;
  public
    constructor Create(X, Y, R1, R2, Step: Double; StartAngle: Double = 0);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

implementation

{ TSpiral }

constructor TSpiral.Create(X, Y, R1, R2, Step: Double; StartAngle: Double = 0);
begin
  FX := X;
  FY := Y;
  FR1 := R1;
  FR2 := R2;

  FStep := Step;
  FStartAngle := StartAngle;
  FAngle := StartAngle;

  FDa := Deg2Rad(4.0);
  FDr := FStep / 90.0;
end;

procedure TSpiral.Rewind(PathID: Cardinal);
begin
  FAngle := FStartAngle;
  FCurrentRadius := FR1;
  FStart := True;
end;

function TSpiral.Vertex(X, Y: PDouble): Cardinal;
var
  Pnt: TPointDouble;
begin
  if FCurrentRadius > FR2 then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  SinCosScale(FAngle, Pnt.Y, Pnt.X, FCurrentRadius);

  X^ := FX + Pnt.X;
  Y^ := FY + Pnt.Y;

  FCurrentRadius := FCurrentRadius + FDr;
  FAngle := FAngle + FDa;

  if FStart then
  begin
    FStart := False;

    Result := CAggPathCmdMoveTo;
  end
  else
    Result := CAggPathCmdLineTo;
end;

end.
