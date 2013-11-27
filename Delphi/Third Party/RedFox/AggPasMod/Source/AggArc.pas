unit AggArc;

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
  Math,
  AggMath,
  AggBasics,
  AggVertexSource;

type
  TAggArc = class(TAggCustomVertexSource)
  private
    FRadius: TPointDouble;
    FX, FY, FAngle, FStart, FEnd, FScale, FDeltaAngle: Double;
    FCounterClockWise, FInitialized: Boolean;
    FPathCmd: Cardinal;

    procedure SetApproximationScale(Value: Double);
  protected
    procedure Normalize(A1, A2: Double; Ccw: Boolean);
  public
    constructor Create; overload;
    constructor Create(X, Y, Rx, Ry, A1, A2: Double;
      Ccw: Boolean = True); overload;

    procedure Init(X, Y, Rx, Ry, A1, A2: Double; Ccw: Boolean = True);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property ApproximationScale: Double read FScale write SetApproximationScale;
  end;

implementation


{ TAggArc }

constructor TAggArc.Create;
begin
  FX := 0;
  FY := 0;
  FRadius.X := 0;
  FRadius.Y := 0;
  FAngle := 0;
  FStart := 0;
  FEnd := 0;
  FDeltaAngle := 0;

  FCounterClockWise := False;
  FPathCmd := 0;

  FScale := 1;

  FInitialized := False;
end;

constructor TAggArc.Create(X, Y, Rx, Ry, A1, A2: Double; Ccw: Boolean = True);
begin
  Create;

  FX := X;
  FY := Y;
  FRadius.X := Rx;
  FRadius.Y := Ry;

  FScale := 1;

  Normalize(A1, A2, Ccw);
end;

procedure TAggArc.Init(X, Y, Rx, Ry, A1, A2: Double; Ccw: Boolean = True);
begin
  FX := X;
  FY := Y;
  FRadius.X := Rx;
  FRadius.Y := Ry;

  Normalize(A1, A2, Ccw);
end;

procedure TAggArc.SetApproximationScale(Value: Double);
begin
  FScale := Value;

  if FInitialized then
    Normalize(FStart, FEnd, FCounterClockWise);
end;

procedure TAggArc.Rewind(PathID: Cardinal);
begin
  FPathCmd := CAggPathCmdMoveTo;
  FAngle := FStart;
end;

function TAggArc.Vertex(X, Y: PDouble): Cardinal;
var
  Pf: Cardinal;
  Pnt : TPointDouble;
begin
  if IsStop(FPathCmd) then
    Result := CAggPathCmdStop

  else if (FAngle < FEnd - FDeltaAngle / 4) <> FCounterClockWise then
  begin
    SinCos(FEnd, Pnt.Y, Pnt.X);
    X^ := FX + Pnt.X * FRadius.X;
    Y^ := FY + Pnt.Y * FRadius.Y;

    FPathCmd := CAggPathCmdStop;

    Result := CAggPathCmdLineTo;
  end
  else
  begin
    SinCos(FAngle, Pnt.Y, Pnt.X);
    X^ := FX + Pnt.X * FRadius.X;
    Y^ := FY + Pnt.Y * FRadius.Y;

    FAngle := FAngle + FDeltaAngle;

    Pf := FPathCmd;
    FPathCmd := CAggPathCmdLineTo;

    Result := Pf;
  end;
end;

procedure TAggArc.Normalize(A1, A2: Double; Ccw: Boolean);
var
  Ra: Double;
begin
  Ra := (Abs(FRadius.X) + Abs(FRadius.Y)) * 0.5;
  FDeltaAngle := ArcCos(Ra / (Ra + 0.125 / FScale)) * 2;

  if Ccw then
    while A2 < A1 do
      A2 := A2 + (Pi * 2.0)
  else
  begin
    while A1 < A2 do
      A1 := A1 + (Pi * 2.0);

    FDeltaAngle := -FDeltaAngle;
  end;

  FCounterClockWise := Ccw;
  FStart := A1;
  FEnd := A2;

  FInitialized := True;
end;

end.
