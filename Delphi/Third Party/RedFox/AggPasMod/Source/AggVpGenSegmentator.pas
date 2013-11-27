unit AggVpGenSegmentator;

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
  AggVpGen,
  AggVertexSource;

type
  TAggVpgenSegmentator = class(TAggCustomVpgen)
  private
    FApproximationScale, FX1, FY1: Double;
    FDeltaL, FDeltaDeltaL: Double;
    FDelta: TPointDouble;
    FCmd: Cardinal;
    procedure SetApproximationScale(S: Double);
    function GetAutoClose: Boolean;
    function GetAutoUnclose: Boolean;
  public
    constructor Create; override;

    procedure Reset; override;
    procedure MoveTo(X, Y: Double); override;
    procedure LineTo(X, Y: Double); override;

    function Vertex(X, Y: PDouble): Cardinal; override;

    property ApproximationScale: Double read FApproximationScale write SetApproximationScale;
    property AutoClose: Boolean read GetAutoClose;
    property AutoUnclose: Boolean read GetAutoUnclose;
  end;

implementation


{ TAggVpgenSegmentator }

constructor TAggVpgenSegmentator.Create;
begin
  FApproximationScale := 1.0;

  FX1 := 0;
  FY1 := 0;
  FDelta.X := 0;
  FDelta.Y := 0;
  FDeltaL := 0;
  FDeltaDeltaL := 0;
  FCmd := 0;
end;

procedure TAggVpgenSegmentator.SetApproximationScale;
begin
  FApproximationScale := S;
end;

function TAggVpgenSegmentator.GetAutoClose;
begin
  Result := False
end;

function TAggVpgenSegmentator.GetAutoUnclose;
begin
  Result := False
end;

procedure TAggVpgenSegmentator.Reset;
begin
  FCmd := CAggPathCmdStop;
end;

procedure TAggVpgenSegmentator.MoveTo;
begin
  FX1 := X;
  FY1 := Y;
  FDelta.X := 0.0;
  FDelta.Y := 0.0;
  FDeltaL := 2.0;
  FDeltaDeltaL := 2.0;
  FCmd := CAggPathCmdMoveTo;
end;

procedure TAggVpgenSegmentator.LineTo;
var
  Len: Double;
begin
  FX1 := FX1 + FDelta.X;
  FY1 := FY1 + FDelta.Y;
  FDelta.X := X - FX1;
  FDelta.Y := Y - FY1;

  Len := Sqrt(FDelta.X * FDelta.X + FDelta.Y * FDelta.Y) * FApproximationScale;

  if Len < 1E-30 then
    Len := 1E-30;

  FDeltaDeltaL := 1.0 / Len;

  if FCmd = CAggPathCmdMoveTo then
    FDeltaL := 0.0
  else
    FDeltaL := FDeltaDeltaL;

  if FCmd = CAggPathCmdStop then
    FCmd := CAggPathCmdLineTo;
end;

function TAggVpgenSegmentator.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  if FCmd = CAggPathCmdStop then
    Result := CAggPathCmdStop

  else
  begin
    Cmd := FCmd;
    FCmd := CAggPathCmdLineTo;

    if FDeltaL >= 1.0 - FDeltaDeltaL then
    begin
      FDeltaL := 1.0;
      FCmd := CAggPathCmdStop;

      X^ := FX1 + FDelta.X;
      Y^ := FY1 + FDelta.Y;

      Result := Cmd;

    end
    else
    begin
      X^ := FX1 + FDelta.X * FDeltaL;
      Y^ := FY1 + FDelta.Y * FDeltaL;

      FDeltaL := FDeltaL + FDeltaDeltaL;

      Result := Cmd;
    end;
  end;
end;

end.
