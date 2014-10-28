unit AggEllipse;

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
  TAggCustomEllipse = class(TAggVertexSource)
  private
    FCenter: TPointDouble;
    FApproximationScale: Double;
    FNum, FStep: Cardinal;
    FClockWise: Boolean;

    procedure SetApproximationScale(Value: Double);
  protected
    procedure CalculateNumSteps; virtual; abstract;
    function GetPathID(Index: Cardinal): Cardinal; override;
  public
    constructor Create; virtual;

    procedure Rewind(PathID: Cardinal); override;

    property ApproximationScale: Double read FApproximationScale write
      SetApproximationScale;
  end;

  TAggCircle = class(TAggCustomEllipse)
  private
    FRadius: Double;
  protected
    procedure CalculateNumSteps; override;
  public
    constructor Create; overload; override;
    constructor Create(X, Y, Radius: Double; NumSteps: Cardinal = 0;
      Cw: Boolean = False); overload;
    constructor Create(Center: TPointDouble; Radius: Double;
      NumSteps: Cardinal = 0; Cw: Boolean = False); overload;

    procedure Initialize(X, Y, Radius: Double; NumSteps: Cardinal = 0;
      Cw: Boolean = False); overload;
    procedure Initialize(Center: TPointDouble; Radius: Double;
      NumSteps: Cardinal = 0; Cw: Boolean = False); overload;

    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TAggEllipse = class(TAggCustomEllipse)
  private
    FRadius: TPointDouble;
  protected
    procedure CalculateNumSteps; override;
  public
    constructor Create; overload; override;
    constructor Create(X, Y, Rx, Ry: Double; NumSteps: Cardinal = 0;
      Cw: Boolean = False); overload;
    constructor Create(Center, Radius: TPointDouble; NumSteps: Cardinal = 0;
      Cw: Boolean = False); overload;

    procedure Initialize(X, Y, Rx, Ry: Double; NumSteps: Cardinal = 0;
      Cw: Boolean = False); overload;
    procedure Initialize(Center, Radius: TPointDouble; NumSteps: Cardinal = 0;
      Cw: Boolean = False); overload;

    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

implementation


{ TAggCustomEllipse }

constructor TAggCustomEllipse.Create;
begin
  inherited Create;
  FApproximationScale := 1.0;
end;

function TAggCustomEllipse.GetPathID(Index: Cardinal): Cardinal;
begin
  Result := 0;
end;

procedure TAggCustomEllipse.Rewind(PathID: Cardinal);
begin
  FStep := 0;
end;

procedure TAggCustomEllipse.SetApproximationScale(Value: Double);
begin
  if FApproximationScale <> Value then
  begin
    FApproximationScale := Value;
    CalculateNumSteps;
  end;
end;


{ TAggCircle }

constructor TAggCircle.Create;
begin
  inherited Create;

  Initialize(PointDouble(0), 1, 4, False);
end;

constructor TAggCircle.Create(X, Y, Radius: Double; NumSteps: Cardinal;
  Cw: Boolean);
begin
  inherited Create;

  Initialize(PointDouble(X, Y), Radius, NumSteps, Cw);
end;

constructor TAggCircle.Create(Center: TPointDouble; Radius: Double;
  NumSteps: Cardinal; Cw: Boolean);
begin
  inherited Create;

  Initialize(Center, Radius, NumSteps, Cw);
end;

procedure TAggCircle.Initialize(X, Y, Radius: Double; NumSteps: Cardinal;
  Cw: Boolean);
begin
  Initialize(PointDouble(X, Y), Radius, NumSteps, Cw);
end;

procedure TAggCircle.Initialize(Center: TPointDouble; Radius: Double;
  NumSteps: Cardinal; Cw: Boolean);
begin
  FCenter := Center;
  FRadius := Radius;

  FNum := NumSteps;
  FStep := 0;
  FClockWise := Cw;

  if FNum = 0 then
    CalculateNumSteps;
end;

function TAggCircle.Vertex(X, Y: PDouble): Cardinal;
var
  Angle: Double;
  Sn, Cn: Double;
begin
  if FStep = FNum then
  begin
    Inc(FStep);

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;

    Exit;
  end;

  if FStep > FNum then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  Angle := 2 * Pi * FStep / FNum;

  if FClockWise then
    Angle := 2 * Pi - Angle;

  SinCosScale(Angle, Sn, Cn, FRadius);
  X^ := FCenter.X + Cn;
  Y^ := FCenter.Y + Sn;

  Inc(FStep);

  if FStep = 1 then
    Result := CAggPathCmdMoveTo
  else
    Result := CAggPathCmdLineTo;
end;

procedure TAggCircle.CalculateNumSteps;
var
  Ra, Da: Double;
begin
  Ra := Abs(FRadius);
  Da := ArcCos(Ra / (Ra + 0.125 / FApproximationScale)) * 2;

  FNum := Trunc(2 * Pi / Da);
end;


{ TAggEllipse }

constructor TAggEllipse.Create;
begin
  inherited Create;

  Initialize(PointDouble(0), PointDouble(1), 4, False);
end;

constructor TAggEllipse.Create(X, Y, Rx, Ry: Double; NumSteps: Cardinal = 0;
  Cw: Boolean = False);
begin
  inherited Create;

  Initialize(PointDouble(X, Y), PointDouble(Rx, Ry), NumSteps, Cw);
end;

constructor TAggEllipse.Create(Center, Radius: TPointDouble; NumSteps: Cardinal;
  Cw: Boolean);
begin
  inherited Create;

  Initialize(Center, Radius, NumSteps, Cw);
end;

procedure TAggEllipse.Initialize(X, Y, Rx, Ry: Double; NumSteps: Cardinal = 0;
  Cw: Boolean = False);
begin
  Initialize(PointDouble(X, Y), PointDouble(Rx, Ry), NumSteps, Cw);
end;

procedure TAggEllipse.Initialize(Center, Radius: TPointDouble; NumSteps: Cardinal = 0;
  Cw: Boolean = False);
begin
  FCenter := Center;
  FRadius := Radius;

  FNum := NumSteps;
  FStep := 0;
  FClockWise := Cw;

  if FNum = 0 then
    CalculateNumSteps;
end;

function TAggEllipse.Vertex(X, Y: PDouble): Cardinal;
var
  Angle: Double;
  Sn, Cn: Double;
begin
  if FStep = FNum then
  begin
    Inc(FStep);

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose or CAggPathFlagsCcw;

    Exit;
  end;

  if FStep > FNum then
  begin
    Result := CAggPathCmdStop;

    Exit;
  end;

  Angle := 2 * Pi * FStep / FNum;

  if FClockWise then
    Angle := 2 * Pi - Angle;

  SinCosScale(Angle, Sn, Cn, FRadius.Y, FRadius.X);
  X^ := FCenter.X + Cn;
  Y^ := FCenter.Y + Sn;

  Inc(FStep);

  if FStep = 1 then
    Result := CAggPathCmdMoveTo
  else
    Result := CAggPathCmdLineTo;
end;

procedure TAggEllipse.CalculateNumSteps;
var
  Ra, Da: Double;
begin
  Ra := (Abs(FRadius.X) + Abs(FRadius.Y)) * 0.5;
  Da := ArcCos(Ra / (Ra + 0.125 / FApproximationScale)) * 2;

  FNum := Trunc(2 * Pi / Da);
end;

end.
