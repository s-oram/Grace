unit AggQuadratureOscillator;

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

uses
  AggBasics;

type
  TAggQuadratureOscillator = class
  private
    function GetPhase: Double;
    procedure SetPhase(const Value: Double);
    procedure SetFrequency(const Value: Double);
    procedure SetAmplitude(const Value: Double);
  protected
    FAmplitude     : Double;
    FFrequency     : Double;
    FAngle         : TPointDouble;
    FPosition      : TPointDouble;
    procedure FrequencyChanged; virtual;
  public
    constructor Create(Frequency: Double; Amplitude: Double = 1); virtual;

    procedure Next; virtual;
    procedure Reset; virtual;

    property Amplitude: Double read FAmplitude write SetAmplitude;
    property Frequency: Double read FFrequency write SetFrequency;
    property Sine: Double read FPosition.Y;
    property Cosine: Double read FPosition.X;
    property Phase: Double read GetPhase write SetPhase;
  end;

implementation

uses
  Math, AggMath;

{ TAggQuadratureOscillator }

constructor TAggQuadratureOscillator.Create(Frequency: Double;
  Amplitude: Double = 1);
begin
  inherited Create;
  FFrequency := Frequency;
  FAmplitude := Amplitude;
  SinCos(FFrequency, FAngle.X, FAngle.Y);
  FPosition.Y := 0;
  FPosition.X := FAmplitude;
end;

procedure TAggQuadratureOscillator.FrequencyChanged;
begin
 SinCos(FFrequency, FAngle.X, FAngle.Y);
end;

function TAggQuadratureOscillator.GetPhase: Double;
begin
  Result := -ArcTan2(FPosition.Y, -FPosition.X);
end;

procedure TAggQuadratureOscillator.Next;
var
  Temp : Double;
begin
  Temp := FPosition.Y * FAngle.Y - FPosition.X * FAngle.X;
  FPosition.X := FPosition.X * FAngle.Y + FPosition.Y * FAngle.X;
  FPosition.Y := Temp;
end;

procedure TAggQuadratureOscillator.Reset;
begin
  Phase := 0;
end;

procedure TAggQuadratureOscillator.SetAmplitude(const Value: Double);
begin
  if FAmplitude <> Value then
  begin
    if FAmplitude = 0 then
    begin
      FPosition.Y := 0;
      FPosition.X := Value;
    end
   else
    begin
      FPosition.Y := FPosition.Y / FAmplitude * Value;
      FPosition.X := FPosition.X / FAmplitude * Value;
    end;
    FAmplitude := Value;
  end;
end;

procedure TAggQuadratureOscillator.SetFrequency(const Value: Double);
begin
  if FFrequency <> Value then
  begin
     FFrequency := Value;
     FrequencyChanged;
  end;
end;

procedure TAggQuadratureOscillator.SetPhase(const Value: Double);
begin
  SinCos(Value, FPosition.Y, FPosition.X);
  FPosition.Y := FPosition.Y * -FAmplitude;
  FPosition.X := FPosition.X * -FAmplitude;
end;

end.

