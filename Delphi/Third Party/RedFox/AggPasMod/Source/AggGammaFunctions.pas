unit AggGammaFunctions;

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
  AggBasics,
  AggVertexSource;

type
  TAggGammaNone = class(TAggCustomVertexSource);

  TAggGammaPower = class(TAggCustomVertexSource)
  private
    FGamma: Double;
  public
    constructor Create; overload;
    constructor Create(Gamma: Double); overload;

    function FuncOperatorGamma(X: Double): Double;  override;

    property Gamma: Double read FGamma write FGamma;
  end;

  TAggGammaThreshold = class(TAggCustomVertexSource)
  private
    FThreshold: Double;
  public
    constructor Create; overload;
    constructor Create(Threshold: Double); overload;

    function FuncOperatorGamma(X: Double): Double;  override;

    property Threshold: Double read FThreshold write FThreshold;
  end;

  TAggGammaLinear = class(TAggCustomVertexSource)
  private
    FStartGamma, FEndGamma: Double;
  public
    constructor Create; overload;
    constructor Create(StartGamma, EndGamma: Double); overload;

    procedure SetStartEnd(StartGamma, EndGamma: Double);

    function FuncOperatorGamma(X: Double): Double;  override;

    property StartGamma: Double read FStartGamma write FStartGamma;
    property EndGamma: Double read FEndGamma write FEndGamma;
  end;

  TAggGammaMultiply = class(TAggCustomVertexSource)
  private
    FMul: Double;
  public
    constructor Create; overload;
    constructor Create(V: Double); overload;

    function FuncOperatorGamma(X: Double): Double; override;

    property Value: Double read FMul write FMul;
  end;

implementation


{ TAggGammaPower }

constructor TAggGammaPower.Create;
begin
  FGamma := 1.0;
end;

constructor TAggGammaPower.Create(Gamma: Double);
begin
  FGamma := Gamma;
end;

function TAggGammaPower.FuncOperatorGamma(X: Double): Double;
begin
  try
    Result := Power(X, FGamma);
  except
    Result := 1;
  end;
end;


{ TAggGammaThreshold }

constructor TAggGammaThreshold.Create;
begin
  FThreshold := 0.5;
end;

constructor TAggGammaThreshold.Create(Threshold: Double);
begin
  FThreshold := Threshold;
end;

function TAggGammaThreshold.FuncOperatorGamma;
begin
  if X < FThreshold then
    Result := 0.0
  else
    Result := 1.0;
end;


{ TAggGammaLinear }

constructor TAggGammaLinear.Create;
begin
  FStartGamma := 0;
  FEndGamma := 1;
end;

constructor TAggGammaLinear.Create(StartGamma, EndGamma: Double);
begin
  FStartGamma := StartGamma;
  FEndGamma := EndGamma;
end;

procedure TAggGammaLinear.SetStartEnd;
begin
  FStartGamma := StartGamma;
  FEndGamma := EndGamma;
end;

function TAggGammaLinear.FuncOperatorGamma;
begin
  if X < FStartGamma then
    Result := 0
  else if X > FEndGamma then
    Result := 1
  else if FEndGamma - FStartGamma <> 0 then
    Result := (X - FStartGamma) / (FEndGamma - FStartGamma)
  else
    Result := 0;
end;


{ TAggGammaMultiply }

constructor TAggGammaMultiply.Create;
begin
  FMul := 1.0;
end;

constructor TAggGammaMultiply.Create(V: Double);
begin
  FMul := V;
end;

function TAggGammaMultiply.FuncOperatorGamma;
var
  Y: Double;
begin
  Y := X * FMul;

  if Y > 1.0 then
    Y := 1.0;

  Result := Y;
end;

end.
