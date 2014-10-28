unit AggConvBSpline;

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
  AggVcgenBSpline,
  AggConvAdaptorVcgen,
  AggVertexSource;

type
  TAggConvBSpline = class(TAggConvAdaptorVcgen)
  private
    FGenerator: TAggVcgenBSpline;

    procedure SetInterpolationStep(Value: Double);
    function GetInterpolationStep: Double;
  public
    constructor Create(Vs: TAggVertexSource);
    destructor Destroy; override;

    property InterpolationStep: Double read GetInterpolationStep
      write SetInterpolationStep;
  end;

implementation


{ TAggConvBSpline }

constructor TAggConvBSpline.Create;
begin
  FGenerator := TAggVcgenBSpline.Create;

  inherited Create(Vs, FGenerator);
end;

destructor TAggConvBSpline.Destroy;
begin
  FGenerator.Free;

  inherited;
end;

procedure TAggConvBSpline.SetInterpolationStep(Value: Double);
begin
  FGenerator.InterpolationStep := Value;
end;

function TAggConvBSpline.GetInterpolationStep;
begin
  Result := FGenerator.InterpolationStep;
end;

end.
