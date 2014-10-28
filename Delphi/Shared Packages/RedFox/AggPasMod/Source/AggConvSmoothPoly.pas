unit AggConvSmoothPoly;

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
  AggVcgenSmoothPoly1,
  AggConvAdaptorVcgen,
  AggConvCurve,
  AggVertexSource;

type
  TAggConvSmoothPoly = class(TAggConvAdaptorVcgen)
  private
    FGenerator: TAggVcgenSmoothPoly1;

    procedure SetSmoothValue(Value: Double);
    function GetSmoothValue: Double;
  public
    constructor Create(VertexSource: TAggVertexSource);
    destructor Destroy; override;

    property SmoothValue: Double read GetSmoothValue write SetSmoothValue;
  end;

  TAggConvSmoothPolyCurve = class(TAggConvCurve)
  private
    FSmooth: TAggConvSmoothPoly;

    procedure SetSmoothValue(Value: Double);
    function GetSmoothValue: Double;
  public
    constructor Create(VertexSource: TAggVertexSource);
    destructor Destroy; override;

    property SmoothValue: Double read GetSmoothValue write SetSmoothValue;
  end;

implementation


{ TAggConvSmoothPoly }

constructor TAggConvSmoothPoly.Create(VertexSource: TAggVertexSource);
begin
  FGenerator := TAggVcgenSmoothPoly1.Create;

  inherited Create(VertexSource, FGenerator);
end;

destructor TAggConvSmoothPoly.Destroy;
begin
  FGenerator.Free;

  inherited;
end;

procedure TAggConvSmoothPoly.SetSmoothValue;
begin
  FGenerator.SmoothValue := Value;
end;

function TAggConvSmoothPoly.GetSmoothValue;
begin
  Result := FGenerator.SmoothValue;
end;


{ TAggConvSmoothPolyCurve }

constructor TAggConvSmoothPolyCurve.Create(VertexSource: TAggVertexSource);
begin
  FSmooth := TAggConvSmoothPoly.Create(VertexSource);

  inherited Create(FSmooth);
end;

destructor TAggConvSmoothPolyCurve.Destroy;
begin
  FSmooth.Free;

  inherited;
end;

procedure TAggConvSmoothPolyCurve.SetSmoothValue;
begin
  TAggVcgenSmoothPoly1(FSmooth.Generator).SmoothValue := Value;
end;

function TAggConvSmoothPolyCurve.GetSmoothValue;
begin
  Result := TAggVcgenSmoothPoly1(FSmooth.Generator).SmoothValue;
end;

end.
