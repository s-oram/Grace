unit AggConvSegmentator;

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
  AggVertexSource,
  AggConvAdaptorVpgen,
  AggVpgenSegmentator;

type
  TAggConvSegmentator = class(TAggConvAdaptorVpgenSegmentator)
  private
    FGenerator: TAggVpgenSegmentator;

    procedure SetApproximationScale(Value: Double);
    function GetApproximationScale: Double;
  public
    constructor Create(Vs: TAggCustomVertexSource);
    destructor Destroy; override;

    property ApproximationScale: Double read GetApproximationScale write
      SetApproximationScale;
  end;


implementation


{ TAggConvSegmentator }

constructor TAggConvSegmentator.Create(Vs: TAggCustomVertexSource);
begin
  FGenerator := TAggVpgenSegmentator.Create;

  inherited Create(Vs, FGenerator);
end;

destructor TAggConvSegmentator.Destroy;
begin
  FGenerator.Free;
  inherited;
end;

procedure TAggConvSegmentator.SetApproximationScale(Value: Double);
begin
  VpGenSegmentator.ApproximationScale := Value;
end;

function TAggConvSegmentator.GetApproximationScale: Double;
begin
  Result := VpGenSegmentator.ApproximationScale;
end;

end.
