unit AggConvMarkerAdaptor;

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
  AggConvAdaptorVcgen,
  AggVcgenVertexSequence,
  AggVertexSource;

type
  TAggConvMarkerAdaptor = class(TAggConvAdaptorVcgen)
  private
    FGenerator: TAggVcgenVertexSequence;

    procedure SetShorten(Value: Double);
    function GetShorten: Double;
  public
    constructor Create(VertexSource: TAggVertexSource);
    destructor Destroy; override;

    property Shorten: Double read GetShorten write SetShorten;
  end;

implementation


{ TAggConvMarkerAdaptor }

constructor TAggConvMarkerAdaptor.Create(VertexSource: TAggVertexSource);
begin
  FGenerator := TAggVcgenVertexSequence.Create;

  inherited Create(VertexSource, FGenerator);
end;

destructor TAggConvMarkerAdaptor.Destroy;
begin
  FGenerator.Free;
  inherited;
end;

procedure TAggConvMarkerAdaptor.SetShorten(Value: Double);
begin
  FGenerator.Shorten := Value;
end;

function TAggConvMarkerAdaptor.GetShorten: Double;
begin
  Result := FGenerator.Shorten;
end;

end.
