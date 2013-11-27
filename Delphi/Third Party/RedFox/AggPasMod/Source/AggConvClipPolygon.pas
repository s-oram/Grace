unit AggConvClipPolygon;

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
//                                                                            //
//  Polygon Clipping Converter:                                               //
//  An optimized Liang-Basky algorithm is used here.                          //
//  The algorithm doesn't optimize the degenerate edges, i.e. it will never   //
//  break a closed polygon into two or more ones, instead, there will be      //
//  degenerate edges coinciding with the respective clipping boundaries.      //
//  This is a sub-optimal solution, because that optimization would require   //
//  extra, rather expensive math while the Rasterizer tolerates it quite      //
//  well, without any considerable overhead.                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  AggBasics,
  AggConvAdaptorVpgen,
  AggVpGenClipPolygon,
  AggVertexSource;

type
  TAggConvClipPolygon = class(TAggConvAdaptorVpgen)
  private
    FGenerator: TAggVpgenClipPolygon;
    function GetX1: Double;
    function GetY1: Double;
    function GetX2: Double;
    function GetY2: Double;
  public
    constructor Create(Vs: TAggVertexSource);
    destructor Destroy; override;

    procedure SetClipBox(X1, Y1, X2, Y2: Double);

    property X1: Double read GetX1;
    property Y1: Double read GetY1;
    property X2: Double read GetX2;
    property Y2: Double read GetY2;
  end;

implementation


{ TAggConvClipPolygon }

constructor TAggConvClipPolygon.Create;
begin
  FGenerator := TAggVpgenClipPolygon.Create;

  inherited Create(Vs, FGenerator);
end;

destructor TAggConvClipPolygon.Destroy;
begin
  FGenerator.Free;
  inherited;
end;

procedure TAggConvClipPolygon.SetClipBox(X1, Y1, X2, Y2: Double);
begin
  TAggVpgenClipPolygon(Vpgen).SetClipBox(X1, Y1, X2, Y2);
end;

function TAggConvClipPolygon.GetX1: Double;
begin
  Result := TAggVpgenClipPolygon(Vpgen).X1;
end;

function TAggConvClipPolygon.GetY1: Double;
begin
  Result := TAggVpgenClipPolygon(Vpgen).Y1;
end;

function TAggConvClipPolygon.GetX2: Double;
begin
  Result := TAggVpgenClipPolygon(Vpgen).X2;
end;

function TAggConvClipPolygon.GetY2: Double;
begin
  Result := TAggVpgenClipPolygon(Vpgen).Y2;
end;

end.
