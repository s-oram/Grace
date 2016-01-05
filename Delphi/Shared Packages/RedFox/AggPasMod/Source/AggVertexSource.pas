unit AggVertexSource;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Anti-Grain Geometry (modernized Pascal fork, aka 'AggPasMod')             //
//    Maintained by Christian-W. Budde (Christian@savioursofsoul.de)          //
//    Copyright (c) 2012-2015                                                      //
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
// Pascal replacement of the TAggVertexSource templetized concept from C++.   //
// This file is originaly not a part of the AGG.                              //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  AggBasics;

type
  TAggCustomVertexSource = class
  protected
    function GetPathCount: Cardinal; virtual;
  public
    procedure Rewind(PathID: Cardinal); virtual;
    function Vertex(X, Y: PDouble): Cardinal; virtual;

    function FuncOperatorGamma(X: Double): Double; virtual;

    property PathCount: Cardinal read GetPathCount;
  end;

  TAggVertexSource = class(TAggCustomVertexSource)
  protected
    function GetPathID(Index: Cardinal): Cardinal; virtual; abstract;
  public
    procedure RemoveAll; virtual;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); virtual;

    property PathID[Index: Cardinal]: Cardinal read GetPathID;
  end;

implementation


{ TAggCustomVertexSource }

function TAggCustomVertexSource.GetPathCount: Cardinal;
begin
  Result := 0;
end;

procedure TAggCustomVertexSource.Rewind(PathID: Cardinal);
begin
end;

function TAggCustomVertexSource.Vertex(X, Y: PDouble): Cardinal;
begin
end;

function TAggCustomVertexSource.FuncOperatorGamma;
begin
  Result := X;
end;


{ TAggVertexSource }

procedure TAggVertexSource.RemoveAll;
begin
end;

procedure TAggVertexSource.AddVertex;
begin
end;

end.
