unit AggConvTransform;

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
  AggTransAffine,
  AggVertexSource;

type
  TAggConvTransform = class(TAggVertexSource)
  private
    FSource: TAggVertexSource;
    FTrans: TAggTransAffine;
  protected
    function GetPathID(Index: Cardinal): Cardinal; override;
    function GetPathCount: Cardinal; override;
  public
    constructor Create(Source: TAggVertexSource; Tr: TAggTransAffine);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Source: TAggVertexSource read FSource write FSource;
    property Transformer: TAggTransAffine read FTrans write FTrans;
  end;

implementation


{ TAggConvTransform }

constructor TAggConvTransform.Create(Source: TAggVertexSource; Tr: TAggTransAffine);
begin
  inherited Create;

  FSource := Source;
  FTrans := Tr;
end;

function TAggConvTransform.GetPathCount: Cardinal;
begin
  Result := FSource.PathCount;
end;

function TAggConvTransform.GetPathID(Index: Cardinal): Cardinal;
begin
  Result := FSource.PathID[Index];
end;

procedure TAggConvTransform.Rewind(PathID: Cardinal);
begin
  FSource.Rewind(PathID);
end;

function TAggConvTransform.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  Cmd := FSource.Vertex(X, Y);

  if IsVertex(Cmd) then
    FTrans.Transform(FTrans, X, Y);

  Result := Cmd;
end;

end.
