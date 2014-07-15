unit AggConvConcat;

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
// Concatenation of two paths. Usually used to combine lines or curves        //
// with markers such as arrowheads                                            //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  AggBasics,
  AggVertexSource;

type
  TAggConvConcat = class(TAggVertexSource)
  private
    FSource1, FSource2: TAggVertexSource;
    FStatus: Integer;
    procedure SetSource1(Source: TAggVertexSource);
    procedure SetSource2(Source: TAggVertexSource);
  public
    constructor Create(Source1, Source2: TAggVertexSource);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Source1: TAggVertexSource read FSource1 write SetSource1;
    property Source2: TAggVertexSource read FSource2 write SetSource2;
  end;

implementation


{ TAggConvConcat }

constructor TAggConvConcat.Create(Source1, Source2: TAggVertexSource);
begin
  FSource1 := Source1;
  FSource2 := Source2;
  FStatus := 2;
end;

procedure TAggConvConcat.SetSource1(Source: TAggVertexSource);
begin
  FSource1 := Source;
end;

procedure TAggConvConcat.SetSource2(Source: TAggVertexSource);
begin
  FSource2 := Source;
end;

procedure TAggConvConcat.Rewind(PathID: Cardinal);
begin
  FSource1.Rewind(PathID);
  FSource2.Rewind(0);

  FStatus := 0;
end;

function TAggConvConcat.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  if FStatus = 0 then
  begin
    Cmd := FSource1.Vertex(X, Y);

    if not IsStop(Cmd) then
    begin
      Result := Cmd;

      Exit;
    end;

    FStatus := 1;
  end;

  if FStatus = 1 then
  begin
    Cmd := FSource2.Vertex(X, Y);

    if not IsStop(Cmd) then
    begin
      Result := Cmd;

      Exit;
    end;

    FStatus := 2;
  end;

  Result := CAggPathCmdStop;
end;

end.
