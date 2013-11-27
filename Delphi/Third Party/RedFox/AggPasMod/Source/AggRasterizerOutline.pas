unit AggRasterizerOutline;

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
  AggRendererPrimitives,
  AggVertexSource;

type
  TAggRasterizerOutline = class
  private
    FRendererPrimitives: TAggRendererPrimitives;
    FStartX, FStartY: Integer;
    FVertices: Cardinal;
  public
    constructor Create(Ren: TAggRendererPrimitives);

    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);

    procedure MoveToDouble(X, Y: Double);
    procedure LineToDouble(X, Y: Double);
    procedure Close;

    procedure AddVertex(X, Y: Double; Cmd: Cardinal);
    procedure AddPath(Vs: TAggVertexSource; PathID: Cardinal = 0);
  end;

implementation


{ TAggRasterizerOutline }

constructor TAggRasterizerOutline.Create(Ren: TAggRendererPrimitives);
begin
  FRendererPrimitives := Ren;

  FStartX := 0;
  FStartY := 0;

  FVertices := 0;
end;

procedure TAggRasterizerOutline.MoveTo(X, Y: Integer);
begin
  FVertices := 1;

  FStartX := X;
  FStartY := Y;

  FRendererPrimitives.MoveTo(X, Y);
end;

procedure TAggRasterizerOutline.LineTo(X, Y: Integer);
begin
  Inc(FVertices);

  FRendererPrimitives.LineTo(X, Y);
end;

procedure TAggRasterizerOutline.MoveToDouble(X, Y: Double);
begin
  MoveTo(FRendererPrimitives.Coord(X), FRendererPrimitives.Coord(Y));
end;

procedure TAggRasterizerOutline.LineToDouble(X, Y: Double);
begin
  LineTo(FRendererPrimitives.Coord(X), FRendererPrimitives.Coord(Y));
end;

procedure TAggRasterizerOutline.Close;
begin
  if FVertices > 2 then
    LineTo(FStartX, FStartY);

  FVertices := 0;
end;

procedure TAggRasterizerOutline.AddVertex(X, Y: Double; Cmd: Cardinal);
begin
  if IsMoveTo(Cmd) then
    MoveToDouble(X, Y)
  else if IsEndPoly(Cmd) then
    if IsClosed(Cmd) then
      Close
    else
  else
    LineToDouble(X, Y);
end;

procedure TAggRasterizerOutline.AddPath(Vs: TAggVertexSource;
  PathID: Cardinal = 0);
var
  Cmd : Cardinal;
  X, Y: Double;
begin
  Vs.Rewind(PathID);

  Cmd := Vs.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    AddVertex(X, Y, Cmd);

    Cmd := Vs.Vertex(@X, @Y);
  end;
end;

end.
