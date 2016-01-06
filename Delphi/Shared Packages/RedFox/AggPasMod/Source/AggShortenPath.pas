unit AggShortenPath;

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

interface

{$I AggCompiler.inc}

uses
  AggBasics,
  AggVertexSequence;

procedure ShortenPath(VertexSequence: TAggVertexSequence; S: Double;
  Closed: Cardinal = 0);

implementation

procedure ShortenPath(VertexSequence: TAggVertexSequence; S: Double;
  Closed: Cardinal = 0);
var
  N: Integer;
  D, X, Y: Double;
  Prev, Last: PAggVertexDistance;
begin
  if (S > 0.0) and (VertexSequence.Size > 1) then
  begin
    N := VertexSequence.Size - 2;

    while N <> 0 do
    begin
      D := PAggVertexDistance(VertexSequence[N])^.Dist;

      if D > S then
        Break;

      VertexSequence.RemoveLast;

      S := S - D;

      Dec(N);
    end;

    if VertexSequence.Size < 2 then
      VertexSequence.RemoveAll

    else
    begin
      N := VertexSequence.Size - 1;

      Prev := VertexSequence[N - 1];
      Last := VertexSequence[N];

      D := (Prev.Dist - S) / Prev.Dist;

      X := Prev.Pos.X + (Last.Pos.X - Prev.Pos.X) * D;
      Y := Prev.Pos.Y + (Last.Pos.Y - Prev.Pos.Y) * D;
      Last.Pos := PointDouble(X, Y);

      if not VertexSequence.FuncOperatorVertexSequence(Prev, Last) then
        VertexSequence.RemoveLast;

      VertexSequence.Close(Boolean(Closed <> 0));
    end;
  end;
end;

end.
