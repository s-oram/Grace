unit AggRenderScanLines;

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
{$Q-}
{$R-}

uses
  AggBasics,
  AggColor,
  AggRasterizerScanLine,
  AggScanLine,
  AggRendererScanLine,
  AggVertexSource;

procedure RenderScanLines(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine);
procedure RenderAllPaths(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine;
  R: TAggCustomRendererScanLineSolid; Vs: TAggVertexSource; Cs: PAggColor;
  PathID: PCardinal; PathCount: Cardinal);

implementation

procedure RenderScanLines(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine;
  Ren: TAggCustomRendererScanLine);
begin
  if Ras.RewindScanLines then
  begin
    Sl.Reset(Ras.MinimumX, Ras.MaximumX);
    Ren.Prepare(Cardinal(Ras.MaximumX - Ras.MinimumX + 2));

    if Sl.IsEmbedded then
      while Ras.SweepScanLineEm(Sl) do
        Ren.Render(Sl)
    else
      while Ras.SweepScanLine(Sl) do
        Ren.Render(Sl);
  end;
end;

procedure RenderAllPaths(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine;
  R: TAggCustomRendererScanLineSolid; Vs: TAggVertexSource; Cs: PAggColor;
  PathID: PCardinal; PathCount: Cardinal);
var
  I: Cardinal;
begin
  I := 0;

  while I < PathCount do
  begin
    Ras.Reset;
    Ras.AddPath(Vs, PathID^);
    R.SetColor(Cs);

    RenderScanLines(Ras, Sl, R);

    Inc(PtrComp(Cs), SizeOf(TAggColor));
    Inc(PtrComp(PathID), SizeOf(Cardinal));
    Inc(I);
  end;
end;

end.
