unit Agg2DGraphics;

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
  Graphics,
  AggBasics,
  Agg2D;

type
  TAgg2DGraphics = class(TAgg2D)
  public
    procedure Attach(Bitmap: TBitmap); overload;
  end;

implementation

{ TAgg2DGraphics }

procedure TAgg2DGraphics.Attach(Bitmap: TBitmap);
begin
  Bitmap.PixelFormat := TPixelFormat.pf32bit;
  Attach(Bitmap.ScanLine[Bitmap.Height - 1], Bitmap.Width, Bitmap.Height,
    -Bitmap.Width * 4);
end;

end.

