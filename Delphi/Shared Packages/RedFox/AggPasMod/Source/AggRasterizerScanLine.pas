unit AggRasterizerScanLine;

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

uses
  AggBasics,
  AggScanLine,
  AggVertexSource;

type
  TAggCustomRasterizerScanLine = class
  protected
    function GetFillingRule: TAggFillingRule; virtual; abstract;
    procedure SetFillingRule(FillingRule: TAggFillingRule); virtual; abstract;
  public
    procedure Reset; virtual; abstract;
    procedure SetClipBox(X1, Y1, X2, Y2: Double); overload; virtual; abstract;
    procedure SetClipBox(Rect: TRectDouble); overload; virtual; abstract;

    procedure AddPath(Vs: TAggCustomVertexSource; PathID: Cardinal = 0);
      virtual; abstract;
    procedure AddVertex(X, Y: Double; Cmd: Cardinal); virtual; abstract;

    procedure Sort; virtual; abstract;
    function RewindScanLines: Boolean; virtual; abstract;

    function HitTest(Tx, Ty: Integer): Boolean; virtual; abstract;

    property FillingRule: TAggFillingRule read GetFillingRule write SetFillingRule;
  end;

  TAggCustomBoundsRasterizerScanLine = class(TAggCustomRasterizerScanLine)
  protected
    function GetMinX: Integer; virtual; abstract;
    function GetMinY: Integer; virtual; abstract;
    function GetMaxX: Integer; virtual; abstract;
    function GetMaxY: Integer; virtual; abstract;
  public
    property MinimumX: Integer read GetMinX;
    property MinimumY: Integer read GetMinY;
    property MaximumX: Integer read GetMaxX;
    property MaximumY: Integer read GetMaxY;
  end;

  IAggRasterizerScanline = interface
    procedure Gamma(AGammaFunction: TAggCustomVertexSource);
    function SweepScanLine(Sl: TAggCustomScanLine): Boolean;
    function SweepScanLineEm(Sl: TAggCustomScanLine): Boolean;
  end;

  TAggRasterizerScanline = class(TAggCustomBoundsRasterizerScanLine)
  public
    procedure Gamma(AGammaFunction: TAggCustomVertexSource); virtual; abstract;
    function SweepScanLine(Sl: TAggCustomScanLine): Boolean; virtual; abstract;
    function SweepScanLineEm(Sl: TAggCustomScanLine): Boolean; virtual; abstract;
  end;

implementation

end.

