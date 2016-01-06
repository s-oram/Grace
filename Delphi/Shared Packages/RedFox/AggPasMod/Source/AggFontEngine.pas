unit AggFontEngine;

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
  AggScanLineStorageAA,
  AggScanLineStorageBin,
  AggPathStorageInteger;

type
  TAggGlyphData = (gdInvalid, gdMono, gdGray8, gdOutline);

  TAggGray8Adaptor = TAggSerializedScanLinesAdaptorAA8;
  TAggGray8ScanLine = TAggEmbeddedScanLineSA;
  TAggMonoAdaptor = TAggSerializedScanLinesAdaptorBin;
  TAggMonoScanLine = TAggEmbeddedScanLineA;
  TAggScanLinesAA = TAggScanLineStorageAA8;
  TAggScanLinesBin = TAggScanLineStorageBin;

  TAggCustomFontEngine = class
  protected
    function GetGlyphIndex: Cardinal; virtual; abstract;
    function GetDataSize: Cardinal; virtual; abstract;
    function GetDataType: TAggGlyphData; virtual; abstract;
    function GetAdvanceX: Double; virtual; abstract;
    function GetAdvanceY: Double; virtual; abstract;
    function GetFlag32: Boolean; virtual; abstract;
  public
    // Interface mandatory to implement for TAggFontCacheManager
    function ChangeStamp: Integer; virtual; abstract;

    function PrepareGlyph(GlyphCode: Cardinal): Boolean; virtual; abstract;

    procedure WriteGlyphTo(Data: PInt8u); virtual; abstract;
    function AddKerning(First, Second: Cardinal; X, Y: PDouble): Boolean;
      virtual; abstract;

    function GetFontSignature: AnsiString; virtual; abstract;
    function GetBounds: PRectInteger; virtual; abstract;

    property Flag32: Boolean read GetFlag32;
    property GlyphIndex: Cardinal read GetGlyphIndex;
    property DataSize: Cardinal read GetDataSize;
    property DataType: TAggGlyphData read GetDataType;
    property AdvanceX: Double read GetAdvanceX;
    property AdvanceY: Double read GetAdvanceY;
  end;

implementation

end.
