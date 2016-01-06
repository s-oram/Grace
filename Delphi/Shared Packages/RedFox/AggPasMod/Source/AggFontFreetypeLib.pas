unit AggFontFreeTypeLib;

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
  AggBasics;

type
  TAggFreeTypeEncoding = array [0..3] of AnsiChar;

const
{$IFDEF AGG_WINDOWS }
  CAggFreeTypeLibrary = 'freetype.dll';
{$ENDIF}

{$IFDEF AGG_LINUX }
  CAggFreeTypeLibrary = 'freetype.so';
{$ENDIF}

{$IFDEF AGG_MACOSX }
  CAggFreeTypeLibrary = 'libfreetype';
{$ENDIF}

  CAggFreeTypeCurveTagOn = 1;
  CAggFreeTypeCurveTagConic = 0;
  CAggFreeTypeCurveTagCubic = 2;

  CAggFreeTypeFaceFlagScalable = 1 shl 0;
  CAggFreeTypeFaceFlagKerning = 1 shl 6;

  CAggFreeTypeEncodingNone: TAggFreeTypeEncoding = (#0, #0, #0, #0);

  CAggFreeTypeLoadDefault = $0000;
  CAggFreeTypeLoadNoHinting = $0002;
  CAggFreeTypeLoadForceAutohint = $0020;

  CAggFreeTypeRenderModeNormal = 0;
  CAggFreeTypeRenderModeLight = CAggFreeTypeRenderModeNormal + 1;
  CAggFreeTypeRenderModeMono = CAggFreeTypeRenderModeLight + 1;
  CAggFreeTypeRenderModeLcd = CAggFreeTypeRenderModeMono + 1;
  CAggFreeTypeRenderModeLcdVertical = CAggFreeTypeRenderModeLcd + 1;
  CAggFreeTypeRenderModeMax = CAggFreeTypeRenderModeLcdVertical + 1;

  CAggFreeTypeKerningDefault = 0;
  CAggFreeTypeKerningUnfitted = 1;
  CAggFreeTypeKerningUnscaled = 2;

  CAggFreeTypeStyleFlagItalic = 1 shl 0;
  CAggFreeTypeStyleFlagBold = 1 shl 1;

  
type
  TAggFreeTypeByte = Byte;
  TAggFreeTypeShort = Smallint;
  TAggFreeTypeUShort = Word;
  TAggFreeTypeInt = Longint;
  TAggFreeTypeUInt = Longword;
  TAggFreeTypeInt32 = Longint;
  TAggFreeTypeLong = Longint;
  TAggFreeTypeULong = Longword;
  TAggFreeTypeFixed = Longint;
  TAggFreeTypePos = Longint;
  TAggFreeTypeError = Longint;
  TAggFreeTypeFixed26Dot6 = Longint;

  PAggFreeTypeByte = ^TAggFreeTypeByte;
  PAggFreeTypeShort = ^TAggFreeTypeShort;

  TAggFreeTypeRenderMode = TAggFreeTypeInt;

  PPAggFreeTypeLibrary = ^PAggFreeTypeLibrary;
  PAggFreeTypeLibrary = ^TAggFreeTypeLibrary;
  TAggFreeTypeLibrary = packed record
  end;

  PAggFreeTypeSubglyph = ^TAggFreeTypeSubglyph;
  TAggFreeTypeSubglyph = packed record // TODO
  end;

  TAggFreeTypeBitmapSize = record
    Height, Width: TAggFreeTypeShort;
  end;

  TAggFreeTypeBitmapSizeArray = array [0..1023] of TAggFreeTypeBitmapSize;
  PAggFreeTypeBitmapSize = ^TAggFreeTypeBitmapSizeArray;
  PAggFreeTypeCharmap = ^TAggFreeTypeCharmap;
  PPAggFreeTypeCharmap = ^PAggFreeTypeCharmap;

  TAggFreeTypeGenericFinalizer = procedure(AnObject: Pointer); cdecl;

  TAggFreeTypeGeneric = packed record
    Data: Pointer;
    Finalizer: TAggFreeTypeGenericFinalizer;
  end;

  PAggFreeTypeBBox = ^TAggFreeTypeBBox;
  TAggFreeTypeBBox = packed record
    XMin, YMin, XMax, YMax: TAggFreeTypePos;
  end;

  PAggFreeTypeVector = ^TAggFreeTypeVector;
  TAggFreeTypeVector = packed record
    X, Y: TAggFreeTypePos;
  end;

  PAggFreeTypeBitmap = ^TAggFreeTypeBitmap;
  TAggFreeTypeBitmap = packed record
    Rows, Width, Pitch: TAggFreeTypeInt;

    Buffer: Pointer;

    Num_grays: TAggFreeTypeShort;
    PixelMode, PaletteMode: AnsiChar;

    Palette: Pointer;
  end;

  PAggFreeTypeOutline = ^TAggFreeTypeOutline;
  TAggFreeTypeOutline = packed record
    NumContours, NumPoints: TAggFreeTypeShort;

    Points: PAggFreeTypeVector;
    Tags: PAnsiChar;

    Contours: PAggFreeTypeShort;
    Flags: TAggFreeTypeInt;
  end;

  TAggFreeTypeGlyphMetrics = packed record
    Width, Height, HoriBearingX, HoriBearingY, HoriAdvance: TAggFreeTypePos;
    VertBearingX, VertBearingY, VertAdvance: TAggFreeTypePos;
  end;

  PPAggFreeTypeFace = ^PAggFreeTypeFace;
  PAggFreeTypeFace = ^TAggFreeTypeFace;

  PAggFreeTypeGlyphSlot = ^TAggFreeTypeGlyphSlot;
  TAggFreeTypeGlyphSlot = packed record
    ALibrary: PAggFreeTypeLibrary;

    Face: PAggFreeTypeFace;
    Next: PAggFreeTypeGlyphSlot;
    Flags: TAggFreeTypeUInt;

    Generic: TAggFreeTypeGeneric;
    Metrics: TAggFreeTypeGlyphMetrics;

    LinearHoriAdvance, LinearVertAdvance: TAggFreeTypeFixed;

    Advance: TAggFreeTypeVector;
    Format: Longword;
    Bitmap: TAggFreeTypeBitmap;

    BitmapLeft, BitmapTop: TAggFreeTypeInt;

    Outline: TAggFreeTypeOutline;

    NumSubglyphs: TAggFreeTypeUInt;
    Subglyphs: PAggFreeTypeSubglyph;
    ControlData: Pointer;
    ControlLen: Longint;

    Other: Pointer;
  end;

  TAggFreeTypeSizeMetrics = record
    X_ppem, Y_ppem: TAggFreeTypeUShort;
    X_scale, Y_scale: TAggFreeTypeFixed;

    Ascender, Descender, Height, MaxAdvance: TAggFreeTypePos;
  end;

  PAggFreeTypeSize = ^TAggFreeTypeSize;
  TAggFreeTypeSize = record
    Face: PAggFreeTypeFace;
    Generic: TAggFreeTypeGeneric;
    Metrics: TAggFreeTypeSizeMetrics;
    // internal : FT_Size_Internal;
  end;

  TAggFreeTypeFace = packed record
    NumFaces, FaceIndex, FaceFlags, StyleFlags, NumGlyphs: TAggFreeTypeLong;
    FamilyName, StyleName: PAnsiChar;

    NumFixedSizes: TAggFreeTypeInt;
    AvailableSizes: PAggFreeTypeBitmapSize; // is array

    NumCharmaps: TAggFreeTypeInt;
    Charmaps: PPAggFreeTypeCharmap; // is array

    Generic: TAggFreeTypeGeneric;
    Bbox: TAggFreeTypeBBox;

    UnitsPerEM: TAggFreeTypeUShort;

    Ascender, Descender, Height,

      MaxAdvanceWidth, MaxAdvanceHeight, UnderlinePosition,
      UnderlineThickness: TAggFreeTypeShort;

    Glyph: PAggFreeTypeGlyphSlot;
    Size: PAggFreeTypeSize;
    Charmap: PAggFreeTypeCharmap;
  end;

  TAggFreeTypeCharmap = packed record
    Face: PAggFreeTypeFace;
    Encoding: TAggFreeTypeEncoding;

    PlatformID, EncodingID: TAggFreeTypeUShort;
  end;

function FreeTypeCurveTag(Flag: AnsiChar): AnsiChar;
function FreeTypeIsScalable(Face: PAggFreeTypeFace): Boolean;
function FreeTypeHasKerning(Face: PAggFreeTypeFace): Boolean;

function FreeTypeInit(ALibrary: PAggFreeTypeLibrary): TAggFreeTypeError; cdecl;
  external CAggFreeTypeLibrary name 'FT_Init_FreeType';

function FreeTypeDone(ALibrary: PAggFreeTypeLibrary): TAggFreeTypeError; cdecl;
  external CAggFreeTypeLibrary name 'FT_Done_FreeType';

function FreeTypeAttachFile(Face: PAggFreeTypeFace; Filepathname: PAnsiChar): TAggFreeTypeError;
  cdecl; external CAggFreeTypeLibrary name 'FT_Attach_File';

function FreeTypeNewMemoryFace(Library_: PAggFreeTypeLibrary; File_base: PAggFreeTypeByte;
  File_size, FaceIndex: TAggFreeTypeLong; var Aface: PAggFreeTypeFace): TAggFreeTypeError; cdecl;
  external CAggFreeTypeLibrary name 'FT_New_Memory_Face';

function FreeTypeNewFace(Library_: PAggFreeTypeLibrary; Filepathname: PAnsiChar;
  FaceIndex: TAggFreeTypeLong; var Aface: PAggFreeTypeFace): TAggFreeTypeError; cdecl;
  external CAggFreeTypeLibrary name 'FT_New_Face';

function FreeTypeDoneFace(Face: PAggFreeTypeFace): TAggFreeTypeError; cdecl;
  external CAggFreeTypeLibrary name 'FT_Done_Face';

function FreeTypeSelectCharmap(Face: PAggFreeTypeFace; Encoding: TAggFreeTypeEncoding): TAggFreeTypeError;
  cdecl; external CAggFreeTypeLibrary name 'FT_Select_Charmap';

function FreeTypeGetCharIndex(Face: PAggFreeTypeFace; Charcode: TAggFreeTypeULong): TAggFreeTypeUInt;
  cdecl; external CAggFreeTypeLibrary name 'FT_Get_Char_Index';

function FreeTypeLoadGlyph(Face: PAggFreeTypeFace; GlyphIndex: TAggFreeTypeUInt;
  Load_flags: TAggFreeTypeInt32): TAggFreeTypeError; cdecl; external CAggFreeTypeLibrary name 'FT_Load_Glyph';

function FreeTypeRenderGlyph(Slot: PAggFreeTypeGlyphSlot; Render_mode: TAggFreeTypeRenderMode)
  : TAggFreeTypeError; cdecl; external CAggFreeTypeLibrary name 'FT_Render_Glyph';

function FreeTypeGetKerning(Face: PAggFreeTypeFace;
  Left_glyph, Right_glyph, Kern_mode: TAggFreeTypeUInt; Akerning: PAggFreeTypeVector)
  : TAggFreeTypeError; cdecl; external CAggFreeTypeLibrary name 'FT_Get_Kerning';

function FreeTypeSetCharSize(Face: PAggFreeTypeFace;
  Char_width, Char_height: TAggFreeTypeFixed26Dot6; Horz_res, Vert_res: TAggFreeTypeUInt): TAggFreeTypeError;
  cdecl; external CAggFreeTypeLibrary name 'FT_Set_Char_Size';

function FreeTypeSetPixelSizes(Face: PAggFreeTypeFace;
  Pixel_width, Pixel_height: TAggFreeTypeUInt): TAggFreeTypeError; cdecl;
  external CAggFreeTypeLibrary name 'FT_Set_Pixel_Sizes';

implementation



function FreeTypeCurveTag(Flag: AnsiChar): AnsiChar;
begin
  Result := AnsiChar(Int8u(Flag) and 3);
end;

function FreeTypeIsScalable(Face: PAggFreeTypeFace): Boolean;
begin
  Result := Boolean(Face.FaceFlags and CAggFreeTypeFaceFlagScalable);
end;

function FreeTypeHasKerning(Face: PAggFreeTypeFace): Boolean;
begin
  Result := Boolean(Face.FaceFlags and CAggFreeTypeFaceFlagKerning);
end;

end.
