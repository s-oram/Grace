unit AggGsvText;

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
{$Q-}
{$R-}

uses
  SysUtils,
  AggBasics,
  AggVertexSource,
  AggConvStroke,
  AggConvTransform,
  AggTransAffine,
  AggMathStroke;

type
  TAggInternalStatus = (isInitial, isNextChar, isStartGlyph, isGlyph);

  TAggGsvText = class(TAggCustomVertexSource)
  private
    FX, FY, FStartX, FWidth, FHeight, FSpace, FLineSpace: Double;
    FCharacters: array [0..1] of Byte;
    FText, FTextBuffer: Pointer;
    FBufferSize: Cardinal;
    FCurrentChr: PInt8u;
    FFont, FLoadedFont: Pointer;
    FLoadFontSize: Cardinal;
    FStatus: TAggInternalStatus;
    FBigEndian, FFlip: Boolean;

    FIndices: PInt8u;
    FGlyphs, FGlyphB, FGlyphE: PInt8;
    FW, FH: Double;

    procedure SetFlip(Value: Boolean);
    procedure SetSpace(Value: Double);
    procedure SetLineSpace(Value: Double);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetFont(Font: Pointer);
    procedure SetLoadFont(FileName: ShortString);
    procedure SetSize(Height: Double; Width: Double = 0.0);
    procedure SetStartPoint(X, Y: Double); overload;
    procedure SetStartPoint(Point: TPointDouble); overload;
    procedure SetText(Text: AnsiString);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    function GetValue(P: PInt8u): Int16u;

    property Flip: Boolean read FFlip write SetFlip;
    property Space: Double read FSpace write SetSpace;
    property LineSpace: Double read FLineSpace write SetLineSpace;
  end;

  TAggGsvTextOutline = class(TAggVertexSource)
  private
    FPolyline: TAggConvStroke;
    FTrans: TAggConvTransform;
    procedure SetWidth(Value: Double);
    procedure SetTransformer(Trans: TAggTransAffine);
    function GetTransformer: TAggTransAffine;
    function GetWidth: Double;
  public
    constructor Create(Text: TAggGsvText; Trans: TAggTransAffine);
    destructor Destroy; override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    property Width: Double read GetWidth write SetWidth;
    property Transformer: TAggTransAffine read GetTransformer write SetTransformer;
  end;

implementation

const
  CAggGsvDefaultFont: array [0..4525] of Int8u = ($40, $00, $6C, $0F, $15, $00,
    $0E, $00, $F9, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $0D,
    $0A, $0D, $0A, $46, $6F, $6E, $74, $20, $28, $63, $29, $20, $4D, $69, $63,
    $72, $6F, $50, $72, $6F, $66, $20, $32, $37, $20, $53, $65, $70, $74, $65,
    $6D, $62, $2E, $31, $39, $38, $39, $00, $0D, $0A, $0D, $0A, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $02, $00, $12, $00, $34, $00, $46, $00, $94, $00, $D0,
    $00, $2E, $01, $3E, $01, $64, $01, $8A, $01, $98, $01, $A2, $01, $B4, $01,
    $BA, $01, $C6, $01, $CC, $01, $F0, $01, $FA, $01, $18, $02, $38, $02, $44,
    $02, $68, $02, $98, $02, $A2, $02, $DE, $02, $0E, $03, $24, $03, $40, $03,
    $48, $03, $52, $03, $5A, $03, $82, $03, $EC, $03, $FA, $03, $26, $04, $4C,
    $04, $6A, $04, $7C, $04, $8A, $04, $B6, $04, $C4, $04, $CA, $04, $E0, $04,
    $EE, $04, $F8, $04, $0A, $05, $18, $05, $44, $05, $5E, $05, $8E, $05, $AC,
    $05, $D6, $05, $E0, $05, $F6, $05, $00, $06, $12, $06, $1C, $06, $28, $06,
    $36, $06, $48, $06, $4E, $06, $60, $06, $6E, $06, $74, $06, $84, $06, $A6,
    $06, $C8, $06, $E6, $06, $08, $07, $2C, $07, $3C, $07, $68, $07, $7C, $07,
    $8C, $07, $A2, $07, $B0, $07, $B6, $07, $D8, $07, $EC, $07, $10, $08, $32,
    $08, $54, $08, $64, $08, $88, $08, $98, $08, $AC, $08, $B6, $08, $C8, $08,
    $D2, $08, $E4, $08, $F2, $08, $3E, $09, $48, $09, $94, $09, $C2, $09, $C4,
    $09, $D0, $09, $E2, $09, $04, $0A, $0E, $0A, $26, $0A, $34, $0A, $4A, $0A,
    $66, $0A, $70, $0A, $7E, $0A, $8E, $0A, $9A, $0A, $A6, $0A, $B4, $0A, $D8,
    $0A, $E2, $0A, $F6, $0A, $18, $0B, $22, $0B, $32, $0B, $56, $0B, $60, $0B,
    $6E, $0B, $7C, $0B, $8A, $0B, $9C, $0B, $9E, $0B, $B2, $0B, $C2, $0B, $D8,
    $0B, $F4, $0B, $08, $0C, $30, $0C, $56, $0C, $72, $0C, $90, $0C, $B2, $0C,
    $CE, $0C, $E2, $0C, $FE, $0C, $10, $0D, $26, $0D, $36, $0D, $42, $0D, $4E,
    $0D, $5C, $0D, $78, $0D, $8C, $0D, $8E, $0D, $90, $0D, $92, $0D, $94, $0D,
    $96, $0D, $98, $0D, $9A, $0D, $9C, $0D, $9E, $0D, $A0, $0D, $A2, $0D, $A4,
    $0D, $A6, $0D, $A8, $0D, $AA, $0D, $AC, $0D, $AE, $0D, $B0, $0D, $B2, $0D,
    $B4, $0D, $B6, $0D, $B8, $0D, $BA, $0D, $BC, $0D, $BE, $0D, $C0, $0D, $C2,
    $0D, $C4, $0D, $C6, $0D, $C8, $0D, $CA, $0D, $CC, $0D, $CE, $0D, $D0, $0D,
    $D2, $0D, $D4, $0D, $D6, $0D, $D8, $0D, $DA, $0D, $DC, $0D, $DE, $0D, $E0,
    $0D, $E2, $0D, $E4, $0D, $E6, $0D, $E8, $0D, $EA, $0D, $EC, $0D, $0C, $0E,
    $26, $0E, $48, $0E, $64, $0E, $88, $0E, $92, $0E, $A6, $0E, $B4, $0E, $D0,
    $0E, $EE, $0E, $02, $0F, $16, $0F, $26, $0F, $3C, $0F, $58, $0F, $6C, $0F,
    $6C, $0F, $6C, $0F, $6C, $0F, $6C, $0F, $6C, $0F, $6C, $0F, $6C, $0F, $6C,
    $0F, $6C, $0F, $6C, $0F, $6C, $0F, $6C, $0F, $6C, $0F, $6C, $0F, $6C, $0F,
    $6C, $0F, $10, $80, $05, $95, $00, $72, $00, $FB, $FF, $7F, $01, $7F, $01,
    $01, $FF, $01, $05, $FE, $05, $95, $FF, $7F, $00, $7A, $01, $86, $FF, $7A,
    $01, $87, $01, $7F, $FE, $7A, $0A, $87, $FF, $7F, $00, $7A, $01, $86, $FF,
    $7A, $01, $87, $01, $7F, $FE, $7A, $05, $F2, $0B, $95, $F9, $64, $0D, $9C,
    $F9, $64, $FA, $91, $0E, $00, $F1, $FA, $0E, $00, $04, $FC, $08, $99, $00,
    $63, $04, $9D, $00, $63, $04, $96, $FF, $7F, $01, $7F, $01, $01, $00, $01,
    $FE, $02, $FD, $01, $FC, $00, $FD, $7F, $FE, $7E, $00, $7E, $01, $7E, $01,
    $7F, $02, $7F, $06, $7E, $02, $7F, $02, $7E, $F2, $89, $02, $7E, $02, $7F,
    $06, $7E, $02, $7F, $01, $7F, $01, $7E, $00, $7C, $FE, $7E, $FD, $7F, $FC,
    $00, $FD, $01, $FE, $02, $00, $01, $01, $01, $01, $7F, $FF, $7F, $10, $FD,
    $15, $95, $EE, $6B, $05, $95, $02, $7E, $00, $7E, $FF, $7E, $FE, $7F, $FE,
    $00, $FE, $02, $00, $02, $01, $02, $02, $01, $02, $00, $02, $7F, $03, $7F,
    $03, $00, $03, $01, $02, $01, $FC, $F2, $FE, $7F, $FF, $7E, $00, $7E, $02,
    $7E, $02, $00, $02, $01, $01, $02, $00, $02, $FE, $02, $FE, $00, $07, $F9,
    $15, $8D, $FF, $7F, $01, $7F, $01, $01, $00, $01, $FF, $01, $FF, $00, $FF,
    $7F, $FF, $7E, $FE, $7B, $FE, $7D, $FE, $7E, $FE, $7F, $FD, $00, $FD, $01,
    $FF, $02, $00, $03, $01, $02, $06, $04, $02, $02, $01, $02, $00, $02, $FF,
    $02, $FE, $01, $FE, $7F, $FF, $7E, $00, $7E, $01, $7D, $02, $7D, $05, $79,
    $02, $7E, $03, $7F, $01, $00, $01, $01, $00, $01, $F1, $FE, $FE, $01, $FF,
    $02, $00, $03, $01, $02, $02, $02, $00, $86, $01, $7E, $08, $75, $02, $7E,
    $02, $7F, $05, $80, $05, $93, $FF, $01, $01, $01, $01, $7F, $00, $7E, $FF,
    $7E, $FF, $7F, $06, $F1, $0B, $99, $FE, $7E, $FE, $7D, $FE, $7C, $FF, $7B,
    $00, $7C, $01, $7B, $02, $7C, $02, $7D, $02, $7E, $FE, $9E, $FE, $7C, $FF,
    $7D, $FF, $7B, $00, $7C, $01, $7B, $01, $7D, $02, $7C, $05, $85, $03, $99,
    $02, $7E, $02, $7D, $02, $7C, $01, $7B, $00, $7C, $FF, $7B, $FE, $7C, $FE,
    $7D, $FE, $7E, $02, $9E, $02, $7C, $01, $7D, $01, $7B, $00, $7C, $FF, $7B,
    $FF, $7D, $FE, $7C, $09, $85, $08, $95, $00, $74, $FB, $89, $0A, $7A, $00,
    $86, $F6, $7A, $0D, $F4, $0D, $92, $00, $6E, $F7, $89, $12, $00, $04, $F7,
    $06, $81, $FF, $7F, $FF, $01, $01, $01, $01, $7F, $00, $7E, $FF, $7E, $FF,
    $7F, $06, $84, $04, $89, $12, $00, $04, $F7, $05, $82, $FF, $7F, $01, $7F,
    $01, $01, $FF, $01, $05, $FE, $00, $FD, $0E, $18, $00, $EB, $09, $95, $FD,
    $7F, $FE, $7D, $FF, $7B, $00, $7D, $01, $7B, $02, $7D, $03, $7F, $02, $00,
    $03, $01, $02, $03, $01, $05, $00, $03, $FF, $05, $FE, $03, $FD, $01, $FE,
    $00, $0B, $EB, $06, $91, $02, $01, $03, $03, $00, $6B, $09, $80, $04, $90,
    $00, $01, $01, $02, $01, $01, $02, $01, $04, $00, $02, $7F, $01, $7F, $01,
    $7E, $00, $7E, $FF, $7E, $FE, $7D, $F6, $76, $0E, $00, $03, $80, $05, $95,
    $0B, $00, $FA, $78, $03, $00, $02, $7F, $01, $7F, $01, $7D, $00, $7E, $FF,
    $7D, $FE, $7E, $FD, $7F, $FD, $00, $FD, $01, $FF, $01, $FF, $02, $11, $FC,
    $0D, $95, $F6, $72, $0F, $00, $FB, $8E, $00, $6B, $07, $80, $0F, $95, $F6,
    $00, $FF, $77, $01, $01, $03, $01, $03, $00, $03, $7F, $02, $7E, $01, $7D,
    $00, $7E, $FF, $7D, $FE, $7E, $FD, $7F, $FD, $00, $FD, $01, $FF, $01, $FF,
    $02, $11, $FC, $10, $92, $FF, $02, $FD, $01, $FE, $00, $FD, $7F, $FE, $7D,
    $FF, $7B, $00, $7B, $01, $7C, $02, $7E, $03, $7F, $01, $00, $03, $01, $02,
    $02, $01, $03, $00, $01, $FF, $03, $FE, $02, $FD, $01, $FF, $00, $FD, $7F,
    $FE, $7E, $FF, $7D, $10, $F9, $11, $95, $F6, $6B, $FC, $95, $0E, $00, $03,
    $EB, $08, $95, $FD, $7F, $FF, $7E, $00, $7E, $01, $7E, $02, $7F, $04, $7F,
    $03, $7F, $02, $7E, $01, $7E, $00, $7D, $FF, $7E, $FF, $7F, $FD, $7F, $FC,
    $00, $FD, $01, $FF, $01, $FF, $02, $00, $03, $01, $02, $02, $02, $03, $01,
    $04, $01, $02, $01, $01, $02, $00, $02, $FF, $02, $FD, $01, $FC, $00, $0C,
    $EB, $10, $8E, $FF, $7D, $FE, $7E, $FD, $7F, $FF, $00, $FD, $01, $FE, $02,
    $FF, $03, $00, $01, $01, $03, $02, $02, $03, $01, $01, $00, $03, $7F, $02,
    $7E, $01, $7C, $00, $7B, $FF, $7B, $FE, $7D, $FD, $7F, $FE, $00, $FD, $01,
    $FF, $02, $10, $FD, $05, $8E, $FF, $7F, $01, $7F, $01, $01, $FF, $01, $00,
    $F4, $FF, $7F, $01, $7F, $01, $01, $FF, $01, $05, $FE, $05, $8E, $FF, $7F,
    $01, $7F, $01, $01, $FF, $01, $01, $F3, $FF, $7F, $FF, $01, $01, $01, $01,
    $7F, $00, $7E, $FF, $7E, $FF, $7F, $06, $84, $14, $92, $F0, $77, $10, $77,
    $04, $80, $04, $8C, $12, $00, $EE, $FA, $12, $00, $04, $FA, $04, $92, $10,
    $77, $F0, $77, $14, $80, $03, $90, $00, $01, $01, $02, $01, $01, $02, $01,
    $04, $00, $02, $7F, $01, $7F, $01, $7E, $00, $7E, $FF, $7E, $FF, $7F, $FC,
    $7E, $00, $7D, $00, $FB, $FF, $7F, $01, $7F, $01, $01, $FF, $01, $09, $FE,
    $12, $8D, $FF, $02, $FE, $01, $FD, $00, $FE, $7F, $FF, $7F, $FF, $7D, $00,
    $7D, $01, $7E, $02, $7F, $03, $00, $02, $01, $01, $02, $FB, $88, $FE, $7E,
    $FF, $7D, $00, $7D, $01, $7E, $01, $7F, $07, $8B, $FF, $78, $00, $7E, $02,
    $7F, $02, $00, $02, $02, $01, $03, $00, $02, $FF, $03, $FF, $02, $FE, $02,
    $FE, $01, $FD, $01, $FD, $00, $FD, $7F, $FE, $7F, $FE, $7E, $FF, $7E, $FF,
    $7D, $00, $7D, $01, $7D, $01, $7E, $02, $7E, $02, $7F, $03, $7F, $03, $00,
    $03, $01, $02, $01, $01, $01, $FE, $8D, $FF, $78, $00, $7E, $01, $7F, $08,
    $FB, $09, $95, $F8, $6B, $08, $95, $08, $6B, $F3, $87, $0A, $00, $04, $F9,
    $04, $95, $00, $6B, $00, $95, $09, $00, $03, $7F, $01, $7F, $01, $7E, $00,
    $7E, $FF, $7E, $FF, $7F, $FD, $7F, $F7, $80, $09, $00, $03, $7F, $01, $7F,
    $01, $7E, $00, $7D, $FF, $7E, $FF, $7F, $FD, $7F, $F7, $00, $11, $80, $12,
    $90, $FF, $02, $FE, $02, $FE, $01, $FC, $00, $FE, $7F, $FE, $7E, $FF, $7E,
    $FF, $7D, $00, $7B, $01, $7D, $01, $7E, $02, $7E, $02, $7F, $04, $00, $02,
    $01, $02, $02, $01, $02, $03, $FB, $04, $95, $00, $6B, $00, $95, $07, $00,
    $03, $7F, $02, $7E, $01, $7E, $01, $7D, $00, $7B, $FF, $7D, $FF, $7E, $FE,
    $7E, $FD, $7F, $F9, $00, $11, $80, $04, $95, $00, $6B, $00, $95, $0D, $00,
    $F3, $F6, $08, $00, $F8, $F5, $0D, $00, $02, $80, $04, $95, $00, $6B, $00,
    $95, $0D, $00, $F3, $F6, $08, $00, $06, $F5, $12, $90, $FF, $02, $FE, $02,
    $FE, $01, $FC, $00, $FE, $7F, $FE, $7E, $FF, $7E, $FF, $7D, $00, $7B, $01,
    $7D, $01, $7E, $02, $7E, $02, $7F, $04, $00, $02, $01, $02, $02, $01, $02,
    $00, $03, $FB, $80, $05, $00, $03, $F8, $04, $95, $00, $6B, $0E, $95, $00,
    $6B, $F2, $8B, $0E, $00, $04, $F5, $04, $95, $00, $6B, $04, $80, $0C, $95,
    $00, $70, $FF, $7D, $FF, $7F, $FE, $7F, $FE, $00, $FE, $01, $FF, $01, $FF,
    $03, $00, $02, $0E, $F9, $04, $95, $00, $6B, $0E, $95, $F2, $72, $05, $85,
    $09, $74, $03, $80, $04, $95, $00, $6B, $00, $80, $0C, $00, $01, $80, $04,
    $95, $00, $6B, $00, $95, $08, $6B, $08, $95, $F8, $6B, $08, $95, $00, $6B,
    $04, $80, $04, $95, $00, $6B, $00, $95, $0E, $6B, $00, $95, $00, $6B, $04,
    $80, $09, $95, $FE, $7F, $FE, $7E, $FF, $7E, $FF, $7D, $00, $7B, $01, $7D,
    $01, $7E, $02, $7E, $02, $7F, $04, $00, $02, $01, $02, $02, $01, $02, $01,
    $03, $00, $05, $FF, $03, $FF, $02, $FE, $02, $FE, $01, $FC, $00, $0D, $EB,
    $04, $95, $00, $6B, $00, $95, $09, $00, $03, $7F, $01, $7F, $01, $7E, $00,
    $7D, $FF, $7E, $FF, $7F, $FD, $7F, $F7, $00, $11, $F6, $09, $95, $FE, $7F,
    $FE, $7E, $FF, $7E, $FF, $7D, $00, $7B, $01, $7D, $01, $7E, $02, $7E, $02,
    $7F, $04, $00, $02, $01, $02, $02, $01, $02, $01, $03, $00, $05, $FF, $03,
    $FF, $02, $FE, $02, $FE, $01, $FC, $00, $03, $EF, $06, $7A, $04, $82, $04,
    $95, $00, $6B, $00, $95, $09, $00, $03, $7F, $01, $7F, $01, $7E, $00, $7E,
    $FF, $7E, $FF, $7F, $FD, $7F, $F7, $00, $07, $80, $07, $75, $03, $80, $11,
    $92, $FE, $02, $FD, $01, $FC, $00, $FD, $7F, $FE, $7E, $00, $7E, $01, $7E,
    $01, $7F, $02, $7F, $06, $7E, $02, $7F, $01, $7F, $01, $7E, $00, $7D, $FE,
    $7E, $FD, $7F, $FC, $00, $FD, $01, $FE, $02, $11, $FD, $08, $95, $00, $6B,
    $F9, $95, $0E, $00, $01, $EB, $04, $95, $00, $71, $01, $7D, $02, $7E, $03,
    $7F, $02, $00, $03, $01, $02, $02, $01, $03, $00, $0F, $04, $EB, $01, $95,
    $08, $6B, $08, $95, $F8, $6B, $09, $80, $02, $95, $05, $6B, $05, $95, $FB,
    $6B, $05, $95, $05, $6B, $05, $95, $FB, $6B, $07, $80, $03, $95, $0E, $6B,
    $00, $95, $F2, $6B, $11, $80, $01, $95, $08, $76, $00, $75, $08, $95, $F8,
    $76, $09, $F5, $11, $95, $F2, $6B, $00, $95, $0E, $00, $F2, $EB, $0E, $00,
    $03, $80, $03, $93, $00, $6C, $01, $94, $00, $6C, $FF, $94, $05, $00, $FB,
    $EC, $05, $00, $02, $81, $00, $95, $0E, $68, $00, $83, $06, $93, $00, $6C,
    $01, $94, $00, $6C, $FB, $94, $05, $00, $FB, $EC, $05, $00, $03, $81, $03,
    $87, $08, $05, $08, $7B, $F0, $80, $08, $04, $08, $7C, $03, $F9, $01, $80,
    $10, $00, $01, $80, $06, $95, $FF, $7F, $FF, $7E, $00, $7E, $01, $7F, $01,
    $01, $FF, $01, $05, $EF, $0F, $8E, $00, $72, $00, $8B, $FE, $02, $FE, $01,
    $FD, $00, $FE, $7F, $FE, $7E, $FF, $7D, $00, $7E, $01, $7D, $02, $7E, $02,
    $7F, $03, $00, $02, $01, $02, $02, $04, $FD, $04, $95, $00, $6B, $00, $8B,
    $02, $02, $02, $01, $03, $00, $02, $7F, $02, $7E, $01, $7D, $00, $7E, $FF,
    $7D, $FE, $7E, $FE, $7F, $FD, $00, $FE, $01, $FE, $02, $0F, $FD, $0F, $8B,
    $FE, $02, $FE, $01, $FD, $00, $FE, $7F, $FE, $7E, $FF, $7D, $00, $7E, $01,
    $7D, $02, $7E, $02, $7F, $03, $00, $02, $01, $02, $02, $03, $FD, $0F, $95,
    $00, $6B, $00, $8B, $FE, $02, $FE, $01, $FD, $00, $FE, $7F, $FE, $7E, $FF,
    $7D, $00, $7E, $01, $7D, $02, $7E, $02, $7F, $03, $00, $02, $01, $02, $02,
    $04, $FD, $03, $88, $0C, $00, $00, $02, $FF, $02, $FF, $01, $FE, $01, $FD,
    $00, $FE, $7F, $FE, $7E, $FF, $7D, $00, $7E, $01, $7D, $02, $7E, $02, $7F,
    $03, $00, $02, $01, $02, $02, $03, $FD, $0A, $95, $FE, $00, $FE, $7F, $FF,
    $7D, $00, $6F, $FD, $8E, $07, $00, $03, $F2, $0F, $8E, $00, $70, $FF, $7D,
    $FF, $7F, $FE, $7F, $FD, $00, $FE, $01, $09, $91, $FE, $02, $FE, $01, $FD,
    $00, $FE, $7F, $FE, $7E, $FF, $7D, $00, $7E, $01, $7D, $02, $7E, $02, $7F,
    $03, $00, $02, $01, $02, $02, $04, $FD, $04, $95, $00, $6B, $00, $8A, $03,
    $03, $02, $01, $03, $00, $02, $7F, $01, $7D, $00, $76, $04, $80, $03, $95,
    $01, $7F, $01, $01, $FF, $01, $FF, $7F, $01, $F9, $00, $72, $04, $80, $05,
    $95, $01, $7F, $01, $01, $FF, $01, $FF, $7F, $01, $F9, $00, $6F, $FF, $7D,
    $FE, $7F, $FE, $00, $09, $87, $04, $95, $00, $6B, $0A, $8E, $F6, $76, $04,
    $84, $07, $78, $02, $80, $04, $95, $00, $6B, $04, $80, $04, $8E, $00, $72,
    $00, $8A, $03, $03, $02, $01, $03, $00, $02, $7F, $01, $7D, $00, $76, $00,
    $8A, $03, $03, $02, $01, $03, $00, $02, $7F, $01, $7D, $00, $76, $04, $80,
    $04, $8E, $00, $72, $00, $8A, $03, $03, $02, $01, $03, $00, $02, $7F, $01,
    $7D, $00, $76, $04, $80, $08, $8E, $FE, $7F, $FE, $7E, $FF, $7D, $00, $7E,
    $01, $7D, $02, $7E, $02, $7F, $03, $00, $02, $01, $02, $02, $01, $03, $00,
    $02, $FF, $03, $FE, $02, $FE, $01, $FD, $00, $0B, $F2, $04, $8E, $00, $6B,
    $00, $92, $02, $02, $02, $01, $03, $00, $02, $7F, $02, $7E, $01, $7D, $00,
    $7E, $FF, $7D, $FE, $7E, $FE, $7F, $FD, $00, $FE, $01, $FE, $02, $0F, $FD,
    $0F, $8E, $00, $6B, $00, $92, $FE, $02, $FE, $01, $FD, $00, $FE, $7F, $FE,
    $7E, $FF, $7D, $00, $7E, $01, $7D, $02, $7E, $02, $7F, $03, $00, $02, $01,
    $02, $02, $04, $FD, $04, $8E, $00, $72, $00, $88, $01, $03, $02, $02, $02,
    $01, $03, $00, $01, $F2, $0E, $8B, $FF, $02, $FD, $01, $FD, $00, $FD, $7F,
    $FF, $7E, $01, $7E, $02, $7F, $05, $7F, $02, $7F, $01, $7E, $00, $7F, $FF,
    $7E, $FD, $7F, $FD, $00, $FD, $01, $FF, $02, $0E, $FD, $05, $95, $00, $6F,
    $01, $7D, $02, $7F, $02, $00, $F8, $8E, $07, $00, $03, $F2, $04, $8E, $00,
    $76, $01, $7D, $02, $7F, $03, $00, $02, $01, $03, $03, $00, $8A, $00, $72,
    $04, $80, $02, $8E, $06, $72, $06, $8E, $FA, $72, $08, $80, $03, $8E, $04,
    $72, $04, $8E, $FC, $72, $04, $8E, $04, $72, $04, $8E, $FC, $72, $07, $80,
    $03, $8E, $0B, $72, $00, $8E, $F5, $72, $0E, $80, $02, $8E, $06, $72, $06,
    $8E, $FA, $72, $FE, $7C, $FE, $7E, $FE, $7F, $FF, $00, $0F, $87, $0E, $8E,
    $F5, $72, $00, $8E, $0B, $00, $F5, $F2, $0B, $00, $03, $80, $09, $99, $FE,
    $7F, $FF, $7F, $FF, $7E, $00, $7E, $01, $7E, $01, $7F, $01, $7E, $00, $7E,
    $FE, $7E, $01, $8E, $FF, $7E, $00, $7E, $01, $7E, $01, $7F, $01, $7E, $00,
    $7E, $FF, $7E, $FC, $7E, $04, $7E, $01, $7E, $00, $7E, $FF, $7E, $FF, $7F,
    $FF, $7E, $00, $7E, $01, $7E, $FF, $8E, $02, $7E, $00, $7E, $FF, $7E, $FF,
    $7F, $FF, $7E, $00, $7E, $01, $7E, $01, $7F, $02, $7F, $05, $87, $04, $95,
    $00, $77, $00, $FD, $00, $77, $04, $80, $05, $99, $02, $7F, $01, $7F, $01,
    $7E, $00, $7E, $FF, $7E, $FF, $7F, $FF, $7E, $00, $7E, $02, $7E, $FF, $8E,
    $01, $7E, $00, $7E, $FF, $7E, $FF, $7F, $FF, $7E, $00, $7E, $01, $7E, $04,
    $7E, $FC, $7E, $FF, $7E, $00, $7E, $01, $7E, $01, $7F, $01, $7E, $00, $7E,
    $FF, $7E, $01, $8E, $FE, $7E, $00, $7E, $01, $7E, $01, $7F, $01, $7E, $00,
    $7E, $FF, $7E, $FF, $7F, $FE, $7F, $09, $87, $03, $86, $00, $02, $01, $03,
    $02, $01, $02, $00, $02, $7F, $04, $7D, $02, $7F, $02, $00, $02, $01, $01,
    $02, $EE, $FE, $01, $02, $02, $01, $02, $00, $02, $7F, $04, $7D, $02, $7F,
    $02, $00, $02, $01, $01, $03, $00, $02, $03, $F4, $10, $80, $03, $80, $07,
    $15, $08, $6B, $FE, $85, $F5, $00, $10, $FB, $0D, $95, $F6, $00, $00, $6B,
    $0A, $00, $02, $02, $00, $08, $FE, $02, $F6, $00, $0E, $F4, $03, $80, $00,
    $15, $0A, $00, $02, $7E, $00, $7E, $00, $7D, $00, $7E, $FE, $7F, $F6, $00,
    $0A, $80, $02, $7E, $01, $7E, $00, $7D, $FF, $7D, $FE, $7F, $F6, $00, $10,
    $80, $03, $80, $00, $15, $0C, $00, $FF, $7E, $03, $ED, $03, $FD, $00, $03,
    $02, $00, $00, $12, $02, $03, $0A, $00, $00, $6B, $02, $00, $00, $7D, $FE,
    $83, $F4, $00, $11, $80, $0F, $80, $F4, $00, $00, $15, $0C, $00, $FF, $F6,
    $F5, $00, $0F, $F5, $04, $95, $07, $76, $00, $0A, $07, $80, $F9, $76, $00,
    $75, $F8, $80, $07, $0C, $09, $F4, $F9, $0C, $09, $F4, $03, $92, $02, $03,
    $07, $00, $03, $7D, $00, $7B, $FC, $7E, $04, $7D, $00, $7A, $FD, $7E, $F9,
    $00, $FE, $02, $06, $89, $02, $00, $06, $F5, $03, $95, $00, $6B, $0C, $15,
    $00, $6B, $02, $80, $03, $95, $00, $6B, $0C, $15, $00, $6B, $F8, $96, $03,
    $00, $07, $EA, $03, $80, $00, $15, $0C, $80, $F7, $76, $FD, $00, $03, $80,
    $0A, $75, $03, $80, $03, $80, $07, $13, $02, $02, $03, $00, $00, $6B, $02,
    $80, $03, $80, $00, $15, $09, $6B, $09, $15, $00, $6B, $03, $80, $03, $80,
    $00, $15, $00, $F6, $0D, $00, $00, $8A, $00, $6B, $03, $80, $07, $80, $FD,
    $00, $FF, $03, $00, $04, $00, $07, $00, $04, $01, $02, $03, $01, $06, $00,
    $03, $7F, $01, $7E, $01, $7C, $00, $79, $FF, $7C, $FF, $7D, $FD, $00, $FA,
    $00, $0E, $80, $03, $80, $00, $15, $0C, $00, $00, $6B, $02, $80, $03, $80,
    $00, $15, $0A, $00, $02, $7F, $01, $7D, $00, $7B, $FF, $7E, $FE, $7F, $F6,
    $00, $10, $F7, $11, $8F, $FF, $03, $FF, $02, $FE, $01, $FA, $00, $FD, $7F,
    $FF, $7E, $00, $7C, $00, $79, $00, $7B, $01, $7E, $03, $00, $06, $00, $02,
    $00, $01, $03, $01, $02, $03, $FB, $03, $95, $0C, $00, $FA, $80, $00, $6B,
    $09, $80, $03, $95, $00, $77, $06, $7A, $06, $06, $00, $09, $FA, $F1, $FA,
    $7A, $0E, $80, $03, $87, $00, $0B, $02, $02, $03, $00, $02, $7E, $01, $02,
    $04, $00, $02, $7E, $00, $75, $FE, $7E, $FC, $00, $FF, $01, $FE, $7F, $FD,
    $00, $FE, $02, $07, $8E, $00, $6B, $09, $80, $03, $80, $0E, $15, $F2, $80,
    $0E, $6B, $03, $80, $03, $95, $00, $6B, $0E, $00, $00, $7D, $FE, $98, $00,
    $6B, $05, $80, $03, $95, $00, $75, $02, $7D, $0A, $00, $00, $8E, $00, $6B,
    $02, $80, $03, $95, $00, $6B, $10, $00, $00, $15, $F8, $80, $00, $6B, $0A,
    $80, $03, $95, $00, $6B, $10, $00, $00, $15, $F8, $80, $00, $6B, $0A, $00,
    $00, $7D, $02, $83, $10, $80, $03, $95, $00, $6B, $09, $00, $03, $02, $00,
    $08, $FD, $02, $F7, $00, $0E, $89, $00, $6B, $03, $80, $03, $95, $00, $6B,
    $09, $00, $03, $02, $00, $08, $FD, $02, $F7, $00, $0E, $F4, $03, $92, $02,
    $03, $07, $00, $03, $7D, $00, $70, $FD, $7E, $F9, $00, $FE, $02, $03, $89,
    $09, $00, $02, $F5, $03, $80, $00, $15, $00, $F5, $07, $00, $00, $08, $02,
    $03, $06, $00, $02, $7D, $00, $70, $FE, $7E, $FA, $00, $FE, $02, $00, $08,
    $0C, $F6, $0F, $80, $00, $15, $F6, $00, $FE, $7D, $00, $79, $02, $7E, $0A,
    $00, $F4, $F7, $07, $09, $07, $F7, $03, $8C, $01, $02, $01, $01, $05, $00,
    $02, $7F, $01, $7E, $00, $74, $00, $86, $FF, $01, $FE, $01, $FB, $00, $FF,
    $7F, $FF, $7F, $00, $7C, $01, $7E, $01, $00, $05, $00, $02, $00, $01, $02,
    $03, $FE, $04, $8E, $02, $01, $04, $00, $02, $7F, $01, $7E, $00, $77, $FF,
    $7E, $FE, $7F, $FC, $00, $FE, $01, $FF, $02, $00, $09, $01, $02, $02, $02,
    $03, $01, $02, $01, $01, $01, $01, $02, $02, $EB, $03, $80, $00, $15, $03,
    $00, $02, $7E, $00, $7B, $FE, $7E, $FD, $00, $03, $80, $04, $00, $03, $7E,
    $00, $78, $FD, $7E, $F9, $00, $0C, $80, $03, $8C, $02, $02, $02, $01, $03,
    $00, $02, $7F, $01, $7D, $FE, $7E, $F9, $7D, $FF, $7E, $00, $7D, $03, $7F,
    $02, $00, $03, $01, $02, $01, $02, $FE, $0D, $8C, $FF, $02, $FE, $01, $FC,
    $00, $FE, $7F, $FF, $7E, $00, $77, $01, $7E, $02, $7F, $04, $00, $02, $01,
    $01, $02, $00, $0F, $FF, $02, $FE, $01, $F9, $00, $0C, $EB, $03, $88, $0A,
    $00, $00, $02, $00, $03, $FE, $02, $FA, $00, $FF, $7E, $FF, $7D, $00, $7B,
    $01, $7C, $01, $7F, $06, $00, $02, $02, $03, $FE, $03, $8F, $06, $77, $06,
    $09, $FA, $80, $00, $71, $FF, $87, $FB, $79, $07, $87, $05, $79, $02, $80,
    $03, $8D, $02, $02, $06, $00, $02, $7E, $00, $7D, $FC, $7D, $04, $7E, $00,
    $7D, $FE, $7E, $FA, $00, $FE, $02, $04, $85, $02, $00, $06, $F9, $03, $8F,
    $00, $73, $01, $7E, $07, $00, $02, $02, $00, $0D, $00, $F3, $01, $7E, $03,
    $80, $03, $8F, $00, $73, $01, $7E, $07, $00, $02, $02, $00, $0D, $00, $F3,
    $01, $7E, $F8, $90, $03, $00, $08, $F0, $03, $80, $00, $15, $00, $F3, $02,
    $00, $06, $07, $FA, $F9, $07, $78, $03, $80, $03, $80, $04, $0C, $02, $03,
    $04, $00, $00, $71, $02, $80, $03, $80, $00, $0F, $06, $77, $06, $09, $00,
    $71, $02, $80, $03, $80, $00, $0F, $0A, $F1, $00, $0F, $F6, $F8, $0A, $00,
    $02, $F9, $05, $80, $FF, $01, $FF, $04, $00, $05, $01, $03, $01, $02, $06,
    $00, $02, $7E, $00, $7D, $00, $7B, $00, $7C, $FE, $7F, $FA, $00, $0B, $80,
    $03, $80, $00, $0F, $00, $FB, $01, $03, $01, $02, $05, $00, $02, $7E, $01,
    $7D, $00, $76, $03, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80,
    $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10,
    $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80,
    $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10,
    $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80,
    $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $10,
    $80, $10, $80, $10, $80, $10, $80, $10, $80, $10, $80, $0A, $8F, $02, $7F,
    $01, $7E, $00, $76, $FF, $7F, $FE, $7F, $FB, $00, $FF, $01, $FF, $01, $00,
    $0A, $01, $02, $01, $01, $05, $00, $F9, $80, $00, $6B, $0C, $86, $0D, $8A,
    $FF, $03, $FE, $02, $FB, $00, $FF, $7E, $FF, $7D, $00, $7B, $01, $7C, $01,
    $7F, $05, $00, $02, $01, $01, $03, $03, $FC, $03, $80, $00, $0F, $00, $FB,
    $01, $03, $01, $02, $04, $00, $01, $7E, $01, $7D, $00, $76, $00, $8A, $01,
    $03, $02, $02, $03, $00, $02, $7E, $01, $7D, $00, $76, $03, $80, $03, $8F,
    $00, $74, $01, $7E, $02, $7F, $04, $00, $02, $01, $01, $01, $00, $8D, $00,
    $6E, $FF, $7E, $FE, $7F, $FB, $00, $FE, $01, $0C, $85, $03, $8D, $01, $02,
    $03, $00, $02, $7E, $01, $02, $03, $00, $02, $7E, $00, $74, $FE, $7F, $FD,
    $00, $FF, $01, $FE, $7F, $FD, $00, $FF, $01, $00, $0C, $06, $82, $00, $6B,
    $08, $86, $03, $80, $0A, $0F, $F6, $80, $0A, $71, $03, $80, $03, $8F, $00,
    $73, $01, $7E, $07, $00, $02, $02, $00, $0D, $00, $F3, $01, $7E, $00, $7E,
    $03, $82, $03, $8F, $00, $79, $02, $7E, $08, $00, $00, $89, $00, $71, $02,
    $80, $03, $8F, $00, $73, $01, $7E, $03, $00, $02, $02, $00, $0D, $00, $F3,
    $01, $7E, $03, $00, $02, $02, $00, $0D, $00, $F3, $01, $7E, $03, $80, $03,
    $8F, $00, $73, $01, $7E, $03, $00, $02, $02, $00, $0D, $00, $F3, $01, $7E,
    $03, $00, $02, $02, $00, $0D, $00, $F3, $01, $7E, $00, $7E, $03, $82, $03,
    $8D, $00, $02, $02, $00, $00, $71, $08, $00, $02, $02, $00, $06, $FE, $02,
    $F8, $00, $0C, $F6, $03, $8F, $00, $71, $07, $00, $02, $02, $00, $06, $FE,
    $02, $F9, $00, $0C, $85, $00, $71, $02, $80, $03, $8F, $00, $71, $07, $00,
    $03, $02, $00, $06, $FD, $02, $F9, $00, $0C, $F6, $03, $8D, $02, $02, $06,
    $00, $02, $7E, $00, $75, $FE, $7E, $FA, $00, $FE, $02, $04, $85, $06, $00,
    $02, $F9, $03, $80, $00, $0F, $00, $F8, $04, $00, $00, $06, $02, $02, $04,
    $00, $02, $7E, $00, $75, $FE, $7E, $FC, $00, $FE, $02, $00, $05, $0A, $F9,
    $0D, $80, $00, $0F, $F7, $00, $FF, $7E, $00, $7B, $01, $7E, $09, $00, $F6,
    $FA, $04, $06, $08, $FA);


{ TAggGsvText }
  
constructor TAggGsvText.Create;
var
  T: Integer;
begin
  inherited Create;

  FX := 0.0;
  FY := 0.0;
  FStartX := 0.0;
  FWidth := 10.0;
  FHeight := 0.0;
  FSpace := 0.0;
  FLineSpace := 0.0;
  FText := @FCharacters[0];
  FTextBuffer := nil;
  FBufferSize := 0;
  FCurrentChr := @FCharacters[0];
  FFont := @CAggGsvDefaultFont[0];
  FLoadedFont := nil;
  FLoadFontSize := 0;
  FStatus := isInitial;
  FFlip := False;

  FCharacters[0] := 0;
  FCharacters[1] := 0;

  // check endianess
  T := 1;
  FBigEndian := Byte(Pointer(@T)^) = 0;
end;

destructor TAggGsvText.Destroy;
begin
  if FLoadedFont <> nil then
    AggFreeMem(FLoadedFont, FLoadFontSize);

  if FTextBuffer <> nil then
    AggFreeMem(FTextBuffer, FBufferSize);

  inherited;
end;

procedure TAggGsvText.SetFont(Font: Pointer);
begin
  FFont := Font;

  if FFont = nil then
    FFont := FLoadedFont;
end;

procedure TAggGsvText.SetFlip(Value: Boolean);
begin
  FFlip := Value;
end;

procedure TAggGsvText.SetLoadFont(FileName: ShortString);
var
  Fd : file;
  Err: Integer;
begin
  if FLoadedFont <> nil then
    AggFreeMem(FLoadedFont, FLoadFontSize);

{$I- }
  Err := Ioresult;

  Assignfile(Fd, FileName);
  Reset(Fd, 1);

  Err := Ioresult;

  if Err = 0 then
  begin
    FLoadFontSize := Filesize(Fd);

    if FLoadFontSize > 0 then
    begin
      AggGetMem(FLoadedFont, FLoadFontSize);
      BlockRead(Fd, FLoadedFont^, FLoadFontSize);

      FFont := FLoadedFont;
    end;

    Close(Fd);
  end;
end;

procedure TAggGsvText.SetSize(Height: Double; Width: Double = 0.0);
begin
  FHeight := Height;
  FWidth := Width;
end;

procedure TAggGsvText.SetSpace(Value: Double);
begin
  FSpace := Value;
end;

procedure TAggGsvText.SetLineSpace(Value: Double);
begin
  FLineSpace := Value;
end;

procedure TAggGsvText.SetStartPoint(X, Y: Double);
begin
  FX := X;
  FY := Y;

  FStartX := X;
end;

procedure TAggGsvText.SetStartPoint(Point: TPointDouble);
begin
  FX := Point.X;
  FY := Point.Y;

  FStartX := Point.X;
end;

procedure TAggGsvText.SetText(Text: AnsiString);
var
  NewSize: Cardinal;
begin
  NewSize := Length(Text) + 1;

  if NewSize > FBufferSize then
  begin
    if FTextBuffer <> nil then
      AggFreeMem(FTextBuffer, FBufferSize);

    AggGetMem(FTextBuffer, NewSize);

    FBufferSize := NewSize;
  end;

  Move(Text[1], FTextBuffer^, NewSize);

  FText := FTextBuffer;
end;

procedure TAggGsvText.Rewind(PathID: Cardinal);
var
  BaseHeight: Double;
begin
  FStatus := isInitial;

  if FFont = nil then
    Exit;

  FIndices := FFont;

  BaseHeight := GetValue(Pointer(PtrComp(FIndices) + 4));

  FIndices := Pointer(PtrComp(FIndices) + GetValue(FIndices));
  FGlyphs := PInt8(FIndices);
  Inc(FGlyphs, 514);

  FH := FHeight / BaseHeight;

  if FWidth = 0 then
    FW := FH
  else
    FW := FWidth / BaseHeight;

  if FFlip then
    FH := -FH;

  FCurrentChr := FText;
end;

function TAggGsvText.Vertex(X, Y: PDouble): Cardinal;
var
  Idx: Cardinal;

  Yc, Yf: Int8;
  Dx, Dy: Integer;

  Quit: Boolean;
label
  _nxch, _strt;

begin
  Quit := False;

  while not Quit do
    case FStatus of
      isInitial:
        if FFont = nil then
          Quit := True

        else
        begin
          FStatus := isNextChar;

          goto _nxch;
        end;

      isNextChar:
      _nxch:
        if FCurrentChr^ = 0 then
          Quit := True

        else
        begin
          Idx := FCurrentChr^ and $FF;

          Inc(PtrComp(FCurrentChr), SizeOf(Int8u));

          if Idx = 13 then
          begin
            FX := FStartX;

            if FFlip then
              FY := FY - (-FHeight - FLineSpace)
            else
              FY := FY - (FHeight + FLineSpace);

          end
          else
          begin
            Idx := Idx shl 1;

            FGlyphB :=
              Pointer(PtrComp(FGlyphs) +
              GetValue(Pointer(PtrComp(FIndices) + Idx)));
            FGlyphE :=
              Pointer(PtrComp(FGlyphs) +
              GetValue(Pointer(PtrComp(FIndices) + Idx + 2)));
            FStatus := isStartGlyph;

            goto _strt;
          end;
        end;

      isStartGlyph:
      _strt:
        begin
          X^ := FX;
          Y^ := FY;

          FStatus := isGlyph;

          Result := CAggPathCmdMoveTo;

          Exit;
        end;

      isGlyph:
        if PtrComp(FGlyphB) >= PtrComp(FGlyphE) then
        begin
          FStatus := isNextChar;

          FX := FX + FSpace;
        end
        else
        begin
          Dx := Integer(FGlyphB^);

          Inc(PtrComp(FGlyphB), SizeOf(Int8u));

          Yc := FGlyphB^;

          Inc(PtrComp(FGlyphB), SizeOf(Int8u));

          Yf := Yc and $80;
          Yc := Yc shl 1;
          Yc := ShrInt8(Yc, 1);

          Dy := Yc;

          FX := FX + (Dx * FW);
          FY := FY + (Dy * FH);

          X^ := FX;
          Y^ := FY;

          if Yf <> 0 then
            Result := CAggPathCmdMoveTo
          else
            Result := CAggPathCmdLineTo;

          Exit;
        end;
    end;

  Result := CAggPathCmdStop;
end;

function TAggGsvText.GetValue(P: PInt8u): Int16u;
var
  V: Int16u;
begin
  if FBigEndian then
  begin
    TInt16uAccess(V).Low := PInt8u(PtrComp(P) + 1)^;
    TInt16uAccess(V).High := P^;
  end
  else
  begin
    TInt16uAccess(V).Low := P^;
    TInt16uAccess(V).High := PInt8u(PtrComp(P) + 1)^;
  end;

  Result := V;
end;


{ TAggGsvTextOutline }

constructor TAggGsvTextOutline.Create(Text: TAggGsvText; Trans: TAggTransAffine);
begin
  FPolyline := TAggConvStroke.Create(Text);
  FTrans := TAggConvTransform.Create(FPolyline, Trans);
end;

destructor TAggGsvTextOutline.Destroy;
begin
  FPolyline.Free;
  FTrans.Free;
  inherited;
end;

function TAggGsvTextOutline.GetTransformer: TAggTransAffine;
begin
  Result := FTrans.Transformer;
end;

function TAggGsvTextOutline.GetWidth: Double;
begin
  Result := FPolyline.Width;
end;

procedure TAggGsvTextOutline.SetWidth(Value: Double);
begin
  FPolyline.Width := Value;
end;

procedure TAggGsvTextOutline.SetTransformer(Trans: TAggTransAffine);
begin
  FTrans.Transformer := Trans;
end;

procedure TAggGsvTextOutline.Rewind(PathID: Cardinal);
begin
  FTrans.Rewind(PathID);

  FPolyline.LineJoin := ljRound;
  FPolyline.LineCap := lcRound;
end;

function TAggGsvTextOutline.Vertex(X, Y: PDouble): Cardinal;
begin
  Result := FTrans.Vertex(X, Y);
end;

end.
