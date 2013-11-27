unit AggFontWin32TrueType;

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
  Windows, SysUtils, Math,
  AggBasics,
  AggArray,
  AggBitsetIterator,
  AggScanLineStorageAA,
  AggScanLineStorageBin,
  AggScanLine,
  AggScanlineUnpacked,
  AggScanLineBin,
  AggRendererScanLine,
  AggRenderScanLines,
  AggPathStorageInteger,
  AggRasterizerScanLineAA,
  AggConvCurve,
  AggTransAffine,
  AggFontCacheManager,
  AggFontEngine,
  AggVertexSource;

const
  CAggBufferSize = 32768 - 32;

type
  TAggWinapiGetGlyphOutlineX = function(DC: HDC; P2, P3: UINT;
    const P4: TGlyphMetrics; P5: DWORD; P6: Pointer; const P7: TMat2): DWORD;
    stdcall;

  HFONT_ptr = ^HFONT;
  PFixed = ^TFixed;

  PAggFontName = ^TAggFontName;
  TAggFontName = AnsiString;

  TAggFontEngineWin32TrueTypeBase = class(TAggCustomFontEngine)
  private
    FFlag32: Boolean;
    FDc: HDC;

    FOldFont: HFONT;
    FFonts: HFONT_ptr;

    FNumFonts, FMaxFonts: Cardinal;
    FFontNames: PAggFontName;
    FCurrentFont: HFONT;

    FChangeStamp: Integer;
    FTypeFace: TAggFontName;
    FTypeFaceLength: Cardinal;
    FSignature: TAggFontName;

    FHeight, FWidth: Cardinal;
    FWeight: Integer;
    FItalic: Boolean;
    FCharSet: DWORD;

    FPitchAndFamily: DWORD;

    FHinting, FFlipY: Boolean;

    FFontCreated: Boolean;
    FResolution: Cardinal;
    FGlyphRendering: TAggGlyphRendering;
    FGlyphIndex: Cardinal;

    FDataSize: Cardinal;
    FDataType: TAggGlyphData;
    FBounds: TRectInteger;
    FAdvanceX, FAdvanceY: Double;

    FMatrix: TMAT2;
    FGlyphBuffer: PInt8u;

    FKerningPairs: PKerningPair;
    FNumKerningPairs, FMaxKerningPairs: Cardinal;

    FAffine: TAggTransAffine;

    FPath16: TAggPathStorageInt16;
    FPath32: TAggPathStorageInt32;
    FCurves16, FCurves32: TAggConvCurve;

    FScanLineAA_: TAggScanLineUnpacked8;
    FScanLineBin: TAggScanLineBin;
    FScanLinesAA: TAggScanLinesAA;
    FScanLinesBin: TAggScanLinesBin;
    FRasterizer: TAggRasterizerScanLineAA;

    // Private
    procedure UpdateSignature;
    function PairLess(V1, V2: PKerningPair): Boolean;

    procedure LoadKerningPairs;
    procedure SortKerningPairs;

    function FindFont(Name: AnsiString): Integer;

    procedure SetFlipY(Flip: Boolean);
    procedure SetHinting(H: Boolean);

    // Accessors
    function GetTypeFace: AnsiString;
    function GetHeight: Double;
    function GetWidth: Double;

    // Set font parameters
    procedure SetResolution(Dpi: Cardinal);
    procedure SetHeight(H: Double);
    procedure SetWidth(W: Double);
    procedure SetWeight(W: Integer);
    procedure SetItalic(It: Boolean);
    procedure SetCharSet(C: DWORD);

    procedure SetPitchAndFamily(P: DWORD);
  protected
    function GetGlyphIndex: Cardinal; override;
    function GetDataSize: Cardinal; override;
    function GetDataType: TAggGlyphData; override;
    function GetFlag32: Boolean; override;
    function GetAdvanceX: Double; override;
    function GetAdvanceY: Double; override;
  public
    constructor Create(AFlag32: Boolean; Dc: HDC; MaxFonts: Cardinal = 32);
    destructor Destroy; override;

    function CreateFont(ATypeFace: AnsiString; RenType: TAggGlyphRendering):
      Boolean; overload;
    function CreateFont(ATypeface: AnsiString; RenType: TAggGlyphRendering;
      Height: Double; Width: Double = 0.0; Weight: Integer = FW_REGULAR;
      Italic: Boolean = False; CharSet: DWORD = ANSI_CHARSET;
      APitchAndFamily: DWORD = FF_DONTCARE): Boolean; overload;

    // Set Gamma
    procedure SetGamma(Value: TAggCustomVertexSource);
    procedure SetTransform(Mtx: TAggTransAffine);

    // Interface mandatory to implement for TAggFontCacheManager
    function GetFontSignature: AnsiString; override;
    function ChangeStamp: Integer; override;

    function PrepareGlyph(GlyphCode: Cardinal): Boolean; override;

    function GetBounds: PRectInteger; override;

    procedure WriteGlyphTo(Data: PInt8u); override;
    function AddKerning(First, Second: Cardinal; X, Y: PDouble): Boolean;
      override;

    property FlipY: Boolean read FFlipY write SetFlipY;
    property Hinting: Boolean read FHinting write SetHinting;
    property Resolution: Cardinal read FResolution write SetResolution;
    property TypeFace: AnsiString read GetTypeFace;
    property Height: Double read GetHeight write SetHeight;
    property Width: Double read GetWidth write SetWidth;
    property Weight: Integer read FWeight write SetWeight;
    property Italic: Boolean read FItalic write SetItalic;
    property CharSet: DWORD read FCharSet write SetCharSet;
    property PitchAndFamily: DWORD read FPitchAndFamily write SetPitchAndFamily;
  end;

  // This class uses values of type int16 (10.6 format) for the vector cache.
  // The vector cache is compact, but when rendering glyphs of height
  // more that 200 there integer overfLow can occur.
  TAggFontEngineWin32TrueTypeInt16 = class(TAggFontEngineWin32TrueTypeBase)
  public
    constructor Create(Dc: HDC; MaxFonts: Cardinal = 32);
  end;

  // This class uses values of type int32 (26.6 format) for the vector cache.
  // The vector cache is twice larger than in FontEngineWin32TrueTypeInt16,
  // but it allows you to render glyphs of very large sizes.
  TAggFontEngineWin32TrueTypeInt32 = class(TAggFontEngineWin32TrueTypeBase)
  public
    constructor Create(Dc: HDC; MaxFonts: Cardinal = 32);
  end;

var
  GetGlyphOutlineX: TAggWinapiGetGlyphOutlineX;

implementation


// ------------------------------------------------------------------------------
//
// This code implements the AUTODIN II polynomial
// The variable corresponding to the macro argument "crc" should
// be an Cardinal long.
// Oroginal code  by Spencer Garrett <srg@quick.com>
//
// generated using the AUTODIN II polynomial
// x^32 + x^26 + x^23 + x^22 + x^16 +
// x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x^1 + 1
//
// ------------------------------------------------------------------------------
const
  Crc32tab: array [0..255] of Cardinal = ($00000000, $77073096, $EE0E612C,
    $990951BA, $076DC419, $706AF48F, $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4,
    $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064,
    $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9, $FA0F3D63,
    $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172, $3C03E4D1, $4B04D447,
    $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940, $32D86CE3,
    $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2,
    $B10BE924, $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106,
    $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433, $7807C9A2,
    $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E, $6C0695ED, $1B01A57B, $8208F4C1,
    $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49,
    $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541,
    $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA, $BE0B1010,
    $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F, $5EDEF90E, $29D9C998,
    $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD, $EDB88320,
    $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27,
    $7D079EB1, $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
    $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC, $F9B9DF6F,
    $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B, $D80D2BDA, $AF0A1B4C, $36034AF6,
    $41047A60, $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79, $CB61B38C, $BC66831A,
    $256FD2A0, $5268E236, $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE,
    $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F, $72076785,
    $05005713, $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38, $92D28E9B, $E5D5BE0D,
    $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E, $81BE16CD,
    $F6B9265B, $6FB077E1, $18B74777, $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354,
    $3903B3C2, $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC,
    $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9, $BDBDF21C,
    $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94, $B40BBE37, $C30C8EA1, $5A05DF1B,
    $2D02EF8D);

  
function CalcCrc32(Buf: PInt8u; Size: Cardinal): Cardinal;
var
  Crc, Len, Nr: Cardinal;
  P: PInt8u;
begin
  Crc := Cardinal(not 0);
  Len := 0;
  Nr := Size;
  Len := Len + Nr;
  P := Buf;

  while Nr <> 0 do
  begin
    Dec(Nr);

    Crc := (Crc shr 8) xor Crc32tab[(Crc xor P^) and $FF];

    Inc(PtrComp(P), SizeOf(Int8u));
  end;

  Result := not Crc;
end;

function DoubleToFixedPoint(D: Double): TFixed;
var
  L: Integer;
begin
  L := Trunc(D * 65536.0);

  Move(L, Result, SizeOf(Integer));
end;

function DoubleToPlainFixedPoint(D: Double): Integer;
begin
  Result := Trunc(D * 65536.0);
end;

function NegateFx(Fx: PFixed): TFixed;
var
  L: Integer;
begin
  L := -Integer(Fx);

  Move(L, Result, SizeOf(Integer));
end;

function FixedPointToDouble(P: PFixed): Double;
begin
  Result := P.Value + P.Fract * (1.0 / 65536.0);
end;

function FixedPointToPlainInt(Fx: PFixed): Integer;
begin
  Result := Integer(Fx);
end;

function FixedPointToInt26p6(P: PFixed): Integer;
begin
  Result := (Integer(P.Value) shl 6) + (ShrInt32(Integer(P.Fract), 10));
end;

function DoubleToInt26p6(P: Double): Integer;
begin
  Result := Trunc(P * 64.0 + 0.5);
end;

procedure DecomposeWin32GlyphBitmapMono(Gbuf: PInt8u; W, H, X, Y: Integer;
  FlipY: Boolean; Sl: TAggCustomScanLine; Storage: TAggCustomRendererScanLine);
var
  I, Pitch, J: Integer;
  Buf : PInt8u;
  Bits: TAggBitsetIterator;
begin
  Pitch := ShrInt32(W + 31, 5) shl 2;
  Buf := Gbuf;

  Sl.Reset(X, X + W);
  Storage.Prepare(W + 2);

  if FlipY then
  begin
    Inc(PtrComp(Buf), (Pitch * (H - 1)) * SizeOf(Int8u));
    Inc(Y, H);

    Pitch := -Pitch;
  end;

  I := 0;

  while I < H do
  begin
    Sl.ResetSpans;

    J := 0;

    Bits := TAggBitsetIterator.Create(Buf, 0);
    try
      while J < W do
      begin
        if Bits.Bit <> 0 then
          Sl.AddCell(X + J, CAggCoverFull);

        Bits.IncOperator;

        Inc(J);
      end;
    finally
      Bits.Free;
    end;

    Inc(PtrComp(Buf), Pitch * SizeOf(Int8u));

    if Sl.NumSpans <> 0 then
    begin
      Sl.Finalize(Y - I - 1);
      Storage.Render(Sl);
    end;

    Inc(I);
  end;
end;

procedure DecomposeWin32GlyphBitmapGray8(Gbuf: PInt8u; W, H, X, Y: Integer;
  FlipY: Boolean; Ras: TAggRasterizerScanLineAA; Sl: TAggCustomScanLine;
  Storage: TAggCustomRendererScanLine);
var
  I, J, Pitch: Integer;
  Buf, P: PInt8u;
  V: Cardinal;
begin
  Pitch := ShrInt32(W + 3, 2) shl 2;
  Buf := Gbuf;

  Sl.Reset(X, X + W);
  Storage.Prepare(W + 2);

  if FlipY then
  begin
    Inc(PtrComp(Buf), (Pitch * (H - 1)) * SizeOf(Int8u));
    Inc(Y, H);

    Pitch := -Pitch;
  end;

  I := 0;

  while I < H do
  begin
    Sl.ResetSpans;

    P := Buf;
    J := 0;

    while J < W do
    begin
      if P^ <> 0 then
      begin
        V := P^;

        if V = 64 then
          V := 255
        else
          V := V shl 2;

        Sl.AddCell(X + J, Ras.ApplyGamma(V));
      end;

      Inc(PtrComp(P), SizeOf(Int8u));
      Inc(J);
    end;

    Inc(PtrComp(Buf), Pitch * SizeOf(Int8u));

    if Sl.NumSpans <> 0 then
    begin
      Sl.Finalize(Y - I - 1);
      Storage.Render(Sl);
    end;

    Inc(I);
  end;
end;

function DecomposeWin32GlyphOutline(Gbuf: PInt8u; TotalSize: Cardinal;
  FlipY: Boolean; Mtx: TAggTransAffine;
  Path: TAggCustomPathStorageInteger): Boolean;
var
  CurrentGlyph, EndGlyph, EndPoly, CurrentPoly: PInt8u;

  X, Y, X2, Y2: Double;

  I, U: Integer;

  Th: PTTPolygonHeader;
  Pc: PTTPolyCurve;

  Pnt_b, Pnt_c: POINTFX;

  Cx, Cy, Bx, By: Integer;
begin
  CurrentGlyph := Gbuf;
  EndGlyph := PInt8u(PtrComp(Gbuf) + TotalSize * SizeOf(Int8u));

  while PtrComp(CurrentGlyph) < PtrComp(EndGlyph) do
  begin
    Th := PTTPolygonHeader(CurrentGlyph);

    EndPoly := PInt8u(PtrComp(CurrentGlyph) + Th.Cb);
    CurrentPoly := PInt8u(PtrComp(CurrentGlyph) + SizeOf(TTPOLYGONHEADER));

    X := FixedPointToDouble(@Th.PfxStart.X);
    Y := FixedPointToDouble(@Th.PfxStart.Y);

    if FlipY then
      Y := -Y;

    Mtx.Transform(Mtx, @X, @Y);
    Path.MoveTo(DoubleToInt26p6(X), DoubleToInt26p6(Y));

    while PtrComp(CurrentPoly) < PtrComp(EndPoly) do
    begin
      Pc := PTTPolyCurve(CurrentPoly);

      if Pc.WType = TT_PRIM_LINE then
      begin
        I := 0;

        while I < Pc.Cpfx do
        begin
          X := FixedPointToDouble(@PPointfx(PtrComp(@Pc.Apfx) + I *
            SizeOf(POINTFX)).X);
          Y := FixedPointToDouble(@PPointfx(PtrComp(@Pc.Apfx) + I *
            SizeOf(POINTFX)).Y);

          if FlipY then
            Y := -Y;

          Mtx.Transform(Mtx, @X, @Y);
          Path.LineTo(DoubleToInt26p6(X), DoubleToInt26p6(Y));

          Inc(I);
        end;
      end;

      if Pc.WType = TT_PRIM_QSPLINE then
      begin
        U := 0;

        while U < Pc.Cpfx - 1 do // Walk through points in spline
        begin
          // B is always the current point
          Pnt_b := PPointfx(PtrComp(@Pc.Apfx) + U * SizeOf(POINTFX))^;
          Pnt_c := PPointfx(PtrComp(@Pc.Apfx) + (U + 1) * SizeOf(POINTFX))^;

          if U < Pc.Cpfx - 2 then // If not on last spline, compute C
          begin
            // midpoint (x,y)
            // Integer(pnt_c.x) := Integer(pnt_b.x) + Integer(pnt_c.x) div 2;
            // Integer(pnt_c.y) := Integer(pnt_b.y) + Integer(pnt_c.y) div 2;
            Move(Pnt_b.X, Bx, SizeOf(Integer));
            Move(Pnt_b.Y, By, SizeOf(Integer));
            Move(Pnt_c.X, Cx, SizeOf(Integer));
            Move(Pnt_c.Y, Cy, SizeOf(Integer));

            Cx := (Bx + Cx) div 2;
            Cy := (By + Cy) div 2;

            Move(Cx, Pnt_c.X, SizeOf(Integer));
            Move(Cy, Pnt_c.Y, SizeOf(Integer));
          end;

          X := FixedPointToDouble(@Pnt_b.X);
          Y := FixedPointToDouble(@Pnt_b.Y);
          X2 := FixedPointToDouble(@Pnt_c.X);
          Y2 := FixedPointToDouble(@Pnt_c.Y);

          if FlipY then
          begin
            Y := -Y;
            Y2 := -Y2;
          end;

          Mtx.Transform(Mtx, @X, @Y);
          Mtx.Transform(Mtx, @X2, @Y2);

          Path.Curve3To(DoubleToInt26p6(X), DoubleToInt26p6(Y), DoubleToInt26p6(X2),
            DoubleToInt26p6(Y2));

          Inc(U);
        end;
      end;

      Inc(PtrComp(CurrentPoly), SizeOf(WORD) * 2 + SizeOf(POINTFX) * Pc.Cpfx);
    end;

    Inc(PtrComp(CurrentGlyph), Th.Cb);
  end;

  Result := True;
end;


{ TAggFontEngineWin32TrueTypeBase }

constructor TAggFontEngineWin32TrueTypeBase.Create;
begin
  FFlag32 := AFlag32;
  FDc := Dc;

  if FDc <> 0 then
    FOldFont := GetCurrentObject(FDc, OBJ_FONT)
  else
    FOldFont := 0;

  AggGetMem(Pointer(FFonts), SizeOf(HFONT) * MaxFonts);

  FNumFonts := 0;
  FMaxFonts := MaxFonts;

  AggGetMem(Pointer(FFontNames), SizeOf(TAggFontName) * MaxFonts);

  FCurrentFont := 0;
  FChangeStamp := 0;

  FTypeFace := '';
  FSignature := '';

  FHeight := 0;
  FWidth := 0;
  FWeight := FW_REGULAR;
  FItalic := False;
  FCharSet := DEFAULT_CHARSET;

  FPitchAndFamily := FF_DONTCARE;

  FHinting := True;
  FFlipY := False;

  FFontCreated := False;
  FResolution := 0;
  FGlyphRendering := grNativeGray8;

  FGlyphIndex := 0;
  FDataSize := 0;
  FDataType := gdInvalid;

  FBounds := RectInteger(1, 1, 0, 0);

  FAdvanceX := 0.0;
  FAdvanceY := 0.0;

  AggGetMem(Pointer(FGlyphBuffer), SizeOf(Int8u) * CAggBufferSize);

  FKerningPairs := nil;
  FNumKerningPairs := 0;
  FMaxKerningPairs := 0;

  FAffine := TAggTransAffine.Create;

  FPath16 := TAggPathStorageInt16.Create;
  FPath32 := TAggPathStorageInt32.Create;
  FCurves16 := TAggConvCurve.Create(FPath16);
  FCurves32 := TAggConvCurve.Create(FPath32);
  FScanLineAA_ := TAggScanLineUnpacked8.Create;
  FScanLineBin := TAggScanLineBin.Create;
  FScanLinesAA := TAggScanLinesAA.Create;
  FScanLinesBin := TAggScanLinesBin.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  FCurves16.ApproximationScale := 4.0;
  FCurves32.ApproximationScale := 4.0;

  FillChar(FMatrix, SizeOf(FMatrix), 0);

  FMatrix.EM11.Value := 1;
  FMatrix.EM22.Value := 1;
end;

destructor TAggFontEngineWin32TrueTypeBase.Destroy;
var
  I: Cardinal;
  F: HFONT_ptr;
  N: PAggFontName;
begin
  AggFreeMem(Pointer(FKerningPairs), FMaxKerningPairs *
    SizeOf(TKerningPair));
  AggFreeMem(Pointer(FGlyphBuffer), SizeOf(Int8u) * CAggBufferSize);

  if (FDc <> 0) and (FOldFont <> 0) then
    SelectObject(FDc, FOldFont);

  I := 0;
  F := FFonts;
  N := FFontNames;

  while I < FNumFonts do
  begin
    DeleteObject(F^);
    Inc(PtrComp(F), SizeOf(HFONT));
    Inc(I);
  end;

  AggFreeMem(Pointer(FFontNames), SizeOf(TAggFontName) * FMaxFonts);
  AggFreeMem(Pointer(FFonts), SizeOf(HFONT) * FMaxFonts);

  FPath16.Free;
  FPath32.Free;
  FCurves16.Free;
  FCurves32.Free;
  FScanLineAA_.Free;
  FScanLineBin.Free;
  FScanLinesAA.Free;
  FScanLinesBin.Free;
  FRasterizer.Free;
  FAffine.Free;

  inherited;
end;

procedure TAggFontEngineWin32TrueTypeBase.SetResolution;
begin
  FResolution := Dpi;
end;

procedure TAggFontEngineWin32TrueTypeBase.SetHeight;
begin
  FHeight := Trunc(H);
end;

procedure TAggFontEngineWin32TrueTypeBase.SetWidth;
begin
  FWidth := Trunc(W);
end;

procedure TAggFontEngineWin32TrueTypeBase.SetWeight;
begin
  FWeight := W;
end;

procedure TAggFontEngineWin32TrueTypeBase.SetItalic;
begin
  FItalic := It;
end;

procedure TAggFontEngineWin32TrueTypeBase.SetCharSet;
begin
  FCharSet := C;
end;

procedure TAggFontEngineWin32TrueTypeBase.SetPitchAndFamily;
begin
  FPitchAndFamily := P;
end;

procedure TAggFontEngineWin32TrueTypeBase.SetFlipY;
begin
  FFlipY := Flip;
end;

procedure TAggFontEngineWin32TrueTypeBase.SetHinting;
begin
  FHinting := H;
end;

function TAggFontEngineWin32TrueTypeBase.CreateFont(ATypeface: AnsiString;
  RenType: TAggGlyphRendering): Boolean;
var
  Len: Cardinal;

  H, W, Index: Integer;

  F: HFONT_ptr;
  N: PAggFontName;
begin
  if FDc <> 0 then
  begin
    Len := Length(ATypeface);

    if Len > FTypeFaceLength then
    begin
(*
      AggFreeMem(Pointer(FSignature.Name), FSignature.Size);
      AggFreeMem(Pointer(FTypeFace.Name), FTypeFace.Size);

      FTypeFace.Size := SizeOf(AnsiChar) * (Len + 32);
      FSignature.Size := SizeOf(AnsiChar) * (Len + 32 + 256);

      AggGetMem(Pointer(FTypeFace.Name), FTypeFace.Size);
      AggGetMem(Pointer(FSignature.Name), FSignature.Size);

      FTypeFaceLength := Len + 32 - 1;
*)
    end;

    FTypeFace := ATypeface;

    H := FHeight;
    W := FWidth;

    if FResolution <> 0 then
    begin
      H := MulDiv(FHeight, FResolution, 72);
      W := MulDiv(FWidth, FResolution, 72);
    end;

    FGlyphRendering := RenType;

    UpdateSignature;

    Index := FindFont(FSignature);

    if Index >= 0 then
    begin
      FCurrentFont := HFONT_ptr(PtrComp(FFonts) + Index * SizeOf(HFONT))^;
      SelectObject(FDc, FCurrentFont);
      FNumKerningPairs := 0;
      Result := True;
      Exit;
    end
    else
    begin
      FCurrentFont := CreateFontA(-H, // height of font
        W, // average character width
        0, // angle of escapement
        0, // base-line orientation angle
        FWeight, // font weight
        Cardinal(FItalic), // italic attribute option
        0, // underline attribute option
        0, // strikeout attribute option
        FCharSet, // character set identifier
        OUT_DEFAULT_PRECIS, // output precision
        CLIP_DEFAULT_PRECIS, // clipping precision
        ANTIALIASED_QUALITY, // output quality
        FPitchAndFamily, // pitch and family
        PAnsiChar(FTypeFace) // typeface name
        );

      if FCurrentFont <> 0 then
      begin
        if FNumFonts >= FMaxFonts then
        begin
          if FOldFont <> 0 then
            SelectObject(FDc, FOldFont);

          DeleteObject(FFonts^);

          Move(HFONT_ptr(PtrComp(FFonts) + 1 * SizeOf(HFONT))^, FFonts^,
            (FMaxFonts - 1) * SizeOf(HFONT));

          Move(PAggFontName(PtrComp(FFontNames) + SizeOf(TAggFontName))^,
            FFontNames^, (FMaxFonts - 1) * SizeOf(TAggFontName));

          FNumFonts := FMaxFonts - 1;
        end;

        UpdateSignature;

        N := PAggFontName(PtrComp(FFontNames) + FNumFonts * SizeOf(TAggFontName));
        F := HFONT_ptr(PtrComp(FFonts) + FNumFonts * SizeOf(HFONT));

        Move(FSignature, N^, SizeOf(TAggFontName));

        F^ := FCurrentFont;

        Inc(FNumFonts);

        SelectObject(FDc, FCurrentFont);

        FNumKerningPairs := 0;

        Result := True;

        Exit;
      end;
    end;
  end;

  Result := False;
end;

function TAggFontEngineWin32TrueTypeBase.CreateFont(ATypeface: AnsiString;
  RenType: TAggGlyphRendering; Height: Double; Width: Double = 0.0;
  Weight: Integer = FW_REGULAR; Italic: Boolean = False;
  CharSet: DWORD = ANSI_CHARSET; APitchAndFamily: DWORD = FF_DONTCARE): Boolean;
begin
  SetHeight(Height);
  SetWidth(Width);
  SetWeight(Weight);
  SetItalic(Italic);
  SetCharSet(CharSet);

  SetPitchAndFamily(APitchAndFamily);

  Result := CreateFont(ATypeface, RenType);
end;

procedure TAggFontEngineWin32TrueTypeBase.SetGamma(Value: TAggCustomVertexSource);
begin
  FRasterizer.Gamma(Value);
end;

procedure TAggFontEngineWin32TrueTypeBase.SetTransform(Mtx: TAggTransAffine);
begin
  FAffine.AssignAll(Mtx);
end;

function TAggFontEngineWin32TrueTypeBase.GetTypeFace: AnsiString;
begin
  Result := FTypeFace;
end;

function TAggFontEngineWin32TrueTypeBase.GetHeight: Double;
begin
  Result := FHeight;
end;

function TAggFontEngineWin32TrueTypeBase.GetWidth: Double;
begin
  Result := FWidth;
end;

function TAggFontEngineWin32TrueTypeBase.GetFontSignature: AnsiString;
begin
  Result := FSignature;
end;

function TAggFontEngineWin32TrueTypeBase.ChangeStamp: Integer;
begin
  Result := FChangeStamp;
end;

function TAggFontEngineWin32TrueTypeBase.PrepareGlyph(GlyphCode: Cardinal): Boolean;
const
  GGO_UNHINTED = $0100; // For compatibility with old SDKs.

var
  Format, TotalSize: Integer;

  Gm: TGLYPHMETRICS;
  Fl: Longint;
  Ts: DWORD;

  Bnd: TRectDouble;
begin
  if (FDc <> 0) and (FCurrentFont <> 0) then
  begin
    Format := GGO_BITMAP;

    case FGlyphRendering of
      grNativeGray8:
        Format := GGO_GRAY8_BITMAP;

      grOutline, grAggMono, grAggGray8:
        Format := GGO_NATIVE;
    end;

    if not FHinting then
      Format := Format or GGO_UNHINTED;

    Ts := GetGlyphOutlineX(FDc, GlyphCode, Format, Gm, CAggBufferSize, FGlyphBuffer,
      FMatrix);

    Move(Ts, TotalSize, SizeOf(Integer));

    if TotalSize < 0 then
    begin
      // GetGlyphOutline() fails when being called for
      // GGO_GRAY8_BITMAP and white space (stupid Microsoft).
      // It doesn't even initialize the glyph metrics
      // structure. So, we have to query the metrics
      // separately (basically we need gmCellIncX).
      Ts := GetGlyphOutlineX(FDc, GlyphCode, GGO_METRICS, Gm, CAggBufferSize,
        FGlyphBuffer, FMatrix);

      Move(Ts, TotalSize, SizeOf(Integer));

      if TotalSize < 0 then
      begin
        Result := False;

        Exit;
      end;

      Gm.GmBlackBoxX := 0;
      Gm.GmBlackBoxY := 0;

      TotalSize := 0;
    end;

    FGlyphIndex := GlyphCode;
    FAdvanceX := Gm.GmCellIncX;
    FAdvanceY := -Gm.GmCellIncY;

    case FGlyphRendering of
      grNativeMono:
        begin
          if FFlipY then
            Fl := -Gm.GmptGlyphOrigin.Y
          else
            Fl := Gm.GmptGlyphOrigin.Y;

          DecomposeWin32GlyphBitmapMono(FGlyphBuffer, Gm.GmBlackBoxX,
            Gm.GmBlackBoxY, Gm.GmptGlyphOrigin.X, Fl, FFlipY, FScanLineBin,
            FScanLinesBin);

          FBounds.X1 := FScanLinesBin.MinimumX;
          FBounds.Y1 := FScanLinesBin.MinimumY;
          FBounds.X2 := FScanLinesBin.MaximumX;
          FBounds.Y2 := FScanLinesBin.MaximumY;
          FDataSize := FScanLinesBin.ByteSize;
          FDataType := gdMono;

          Result := True;

          Exit;
        end;

      grNativeGray8:
        begin
          if FFlipY then
            Fl := -Gm.GmptGlyphOrigin.Y
          else
            Fl := Gm.GmptGlyphOrigin.Y;

          DecomposeWin32GlyphBitmapGray8(FGlyphBuffer, Gm.GmBlackBoxX,
            Gm.GmBlackBoxY, Gm.GmptGlyphOrigin.X, Fl, FFlipY, FRasterizer,
            FScanLineAA_, FScanLinesAA);

          FBounds.X1 := FScanLinesAA.GetMinX;
          FBounds.Y1 := FScanLinesAA.GetMinY;
          FBounds.X2 := FScanLinesAA.GetMaxX;
          FBounds.Y2 := FScanLinesAA.GetMaxY;
          FDataSize := FScanLinesAA.ByteSize;
          FDataType := gdGray8;

          Result := True;

          Exit;
        end;

      grOutline:
        begin
          FAffine.Transform(FAffine, @FAdvanceX, @FAdvanceY);

          if FFlag32 then
          begin
            FPath32.RemoveAll;

            if DecomposeWin32GlyphOutline(FGlyphBuffer, TotalSize, FFlipY,
              FAffine, FPath32) then
            begin
              Bnd := FPath32.GetBoundingRect;

              FDataSize := FPath32.ByteSize;
              FDataType := gdOutline;
              FBounds.X1 := Floor(Bnd.X1);
              FBounds.Y1 := Floor(Bnd.Y1);
              FBounds.X2 := Ceil(Bnd.X2);
              FBounds.Y2 := Ceil(Bnd.Y2);

              Result := True;

              Exit;
            end;

          end
          else
          begin
            FPath16.RemoveAll;

            if DecomposeWin32GlyphOutline(FGlyphBuffer, TotalSize, FFlipY,
              FAffine, FPath16) then
            begin
              Bnd := FPath16.GetBoundingRect;

              FDataSize := FPath16.ByteSize;
              FDataType := gdOutline;
              FBounds.X1 := Floor(Bnd.X1);
              FBounds.Y1 := Floor(Bnd.Y1);
              FBounds.X2 := Ceil(Bnd.X2);
              FBounds.Y2 := Ceil(Bnd.Y2);

              Result := True;

              Exit;
            end;
          end;
        end;

      grAggMono:
        begin
          FRasterizer.Reset;
          FAffine.Transform(FAffine, @FAdvanceX, @FAdvanceY);

          if FFlag32 then
          begin
            FPath32.RemoveAll;

            DecomposeWin32GlyphOutline(FGlyphBuffer, TotalSize, FFlipY,
              FAffine, FPath32);

            FRasterizer.AddPath(FCurves32);
          end
          else
          begin
            FPath16.RemoveAll;

            DecomposeWin32GlyphOutline(FGlyphBuffer, TotalSize, FFlipY,
              FAffine, FPath16);

            FRasterizer.AddPath(FCurves16);
          end;

          FScanLinesBin.Prepare(1); // Remove all

          RenderScanLines(FRasterizer, FScanLineBin, FScanLinesBin);

          FBounds.X1 := FScanLinesBin.MinimumX;
          FBounds.Y1 := FScanLinesBin.MinimumY;
          FBounds.X2 := FScanLinesBin.MaximumX;
          FBounds.Y2 := FScanLinesBin.MaximumY;
          FDataSize := FScanLinesBin.ByteSize;
          FDataType := gdMono;

          Result := True;

          Exit;
        end;

      grAggGray8:
        begin
          FRasterizer.Reset;
          FAffine.Transform(FAffine, @FAdvanceX, @FAdvanceY);

          if FFlag32 then
          begin
            FPath32.RemoveAll;

            DecomposeWin32GlyphOutline(FGlyphBuffer, TotalSize, FFlipY,
              FAffine, FPath32);

            FRasterizer.AddPath(FCurves32);
          end
          else
          begin
            FPath16.RemoveAll;

            DecomposeWin32GlyphOutline(FGlyphBuffer, TotalSize, FFlipY,
              FAffine, FPath16);

            FRasterizer.AddPath(FCurves16);
          end;

          FScanLinesAA.Prepare(1); // Remove all

          RenderScanLines(FRasterizer, FScanLineAA_, FScanLinesAA);

          FBounds.X1 := FScanLinesAA.GetMinX;
          FBounds.Y1 := FScanLinesAA.GetMinY;
          FBounds.X2 := FScanLinesAA.GetMaxX;
          FBounds.Y2 := FScanLinesAA.GetMaxY;
          FDataSize := FScanLinesAA.ByteSize;
          FDataType := gdGray8;

          Result := True;

          Exit;
        end;
    end;
  end;

  Result := False;
end;

function TAggFontEngineWin32TrueTypeBase.GetGlyphIndex: Cardinal;
begin
  Result := FGlyphIndex;
end;

function TAggFontEngineWin32TrueTypeBase.GetDataSize: Cardinal;
begin
  Result := FDataSize;
end;

function TAggFontEngineWin32TrueTypeBase.GetDataType: TAggGlyphData;
begin
  Result := FDataType;
end;

function TAggFontEngineWin32TrueTypeBase.GetBounds: PRectInteger;
begin
  Result := @FBounds;
end;

function TAggFontEngineWin32TrueTypeBase.GetAdvanceX: Double;
begin
  Result := FAdvanceX;
end;

function TAggFontEngineWin32TrueTypeBase.GetAdvanceY: Double;
begin
  Result := FAdvanceY;
end;

procedure TAggFontEngineWin32TrueTypeBase.WriteGlyphTo(Data: PInt8u);
begin
  if (Data <> nil) and (FDataSize <> 0) then
    case FDataType of
      gdMono:
        FScanLinesBin.Serialize(Data);

      gdGray8:
        FScanLinesAA.Serialize(Data);

      gdOutline:
        if FFlag32 then
          FPath32.Serialize(Data)
        else
          FPath16.Serialize(Data);
    end;
end;

function TAggFontEngineWin32TrueTypeBase.AddKerning(First, Second: Cardinal;
  X, Y: PDouble): Boolean;
var
  Stop, Middle, Start: Integer;

  Delta: TPointDouble;

  T : TKerningPair;
  Kp: PKerningPair;
begin
  if (FDc <> 0) and (FCurrentFont <> 0) then
  begin
    if FNumKerningPairs = 0 then
      LoadKerningPairs;

    Stop := FNumKerningPairs;
    Dec(Stop);
    Start := 0;

    T.WFirst := WORD(First);
    T.WSecond := WORD(Second);

    while Start <= Stop do
    begin
      Middle := (Stop + Start) div 2;
      Kp := PKerningPair(PtrComp(FKerningPairs) + Middle *
        SizeOf(TKerningPair));

      if (Kp.WFirst = T.WFirst) and (Kp.WSecond = T.WSecond) then
      begin
        Delta.X := Kp.IKernAmount;
        Delta.Y := 0.0;

        if (FGlyphRendering = grOutline) or
          (FGlyphRendering = grAggMono) or
          (FGlyphRendering = grAggGray8) then
          FAffine.Transform2x2(FAffine, @Delta.X, @Delta.Y);

        X^ := X^ + Delta.X;
        Y^ := Y^ + Delta.Y;

        Result := True;

        Exit;
      end
      else if PairLess(@T, Kp) then
        Stop := Middle - 1
      else
        Start := Middle + 1;
    end;
  end;

  Result := False;
end;

function TAggFontEngineWin32TrueTypeBase.GetFlag32;
begin
  Result := FFlag32;
end;

procedure TAggFontEngineWin32TrueTypeBase.UpdateSignature;
var
  GammaHash, I: Cardinal;
  GammaTable: array [0..CAggAntiAliasingNum - 1] of Int8u;
  MatrixData: TAggParallelogram;
  Str: AnsiString;
begin
  if (FDc <> 0) and (FCurrentFont <> 0) then
  begin
    GammaHash := 0;

    if (FGlyphRendering = grNativeGray8) or
      (FGlyphRendering = grAggMono) or
      (FGlyphRendering = grAggGray8) then
    begin
      for I := 0 to CAggAntiAliasingNum - 1 do
        GammaTable[I] := FRasterizer.ApplyGamma(I);

      GammaHash := CalcCrc32(@GammaTable, SizeOf(GammaTable));
    end;

    Str := Format('%s,%u,%d,%u:%dx%d,%d,%d,%d,%d,%u,%x', [FTypeFace,
      FCharSet, Integer(FGlyphRendering), FResolution, FHeight,
      FWidth, FWeight, Integer(FItalic), Integer(FHinting), Integer(FFlipY),
      FPitchAndFamily, GammaHash]);

    if (FGlyphRendering = grOutline) or
      (FGlyphRendering = grAggMono) or
      (FGlyphRendering = grAggGray8) then
    begin
      FAffine.StoreTo(@MatrixData);

      Str := Str + Format(',%x,%x,%x,%x,%x,%x', [
        DoubleToPlainFixedPoint(MatrixData[0]),
        DoubleToPlainFixedPoint(MatrixData[1]),
        DoubleToPlainFixedPoint(MatrixData[2]),
        DoubleToPlainFixedPoint(MatrixData[3]),
        DoubleToPlainFixedPoint(MatrixData[4]),
        DoubleToPlainFixedPoint(MatrixData[5])]);
    end;

    FSignature := Str;
    Inc(FChangeStamp);
  end;
end;

function TAggFontEngineWin32TrueTypeBase.PairLess(V1, V2: PKerningPair): Boolean;
begin
  if V1.WFirst <> V2.WFirst then
    Result := V1.WFirst < V2.WFirst
  else
    Result := V1.WSecond < V2.WSecond;
end;

procedure TAggFontEngineWin32TrueTypeBase.LoadKerningPairs;
var
  I: Cardinal;
begin
  if (FDc <> 0) and (FCurrentFont <> 0) then
  begin
    if FKerningPairs = nil then
    begin
      FMaxKerningPairs := 16384 - 16;

      AggGetMem(Pointer(FKerningPairs), FMaxKerningPairs *
        SizeOf(TKerningPair));
    end;

    FNumKerningPairs := GetKerningPairs(FDc, FMaxKerningPairs,
      FKerningPairs^);

    if FNumKerningPairs <> 0 then
    begin
      // Check to see if the kerning pairs are sorted and
      // sort them if they are not.
      I := 1;

      while I < FNumKerningPairs do
      begin
        if not PairLess(PKerningPair(PtrComp(FKerningPairs) + (I - 1) *
          SizeOf(TKerningPair)), PKerningPair(PtrComp(FKerningPairs) + I *
          SizeOf(TKerningPair))) then
        begin
          SortKerningPairs;

          Break;
        end;

        Inc(I);
      end;
    end;
  end;
end;

function GetPairLess(V1, V2: PKerningPair): Boolean;
begin
  if V1.WFirst <> V2.WFirst then
    Result := V1.WFirst < V2.WFirst
  else
    Result := V1.WSecond < V2.WSecond;
end;

procedure TAggFontEngineWin32TrueTypeBase.SortKerningPairs;
var
  Pairs: TAggPodArrayAdaptor;
begin
  Pairs := TAggPodArrayAdaptor.Create(FKerningPairs, FNumKerningPairs, SizeOf(TKerningPair));
  try
    QuickSort(Pairs, @GetPairLess);
  finally
    Pairs.Free;
  end;
end;

function TAggFontEngineWin32TrueTypeBase.FindFont(Name: AnsiString): Integer;
var
  I: Cardinal;
  N: PAggFontName;
begin
  N := FFontNames;
  I := 0;

  while I < FNumFonts do
  begin
    if Name = N^ then
    begin
      Result := I;

      Exit;
    end;

    Inc(PtrComp(N), SizeOf(TAggFontName));
    Inc(I);
  end;

  Result := -1;
end;

constructor TAggFontEngineWin32TrueTypeInt16.Create;
begin
  inherited Create(False, Dc, MaxFonts);
end;

constructor TAggFontEngineWin32TrueTypeInt32.Create;
begin
  inherited Create(True, Dc, MaxFonts);
end;

initialization
{$IFDEF AGG_WIN9X_COMPLIANT }
  GetGlyphOutlineX := @GetGlyphOutline;
{$ELSE }
  GetGlyphOutlineX := @GetGlyphOutlineW;
{$ENDIF }

end.
