unit AggFontFreeType;

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
  SysUtils, Math,
  AggFontFreeTypeLib,
  AggBasics,
  AggFontEngine,
  AggFontCacheManager,
  AggTransAffine,
  AggVertexSource,
  AggPathStorageInteger,
  AggConvCurve,
  AggScanLine,
  AggScanlineUnpacked,
  AggScanLineBin,
  AggRasterizerScanLineAA,
  AggRendererScanLine,
  AggRenderScanLines,
  AggBitsetIterator;

type
  PAggFaceName = ^TAggFaceName;
  TAggFaceName = AnsiString;

  TAggFontEngineFreetypeBase = class(TAggCustomFontEngine)
  private
    FFlag32: Boolean;

    FChangeStamp, FLastError: Integer;

    FFaceName: TAggFaceName;
    FFaceIndex: Cardinal;
    FCharMap: TAggFreeTypeEncoding;
    FSignature: TAggFaceName;

    FHeight, FWidth: Cardinal;

    FHinting, FFlipY, FLibraryInitialized: Boolean;

    FLibrary: PAggFreeTypeLibrary; // handle to library
    FFaces: PPAggFreeTypeFace; // A pool of font faces

    FFaceNames: PAggFaceName;
    FNumFaces, FMaxFaces: Cardinal;
    FCurFace: PAggFreeTypeFace; // handle to the current face object
    FResolution: Integer;

    FGlyphRendering: TAggGlyphRendering;
    FGlyphIndex, FDataSize: Cardinal;
    FDataType: TAggGlyphData;

    FBounds: TRectInteger;
    FAdvanceX, FAdvanceY: Double;
    FAffine: TAggTransAffine;

    FPath16: TAggPathStorageInt16;
    FPath32: TAggPathStorageInt32;
    FCurves16, FCurves32: TAggConvCurve;

    FScanLineAA: TAggScanLineUnpacked8;
    FScanLineBin: TAggScanLineBin;
    FScanLinesAA: TAggScanLinesAA;
    FScanLinesBin: TAggScanLinesBin;
    FRasterizer: TAggRasterizerScanLineAA;

    // private
    procedure UpdateCharSize;
    procedure UpdateSignature;

    function FindFace(Name: AnsiString): Integer;

    procedure SetHinting(H: Boolean);
    procedure SetFlipY(Flip: Boolean);
  protected
    function GetGlyphIndex: Cardinal; override;
    function GetDataSize: Cardinal; override;
    function GetDataType: TAggGlyphData; override;
    function GetAdvanceX: Double; override;
    function GetAdvanceY: Double; override;
    function GetFlag32: Boolean; override;
  public
    constructor Create(AFlag32: Boolean; MaxFaces: Cardinal = 32);
    destructor Destroy; override;

    // Set font parameters
    procedure SetResolution(Dpi: Cardinal);

    function LoadFont(FontName: PAnsiChar; FaceIndex: Cardinal;
      RenType: TAggGlyphRendering; FontMem: PAnsiChar = nil;
      FontMemSize: Integer = 0): Boolean;

    function Attach(FileName: PAnsiChar): Boolean;

    function SetCharMap(Map: TAggFreeTypeEncoding): Boolean;
    function SetHeight(H: Double): Boolean;
    function SetWidth(W: Double): Boolean;
    procedure SetTransform(Affine: TAggTransAffine);

    // Set Gamma
    procedure SetGamma(F: TAggCustomVertexSource);

    // Accessors
    function GetLastError: Integer;
    function GetResolution: Cardinal;
    function GetName: AnsiString;
    function GetNum_faces: Cardinal;
    function GetCharMap: TAggFreeTypeEncoding;
    function GetHeight: Double;
    function GetWidth: Double;
    function GetAscender: Double;
    function GetDescender: Double;

    // Interface mandatory to implement for TAggFontCacheManager
    function GetFontSignature: AnsiString; override;
    function GetBounds: PRectInteger; override;
    function ChangeStamp: Integer; override;

    function PrepareGlyph(GlyphCode: Cardinal): Boolean; override;

    procedure WriteGlyphTo(Data: PInt8u); override;
    function AddKerning(First, Second: Cardinal; X, Y: PDouble): Boolean;
      override;

    property FlipY: Boolean read FFlipY write SetFlipY;
    property Hinting: Boolean read FHinting write SetHinting;
  end;

  // ------------------------------------------------FontEngineFreetypeInt16
  // This class uses values of type int16 (10.6 format) for the vector cache.
  // The vector cache is compact, but when rendering glyphs of height
  // more that 200 there integer overfLow can occur.
  TAggFontEngineFreetypeInt16 = class(TAggFontEngineFreetypeBase)
  public
    constructor Create(MaxFaces: Cardinal = 32);
  end;

  // ------------------------------------------------FontEngineFreetypeInt32
  // This class uses values of type int32 (26.6 format) for the vector cache.
  // The vector cache is twice larger than in FontEngineFreetypeInt16,
  // but it allows you to render glyphs of very large sizes.
  TAggFontEngineFreetypeInt32 = class(TAggFontEngineFreetypeBase)
  public
    constructor Create(MaxFaces: Cardinal = 32);
  end;

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

  
function Calc_crc32(Buf: PInt8u; Size: Cardinal): Cardinal;
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

function DoubleToPlainFixedPoint(D: Double): Integer;
begin
  Result := Trunc(D * 65536.0)
end;

function Int26p6ToDouble(P: Integer): Double;
begin
  Result := P / 64.0;
end;

function DoubleToInt26p6(P: Double): Integer;
begin
  Result := Trunc(P * 64.0 + 0.5);
end;

function DecomposeFreeTypeOutline(Outline: PAggFreeTypeOutline; FlipY: Boolean;
  Mtx: TAggTransAffine; Path: TAggCustomPathStorageInteger): Boolean;
var
  V_last, V_control, V_start, Vec, V_middle, Vec1, Vec2: TAggFreeTypeVector;

  X1, Y1, X2, Y2, X3, Y3: Double;

  Point, Limit: PAggFreeTypeVector;

  Tags: PAnsiChar;

  N, // index of contour in outline
  First, // index of first point in contour
  Last: Integer; // index of last point in contour

  Tag: AnsiChar; // current point's state

label
  Do_Conic, Close;

begin
  First := 0;
  N := 0;

  while N < Outline.NumContours do
  begin
    Last := PAggFreeTypeShort(PtrComp(Outline.Contours) + N * SizeOf(TAggFreeTypeShort))^;
    Limit := PAggFreeTypeVector(PtrComp(Outline.Points) + Last * SizeOf(TAggFreeTypeVector));

    V_start := PAggFreeTypeVector(PtrComp(Outline.Points) + First *
      SizeOf(TAggFreeTypeVector))^;
    V_last := PAggFreeTypeVector(PtrComp(Outline.Points) + Last *
      SizeOf(TAggFreeTypeVector))^;

    V_control := V_start;

    Point := PAggFreeTypeVector(PtrComp(Outline.Points) + First * SizeOf(TAggFreeTypeVector));
    Tags := PAnsiChar(PtrComp(Outline.Tags) + First * SizeOf(AnsiChar));
    Tag := FreeTypeCurveTag(Tags^);

    // A contour cannot start with a cubic control point!
    if Tag = AnsiChar(CAggFreeTypeCurveTagCubic) then
    begin
      Result := False;
      Exit;
    end;

    // check first point to determine origin
    if Tag = AnsiChar(CAggFreeTypeCurveTagConic) then
    begin
      // first point is conic control. Yes, this happens.
      if FreeTypeCurveTag(PAnsiChar(PtrComp(Outline.Tags) + Last)^)
        = AnsiChar(CAggFreeTypeCurveTagOn) then
      begin
        // start at last point if it is on the curve
        V_start := V_last;

        Dec(Limit);

      end
      else
      begin
        // if both first and last points are conic,
        // start at their middle and record its position
        // for closure
        V_start.X := (V_start.X + V_last.X) div 2;
        V_start.Y := (V_start.Y + V_last.Y) div 2;

        V_last := V_start;
      end;

      Dec(PtrComp(Point), SizeOf(TAggFreeTypeVector));
      Dec(PtrComp(Tags));
    end;

    X1 := Int26p6ToDouble(V_start.X);
    Y1 := Int26p6ToDouble(V_start.Y);

    if FlipY then
      Y1 := -Y1;

    Mtx.Transform(Mtx, @X1, @Y1);
    Path.MoveTo(DoubleToInt26p6(X1), DoubleToInt26p6(Y1));

    while PtrComp(Point) < PtrComp(Limit) do
    begin
      Inc(PtrComp(Point), SizeOf(TAggFreeTypeVector));
      Inc(PtrComp(Tags));

      Tag := FreeTypeCurveTag(Tags^);

      case Tag of
        // emit a single LineTo
        AnsiChar(CAggFreeTypeCurveTagOn):
          begin
            X1 := Int26p6ToDouble(Point.X);
            Y1 := Int26p6ToDouble(Point.Y);

            if FlipY then
              Y1 := -Y1;

            Mtx.Transform(Mtx, @X1, @Y1);
            Path.LineTo(DoubleToInt26p6(X1), DoubleToInt26p6(Y1));

            Continue;
          end;

        // consume conic arcs
        AnsiChar(CAggFreeTypeCurveTagConic):
          begin
            V_control.X := Point.X;
            V_control.Y := Point.Y;

          Do_Conic:
            if PtrComp(Point) < PtrComp(Limit) then
            begin
              Inc(PtrComp(Point), SizeOf(TAggFreeTypeVector));
              Inc(PtrComp(Tags));

              Tag := FreeTypeCurveTag(Tags^);

              Vec.X := Point.X;
              Vec.Y := Point.Y;

              if Tag = AnsiChar(CAggFreeTypeCurveTagOn) then
              begin
                X1 := Int26p6ToDouble(V_control.X);
                Y1 := Int26p6ToDouble(V_control.Y);
                X2 := Int26p6ToDouble(Vec.X);
                Y2 := Int26p6ToDouble(Vec.Y);

                if FlipY then
                begin
                  Y1 := -Y1;
                  Y2 := -Y2;
                end;

                Mtx.Transform(Mtx, @X1, @Y1);
                Mtx.Transform(Mtx, @X2, @Y2);

                Path.Curve3To(DoubleToInt26p6(X1), DoubleToInt26p6(Y1),
                  DoubleToInt26p6(X2), DoubleToInt26p6(Y2));

                Continue;
              end;

              if Tag <> AnsiChar(CAggFreeTypeCurveTagConic) then
              begin
                Result := False;

                Exit;
              end;

              V_middle.X := (V_control.X + Vec.X) div 2;
              V_middle.Y := (V_control.Y + Vec.Y) div 2;

              X1 := Int26p6ToDouble(V_control.X);
              Y1 := Int26p6ToDouble(V_control.Y);
              X2 := Int26p6ToDouble(V_middle.X);
              Y2 := Int26p6ToDouble(V_middle.Y);

              if FlipY then
              begin
                Y1 := -Y1;
                Y2 := -Y2;
              end;

              Mtx.Transform(Mtx, @X1, @Y1);
              Mtx.Transform(Mtx, @X2, @Y2);

              Path.Curve3To(DoubleToInt26p6(X1), DoubleToInt26p6(Y1),
                DoubleToInt26p6(X2), DoubleToInt26p6(Y2));

              V_control := Vec;

              goto Do_Conic;
            end;

            X1 := Int26p6ToDouble(V_control.X);
            Y1 := Int26p6ToDouble(V_control.Y);
            X2 := Int26p6ToDouble(V_start.X);
            Y2 := Int26p6ToDouble(V_start.Y);

            if FlipY then
            begin
              Y1 := -Y1;
              Y2 := -Y2;
            end;

            Mtx.Transform(Mtx, @X1, @Y1);
            Mtx.Transform(Mtx, @X2, @Y2);

            Path.Curve3To(DoubleToInt26p6(X1), DoubleToInt26p6(Y1),
              DoubleToInt26p6(X2), DoubleToInt26p6(Y2));

            goto Close;
          end;

        // TAggFreeTypeCurveTag_CUBIC
      else
        begin
          if (PtrComp(Point) + SizeOf(TAggFreeTypeVector) > PtrComp(Limit)) or
            (FreeTypeCurveTag(PAnsiChar(PtrComp(Tags) + 1)^) <>
            AnsiChar(CAggFreeTypeCurveTagCubic)) then
          begin
            Result := False;

            Exit;
          end;

          Vec1.X := Point.X;
          Vec1.Y := Point.Y;
          Vec2.X := PAggFreeTypeVector(PtrComp(Point) + SizeOf(TAggFreeTypeVector)).X;
          Vec2.Y := PAggFreeTypeVector(PtrComp(Point) + SizeOf(TAggFreeTypeVector)).Y;

          Inc(PtrComp(Point), 2 * SizeOf(TAggFreeTypeVector));
          Inc(PtrComp(Tags), 2);

          if PtrComp(Point) <= PtrComp(Limit) then
          begin
            Vec.X := Point.X;
            Vec.Y := Point.Y;

            X1 := Int26p6ToDouble(Vec1.X);
            Y1 := Int26p6ToDouble(Vec1.Y);
            X2 := Int26p6ToDouble(Vec2.X);
            Y2 := Int26p6ToDouble(Vec2.Y);
            X3 := Int26p6ToDouble(Vec.X);
            Y3 := Int26p6ToDouble(Vec.Y);

            if FlipY then
            begin
              Y1 := -Y1;
              Y2 := -Y2;
              Y3 := -Y3;
            end;

            Mtx.Transform(Mtx, @X1, @Y1);
            Mtx.Transform(Mtx, @X2, @Y2);
            Mtx.Transform(Mtx, @X3, @Y3);

            Path.Curve4To(DoubleToInt26p6(X1), DoubleToInt26p6(Y1),
              DoubleToInt26p6(X2), DoubleToInt26p6(Y2), DoubleToInt26p6(X3),
              DoubleToInt26p6(Y3));

            Continue;
          end;

          X1 := Int26p6ToDouble(Vec1.X);
          Y1 := Int26p6ToDouble(Vec1.Y);
          X2 := Int26p6ToDouble(Vec2.X);
          Y2 := Int26p6ToDouble(Vec2.Y);
          X3 := Int26p6ToDouble(V_start.X);
          Y3 := Int26p6ToDouble(V_start.Y);

          if FlipY then
          begin
            Y1 := -Y1;
            Y2 := -Y2;
            Y3 := -Y3;
          end;

          Mtx.Transform(Mtx, @X1, @Y1);
          Mtx.Transform(Mtx, @X2, @Y2);
          Mtx.Transform(Mtx, @X3, @Y3);

          Path.Curve4To(DoubleToInt26p6(X1), DoubleToInt26p6(Y1),
            DoubleToInt26p6(X2), DoubleToInt26p6(Y2), DoubleToInt26p6(X3),
            DoubleToInt26p6(Y3));

          goto Close;
        end;
      end;
    end;

    Path.ClosePolygon;

  Close:
    First := Last + 1;

    Inc(N);
  end;

  Result := True;
end;

procedure DecomposeFreeTypeBitmapMono(Bitmap: PAggFreeTypeBitmap; X, Y: Integer;
  FlipY: Boolean; Sl: TAggCustomScanLine; Storage: TAggCustomRendererScanLine);
var
  I, Pitch, J: Integer;

  Buf : PInt8u;
  Bits: TAggBitsetIterator;

begin
  Buf := PInt8u(Bitmap.Buffer);
  Pitch := Bitmap.Pitch;

  Sl.Reset(X, X + Bitmap.Width);
  Storage.Prepare(Bitmap.Width + 2);

  if FlipY then
  begin
    Inc(PtrComp(Buf), Bitmap.Pitch * (Bitmap.Rows - 1));
    Inc(Y, Bitmap.Rows);

    Pitch := -Pitch;
  end;

  I := 0;

  while I < Bitmap.Rows do
  begin
    Sl.ResetSpans;

    J := 0;

    Bits := TAggBitsetIterator.Create(Buf, 0);
    try
      while J < Bitmap.Width do
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

procedure DecomposeFreeTypeBitmapGray8(Bitmap: PAggFreeTypeBitmap; X, Y: Integer;
  FlipY: Boolean; Ras: TAggRasterizerScanLineAA; Sl: TAggCustomScanLine;
  Storage: TAggCustomRendererScanLine);
var
  I, J, Pitch: Integer;

  Buf, P: PInt8u;

begin
  Buf := PInt8u(Bitmap.Buffer);
  Pitch := Bitmap.Pitch;

  Sl.Reset(X, X + Bitmap.Width);
  Storage.Prepare(Bitmap.Width + 2);

  if FlipY then
  begin
    Inc(PtrComp(Buf), Bitmap.Pitch * (Bitmap.Rows - 1));
    Inc(Y, Bitmap.Rows);

    Pitch := -Pitch;
  end;

  I := 0;

  while I < Bitmap.Rows do
  begin
    Sl.ResetSpans;

    P := Buf;
    J := 0;

    while J < Bitmap.Width do
    begin
      if P^ <> 0 then
        Sl.AddCell(X + J, Ras.ApplyGamma(P^));

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


{ TAggFontEngineFreetypeBase }

constructor TAggFontEngineFreetypeBase.Create(AFlag32: Boolean;
  MaxFaces: Cardinal = 32);
begin
  FFlag32 := AFlag32;

  FChangeStamp := 0;
  FLastError := 0;

  FFaceIndex := 0;

  FCharMap := CAggFreeTypeEncodingNone;
  FFaceName := '';

  FSignature := '';

  FHeight := 0;
  FWidth := 0;
  FHinting := True;
  FFlipY := False;

  FLibraryInitialized := False;

  FLibrary := nil;

  AggGetMem(Pointer(FFaces), MaxFaces * SizeOf(PAggFreeTypeFace));
  AggGetMem(Pointer(FFaceNames), MaxFaces * SizeOf(TAggFaceName));

  FNumFaces := 0;
  FMaxFaces := MaxFaces;
  FCurFace := nil;
  FResolution := 0;

  FGlyphRendering := grNativeGray8;
  FGlyphIndex := 0;
  FDataSize := 0;
  FDataType := gdInvalid;

  FBounds := RectInteger(1, 1, 0, 0);

  FAdvanceX := 0.0;
  FAdvanceY := 0.0;

  FAffine := TAggTransAffine.Create;

  FPath16 := TAggPathStorageInt16.Create;
  FPath32 := TAggPathStorageInt32.Create;
  FCurves16 := TAggConvCurve.Create(FPath16);
  FCurves32 := TAggConvCurve.Create(FPath32);
  FScanLineAA := TAggScanLineUnpacked8.Create;
  FScanLineBin := TAggScanLineBin.Create;
  FScanLinesAA := TAggScanLinesAA.Create;
  FScanLinesBin := TAggScanLinesBin.Create;
  FRasterizer := TAggRasterizerScanLineAA.Create;

  FCurves16.ApproximationScale := 4.0;
  FCurves32.ApproximationScale := 4.0;

  FLastError := FreeTypeInit(@FLibrary);

  if FLastError = 0 then
    FLibraryInitialized := True;
end;

destructor TAggFontEngineFreetypeBase.Destroy;
var
  I: Cardinal;
begin
  I := 0;

  while I < FNumFaces do
  begin
    FreeTypeDoneFace(PPAggFreeTypeFace(PtrComp(FFaces) + I * SizeOf(PAggFreeTypeFace))^);

    Inc(I);
  end;

  AggFreeMem(Pointer(FFaceNames), FMaxFaces * SizeOf(TAggFaceName));
  AggFreeMem(Pointer(FFaces), FMaxFaces * SizeOf(PAggFreeTypeFace));

  if FLibraryInitialized then
    FreeTypeDone(FLibrary);

  FAffine.Free;
  FPath16.Free;
  FPath32.Free;
  FCurves16.Free;
  FCurves32.Free;
  FScanLineAA.Free;
  FScanLineBin.Free;

  FScanLinesAA.Free;
  FScanLinesBin.Free;
  FRasterizer.Free;

  inherited;
end;

procedure TAggFontEngineFreetypeBase.SetResolution(Dpi: Cardinal);
begin
  FResolution := Dpi;

  UpdateCharSize;
end;

function TAggFontEngineFreetypeBase.LoadFont(FontName: PAnsiChar;
  FaceIndex: Cardinal; RenType: TAggGlyphRendering; FontMem: PAnsiChar = nil;
  FontMemSize: Integer = 0): Boolean;
var
  Idx: Integer;
begin
  Result := False;

  if FLibraryInitialized then
  begin
    FLastError := 0;

    Idx := FindFace(FontName);

    if Idx >= 0 then
    begin
      FCurFace := PPAggFreeTypeFace(PtrComp(FFaces) + Idx *
        SizeOf(PAggFreeTypeFace))^;
      FFaceName := PAggFaceName(PtrComp(FFaceNames) + Idx * SizeOf(TAggFaceName))^;
    end
    else
    begin
      if FNumFaces >= FMaxFaces then
      begin
        FreeTypeDoneFace(FFaces^);

        Move(PPAggFreeTypeFace(PtrComp(FFaces) + SizeOf(PAggFreeTypeFace))^, FFaces^,
          (FMaxFaces - 1) * SizeOf(PAggFreeTypeFace));

        Move(PAggFaceName(PtrComp(FFaceNames) + SizeOf(TAggFaceName))^,
          FFaceNames^, (FMaxFaces - 1) * SizeOf(TAggFaceName));

        Dec(FNumFaces);
      end;

      if (FontMem <> nil) and (FontMemSize > 0) then
        FLastError := FreeTypeNewMemoryFace(FLibrary, PAggFreeTypeByte(FontMem),
          FontMemSize, FaceIndex,
          PPAggFreeTypeFace(PtrComp(FFaces) + FNumFaces *
          SizeOf(PAggFreeTypeFace))^)
      else
        FLastError := FreeTypeNewFace(FLibrary, FontName, FaceIndex,
          PPAggFreeTypeFace(PtrComp(FFaces) + FNumFaces *
          SizeOf(PAggFreeTypeFace))^);

      if FLastError = 0 then
      begin
(*
        PAggFaceName(PtrComp(FFaceNames) +
          FNumFaces * SizeOf(TAggFaceName)).Size := StrLen(FontName) + 1;

        AggGetMem(Pointer(PAggFaceName(PtrComp(FFaceNames) + FNumFaces *
          SizeOf(TAggFaceName)).Name),
          PAggFaceName(PtrComp(FFaceNames) + FNumFaces *
          SizeOf(TAggFaceName)).Size);

        StrCopy(PAnsiChar(PAggFaceName(PtrComp(FFaceNames) + FNumFaces *
          SizeOf(TAggFaceName)).Name), FontName);
*)

        FCurFace := PPAggFreeTypeFace(PtrComp(FFaces) + FNumFaces *
          SizeOf(PAggFreeTypeFace))^;
(*
        FName := PAnsiChar(PAggFaceName(PtrComp(FFaceNames) + FNumFaces *
          SizeOf(TAggFaceName)).Name);
*)

        Inc(FNumFaces);
      end
      else
      begin
(*
        PAnsiChar(PAggFaceName(PtrComp(FFaceNames) + FNumFaces *
          SizeOf(TAggFaceName)).Name) := nil;
*)

        FCurFace := nil;
        FFaceName := '';
      end;
    end;

    if FLastError = 0 then
    begin
      Result := True;

      case RenType of
        grNativeMono:
          FGlyphRendering := grNativeMono;

        grNativeGray8:
          FGlyphRendering := grNativeGray8;

        grOutline:
          if FreeTypeIsScalable(FCurFace) then
            FGlyphRendering := grOutline
          else
            FGlyphRendering := grNativeGray8;

        grAggMono:
          if FreeTypeIsScalable(FCurFace) then
            FGlyphRendering := grAggMono
          else
            FGlyphRendering := grNativeMono;

        grAggGray8:
          if FreeTypeIsScalable(FCurFace) then
            FGlyphRendering := grAggGray8
          else
            FGlyphRendering := grNativeGray8;
      end;

      UpdateSignature;
    end;
  end;
end;

function TAggFontEngineFreetypeBase.Attach(FileName: PAnsiChar): Boolean;
begin
  Result := False;

  if FCurFace <> nil then
  begin
    FLastError := FreeTypeAttachFile(FCurFace, FileName);

    Result := FLastError = 0;
  end;
end;

function TAggFontEngineFreetypeBase.SetCharMap(Map: TAggFreeTypeEncoding): Boolean;
begin
  Result := False;

  if FCurFace <> nil then
  begin
    FLastError := FreeTypeSelectCharmap(FCurFace, Map);

    if FLastError = 0 then
    begin
      FCharMap := Map;

      UpdateSignature;

      Result := True;
    end;
  end;
end;

function TAggFontEngineFreetypeBase.SetHeight(H: Double): Boolean;
begin
  Result := False;
  FHeight := Trunc(H * 64.0);

  if FCurFace <> nil then
  begin
    UpdateCharSize;

    Result := True;
  end;
end;

function TAggFontEngineFreetypeBase.SetWidth(W: Double): Boolean;
begin
  Result := False;
  FWidth := Trunc(W * 64.0);

  if FCurFace <> nil then
  begin
    UpdateCharSize;

    Result := True;
  end;
end;

procedure TAggFontEngineFreetypeBase.SetHinting(H: Boolean);
begin
  FHinting := H;

  if FCurFace <> nil then
    UpdateSignature;
end;

procedure TAggFontEngineFreetypeBase.SetFlipY(Flip: Boolean);
begin
  FFlipY := Flip;

  if FCurFace <> nil then
    UpdateSignature;
end;

procedure TAggFontEngineFreetypeBase.SetTransform(Affine: TAggTransAffine);
begin
  FAffine.AssignAll(Affine);

  if FCurFace <> nil then
    UpdateSignature;
end;

procedure TAggFontEngineFreetypeBase.SetGamma(F: TAggCustomVertexSource);
begin
  FRasterizer.Gamma(F);
end;

function TAggFontEngineFreetypeBase.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TAggFontEngineFreetypeBase.GetResolution: Cardinal;
begin
  Result := FResolution;
end;

function TAggFontEngineFreetypeBase.GetName: AnsiString;
begin
  Result := FFaceName;
end;

function TAggFontEngineFreetypeBase.GetNum_faces: Cardinal;
begin
  if FCurFace <> nil then
    Result := FCurFace.NumFaces
  else
    Result := 0;
end;

function TAggFontEngineFreetypeBase.GetCharMap: TAggFreeTypeEncoding;
begin
  Result := FCharMap;
end;

function TAggFontEngineFreetypeBase.GetHeight: Double;
begin
  Result := FHeight / 64.0;
end;

function TAggFontEngineFreetypeBase.GetWidth: Double;
begin
  Result := FWidth / 64.0;
end;

function TAggFontEngineFreetypeBase.GetAscender: Double;
begin
  if FCurFace <> nil then
    Result := FCurFace.Ascender * GetHeight / FCurFace.Height
  else
    Result := 0.0;
end;

function TAggFontEngineFreetypeBase.GetDescender: Double;
begin
  if FCurFace <> nil then
    Result := FCurFace.Descender * GetHeight / FCurFace.Height
  else
    Result := 0.0;
end;

function TAggFontEngineFreetypeBase.GetFontSignature: AnsiString;
begin
  Result := FSignature;
end;

function TAggFontEngineFreetypeBase.ChangeStamp: Integer;
begin
  Result := FChangeStamp;
end;

function TAggFontEngineFreetypeBase.PrepareGlyph(GlyphCode: Cardinal): Boolean;
var
  Fl: Integer;
  Bnd: TRectDouble;
begin
  Result := False;

  FGlyphIndex := FreeTypeGetCharIndex(FCurFace, GlyphCode);

  if FHinting then
    FLastError := FreeTypeLoadGlyph(FCurFace, FGlyphIndex,
      CAggFreeTypeLoadDefault { } { FT_LOAD_FORCE_AUTOHINT{ } )
  else
    FLastError := FreeTypeLoadGlyph(FCurFace, FGlyphIndex,
      CAggFreeTypeLoadNoHinting);

  if FLastError = 0 then
    case FGlyphRendering of
      grNativeMono:
        begin
          FLastError := FreeTypeRenderGlyph(FCurFace.Glyph,
            CAggFreeTypeRenderModeMono);

          if FLastError = 0 then
          begin
            if FFlipY then
              Fl := -FCurFace.Glyph.BitmapTop
            else
              Fl := FCurFace.Glyph.BitmapTop;

            DecomposeFreeTypeBitmapMono(@FCurFace.Glyph.Bitmap,
              FCurFace.Glyph.BitmapLeft, Fl, FFlipY, FScanLineBin,
              FScanLinesBin);

            FBounds.X1 := FScanLinesBin.MinimumX;
            FBounds.Y1 := FScanLinesBin.MinimumY;
            FBounds.X2 := FScanLinesBin.MaximumX;
            FBounds.Y2 := FScanLinesBin.MaximumY;
            FDataSize := FScanLinesBin.ByteSize;
            FDataType := gdMono;
            FAdvanceX := Int26p6ToDouble(FCurFace.Glyph.Advance.X);
            FAdvanceY := Int26p6ToDouble(FCurFace.Glyph.Advance.Y);

            Result := True;
          end;
        end;

      grNativeGray8:
        begin
          FLastError := FreeTypeRenderGlyph(FCurFace.Glyph,
            CAggFreeTypeRenderModeNormal);

          if FLastError = 0 then
          begin
            if FFlipY then
              Fl := -FCurFace.Glyph.BitmapTop
            else
              Fl := FCurFace.Glyph.BitmapTop;

            DecomposeFreeTypeBitmapGray8(@FCurFace.Glyph.Bitmap,
              FCurFace.Glyph.BitmapLeft, Fl, FFlipY, FRasterizer,
              FScanLineAA, FScanLinesAA);

            FBounds.X1 := FScanLinesAA.MinimumX;
            FBounds.Y1 := FScanLinesAA.MinimumY;
            FBounds.X2 := FScanLinesAA.MaximumX;
            FBounds.Y2 := FScanLinesAA.MaximumY;
            FDataSize := FScanLinesAA.ByteSize;
            FDataType := gdGray8;
            FAdvanceX := Int26p6ToDouble(FCurFace.Glyph.Advance.X);
            FAdvanceY := Int26p6ToDouble(FCurFace.Glyph.Advance.Y);

            Result := True;
          end;
        end;

      grOutline:
        if FLastError = 0 then
          if FFlag32 then
          begin
            FPath32.RemoveAll;

            if DecomposeFreeTypeOutline(@FCurFace.Glyph.Outline, FFlipY,
              FAffine, FPath32) then
            begin
              Bnd := FPath32.GetBoundingRect;

              FDataSize := FPath32.ByteSize;
              FDataType := gdOutline;
              FBounds.X1 := Floor(Bnd.X1);
              FBounds.Y1 := Floor(Bnd.Y1);
              FBounds.X2 := Ceil(Bnd.X2);
              FBounds.Y2 := Ceil(Bnd.Y2);
              FAdvanceX := Int26p6ToDouble(FCurFace.Glyph.Advance.X);
              FAdvanceY := Int26p6ToDouble(FCurFace.Glyph.Advance.Y);

              FAffine.Transform(FAffine, @FAdvanceX, @FAdvanceY);

              Result := True;
            end;

          end
          else
          begin
            FPath16.RemoveAll;

            if DecomposeFreeTypeOutline(@FCurFace.Glyph.Outline, FFlipY,
              FAffine, FPath16) then
            begin
              Bnd := FPath16.GetBoundingRect;

              FDataSize := FPath16.ByteSize;
              FDataType := gdOutline;
              FBounds.X1 := Floor(Bnd.X1);
              FBounds.Y1 := Floor(Bnd.Y1);
              FBounds.X2 := Ceil(Bnd.X2);
              FBounds.Y2 := Ceil(Bnd.Y2);
              FAdvanceX := Int26p6ToDouble(FCurFace.Glyph.Advance.X);
              FAdvanceY := Int26p6ToDouble(FCurFace.Glyph.Advance.Y);

              FAffine.Transform(FAffine, @FAdvanceX, @FAdvanceY);

              Result := True;
            end;
          end;

      grAggMono:
        if FLastError = 0 then
        begin
          FRasterizer.Reset;

          if FFlag32 then
          begin
            FPath32.RemoveAll;

            DecomposeFreeTypeOutline(@FCurFace.Glyph.Outline, FFlipY, FAffine,
              FPath32);

            FRasterizer.AddPath(FCurves32);
          end
          else
          begin
            FPath16.RemoveAll;

            DecomposeFreeTypeOutline(@FCurFace.Glyph.Outline, FFlipY, FAffine,
              FPath16);

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
          FAdvanceX := Int26p6ToDouble(FCurFace.Glyph.Advance.X);
          FAdvanceY := Int26p6ToDouble(FCurFace.Glyph.Advance.Y);

          FAffine.Transform(FAffine, @FAdvanceX, @FAdvanceY);

          Result := True;
        end;

      grAggGray8:
        if FLastError = 0 then
        begin
          FRasterizer.Reset;

          if FFlag32 then
          begin
            FPath32.RemoveAll;

            DecomposeFreeTypeOutline(@FCurFace.Glyph.Outline, FFlipY, FAffine,
              FPath32);

            FRasterizer.AddPath(FCurves32);

          end
          else
          begin
            FPath16.RemoveAll;

            DecomposeFreeTypeOutline(@FCurFace.Glyph.Outline, FFlipY, FAffine,
              FPath16);

            FRasterizer.AddPath(FCurves16);
          end;

          FScanLinesAA.Prepare(1); // Remove all

          RenderScanLines(FRasterizer, FScanLineAA, FScanLinesAA);

          FBounds.X1 := FScanLinesAA.MinimumX;
          FBounds.Y1 := FScanLinesAA.MinimumY;
          FBounds.X2 := FScanLinesAA.MaximumX;
          FBounds.Y2 := FScanLinesAA.MaximumY;
          FDataSize := FScanLinesAA.ByteSize;
          FDataType := gdGray8;
          FAdvanceX := Int26p6ToDouble(FCurFace.Glyph.Advance.X);
          FAdvanceY := Int26p6ToDouble(FCurFace.Glyph.Advance.Y);

          FAffine.Transform(FAffine, @FAdvanceX, @FAdvanceY);

          Result := True;
        end;
    end;
end;

function TAggFontEngineFreetypeBase.GetGlyphIndex: Cardinal;
begin
  Result := FGlyphIndex;
end;

function TAggFontEngineFreetypeBase.GetDataSize: Cardinal;
begin
  Result := FDataSize;
end;

function TAggFontEngineFreetypeBase.GetDataType: TAggGlyphData;
begin
  Result := FDataType;
end;

function TAggFontEngineFreetypeBase.GetBounds: PRectInteger;
begin
  Result := @FBounds;
end;

function TAggFontEngineFreetypeBase.GetAdvanceX: Double;
begin
  Result := FAdvanceX;
end;

function TAggFontEngineFreetypeBase.GetAdvanceY: Double;
begin
  Result := FAdvanceY;
end;

procedure TAggFontEngineFreetypeBase.WriteGlyphTo(Data: PInt8u);
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

function TAggFontEngineFreetypeBase.AddKerning(First, Second: Cardinal;
  X, Y: PDouble): Boolean;
var
  Delta : TAggFreeTypeVector;
  Dx, Dy: Double;
begin
  if (FCurFace <> nil) and (First <> 0) and (Second <> 0) and
    FreeTypeHasKerning(FCurFace) then
  begin
    FreeTypeGetKerning(FCurFace, First, Second, CAggFreeTypeKerningDefault, @Delta);

    Dx := Int26p6ToDouble(Delta.X);
    Dy := Int26p6ToDouble(Delta.Y);

    if (FGlyphRendering = grOutline) or
      (FGlyphRendering = grAggMono) or
      (FGlyphRendering = grAggGray8) then
      FAffine.Transform2x2(FAffine, @Dx, @Dy);

    X^ := X^ + Dx;
    Y^ := Y^ + Dy;

    Result := True;

  end
  else
    Result := False;
end;

function TAggFontEngineFreetypeBase.GetFlag32;
begin
  Result := FFlag32;
end;

procedure TAggFontEngineFreetypeBase.UpdateCharSize;
begin
  if FCurFace <> nil then
  begin
    if FResolution <> 0 then
      FreeTypeSetCharSize(FCurFace, FWidth, // char_width in 1/64th of points
        FHeight, // char_height in 1/64th of points
        FResolution, // horizontal device resolution
        FResolution) // vertical device resolution
    else
      FreeTypeSetPixelSizes(FCurFace, FWidth shr 6, // pixel_width
        FHeight shr 6); // pixel_height

    UpdateSignature;
  end;
end;

procedure TAggFontEngineFreetypeBase.UpdateSignature;
var
  NameLength, GammaHash, I: Cardinal;

  GammaTable: array [0..CAggAntiAliasingNum - 1] of Int8u;

  MatrixData: TAggParallelogram;
  Str: AnsiString;
begin
  if (FCurFace <> nil) and (FFaceName <> '') then
  begin
    NameLength := Length(FFaceName);

    GammaHash := 0;

    if (FGlyphRendering = grNativeGray8) or
      (FGlyphRendering = grAggMono) or
      (FGlyphRendering = grAggGray8) then
    begin
      for I := 0 to CAggAntiAliasingNum - 1 do
        GammaTable[I] := FRasterizer.ApplyGamma(I);

      GammaHash := Calc_crc32(@GammaTable, SizeOf(GammaTable));
    end;

    Str := Format('%s,%u,%d,%d,%d:%dx%d,%d,%d,%x', [FFaceName,
      FCharMap, FFaceIndex, Integer(FGlyphRendering), FResolution, FHeight,
      FWidth, Integer(FHinting), Integer(FFlipY), GammaHash]);

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

function TAggFontEngineFreetypeBase.FindFace(Name: AnsiString): Integer;
var
  I: Cardinal;
  N: PAggFaceName;
begin
  Result := -1;

  N := FFaceNames;
  I := 0;

  while I < FNumFaces do
  begin
    if Name = N^ then
    begin
      Result := I;

      Exit;
    end;

    Inc(PtrComp(N), SizeOf(TAggFaceName));
    Inc(I);
  end;
end;


{ TAggFontEngineFreetypeInt16 }

constructor TAggFontEngineFreetypeInt16.Create(MaxFaces: Cardinal = 32);
begin
  inherited Create(False, MaxFaces);
end;


{ TAggFontEngineFreetypeInt32 }

constructor TAggFontEngineFreetypeInt32.Create(MaxFaces: Cardinal = 32);
begin
  inherited Create(True, MaxFaces);
end;

end.
