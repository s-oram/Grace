unit AggRendererRasterText;

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
  AggColor,
  AggRendererBase,
  AggRendererScanLine,
  AggScanLine,
  AggGlyphRasterBin;

type
  TAggRendererRasterHorizontalTextSolid = class
  private
    FRenderBase: TAggRendererBase;
    FGlyph: TAggGlyphRasterBin;
    FColor: TAggColor;
  public
    constructor Create(Ren: TAggRendererBase; Glyph: TAggGlyphRasterBin);

    procedure SetColor(C: PAggColor); overload;
    procedure SetColor(C: TAggRgba8); overload;
    function GetColor: PAggColor;

    procedure RenderText(FX, Y: Double; Str: PAnsiChar; Flip: Boolean = False);
  end;

  TAggRendererRasterVerticalTextSolid = class
  private
    FRenderBase: TAggRendererBase;
    FGlyph: TAggGlyphRasterBin;
    FColor: TAggColor;
  public
    constructor Create(Ren: TAggRendererBase; Glyph: TAggGlyphRasterBin);

    procedure SetColor(C: PAggColor);
    function GetColor: PAggColor;

    procedure RenderText(FX, Y: Double; Str: PAnsiChar; Flip: Boolean = False);
  end;

  PAggConstSpan = ^TAggConstSpan;
  TAggConstSpan = record
    X, Len: Int16;
    Covers: PInt8u;
  public
    procedure Initialize(X: Integer; Len: Cardinal; Covers: PInt8u);
  end;

  TAggScanLineSingleSpan = class(TAggCustomScanLine)
  private
    FY: Integer;
    FSpan: TAggConstSpan;
  protected
    function GetY: Integer; override;
    function GetNumSpans: Cardinal; override;
    function GetSizeOfSpan: Cardinal; override;
  public
    constructor Create(X, Y: Integer; Len: Cardinal; Covers: PInt8u);

    function GetBegin: Pointer; override;
  end;

  TAggRendererRasterHorizontalText = class
  private
    FRenderBase: TAggCustomRendererScanLine;
    FGlyph: TAggGlyphRasterBin;
  public
    constructor Create(Ren: TAggCustomRendererScanLine;
      Glyph: TAggGlyphRasterBin);

    procedure RenderText(FX, Y: Double; Str: PAnsiChar; Flip: Boolean = False);
  end;

implementation


{ TAggRendererRasterHorizontalTextSolid }

constructor TAggRendererRasterHorizontalTextSolid.Create(Ren: TAggRendererBase;
  Glyph: TAggGlyphRasterBin);
begin
  Assert(Ren is TAggRendererBase);

  FRenderBase := Ren;
  FGlyph := Glyph;
end;

procedure TAggRendererRasterHorizontalTextSolid.SetColor(C: PAggColor);
begin
  FColor := C^;
end;

procedure TAggRendererRasterHorizontalTextSolid.SetColor(C: TAggRgba8);
begin
  FColor.FromRgba8(C);
end;

function TAggRendererRasterHorizontalTextSolid.GetColor: PAggColor;
begin
  Result := @FColor;
end;

procedure TAggRendererRasterHorizontalTextSolid.RenderText(FX, Y: Double; Str: PAnsiChar;
  Flip: Boolean = False);
var
  R: TAggGlyphRect;
  I: Integer;
begin
  while PInt8u(Str)^ <> 0 do
  begin
    FGlyph.Prepare(@R, FX, Y, PInt8u(Str)^, Flip);

    if R.X2 >= R.X1 then
      if Flip then
      begin
        I := R.Y1;

        while I <= R.Y2 do
        begin
          FRenderBase.BlendSolidHSpan(R.X1, I, R.X2 - R.X1 + 1, @FColor,
            FGlyph.Span(R.Y2 - I));

          Inc(I);
        end;

      end
      else
      begin
        I := R.Y1;

        while I <= R.Y2 do
        begin
          FRenderBase.BlendSolidHSpan(R.X1, I, R.X2 - R.X1 + 1, @FColor,
            FGlyph.Span(I - R.Y1));

          Inc(I);
        end;
      end;

    FX := FX + R.Dx;
    Y := Y + R.Dy;

    Inc(PtrComp(Str));
  end;
end;


{ TAggRendererRasterVerticalTextSolid }

constructor TAggRendererRasterVerticalTextSolid.Create(Ren: TAggRendererBase; Glyph: TAggGlyphRasterBin);
begin
  FRenderBase := Ren;
  FGlyph := Glyph;
end;

procedure TAggRendererRasterVerticalTextSolid.SetColor(C: PAggColor);
begin
  FColor := C^;
end;

function TAggRendererRasterVerticalTextSolid.GetColor: PAggColor;
begin
  Result := @FColor;
end;

procedure TAggRendererRasterVerticalTextSolid.RenderText(FX, Y: Double; Str: PAnsiChar;
  Flip: Boolean = False);
var
  R: TAggGlyphRect;
  I: Integer;
begin
  while PInt8u(Str)^ <> 0 do
  begin
    FGlyph.Prepare(@R, FX, Y, PInt8u(Str)^, Flip);

    if R.X2 >= R.X1 then
      if Flip then
      begin
        I := R.Y1;

        while I <= R.Y2 do
        begin
          FRenderBase.BlendSolidVSpan(I, R.X1, R.X2 - R.X1 + 1, @FColor,
            FGlyph.Span(I - R.Y1));

          Inc(I);
        end;
      end
      else
      begin
        I := R.Y1;

        while I <= R.Y2 do
        begin
          FRenderBase.BlendSolidVSpan(I, R.X1, R.X2 - R.X1 + 1, @FColor,
            FGlyph.Span(R.Y2 - I));

          Inc(I);
        end;
      end;

    FX := FX + R.Dx;
    Y := Y + R.Dy;

    Inc(PtrComp(Str));
  end;
end;


{ TAggConstSpan }

procedure TAggConstSpan.Initialize(X: Integer; Len: Cardinal; Covers: PInt8u);
begin
  Self.X := X;
  Self.Len := Len;
  Self.Covers := Covers;
end;


{ TAggScanLineSingleSpan }

constructor TAggScanLineSingleSpan.Create(X, Y: Integer; Len: Cardinal;
  Covers: PInt8u);
begin
  FY := Y;

  FSpan.Initialize(X, Len, Covers);
end;

function TAggScanLineSingleSpan.GetY;
begin
  Result := FY;
end;

function TAggScanLineSingleSpan.GetNumSpans;
begin
  Result := 1;
end;

function TAggScanLineSingleSpan.GetBegin;
begin
  Result := @FSpan;
end;

function TAggScanLineSingleSpan.GetSizeOfSpan;
begin
  Result := SizeOf(TAggConstSpan);
end;


{ TAggRendererRasterHorizontalText }

constructor TAggRendererRasterHorizontalText.Create;
begin
  FRenderBase := Ren;
  FGlyph := Glyph;
end;

procedure TAggRendererRasterHorizontalText.RenderText;
var
  R: TAggGlyphRect;
  I: Integer;
  S: TAggScanLineSingleSpan;
begin
  while PInt8u(Str)^ <> 0 do
  begin
    FGlyph.Prepare(@R, FX, Y, PInt8u(Str)^, Flip);

    if R.X2 >= R.X1 then
    begin
      FRenderBase.Prepare(R.X2 - R.X1 + 1);

      if Flip then
      begin
        I := R.Y1;

        while I <= R.Y2 do
        begin
          S := TAggScanLineSingleSpan.Create(R.X1, I, R.X2 - R.X1 + 1,
            FGlyph.Span(R.Y2 - I));
          try
            FRenderBase.Render(S);
          finally
            S.Free;
          end;
          Inc(I);
        end;

      end
      else
      begin
        I := R.Y1;

        while I <= R.Y2 do
        begin
          S := TAggScanLineSingleSpan.Create(R.X1, I, (R.X2 - R.X1 + 1),
            FGlyph.Span(I - R.Y1));
          try
            FRenderBase.Render(S);
          finally
            S.Free;
          end;
          Inc(I);
        end;
      end;
    end;

    FX := FX + R.Dx;
    Y := Y + R.Dy;

    Inc(PtrComp(Str));
  end;
end;

end.
