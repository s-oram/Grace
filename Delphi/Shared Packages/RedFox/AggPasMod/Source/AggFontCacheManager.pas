unit AggFontCacheManager;

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
  SysUtils,
  AggBasics,
  AggArray,
  AggFontEngine,
  AggPathStorageInteger;

const
  CAggBlockSize = 16384 - 16;

type
  PPAggGlyphCache = ^PAggGlyphCache;
  PAggGlyphCache = ^TAggGlyphCache;
  TAggGlyphCache = record
    GlyphIndex: Cardinal;
    Data: PInt8u;
    DataSize: Cardinal;
    DataType: TAggGlyphData;
    Bounds: TRectInteger;
    AdvanceX, AdvanceY: Double;
  end;

  TAggGlyphRendering = (grNativeMono, grNativeGray8, grOutline, grAggMono,
    grAggGray8);

  PAggFontCache = ^TAggFontCache;
  TAggFontCache = class
  private
    FAllocator: TAggPodAllocator;
    FGlyphs: array [0..256] of PPAggGlyphCache;
    FFontSignature: AnsiString;
  public
    constructor Create(FontSignature: AnsiString);
    destructor Destroy; override;

    function FontIs(FontSignature: AnsiString): Boolean;

    function FindGlyph(GlyphCode: Cardinal): PAggGlyphCache;

    function CacheGlyph(GlyphCode, GlyphIndex, DataSize: Cardinal;
      DataType: TAggGlyphData; var Bounds: TRectInteger; AdvanceX, AdvanceY: Double)
      : PAggGlyphCache;
  end;

  TAggFontCachePool = class
  private
    FFonts: PAggFontCache;
    FMaxFonts, FNumFonts: Cardinal;
    FCurrentFont: TAggFontCache;
  public
    constructor Create(MaxFonts: Cardinal = 32);
    destructor Destroy; override;

    procedure SetFont(FontSignature: AnsiString; ResetCache: Boolean = False);
    function GetFont: TAggFontCache;

    function FindGlyph(GlyphCode: Cardinal): PAggGlyphCache;

    function CacheGlyph(GlyphCode, GlyphIndex, DataSize: Cardinal;
      DataType: TAggGlyphData; var Bounds: TRectInteger; AdvanceX, AdvanceY: Double)
      : PAggGlyphCache;

    function FindFont(FontSignature: AnsiString): Integer;
  end;

  TAggFontCacheManager = class
  private
    FFonts: TAggFontCachePool;
    FEngine: TAggCustomFontEngine;

    FChangeStamp: Integer;

    FPrevGlyph, FLastGlyph: PAggGlyphCache;

    FPathAdaptor: TAggCustomSerializedIntegerPathAdaptor;
    FGray8Adaptor: TAggGray8Adaptor;
    FGray8ScanLine: TAggGray8ScanLine;
    FMonoAdaptor: TAggMonoAdaptor;
    FMonoScanLine: TAggMonoScanLine;
  protected
    procedure Synchronize;
  public
    constructor Create(Engine: TAggCustomFontEngine; MaxFonts: Cardinal = 32);
    destructor Destroy; override;

    function Glyph(GlyphCode: Cardinal): PAggGlyphCache;

    procedure InitEmbeddedAdaptors(Gl: PAggGlyphCache; X, Y: Double;
      Scale: Double = 1.0);

    function PrevGlyph: PAggGlyphCache;
    function LastGlyph: PAggGlyphCache;

    function AddKerning(X, Y: PDouble): Boolean;

    procedure PreCache(AFrom, ATo: Cardinal);
    procedure ResetCache;

    property Gray8Adaptor: TAggGray8Adaptor read FGray8Adaptor;
    property MonoAdaptor: TAggMonoAdaptor read FMonoAdaptor;
    property PathAdaptor: TAggCustomSerializedIntegerPathAdaptor read FPathAdaptor;
    property Gray8ScanLine: TAggGray8ScanLine read FGray8ScanLine;
    property MonoScanLine: TAggMonoScanLine read FMonoScanLine;
  end;

implementation


{ TAggFontCache }

constructor TAggFontCache.Create(FontSignature: AnsiString);
begin
  FAllocator := TAggPodAllocator.Create(CAggBlockSize);

  FFontSignature := FontSignature;
  FillChar(FGlyphs, SizeOf(FGlyphs), 0);
end;

destructor TAggFontCache.Destroy;
begin
  FAllocator.Free;
  inherited;
end;

function TAggFontCache.FontIs(FontSignature: AnsiString): Boolean;
begin
  Result := FontSignature = FFontSignature;
  inherited;
end;

function TAggFontCache.FindGlyph(GlyphCode: Cardinal): PAggGlyphCache;
var
  Msb: Cardinal;
begin
  Msb := (GlyphCode shr 8) and $FF;

  if FGlyphs[Msb] <> nil then
    Result := PPAggGlyphCache(PtrComp(FGlyphs[Msb]) + (GlyphCode and $FF)
      * SizeOf(PAggGlyphCache))^
  else
    Result := nil;
end;

function TAggFontCache.CacheGlyph;
var
  Msb, Lsb: Cardinal;
  Glyph: PAggGlyphCache;
begin
  Msb := (GlyphCode shr 8) and $FF;

  if FGlyphs[Msb] = nil then
  begin
    FGlyphs[Msb] := PPAggGlyphCache
      (FAllocator.Allocate(SizeOf(PAggGlyphCache) * 256,
      SizeOf(PAggGlyphCache)));

    FillChar(FGlyphs[Msb]^, SizeOf(PAggGlyphCache) * 256, 0);
  end;

  Lsb := GlyphCode and $FF;

  if PPAggGlyphCache(PtrComp(FGlyphs[Msb]) + Lsb * SizeOf(PAggGlyphCache))
    ^ <> nil then
  begin
    Result := nil; // Already exists, do not overwrite

    Exit;
  end;

  Glyph := PAggGlyphCache(FAllocator.Allocate(SizeOf(TAggGlyphCache),
    SizeOf(Double)));

  Glyph.GlyphIndex := GlyphIndex;
  Glyph.Data := FAllocator.Allocate(DataSize);
  Glyph.DataSize := DataSize;
  Glyph.DataType := DataType;
  Glyph.Bounds := Bounds;
  Glyph.AdvanceX := AdvanceX;
  Glyph.AdvanceY := AdvanceY;

  PPAggGlyphCache(PtrComp(FGlyphs[Msb]) + Lsb * SizeOf(PAggGlyphCache))
    ^ := Glyph;

  Result := Glyph;
end;

{ TAggFontCachePool }

constructor TAggFontCachePool.Create;
begin
  AggGetMem(Pointer(FFonts), MaxFonts * SizeOf(TAggFontCache));

  FMaxFonts := MaxFonts;
  FNumFonts := 0;
  FCurrentFont := nil;
end;

destructor TAggFontCachePool.Destroy;
var
  I: Cardinal;
  Fnt: PAggFontCache;
begin
  Fnt := FFonts;
  I := 0;

  while I < FNumFonts do
  begin
    Fnt^.Free;

    Inc(PtrComp(Fnt), SizeOf(TAggFontCache));
    Inc(I);
  end;

  AggFreeMem(Pointer(FFonts), FMaxFonts * SizeOf(TAggFontCache));
  inherited;
end;

procedure TAggFontCachePool.SetFont(FontSignature: AnsiString;
  ResetCache: Boolean = False);
var
  Idx: Integer;
  Fnt: PAggFontCache;
begin
  Idx := FindFont(FontSignature);

  if Idx >= 0 then
  begin
    Fnt := PAggFontCache(PtrComp(FFonts) + Idx * SizeOf(TAggFontCache));

    if ResetCache then
    begin
      Fnt^.Free;
      Fnt^ := TAggFontCache.Create(FontSignature);
    end;

    FCurrentFont := Fnt^;
  end
  else
  begin
    if FNumFonts >= FMaxFonts then
    begin
      Fnt := PAggFontCache(PtrComp(FFonts) + SizeOf(TAggFontCache));

      FFonts^.Free;
      Move(Fnt^, FFonts^, (FMaxFonts - 1) * SizeOf(TAggFontCache));

      FNumFonts := FMaxFonts - 1;
    end;

    Fnt := PAggFontCache(PtrComp(FFonts) + FNumFonts * SizeOf(TAggFontCache));
    Fnt^ := TAggFontCache.Create(FontSignature);

    FCurrentFont := Fnt^;

    Inc(FNumFonts);
  end;
end;

function TAggFontCachePool.GetFont;
begin
  Result := FCurrentFont;
end;

function TAggFontCachePool.FindGlyph;
begin
  if FCurrentFont <> nil then
    Result := FCurrentFont.FindGlyph(GlyphCode)
  else
    Result := nil;
end;

function TAggFontCachePool.CacheGlyph;
begin
  if FCurrentFont <> nil then
    Result := FCurrentFont.CacheGlyph(GlyphCode, GlyphIndex, DataSize,
      DataType, Bounds, AdvanceX, AdvanceY)
  else
    Result := nil;
end;

function TAggFontCachePool.FindFont(FontSignature: AnsiString): Integer;
var
  I: Cardinal;
  F: PAggFontCache;
begin
  I := 0;
  F := FFonts;

  while I < FNumFonts do
  begin
    if F^.FontIs(FontSignature) then
    begin
      Result := I;

      Exit;
    end;

    Inc(PtrComp(F), SizeOf(TAggFontCache));
    Inc(I);
  end;

  Result := -1;
end;


{ TAggFontCacheManager }

constructor TAggFontCacheManager.Create(Engine: TAggCustomFontEngine;
  MaxFonts: Cardinal = 32);
begin
  FFonts := TAggFontCachePool.Create(MaxFonts);

  FEngine := Engine;

  FChangeStamp := -1;

  FPrevGlyph := nil;
  FLastGlyph := nil;

  if FEngine.Flag32 then
    FPathAdaptor := TAggSerializedInt32PathAdaptor.Create
  else
    FPathAdaptor := TAggSerializedInt16PathAdaptor.Create;

  FGray8Adaptor := TAggGray8Adaptor.Create;
  FGray8ScanLine := TAggGray8ScanLine.Create(FGray8Adaptor.Size);
  FMonoAdaptor := TAggMonoAdaptor.Create;
  FMonoScanLine := TAggMonoScanLine.Create;
end;

destructor TAggFontCacheManager.Destroy;
begin
  FFonts.Free;
  FPathAdaptor.Free;
  FGray8Adaptor.Free;
  FMonoAdaptor.Free;
  FGray8ScanLine.Free;
  FMonoScanLine.Free;

  inherited;
end;

function TAggFontCacheManager.Glyph(GlyphCode: Cardinal): PAggGlyphCache;
var
  Gl: PAggGlyphCache;
begin
  Synchronize;

  Gl := FFonts.FindGlyph(GlyphCode);

  Assert(Assigned(FEngine));
  if Gl <> nil then
  begin
    FPrevGlyph := FLastGlyph;
    FLastGlyph := Gl;

    Result := Gl;

    Exit;
  end
  else if FEngine.PrepareGlyph(GlyphCode) then
  begin
    FPrevGlyph := FLastGlyph;
    FLastGlyph := FFonts.CacheGlyph(GlyphCode, FEngine.GlyphIndex,
      FEngine.DataSize, FEngine.DataType, FEngine.GetBounds^,
      FEngine.AdvanceX, FEngine.AdvanceY);

    FEngine.WriteGlyphTo(FLastGlyph.Data);

    Result := FLastGlyph;

    Exit;
  end;

  Result := nil;
end;

procedure TAggFontCacheManager.InitEmbeddedAdaptors(Gl: PAggGlyphCache; X, Y: Double;
  Scale: Double = 1.0);
begin
  if Gl <> nil then
    case Gl.DataType of
      gdMono:
        FMonoAdaptor.Init(Gl.Data, Gl.DataSize, X, Y);

      gdGray8:
        FGray8Adaptor.Init(Gl.Data, Gl.DataSize, X, Y);

      gdOutline:
        FPathAdaptor.Init(Gl.Data, Gl.DataSize, X, Y, Scale);
    end;
end;

function TAggFontCacheManager.PrevGlyph: PAggGlyphCache;
begin
  Result := FPrevGlyph;
end;

function TAggFontCacheManager.LastGlyph: PAggGlyphCache;
begin
  Result := @FLastGlyph;
end;

function TAggFontCacheManager.AddKerning(X, Y: PDouble): Boolean;
begin
  if (FPrevGlyph <> nil) and (FLastGlyph <> nil) then
    Result := FEngine.AddKerning(FPrevGlyph.GlyphIndex,
      FLastGlyph.GlyphIndex, X, Y)
  else
    Result := False;
end;

procedure TAggFontCacheManager.PreCache(AFrom, ATo: Cardinal);
begin
  while AFrom <= ATo do
  begin
    Glyph(AFrom);
    Inc(AFrom);
  end;
end;

procedure TAggFontCacheManager.ResetCache;
begin
  FFonts.SetFont(FEngine.GetFontSignature, True);

  FChangeStamp := FEngine.ChangeStamp;

  FPrevGlyph := nil;
  FLastGlyph := nil;
end;

procedure TAggFontCacheManager.Synchronize;
begin
  if FChangeStamp <> FEngine.ChangeStamp then
  begin
    FFonts.SetFont(FEngine.GetFontSignature);

    FChangeStamp := FEngine.ChangeStamp;

    FPrevGlyph := nil;
    FLastGlyph := nil;
  end;
end;

end.
