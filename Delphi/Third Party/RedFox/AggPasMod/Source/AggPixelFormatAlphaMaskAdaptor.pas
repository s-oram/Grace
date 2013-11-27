unit AggPixelFormatAlphaMaskAdaptor;

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
  AggRenderingBuffer,
  AggPixelFormat,
  AggAlphaMaskUnpacked8;

const
  CSpanExtraTail = 256;

type
  TAggPixelFormatProcessorAlphaMaskAdaptor = class(TAggPixelFormatProcessor)
  private
    FPixelFormats: TAggPixelFormatProcessor;
    FMask: TAggCustomAlphaMask;

    FSpan: PInt8u;
    FMaxLength: Cardinal;
  public
    constructor Create(PixelFormat: TAggPixelFormatProcessor;
      Mask: TAggCustomAlphaMask);
    destructor Destroy; override;

    procedure ReallocSpan(Length: Cardinal);

    procedure IniTAggSpan(Length: Cardinal); overload;
    procedure IniTAggSpan(Length: Cardinal; Covers: PInt8u); overload;
  end;

implementation

procedure CopyHorizontalLineAdaptor(This: TAggPixelFormatProcessorAlphaMaskAdaptor;
  X, Y: Integer; Length: Cardinal; C: PAggColor);
begin
  This.ReallocSpan(Length);
  This.FMask.FillHSpan(X, Y, This.FSpan, Length);
  This.FPixelFormats.BlendSolidHSpan(This.FPixelFormats, X, Y, Length, C,
    This.FSpan);
end;

procedure BlendHorizontalLineAdaptor(This: TAggPixelFormatProcessorAlphaMaskAdaptor;
  X, Y: Integer; Length: Cardinal; C: PAggColor; Cover: Int8u);
begin
  This.IniTAggSpan(Length);
  This.FMask.CombineHSpan(X, Y, This.FSpan, Length);
  This.FPixelFormats.BlendSolidHSpan(This.FPixelFormats, X, Y, Length, C,
    This.FSpan);
end;

procedure BlendVerticalLineAdaptor(This: TAggPixelFormatProcessorAlphaMaskAdaptor;
  X, Y: Integer; Length: Cardinal; C: PAggColor; Cover: Int8u);
begin
  This.IniTAggSpan(Length);
  This.FMask.CombineVSpan(X, Y, This.FSpan, Length);
  This.FPixelFormats.BlendSolidVSpan(This.FPixelFormats, X, Y, Length, C,
    This.FSpan);
end;

procedure BlendSolidHSpanAdaptor(This: TAggPixelFormatProcessorAlphaMaskAdaptor;
  X, Y: Integer; Length: Cardinal; C: PAggColor; Covers: PInt8u);
begin
  This.IniTAggSpan(Length, Covers);
  This.FMask.CombineHSpan(X, Y, This.FSpan, Length);
  This.FPixelFormats.BlendSolidHSpan(This.FPixelFormats, X, Y, Length, C,
    This.FSpan);
end;

procedure BlendSolidVSpanAdaptor(This: TAggPixelFormatProcessorAlphaMaskAdaptor;
  X, Y: Integer; Length: Cardinal; C: PAggColor; Covers: PInt8u);
begin
  This.IniTAggSpan(Length, Covers);
  This.FMask.CombineVSpan(X, Y, This.FSpan, Length);
  This.FPixelFormats.BlendSolidVSpan(This.FPixelFormats, X, Y, Length, C,
    This.FSpan);
end;

procedure BlendColorHSpanAdaptor(This: TAggPixelFormatProcessorAlphaMaskAdaptor;
  X, Y: Integer; Length: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
begin
  if Covers <> nil then
  begin
    This.IniTAggSpan(Length, Covers);
    This.FMask.CombineHSpan(X, Y, This.FSpan, Length);
  end
  else
  begin
    This.ReallocSpan(Length);
    This.FMask.FillHSpan(X, Y, This.FSpan, Length);
  end;

  This.FPixelFormats.BlendColorHSpan(This.FPixelFormats, X, Y, Length, Colors,
    This.FSpan, Cover);
end;

procedure BlendColorVSpanAdaptor(This: TAggPixelFormatProcessorAlphaMaskAdaptor;
  X, Y: Integer; Length: Cardinal; Colors: PAggColor; Covers: PInt8u;
  Cover: Int8u);
begin
  if Covers <> nil then
  begin
    This.IniTAggSpan(Length, Covers);
    This.FMask.CombineVSpan(X, Y, This.FSpan, Length);
  end
  else
  begin
    This.ReallocSpan(Length);
    This.FMask.FillVSpan(X, Y, This.FSpan, Length);
  end;

  This.FPixelFormats.BlendColorVSpan(This.FPixelFormats, X, Y, Length, Colors,
    This.FSpan, Cover);
end;

procedure BlendPixelAdaptor(This: TAggPixelFormatProcessorAlphaMaskAdaptor;
  X, Y: Integer; C: Pointer; Cover: Int8u);
begin
  This.FPixelFormats.BlendPixel(This.FPixelFormats, X, Y, C,
    This.FMask.CombinePixel(X, Y, Cover));
end;


{ TAggPixelFormatProcessorAlphaMaskAdaptor }

constructor TAggPixelFormatProcessorAlphaMaskAdaptor.Create(
  PixelFormat: TAggPixelFormatProcessor; Mask: TAggCustomAlphaMask);
begin
  inherited Create(PixelFormat.RenderingBuffer);

  FPixelFormats := PixelFormat;
  FMask := Mask;

  FSpan := nil;
  FMaxLength := 0;

  CopyHorizontalLine := @CopyHorizontalLineAdaptor;
  BlendHorizontalLine := @BlendHorizontalLineAdaptor;
  BlendVerticalLine := @BlendVerticalLineAdaptor;

  BlendSolidHSpan := @BlendSolidHSpanAdaptor;
  BlendSolidVSpan := @BlendSolidVSpanAdaptor;
  BlendColorHSpan := @BlendColorHSpanAdaptor;
  BlendColorVSpan := @BlendColorVSpanAdaptor;

  BlendPixel := @BlendPixelAdaptor;
end;

destructor TAggPixelFormatProcessorAlphaMaskAdaptor.Destroy;
begin
  if Assigned(FSpan) then
    AggFreeMem(Pointer(FSpan), FMaxLength * SizeOf(Int8u));
  inherited;
end;

procedure TAggPixelFormatProcessorAlphaMaskAdaptor.ReallocSpan;
begin
  if Length > FMaxLength then
  begin
    AggFreeMem(Pointer(FSpan), FMaxLength * SizeOf(Int8u));

    FMaxLength := Length + CSpanExtraTail;

    AggGetMem(Pointer(FSpan), FMaxLength * SizeOf(Int8u));
  end;
end;

procedure TAggPixelFormatProcessorAlphaMaskAdaptor.IniTAggSpan(Length: Cardinal);
begin
  ReallocSpan(Length);

  FillChar(FSpan^, Length * SizeOf(Int8u), CAggCoverFull);
end;

procedure TAggPixelFormatProcessorAlphaMaskAdaptor.IniTAggSpan(Length: Cardinal;
  Covers: PInt8u);
begin
  ReallocSpan(Length);

  Move(Covers^, FSpan^, Length * SizeOf(Int8u));
end;

end.
