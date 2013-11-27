unit AggSpanAllocator;

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
  AggColor;

type
  TAggSpanAllocator = class
  private
    FMaxSpanLength: Cardinal;

    FSpan: PAggColor;
  public
    constructor Create;
    destructor Destroy; override;

    function Allocate(MaxSpanLength: Cardinal): PAggColor;
    function Span: PAggColor;
  end;

implementation


{ TAggSpanAllocator }

constructor TAggSpanAllocator.Create;
begin
  FMaxSpanLength := 0;

  FSpan := nil;
end;

destructor TAggSpanAllocator.Destroy;
begin
  AggFreeMem(Pointer(FSpan), FMaxSpanLength * SizeOf(TAggColor));

  inherited;
end;

function TAggSpanAllocator.Allocate(MaxSpanLength: Cardinal): PAggColor;
begin
  if MaxSpanLength > FMaxSpanLength then
  begin
    AggFreeMem(Pointer(FSpan), FMaxSpanLength * SizeOf(TAggColor));

    // To reduce the number of reallocs we align the
    // SpanLen to 256 color elements.
    // Well, I just like this number and it looks reasonable.
    MaxSpanLength := ((MaxSpanLength + 255) shr 8) shl 8;

    AggGetMem(Pointer(FSpan), MaxSpanLength * SizeOf(TAggColor));

    FMaxSpanLength := MaxSpanLength;
  end;

  Result := FSpan;
end;

function TAggSpanAllocator.Span: PAggColor;
begin
  Result := FSpan;
end;

end.
