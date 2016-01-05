unit AggSpanSolid;

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
  AggSpanAllocator,
  AggSpanGenerator,
  AggColor;

type
  TAggSpanSolid = class(TAggSpanGenerator)
  private
    FColor: TAggColor;

    procedure SetColor(C: PAggColor);
    function GetColor: PAggColor;
  public
    constructor Create(Alloc: TAggSpanAllocator);

    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanSolid }

constructor TAggSpanSolid.Create(Alloc: TAggSpanAllocator);
begin
  inherited Create(Alloc);
end;

procedure TAggSpanSolid.SetColor(C: PAggColor);
begin
  FColor := C^;
end;

function TAggSpanSolid.GetColor: PAggColor;
begin
  Result := @FColor;
end;

function TAggSpanSolid.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
begin
  Span := Allocator.Span;

  repeat
    Span^ := FColor;

    Inc(PtrComp(Span), SizeOf(TAggColor));
    Dec(Len);
  until Len = 0;

  Result := Allocator.Span;
end;

end.
