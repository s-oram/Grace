unit AggSpanConverter;

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
  AggColor,
  AggSpanGenerator;

type
  TAggSpanConvertor = class
  public
    procedure Convert(Span: PAggColor; X, Y: Integer; Len: Cardinal); virtual;
      abstract;
  end;

  TAggSpanConverter = class(TAggSpanGenerator)
  private
    FSpanGen: TAggSpanGenerator;
    FConv: TAggSpanConvertor;
  public
    constructor Create(SpanGen: TAggSpanGenerator;
      Conv: TAggSpanConvertor);

    procedure Prepare(MaxSpanLength: Cardinal); override;
    function Generate(X, Y: Integer; Len: Cardinal): PAggColor; override;
  end;

implementation


{ TAggSpanConverter }

constructor TAggSpanConverter.Create(SpanGen: TAggSpanGenerator;
  Conv: TAggSpanConvertor);
begin
  FSpanGen := SpanGen;
  FConv := Conv;
end;

procedure TAggSpanConverter.Prepare(MaxSpanLength: Cardinal);
begin
  FSpanGen.Prepare(MaxSpanLength);
end;

function TAggSpanConverter.Generate(X, Y: Integer; Len: Cardinal): PAggColor;
var
  Span: PAggColor;
begin
  Span := FSpanGen.Generate(X, Y, Len);
  FConv.Convert(Span, X, Y, Len);
  Result := Span;
end;

end.
