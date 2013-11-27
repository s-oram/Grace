unit AggBitsetIterator;

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
  AggBasics;

type
  TAggBitsetIterator = class
  private
    FBits: PInt8u;
    FMask: Int8u;
    function GetBit: Cardinal;
  public
    constructor Create(Bits: PInt8u; Offset: Cardinal = 0);
    procedure IncOperator;

    property Bit: Cardinal read GetBit;
  end;

implementation


{ TAggBitsetIterator }

constructor TAggBitsetIterator.Create;
begin
  FBits := PInt8u(PtrComp(Bits) + (Offset shr 3) * SizeOf(Int8u));
  FMask := ($80 shr (Offset and 7));
end;

procedure TAggBitsetIterator.IncOperator;
begin
  FMask := FMask shr 1;

  if FMask = 0 then
  begin
    Inc(PtrComp(FBits), SizeOf(Int8u));

    FMask := $80;
  end;
end;

function TAggBitsetIterator.GetBit;
begin
  Result := FBits^ and FMask;
end;

end.
