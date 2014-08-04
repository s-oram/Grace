(*
  This file is a part of Audio Components Suite v 2.2
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at audiocomps@mail.ru
*)

unit ACS_Types;

interface

type

  TBuffer16 = array[0..0] of SmallInt;
  PBuffer16 = ^TBuffer16;

  TBuffer8 = array[0..0] of Byte;
  PBuffer8 = ^TBuffer8;

  TStereoSample16 = packed record
    Left, Right : SmallInt;
  end;

  TStereoBuffer16 = array[0..0] of TStereoSample16;
  PStereoBuffer16 = ^TStereoBuffer16;

  TStereoSample8 = packed record
    Left, Right : Byte;
  end;

  TStereoBuffer8 = array[0..0] of TStereoSample8;
  PStereoBuffer8 = ^TStereoBuffer8;


  TComplex = packed record
    Re, Im : Double;
  end;

  PComplex = ^TComplex;

  TComplexArray = array[0..0] of TComplex;
  PComplexArray = ^TComplexArray;

  TDoubleArray = array[0..0] of Double;
  PDoubleArray = ^TDoubleArray;

  TStereoSampleD = record
    Left : Double;
    Right : Double;
  end;

  TStereoBufferD = array[0..0] of TStereoSampleD;
  PStereoBufferD = ^TStereoBufferD;

const

  Pi = 3.14159265359;
  TwoPi = 6.28318530718;
  HalfPi = 1.57079632679;

implementation

end.
