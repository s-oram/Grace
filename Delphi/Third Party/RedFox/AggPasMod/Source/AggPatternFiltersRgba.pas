unit AggPatternFiltersRgba;

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
  AggLineAABasics,
  AggColor;

type
  TAggPatternFilter = class
  public
    function Dilation: Cardinal; virtual; abstract;
    procedure PixelLowResolution(Buf: Pointer; P: PAggColor; X, Y: Integer);
      virtual; abstract;
    procedure PixelHighResolution(Buf: Pointer; P: PAggColor; X, Y: Integer);
      virtual; abstract;
  end;

  TAggPatternFilterNN = class(TAggPatternFilter)
  public
    function Dilation: Cardinal; override;
    procedure PixelLowResolution(Buf: Pointer; P: PAggColor; X, Y: Integer); override;
    procedure PixelHighResolution(Buf: Pointer; P: PAggColor; X, Y: Integer); override;
  end;

  TAggPatternFilterBilinearRgba = class(TAggPatternFilter)
  public
    function Dilation: Cardinal; override;
    procedure PixelLowResolution(Buf: Pointer; P: PAggColor; X, Y: Integer); override;
    procedure PixelHighResolution(Buf: Pointer; P: PAggColor; X, Y: Integer); override;
  end;

  TAggPatternFilterBilinearGray8 = class(TAggPatternFilterBilinearRgba)
  public
    procedure PixelHighResolution(Buf: Pointer; P: PAggColor; X, Y: Integer); override;
  end;

implementation


{ TAggPatternFilterNN }

function TAggPatternFilterNN.Dilation;
begin
  Result := 0;
end;

procedure TAggPatternFilterNN.PixelLowResolution(Buf: Pointer; P: PAggColor;
  X, Y: Integer);
begin
  P^.FromRgba8(PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + Y *
    SizeOf(Pointer))^) + X * SizeOf(TAggRgba8))^);
end;

procedure TAggPatternFilterNN.PixelHighResolution(Buf: Pointer; P: PAggColor;
  X, Y: Integer);
begin
  P^.FromRgba8(PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + ShrInt32(Y,
    CAggLineSubpixelShift) * SizeOf(Pointer))^) + ShrInt32(X,
    CAggLineSubpixelShift) * SizeOf(TAggRgba8))^);
end;


{ TAggPatternFilterBilinearRgba }

function TAggPatternFilterBilinearRgba.Dilation;
begin
  Result := 1;
end;

procedure TAggPatternFilterBilinearRgba.PixelLowResolution;
begin
  P^.FromRgba8(PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + Y *
    SizeOf(Pointer))^) + X * SizeOf(TAggRgba8))^);
end;

procedure TAggPatternFilterBilinearRgba.PixelHighResolution;
var
  R, G, B, A, Weight: Int32u;
  LowRes: TPointInteger;
  Ptr: PAggRgba8;
begin
  R := CAggLineSubpixelSize * CAggLineSubpixelSize div 2;
  G := R;
  B := G;
  A := B;

  LowRes.X := ShrInt32(X, CAggLineSubpixelShift);
  LowRes.Y := ShrInt32(Y, CAggLineSubpixelShift);

  X := X and CAggLineSubpixelMask;
  Y := Y and CAggLineSubpixelMask;

  Ptr := PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + LowRes.Y * SizeOf(Pointer))^) +
    LowRes.X * SizeOf(TAggRgba8));

  Weight := (CAggLineSubpixelSize - X) * (CAggLineSubpixelSize - Y);

  Inc(R, Weight * Ptr.R);
  Inc(G, Weight * Ptr.G);
  Inc(B, Weight * Ptr.B);
  Inc(A, Weight * Ptr.A);

  Inc(PtrComp(Ptr), SizeOf(TAggRgba8));

  Weight := X * (CAggLineSubpixelSize - Y);

  Inc(R, Weight * Ptr.R);
  Inc(G, Weight * Ptr.G);
  Inc(B, Weight * Ptr.B);
  Inc(A, Weight * Ptr.A);

  Ptr := PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + (LowRes.Y + 1) *
    SizeOf(Pointer))^) + LowRes.X * SizeOf(TAggRgba8));

  Weight := (CAggLineSubpixelSize - X) * Y;

  Inc(R, Weight * Ptr.R);
  Inc(G, Weight * Ptr.G);
  Inc(B, Weight * Ptr.B);
  Inc(A, Weight * Ptr.A);

  Inc(PtrComp(Ptr), SizeOf(TAggRgba8));

  Weight := X * Y;

  Inc(R, Weight * Ptr.R);
  Inc(G, Weight * Ptr.G);
  Inc(B, Weight * Ptr.B);
  Inc(A, Weight * Ptr.A);

  P.Rgba8.R := Int8u(R shr (CAggLineSubpixelShift * 2));
  P.Rgba8.G := Int8u(G shr (CAggLineSubpixelShift * 2));
  P.Rgba8.B := Int8u(B shr (CAggLineSubpixelShift * 2));
  P.Rgba8.A := Int8u(A shr (CAggLineSubpixelShift * 2));
end;


{ TAggPatternFilterBilinearGray8 }

procedure TAggPatternFilterBilinearGray8.PixelHighResolution;
var
  R, G, B, A, Weight: Int32u;
  LowRes: TPointInteger;
  Ptr: PAggRgba8;
begin
  R := CAggLineSubpixelSize * CAggLineSubpixelSize div 2;
  G := R;
  B := G;
  A := B;

  LowRes.X := ShrInt32(X, CAggLineSubpixelShift);
  LowRes.Y := ShrInt32(Y, CAggLineSubpixelShift);

  X := X and CAggLineSubpixelMask;
  Y := Y and CAggLineSubpixelMask;

  Ptr := PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) +
    LowRes.Y * SizeOf(Pointer))^) + LowRes.X * SizeOf(TAggRgba8));

  Weight := (CAggLineSubpixelSize - X) * (CAggLineSubpixelSize - Y);

  Inc(R, Weight * Ptr.R);
  Inc(G, Weight * Ptr.G);
  Inc(B, Weight * Ptr.B);
  Inc(A, Weight * Ptr.A);

  Inc(PtrComp(Ptr), SizeOf(TAggRgba8));

  Weight := X * (CAggLineSubpixelSize - Y);

  Inc(R, Weight * Ptr.R);
  Inc(G, Weight * Ptr.G);
  Inc(B, Weight * Ptr.B);
  Inc(A, Weight * Ptr.A);

  Ptr := PAggRgba8(PtrComp(PPAggRgba8(PtrComp(Buf) + (LowRes.Y + 1) *
    SizeOf(Pointer))^) + LowRes.X * SizeOf(TAggRgba8));

  Weight := (CAggLineSubpixelSize - X) * Y;

  Inc(R, Weight * Ptr.R);
  Inc(G, Weight * Ptr.G);
  Inc(B, Weight * Ptr.B);
  Inc(A, Weight * Ptr.A);

  Inc(PtrComp(Ptr), SizeOf(TAggRgba8));

  Weight := X * Y;

  Inc(R, Weight * Ptr.R);
  Inc(G, Weight * Ptr.G);
  Inc(B, Weight * Ptr.B);
  Inc(A, Weight * Ptr.A);

  P.Rgba8.R := Int8u(R shr (CAggLineSubpixelShift * 2));
  P.Rgba8.G := Int8u(G shr (CAggLineSubpixelShift * 2));
  P.Rgba8.B := Int8u(B shr (CAggLineSubpixelShift * 2));
  P.Rgba8.A := Int8u(A shr (CAggLineSubpixelShift * 2));
  P.V := (P.Rgba8.R * 77 + P.Rgba8.G * 150 + P.Rgba8.B * 29) shr 8;
end;

end.
