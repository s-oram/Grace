unit AggSpanGradientContour;

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
  Math,
  AggBasics,
  AggColor,
  AggSpanGradient,
  AggPathStorage,
  AggBoundingRect,
  AggConvCurve,
  AggConvStroke,
  AggConvTransform,
  AggTransAffine,
  AggRenderingBuffer,
  AggRendererBase,
  AggRendererScanLine,
  AggRendererPrimitives,
  AggRenderScanLines,
  AggRasterizerOutline,
  AggRasterizerScanLineAA,
  AggScanLine,
  AggScanlineUnpacked,
  AggPixelFormat,
  AggPixelFormatGray;

type
  PAggIntArray = ^TAggIntArray;
  TAggIntArray = array [0..65535] of Integer;

  PAggSingleArray = ^TAggSingleArray;
  TAggSingleArray = array [0..65535] of Single;

  TAggGradientContour = class(TAggCustomGradient)
  private
    FBuffer: Pointer;
    FWidth, FHeight, FFrame: Integer;

    FD1, FD2: Double;

    procedure SetFrame(F: Integer); overload;

    function Calculate(X, Y, D: Integer): Integer; override;

    procedure SetD1(D: Double);
    procedure SetD2(D: Double);
  public
    constructor Create(AD1: Double = 0; AD2: Double = 100);
    destructor Destroy; override;

    function ContourCreate(Ps: TAggPathStorage): Pointer;

    property Frame: Integer read FFrame write SetFrame;
    property ContourWidth: Integer read FWidth;
    property ContourHeight: Integer read FHeight;

    property D1: Double read FD1 write SetD1;
    property D2: Double read FD2 write SetD2;
  end;

implementation

const
  CInfinity = 1E20;


{ TAggGradientContour }

constructor TAggGradientContour.Create(AD1: Double = 0; AD2: Double = 100);
begin
  inherited Create;

  FBuffer := nil;
  FWidth := 0;
  FHeight := 0;
  FFrame := 10;

  FD1 := AD1;
  FD2 := AD2;
end;

destructor TAggGradientContour.Destroy;
begin
  if FBuffer <> nil then
    AggFreeMem(FBuffer, FWidth * FHeight);
end;

function Square(X: Integer): Integer;
begin
  Result := X * X;
end;

// DT algorithm by: Pedro Felzenszwalb
procedure Dt(Spanf, Spang, Spanr: PAggSingleArray; Spann: PAggIntArray;
  Length: Integer);
var
  K, Q: Integer;
  S: Single;
begin
  K := 0;

  Spann[0] := 0;
  Spang[0] := -CInfinity;
  Spang[1] := +CInfinity;

  Q := 1;

  while Q <= Length - 1 do
  begin
    S := ((Spanf[Q] + Square(Q)) - (Spanf[Spann[K]] + Square(Spann[K]))) /
      (2 * Q - 2 * Spann[K]);

    while S <= Spang[K] do
    begin
      Dec(K);

      S := ((Spanf[Q] + Square(Q)) - (Spanf[Spann[K]] + Square(Spann[K]))) /
        (2 * Q - 2 * Spann[K]);
    end;

    Inc(K);

    Spann[K] := Q;
    Spang[K] := S;

    Spang[K + 1] := +CInfinity;

    Inc(Q);
  end;

  K := 0;
  Q := 0;

  while Q <= Length - 1 do
  begin
    while Spang[K + 1] < Q do
      Inc(K);

    Spanr[Q] := Square(Q - Spann[K]) + Spanf[Spann[K]];

    Inc(Q);
  end;
end;

function TAggGradientContour.ContourCreate(Ps: TAggPathStorage): Pointer;
var
  Rb: TAggRenderingBuffer;
  Pf: TAggPixelFormatProcessor;

  Ras: TAggRasterizerOutline;
  Mtx: TAggTransAffine;

  Rgba: TAggColor;
  Renb: TAggRendererBase;
  Prim: TAggRendererPrimitives;
  Conv: TAggConvCurve;

  Trans: TAggConvTransform;

  Min, Max, Scale: Single;

  X1, Y1, X2, Y2: Double;
  Width, Height, Length, Fcx, Fcy: Integer;
  Buffer, Image: Pointer;
  Src: PInt8u;
  Dst, Im, Spanf, Spang, Spanr: PSingle;
  Spann: PInteger;
begin
  Result := nil;
  Buffer := nil;

  if Ps <> nil then
  begin
    { I. Render Black And White NonAA Stroke of the Path }
    { Path Bounding Box + Some GetFrame Space Around [configurable] }
    Conv := TAggConvCurve.Create(Ps);
    try
      if BoundingRectSingle(Conv, 0, @X1, @Y1, @X2, @Y2) then
      begin
        { Create BW Rendering Surface }
        Width := Ceil(X2 - X1) + FFrame * 2 + 1;
        Height := Ceil(Y2 - Y1) + FFrame * 2 + 1;

        if AggGetMem(Buffer, Width * Height) then
        begin
          FillChar(Buffer^, Width * Height, 255);

          { Setup VG Engine & Render }
          Rb := TAggRenderingBuffer.Create;
          Rb.Attach(Buffer, Width, Height, Width);

          PixelFormatGray8(Pf, Rb);

          Renb := TAggRendererBase.Create(Pf, True);
          Prim := TAggRendererPrimitives.Create(Renb);
          Ras := TAggRasterizerOutline.Create(Prim);
          try
            Mtx := TAggTransAffine.Create;
            try
              Mtx.Translate(FFrame - X1, FFrame - Y1);

              Trans := TAggConvTransform.Create(Conv, Mtx);
              try
                Rgba.Black;
                Prim.LineColor := Rgba;
                Ras.AddPath(Trans);
              finally
                Trans.Free;
              end;
            finally
              Mtx.Free;
            end;
            Rb.Free;
          finally
            Ras.Free;
            Prim.Free;
            Renb.Free;
          end;

          { II. Distance Transform }
          { Create Float Buffer + 0 vs CInfinity (1e20) assignment }
          if AggGetMem(Image, Width * Height * SizeOf(Single)) then
          begin
            Src := Buffer;
            Dst := Image;

            for Fcy := 0 to Height - 1 do
              for Fcx := 0 to Width - 1 do
              begin
                if Src^ = 0 then
                  Dst^ := 0
                else
                  Dst^ := CInfinity;

                Inc(PtrComp(Src));
                Inc(PtrComp(Dst), SizeOf(Single));
              end;

            { DT of 2d }
            { SubBuff<float> max width,height }
            Length := Width;

            if Height > Length then
              Length := Height;

            Spanf := nil;
            Spang := nil;
            Spanr := nil;
            Spann := nil;

            if AggGetMem(Pointer(Spanf), Length * SizeOf(Single)) and
              AggGetMem(Pointer(Spang), (Length + 1) * SizeOf(Single)) and
              AggGetMem(Pointer(Spanr), Length * SizeOf(Single)) and
              AggGetMem(Pointer(Spann), Length * SizeOf(Integer)) then
            begin
              { Transform along columns }
              for Fcx := 0 to Width - 1 do
              begin
                Im := Pointer(PtrComp(Image) + Fcx * SizeOf(Single));
                Dst := Spanf;

                for Fcy := 0 to Height - 1 do
                begin
                  Dst^ := Im^;

                  Inc(PtrComp(Dst), SizeOf(Single));
                  Inc(PtrComp(Im), Width * SizeOf(Single));
                end;

                { DT of 1d }
                Dt(Pointer(Spanf), Pointer(Spang), Pointer(Spanr),
                  Pointer(Spann), Height);

                Im := Pointer(PtrComp(Image) + Fcx * SizeOf(Single));
                Dst := Spanr;

                for Fcy := 0 to Height - 1 do
                begin
                  Im^ := Dst^;

                  Inc(PtrComp(Dst), SizeOf(Single));
                  Inc(PtrComp(Im), Width * SizeOf(Single));
                end;
              end;

              { Transform along rows }
              for Fcy := 0 to Height - 1 do
              begin
                Im := Pointer(PtrComp(Image) + Fcy * Width * SizeOf(Single));
                Dst := Spanf;

                for Fcx := 0 to Width - 1 do
                begin
                  Dst^ := Im^;

                  Inc(PtrComp(Dst), SizeOf(Single));
                  Inc(PtrComp(Im), SizeOf(Single));
                end;

                { DT of 1d }
                Dt(Pointer(Spanf), Pointer(Spang), Pointer(Spanr),
                  Pointer(Spann), Width);

                Im := Pointer(PtrComp(Image) + Fcy * Width * SizeOf(Single));
                Dst := Spanr;

                for Fcx := 0 to Width - 1 do
                begin
                  Im^ := Dst^;

                  Inc(PtrComp(Dst), SizeOf(Single));
                  Inc(PtrComp(Im), SizeOf(Single));
                end;
              end;

              { Take Square Roots, Min & Max }
              Dst := Image;
              Min := Sqrt(Dst^);
              Max := Min;

              for Fcy := 0 to Height - 1 do
                for Fcx := 0 to Width - 1 do
                begin
                  Dst^ := Sqrt(Dst^);

                  if Min > Dst^ then
                    Min := Dst^;

                  if Max < Dst^ then
                    Max := Dst^;

                  Inc(PtrComp(Dst), SizeOf(Single));
                end;

              { III. Convert To Grayscale }
              if Min = Max then
                FillChar(Buffer^, Width * Height, 0)
              else
              begin
                Scale := 255 / (Max - Min);

                Src := Buffer;
                Dst := Image;

                for Fcy := 0 to Height - 1 do
                  for Fcx := 0 to Width - 1 do
                  begin
                    Src^ := Int8u(Trunc((Dst^ - Min) * Scale));

                    Inc(PtrComp(Src));
                    Inc(PtrComp(Dst), SizeOf(Single));
                  end;
              end;

              { OK }
              if FBuffer <> nil then
                AggFreeMem(FBuffer, FWidth * FHeight);

              FBuffer := Buffer;
              FWidth := Width;
              FHeight := Height;

              Buffer := nil;
              Result := FBuffer;
            end;

            if Spanf <> nil then
              AggFreeMem(Pointer(Spanf), Length * SizeOf(Single));

            if Spang <> nil then
              AggFreeMem(Pointer(Spang), (Length + 1) * SizeOf(Single));

            if Spanr <> nil then
              AggFreeMem(Pointer(Spanr), Length * SizeOf(Single));

            if Spann <> nil then
              AggFreeMem(Pointer(Spann), Length * SizeOf(Integer));

            AggFreeMem(Image, Width * Height * SizeOf(Single));
          end;
        end;
      end;
    finally
      Conv.Free;
    end;

    if Buffer <> nil then
      AggFreeMem(Buffer, Width * Height);
  end;
end;

procedure TAggGradientContour.SetD1(D: Double);
begin
  FD1 := D;
end;

procedure TAggGradientContour.SetD2(D: Double);
begin
  FD2 := D;
end;

procedure TAggGradientContour.SetFrame(F: Integer);
begin
  FFrame := F;
end;

function TAggGradientContour.Calculate(X, Y, D: Integer): Integer;
var
  Px, Py: Integer;
  Pixel: PInt8u;
begin
  if FBuffer <> nil then
  begin
    Px := ShrInt32(X, CAggGradientSubpixelShift);
    Py := ShrInt32(Y, CAggGradientSubpixelShift);

    Px := Px mod FWidth;

    if Px < 0 then
      Px := FWidth + Px;

    Py := Py mod FHeight;

    if Py < 0 then
      Py := FHeight + Py;

    Pixel := PInt8u(PtrComp(FBuffer) + Py * FWidth + Px);
    Result := Round(Pixel^ * (FD2 / 256) + FD1) shl CAggGradientSubpixelShift;
  end
  else
    Result := 0;
end;

end.
