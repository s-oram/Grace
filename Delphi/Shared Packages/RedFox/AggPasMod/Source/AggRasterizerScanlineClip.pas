unit AggRasterizerScanlineClip;

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
  AggClipLiangBarsky,
  AggRasterizerCellsAA;

const
  CAggPolyMaxCoord = (1 shl 30) - 1;
  CMaxStack = 4;
  
type
  TAggRasterizerConv = class
  public
    function MulDiv(A, B, C: Double): Pointer; virtual; abstract;

    function Xi(V: Pointer): Integer; virtual; abstract;
    function Yi(V: Pointer): Integer; virtual; abstract;

    function Upscale(V: Double): Pointer; virtual; abstract;
    function Downscale(V: Integer): Pointer; virtual; abstract;
  end;

  TAggRasConvInt = class(TAggRasterizerConv)
  private
    FResult: array [1..CMaxStack] of Integer;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(A, B, C: Double): Pointer; override;

    function Xi(V: Pointer): Integer; override;
    function Yi(V: Pointer): Integer; override;

    function Upscale(V: Double): Pointer; override;
    function Downscale(V: Integer): Pointer; override;
  end;

  TAggRasConvIntSat = class(TAggRasterizerConv)
  private
    FResult: array [1..CMaxStack] of Integer;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(A, B, C: Double): Pointer; override;

    function Xi(V: Pointer): Integer; override;
    function Yi(V: Pointer): Integer; override;

    function Upscale(V: Double): Pointer; override;
    function Downscale(V: Integer): Pointer; override;
  end;

  TAggRasConvInt3x = class(TAggRasterizerConv)
  private
    FResult: array [1..CMaxStack] of Integer;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(A, B, C: Double): Pointer; override;

    function Xi(V: Pointer): Integer; override;
    function Yi(V: Pointer): Integer; override;

    function Upscale(V: Double): Pointer; override;
    function Downscale(V: Integer): Pointer; override;
  end;

  TRasConvDouble = class(TAggRasterizerConv)
  private
    FResult: array [1..CMaxStack] of Double;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(A, B, C: Double): Pointer; override;

    function Xi(V: Pointer): Integer; override;
    function Yi(V: Pointer): Integer; override;

    function Upscale(V: Double): Pointer; override;
    function Downscale(V: Integer): Pointer; override;
  end;

  TRasConvDouble3x = class(TAggRasterizerConv)
  private
    FResult: array [1..CMaxStack] of Double;
    FStack: Integer;
  public
    constructor Create;

    function MulDiv(A, B, C: Double): Pointer; override;

    function Xi(V: Pointer): Integer; override;
    function Yi(V: Pointer): Integer; override;

    function Upscale(V: Double): Pointer; override;
    function Downscale(V: Integer): Pointer; override;
  end;

  TAggRasterizerScanLineClip = class
  protected
    function GetConverterType: TAggRasterizerConv; virtual; abstract;
  public
    procedure ResetClipping; virtual; abstract;
    procedure SetClipBox(X1, Y1, X2, Y2: Pointer); overload; virtual; abstract;
    procedure SetClipBox(Bounds: Pointer); overload; virtual; abstract;
    procedure MoveTo(X1, Y1: Pointer); virtual; abstract;
    procedure LineTo(Ras: TAggRasterizerCellsAA; X2, Y2: Pointer);
      virtual; abstract;

    property ConverterType: TAggRasterizerConv read GetConverterType;
  end;

  TAggRasterizerScanLineClipInteger = class(TAggRasterizerScanLineClip)
  private
    RasterizerConverter: TAggRasterizerConv;
    FClipBox: TRectInteger;
    FX1, FY1: Integer;
    FF1: Cardinal;
    FClipping: Boolean;
    procedure LineClipY(Ras: TAggRasterizerCellsAA; X1, Y1, X2, Y2: Integer;
      F1, F2: Cardinal);
  protected
    function GetConverterType: TAggRasterizerConv; override;
  public
    constructor Create(Conv: TAggRasterizerConv);

    procedure ResetClipping; override;
    procedure SetClipBox(X1, Y1, X2, Y2: Pointer); override;
    procedure SetClipBox(Bounds: Pointer); override;
    procedure MoveTo(X1, Y1: Pointer); override;
    procedure LineTo(Ras: TAggRasterizerCellsAA; X2, Y2: Pointer); override;
  end;

  TAggRasterizerScanLineClipDouble = class(TAggRasterizerScanLineClip)
  private
    RasterizerConverter: TAggRasterizerConv;
    FClipBox: TRectDouble;
    FX1, FY1: Double;
    FF1: Cardinal;
    FClipping: Boolean;
    procedure LineClipY(Ras: TAggRasterizerCellsAA; X1, Y1, X2, Y2: Double;
      F1, F2: Cardinal);
  protected
    function GetConverterType: TAggRasterizerConv; override;
  public
    constructor Create(Conv: TAggRasterizerConv);

    procedure ResetClipping; override;
    procedure SetClipBox(X1, Y1, X2, Y2: Pointer); override;
    procedure SetClipBox(Bounds: Pointer); override;
    procedure MoveTo(X1, Y1: Pointer); override;
    procedure LineTo(Ras: TAggRasterizerCellsAA; X2, Y2: Pointer); override;
  end;

  TAggRasterizerScanLineNoClip = class(TAggRasterizerScanLineClip)
  private
    FX1, FY1: Integer;
    FConv: TAggRasConvInt;
  protected
    function GetConverterType: TAggRasterizerConv; override;
  public
    constructor Create;

    procedure ResetClipping; override;
    procedure SetClipBox(X1, Y1, X2, Y2: Pointer); override;
    procedure SetClipBox(Bounds: Pointer); override;
    procedure MoveTo(X1, Y1: Pointer); override;
    procedure LineTo(Ras: TAggRasterizerCellsAA; X2, Y2: Pointer); override;
  end;

  TAggRasterizerScanLineClipInt = class(TAggRasterizerScanLineClipInteger)
  private
    FConv: TAggRasConvInt;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAggRasterizerScanLineClipIntegerSat = class(TAggRasterizerScanLineClipInteger)
  private
    FConv: TAggRasConvIntSat;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAggRasterizerScanLineClipInteger3x = class(TAggRasterizerScanLineClipInteger)
  private
    FConv: TAggRasConvInt3x;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAggRasterizerScanLineDoubleClip = class(TAggRasterizerScanLineClipDouble)
  private
    FConv: TRasConvDouble;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TAggRasterizerScanLineClipDouble3x = class(TAggRasterizerScanLineClipDouble)
  private
    FConv: TRasConvDouble3x;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation


{ TAggRasConvInt }

constructor TAggRasConvInt.Create;
begin
  FStack := 1;
  inherited;
end;

function TAggRasConvInt.MulDiv(A, B, C: Double): Pointer;
begin
  FResult[FStack] := IntegerRound(A * B / C);

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TAggRasConvInt.Xi(V: Pointer): Integer;
begin
  Result := PInteger(V)^;
end;

function TAggRasConvInt.Yi(V: Pointer): Integer;
begin
  Result := PInteger(V)^;
end;

function TAggRasConvInt.Upscale(V: Double): Pointer;
begin
  FResult[FStack] := IntegerRound(V * CAggPolySubpixelScale);

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TAggRasConvInt.Downscale(V: Integer): Pointer;
begin
  FResult[FStack] := V;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;


{ TAggRasConvIntSat }

constructor TAggRasConvIntSat.Create;
begin
  FStack := 1;
end;

function TAggRasConvIntSat.MulDiv(A, B, C: Double): Pointer;
begin
  FResult[FStack] := SaturationIntegerRound(CAggPolyMaxCoord, A * B / C);

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TAggRasConvIntSat.Xi(V: Pointer): Integer;
begin
  Result := PInteger(V)^;
end;

function TAggRasConvIntSat.Yi(V: Pointer): Integer;
begin
  Result := PInteger(V)^;
end;

function TAggRasConvIntSat.Upscale(V: Double): Pointer;
begin
  FResult[FStack] := SaturationIntegerRound(CAggPolyMaxCoord,
    V * CAggPolySubpixelScale);

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TAggRasConvIntSat.Downscale(V: Integer): Pointer;
begin
  FResult[FStack] := V;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;


{ TAggRasConvInt3x }

constructor TAggRasConvInt3x.Create;
begin
  FStack := 1;
end;

function TAggRasConvInt3x.MulDiv(A, B, C: Double): Pointer;
begin
  FResult[FStack] := IntegerRound(A * B / C);

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TAggRasConvInt3x.Xi(V: Pointer): Integer;
begin
  Result := PInteger(V)^ * 3;
end;

function TAggRasConvInt3x.Yi(V: Pointer): Integer;
begin
  Result := PInteger(V)^;
end;

function TAggRasConvInt3x.Upscale(V: Double): Pointer;
begin
  FResult[FStack] := IntegerRound(V * CAggPolySubpixelScale);

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TAggRasConvInt3x.Downscale(V: Integer): Pointer;
begin
  FResult[FStack] := V;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;


{ TRasConvDouble }

constructor TRasConvDouble.Create;
begin
  FStack := 1;
end;

function TRasConvDouble.MulDiv(A, B, C: Double): Pointer;
begin
  FResult[FStack] := A * B / C;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TRasConvDouble.Xi(V: Pointer): Integer;
begin
  Result := IntegerRound(PDouble(V)^ * CAggPolySubpixelScale);
end;

function TRasConvDouble.Yi(V: Pointer): Integer;
begin
  Result := IntegerRound(PDouble(V)^ * CAggPolySubpixelScale);
end;

function TRasConvDouble.Upscale(V: Double): Pointer;
begin
  FResult[FStack] := V;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TRasConvDouble.Downscale(V: Integer): Pointer;
begin
  FResult[FStack] := V / CAggPolySubpixelScale;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;


{ TRasConvDouble3x }

constructor TRasConvDouble3x.Create;
begin
  FStack := 1;
end;

function TRasConvDouble3x.MulDiv(A, B, C: Double): Pointer;
begin
  FResult[FStack] := A * B / C;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TRasConvDouble3x.Xi(V: Pointer): Integer;
begin
  Result := IntegerRound(PDouble(V)^ * CAggPolySubpixelScale * 3);
end;

function TRasConvDouble3x.Yi(V: Pointer): Integer;
begin
  Result := IntegerRound(PDouble(V)^ * CAggPolySubpixelScale);
end;

function TRasConvDouble3x.Upscale(V: Double): Pointer;
begin
  FResult[FStack] := V;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;

function TRasConvDouble3x.Downscale(V: Integer): Pointer;
begin
  FResult[FStack] := V / CAggPolySubpixelScale;

  Result := @FResult[FStack];

  Inc(FStack);

  if FStack > CMaxStack then
    FStack := 1;
end;


{ TAggRasterizerScanLineClipInteger }

constructor TAggRasterizerScanLineClipInteger.Create(Conv: TAggRasterizerConv);
begin
  RasterizerConverter := Conv;

  FClipBox := RectInteger(0, 0, 0, 0);

  FX1 := 0;
  FY1 := 0;
  FF1 := 0;

  FClipping := False;
end;

procedure TAggRasterizerScanLineClipInteger.ResetClipping;
begin
  FClipping := False;
end;

procedure TAggRasterizerScanLineClipInteger.SetClipBox(Bounds: Pointer);
begin
  FClipBox := PRectInteger(Bounds)^;
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineClipInteger.SetClipBox(X1, Y1, X2, Y2: Pointer);
begin
  FClipBox := RectInteger(PInteger(X1)^, PInteger(Y1)^, PInteger(X2)^,
    PInteger(Y2)^);
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineClipInteger.MoveTo(X1, Y1: Pointer);
begin
  FX1 := PInteger(X1)^;
  FY1 := PInteger(Y1)^;

  if FClipping then
    FF1 := ClippingFlagsInteger(PInteger(X1)^, PInteger(Y1)^, FClipBox);
end;

procedure TAggRasterizerScanLineClipInteger.LineTo(Ras: TAggRasterizerCellsAA;
  X2, Y2: Pointer);
var
  F1, F2, F3, F4: Cardinal;
  X1, Y1, Y3, Y4: Integer;

begin
  if FClipping then
  begin
    F2 := ClippingFlagsInteger(PInteger(X2)^, PInteger(Y2)^, FClipBox);

    // Invisible by Y
    if ((FF1 and 10) = (F2 and 10)) and (FF1 and 10 <> 0) then
    begin
      FX1 := PInteger(X2)^;
      FY1 := PInteger(Y2)^;
      FF1 := F2;

      Exit;
    end;

    X1 := FX1;
    Y1 := FY1;
    F1 := FF1;

    case ((F1 and 5) shl 1) or (F2 and 5) of
      // Visible by X
      0:
        LineClipY(Ras, X1, Y1, PInteger(X2)^, PInteger(Y2)^, F1, F2);

      // x2 > clip.x2
      1:
        begin
          Y3 := Y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.X2 - X1,
            PInteger(Y2)^ - Y1, PInteger(X2)^ - X1))^;
          F3 := ClippingFlagsYInteger(Y3, FClipBox);

          LineClipY(Ras, X1, Y1, FClipBox.X2, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X2, Y3, FClipBox.X2,
            PInteger(Y2)^, F3, F2);
        end;

      // x1 > clip.x2
      2:
        begin
          Y3 := Y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.X2 - X1,
            PInteger(Y2)^ - Y1, PInteger(X2)^ - X1))^;
          F3 := ClippingFlagsYInteger(Y3, FClipBox);

          LineClipY(Ras, FClipBox.X2, Y1, FClipBox.X2, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X2, Y3, PInteger(X2)^,
            PInteger(Y2)^, F3, F2);
        end;

      // x1 > clip.x2 && x2 > clip.x2
      3:
        LineClipY(Ras, FClipBox.X2, Y1, FClipBox.X2,
          PInteger(Y2)^, F1, F2);

      // x2 < clip.x1
      4:
        begin
          Y3 := Y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.X1 - X1,
            PInteger(Y2)^ - Y1, PInteger(X2)^ - X1))^;
          F3 := ClippingFlagsYInteger(Y3, FClipBox);

          LineClipY(Ras, X1, Y1, FClipBox.X1, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X1, Y3, FClipBox.X1,
            PInteger(Y2)^, F3, F2);
        end;

      // x1 > clip.x2 && x2 < clip.x1
      6:
        begin
          Y3 := Y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.X2 - X1,
            PInteger(Y2)^ - Y1, PInteger(X2)^ - X1))^;
          Y4 := Y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.X1 - X1,
            PInteger(Y2)^ - Y1, PInteger(X2)^ - X1))^;

          F3 := ClippingFlagsYInteger(Y3, FClipBox);
          F4 := ClippingFlagsYInteger(Y4, FClipBox);

          LineClipY(Ras, FClipBox.X2, Y1, FClipBox.X2, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X2, Y3, FClipBox.X1, Y4, F3, F4);
          LineClipY(Ras, FClipBox.X1, Y4, FClipBox.X1,
            PInteger(Y2)^, F4, F2);
        end;

      // x1 < clip.x1
      8:
        begin
          Y3 := Y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.X1 - X1,
            PInteger(Y2)^ - Y1, PInteger(X2)^ - X1))^;
          F3 := ClippingFlagsYInteger(Y3, FClipBox);

          LineClipY(Ras, FClipBox.X1, Y1, FClipBox.X1, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X1, Y3, PInteger(X2)^,
            PInteger(Y2)^, F3, F2);
        end;

      // x1 < clip.x1 && x2 > clip.x2
      9:
        begin
          Y3 := Y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.X1 - X1,
            PInteger(Y2)^ - Y1, PInteger(X2)^ - X1))^;
          Y4 := Y1 + PInteger(RasterizerConverter.MulDiv(FClipBox.X2 - X1,
            PInteger(Y2)^ - Y1, PInteger(X2)^ - X1))^;
          F3 := ClippingFlagsYInteger(Y3, FClipBox);
          F4 := ClippingFlagsYInteger(Y4, FClipBox);

          LineClipY(Ras, FClipBox.X1, Y1, FClipBox.X1, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X1, Y3, FClipBox.X2, Y4, F3, F4);
          LineClipY(Ras, FClipBox.X2, Y4, FClipBox.X2,
            PInteger(Y2)^, F4, F2);
        end;

      // x1 < clip.x1 && x2 < clip.x1
      12:
        LineClipY(Ras, FClipBox.X1, Y1, FClipBox.X1,
          PInteger(Y2)^, F1, F2);
    end;

    FF1 := F2;

  end
  else
    Ras.Line(RasterizerConverter.Xi(@FX1), RasterizerConverter.Yi(@FY1), RasterizerConverter.Xi(X2), RasterizerConverter.Yi(Y2));

  FX1 := PInteger(X2)^;
  FY1 := PInteger(Y2)^;
end;

function TAggRasterizerScanLineClipInteger.GetConverterType: TAggRasterizerConv;
begin
  Result := RasterizerConverter;
end;

procedure TAggRasterizerScanLineClipInteger.LineClipY(Ras: TAggRasterizerCellsAA;
  X1, Y1, X2, Y2: Integer; F1, F2: Cardinal);
var
  Tx1, Ty1, Tx2, Ty2: Integer;
begin
  F1 := F1 and 10;
  F2 := F2 and 10;

  if F1 or F2 = 0 then
    // Fully visible
    Ras.Line(RasterizerConverter.Xi(@X1), RasterizerConverter.Yi(@Y1), RasterizerConverter.Xi(@X2), RasterizerConverter.Yi(@Y2))

  else
  begin
    // Invisible by Y
    if F1 = F2 then
      Exit;

    Tx1 := X1;
    Ty1 := Y1;
    Tx2 := X2;
    Ty2 := Y2;

    // y1 < clip.y1
    if F1 and 8 <> 0 then
    begin
      Tx1 := X1 + PInteger(RasterizerConverter.MulDiv(FClipBox.Y1 - Y1, X2 - X1,
        Y2 - Y1))^;
      Ty1 := FClipBox.Y1;
    end;

    // y1 > clip.y2
    if F1 and 2 <> 0 then
    begin
      Tx1 := X1 + PInteger(RasterizerConverter.MulDiv(FClipBox.Y2 - Y1, X2 - X1,
        Y2 - Y1))^;
      Ty1 := FClipBox.Y2;
    end;

    // y2 < clip.y1
    if F2 and 8 <> 0 then
    begin
      Tx2 := X1 + PInteger(RasterizerConverter.MulDiv(FClipBox.Y1 - Y1, X2 - X1,
        Y2 - Y1))^;
      Ty2 := FClipBox.Y1;
    end;

    // y2 > clip.y2
    if F2 and 2 <> 0 then
    begin
      Tx2 := X1 + PInteger(RasterizerConverter.MulDiv(FClipBox.Y2 - Y1, X2 - X1,
        Y2 - Y1))^;
      Ty2 := FClipBox.Y2;
    end;

    Ras.Line(RasterizerConverter.Xi(@Tx1), RasterizerConverter.Yi(@Ty1), RasterizerConverter.Xi(@Tx2),
      RasterizerConverter.Yi(@Ty2));
  end;
end;


{ TAggRasterizerScanLineClipDouble }

constructor TAggRasterizerScanLineClipDouble.Create(Conv: TAggRasterizerConv);
begin
  RasterizerConverter := Conv;

  FClipBox := RectDouble(0, 0, 0, 0);

  FX1 := 0;
  FY1 := 0;
  FF1 := 0;

  FClipping := False;
end;

procedure TAggRasterizerScanLineClipDouble.ResetClipping;
begin
  FClipping := False;
end;

procedure TAggRasterizerScanLineClipDouble.SetClipBox(Bounds: Pointer);
begin
  FClipBox := PRectDouble(Bounds)^;
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineClipDouble.SetClipBox(X1, Y1, X2, Y2: Pointer);
begin
  FClipBox := RectDouble(PDouble(X1)^, PDouble(Y1)^, PDouble(X2)^,
    PDouble(Y2)^);
  FClipBox.Normalize;

  FClipping := True;
end;

procedure TAggRasterizerScanLineClipDouble.MoveTo(X1, Y1: Pointer);
begin
  FX1 := PDouble(X1)^;
  FY1 := PDouble(Y1)^;

  if FClipping then
    FF1 := ClippingFlagsDouble(PDouble(X1)^, PDouble(Y1)^, @FClipBox);
end;

procedure TAggRasterizerScanLineClipDouble.LineTo(Ras: TAggRasterizerCellsAA;
  X2, Y2: Pointer);
var
  F1, F2, F3, F4: Cardinal;
  X1, Y1, Y3, Y4: Double;

begin
  if FClipping then
  begin
    F2 := ClippingFlagsDouble(PDouble(X2)^, PDouble(Y2)^, @FClipBox);

    // Invisible by Y
    if ((FF1 and 10) = (F2 and 10)) and (FF1 and 10 <> 0) then
    begin
      FX1 := PDouble(X2)^;
      FY1 := PDouble(Y2)^;
      FF1 := F2;

      Exit;
    end;

    X1 := FX1;
    Y1 := FY1;
    F1 := FF1;

    case ((F1 and 5) shl 1) or (F2 and 5) of
      // Visible by X
      0:
        LineClipY(Ras, X1, Y1, PDouble(X2)^, PDouble(Y2)^, F1, F2);

      // x2 > clip.x2
      1:
        begin
          Y3 := Y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.X2 - X1,
            PDouble(Y2)^ - Y1, PDouble(X2)^ - X1))^;
          F3 := ClippingFlagsYDouble(Y3, @FClipBox);

          LineClipY(Ras, X1, Y1, FClipBox.X2, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X2, Y3, FClipBox.X2,
            PDouble(Y2)^, F3, F2);
        end;

      // x1 > clip.x2
      2:
        begin
          Y3 := Y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.X2 - X1,
            PDouble(Y2)^ - Y1, PDouble(X2)^ - X1))^;
          F3 := ClippingFlagsYDouble(Y3, @FClipBox);

          LineClipY(Ras, FClipBox.X2, Y1, FClipBox.X2, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X2, Y3, PDouble(X2)^,
            PDouble(Y2)^, F3, F2);
        end;

      // x1 > clip.x2 && x2 > clip.x2
      3:
        LineClipY(Ras, FClipBox.X2, Y1, FClipBox.X2,
          PDouble(Y2)^, F1, F2);

      // x2 < clip.x1
      4:
        begin
          Y3 := Y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.X1 - X1,
            PDouble(Y2)^ - Y1, PDouble(X2)^ - X1))^;
          F3 := ClippingFlagsYDouble(Y3, @FClipBox);

          LineClipY(Ras, X1, Y1, FClipBox.X1, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X1, Y3, FClipBox.X1,
            PDouble(Y2)^, F3, F2);
        end;

      // x1 > clip.x2 && x2 < clip.x1
      6:
        begin
          Y3 := Y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.X2 - X1,
            PDouble(Y2)^ - Y1, PDouble(X2)^ - X1))^;
          Y4 := Y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.X1 - X1,
            PDouble(Y2)^ - Y1, PDouble(X2)^ - X1))^;

          F3 := ClippingFlagsYDouble(Y3, @FClipBox);
          F4 := ClippingFlagsYDouble(Y4, @FClipBox);

          LineClipY(Ras, FClipBox.X2, Y1, FClipBox.X2, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X2, Y3, FClipBox.X1, Y4, F3, F4);
          LineClipY(Ras, FClipBox.X1, Y4, FClipBox.X1,
            PDouble(Y2)^, F4, F2);
        end;

      // x1 < clip.x1
      8:
        begin
          Y3 := Y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.X1 - X1,
            PDouble(Y2)^ - Y1, PDouble(X2)^ - X1))^;
          F3 := ClippingFlagsYDouble(Y3, @FClipBox);

          LineClipY(Ras, FClipBox.X1, Y1, FClipBox.X1, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X1, Y3, PDouble(X2)^,
            PDouble(Y2)^, F3, F2);
        end;

      // x1 < clip.x1 && x2 > clip.x2
      9:
        begin
          Y3 := Y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.X1 - X1,
            PDouble(Y2)^ - Y1, PDouble(X2)^ - X1))^;
          Y4 := Y1 + PDouble(RasterizerConverter.MulDiv(FClipBox.X2 - X1,
            PDouble(Y2)^ - Y1, PDouble(X2)^ - X1))^;
          F3 := ClippingFlagsYDouble(Y3, @FClipBox);
          F4 := ClippingFlagsYDouble(Y4, @FClipBox);

          LineClipY(Ras, FClipBox.X1, Y1, FClipBox.X1, Y3, F1, F3);
          LineClipY(Ras, FClipBox.X1, Y3, FClipBox.X2, Y4, F3, F4);
          LineClipY(Ras, FClipBox.X2, Y4, FClipBox.X2,
            PDouble(Y2)^, F4, F2);
        end;

      // x1 < clip.x1 && x2 < clip.x1
      12:
        LineClipY(Ras, FClipBox.X1, Y1, FClipBox.X1,
          PDouble(Y2)^, F1, F2);
    end;

    FF1 := F2;

  end
  else
    Ras.Line(RasterizerConverter.Xi(@FX1), RasterizerConverter.Yi(@FY1), RasterizerConverter.Xi(X2), RasterizerConverter.Yi(Y2));

  FX1 := PDouble(X2)^;
  FY1 := PDouble(Y2)^;
end;

function TAggRasterizerScanLineClipDouble.GetConverterType: TAggRasterizerConv;
begin
  Result := RasterizerConverter;
end;

procedure TAggRasterizerScanLineClipDouble.LineClipY(Ras: TAggRasterizerCellsAA;
  X1, Y1, X2, Y2: Double; F1, F2: Cardinal);
var
  Tx1, Ty1, Tx2, Ty2: Double;

begin
  F1 := F1 and 10;
  F2 := F2 and 10;

  if F1 or F2 = 0 then
    // Fully visible
    Ras.Line(RasterizerConverter.Xi(@X1), RasterizerConverter.Yi(@Y1), RasterizerConverter.Xi(@X2), RasterizerConverter.Yi(@Y2))

  else
  begin
    // Invisible by Y
    if F1 = F2 then
      Exit;

    Tx1 := X1;
    Ty1 := Y1;
    Tx2 := X2;
    Ty2 := Y2;

    // y1 < clip.y1
    if F1 and 8 <> 0 then
    begin
      Tx1 := X1 + PDouble(RasterizerConverter.MulDiv(FClipBox.Y1 - Y1, X2 - X1,
        Y2 - Y1))^;
      Ty1 := FClipBox.Y1;
    end;

    // y1 > clip.y2
    if F1 and 2 <> 0 then
    begin
      Tx1 := X1 + PDouble(RasterizerConverter.MulDiv(FClipBox.Y2 - Y1, X2 - X1,
        Y2 - Y1))^;
      Ty1 := FClipBox.Y2;
    end;

    // y2 < clip.y1
    if F2 and 8 <> 0 then
    begin
      Tx2 := X1 + PDouble(RasterizerConverter.MulDiv(FClipBox.Y1 - Y1, X2 - X1,
        Y2 - Y1))^;
      Ty2 := FClipBox.Y1;
    end;

    // y2 > clip.y2
    if F2 and 2 <> 0 then
    begin
      Tx2 := X1 + PDouble(RasterizerConverter.MulDiv(FClipBox.Y2 - Y1, X2 - X1,
        Y2 - Y1))^;
      Ty2 := FClipBox.Y2;
    end;

    Ras.Line(RasterizerConverter.Xi(@Tx1), RasterizerConverter.Yi(@Ty1), RasterizerConverter.Xi(@Tx2),
      RasterizerConverter.Yi(@Ty2));
  end;
end;


{ TAggRasterizerScanLineNoClip }

constructor TAggRasterizerScanLineNoClip.Create;
begin
  FX1 := 0;
  FY1 := 0;

  FConv := TAggRasConvInt.Create;
end;

procedure TAggRasterizerScanLineNoClip.ResetClipping;
begin
end;

procedure TAggRasterizerScanLineNoClip.SetClipBox(X1, Y1, X2, Y2: Pointer);
begin
end;

procedure TAggRasterizerScanLineNoClip.SetClipBox(Bounds: Pointer);
begin
end;

procedure TAggRasterizerScanLineNoClip.MoveTo(X1, Y1: Pointer);
begin
  FX1 := PInteger(X1)^;
  FY1 := PInteger(Y1)^;
end;

procedure TAggRasterizerScanLineNoClip.LineTo(Ras: TAggRasterizerCellsAA;
  X2, Y2: Pointer);
begin
  Ras.Line(FX1, FY1, PInteger(X2)^, PInteger(Y2)^);

  FX1 := PInteger(X2)^;
  FY1 := PInteger(Y2)^;
end;

function TAggRasterizerScanLineNoClip.GetConverterType: TAggRasterizerConv;
begin
  Result := FConv;
end;


{ TAggRasterizerScanLineClipInt }

constructor TAggRasterizerScanLineClipInt.Create;
begin
  FConv := TAggRasConvInt.Create;

  inherited Create(FConv);
end;


destructor TAggRasterizerScanLineClipInt.Destroy;
begin
  FConv.Free;
  inherited;
end;

{ TAggRasterizerScanLineClipIntegerSat }

constructor TAggRasterizerScanLineClipIntegerSat.Create;
begin
  FConv := TAggRasConvIntSat.Create;

  inherited Create(FConv);
end;


destructor TAggRasterizerScanLineClipIntegerSat.Destroy;
begin
  FConv.Free;
  inherited;
end;

{ TAggRasterizerScanLineClipInteger3x }

constructor TAggRasterizerScanLineClipInteger3x.Create;
begin
  FConv := TAggRasConvInt3x.Create;

  inherited Create(FConv);
end;


destructor TAggRasterizerScanLineClipInteger3x.Destroy;
begin
  FConv.Free;
  inherited;
end;

{ TAggRasterizerScanLineDoubleClip }

constructor TAggRasterizerScanLineDoubleClip.Create;
begin
  FConv := TRasConvDouble.Create;

  inherited Create(FConv);
end;


destructor TAggRasterizerScanLineDoubleClip.Destroy;
begin
  FConv.Free;
  inherited;
end;

{ TAggRasterizerScanLineClipDouble3x }

constructor TAggRasterizerScanLineClipDouble3x.Create;
begin
  FConv := TRasConvDouble3x.Create;

  inherited Create(FConv);
end;

destructor TAggRasterizerScanLineClipDouble3x.Destroy;
begin
  FConv.Free;
  inherited;
end;

end.
