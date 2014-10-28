unit AggSpanInterpolatorLinear;

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
{$Q-}
{$R-}

uses
  AggBasics,
  AggDdaLine,
  AggTransAffine;

type
  TAggSpanInterpolator = class
  protected
    FSubpixelShift, FSubpixelSize: Cardinal;
    function GetTransformer: TAggTransAffine; virtual; abstract;
    procedure SetTransformer(Trans: TAggTransAffine); virtual; abstract;
  public
    constructor Create(SS: Cardinal = 8); virtual;

    procedure SetBegin(X, Y: Double; Len: Cardinal); virtual; abstract;

    procedure Resynchronize(Xe, Ye: Double; Len: Cardinal); virtual; abstract;

    procedure IncOperator; virtual; abstract;
    procedure Coordinates(X, Y: PInteger); overload; virtual; abstract;
    procedure Coordinates(var X, Y: Integer); overload; virtual;

    procedure LocalScale(X, Y: PInteger); virtual;

    property SubpixelShift: Cardinal read FSubpixelShift;
    property Transformer: TAggTransAffine read GetTransformer write SetTransformer;
  end;

  TAggSpanInterpolatorLinear = class(TAggSpanInterpolator)
  private
    FTrans: TAggTransAffine;
    FLineInterpolatorX, FLineInterpolatorY: TAggDda2LineInterpolator;
  protected
    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;
  public
    constructor Create(SS: Cardinal = 8); overload; override;
    constructor Create(Trans: TAggTransAffine; SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; X, Y: Double; Len: Cardinal;
      SS: Cardinal = 8); overload;

    procedure SetBegin(X, Y: Double; Len: Cardinal); override;

    procedure Resynchronize(Xe, Ye: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;
  end;

  TAggSpanInterpolatorLinearSubdiv = class(TAggSpanInterpolator)
  private
    FSubdivShift, FSubdivSize, FSubdivMask: Cardinal;

    FTrans: TAggTransAffine;
    FLineInterpolatorX, FLineInterpolatorY: TAggDda2LineInterpolator;

    FSourceX: Integer;
    FSourceY: Double;
    FPos, FLength: Cardinal;

    procedure SetSubdivShift(Shift: Cardinal);
  protected
    function GetTransformer: TAggTransAffine; override;
    procedure SetTransformer(Trans: TAggTransAffine); override;
  public
    constructor Create(SS: Cardinal = 8); overload; override;
    constructor Create(Trans: TAggTransAffine; ASubdivShift: Cardinal = 4;
      SS: Cardinal = 8); overload;
    constructor Create(Trans: TAggTransAffine; X, Y: Double; Len: Cardinal;
      ASubdivShift: Cardinal = 4; SS: Cardinal = 8); overload;

    procedure SetBegin(X, Y: Double; Len: Cardinal); override;

    procedure IncOperator; override;
    procedure Coordinates(X, Y: PInteger); override;
    procedure Coordinates(var X, Y: Integer); override;

    property SubdivShift: Cardinal read FSubdivShift write SetSubdivShift;
  end;

implementation


{ TAggSpanInterpolator }

constructor TAggSpanInterpolator.Create(SS: Cardinal = 8);
begin
  FSubpixelShift := SS;
  FSubpixelSize := 1 shl FSubpixelShift;
end;

procedure TAggSpanInterpolator.Coordinates(var X, Y: Integer);
begin
  Coordinates(@X, @Y);
end;

procedure TAggSpanInterpolator.LocalScale;
begin
end;


{ TAggSpanInterpolatorLinear }

constructor TAggSpanInterpolatorLinear.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);
end;

constructor TAggSpanInterpolatorLinear.Create(Trans: TAggTransAffine;
  SS: Cardinal = 8);
begin
  Create(SS);

  FTrans := Trans;
end;

constructor TAggSpanInterpolatorLinear.Create(Trans: TAggTransAffine;
  X, Y: Double; Len: Cardinal; SS: Cardinal = 8);
begin
  Create(Trans, SS);

  SetBegin(X, Y, Len);
end;

function TAggSpanInterpolatorLinear.GetTransformer;
begin
  Result := FTrans;
end;

procedure TAggSpanInterpolatorLinear.SetTransformer;
begin
  FTrans := Trans;
end;

procedure TAggSpanInterpolatorLinear.SetBegin(X, Y: Double; Len: Cardinal);
var
  Tx, Ty: Double;
  X1, Y1, X2, Y2: Integer;
begin
  Tx := X;
  Ty := Y;

  FTrans.Transform(FTrans, @Tx, @Ty);

  X1 := Trunc(Tx * FSubpixelSize);
  Y1 := Trunc(Ty * FSubpixelSize);

  Tx := X + Len;
  Ty := Y;

  FTrans.Transform(FTrans, @Tx, @Ty);

  X2 := Trunc(Tx * FSubpixelSize);
  Y2 := Trunc(Ty * FSubpixelSize);

  FLineInterpolatorX.Initialize(X1, X2, Len);
  FLineInterpolatorY.Initialize(Y1, Y2, Len);
end;

procedure TAggSpanInterpolatorLinear.Resynchronize;
begin
  FTrans.Transform(FTrans, @Xe, @Ye);

  FLineInterpolatorX.Initialize(FLineInterpolatorX.Y, Trunc(Xe * FSubpixelSize), Len);
  FLineInterpolatorY.Initialize(FLineInterpolatorY.Y, Trunc(Ye * FSubpixelSize), Len);
end;

procedure TAggSpanInterpolatorLinear.IncOperator;
begin
  FLineInterpolatorX.PlusOperator;
  FLineInterpolatorY.PlusOperator;
end;

procedure TAggSpanInterpolatorLinear.Coordinates(X, Y: PInteger);
begin
  X^ := FLineInterpolatorX.Y;
  Y^ := FLineInterpolatorY.Y;
end;

procedure TAggSpanInterpolatorLinear.Coordinates(var X, Y: Integer);
begin
  X := FLineInterpolatorX.Y;
  Y := FLineInterpolatorY.Y;
end;


{ TAggSpanInterpolatorLinearSubdiv }

constructor TAggSpanInterpolatorLinearSubdiv.Create(SS: Cardinal = 8);
begin
  inherited Create(SS);

  FSubdivShift := 4;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;
end;

constructor TAggSpanInterpolatorLinearSubdiv.Create(Trans: TAggTransAffine;
  ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  inherited Create(SS);

  FSubdivShift := ASubdivShift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;

  FTrans := Trans;
end;

constructor TAggSpanInterpolatorLinearSubdiv.Create(Trans: TAggTransAffine;
  X, Y: Double; Len: Cardinal; ASubdivShift: Cardinal = 4; SS: Cardinal = 8);
begin
  Create(Trans, ASubdivShift, SS);

  SetBegin(X, Y, Len);
end;

function TAggSpanInterpolatorLinearSubdiv.GetTransformer;
begin
  Result := FTrans;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetTransformer;
begin
  FTrans := Trans;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetSubdivShift;
begin
  FSubdivShift := Shift;
  FSubdivSize := 1 shl FSubdivShift;
  FSubdivMask := FSubdivSize - 1;
end;

procedure TAggSpanInterpolatorLinearSubdiv.SetBegin;
var
  Tx, Ty: Double;
  X1, Y1: Integer;
begin
  FPos := 1;
  FSourceX := Trunc(X * FSubpixelSize) + FSubpixelSize;
  FSourceY := Y;
  FLength := Len;

  if Len > FSubdivSize then
    Len := FSubdivSize;

  Tx := X;
  Ty := Y;

  FTrans.Transform(FTrans, @Tx, @Ty);

  X1 := Trunc(Tx * FSubpixelSize);
  Y1 := Trunc(Ty * FSubpixelSize);

  Tx := X + Len;
  Ty := Y;

  FTrans.Transform(FTrans, @Tx, @Ty);

  FLineInterpolatorX.Initialize(X1, Trunc(Tx * FSubpixelSize), Len);
  FLineInterpolatorY.Initialize(Y1, Trunc(Ty * FSubpixelSize), Len);
end;

procedure TAggSpanInterpolatorLinearSubdiv.IncOperator;
var
  Tx, Ty: Double;
  Len: Cardinal;
begin
  FLineInterpolatorX.PlusOperator;
  FLineInterpolatorY.PlusOperator;

  if FPos >= FSubdivSize then
  begin
    Len := FLength;

    if Len > FSubdivSize then
      Len := FSubdivSize;

    Tx := FSourceX / FSubpixelSize + Len;
    Ty := FSourceY;

    FTrans.Transform(FTrans, @Tx, @Ty);

    FLineInterpolatorX.Initialize(FLineInterpolatorX.Y, Trunc(Tx * FSubpixelSize), Len);
    FLineInterpolatorY.Initialize(FLineInterpolatorY.Y, Trunc(Ty * FSubpixelSize), Len);

    FPos := 0;
  end;

  Inc(FSourceX, FSubpixelSize);
  Inc(FPos);
  Dec(FLength);
end;

procedure TAggSpanInterpolatorLinearSubdiv.Coordinates(X, Y: PInteger);
begin
  X^ := FLineInterpolatorX.Y;
  Y^ := FLineInterpolatorY.Y;
end;

procedure TAggSpanInterpolatorLinearSubdiv.Coordinates(var X, Y: Integer);
begin
  X := FLineInterpolatorX.Y;
  Y := FLineInterpolatorY.Y;
end;

end.
