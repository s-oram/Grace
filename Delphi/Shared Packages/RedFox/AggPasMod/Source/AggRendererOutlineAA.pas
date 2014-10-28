unit AggRendererOutlineAA;

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
  AggColor,
  AggMath,
  AggLineAABasics,
  AggDdaLine,
  AggEllipseBresenham,
  AggRendererBase,
  AggGammaFunctions,
  AggVertexSource;

const
  CMaxHalfWidth = 64;

  CAggSubpixelShift = CAggLineSubpixelShift;
  CAggSubpixelSize = 1 shl CAggSubpixelShift;
  CAggSubpixelMask = CAggSubpixelSize - 1;

  CAggAntiAliasingShift = 8;
  CAggAntiAliasingNum = 1 shl CAggAntiAliasingShift;
  CAggAntiAliasingMask = CAggAntiAliasingNum - 1;

type
  TAggCustomDistanceInterpolator = class
  public
    procedure IncX; virtual; abstract;
    procedure DecX; virtual; abstract;
    procedure IncY; virtual; abstract;
    procedure DecY; virtual; abstract;

    procedure SetIncX(Value: Integer); virtual; abstract;
    procedure SetDecX(Value: Integer); virtual; abstract;
    procedure SetIncY(Value: Integer); virtual; abstract;
    procedure SetDecY(Value: Integer); virtual; abstract;
  end;

  TAggCustomDistance0Interpolator = class(TAggCustomDistanceInterpolator)
  protected
    function GetDeltaX: Integer; virtual; abstract;
    function GetDeltaY: Integer; virtual; abstract;
    function GetDistance: Integer; virtual; abstract;
  public
    property Distance: Integer read GetDistance;

    property DeltaX: Integer read GetDeltaX;
    property DeltaY: Integer read GetDeltaY;
  end;

  TAggDistanceInterpolator0 = class(TAggCustomDistance0Interpolator)
  private
    FDelta: TPointInteger;
    FDist: Integer;
  protected
    function GetDistance: Integer; override;
    function GetDeltaX: Integer; override;
    function GetDeltaY: Integer; override;
  public
    constructor Create(X1, Y1, X2, Y2, X, Y: Integer);

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;
  end;

  TAggDistanceInterpolator1 = class(TAggCustomDistance0Interpolator)
  private
    FDelta: TPointInteger;
    FDist: Integer;
  protected
    function GetDistance: Integer; override;
    function GetDeltaX: Integer; override;
    function GetDeltaY: Integer; override;
  public
    constructor Create(X1, Y1, X2, Y2, X, Y: Integer);

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;
  end;

  TAggCustomDistance2Interpolator = class(TAggCustomDistance0Interpolator)
  protected
    function GetDeltaXStart: Integer; virtual; abstract;
    function GetDeltaYStart: Integer; virtual; abstract;
    function GetDeltaXEnd: Integer; virtual; abstract;
    function GetDeltaYEnd: Integer; virtual; abstract;
    function GetDistanceStart: Integer; virtual; abstract;
    function GetDistanceEnd: Integer; virtual; abstract;
  public
    property DistanceStart: Integer read GetDistanceStart;
    property DistanceEnd: Integer read GetDistanceEnd;

    property DxStart: Integer read GetDeltaXStart;
    property DyStart: Integer read GetDeltaYStart;
    property DxEnd: Integer read GetDeltaXEnd;
    property DyEnd: Integer read GetDeltaYEnd;
  end;

  TAggDistanceInterpolator2 = class(TAggCustomDistance2Interpolator)
  private
    FDelta, FDeltaStart: TPointInteger;
    FDist, FDistStart: Integer;
  protected
    function GetDistance: Integer; override;
    function GetDistanceStart: Integer; override;
    function GetDistanceEnd: Integer; override;
    function GetDeltaX: Integer; override;
    function GetDeltaY: Integer; override;
    function GetDeltaXStart: Integer; override;
    function GetDeltaYStart: Integer; override;
    function GetDeltaXEnd: Integer; override;
    function GetDeltaYEnd: Integer; override;
  public
    constructor Create(X1, Y1, X2, Y2, Sx, Sy, X, Y: Integer); overload;
    constructor Create(X1, Y1, X2, Y2, Ex, Ey, X, Y, Z: Integer); overload;

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;
  end;

  TAggDistanceInterpolator3 = class(TAggCustomDistance2Interpolator)
  private
    FDelta, FDeltaStart, FDeltaEnd: TPointInteger;
    FDist, FDistStart, FDistEnd: Integer;
  protected
    function GetDistance: Integer; override;
    function GetDistanceStart: Integer; override;
    function GetDistanceEnd: Integer; override;
    function GetDeltaX: Integer; override;
    function GetDeltaY: Integer; override;
    function GetDeltaXStart: Integer; override;
    function GetDeltaYStart: Integer; override;
    function GetDeltaXEnd: Integer; override;
    function GetDeltaYEnd: Integer; override;
  public
    constructor Create(X1, Y1, X2, Y2, Sx, Sy, Ex, Ey, X, Y: Integer);

    procedure IncX; override;
    procedure DecX; override;
    procedure IncY; override;
    procedure DecY; override;

    procedure SetIncX(Value: Integer); override;
    procedure SetDecX(Value: Integer); override;
    procedure SetIncY(Value: Integer); override;
    procedure SetDecY(Value: Integer); override;
  end;

  TAggCustomLineInterpolator = class
  protected
    function GetWidth: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
  public
    function StepHorizontal: Boolean; virtual; abstract;
    function StepVertical: Boolean; virtual; abstract;

    property Width: Integer read GetWidth;
    property Count: Integer read GetCount;
  end;

  TAggRendererOutlineAA = class;

  TAggCustomLineInterpolatorAA = class(TAggCustomLineInterpolator)
  private
    FLineParameters: PAggLineParameters;
    FLineInterpolator: TAggDda2LineInterpolator;
    FRendererBase: TAggRendererOutlineAA;

    FLength, FX, FY, FOldX, FOldY, FCount, FWidth, FMaxExtent, FStep: Integer;

    FDist: array [0..CMaxHalfWidth + 1 - 1] of Integer;
    FCovers: array [0..CMaxHalfWidth * 2 + 4 - 1] of Int8u;
  protected
    function GetCount: Integer; override;
    function GetWidth: Integer; override;
    function GetVertical: Boolean;
  public
    constructor Create(Ren: TAggRendererOutlineAA; Lp: PAggLineParameters);

    function StepHorizontalBase(Di: TAggCustomDistance0Interpolator): Integer;
    function StepVerticalBase(Di: TAggCustomDistance0Interpolator): Integer;

    property Vertical: Boolean read GetVertical;
  end;

  TAggLineInterpolatorAA0 = class(TAggCustomLineInterpolatorAA)
  private
    FDistanceInterpolator: TAggDistanceInterpolator1;
  public
    constructor Create(Ren: TAggRendererOutlineAA;
      Lp: PAggLineParameters);
    destructor Destroy; override;

    function StepHorizontal: Boolean; override;
    function StepVertical: Boolean; override;
  end;

  TAggLineInterpolatorAA1 = class(TAggCustomLineInterpolatorAA)
  private
    FDistanceInterpolator: TAggDistanceInterpolator2;
  public
    constructor Create(Ren: TAggRendererOutlineAA; Lp: PAggLineParameters;
      Sx, Sy: Integer);
    destructor Destroy; override;

    function StepHorizontal: Boolean; override;
    function StepVertical: Boolean; override;
  end;

  TAggLineInterpolatorAA2 = class(TAggCustomLineInterpolatorAA)
  private
    FDistanceInterpolator: TAggDistanceInterpolator2;
  public
    constructor Create(Ren: TAggRendererOutlineAA; Lp: PAggLineParameters;
      Ex, Ey: Integer);
    destructor Destroy; override;

    function StepHorizontal: Boolean; virtual;
    function StepVertical: Boolean; virtual;
  end;

  TAggLineInterpolatorAA3 = class(TAggCustomLineInterpolatorAA)
  private
    FDistanceInterpolator: TAggDistanceInterpolator3;
  public
    constructor Create(Ren: TAggRendererOutlineAA; Lp: PAggLineParameters;
      Sx, Sy, Ex, Ey: Integer);
    destructor Destroy; override;

    function StepHorizontal: Boolean; virtual;
    function StepVertical: Boolean; virtual;
  end;

  TAggLineProfileAA = class
  private
    FSize: Cardinal;
    FProfile: PInt8u;
    FGamma: array [0..CAggAntiAliasingNum - 1] of Int8u;

    FSubpixelWidth: Integer;
    FMinWidth, FSmootherWidth: Double;

    procedure SetMinWidth(W: Double);
    procedure SetSmootherWidth(W: Double);
  public
    constructor Create; overload;
    constructor Create(Width: Double; GammaFunction: TAggCustomVertexSource); overload;
    destructor Destroy; override;

    procedure SetGamma(Value: TAggCustomVertexSource);
    procedure SetWidth(Value: Double); overload;
    procedure SetWidth(CenterWidth, SmootherWidth: Double); overload;

    function GetValue(GetDist: Integer): Int8u;

    function Profile(W: Double): PInt8u;

    property ProfileSize: Cardinal read FSize;
    property SubpixelWidth: Integer read FSubpixelWidth;
    property MinWidth: Double read FMinWidth write SetMinWidth;
    property SmootherWidth: Double read FSmootherWidth write SetSmootherWidth;
  end;

  TCompareFunction = function(D: Integer): Boolean;

  TAggRendererOutline = class
  protected
    function GetAccurateJoinOnly: Boolean; virtual; abstract;
    function GetSubpixelWidth: Integer; virtual; abstract;
  public
    procedure SetColor(C: PAggColor); virtual; abstract;

    procedure Semidot(Cmp: TCompareFunction; Xc1, Yc1, Xc2, Yc2: Integer);
      virtual; abstract;

    procedure Line0(Lp: PAggLineParameters); virtual; abstract;
    procedure Line1(Lp: PAggLineParameters; Sx, Sy: Integer); virtual; abstract;
    procedure Line2(Lp: PAggLineParameters; Ex, Ey: Integer); virtual; abstract;
    procedure Line3(Lp: PAggLineParameters; Sx, Sy, Ex, Ey: Integer);
      virtual; abstract;

    property AccurateJoinOnly: Boolean read GetAccurateJoinOnly;
    property SubpixelWidth: Integer read GetSubpixelWidth;
  end;

  TAggRendererOutlineAA = class(TAggRendererOutline)
  private
    FRendererBase: TAggRendererBase;
    FProfile: TAggLineProfileAA;
    FColor: TAggColor;
    procedure SetProfile(Prof: TAggLineProfileAA);
  protected
    function GetAccurateJoinOnly: Boolean; override;
    function GetSubpixelWidth: Integer; override;
  public
    constructor Create(Ren: TAggRendererBase; Prof: TAggLineProfileAA);

    procedure SetColor(C: PAggColor); override;
    function GetColor: PAggColor;

    function Cover(D: Integer): Int8u;

    procedure BlendSolidHSpan(X, Y: Integer; Len: Cardinal; Covers: PInt8u);
    procedure BlendSolidVSpan(X, Y: Integer; Len: Cardinal; Covers: PInt8u);

    procedure SemidotHorizontalLine(Cmp: TCompareFunction;
      Xc1, Yc1, Xc2, Yc2, X1, Y1, X2: Integer);
    procedure Semidot(Cmp: TCompareFunction; Xc1, Yc1, Xc2, Yc2: Integer); override;

    procedure Line0(Lp: PAggLineParameters); override;
    procedure Line1(Lp: PAggLineParameters; Sx, Sy: Integer); override;
    procedure Line2(Lp: PAggLineParameters; Ex, Ey: Integer); override;
    procedure Line3(Lp: PAggLineParameters; Sx, Sy, Ex, Ey: Integer); override;

    property Profile: TAggLineProfileAA read FProfile write SetProfile;
  end;

implementation


{ TAggDistanceInterpolator0 }

constructor TAggDistanceInterpolator0.Create(X1, Y1, X2, Y2, X, Y: Integer);
begin
  FDelta.X := LineMedResolution(X2) - LineMedResolution(X1);
  FDelta.Y := LineMedResolution(Y2) - LineMedResolution(Y1);

  FDist := (LineMedResolution(X + CAggLineSubpixelSize div 2) - LineMedResolution(X2)) * FDelta.Y -
    (LineMedResolution(Y + CAggLineSubpixelSize div 2) - LineMedResolution(Y2)) * FDelta.X;

  FDelta.X := FDelta.X shl CAggLineMrSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineMrSubpixelShift;
end;

procedure TAggDistanceInterpolator0.IncX;
begin
  Inc(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator0.DecX;
begin
  Dec(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator0.IncY;
begin
  Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator0.DecY;
begin
  Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator0.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);

  if Value > 0 then
    Dec(FDist, FDelta.X);

  if Value < 0 then
    Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator0.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);

  if Value > 0 then
    Dec(FDist, FDelta.X);

  if Value < 0 then
    Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator0.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);

  if Value > 0 then
    Inc(FDist, FDelta.Y);

  if Value < 0 then
    Dec(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator0.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);

  if Value > 0 then
    Inc(FDist, FDelta.Y);

  if Value < 0 then
    Dec(FDist, FDelta.Y);
end;

function TAggDistanceInterpolator0.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator0.GetDeltaX;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator0.GetDeltaY;
begin
  Result := FDelta.Y;
end;


{ TAggDistanceInterpolator1 }

constructor TAggDistanceInterpolator1.Create(X1, Y1, X2, Y2, X, Y: Integer);
begin
  FDelta.X := X2 - X1;
  FDelta.Y := Y2 - Y1;

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - X2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - Y2) * FDelta.X);

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;
end;

procedure TAggDistanceInterpolator1.IncX;
begin
  Inc(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator1.DecX;
begin
  Dec(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator1.IncY;
begin
  Dec(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator1.DecY;
begin
  Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator1.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);

  if Value > 0 then
    Dec(FDist, FDelta.X);

  if Value < 0 then
    Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator1.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);

  if Value > 0 then
    Dec(FDist, FDelta.X);

  if Value < 0 then
    Inc(FDist, FDelta.X);
end;

procedure TAggDistanceInterpolator1.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);

  if Value > 0 then
    Inc(FDist, FDelta.Y);

  if Value < 0 then
    Dec(FDist, FDelta.Y);
end;

procedure TAggDistanceInterpolator1.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);

  if Value > 0 then
    Inc(FDist, FDelta.Y);

  if Value < 0 then
    Dec(FDist, FDelta.Y);
end;

function TAggDistanceInterpolator1.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator1.GetDeltaX;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator1.GetDeltaY;
begin
  Result := FDelta.Y;
end;


{ TAggDistanceInterpolator2 }

constructor TAggDistanceInterpolator2.Create(X1, Y1, X2, Y2, Sx, Sy, X, Y: Integer);
begin
  FDelta.X := X2 - X1;
  FDelta.Y := Y2 - Y1;

  FDeltaStart.X := LineMedResolution(Sx) - LineMedResolution(X1);
  FDeltaStart.Y := LineMedResolution(Sy) - LineMedResolution(Y1);

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - X2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - Y2) * FDelta.X);

  FDistStart := (LineMedResolution(X + CAggLineSubpixelSize div 2) - LineMedResolution(Sx)) *
    FDeltaStart.Y - (LineMedResolution(Y + CAggLineSubpixelSize div 2) - LineMedResolution(Sy)) *
    FDeltaStart.X;

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;

  FDeltaStart.X := FDeltaStart.X shl CAggLineMrSubpixelShift;
  FDeltaStart.Y := FDeltaStart.Y shl CAggLineMrSubpixelShift;
end;

constructor TAggDistanceInterpolator2.Create(X1, Y1, X2, Y2, Ex, Ey, X,
  Y, Z: Integer);
begin
  FDelta.X := X2 - X1;
  FDelta.Y := Y2 - Y1;

  FDeltaStart.X := LineMedResolution(Ex) - LineMedResolution(X2);
  FDeltaStart.Y := LineMedResolution(Ey) - LineMedResolution(Y2);

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - X2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - Y2) * FDelta.X);

  FDistStart := (LineMedResolution(X + CAggLineSubpixelSize div 2) - LineMedResolution(Ex)) *
    FDeltaStart.Y - (LineMedResolution(Y + CAggLineSubpixelSize div 2) - LineMedResolution(Ey)) *
    FDeltaStart.X;

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;

  FDeltaStart.X := FDeltaStart.X shl CAggLineMrSubpixelShift;
  FDeltaStart.Y := FDeltaStart.Y shl CAggLineMrSubpixelShift;
end;

procedure TAggDistanceInterpolator2.IncX;
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
end;

procedure TAggDistanceInterpolator2.DecX;
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
end;

procedure TAggDistanceInterpolator2.IncY;
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
end;

procedure TAggDistanceInterpolator2.DecY;
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
end;

procedure TAggDistanceInterpolator2.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);

  if Value > 0 then
  begin
    Dec(FDist, FDelta.X);
    Dec(FDistStart, FDeltaStart.X);
  end;

  if Value < 0 then
  begin
    Inc(FDist, FDelta.X);
    Inc(FDistStart, FDeltaStart.X);
  end;
end;

procedure TAggDistanceInterpolator2.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);

  if Value > 0 then
  begin
    Dec(FDist, FDelta.X);
    Dec(FDistStart, FDeltaStart.X);
  end;

  if Value < 0 then
  begin
    Inc(FDist, FDelta.X);
    Inc(FDistStart, FDeltaStart.X);
  end;
end;

procedure TAggDistanceInterpolator2.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);

  if Value > 0 then
  begin
    Inc(FDist, FDelta.Y);
    Inc(FDistStart, FDeltaStart.Y);
  end;

  if Value < 0 then
  begin
    Dec(FDist, FDelta.Y);
    Dec(FDistStart, FDeltaStart.Y);
  end;
end;

procedure TAggDistanceInterpolator2.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);

  if Value > 0 then
  begin
    Inc(FDist, FDelta.Y);
    Inc(FDistStart, FDeltaStart.Y);
  end;

  if Value < 0 then
  begin
    Dec(FDist, FDelta.Y);
    Dec(FDistStart, FDeltaStart.Y);
  end;
end;

function TAggDistanceInterpolator2.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator2.GetDistanceStart;
begin
  Result := FDistStart;
end;

function TAggDistanceInterpolator2.GetDistanceEnd;
begin
  Result := FDistStart;
end;

function TAggDistanceInterpolator2.GetDeltaX;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator2.GetDeltaY;
begin
  Result := FDelta.Y;
end;

function TAggDistanceInterpolator2.GetDeltaXStart;
begin
  Result := FDeltaStart.X;
end;

function TAggDistanceInterpolator2.GetDeltaYStart;
begin
  Result := FDeltaStart.Y;
end;

function TAggDistanceInterpolator2.GetDeltaXEnd;
begin
  Result := FDeltaStart.X;
end;

function TAggDistanceInterpolator2.GetDeltaYEnd;
begin
  Result := FDeltaStart.Y;
end;


{ TAggDistanceInterpolator3 }

constructor TAggDistanceInterpolator3.Create(X1, Y1, X2, Y2, Sx, Sy, Ex, Ey,
  X, Y: Integer);
begin
  FDelta.X := X2 - X1;
  FDelta.Y := Y2 - Y1;

  FDeltaStart.X := LineMedResolution(Sx) - LineMedResolution(X1);
  FDeltaStart.Y := LineMedResolution(Sy) - LineMedResolution(Y1);

  FDeltaEnd.X := LineMedResolution(Ex) - LineMedResolution(X2);
  FDeltaEnd.Y := LineMedResolution(Ey) - LineMedResolution(Y2);

  FDist := Trunc((X + CAggLineSubpixelSize * 0.5 - X2) * FDelta.Y -
    (Y + CAggLineSubpixelSize * 0.5 - Y2) * FDelta.X);

  FDistStart := (LineMedResolution(X + CAggLineSubpixelSize div 2) - LineMedResolution(Sx)) *
    FDeltaStart.Y - (LineMedResolution(Y + CAggLineSubpixelSize div 2) - LineMedResolution(Sy)) *
    FDeltaStart.X;

  FDistEnd := (LineMedResolution(X + CAggLineSubpixelSize div 2) - LineMedResolution(Ex)) * FDeltaEnd.Y
    - (LineMedResolution(Y + CAggLineSubpixelSize div 2) - LineMedResolution(Ey)) * FDeltaEnd.X;

  FDelta.X := FDelta.X shl CAggLineSubpixelShift;
  FDelta.Y := FDelta.Y shl CAggLineSubpixelShift;

  FDeltaStart.X := FDeltaStart.X shl CAggLineMrSubpixelShift;
  FDeltaStart.Y := FDeltaStart.Y shl CAggLineMrSubpixelShift;

  FDeltaEnd.X := FDeltaEnd.X shl CAggLineMrSubpixelShift;
  FDeltaEnd.Y := FDeltaEnd.Y shl CAggLineMrSubpixelShift;
end;

procedure TAggDistanceInterpolator3.IncX;
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
  Inc(FDistEnd, FDeltaEnd.Y);
end;

procedure TAggDistanceInterpolator3.DecX;
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
  Dec(FDistEnd, FDeltaEnd.Y);
end;

procedure TAggDistanceInterpolator3.IncY;
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
  Dec(FDistEnd, FDeltaEnd.X);
end;

procedure TAggDistanceInterpolator3.DecY;
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
  Inc(FDistEnd, FDeltaEnd.X);
end;

procedure TAggDistanceInterpolator3.SetIncX(Value: Integer);
begin
  Inc(FDist, FDelta.Y);
  Inc(FDistStart, FDeltaStart.Y);
  Inc(FDistEnd, FDeltaEnd.Y);

  if Value > 0 then
  begin
    Dec(FDist, FDelta.X);
    Dec(FDistStart, FDeltaStart.X);
    Dec(FDistEnd, FDeltaEnd.X);
  end;

  if Value < 0 then
  begin
    Inc(FDist, FDelta.X);
    Inc(FDistStart, FDeltaStart.X);
    Inc(FDistEnd, FDeltaEnd.X);
  end;
end;

procedure TAggDistanceInterpolator3.SetDecX(Value: Integer);
begin
  Dec(FDist, FDelta.Y);
  Dec(FDistStart, FDeltaStart.Y);
  Dec(FDistEnd, FDeltaEnd.Y);

  if Value > 0 then
  begin
    Dec(FDist, FDelta.X);
    Dec(FDistStart, FDeltaStart.X);
    Dec(FDistEnd, FDeltaEnd.X);
  end;

  if Value < 0 then
  begin
    Inc(FDist, FDelta.X);
    Inc(FDistStart, FDeltaStart.X);
    Inc(FDistEnd, FDeltaEnd.X);
  end;
end;

procedure TAggDistanceInterpolator3.SetIncY(Value: Integer);
begin
  Dec(FDist, FDelta.X);
  Dec(FDistStart, FDeltaStart.X);
  Dec(FDistEnd, FDeltaEnd.X);

  if Value > 0 then
  begin
    Inc(FDist, FDelta.Y);
    Inc(FDistStart, FDeltaStart.Y);
    Inc(FDistEnd, FDeltaEnd.Y);
  end;

  if Value < 0 then
  begin
    Dec(FDist, FDelta.Y);
    Dec(FDistStart, FDeltaStart.Y);
    Dec(FDistEnd, FDeltaEnd.Y);
  end;
end;

procedure TAggDistanceInterpolator3.SetDecY(Value: Integer);
begin
  Inc(FDist, FDelta.X);
  Inc(FDistStart, FDeltaStart.X);
  Inc(FDistEnd, FDeltaEnd.X);

  if Value > 0 then
  begin
    Inc(FDist, FDelta.Y);
    Inc(FDistStart, FDeltaStart.Y);
    Inc(FDistEnd, FDeltaEnd.Y);
  end;

  if Value < 0 then
  begin
    Dec(FDist, FDelta.Y);
    Dec(FDistStart, FDeltaStart.Y);
    Dec(FDistEnd, FDeltaEnd.Y);
  end;
end;

function TAggDistanceInterpolator3.GetDistance;
begin
  Result := FDist;
end;

function TAggDistanceInterpolator3.GetDistanceStart;
begin
  Result := FDistStart;
end;

function TAggDistanceInterpolator3.GetDistanceEnd;
begin
  Result := FDistEnd;
end;

function TAggDistanceInterpolator3.GetDeltaX;
begin
  Result := FDelta.X;
end;

function TAggDistanceInterpolator3.GetDeltaY;
begin
  Result := FDelta.Y;
end;

function TAggDistanceInterpolator3.GetDeltaXStart;
begin
  Result := FDeltaStart.X;
end;

function TAggDistanceInterpolator3.GetDeltaYStart;
begin
  Result := FDeltaStart.Y;
end;

function TAggDistanceInterpolator3.GetDeltaXEnd;
begin
  Result := FDeltaEnd.X;
end;

function TAggDistanceInterpolator3.GetDeltaYEnd;
begin
  Result := FDeltaEnd.Y;
end;


{ TAggCustomLineInterpolatorAA }

constructor TAggCustomLineInterpolatorAA.Create(Ren: TAggRendererOutlineAA;
  Lp: PAggLineParameters);
var
  Li: TAggDda2LineInterpolator;
  I : Cardinal;
  Stop: Integer;
begin
  FLineParameters := Lp;

  if Lp.Vertical then
    FLineInterpolator.Initialize(LineDoubleHighResolution(Lp.X2 - Lp.X1), Abs(Lp.Y2 - Lp.Y1))
  else
    FLineInterpolator.Initialize(LineDoubleHighResolution(Lp.Y2 - Lp.Y1), Abs(Lp.X2 - Lp.X1) + 1);

  FRendererBase := Ren;

  if Lp.Vertical = (Lp.IncValue > 0) then
    FLength := -Lp.Len
  else
    FLength := Lp.Len;

  FX := ShrInt32(Lp.X1, CAggLineSubpixelShift);
  FY := ShrInt32(Lp.Y1, CAggLineSubpixelShift);

  FOldX := FX;
  FOldY := FY;

  if Lp.Vertical then
    FCount := Abs(ShrInt32(Lp.Y2, CAggLineSubpixelShift) - FY)
  else
    FCount := Abs(ShrInt32(Lp.X2, CAggLineSubpixelShift) - FX);

  FWidth := Ren.GetSubpixelWidth;
  FMaxExtent := ShrInt32(FWidth, CAggLineSubpixelShift - 2);
  FStep := 0;

  if Lp.Vertical then
    Li.Initialize(0, Lp.Delta.Y shl CAggLineSubpixelShift, Lp.Len)
  else
    Li.Initialize(0, Lp.Delta.X shl CAggLineSubpixelShift, Lp.Len);

  Stop := FWidth + CAggLineSubpixelSize * 2;

  I := 0;

  while I < CMaxHalfWidth do
  begin
    FDist[I] := Li.Y;

    if FDist[I] >= Stop then
      Break;

    Li.PlusOperator;

    Inc(I);
  end;

  FDist[I] := $7FFF0000;
end;

function TAggCustomLineInterpolatorAA.StepHorizontalBase;
begin
  FLineInterpolator.PlusOperator;

  Inc(FX, FLineParameters.IncValue);

  FY := ShrInt32(FLineParameters.Y1 + FLineInterpolator.Y, CAggLineSubpixelShift);

  if FLineParameters.IncValue > 0 then
    Di.SetIncX(FY - FOldY)
  else
    Di.SetDecX(FY - FOldY);

  FOldY := FY;

  Result := Di.Distance div FLength;
end;

function TAggCustomLineInterpolatorAA.StepVerticalBase;
begin
  FLineInterpolator.PlusOperator;

  Inc(FY, FLineParameters.IncValue);

  FX := ShrInt32(FLineParameters.X1 + FLineInterpolator.Y, CAggLineSubpixelShift);

  if FLineParameters.IncValue > 0 then
    Di.SetIncY(FX - FOldX)
  else
    Di.SetDecY(FX - FOldX);

  FOldX := FX;

  Result := Di.Distance div FLength;
end;

function TAggCustomLineInterpolatorAA.GetVertical;
begin
  Result := FLineParameters.Vertical;
end;

function TAggCustomLineInterpolatorAA.GetWidth;
begin
  Result := FWidth;
end;

function TAggCustomLineInterpolatorAA.GetCount;
begin
  Result := FCount;
end;


{ TAggLineInterpolatorAA0 }

constructor TAggLineInterpolatorAA0.Create;
begin
  inherited Create(Ren, Lp);

  FDistanceInterpolator := TAggDistanceInterpolator1.Create(Lp.X1, Lp.Y1, Lp.X2,
    Lp.Y2, Lp.X1 and not CAggLineSubpixelMask, Lp.Y1 and not CAggLineSubpixelMask);

  FLineInterpolator.AdjustForward;
end;

destructor TAggLineInterpolatorAA0.Destroy;
begin
  FDistanceInterpolator.Free;
  inherited;
end;

function TAggLineInterpolatorAA0.StepHorizontal;
var
  Dist, Dy, S1: Integer;
  P0, P1: PInt8u;
begin
  S1 := StepHorizontalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  P1 := P0;

  P1^ := Int8u(FRendererBase.Cover(S1));

  Inc(PtrComp(P1), SizeOf(Int8u));

  Dy := 1;
  Dist := FDist[Dy] - S1;

  while Dist <= FWidth do
  begin
    P1^ := Int8u(FRendererBase.Cover(Dist));

    Inc(PtrComp(P1), SizeOf(Int8u));
    Inc(Dy);

    Dist := FDist[Dy] - S1;
  end;

  Dy := 1;
  Dist := FDist[Dy] + S1;

  while Dist <= FWidth do
  begin
    Dec(PtrComp(P0), SizeOf(Int8u));

    P0^ := Int8u(FRendererBase.Cover(Dist));

    Inc(Dy);

    Dist := FDist[Dy] + S1;
  end;

  FRendererBase.BlendSolidVSpan(FX, FY - Dy + 1,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := FStep < FCount;
end;

function TAggLineInterpolatorAA0.StepVertical;
var
  Dist, Dx, S1: Integer;
  P0, P1: PInt8u;
begin
  S1 := StepVerticalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  P1 := P0;

  P1^ := Int8u(FRendererBase.Cover(S1));

  Inc(PtrComp(P1), SizeOf(Int8u));

  Dx := 1;
  Dist := FDist[Dx] - S1;

  while Dist <= FWidth do
  begin
    P1^ := Int8u(FRendererBase.Cover(Dist));

    Inc(PtrComp(P1), SizeOf(Int8u));
    Inc(Dx);

    Dist := FDist[Dx] - S1;
  end;

  Dx := 1;
  Dist := FDist[Dx] + S1;

  while Dist <= FWidth do
  begin
    Dec(PtrComp(P0), SizeOf(Int8u));

    P0^ := Int8u(FRendererBase.Cover(Dist));

    Inc(Dx);

    Dist := FDist[Dx] + S1;
  end;

  FRendererBase.BlendSolidHSpan(FX - Dx + 1, FY,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := FStep < FCount;
end;


{ TAggLineInterpolatorAA1 }

constructor TAggLineInterpolatorAA1.Create;
var
  Npix, Dx, Dy: Integer;
  DistStart: array [0..1] of Integer;
begin
  inherited Create(Ren, Lp);

  FDistanceInterpolator := TAggDistanceInterpolator2.Create(Lp.X1, Lp.Y1, Lp.X2,
    Lp.Y2, Sx, Sy, Lp.X1 and not CAggLineSubpixelMask,
    Lp.Y1 and not CAggLineSubpixelMask);

  Npix := 1;

  if Lp.Vertical then
    repeat
      FLineInterpolator.MinusOperator;

      Dec(FY, Lp.IncValue);

      FX := ShrInt32(FLineParameters.X1 + FLineInterpolator.Y, CAggLineSubpixelShift);

      if Lp.IncValue > 0 then
        FDistanceInterpolator.SetDecY(FX - FOldX)
      else
        FDistanceInterpolator.SetIncY(FX - FOldX);

      FOldX := FX;

      DistStart[0] := FDistanceInterpolator.DistanceStart;
      DistStart[1] := DistStart[0];

      Dx := 0;

      if DistStart[0] < 0 then
        Inc(Npix);

      repeat
        Inc(DistStart[0], FDistanceInterpolator.DyStart);
        Dec(DistStart[1], FDistanceInterpolator.DyStart);

        if DistStart[0] < 0 then
          Inc(Npix);

        if DistStart[1] < 0 then
          Inc(Npix);

        Inc(Dx);

      until FDist[Dx] > FWidth;

      Dec(FStep);

      if Npix = 0 then
        Break;

      Npix := 0;

    until FStep < -FMaxExtent
  else
    repeat
      FLineInterpolator.MinusOperator;

      Dec(FX, Lp.IncValue);

      FY := ShrInt32(FLineParameters.Y1 + FLineInterpolator.Y, CAggLineSubpixelShift);

      if Lp.IncValue > 0 then
        FDistanceInterpolator.SetDecX(FY - FOldY)
      else
        FDistanceInterpolator.SetIncX(FY - FOldY);

      FOldY := FY;

      DistStart[0] := FDistanceInterpolator.DistanceStart;
      DistStart[1] := DistStart[0];

      Dy := 0;

      if DistStart[0] < 0 then
        Inc(Npix);

      repeat
        Dec(DistStart[0], FDistanceInterpolator.DxStart);
        Inc(DistStart[1], FDistanceInterpolator.DxStart);

        if DistStart[0] < 0 then
          Inc(Npix);

        if DistStart[1] < 0 then
          Inc(Npix);

        Inc(Dy);

      until FDist[Dy] > FWidth;

      Dec(FStep);

      if Npix = 0 then
        Break;

      Npix := 0;

    until FStep < -FMaxExtent;

  FLineInterpolator.AdjustForward;
end;

destructor TAggLineInterpolatorAA1.Destroy;
begin
  FDistanceInterpolator.Free;
  inherited;
end;

function TAggLineInterpolatorAA1.StepHorizontal;
var
  DistStart, Dist, Dy, S1: Integer;
  P0, P1: PInt8u;
begin
  S1 := StepHorizontalBase(FDistanceInterpolator);

  DistStart := FDistanceInterpolator.DistanceStart;

  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  P1 := P0;

  P1^ := 0;

  if DistStart <= 0 then
    P1^ := Int8u(FRendererBase.Cover(S1));

  Inc(PtrComp(P1), SizeOf(Int8u));

  Dy := 1;
  Dist := FDist[Dy] - S1;

  while Dist <= FWidth do
  begin
    Dec(DistStart, FDistanceInterpolator.DxStart);

    P1^ := 0;

    if DistStart <= 0 then
      P1^ := Int8u(FRendererBase.Cover(Dist));

    Inc(PtrComp(P1), SizeOf(Int8u));
    Inc(Dy);

    Dist := FDist[Dy] - S1;
  end;

  Dy := 1;
  DistStart := FDistanceInterpolator.DistanceStart;
  Dist := FDist[Dy] + S1;

  while Dist <= FWidth do
  begin
    Inc(DistStart, FDistanceInterpolator.DxStart);
    Dec(PtrComp(P0), SizeOf(Int8u));

    P0^ := 0;

    if DistStart <= 0 then
      P0^ := Int8u(FRendererBase.Cover(Dist));

    Inc(Dy);

    Dist := FDist[Dy] + S1;
  end;

  FRendererBase.BlendSolidVSpan(FX, FY - Dy + 1,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := FStep < FCount;
end;

function TAggLineInterpolatorAA1.StepVertical;
var
  DistStart, Dist, Dx, S1: Integer;
  P0, P1: PInt8u;
begin
  S1 := StepVerticalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  P1 := P0;

  DistStart := FDistanceInterpolator.DistanceStart;

  P1^ := 0;

  if DistStart <= 0 then
    P1^ := Int8u(FRendererBase.Cover(S1));

  Inc(PtrComp(P1), SizeOf(Int8u));

  Dx := 1;
  Dist := FDist[Dx] - S1;

  while Dist <= FWidth do
  begin
    Inc(DistStart, FDistanceInterpolator.DyStart);

    P1^ := 0;

    if DistStart <= 0 then
      P1^ := Int8u(FRendererBase.Cover(Dist));

    Inc(PtrComp(P1), SizeOf(Int8u));
    Inc(Dx);

    Dist := FDist[Dx] - S1;
  end;

  Dx := 1;
  DistStart := FDistanceInterpolator.DistanceStart;
  Dist := FDist[Dx] + S1;

  while Dist <= FWidth do
  begin
    Dec(DistStart, FDistanceInterpolator.DyStart);
    Dec(PtrComp(P0), SizeOf(Int8u));

    P0^ := 0;

    if DistStart <= 0 then
      P0^ := Int8u(FRendererBase.Cover(Dist));

    Inc(Dx);

    Dist := FDist[Dx] + S1;
  end;

  FRendererBase.BlendSolidHSpan(FX - Dx + 1, FY,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := FStep < FCount;
end;


{ TAggLineInterpolatorAA2 }

constructor TAggLineInterpolatorAA2.Create;
begin
  inherited Create(Ren, Lp);

  FDistanceInterpolator := TAggDistanceInterpolator2.Create(Lp.X1, Lp.Y1, Lp.X2,
    Lp.Y2, Ex, Ey, Lp.X1 and not CAggLineSubpixelMask,
    Lp.Y1 and not CAggLineSubpixelMask, 0);

  FLineInterpolator.AdjustForward;

  Dec(FStep, FMaxExtent);
end;

destructor TAggLineInterpolatorAA2.Destroy;
begin
  FDistanceInterpolator.Free;
  inherited
end;

function TAggLineInterpolatorAA2.StepHorizontal;
var
  DistEnd, Dist, Dy, S1, Npix: Integer;
  P0, P1: PInt8u;
begin
  S1 := StepHorizontalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  P1 := P0;

  DistEnd := FDistanceInterpolator.DistanceEnd;

  Npix := 0;
  P1^ := 0;

  if DistEnd > 0 then
  begin
    P1^ := Int8u(FRendererBase.Cover(S1));

    Inc(Npix);
  end;

  Inc(PtrComp(P1), SizeOf(Int8u));

  Dy := 1;
  Dist := FDist[Dy] - S1;

  while Dist <= FWidth do
  begin
    Dec(DistEnd, FDistanceInterpolator.DxEnd);

    P1^ := 0;

    if DistEnd > 0 then
    begin
      P1^ := Int8u(FRendererBase.Cover(Dist));

      Inc(Npix);
    end;

    Inc(PtrComp(P1), SizeOf(Int8u));
    Inc(Dy);

    Dist := FDist[Dy] - S1;
  end;

  Dy := 1;
  DistEnd := FDistanceInterpolator.DistanceEnd;
  Dist := FDist[Dy] + S1;

  while Dist <= FWidth do
  begin
    Inc(DistEnd, FDistanceInterpolator.DxEnd);
    Dec(PtrComp(P0), SizeOf(Int8u));

    P0^ := 0;

    if DistEnd > 0 then
    begin
      P0^ := Int8u(FRendererBase.Cover(Dist));

      Inc(Npix);
    end;

    Inc(Dy);

    Dist := FDist[Dy] + S1;
  end;

  FRendererBase.BlendSolidVSpan(FX, FY - Dy + 1,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

function TAggLineInterpolatorAA2.StepVertical;
var
  DistEnd, Dist, Dx, S1, Npix: Integer;
  P0, P1: PInt8u;
begin
  S1 := StepVerticalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  P1 := P0;

  DistEnd := FDistanceInterpolator.DistanceEnd;

  Npix := 0;
  P1^ := 0;

  if DistEnd > 0 then
  begin
    P1^ := Int8u(FRendererBase.Cover(S1));

    Inc(Npix);
  end;

  Inc(PtrComp(P1), SizeOf(Int8u));

  Dx := 1;
  Dist := FDist[Dx] - S1;

  while Dist <= FWidth do
  begin
    Inc(DistEnd, FDistanceInterpolator.DyEnd);

    P1^ := 0;

    if DistEnd > 0 then
    begin
      P1^ := Int8u(FRendererBase.Cover(Dist));

      Inc(Npix);
    end;

    Inc(PtrComp(P1), SizeOf(Int8u));
    Inc(Dx);

    Dist := FDist[Dx] - S1;
  end;

  Dx := 1;
  DistEnd := FDistanceInterpolator.DistanceEnd;
  Dist := FDist[Dx] + S1;

  while Dist <= FWidth do
  begin
    Dec(DistEnd, FDistanceInterpolator.DyEnd);
    Dec(PtrComp(P0), SizeOf(Int8u));

    P0^ := 0;

    if DistEnd > 0 then
    begin
      P0^ := Int8u(FRendererBase.Cover(Dist));

      Inc(Npix);
    end;

    Inc(Dx);

    Dist := FDist[Dx] + S1;
  end;

  FRendererBase.BlendSolidHSpan(FX - Dx + 1, FY,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;


{ TAggLineInterpolatorAA3 }

constructor TAggLineInterpolatorAA3.Create(Ren: TAggRendererOutlineAA;
  Lp: PAggLineParameters; Sx, Sy, Ex, Ey: Integer);
var
  Dist1Start, Dist2Start, Npix, Dx, Dy: Integer;
begin
  inherited Create(Ren, Lp);

  FDistanceInterpolator := TAggDistanceInterpolator3.Create(Lp.X1, Lp.Y1, Lp.X2,
    Lp.Y2, Sx, Sy, Ex, Ey, Lp.X1 and not CAggLineSubpixelMask,
    Lp.Y1 and not CAggLineSubpixelMask);

  Npix := 1;

  if Lp.Vertical then
    repeat
      FLineInterpolator.MinusOperator;

      Dec(FY, Lp.IncValue);

      FX := ShrInt32(FLineParameters.X1 + FLineInterpolator.Y, CAggLineSubpixelShift);

      if Lp.IncValue > 0 then
        FDistanceInterpolator.SetDecY(FX - FOldX)
      else
        FDistanceInterpolator.SetIncY(FX - FOldX);

      FOldX := FX;

      Dist1Start := FDistanceInterpolator.DistanceStart;
      Dist2Start := Dist1Start;

      Dx := 0;

      if Dist1Start < 0 then
        Inc(Npix);

      repeat
        Inc(Dist1Start, FDistanceInterpolator.DyStart);
        Dec(Dist2Start, FDistanceInterpolator.DyStart);

        if Dist1Start < 0 then
          Inc(Npix);

        if Dist2Start < 0 then
          Inc(Npix);

        Inc(Dx);

      until FDist[Dx] > FWidth;

      if Npix = 0 then
        Break;

      Npix := 0;

      Dec(FStep);

    until FStep < -FMaxExtent
  else
    repeat
      FLineInterpolator.MinusOperator;

      Dec(FX, Lp.IncValue);

      FY := ShrInt32(FLineParameters.Y1 + FLineInterpolator.Y, CAggLineSubpixelShift);

      if Lp.IncValue > 0 then
        FDistanceInterpolator.SetDecX(FY - FOldY)
      else
        FDistanceInterpolator.SetIncX(FY - FOldY);

      FOldY := FY;

      Dist1Start := FDistanceInterpolator.DistanceStart;
      Dist2Start := Dist1Start;

      Dy := 0;

      if Dist1Start < 0 then
        Inc(Npix);

      repeat
        Dec(Dist1Start, FDistanceInterpolator.DxStart);
        Inc(Dist2Start, FDistanceInterpolator.DxStart);

        if Dist1Start < 0 then
          Inc(Npix);

        if Dist2Start < 0 then
          Inc(Npix);

        Inc(Dy);

      until FDist[Dy] > FWidth;

      if Npix = 0 then
        Break;

      Npix := 0;

      Dec(FStep);

    until FStep < -FMaxExtent;

  FLineInterpolator.AdjustForward;

  Dec(FStep, FMaxExtent);
end;

destructor TAggLineInterpolatorAA3.Destroy;
begin
  FDistanceInterpolator.Free;
  inherited;
end;

function TAggLineInterpolatorAA3.StepHorizontal;
var
  DistStart, DistEnd, GetDist, Dy, S1, Npix: Integer;
  P0, P1: PInt8u;
begin
  S1 := StepHorizontalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  P1 := P0;

  DistStart := FDistanceInterpolator.DistanceStart;
  DistEnd := FDistanceInterpolator.DistanceEnd;

  Npix := 0;
  P1^ := 0;

  if DistEnd > 0 then
  begin
    if DistStart <= 0 then
      P1^ := Int8u(FRendererBase.Cover(S1));

    Inc(Npix);
  end;

  Inc(PtrComp(P1), SizeOf(Int8u));

  Dy := 1;
  GetDist := FDist[Dy] - S1;

  while GetDist <= FWidth do
  begin
    Dec(DistStart, FDistanceInterpolator.DxStart);
    Dec(DistEnd, FDistanceInterpolator.DxEnd);

    P1^ := 0;

    if (DistEnd > 0) and (DistStart <= 0) then
    begin
      P1^ := Int8u(FRendererBase.Cover(GetDist));

      Inc(Npix);
    end;

    Inc(PtrComp(P1), SizeOf(Int8u));
    Inc(Dy);

    GetDist := FDist[Dy] - S1;
  end;

  Dy := 1;
  DistStart := FDistanceInterpolator.DistanceStart;
  DistEnd := FDistanceInterpolator.DistanceEnd;
  GetDist := FDist[Dy] + S1;

  while GetDist <= FWidth do
  begin
    Inc(DistStart, FDistanceInterpolator.DxStart);
    Inc(DistEnd, FDistanceInterpolator.DxEnd);
    Dec(PtrComp(P0), SizeOf(Int8u));

    P0^ := 0;

    if (DistEnd > 0) and (DistStart <= 0) then
    begin
      P0^ := Int8u(FRendererBase.Cover(GetDist));

      Inc(Npix);
    end;

    Inc(Dy);

    GetDist := FDist[Dy] + S1;
  end;

  FRendererBase.BlendSolidVSpan(FX, FY - Dy + 1,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;

function TAggLineInterpolatorAA3.StepVertical;
var
  DistStart, DistEnd, GetDist, Dx, S1, Npix: Integer;
  P0, P1: PInt8u;
begin
  S1 := StepVerticalBase(FDistanceInterpolator);
  P0 := PInt8u(PtrComp(@FCovers[0]) + (CMaxHalfWidth + 2) * SizeOf(Int8u));
  P1 := P0;

  DistStart := FDistanceInterpolator.DistanceStart;
  DistEnd := FDistanceInterpolator.DistanceEnd;

  Npix := 0;
  P1^ := 0;

  if DistEnd > 0 then
  begin
    if DistStart <= 0 then
      P1^ := Int8u(FRendererBase.Cover(S1));

    Inc(Npix);
  end;

  Inc(PtrComp(P1), SizeOf(Int8u));

  Dx := 1;
  GetDist := FDist[Dx] - S1;

  while GetDist <= FWidth do
  begin
    Inc(DistStart, FDistanceInterpolator.DyStart);
    Inc(DistEnd, FDistanceInterpolator.DyEnd);

    P1^ := 0;

    if (DistEnd > 0) and (DistStart <= 0) then
    begin
      P1^ := Int8u(FRendererBase.Cover(GetDist));

      Inc(Npix);
    end;

    Inc(PtrComp(P1), SizeOf(Int8u));
    Inc(Dx);

    GetDist := FDist[Dx] - S1;
  end;

  Dx := 1;
  DistStart := FDistanceInterpolator.DistanceStart;
  DistEnd := FDistanceInterpolator.DistanceEnd;
  GetDist := FDist[Dx] + S1;

  while GetDist <= FWidth do
  begin
    Dec(DistStart, FDistanceInterpolator.DyStart);
    Dec(DistEnd, FDistanceInterpolator.DyEnd);
    Dec(PtrComp(P0), SizeOf(Int8u));

    P0^ := 0;

    if (DistEnd > 0) and (DistStart <= 0) then
    begin
      P0^ := Int8u(FRendererBase.Cover(GetDist));

      Inc(Npix);
    end;

    Inc(Dx);

    GetDist := FDist[Dx] + S1;
  end;

  FRendererBase.BlendSolidHSpan(FX - Dx + 1, FY,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), P0);

  Inc(FStep);

  Result := (Npix <> 0) and (FStep < FCount);
end;


{ TAggLineProfileAA }

constructor TAggLineProfileAA.Create;
var
  I: Integer;
begin
  FSize := 0;
  FProfile := 0;

  FSubpixelWidth := 0;
  FMinWidth := 1.0;
  FSmootherWidth := 1.0;

  for I := 0 to CAggAntiAliasingNum - 1 do
    FGamma[I] := Int8u(I);
end;

constructor TAggLineProfileAA.Create(Width: Double;
  GammaFunction: TAggCustomVertexSource);
begin
  FSize := 0;
  FProfile := 0;

  FSubpixelWidth := 0;
  FMinWidth := 1.0;
  FSmootherWidth := 1.0;

  SetGamma(GammaFunction);
  SetWidth(Width);
end;

destructor TAggLineProfileAA.Destroy;
begin
  AggFreeMem(Pointer(FProfile), FSize * SizeOf(Int8u));
  inherited;
end;

procedure TAggLineProfileAA.SetMinWidth;
begin
  FMinWidth := W;
end;

procedure TAggLineProfileAA.SetSmootherWidth;
begin
  FSmootherWidth := W;
end;

procedure TAggLineProfileAA.SetGamma(Value: TAggCustomVertexSource);
var
  I: Integer;
begin
  for I := 0 to CAggAntiAliasingNum - 1 do
    FGamma[I] := Int8u(Trunc(Value.FuncOperatorGamma(I / CAggAntiAliasingMask) *
      CAggAntiAliasingMask + 0.5));
end;

procedure TAggLineProfileAA.SetWidth(Value: Double);
var
  SmootherWidth: Double;
begin
  if Value < 0.0 then
    Value := 0.0;

  if Value < FSmootherWidth then
    Value := Value + Value
  else
    Value := Value + FSmootherWidth;

  Value := Value * 0.5;
  Value := Value - FSmootherWidth;
  SmootherWidth := FSmootherWidth;

  if Value < 0.0 then
  begin
    SmootherWidth := SmootherWidth + Value;
    Value := 0.0;
  end;

  SetWidth(Value, SmootherWidth);
end;

function TAggLineProfileAA.GetValue;
begin
  Result := PInt8u(PtrComp(FProfile) + (GetDist + CAggSubpixelSize * 2) *
    SizeOf(Int8u))^;
end;

function TAggLineProfileAA.Profile(W: Double): PInt8u;
var
  Size: Cardinal;
begin
  FSubpixelWidth := Trunc(W * CAggSubpixelSize);

  Size := FSubpixelWidth + CAggSubpixelSize * 6;

  if Size > FSize then
  begin
    AggFreeMem(Pointer(FProfile), FSize * SizeOf(Int8u));
    AggGetMem(Pointer(FProfile), Size * SizeOf(Int8u));

    FSize := Size;
  end;

  Result := FProfile;
end;

procedure TAggLineProfileAA.SetWidth(CenterWidth, SmootherWidth: Double);
var
  BaseVal, Width, K: Double;
  SubpixelCenterWidth, SubpixelSmootherWidth, I, Val, N_smoother: Cardinal;
  Ch, ChCenter, ChCmoother: PInt8u;
begin
  BaseVal := 1.0;

  if CenterWidth = 0.0 then
    CenterWidth := 1.0 / CAggSubpixelSize;

  if SmootherWidth = 0.0 then
    SmootherWidth := 1.0 / CAggSubpixelSize;

  Width := CenterWidth + SmootherWidth;

  if Width < FMinWidth then
  begin
    K := Width / FMinWidth;

    BaseVal := BaseVal * K;
    CenterWidth := CenterWidth / K;
    SmootherWidth := SmootherWidth / K;
  end;

  Ch := Profile(CenterWidth + SmootherWidth);

  SubpixelCenterWidth := Trunc(CenterWidth * CAggSubpixelSize);
  SubpixelSmootherWidth := Trunc(SmootherWidth * CAggSubpixelSize);

  ChCenter := PInt8u(PtrComp(Ch) + CAggSubpixelSize * 2 * SizeOf(Int8u));
  ChCmoother := PInt8u(PtrComp(ChCenter) + SubpixelCenterWidth *
    SizeOf(Int8u));

  Val := FGamma[Trunc(BaseVal * CAggAntiAliasingMask)];

  Ch := ChCenter;

  I := 0;

  while I < SubpixelCenterWidth do
  begin
    Ch^ := Int8u(Val);

    Inc(PtrComp(Ch), SizeOf(Int8u));
    Inc(I);
  end;

  I := 0;

  while I < SubpixelSmootherWidth do
  begin
    ChCmoother^ := FGamma
      [Trunc((BaseVal - BaseVal * (I / SubpixelSmootherWidth)) * CAggAntiAliasingMask)];

    Inc(PtrComp(ChCmoother), SizeOf(Int8u));
    Inc(I);
  end;

  N_smoother := ProfileSize - SubpixelSmootherWidth - SubpixelCenterWidth
    - CAggSubpixelSize * 2;

  Val := FGamma[0];

  for I := 0 to N_smoother - 1 do
  begin
    ChCmoother^ := Int8u(Val);

    Inc(PtrComp(ChCmoother), SizeOf(Int8u));
  end;

  Ch := ChCenter;

  for I := 0 to CAggSubpixelSize * 2 - 1 do
  begin
    Ch^ := ChCenter^;

    Dec(PtrComp(Ch), SizeOf(Int8u));
    Inc(PtrComp(ChCenter), SizeOf(Int8u));
  end;
end;


{ TAggRendererOutlineAA }

constructor TAggRendererOutlineAA.Create(Ren: TAggRendererBase;
  Prof: TAggLineProfileAA);
begin
  Assert(Ren is TAggRendererBase);
  FRendererBase := Ren;
  FProfile := Prof;
end;

procedure TAggRendererOutlineAA.SetColor;
begin
  FColor := C^;
end;

function TAggRendererOutlineAA.GetColor;
begin
  Result := @FColor;
end;

procedure TAggRendererOutlineAA.SetProfile;
begin
  FProfile := Prof;
end;

function TAggRendererOutlineAA.GetSubpixelWidth;
begin
  Result := FProfile.SubpixelWidth;
end;

function TAggRendererOutlineAA.Cover;
begin
  Result := Int8u(FProfile.GetValue(D));
end;

procedure TAggRendererOutlineAA.BlendSolidHSpan;
begin
  FRendererBase.BlendSolidHSpan(X, Y, Len, @FColor, Covers);
end;

procedure TAggRendererOutlineAA.BlendSolidVSpan;
begin
  FRendererBase.BlendSolidVSpan(X, Y, Len, @FColor, Covers);
end;

function TAggRendererOutlineAA.GetAccurateJoinOnly;
begin
  Result := False;
end;

procedure TAggRendererOutlineAA.SemidotHorizontalLine;
var
  Covers: array [0..CMaxHalfWidth * 2 + 4 - 1] of Int8u;
  P0, P1: PInt8u;

  X, Y, W, X0, Dx, Dy, D: Integer;

  Di: TAggDistanceInterpolator0;

begin
  P0 := @Covers[0];
  P1 := @Covers[0];

  X := X1 shl CAggLineSubpixelShift;
  Y := Y1 shl CAggLineSubpixelShift;
  W := GetSubpixelWidth;

  Di := TAggDistanceInterpolator0.Create(Xc1, Yc1, Xc2, Yc2, X, Y);
  try
    Inc(X, CAggLineSubpixelSize div 2);
    Inc(Y, CAggLineSubpixelSize div 2);

    X0 := X1;
    Dx := X - Xc1;
    Dy := Y - Yc1;

    repeat
      D := Trunc(FastSqrt(Dx * Dx + Dy * Dy));

      P1^ := 0;

      if Cmp(Di.Distance) and (D <= W) then
        P1^ := Int8u(Cover(D));

      Inc(PtrComp(P1), SizeOf(Int8u));
      Inc(Dx, CAggLineSubpixelSize);

      Di.IncX;

      Inc(X1);
    until X1 > X2;
  finally
    Di.Free;
  end;

  FRendererBase.BlendSolidHSpan(X0, Y1,
    Cardinal((PtrComp(P1) - PtrComp(P0)) div SizeOf(Int8u)), GetColor, P0);
end;

procedure TAggRendererOutlineAA.Semidot;
var
  R, Dx, Dy, Dy0, Dx0, X, Y: Integer;
  Ei: TAggEllipseBresenhamInterpolator;
begin
  R := ShrInt32(GetSubpixelWidth + CAggLineSubpixelMask, CAggLineSubpixelShift);

  if R < 1 then
    R := 1;

  Ei.Initialize(R);

  Dx := 0;
  Dy := -R;
  Dy0 := Dy;
  Dx0 := Dx;
  X := ShrInt32(Xc1, CAggLineSubpixelShift);
  Y := ShrInt32(Yc1, CAggLineSubpixelShift);

  repeat
    Inc(Dx, Ei.DeltaX);
    Inc(Dy, Ei.DeltaY);

    if Dy <> Dy0 then
    begin
      SemidotHorizontalLine(Cmp, Xc1, Yc1, Xc2, Yc2, X - Dx0, Y + Dy0, X + Dx0);
      SemidotHorizontalLine(Cmp, Xc1, Yc1, Xc2, Yc2, X - Dx0, Y - Dy0, X + Dx0);
    end;

    Dx0 := Dx;
    Dy0 := Dy;

    Ei.IncOperator;

  until Dy >= 0;

  SemidotHorizontalLine(Cmp, Xc1, Yc1, Xc2, Yc2, X - Dx0, Y + Dy0, X + Dx0);
end;

procedure TAggRendererOutlineAA.Line0(Lp: PAggLineParameters);
var
  Li: TAggLineInterpolatorAA0;
begin
  Li := TAggLineInterpolatorAA0.Create(Self, Lp);
  try
    if Li.Count <> 0 then
      if Li.Vertical then
        while Li.StepVertical do
        else
          while Li.StepHorizontal do;
  finally
    Li.Free;
  end;
end;

procedure TAggRendererOutlineAA.Line1(Lp: PAggLineParameters; Sx, Sy: Integer);
var
  Li: TAggLineInterpolatorAA1;
begin
  FixDegenerateBisectrixStart(Lp, @Sx, @Sy);

  Li := TAggLineInterpolatorAA1.Create(Self, Lp, Sx, Sy);
  try
    if Li.Vertical then
      while Li.StepVertical do
      else
        while Li.StepHorizontal do;
  finally
    Li.Free;
  end;
end;

procedure TAggRendererOutlineAA.Line2(Lp: PAggLineParameters; Ex, Ey: Integer);
var
  Li: TAggLineInterpolatorAA2;
begin
  FixDegenerateBisectrixEnd(Lp, @Ex, @Ey);

  Li := TAggLineInterpolatorAA2.Create(Self, Lp, Ex, Ey);
  try
    if Li.Vertical then
      while Li.StepVertical do
      else
        while Li.StepHorizontal do;
  finally
    Li.Free;
  end;
end;

procedure TAggRendererOutlineAA.Line3(Lp: PAggLineParameters; Sx, Sy,
  Ex, Ey: Integer);
var
  Li: TAggLineInterpolatorAA3;
begin
  FixDegenerateBisectrixStart(Lp, @Sx, @Sy);
  FixDegenerateBisectrixEnd(Lp, @Ex, @Ey);

  Li := TAggLineInterpolatorAA3.Create(Self, Lp, Sx, Sy, Ex, Ey);
  try
    if Li.Vertical then
      while Li.StepVertical do
      else
        while Li.StepHorizontal do;
  finally
    Li.Free;
  end;
end;

end.
