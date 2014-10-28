unit AggControl;

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
  AggTransAffine,
  AggRasterizerScanLine,
  AggScanLine,
  AggRendererScanLine,
  AggRenderScanLines,
  AggVertexSource,
  AggColor;

type
  TAggCustomAggControl = class(TAggCustomVertexSource)
  private
    FFlipY: Boolean;
    FMatrix: TAggTransAffine;
    function GetScale: Double;
  protected
    FRect: TRectDouble;
    function GetColorPointer(Index: Cardinal): PAggColor; virtual; abstract;
  public
    constructor Create(X1, Y1, X2, Y2: Double; FlipY: Boolean); virtual;
    destructor Destroy; override;

    procedure SetClipBox(X1, Y1, X2, Y2: Double); overload; virtual;
    procedure SetClipBox(ClipBox: TRectDouble); overload; virtual;

    function InRect(X, Y: Double): Boolean; virtual;

    function OnMouseButtonDown(X, Y: Double): Boolean; virtual;
    function OnMouseButtonUp(X, Y: Double): Boolean; virtual;

    function OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean; virtual;
    function OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean; virtual;

    procedure Transform(Matrix: TAggTransAffine);
    procedure TransformXY(X, Y: PDouble);
    procedure InverseTransformXY(X, Y: PDouble); overload;
    procedure InverseTransformXY(var X, Y: Double); overload;
    procedure NoTransform;

    property ColorPointer[Index: Cardinal]: PAggColor read GetColorPointer;
    property Scale: Double read GetScale;
  end;

procedure RenderControl(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine;
  R: TAggCustomRendererScanLineSolid; C: TAggCustomAggControl);

implementation


{ TAggCustomAggControl }

constructor TAggCustomAggControl.Create;
begin
  inherited Create;

  FRect.X1 := X1;
  FRect.Y1 := Y1;
  FRect.X2 := X2;
  FRect.Y2 := Y2;

  FFlipY := FlipY;

  FMatrix := nil;
end;

destructor TAggCustomAggControl.Destroy;
begin
  inherited;
end;

function TAggCustomAggControl.InRect(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseButtonDown(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseButtonUp(X, Y: Double): Boolean;
begin
  Result := False;
end;

function TAggCustomAggControl.OnMouseMove(X, Y: Double; ButtonFlag: Boolean): Boolean;
begin
  Result := False;
end;

procedure TAggCustomAggControl.SetClipBox(X1, Y1, X2, Y2: Double);
begin
  SetClipBox(RectDouble(X1, Y1, X2, Y2));
end;

procedure TAggCustomAggControl.SetClipBox(ClipBox: TRectDouble);
begin
  FRect := ClipBox;
end;

function TAggCustomAggControl.OnArrowKeys(Left, Right, Down, Up: Boolean): Boolean;
begin
  Result := False;
end;

procedure TAggCustomAggControl.Transform(Matrix: TAggTransAffine);
begin
  FMatrix := Matrix;
end;

procedure TAggCustomAggControl.NoTransform;
begin
  FMatrix := nil;
end;

procedure TAggCustomAggControl.TransformXY(X, Y: PDouble);
begin
  if FFlipY then
    Y^ := FRect.Y1 + FRect.Y2 - Y^;

  if FMatrix <> nil then
    FMatrix.Transform(FMatrix, X, Y);
end;

procedure TAggCustomAggControl.InverseTransformXY(X, Y: PDouble);
begin
  if FMatrix <> nil then
    FMatrix.InverseTransform(FMatrix, X, Y);

  if FFlipY then
    Y^ := FRect.Y1 + FRect.Y2 - Y^;
end;

procedure TAggCustomAggControl.InverseTransformXY(var X, Y: Double);
begin
  InverseTransformXY(@X, @Y);
end;

function TAggCustomAggControl.GetScale: Double;
begin
  if FMatrix <> nil then
    Result := FMatrix.GetScale
  else
    Result := 1.0;
end;

procedure RenderControl(Ras: TAggRasterizerScanLine; Sl: TAggCustomScanLine;
  R: TAggCustomRendererScanLineSolid; C: TAggCustomAggControl);
var
  I: Cardinal;
begin
  if C.PathCount > 0 then
    for I := 0 to C.PathCount - 1 do
    begin
      Ras.Reset;
      Ras.AddPath(C, I);

      R.SetColor(C.ColorPointer[I]);

      RenderScanLines(Ras, Sl, R);
    end;
end;

end.
