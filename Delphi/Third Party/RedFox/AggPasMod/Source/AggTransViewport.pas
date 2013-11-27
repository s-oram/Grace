unit AggTransViewport;

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
//                                                                            //
//  Viewport transformer - simple orthogonal conversions from world           //
//  coordinates to screen (device) ones                                       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  AggBasics,
  AggTransAffine;

type
  TAggAspectRatio = (arStretch, arMeet, arSlice);

  TAggTransViewport = class(TAggTransAffine)
  private
    FAspect: TAggAspectRatio;
    FIsValid: Boolean;
    FAlign, FDelta: TPointDouble;
    FActualWorld, FWorld, FDevice: TRectDouble;
    FK: TPointDouble;
    function GetDeviceDeltaX: Double;
    function GetDeviceDeltaY: Double;
    function GetScale: Double;
    function GetByteSize: Cardinal;
  public
    constructor Create;

    procedure PreserveAspectRatio(AlignX, AlignY: Double;
      Aspect: TAggAspectRatio); overload;
    procedure PreserveAspectRatio(Align: TPointDouble;
      Aspect: TAggAspectRatio); overload;

    procedure DeviceViewport(X1, Y1, X2, Y2: Double); overload;
    procedure DeviceViewport(Rect: TRectDouble); overload;
    procedure DeviceViewportNoUpdate(var X1, Y1, X2, Y2: Double);

    procedure WorldViewport(X1, Y1, X2, Y2: Double); overload;
    procedure WorldViewport(Rect: TRectDouble); overload;

    procedure WorldViewportNoUpdate(var X1, Y1, X2, Y2: Double); overload;
    procedure WorldViewportNoUpdate(var Rect: TRectDouble); overload;

    procedure WorldViewportActual(var X1, Y1, X2, Y2: Double);

    procedure InverseTransformScaleOnly(var X, Y: Double);

    procedure ToAffine(Mtx: TAggTransAffine);
    procedure ToAffineScaleOnly(Mtx: TAggTransAffine);

    procedure Serialize(Ptr: PInt8u);
    procedure Deserialize(Ptr: PInt8u);

    procedure Update;

    property AspectRatio: TAggAspectRatio read FAspect;
    property IsValid: Boolean read FIsValid;
    property AlignX: Double read FAlign.X;
    property AlignY: Double read FAlign.Y;

    property ByteSize: Cardinal read GetByteSize;
    property DeviceDeltaX: Double read GetDeviceDeltaX;
    property DeviceDeltaY: Double read GetDeviceDeltay;
    property ScaleX: Double read FK.X;
    property ScaleY: Double read FK.Y;
    property Scale: Double read GetScale;
  end;

implementation

procedure Transform(This: TAggTransViewport; X, Y: PDouble);
begin
  X^ := X^ * This.FK.X;
  Y^ := Y^ * This.FK.Y;
end;

procedure InverseTransform(This: TAggTransViewport; X, Y: PDouble);
begin
  X^ := (X^ - This.FDelta.X) / This.FK.X + This.FActualWorld.X1;
  Y^ := (Y^ - This.FDelta.Y) / This.FK.Y + This.FActualWorld.Y1;
end;


{ TAggTransViewport }

constructor TAggTransViewport.Create;
begin
  inherited Create;

  Transform := @Transform;
  InverseTransform := @InverseTransform;

  FWorld.X1 := 0;
  FWorld.Y1 := 0;
  FWorld.X2 := 1;
  FWorld.Y2 := 1;

  FDevice.X1 := 0;
  FDevice.Y1 := 0;
  FDevice.X2 := 1;
  FDevice.Y2 := 1;

  FAspect := arStretch;

  FAlign.X := 0.5;
  FAlign.Y := 0.5;

  FActualWorld.X1 := 0;
  FActualWorld.Y1 := 0;
  FActualWorld.X2 := 1;
  FActualWorld.Y2 := 1;
  FDelta.X := 0;
  FDelta.Y := 0;

  FK.X := 1;
  FK.Y := 1;
end;

procedure TAggTransViewport.PreserveAspectRatio(AlignX, AlignY: Double;
  Aspect: TAggAspectRatio);
begin
  FAlign.X := AlignX;
  FAlign.Y := AlignY;
  FAspect := Aspect;

  Update;
end;

procedure TAggTransViewport.PreserveAspectRatio(Align: TPointDouble;
  Aspect: TAggAspectRatio);
begin
  FAlign := Align;
  FAspect := Aspect;

  Update;
end;

procedure TAggTransViewport.DeviceViewport(X1, Y1, X2, Y2: Double);
begin
  FDevice.X1 := X1;
  FDevice.Y1 := Y1;
  FDevice.X2 := X2;
  FDevice.Y2 := Y2;

  Update;
end;

procedure TAggTransViewport.DeviceViewport(Rect: TRectDouble);
begin
  FDevice.X1 := Rect.X1;
  FDevice.Y1 := Rect.Y1;
  FDevice.X2 := Rect.X2;
  FDevice.Y2 := Rect.Y2;

  Update;
end;

procedure TAggTransViewport.DeviceViewportNoUpdate(var X1, Y1, X2, Y2: Double);
begin
  X1 := FDevice.X1;
  Y1 := FDevice.Y1;
  X2 := FDevice.X2;
  Y2 := FDevice.Y2;
end;

procedure TAggTransViewport.WorldViewport(X1, Y1, X2, Y2: Double);
begin
  FWorld.X1 := X1;
  FWorld.Y1 := Y1;
  FWorld.X2 := X2;
  FWorld.Y2 := Y2;

  Update;
end;

procedure TAggTransViewport.WorldViewport(Rect: TRectDouble);
begin
  FWorld.X1 := Rect.X1;
  FWorld.Y1 := Rect.Y1;
  FWorld.X2 := Rect.X2;
  FWorld.Y2 := Rect.Y2;

  Update;
end;

procedure TAggTransViewport.WorldViewportNoUpdate(var X1, Y1, X2, Y2: Double);
begin
  X1 := FWorld.X1;
  Y1 := FWorld.Y1;
  X2 := FWorld.X2;
  Y2 := FWorld.Y2;
end;

procedure TAggTransViewport.WorldViewportNoUpdate(var Rect: TRectDouble);
begin
  Rect.X1 := FWorld.X1;
  Rect.Y1 := FWorld.Y1;
  Rect.X2 := FWorld.X2;
  Rect.Y2 := FWorld.Y2;
end;

procedure TAggTransViewport.WorldViewportActual(var X1, Y1, X2, Y2: Double);
begin
  X1 := FActualWorld.X1;
  Y1 := FActualWorld.Y1;
  X2 := FActualWorld.X2;
  Y2 := FActualWorld.Y2;
end;

procedure TAggTransViewport.InverseTransformScaleOnly(var X, Y: Double);
begin
  X := X / FK.X;
  Y := Y / FK.Y;
end;

function TAggTransViewport.GetDeviceDeltaX: Double;
begin
  Result := FDelta.X - FActualWorld.X1 * FK.X;
end;

function TAggTransViewport.GetDeviceDeltaY: Double;
begin
  Result := FDelta.Y - FActualWorld.Y1 * FK.Y;
end;

function TAggTransViewport.GetScale: Double;
begin
  Result := (FK.X + FK.Y) * 0.5;
end;

procedure TAggTransViewport.ToAffine(Mtx: TAggTransAffine);
var
  M: TAggTransAffineTranslation;
begin
  M := TAggTransAffineTranslation.Create(-FActualWorld.X1, -FActualWorld.Y1);
  try
    M.Scale(FK);
    M.Translate(FDelta);
    Mtx.Assign(M);
  finally
    M.Free;
  end;
end;

procedure TAggTransViewport.ToAffineScaleOnly(Mtx: TAggTransAffine);
begin
  Mtx.Scale(FK)
end;

function TAggTransViewport.GetByteSize: Cardinal;
begin
  Result := SizeOf(Self);
end;

procedure TAggTransViewport.Serialize(Ptr: PInt8u);
begin
  Move(Self, Ptr^, SizeOf(Self));
end;

procedure TAggTransViewport.Deserialize(Ptr: PInt8u);
begin
  Move(Ptr^, Self, SizeOf(Self));
end;

procedure TAggTransViewport.Update;
const
  CEpsilon: Double = 1E-30;
var
  D: Double;
  World: TRectDouble;
  Device: TRectDouble;
begin
  if (Abs(FWorld.X1 - FWorld.X2) < CEpsilon) or
    (Abs(FWorld.Y1 - FWorld.Y2) < CEpsilon) or
    (Abs(FDevice.X1 - FDevice.X2) < CEpsilon) or
    (Abs(FDevice.Y1 - FDevice.Y2) < CEpsilon) then
  begin
    FActualWorld.X1 := FWorld.X1;
    FActualWorld.Y1 := FWorld.Y1;
    FActualWorld.X2 := FWorld.X1 + 1;  // possibly wrong???
    FActualWorld.Y2 := FWorld.Y2 + 1;
    FDelta.X := FDevice.X1;
    FDelta.Y := FDevice.Y1;
    FK.X := 1;
    FK.Y := 1;

    FIsValid := False;
  end
  else
  begin
    World.X1 := FWorld.X1;
    World.Y1 := FWorld.Y1;
    World.X2 := FWorld.X2;
    World.Y2 := FWorld.Y2;
    Device.X1 := FDevice.X1;
    Device.Y1 := FDevice.Y1;
    Device.X2 := FDevice.X2;
    Device.Y2 := FDevice.Y2;

    if not(FAspect = arStretch) then
    begin
      FK.X := (Device.X2 - Device.X1) / (World.X2 - World.X1);
      FK.Y := (Device.Y2 - Device.Y1) / (World.Y2 - World.Y1);

      if (FAspect = arMeet) = (FK.X < FK.Y) then
      begin
        D := (World.Y2 - World.Y1) * FK.Y / FK.X;

        World.Y1 := World.Y1 + ((World.Y2 - World.Y1 - D) * FAlign.Y);
        World.Y2 := World.Y1 + D;
      end
      else
      begin
        D := (World.X2 - World.X1) * FK.X / FK.Y;

        World.X1 := World.X1 + ((World.X2 - World.X1 - D) * FAlign.X);
        World.X2 := World.X1 + D;
      end;
    end;

    FActualWorld.X1 := World.X1;
    FActualWorld.Y1 := World.Y1;
    FActualWorld.X2 := World.X2;
    FActualWorld.Y2 := World.Y2;
    FDelta.X := Device.X1;
    FDelta.Y := Device.Y1;
    FK.X := (Device.X2 - Device.X1) / (World.X2 - World.X1);
    FK.Y := (Device.Y2 - Device.Y1) / (World.Y2 - World.Y1);

    FIsValid := True;
  end;
end;

end.
