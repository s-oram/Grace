unit AggPathStorageInteger;

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
  AggArray,
  AggVertexSource;

const
  CAggCmdMoveTo = 0;
  CAggCmdLineTo = 1;
  CAggCmdCurve3 = 2;
  CAggCmdCurve4 = 3;

  CAggCoordShift = 6;
  CAggCoordScale = 1 shl CAggCoordShift;

type
  PAggVertexInt16 = ^TAggVertexInt16;
  TAggVertexInt16 = record
  private
    X, Y: Int16;
  public
    procedure Initialize(X, Y: Int16; Flag: Cardinal);

    function Vertex(X, Y: PDouble; Dx: Double = 0; Dy: Double = 0;
      Scale: Double = 1.0): Cardinal;
  end;

  PAggVertexInt32 = ^TAggVertexInt32;
  TAggVertexInt32 = record
  private
    X, Y: Int32;
  public
    procedure Initialize(X, Y: Int32; Flag: Cardinal);

    function Vertex(X, Y: PDouble; Dx: Double = 0; Dy: Double = 0;
      Scale: Double = 1.0): Cardinal;
  end;

  TAggCustomPathStorageInteger = class(TAggVertexSource)
  public
    procedure MoveTo(X, Y: Int32); virtual; abstract;
    procedure LineTo(X, Y: Int32); virtual; abstract;
    procedure Curve3To(ControlX, ControlY, ToX, ToY: Int32); virtual; abstract;
    procedure Curve4To(Control1X, Control1Y, Control2X, Control2Y, ToX, ToY: Int32);
      virtual; abstract;

    function SetVertex(Index: Cardinal; X, Y: PInt32): Cardinal;
      virtual; abstract;

    procedure ClosePolygon; virtual; abstract;
  end;

  TAggPathStorageInt16 = class(TAggCustomPathStorageInteger)
  private
    FStorage: TAggPodDeque;
    FVertexIndex: Cardinal;
    FClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveAll; override;

    procedure MoveTo(X, Y: Int32); override;
    procedure LineTo(X, Y: Int32); override;
    procedure Curve3To(ControlX, ControlY, ToX, ToY: Int32); override;
    procedure Curve4To(Control1X, Control1Y, Control2X, Control2Y, ToX,
      ToY: Int32); override;

    procedure ClosePolygon; override;

    function Size: Cardinal;
    function SetVertex(Index: Cardinal; X, Y: PInt32): Cardinal; override;

    function ByteSize: Cardinal;
    procedure Serialize(Ptr: PInt8u);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    function GetBoundingRect: TRectDouble;
  end;

  TAggPathStorageInt32 = class(TAggCustomPathStorageInteger)
  private
    FStorage: TAggPodDeque;
    FVertexIndex: Cardinal;
    FClosed: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RemoveAll; override;

    procedure MoveTo(X, Y: Int32); override;
    procedure LineTo(X, Y: Int32); override;
    procedure Curve3To(ControlX, ControlY, ToX, ToY: Int32); override;
    procedure Curve4To(Control1X, Control1Y, Control2X, Control2Y, ToX,
      ToY: Int32); override;

    procedure ClosePolygon; override;

    function Size: Cardinal;
    function SetVertex(Index: Cardinal; X, Y: PInt32): Cardinal; override;

    function ByteSize: Cardinal;
    procedure Serialize(Ptr: PInt8u);

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;

    function GetBoundingRect: TRectDouble;
  end;

  TAggCustomSerializedIntegerPathAdaptor = class(TAggVertexSource)
  public
    procedure Init(Data: PInt8u; Size: Cardinal; Dx, Dy: Double;
      Scale: Double = 1.0); virtual; abstract;
  end;

  TAggSerializedInt16PathAdaptor = class(TAggCustomSerializedIntegerPathAdaptor)
  private
    FData, FEnd: PInt8u;
    FInternalData: PInt8u;
    FDelta: TPointDouble;
    FScale: Double;
    FVertices: Cardinal;
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; Size: Cardinal;
      Dx, Dy: Double); overload;

    procedure Init(Data: PInt8u; Size: Cardinal; Dx, Dy: Double;
      Scale: Double = 1.0); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

  TAggSerializedInt32PathAdaptor = class(TAggCustomSerializedIntegerPathAdaptor)
  private
    FData, FEnd: PInt8u;
    FInternalData: PInt8u;
    FDelta: TPointDouble;
    FScale: Double;
    FVertices: Cardinal;
  protected
    function GetPathCount: Cardinal; override;
  public
    constructor Create; overload;
    constructor Create(Data: PInt8u; Size: Cardinal;
      Dx, Dy: Double); overload;

    procedure Init(Data: PInt8u; Size: Cardinal; Dx, Dy: Double;
      Scale: Double = 1.0); override;

    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

implementation


{ TAggVertexInt16 }

procedure TAggVertexInt16.Initialize(X, Y: Int16; Flag: Cardinal);
begin
  Self.X := ((X shl 1) and not 1) or (Flag and 1);
  Self.Y := ((Y shl 1) and not 1) or (Flag shr 1);
end;

function TAggVertexInt16.Vertex(X, Y: PDouble; Dx: Double = 0; Dy: Double = 0;
      Scale: Double = 1.0): Cardinal;
begin
  X^ := Dx + (ShrInt16(Self.X, 1) / CAggCoordScale) * Scale;
  Y^ := Dy + (ShrInt16(Self.Y, 1) / CAggCoordScale) * Scale;

  case ((Self.Y and 1) shl 1) or (Self.X and 1) of
    CAggCmdMoveTo:
      Result := CAggPathCmdMoveTo;

    CAggCmdLineTo:
      Result := CAggPathCmdLineTo;

    CAggCmdCurve3:
      Result := CAggPathCmdCurve3;

    CAggCmdCurve4:
      Result := CAggPathCmdCurve4;

  else
    Result := CAggPathCmdStop;
  end;
end;


{ TAggVertexInt32 }

procedure TAggVertexInt32.Initialize(X, Y: Int32; Flag: Cardinal);
begin
  Self.X := ((X shl 1) and not 1) or (Flag and 1);
  Self.Y := ((Y shl 1) and not 1) or (Flag shr 1);
end;

function TAggVertexInt32.Vertex(X, Y: PDouble; Dx: Double = 0; Dy: Double = 0;
      Scale: Double = 1.0): Cardinal;
begin
  X^ := Dx + (ShrInt32(Self.X, 1) / CAggCoordScale) * Scale;
  Y^ := Dy + (ShrInt32(Self.Y, 1) / CAggCoordScale) * Scale;

  case ((Self.Y and 1) shl 1) or (Self.X and 1) of
    CAggCmdMoveTo:
      Result := CAggPathCmdMoveTo;

    CAggCmdLineTo:
      Result := CAggPathCmdLineTo;

    CAggCmdCurve3:
      Result := CAggPathCmdCurve3;

    CAggCmdCurve4:
      Result := CAggPathCmdCurve4;

  else
    Result := CAggPathCmdStop;
  end;
end;


{ TAggPathStorageInt16 }

constructor TAggPathStorageInt16.Create;
begin
  FStorage := TAggPodDeque.Create(SizeOf(TAggVertexInt16));

  FVertexIndex := 0;
  FClosed := True;
end;

destructor TAggPathStorageInt16.Destroy;
begin
  FStorage.Free;
  inherited
end;

procedure TAggPathStorageInt16.RemoveAll;
begin
  FStorage.RemoveAll;
end;

procedure TAggPathStorageInt16.MoveTo;
var
  V: TAggVertexInt16;
begin
  V.Initialize(Int16(X), Int16(Y), CAggCmdMoveTo);
  FStorage.Add(@V);
end;

procedure TAggPathStorageInt16.LineTo(X, Y: Int32);
var
  V: TAggVertexInt16;
begin
  V.Initialize(Int16(X), Int16(Y), CAggCmdLineTo);
  FStorage.Add(@V);
end;

procedure TAggPathStorageInt16.Curve3To(ControlX, ControlY, ToX, ToY: Int32);
var
  V: TAggVertexInt16;
begin
  V.Initialize(Int16(ControlX), Int16(ControlY), CAggCmdCurve3);
  FStorage.Add(@V);

  V.Initialize(Int16(ToX), Int16(ToY), CAggCmdCurve3);
  FStorage.Add(@V);
end;

procedure TAggPathStorageInt16.Curve4To(Control1X, Control1Y, Control2X, Control2Y, ToX,
  ToY: Int32);
var
  V: TAggVertexInt16;
begin
  V.Initialize(Int16(Control1X), Int16(Control1Y), CAggCmdCurve4);
  FStorage.Add(@V);

  V.Initialize(Int16(Control2X), Int16(Control2Y), Int16(CAggCmdCurve4));
  FStorage.Add(@V);

  V.Initialize(Int16(ToX), Int16(ToY), CAggCmdCurve4);
  FStorage.Add(@V);
end;

procedure TAggPathStorageInt16.ClosePolygon;
begin
end;

function TAggPathStorageInt16.Size;
begin
  Result := FStorage.Size;
end;

function TAggPathStorageInt16.SetVertex;
var
  V: PAggVertexInt16;
begin
  V := FStorage[Index];

  PInt16(X)^ := ShrInt16(V.X, 1);
  PInt16(Y)^ := ShrInt16(V.Y, 1);

  Result := ((V.Y and 1) shl 1) or (V.X and 1);
end;

function TAggPathStorageInt16.ByteSize;
begin
  Result := FStorage.Size * SizeOf(TAggVertexInt16);
end;

procedure TAggPathStorageInt16.Serialize;
var
  I: Cardinal;
begin
  I := 0;

  while I < FStorage.Size do
  begin
    Move(FStorage[I]^, Ptr^, SizeOf(TAggVertexInt16));

    Inc(PtrComp(Ptr), SizeOf(TAggVertexInt16));
    Inc(I);
  end;
end;

procedure TAggPathStorageInt16.Rewind(PathID: Cardinal);
begin
  FVertexIndex := 0;
  FClosed := True;
end;

function TAggPathStorageInt16.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  if (FStorage.Size < 2) or (FVertexIndex > FStorage.Size) then
  begin
    X^ := 0;
    Y^ := 0;

    Result := CAggPathCmdStop;

    Exit;
  end;

  if FVertexIndex = FStorage.Size then
  begin
    X^ := 0;
    Y^ := 0;

    Inc(FVertexIndex);

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

    Exit;
  end;

  Cmd := PAggVertexInt16(FStorage[FVertexIndex]).Vertex(X, Y);

  if IsMoveTo(Cmd) and not FClosed then
  begin
    X^ := 0;
    Y^ := 0;

    FClosed := True;

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

    Exit;
  end;

  FClosed := False;

  Inc(FVertexIndex);

  Result := Cmd;
end;

function TAggPathStorageInt16.GetBoundingRect: TRectDouble;
var
  I: Cardinal;
  X, Y: Double;
begin
  Result := RectDouble(1E100, 1E100, -1E100, -1E100);

  if FStorage.Size = 0 then
    Result := RectDouble(0.0, 0.0, 0.0, 0.0)
  else
    for I := 0 to FStorage.Size - 1 do
    begin
      PAggVertexInt16(FStorage[I]).Vertex(@X, @Y);

      if X < Result.X1 then
        Result.X1 := X;

      if Y < Result.Y1 then
        Result.Y1 := Y;

      if X > Result.X2 then
        Result.X2 := X;

      if Y > Result.Y2 then
        Result.Y2 := Y;
    end;
end;


{ TAggPathStorageInt32 }

constructor TAggPathStorageInt32.Create;
begin
  FStorage := TAggPodDeque.Create(SizeOf(TAggVertexInt32));

  FVertexIndex := 0;
  FClosed := True;
end;

destructor TAggPathStorageInt32.Destroy;
begin
  FStorage.Free;
  inherited;
end;

procedure TAggPathStorageInt32.RemoveAll;
begin
  FStorage.RemoveAll;
end;

procedure TAggPathStorageInt32.MoveTo;
var
  V: TAggVertexInt32;
begin
  V.Initialize(X, Y, CAggCmdMoveTo);
  FStorage.Add(@V);
end;

procedure TAggPathStorageInt32.LineTo;
var
  V: TAggVertexInt32;
begin
  V.Initialize(X, Y, CAggCmdLineTo);
  FStorage.Add(@V);
end;

procedure TAggPathStorageInt32.Curve3To;
var
  V: TAggVertexInt32;
begin
  V.Initialize(ControlX, ControlY, CAggCmdCurve3);
  FStorage.Add(@V);

  V.Initialize(ToX, ToY, CAggCmdCurve3);
  FStorage.Add(@V);
end;

procedure TAggPathStorageInt32.Curve4To;
var
  V: TAggVertexInt32;
begin
  V.Initialize(Control1X, Control1Y, CAggCmdCurve4);
  FStorage.Add(@V);

  V.Initialize(Control2X, Control2Y, CAggCmdCurve4);
  FStorage.Add(@V);

  V.Initialize(ToX, ToY, CAggCmdCurve4);
  FStorage.Add(@V);
end;

procedure TAggPathStorageInt32.ClosePolygon;
begin
end;

function TAggPathStorageInt32.Size;
begin
  Result := FStorage.Size;
end;

function TAggPathStorageInt32.SetVertex;
var
  V: PAggVertexInt32;
begin
  V := FStorage[Index];

  X^ := ShrInt32(V.X, 1);
  Y^ := ShrInt32(V.Y, 1);

  Result := ((V.Y and 1) shl 1) or (V.X and 1);
end;

function TAggPathStorageInt32.ByteSize;
begin
  Result := FStorage.Size * SizeOf(TAggVertexInt32);
end;

procedure TAggPathStorageInt32.Serialize;
var
  I: Cardinal;
begin
  I := 0;

  while I < FStorage.Size do
  begin
    Move(FStorage[I]^, Ptr^, SizeOf(TAggVertexInt32));

    Inc(PtrComp(Ptr), SizeOf(TAggVertexInt32));
    Inc(I);
  end;
end;

procedure TAggPathStorageInt32.Rewind(PathID: Cardinal);
begin
  FVertexIndex := 0;
  FClosed := True;
end;

function TAggPathStorageInt32.Vertex(X, Y: PDouble): Cardinal;
var
  Cmd: Cardinal;
begin
  if (FStorage.Size < 2) or (FVertexIndex > FStorage.Size) then
  begin
    X^ := 0;
    Y^ := 0;

    Result := CAggPathCmdStop;

    Exit;
  end;

  if FVertexIndex = FStorage.Size then
  begin
    X^ := 0;
    Y^ := 0;

    Inc(FVertexIndex);

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

    Exit;
  end;

  Cmd := PAggVertexInt32(FStorage[FVertexIndex]).Vertex(X, Y);

  if IsMoveTo(Cmd) and not FClosed then
  begin
    X^ := 0;
    Y^ := 0;

    FClosed := True;

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

    Exit;
  end;

  FClosed := False;

  Inc(FVertexIndex);

  Result := Cmd;
end;

function TAggPathStorageInt32.GetBoundingRect;
var
  I: Cardinal;
  X, Y: Double;
begin
  Result := RectDouble(1E100, 1E100, -1E100, -1E100);

  if FStorage.Size = 0 then
    Result := RectDouble(0.0, 0.0, 0.0, 0.0)
  else
    for I := 0 to FStorage.Size - 1 do
    begin
      PAggVertexInt32(FStorage[I]).Vertex(@X, @Y);

      if X < Result.X1 then
        Result.X1 := X;

      if Y < Result.Y1 then
        Result.Y1 := Y;

      if X > Result.X2 then
        Result.X2 := X;

      if Y > Result.Y2 then
        Result.Y2 := Y;
    end;
end;


{ TAggSerializedInt16PathAdaptor }

constructor TAggSerializedInt16PathAdaptor.Create;
begin
  FData := nil;
  FEnd := nil;
  FInternalData := nil;

  FDelta.X := 0.0;
  FDelta.Y := 0.0;

  FScale := 1.0;
  FVertices := 0;
end;

constructor TAggSerializedInt16PathAdaptor.Create(Data: PInt8u;
  Size: Cardinal; Dx, Dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.X := Dx;
  FDelta.Y := Dy;

  FScale := 0.0;
  FVertices := 0;
end;

procedure TAggSerializedInt16PathAdaptor.Init;
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.X := Dx;
  FDelta.Y := Dy;

  FScale := Scale;
  FVertices := 0;
end;

function TAggSerializedInt16PathAdaptor.GetPathCount: Cardinal;
begin
  Result := FVertices;
end;

procedure TAggSerializedInt16PathAdaptor.Rewind(PathID: Cardinal);
begin
  FInternalData := FData;

  FVertices := 0;
end;

function TAggSerializedInt16PathAdaptor.Vertex(X, Y: PDouble): Cardinal;
var
  V: TAggVertexInt16;
  Cmd: Cardinal;
begin
  if (FData = nil) or (PtrComp(FInternalData) > PtrComp(FEnd)) then
  begin
    X^ := 0;
    Y^ := 0;

    Result := CAggPathCmdStop;

    Exit;
  end;

  if PtrComp(FInternalData) = PtrComp(FEnd) then
  begin
    X^ := 0;
    Y^ := 0;

    Inc(PtrComp(FInternalData), SizeOf(TAggVertexInt16));

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

    Exit;
  end;

  Move(FInternalData^, V, SizeOf(TAggVertexInt16));

  Cmd := V.Vertex(X, Y, FDelta.X, FDelta.Y, FScale);

  if IsMoveTo(Cmd) and (FVertices > 2) then
  begin
    X^ := 0;
    Y^ := 0;

    FVertices := 0;

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

    Exit;
  end;

  Inc(FVertices);
  Inc(PtrComp(FInternalData), SizeOf(TAggVertexInt16));

  Result := Cmd;
end;


{ TAggSerializedInt32PathAdaptor }

constructor TAggSerializedInt32PathAdaptor.Create;
begin
  FData := nil;
  FEnd := nil;
  FInternalData := nil;

  FDelta.X := 0.0;
  FDelta.Y := 0.0;

  FScale := 1.0;
  FVertices := 0;
end;

constructor TAggSerializedInt32PathAdaptor.Create(Data: PInt8u;
  Size: Cardinal; Dx, Dy: Double);
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.X := Dx;
  FDelta.Y := Dy;

  FScale := 0.0;
  FVertices := 0;
end;

procedure TAggSerializedInt32PathAdaptor.Init;
begin
  FData := Data;
  FEnd := PInt8u(PtrComp(Data) + Size);
  FInternalData := Data;

  FDelta.X := Dx;
  FDelta.Y := Dy;

  FScale := Scale;
  FVertices := 0;
end;

function TAggSerializedInt32PathAdaptor.GetPathCount: Cardinal;
begin
  Result := FVertices;
end;

procedure TAggSerializedInt32PathAdaptor.Rewind(PathID: Cardinal);
begin
  FInternalData := FData;

  FVertices := 0;
end;

function TAggSerializedInt32PathAdaptor.Vertex(X, Y: PDouble): Cardinal;
var
  V: TAggVertexInt32;
  Cmd: Cardinal;
begin
  if (FData = nil) or (PtrComp(FInternalData) > PtrComp(FEnd)) then
  begin
    X^ := 0;
    Y^ := 0;

    Result := CAggPathCmdStop;
    Exit;
  end;

  if PtrComp(FInternalData) = PtrComp(FEnd) then
  begin
    X^ := 0;
    Y^ := 0;

    Inc(PtrComp(FInternalData), SizeOf(TAggVertexInt32));

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;
    Exit;
  end;

  Move(FInternalData^, V, SizeOf(TAggVertexInt32));

  Cmd := V.Vertex(X, Y, FDelta.X, FDelta.Y, FScale);

  if IsMoveTo(Cmd) and (FVertices > 2) then
  begin
    X^ := 0;
    Y^ := 0;

    FVertices := 0;

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;
    Exit;
  end;

  Inc(FVertices);
  Inc(PtrComp(FInternalData), SizeOf(TAggVertexInt32));

  Result := Cmd;
end;

end.
