unit AggRasterizerOutlineAA;

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
  AggColor,
  AggLineAABasics,
  AggVertexSource,
  AggVertexSequence,
  AggRendererOutlineAA,
  AggControl;

type
  // Vertex (x, y) with the distance to the next one. The last vertex has
  // the distance between the last and the first points
  PAggLineAAVertex = ^TAggLineAAVertex;
  TAggLineAAVertex = record
    X, Y, Len: Integer;
    procedure Initialize(X, Y: Integer);
  end;

  PAggDrawVars = ^TAggDrawVars;
  TAggDrawVars = record
    Idx: Cardinal;
    X1, Y1, X2, Y2: Integer;
    Curr, Next: TAggLineParameters;
    Lcurr, Lnext, Xb1, Yb1, Xb2, Yb2: Integer;
    Flags: Cardinal;
  end;

  TAggRasterizerOutlineAA = class
  private
    FRen: TAggRendererOutline;
    FSourceVertices: TAggVertexSequence;
    FAccurateJoin, FRoundCap: Boolean;
    FStart: TPointInteger;

    function GetAccurateJoin: Boolean;
    function GetRoundCap: Boolean;
    procedure SetAccurateJoin(V: Boolean);
    procedure SetRoundCap(V: Boolean);
  public
    constructor Create(Ren: TAggRendererOutline);
    destructor Destroy; override;

    procedure Renderer(Ren: TAggRendererOutline);

    procedure Draw(Dv: PAggDrawVars; Start, Stop: Cardinal);

    procedure MoveTo(X, Y: Integer);
    procedure LineTo(X, Y: Integer);

    procedure MoveToDouble(X, Y: Double);
    procedure LineToDouble(X, Y: Double);

    procedure Render(ClosePolygon: Boolean);

    procedure AddVertex(X, Y: Double; Cmd: Cardinal);
    procedure AddPath(VertexSource: TAggCustomVertexSource; PathID: Cardinal = 0);

    procedure RenderAllPaths(VertexSource: TAggVertexSource; Colors: PAggColor;
      PathID: PCardinal; PathCount: Cardinal);

    procedure RenderControl(C: TAggCustomAggControl);

    property RoundCap: Boolean read GetRoundCap write SetRoundCap;
    property AccurateJoin: Boolean read GetAccurateJoin write SetAccurateJoin;
  end;

function CompareDistStart(D: Integer): Boolean;
function CompareDistEnd(D: Integer): Boolean;

function LineAAVertexFuncOperator(This, Val: PAggLineAAVertex): Boolean;

implementation


function CompareDistStart;
begin
  Result := D > 0;
end;

function CompareDistEnd;
begin
  Result := D <= 0;
end;


{ TAggLineAAVertex }

procedure TAggLineAAVertex.Initialize(X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
  Len := 0;
end;

function LineAAVertexFuncOperator;
var
  Dx, Dy: Double;

begin
  Dx := Val.X - This.X;
  Dy := Val.Y - This.Y;

  This.Len := Trunc(Sqrt(Dx * Dx + Dy * Dy));

  Result := This.Len > (CAggLineSubpixelSize + CAggLineSubpixelSize div 2);
end;


{ TAggRasterizerOutlineAA }

constructor TAggRasterizerOutlineAA.Create(Ren: TAggRendererOutline);
begin
  FSourceVertices := TAggVertexSequence.Create(SizeOf(TAggLineAAVertex), 6,
    @LineAAVertexFuncOperator);

  FRen := Ren;

  FAccurateJoin := FRen.AccurateJoinOnly;
  FRoundCap := False;

  FStart := PointInteger(0);
end;

destructor TAggRasterizerOutlineAA.Destroy;
begin
  FSourceVertices.Free;
  inherited;
end;

procedure TAggRasterizerOutlineAA.Renderer(Ren: TAggRendererOutline);
begin
  FRen := Ren;
end;

procedure TAggRasterizerOutlineAA.Draw(Dv: PAggDrawVars; Start, Stop: Cardinal);
var
  I: Cardinal;
  V: PAggLineAAVertex;
begin
  I := Start;

  while I < Stop do
  begin
    case Dv.Flags of
      0:
        FRen.Line3(@Dv.Curr, Dv.Xb1, Dv.Yb1, Dv.Xb2, Dv.Yb2);
      1:
        FRen.Line2(@Dv.Curr, Dv.Xb2, Dv.Yb2);
      2:
        FRen.Line1(@Dv.Curr, Dv.Xb1, Dv.Yb1);
      3:
        FRen.Line0(@Dv.Curr);
    end;

    Dv.X1 := Dv.X2;
    Dv.Y1 := Dv.Y2;

    Dv.Lcurr := Dv.Lnext;
    Dv.Lnext := PAggLineAAVertex(FSourceVertices[Dv.Idx]).Len;

    Inc(Dv.Idx);

    if Dv.Idx >= FSourceVertices.Size then
      Dv.Idx := 0;

    V := FSourceVertices[Dv.Idx];

    Dv.X2 := V.X;
    Dv.Y2 := V.Y;

    Dv.Curr := Dv.Next;

    Dv.Next.Initialize(Dv.X1, Dv.Y1, Dv.X2, Dv.Y2, Dv.Lnext);

    Dv.Xb1 := Dv.Xb2;
    Dv.Yb1 := Dv.Yb2;

    if FAccurateJoin then
      Dv.Flags := 0
    else
    begin
      Dv.Flags := Dv.Flags shr 1;

      Dv.Flags := Dv.Flags or
        (Cardinal(Dv.Curr.DiagonalQuadrant = Dv.Next.DiagonalQuadrant) shl 1);
    end;

    if Dv.Flags and 2 = 0 then
      Bisectrix(@Dv.Curr, @Dv.Next, @Dv.Xb2, @Dv.Yb2);

    Inc(I)
  end;
end;

procedure TAggRasterizerOutlineAA.SetAccurateJoin;
begin
  if FRen.AccurateJoinOnly then
    FAccurateJoin := True
  else
    FAccurateJoin := V;
end;

function TAggRasterizerOutlineAA.GetAccurateJoin;
begin
  Result := FAccurateJoin;
end;

procedure TAggRasterizerOutlineAA.SetRoundCap;
begin
  FRoundCap := V;
end;

function TAggRasterizerOutlineAA.GetRoundCap;
begin
  Result := FRoundCap;
end;

procedure TAggRasterizerOutlineAA.MoveTo(X, Y: Integer);
var
  Vt: TAggLineAAVertex;
begin
  FStart := PointInteger(X, Y);

  Vt.Initialize(X, Y);

  FSourceVertices.ModifyLast(@Vt);
end;

procedure TAggRasterizerOutlineAA.LineTo(X, Y: Integer);
var
  Vt: TAggLineAAVertex;
begin
  Vt.Initialize(X, Y);

  FSourceVertices.Add(@Vt);
end;

procedure TAggRasterizerOutlineAA.MoveToDouble(X, Y: Double);
begin
  MoveTo(LineCoord(X), LineCoord(Y));
end;

procedure TAggRasterizerOutlineAA.LineToDouble(X, Y: Double);
begin
  LineTo(LineCoord(X), LineCoord(Y));
end;

procedure TAggRasterizerOutlineAA.Render(ClosePolygon: Boolean);
var
  Dv: TAggDrawVars;
  V : PAggLineAAVertex;
  X1, Y1, X2, Y2, Lprev, X3, Y3, Lnext: Integer;
  Prev, Lp, Lp1, Lp2: TAggLineParameters;
begin
  FSourceVertices.Close(ClosePolygon);

  if ClosePolygon then
    if FSourceVertices.Size >= 3 then
    begin
      Dv.Idx := 2;

      V := FSourceVertices[FSourceVertices.Size - 1];
      X1 := V.X;
      Y1 := V.Y;
      Lprev := V.Len;

      V := FSourceVertices[0];
      X2 := V.X;
      Y2 := V.Y;

      Dv.Lcurr := V.Len;

      Prev.Initialize(X1, Y1, X2, Y2, Lprev);

      V := FSourceVertices[1];
      Dv.X1 := V.X;
      Dv.Y1 := V.Y;

      Dv.Lnext := V.Len;

      Dv.Curr.Initialize(X2, Y2, Dv.X1, Dv.Y1, Dv.Lcurr);

      V := FSourceVertices[Dv.Idx];
      Dv.X2 := V.X;
      Dv.Y2 := V.Y;

      Dv.Next.Initialize(Dv.X1, Dv.Y1, Dv.X2, Dv.Y2, Dv.Lnext);

      Dv.Xb1 := 0;
      Dv.Yb1 := 0;
      Dv.Xb2 := 0;
      Dv.Yb2 := 0;

      if FAccurateJoin then
        Dv.Flags := 0
      else
        Dv.Flags := Cardinal(Prev.DiagonalQuadrant = Dv.Curr.DiagonalQuadrant)
          or (Cardinal(Dv.Curr.DiagonalQuadrant = Dv.Next.DiagonalQuadrant) shl 1);

      if Dv.Flags and 1 = 0 then
        Bisectrix(@Prev, @Dv.Curr, @Dv.Xb1, @Dv.Yb1);

      if Dv.Flags and 2 = 0 then
        Bisectrix(@Dv.Curr, @Dv.Next, @Dv.Xb2, @Dv.Yb2);

      Draw(@Dv, 0, FSourceVertices.Size);
    end
    else
  else
    case FSourceVertices.Size of
      2:
        begin
          V := FSourceVertices[0];
          X1 := V.X;
          Y1 := V.Y;
          Lprev := V.Len;
          V := FSourceVertices[1];
          X2 := V.X;
          Y2 := V.Y;

          Lp.Initialize(X1, Y1, X2, Y2, Lprev);

          if FRoundCap then
            FRen.Semidot(@CompareDistStart, X1, Y1, X1 + (Y2 - Y1),
              Y1 - (X2 - X1));

          FRen.Line3(@Lp, X1 + (Y2 - Y1), Y1 - (X2 - X1), X2 + (Y2 - Y1),
            Y2 - (X2 - X1));

          if FRoundCap then
            FRen.Semidot(@CompareDistEnd, X2, Y2, X2 + (Y2 - Y1),
              Y2 - (X2 - X1));
        end;

      3:
        begin
          V := FSourceVertices[0];
          X1 := V.X;
          Y1 := V.Y;
          Lprev := V.Len;
          V := FSourceVertices[1];
          X2 := V.X;
          Y2 := V.Y;
          Lnext := V.Len;
          V := FSourceVertices[2];
          X3 := V.X;
          Y3 := V.Y;

          Lp1.Initialize(X1, Y1, X2, Y2, Lprev);
          Lp2.Initialize(X2, Y2, X3, Y3, Lnext);

          Bisectrix(@Lp1, @Lp2, @Dv.Xb1, @Dv.Yb1);

          if FRoundCap then
            FRen.Semidot(@CompareDistStart, X1, Y1, X1 + (Y2 - Y1),
              Y1 - (X2 - X1));

          FRen.Line3(@Lp1, X1 + (Y2 - Y1), Y1 - (X2 - X1), Dv.Xb1, Dv.Yb1);

          FRen.Line3(@Lp2, Dv.Xb1, Dv.Yb1, X3 + (Y3 - Y2), Y3 - (X3 - X2));

          if FRoundCap then
            FRen.Semidot(@CompareDistEnd, X3, Y3, X3 + (Y3 - Y2),
              Y3 - (X3 - X2));
        end;

      0, 1:
      else
      begin
        Dv.Idx := 3;

        V := FSourceVertices[0];
        X1 := V.X;
        Y1 := V.Y;
        Lprev := V.Len;

        V := FSourceVertices[1];
        X2 := V.X;
        Y2 := V.Y;

        Dv.Lcurr := V.Len;

        Prev.Initialize(X1, Y1, X2, Y2, Lprev);

        V := FSourceVertices[2];
        Dv.X1 := V.X;
        Dv.Y1 := V.Y;

        Dv.Lnext := V.Len;

        Dv.Curr.Initialize(X2, Y2, Dv.X1, Dv.Y1, Dv.Lcurr);

        V := FSourceVertices[Dv.Idx];
        Dv.X2 := V.X;
        Dv.Y2 := V.Y;

        Dv.Next.Initialize(Dv.X1, Dv.Y1, Dv.X2, Dv.Y2, Dv.Lnext);

        Dv.Xb1 := 0;
        Dv.Yb1 := 0;
        Dv.Xb2 := 0;
        Dv.Yb2 := 0;

        if FAccurateJoin then
          Dv.Flags := 0
        else
          Dv.Flags :=
            Cardinal(Prev.DiagonalQuadrant = Dv.Curr.DiagonalQuadrant) or
            (Cardinal(Dv.Curr.DiagonalQuadrant = Dv.Next.
            DiagonalQuadrant) shl 1);

        if Dv.Flags and 1 = 0 then
        begin
          Bisectrix(@Prev, @Dv.Curr, @Dv.Xb1, @Dv.Yb1);
          FRen.Line3(@Prev, X1 + (Y2 - Y1), Y1 - (X2 - X1), Dv.Xb1, Dv.Yb1);

        end
        else
          FRen.Line1(@Prev, X1 + (Y2 - Y1), Y1 - (X2 - X1));

        if FRoundCap then
          FRen.Semidot(@CompareDistStart, X1, Y1, X1 + (Y2 - Y1),
            Y1 - (X2 - X1));

        if Dv.Flags and 2 = 0 then
          Bisectrix(@Dv.Curr, @Dv.Next, @Dv.Xb2, @Dv.Yb2);

        Draw(@Dv, 1, FSourceVertices.Size - 2);

        if Dv.Flags and 1 = 0 then
          FRen.Line3(@Dv.Curr, Dv.Xb1, Dv.Yb1,
            Dv.Curr.X2 + (Dv.Curr.Y2 - Dv.Curr.Y1),
            Dv.Curr.Y2 - (Dv.Curr.X2 - Dv.Curr.X1))
        else
          FRen.Line2(@Dv.Curr, Dv.Curr.X2 + (Dv.Curr.Y2 - Dv.Curr.Y1),
            Dv.Curr.Y2 - (Dv.Curr.X2 - Dv.Curr.X1));

        if FRoundCap then
          FRen.Semidot(@CompareDistEnd, Dv.Curr.X2, Dv.Curr.Y2,
            Dv.Curr.X2 + (Dv.Curr.Y2 - Dv.Curr.Y1),
            Dv.Curr.Y2 - (Dv.Curr.X2 - Dv.Curr.X1));
      end;
    end;

  FSourceVertices.RemoveAll;
end;

procedure TAggRasterizerOutlineAA.AddVertex;
begin
  if IsMoveTo(Cmd) then
  begin
    Render(False);
    MoveToDouble(X, Y);
  end
  else if IsEndPoly(Cmd) then
  begin
    Render(IsClosed(Cmd));

    if IsClosed(Cmd) then
      MoveTo(FStart.X, FStart.Y);
  end
  else
    LineToDouble(X, Y);
end;

procedure TAggRasterizerOutlineAA.AddPath(VertexSource: TAggCustomVertexSource; PathID: Cardinal = 0);
var
  X, Y: Double;
  Cmd : Cardinal;
begin
  VertexSource.Rewind(PathID);

  Cmd := VertexSource.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    AddVertex(X, Y, Cmd);

    Cmd := VertexSource.Vertex(@X, @Y);
  end;

  Render(False);
end;

procedure TAggRasterizerOutlineAA.RenderAllPaths;
var
  I: Cardinal;
begin
  for I := 0 to PathCount - 1 do
  begin
    FRen.SetColor(PAggColor(PtrComp(Colors) + I * SizeOf(TAggColor)));
    AddPath(VertexSource, PCardinal(PtrComp(PathID) + I * SizeOf(Cardinal))^);
  end;
end;

procedure TAggRasterizerOutlineAA.RenderControl;
var
  I: Cardinal;
begin
  for I := 0 to C.PathCount - 1 do
  begin
    FRen.SetColor(C.ColorPointer[I]);
    AddPath(C, I);
  end;
end;

end.
