unit AggConvGPC;

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
//  General Polygon Clipper based on the GPC library by Alan Murta            //
//  Union, Intersection, XOR, A-B, B-A                                        //
//  Contact the author if you intend to use it in commercial applications!    //
//  http://www.cs.man.ac.uk/aig/staff/alan/software/                          //
//  Alan Murta (email: gpc@cs.man.ac.uk)                                      //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I AggCompiler.inc}

uses
  Gpc,
  AggBasics,
  AggArray,
  AggVertexSource;

type
  TAggGpcOp = (goOr, goAnd, goXor, goAMinusB, goBMinusA);

  TAggInternalStatus = (siMoveTo, siLineTo, siStop);

  PAggGpcVertex = ^TGpcVertex;

  PAggContourHeader = ^TAggContourHeader;
  TAggContourHeader = record
    NumVertices, HoleFlag: Integer;
    Vertices: PAggGpcVertex;
  end;

  TAggConvGpc = class(TAggVertexSource)
  private
    FSourceA, FSourceB: TAggVertexSource;

    FStatus: TAggInternalStatus;
    FVertex, FContour: Integer;
    FOperation: TAggGpcOp;

    FVertexAccumulator, FContourAccumulator: TAggPodDeque;

    FPolygonA, FPolygonB, FResult: TGpcPolygon;

    // Private
    procedure FreePolygon(P: PGpcPolygon);
    procedure FreeResult;
    procedure FreeGpcData;
    procedure StartContour;
    procedure SetAddVertex(X, Y: Double);
    procedure EndContour(Orientation: Cardinal);
    procedure MakePolygon(P: PGpcPolygon);
    procedure StartExtracting;

    function NextContour: Boolean;
    function NextVertex(X, Y: PDouble): Boolean;

    procedure Add(Src: TAggVertexSource; P: PGpcPolygon);
  public
    constructor Create(A, B: TAggVertexSource; Op: TAggGpcOp = goOr);
    destructor Destroy; override;

    procedure SetSource1(Source: TAggVertexSource);
    procedure SetSource2(Source: TAggVertexSource);

    procedure Operation(V: TAggGpcOp);

    // Vertex Source Interface
    procedure Rewind(PathID: Cardinal); override;
    function Vertex(X, Y: PDouble): Cardinal; override;
  end;

implementation


{ TAggConvGpc }

constructor TAggConvGpc.Create;
begin
  FVertexAccumulator := TAggPodDeque.Create(SizeOf(TGpcVertex), 8);
  FContourAccumulator := TAggPodDeque.Create(SizeOf(TAggContourHeader), 6);

  FSourceA := A;
  FSourceB := B;

  FStatus := siMoveTo;
  FVertex := -1;
  FContour := -1;
  FOperation := Op;

  FillChar(FPolygonA, SizeOf(FPolygonA), 0);
  FillChar(FPolygonB, SizeOf(FPolygonB), 0);
  FillChar(FResult, SizeOf(FResult), 0);
end;

destructor TAggConvGpc.Destroy;
begin
  FreeGpcData;

  FVertexAccumulator.Free;
  FContourAccumulator.Free;

  inherited;
end;

procedure TAggConvGpc.SetSource1;
begin
  FSourceA := Source;
end;

procedure TAggConvGpc.SetSource2;
begin
  FSourceB := Source;
end;

procedure TAggConvGpc.Operation;
begin
  FOperation := V;
end;

procedure TAggConvGpc.Rewind(PathID: Cardinal);
begin
  FreeResult;

  FSourceA.Rewind(PathID);
  FSourceB.Rewind(PathID);

  Add(FSourceA, @FPolygonA);
  Add(FSourceB, @FPolygonB);

  case FOperation of
    goOr:
      GpcPolygonClip(Gpc.goUnion, @FPolygonA, @FPolygonB, @FResult);

    goAnd:
      GpcPolygonClip(Gpc.goInt, @FPolygonA, @FPolygonB, @FResult);

    goXor:
      GpcPolygonClip(Gpc.goXor, @FPolygonA, @FPolygonB, @FResult);

    goAMinusB:
      GpcPolygonClip(Gpc.goDiff, @FPolygonA, @FPolygonB, @FResult);

    goBMinusA:
      GpcPolygonClip(Gpc.goDiff, @FPolygonB, @FPolygonA, @FResult);
  end;

  StartExtracting;
end;

function TAggConvGpc.Vertex(X, Y: PDouble): Cardinal;
begin
  if FStatus = siMoveTo then
    if NextContour then
    begin
      if NextVertex(X, Y) then
      begin
        FStatus := siLineTo;
        Result := CAggPathCmdMoveTo;

        Exit;
      end;

      FStatus := siStop;
      Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

      Exit;

    end
    else
  else
  begin
    if NextVertex(X, Y) then
    begin
      Result := CAggPathCmdLineTo;

      Exit;

    end
    else
      FStatus := siMoveTo;

    Result := CAggPathCmdEndPoly or CAggPathFlagsClose;

    Exit;
  end;

  Result := CAggPathCmdStop;
end;

procedure TAggConvGpc.FreePolygon;
var
  I: Integer;

begin
  I := 0;

  while I < P.NumContours do
  begin
    AggFreeMem(Pointer(P.Contour[I].Vertex), P.Contour[I].NumVertices *
      SizeOf(TGpcVertex));

    Inc(I);
  end;

  // AggFreeMem(pointer(p.hole ) ,? );
  AggFreeMem(Pointer(P.Contour), P.NumContours * SizeOf(TGpcVertexList));

  FillChar(P^, SizeOf(TGpcPolygon), 0);
end;

procedure TAggConvGpc.FreeResult;
begin
  if FResult.Contour <> nil then
    GpcFreePolygon(@FResult);

  FillChar(FResult, SizeOf(FResult), 0);
end;

procedure TAggConvGpc.FreeGpcData;
begin
  FreePolygon(@FPolygonA);
  FreePolygon(@FPolygonB);
  FreeResult;
end;

procedure TAggConvGpc.StartContour;
var
  H: TAggContourHeader;
begin
  FillChar(H, SizeOf(H), 0);
  FContourAccumulator.Add(@H);
  FVertexAccumulator.RemoveAll;
end;

procedure TAggConvGpc.SetAddVertex;
var
  V: TGpcVertex;
begin
  V.X := X;
  V.Y := Y;

  FVertexAccumulator.Add(@V);
end;

procedure TAggConvGpc.EndContour;
var
  H   : PAggContourHeader;
  D, S: PAggGpcVertex;
  I   : Integer;

begin
  if FContourAccumulator.Size <> 0 then
    if FVertexAccumulator.Size > 2 then
    begin
      H := FContourAccumulator[FContourAccumulator.Size - 1];

      H.NumVertices := FVertexAccumulator.Size;
      H.HoleFlag := 0;

      // TO DO: Clarify the "holes"
      // if IsClockwise(orientation ) then h.HoleFlag:=1;

      AggGetMem(Pointer(H.Vertices), H.NumVertices * SizeOf(TGpcVertex));

      D := H.Vertices;

      for I := 0 to H.NumVertices - 1 do
      begin
        S := FVertexAccumulator[I];

        D.X := S.X;
        D.Y := S.Y;

        Inc(PtrComp(D), SizeOf(TGpcVertex));
      end;

    end
    else
      FVertexAccumulator.RemoveLast;
end;

procedure TAggConvGpc.MakePolygon;
var
  I: Integer;
  H: PAggContourHeader;

  // ph : PInteger;
  Pv: PGpcVertexList;
begin
  FreePolygon(P);

  if FContourAccumulator.Size <> 0 then
  begin
    P.NumContours := FContourAccumulator.Size;

    // TO DO: Clarify the "holes"
    // p.hole = new goInt[p.NumContours];

    P.Hole := nil;

    AggGetMem(Pointer(P.Contour), P.NumContours * SizeOf(TGpcVertexList));

    // ph:=p.hole;
    Pv := PGpcVertexList(P.Contour);

    if P.NumContours > 0 then
      for I := 0 to P.NumContours - 1 do
      begin
        H := FContourAccumulator[I];

        // *ph++ = h.HoleFlag;
        Pv.NumVertices := H.NumVertices;
        Pv.Vertex := PGpcVertexArray(H.Vertices);

        Inc(PtrComp(Pv), SizeOf(TGpcVertexList));
      end;
  end;
end;

procedure TAggConvGpc.StartExtracting;
begin
  FStatus := siMoveTo;
  FContour := -1;
  FVertex := -1;
end;

function TAggConvGpc.NextContour;
begin
  Inc(FContour);

  if FContour < FResult.NumContours then
  begin
    FVertex := -1;

    Result := True;

  end
  else
    Result := False;
end;

function TAggConvGpc.NextVertex;
var
  Vlist: PGpcVertexList;
  V: PAggGpcVertex;
begin
  Vlist := @FResult.Contour[FContour];

  Inc(FVertex);

  if FVertex < Vlist.NumVertices then
  begin
    V := @Vlist.Vertex[FVertex];

    X^ := V.X;
    Y^ := V.Y;

    Result := True;

  end
  else
    Result := False;
end;

procedure TAggConvGpc.Add;
var
  Cmd, Orientation: Cardinal;
  X, Y, StartX, StartY: Double;
  LineTo: Boolean;
begin
  StartX := 0.0;
  StartY := 0.0;
  LineTo := False;

  Orientation := 0;

  FContourAccumulator.RemoveAll;

  Cmd := Src.Vertex(@X, @Y);

  while not IsStop(Cmd) do
  begin
    if IsVertex(Cmd) then
    begin
      if IsMoveTo(Cmd) then
      begin
        if LineTo then
        begin
          EndContour(Orientation);

          Orientation := 0;
        end;

        StartContour;

        StartX := X;
        StartY := Y;
      end;

      SetAddVertex(X, Y);

      LineTo := True;

    end
    else if IsEndPoly(Cmd) then
    begin
      Orientation := GetOrientation(Cmd);

      if LineTo and IsClosed(Cmd) then
        SetAddVertex(StartX, StartY);
    end;

    Cmd := Src.Vertex(@X, @Y);
  end;

  if LineTo then
    EndContour(Orientation);

  MakePolygon(P);
end;

end.
