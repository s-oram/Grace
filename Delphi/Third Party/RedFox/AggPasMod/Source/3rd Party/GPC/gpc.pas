unit GPC;

(*
===========================================================================

Project:   Generic Polygon Clipper

           A new algorithm for calculating the difference, intersection,
           exclusive-or or union of arbitrary Polygon sets.

File:      GPC.pas
Author:    Alan Murta (GPC@cs.man.ac.uk)
CVersion:   2.32
Date:      17hesTH December 2004

Copyright: (C) Advanced Interfaces Group,
           University of Manchester.

           This software may be freely copied, modified, and redistributed
           provided that this copyright notice is preserved on all copies.
           The intellectual property rights of the algorithms used reside
           with the University of Manchester Advanced Interfaces Group.

           You may not distribute this software, in whole or in part, as
           part of any commercial product without the express consent of
           the author.

           There is no warranty or other guarantee of fitness of this
           software for any purpose. It is provided solely "as is".

===========================================================================

  Ported to Delphi by Richard B. Winston (rbwinst@usgs.gov) Dec. 17, 2008.
  Based in part on a previous port by Stefan Schedel.

  Mar. 18, 2009 Correction submitted by César Aguilar (cesar.aguilar@gmx.net)
*)

interface

uses
  Windows;

// ===========================================================================
// Constants
// ===========================================================================

const
  CVersion = 'GPC_VERSION "2.32"';
  CGPCEpsilon: Double = 2.2204460492503131E-16; { from float.h }

  // ===========================================================================
  // Public Data Types
  // ===========================================================================

type

  TGpcOp = { Set operation type }
    (goDiff, { Difference }
     goInt, { Intersection }
     goXor, { Exclusive or }
     goUnion { Union }
    );

  TGpcVertex = record { Polygon Vertex structure }
    X: Double; { Vertex x component }
    Y: Double; { Vertex y component }
  end;

  PGpcVertexArray = ^TGpcVertexArray; { Helper Type for indexing }
  TGpcVertexArray = array [0..MaxInt div Sizeof(TGpcVertex) - 1]
    of TGpcVertex;

  PGpcVertexList = ^TGpcVertexList; { Vertex list structure }
  TGpcVertexList = record
    NumVertices: Integer; { Number of vertices in list }
    Vertex: PGpcVertexArray; { Vertex array pointer }
  end;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array [0..MaxInt div Sizeof(Integer) - 1] of Integer;

  PGpcVertexListArray = ^TGpcVertexListArray; { Helper Type for indexing }
  TGpcVertexListArray = array [0..MaxInt div Sizeof(TGpcVertexList) - 1]
    of TGpcVertexList;

  PGpcPolygon = ^TGpcPolygon;
  TGpcPolygon = record { Polygon set structure }
    NumContours: Integer; { Number of contours in Polygon }
    Hole: PIntegerArray; { Hole / external Contour flags }
    Contour: PGpcVertexListArray; { Contour array pointer }
  end;

  PGpcTristrip = ^TGpcTriStrip; { TriStrip set structure }
  TGpcTriStrip = record
    NumStrips: Integer; { Number of tristrips }
    Strip: PGpcVertexListArray; { TriStrip array pointer }
  end;



  // ===========================================================================
  // Public Function Prototypes
  // ===========================================================================

procedure GpcReadPolygon(var F: Text; ReadHoleFlags: Integer; P: PGpcPolygon);

procedure GpcWritePolygon(var F: Text; WriteHoleFlags: Integer; P: PGpcPolygon);

procedure GpcAddContour(Polygon: PGpcPolygon; Contour: PGpcVertexList;
  Hole: Integer);

procedure GpcPolygonClip(SetOperation: TGpcOp; SubjectPolygon: PGpcPolygon;
  ClipPolygon: PGpcPolygon; ResultPolygon: PGpcPolygon);

procedure GpcTristripClip(Op: TGpcOp; CSubj: PGpcPolygon; CClip: PGpcPolygon;
  Result: PGpcTristrip);

procedure GpcPolygonToTristrip(S: PGpcPolygon; T: PGpcTristrip);

procedure GpcFreePolygon(Polygon: PGpcPolygon);

procedure GpcFreeTristrip(TriStrip: PGpcTristrip);

implementation

uses
  SysUtils,
  Math;



// ===========================================================================
// Constants
// ===========================================================================

const
  CDoubleMax: Double = MaxDouble;

  CDoubleDig = 15;

  FFALSE = 0;
  FTRUE = 1;

  CLeft = 0;
  CRight = 1;

  CAbove = 0;
  CBelow = 1;

  CClip = 0;
  CSubj = 1;

  CInvertTriStrips = FFALSE;



  // ===========================================================================
  // Private Data Types
  // ===========================================================================

type
  TVertexType = ( { Edge intersection classes }
    vtNUL, { Empty non-intersection }
    vtEMX, { External maximum }
    vtELI, { External CLeft intermediate }
    vtTED, { Top edge }
    vtERI, { External CRight intermediate }
    vtRED, { CRight edge }
    vtIMM, { Internal maximum and minimum }
    vtIMN, { Internal minimum }
    vtEMN, { External minimum }
    vtEMM, { External maximum and minimum }
    vtLED, { CLeft edge }
    vtILI, { Internal CLeft intermediate }
    vtBED, { Bottom edge }
    vtIRI, { Internal CRight intermediate }
    vtIMX, { Internal maximum }
    vtFUL { Full non-intersection }
    );

  THorizontalEdgeState = (
    hesNH, { No horizontal edge }
    hesBH, { Bottom horizontal edge }
    hesTH { Top horizontal edge }
    );

  TBundleState = (UNBUNDLED, BUNDLE_HEAD, BUNDLE_TAIL);

  PPVertexNode = ^PVertexNode;
  PVertexNode = ^TVertexNode; { Internal Vertex list datatype }

  TVertexNode = record
    X: Double; { X coordinate component }
    Y: Double; { Y coordinate component }
    Next: PVertexNode; { Pointer to next Vertex in list }
  end;

  PVertexNodeArray = ^TVertexNodeArray; { Helper type for indexing }
  TVertexNodeArray = array [0..1] of PVertexNode;

  PPpolygonNode = ^PPolygonNode;
  PPolygonNode = ^TPolygonNode;

  TPolygonNode = record
    Active: Integer;
    Hole: Integer;
    V: array [0..1] of PVertexNode;
    Next: PPolygonNode;
    Proxy: PPolygonNode;
  end;

  PPEdgeNode = ^PEdgeNode;
  PEdgeNode = ^TEdgeNode;

  TEdgeNode = record
    Vertex: TGpcVertex; { Piggy-backed Contour Vertex data }
    Bot: TGpcVertex; { Edge lower (x, y) coordinate }
    Top: TGpcVertex; { Edge upper (x, y) coordinate }
    Xb: Double; { Scanbeam bottom x coordinate }
    Xt: Double; { Scanbeam top x coordinate }
    Dx: Double; { Change in x for a unit y increase }
    Typ: Integer; { CClip / subject edge flag }
    Bundle: array [0..1, 0..1] of Integer; { Bundle edge flags }
    Bside: array [0..1] of Integer; { Bundle CLeft / CRight indicators }
    Bstate: array [0..1] of TBundleState; { Edge bundle state }
    Outp: array [0..1] of PPolygonNode; { Output Polygon / TriStrip pointer }
    Prev: PEdgeNode; { Previous edge in the AET }
    Next: PEdgeNode; { Next edge in the AET }
    Pred: PEdgeNode; { Edge connected at the lower end }
    Succ: PEdgeNode; { Edge connected at the upper end }
    Next_bound: PEdgeNode; { Pointer to next bound in LocalMinimaTable }
  end;

  PPEdgeNodeArray = ^PEdgeNodeArray;
  PEdgeNodeArray = ^TEdgeNodeArray;
  TEdgeNodeArray = array [0..MaxInt div Sizeof(TEdgeNode) - 1] of TEdgeNode;

  PPLocalMinimaTableNode = ^PLocalMinimaTableNode;
  PLocalMinimaTableNode = ^TLocalMinimaTableNode;

  TLocalMinimaTableNode = record { Local minima table }
    Y: Double; { Y coordinate at local minimum }
    FirstBound: PEdgeNode; { Pointer to bound list }
    Next: PLocalMinimaTableNode; { Pointer to next local minimum }
  end;

  PPScanBeamTree = ^PScanBeamTree;
  PScanBeamTree = ^TScanBeamTree;

  TScanBeamTree = record { Scanbeam tree }
    Y: Double; { Scanbeam node y value }
    Less: PScanBeamTree; { Pointer to nodes with lower y }
    More: PScanBeamTree; { Pointer to nodes with higher y }
  end;

  PPIntersectionNode = ^PIntersectionNode;
  PIntersectionNode = ^TIntersectionNode; { Intersection table }

  TIntersectionNode = record
    Ie: array [0..1] of PEdgeNode; { Intersecting edge (bundle) pair }
    Point: TGpcVertex; { Point of intersection }
    Next: PIntersectionNode; { The next intersection table node }
  end;

  PPSortedEdgeTableNode = ^PSortedEdgeTableNode;
  PSortedEdgeTableNode = ^TSortedEdgeTableNode; { Sorted edge table }

  TSortedEdgeTableNode = record
    Edge: PEdgeNode; { Pointer to AET edge }
    Xb: Double; { Scanbeam bottom x coordinate }
    Xt: Double; { Scanbeam top x coordinate }
    Dx: Double; { Change in x for a unit y increase }
    Prev: PSortedEdgeTableNode; { Previous edge in sorted list }
  end;

  PBoundingBox = ^TBoundingBox;

  TBoundingBox = record { Contour axis-aligned bounding box }
    Xmin: Double; { Minimum x coordinate }
    Ymin: Double; { Minimum y coordinate }
    Xmax: Double; { Maximum x coordinate }
    Ymax: Double; { Maximum y coordinate }
  end;

  PBoundingBoxArray = ^TBoundingBoxArray;
  TBoundingBoxArray = array [0..MaxInt div Sizeof(TBoundingBox) - 1] of TBoundingBox;

  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array [0..MaxInt div Sizeof(Double) - 1] of Double;



  // ===========================================================================
  // C Macros, defined as function for PASCAL
  // ===========================================================================

function EQ(A, B: Double): Boolean;
begin
  EQ := Abs(A - B) <= CGPCEpsilon
end;

function PREV_INDEX(I, N: Integer): Integer;
begin
  PREV_INDEX := ((I - 1 + N) mod N);
end;

function NEXT_INDEX(I, N: Integer): Integer;
begin
  NEXT_INDEX := ((I + 1) mod N);
end;

function OPTIMAL(V: PGpcVertexArray; I, N: Integer): Boolean;
begin
  OPTIMAL := (V[PREV_INDEX(I, N)].Y <> V[I].Y) or
    (V[NEXT_INDEX(I, N)].Y <> V[I].Y);
end;

function FWD_MIN(V: PEdgeNodeArray; I, N: Integer): Boolean;
begin
  FWD_MIN := (V[PREV_INDEX(I, N)].Vertex.Y >= V[I].Vertex.Y) and
    (V[NEXT_INDEX(I, N)].Vertex.Y > V[I].Vertex.Y);
end;

function NOT_FMAX(V: PEdgeNodeArray; I, N: Integer): Boolean;
begin
  NOT_FMAX := (V[NEXT_INDEX(I, N)].Vertex.Y > V[I].Vertex.Y);
end;

function REV_MIN(V: PEdgeNodeArray; I, N: Integer): Boolean;
begin
  REV_MIN := (V[PREV_INDEX(I, N)].Vertex.Y > V[I].Vertex.Y) and
    (V[NEXT_INDEX(I, N)].Vertex.Y >= V[I].Vertex.Y);
end;

function NOT_RMAX(V: PEdgeNodeArray; I, N: Integer): Boolean;
begin
  NOT_RMAX := (V[PREV_INDEX(I, N)].Vertex.Y > V[I].Vertex.Y);
end;

procedure MALLOC(var P: Pointer; B: Integer; S: string);
begin
  GetMem(P, B);
  if (P = nil) and (B <> 0) then
    raise Exception.Create(Format('gpc malloc failure: %s', [S]));
end;

procedure AddVertex(var P: PVertexNode; X, Y: Double);
begin
  if P = nil then
  begin
    MALLOC(Pointer(P), Sizeof(TVertexNode), 'tristrip vertex creation');
    P.X := X;
    P.Y := Y;
    P.Next := nil;
  end
  else
    { Head further down the list }
    AddVertex(P.Next, X, Y);
end;

procedure Vertex(var E: PEdgeNode; P, S: Integer; var X, Y: Double);
begin
  AddVertex(E.Outp[P].V[S], X, Y);
  Inc(E.Outp[P].Active);
end;

procedure P_EDGE(var D, E: PEdgeNode; P: Integer; var I, J: Double);
begin
  D := E;
  repeat
    D := D.Prev
  until D.Outp[P] <> nil;
  I := D.Bot.X + D.Dx * (J - D.Bot.Y);
end;

procedure N_EDGE(var D, E: PEdgeNode; P: Integer; var I, J: Double);
begin
  D := E;
  repeat
    D := D.Next;
  until D.Outp[P] <> nil;
  I := D.Bot.X + D.Dx * (J - D.Bot.Y);
end;

procedure Free(var P: Pointer);
begin
  FreeMem(P);
  P := nil;
end;

procedure CFree(var P: Pointer);
begin
  if P <> nil then
    Free(P);
end;



// ===========================================================================
// Global Data
// ===========================================================================

{ Horizontal edge state transitions within scanbeam boundary }
const
  CNextHorizontalEdgeState: array [0..2, 0..5] of THorizontalEdgeState =
  { CAbove     CBelow     CROSS }
  { L   R     L   R     L   R }
  { hesNH } ((hesBH, hesTH, hesTH, hesBH, hesNH, hesNH),
    { hesBH } (hesNH, hesNH, hesNH, hesNH, hesTH, hesTH),
    { hesTH } (hesNH, hesNH, hesNH, hesNH, hesBH, hesBH));




  // ===========================================================================
  // Private Functions
  // ===========================================================================

procedure Reset_it(var It: PIntersectionNode);
var
  Itn: PIntersectionNode;
begin
  while (It <> nil) do
  begin
    Itn := It.Next;
    Free(Pointer(It));
    It := Itn;
  end;
end;

procedure ResetLocalMinimaTable(var LocalMinimaTable: PLocalMinimaTableNode);
var
  LocalMinimaTableNode: PLocalMinimaTableNode;
begin
  while LocalMinimaTable <> nil do
  begin
    LocalMinimaTableNode := LocalMinimaTable^.Next;
    Free(Pointer(LocalMinimaTable));
    LocalMinimaTable := LocalMinimaTableNode;
  end;
end;

procedure InsertBound(B: PPEdgeNodeArray; E: PEdgeNodeArray);
var
  Existing_bound: Pointer;
begin
  if B^ = nil then
  begin
    { Link node e to the tail of the list }
    B^ := E;
  end
  else
  begin
    { Do primary sort on the x field }
    if ((E[0].Bot.X < B^[0].Bot.X)) then
    begin
      { Insert a new node mid-list }
      Existing_bound := B^;
      B^ := E;
      B^[0].Next_bound := Existing_bound;
    end
    else
    begin
      if ((E[0].Bot.X = B^[0].Bot.X)) then
      begin
        { Do secondary sort on the dx field }
        if ((E[0].Dx < B^[0].Dx)) then
        begin
          { Insert a new node mid-list }
          Existing_bound := B^;
          B^ := E;
          B^[0].Next_bound := Existing_bound;
        end
        else
        begin
          { Head further down the list }
          InsertBound(@(B^[0].Next_bound), E);
        end;
      end
      else
      begin
        { Head further down the list }
        InsertBound(@(B^[0].Next_bound), E);
      end;
    end;
  end;
end;

function BoundList(var LocalMinimaTable: PLocalMinimaTableNode;
  Y: Double): PPEdgeNodeArray;
var
  Existing_node: PLocalMinimaTableNode;
begin
  if LocalMinimaTable = nil then
  begin
    { Add node onto the tail end of the LocalMinimaTable }
    MALLOC(Pointer(LocalMinimaTable), Sizeof(TLocalMinimaTableNode), 'LMT insertion');
    LocalMinimaTable.Y := Y;
    LocalMinimaTable.FirstBound := nil;
    LocalMinimaTable.Next := nil;
    Result := @LocalMinimaTable.FirstBound;
  end
  else if (Y < LocalMinimaTable.Y) then
  begin
    { Insert a new LocalMinimaTable node before the current node }
    Existing_node := LocalMinimaTable;
    MALLOC(Pointer(LocalMinimaTable), Sizeof(TLocalMinimaTableNode), 'LMT insertion');
    LocalMinimaTable.Y := Y;
    LocalMinimaTable.FirstBound := nil;
    LocalMinimaTable.Next := Existing_node;
    Result := @LocalMinimaTable.FirstBound;
  end
  else if (Y > LocalMinimaTable.Y) then
    { Head further up the LocalMinimaTable }
    Result := BoundList(LocalMinimaTable.Next, Y)
  else
    { Use this existing LocalMinimaTable node }
    Result := @LocalMinimaTable.FirstBound;
end;

procedure AddToScanBeamTree(var Entries: Integer; var SbTree: PScanBeamTree;
  const Y: Double);
begin
  if SbTree = nil then
  begin
    { Add a new tree node here }
    MALLOC(Pointer(SbTree), Sizeof(TScanBeamTree), 'scanbeam tree insertion');
    SbTree.Y := Y;
    SbTree.Less := nil;
    SbTree.More := nil;
    Inc(Entries);
  end
  else
  begin
    if (SbTree.Y > Y) then
    begin
      { Head into the 'less' sub-tree }
      AddToScanBeamTree(Entries, SbTree.Less, Y);
    end
    else
    begin
      if (SbTree.Y < Y) then
      begin
        { Head into the 'more' sub-tree }
        AddToScanBeamTree(Entries, SbTree.More, Y);
      end;
    end;
  end;
end;

procedure BuildScanBeamTree(var Entries: Integer; var Sbt: PDoubleArray;
  SbTree: PScanBeamTree);
begin
  if SbTree.Less <> nil then
    BuildScanBeamTree(Entries, Sbt, SbTree.Less);
  Sbt[Entries] := SbTree.Y;
  Inc(Entries);
  if SbTree.More <> nil then
    BuildScanBeamTree(Entries, Sbt, SbTree.More);
end;

procedure FreeScanBeamTree(var SbTree: PScanBeamTree);
begin
  if SbTree <> nil then
  begin
    FreeScanBeamTree(SbTree.Less);
    FreeScanBeamTree(SbTree.More);
    Free(Pointer(SbTree));
  end;
end;

function CountOptimalVertices(C: TGpcVertexList): Integer;
var
  I: Integer;
begin
  Result := 0;

  { Ignore non-contributing contours }
  if C.NumVertices > 0 then
  begin
    for I := 0 to C.NumVertices - 1 do
      { Ignore superfluous vertices embedded in horizontal edges }
      if OPTIMAL(C.Vertex, I, C.NumVertices) then
        Inc(Result);
  end;
end;

function BuildLocalMinimaTable(var LocalMinimaTable: PLocalMinimaTableNode; var SbTree: PScanBeamTree;
  var Sbt_entries: Integer; P: PGpcPolygon; Typ: Integer; Op: TGpcOp)
  : PEdgeNodeArray;

var
  C, I, Min, Max, NumEdges, V, NumVertices: Integer;
  TotalVertices, E_index                  : Integer;
  E, EdgeTable                            : PEdgeNodeArray;
begin
  TotalVertices := 0;
  E_index := 0;

  for C := 0 to P.NumContours - 1 do
    Inc(TotalVertices, CountOptimalVertices(P.Contour[C]));

  { Create the entire input Polygon edge table in one go }
  MALLOC(Pointer(EdgeTable), TotalVertices * Sizeof(TEdgeNode),
    'edge table creation');

  for C := 0 to P.NumContours - 1 do
  begin
    if P.Contour[C].NumVertices < 0 then
    begin
      { Ignore the non-contributing Contour and repair the Vertex count }
      P.Contour[C].NumVertices := -P.Contour[C].NumVertices;
    end
    else
    begin
      { Perform Contour optimisation }
      NumVertices := 0;
      for I := 0 to P.Contour[C].NumVertices - 1 do
        if (OPTIMAL(P.Contour[C].Vertex, I, P.Contour[C].NumVertices)) then
        begin
          EdgeTable[NumVertices].Vertex.X := P.Contour[C].Vertex[I].X;
          EdgeTable[NumVertices].Vertex.Y := P.Contour[C].Vertex[I].Y;

          { Record Vertex in the scanbeam table }
          AddToScanBeamTree(Sbt_entries, SbTree, EdgeTable[NumVertices].Vertex.Y);

          Inc(NumVertices);
        end;

      { Do the Contour forward pass }
      for Min := 0 to NumVertices - 1 do
      begin
        { If a forward local minimum... }
        if FWD_MIN(EdgeTable, Min, NumVertices) then
        begin
          { Search for the next local maximum... }
          NumEdges := 1;
          Max := NEXT_INDEX(Min, NumVertices);
          while (NOT_FMAX(EdgeTable, Max, NumVertices)) do
          begin
            Inc(NumEdges);
            Max := NEXT_INDEX(Max, NumVertices);
          end;

          { Build the next edge list }
          E := @EdgeTable[E_index];
          Inc(E_index, NumEdges);
          V := Min;
          E[0].Bstate[CBelow] := UNBUNDLED;
          E[0].Bundle[CBelow][CClip] := FFALSE;
          E[0].Bundle[CBelow][CSubj] := FFALSE;
          for I := 0 to NumEdges - 1 do
          begin
            E[I].Xb := EdgeTable[V].Vertex.X;
            E[I].Bot.X := EdgeTable[V].Vertex.X;
            E[I].Bot.Y := EdgeTable[V].Vertex.Y;

            V := NEXT_INDEX(V, NumVertices);

            E[I].Top.X := EdgeTable[V].Vertex.X;
            E[I].Top.Y := EdgeTable[V].Vertex.Y;
            E[I].Dx := (EdgeTable[V].Vertex.X - E[I].Bot.X) /
              (E[I].Top.Y - E[I].Bot.Y);
            E[I].Typ := Typ;
            E[I].Outp[CAbove] := nil;
            E[I].Outp[CBelow] := nil;
            E[I].Next := nil;
            E[I].Prev := nil;
            if (NumEdges > 1) and (I < (NumEdges - 1)) then
              E[I].Succ := @E[I + 1]
            else
              E[I].Succ := nil;
            if (NumEdges > 1) and (I > 0) then
              E[I].Pred := @E[I - 1]
            else
              E[I].Pred := nil;
            E[I].Next_bound := nil;
            if Op = goDiff then
              E[I].Bside[CClip] := CRight
            else
              E[I].Bside[CClip] := CLeft;
            E[I].Bside[CSubj] := CLeft;
          end;
          InsertBound(BoundList(LocalMinimaTable, EdgeTable[Min].Vertex.Y), E);
        end;
      end;

      { Do the Contour reverse pass }
      for Min := 0 to NumVertices - 1 do
      begin
        { If a reverse local minimum... }
        if REV_MIN(EdgeTable, Min, NumVertices) then
        begin
          { Search for the previous local maximum... }
          NumEdges := 1;
          Max := PREV_INDEX(Min, NumVertices);
          while NOT_RMAX(EdgeTable, Max, NumVertices) do
          begin
            Inc(NumEdges);
            Max := PREV_INDEX(Max, NumVertices);
          end;

          { Build the previous edge list }
          E := @EdgeTable[E_index];
          Inc(E_index, NumEdges);
          V := Min;
          E[0].Bstate[CBelow] := UNBUNDLED;
          E[0].Bundle[CBelow][CClip] := FFALSE;
          E[0].Bundle[CBelow][CSubj] := FFALSE;
          for I := 0 to NumEdges - 1 do
          begin
            E[I].Xb := EdgeTable[V].Vertex.X;
            E[I].Bot.X := EdgeTable[V].Vertex.X;
            E[I].Bot.Y := EdgeTable[V].Vertex.Y;

            V := PREV_INDEX(V, NumVertices);

            E[I].Top.X := EdgeTable[V].Vertex.X;
            E[I].Top.Y := EdgeTable[V].Vertex.Y;
            E[I].Dx := (EdgeTable[V].Vertex.X - E[I].Bot.X) /
              (E[I].Top.Y - E[I].Bot.Y);
            E[I].Typ := Typ;
            E[I].Outp[CAbove] := nil;
            E[I].Outp[CBelow] := nil;
            E[I].Next := nil;
            E[I].Prev := nil;
            if (NumEdges > 1) and (I < (NumEdges - 1)) then
              E[I].Succ := @E[I + 1]
            else
              E[I].Succ := nil;
            if (NumEdges > 1) and (I > 0) then
              E[I].Pred := @E[I - 1]
            else
              E[I].Pred := nil;
            E[I].Next_bound := nil;
            if Op = goDiff then
              E[I].Bside[CClip] := CRight
            else
              E[I].Bside[CClip] := CLeft;
            E[I].Bside[CSubj] := CLeft;
          end;
          InsertBound(BoundList(LocalMinimaTable, EdgeTable[Min].Vertex.Y), E);
        end;
      end;
    end;
  end;
  Result := EdgeTable;
end;

procedure AddEdgeToAET(var Aet: PEdgeNode; Edge: PEdgeNode; Prev: PEdgeNode);
begin
  if Aet = nil then
  begin
    { Append edge onto the tail end of the AET }
    Aet := Edge;
    Edge.Prev := Prev;
    Edge.Next := nil;
  end
  else
  begin
    { Do primary sort on the xb field }
    if (Edge.Xb < Aet.Xb) then
    begin
      { Insert edge here (before the AET edge) }
      Edge.Prev := Prev;
      Edge.Next := Aet;
      Aet.Prev := Edge;
      Aet := Edge;
    end
    else
    begin
      if (Edge.Xb = Aet.Xb) then
      begin
        { Do secondary sort on the dx field }
        if (Edge.Dx < Aet.Dx) then
        begin
          { Insert edge here (before the AET edge) }
          Edge.Prev := Prev;
          Edge.Next := Aet;
          Aet.Prev := Edge;
          Aet := Edge;
        end
        else
        begin
          { Head further into the AET }
          AddEdgeToAET(Aet.Next, Edge, Aet);
        end;
      end
      else
      begin
        { Head further into the AET }
        AddEdgeToAET(Aet.Next, Edge, Aet);
      end;
    end;
  end;
end;

procedure AddIntersection(var It: PIntersectionNode; Edge0, Edge1: PEdgeNode;
  X, Y: Double);
var
  Existing_node: PIntersectionNode;
begin

  if It = nil then
  begin
    { Append a new node to the tail of the list }
    MALLOC(Pointer(It), Sizeof(TIntersectionNode), 'IT insertion');
    It.Ie[0] := Edge0;
    It.Ie[1] := Edge1;
    It.Point.X := X;
    It.Point.Y := Y;
    It.Next := nil;
  end
  else
  begin
    if (It.Point.Y > Y) then
    begin
      { Insert a new node mid-list }
      Existing_node := It;
      MALLOC(Pointer(It), Sizeof(TIntersectionNode), 'IT insertion');
      It.Ie[0] := Edge0;
      It.Ie[1] := Edge1;
      It.Point.X := X;
      It.Point.Y := Y;
      It.Next := Existing_node;
    end
    else
      { Head further down the list }
      AddIntersection(It.Next, Edge0, Edge1, X, Y);
  end;
end;

procedure AddSortedTableEdge(var St: PSortedEdgeTableNode; var It: PIntersectionNode; Edge: PEdgeNode;
  Dy: Double);
var
  Existing_node: PSortedEdgeTableNode;
  Den, X, Y, R : Double;
begin
  if St = nil then
  begin
    { Append edge onto the tail end of the ST }
    MALLOC(Pointer(St), Sizeof(TSortedEdgeTableNode), 'ST insertion');
    St.Edge := Edge;
    St.Xb := Edge.Xb;
    St.Xt := Edge.Xt;
    St.Dx := Edge.Dx;
    St.Prev := nil;
  end
  else
  begin
    Den := (St.Xt - St.Xb) - (Edge.Xt - Edge.Xb);

    { If new edge and ST edge don't cross }
    if ((Edge.Xt >= St.Xt) or (Edge.Dx = St.Dx) or (Abs(Den) <= CGPCEpsilon))
    then
    begin
      { No intersection - insert edge here (before the ST edge) }
      Existing_node := St;
      MALLOC(Pointer(St), Sizeof(TSortedEdgeTableNode), 'ST insertion');
      St.Edge := Edge;
      St.Xb := Edge.Xb;
      St.Xt := Edge.Xt;
      St.Dx := Edge.Dx;
      St.Prev := Existing_node;
    end
    else
    begin
      { Compute intersection between new edge and ST edge }
      R := (Edge.Xb - St.Xb) / Den;
      X := St.Xb + R * (St.Xt - St.Xb);
      Y := R * Dy;

      { Insert the edge pointers and the intersection point in the IT }
      AddIntersection(It, St.Edge, Edge, X, Y);

      { Head further into the ST }
      AddSortedTableEdge(St.Prev, It, Edge, Dy);

    end;
  end;
end;

procedure BuildIntersectionTable(var It: PIntersectionNode; Aet: PEdgeNode;
  Dy: Double);
var
  St, Stp: PSortedEdgeTableNode;
  Edge: PEdgeNode;
begin

  { Build intersection table for the current scanbeam }
  Reset_it(It);
  St := nil;

  { Process each AET edge }
  Edge := Aet;
  while Edge <> nil do
  begin
    if (Edge.Bstate[CAbove] = BUNDLE_HEAD) or (Edge.Bundle[CAbove][CClip] <> 0) or
      (Edge.Bundle[CAbove][CSubj] <> 0) then
      AddSortedTableEdge(St, It, Edge, Dy);
    Edge := Edge.Next;
  end;

  { Free the sorted edge table }
  while St <> nil do
  begin
    Stp := St.Prev;
    Free(Pointer(St));
    St := Stp;
  end;
end;

function CountContours(Polygon: PPolygonNode): Integer;
var
  Nv: Integer;
  V, Nextv: PVertexNode;
begin

  Result := 0;
  while Polygon <> nil do
  begin
    if Polygon.Active <> 0 then
    begin
      { Count the vertices in the current Contour }
      Nv := 0;
      V := Polygon.Proxy.V[CLeft];
      while V <> nil do
      begin
        Inc(Nv);
        V := V.Next;
      end;

      { Record valid Vertex counts in the Active field }
      if (Nv > 2) then
      begin
        Polygon.Active := Nv;
        Inc(Result);
      end
      else
      begin
        { Invalid Contour: just Free the heap }
        V := Polygon.Proxy.V[CLeft];
        while V <> nil do
        begin
          Nextv := V.Next;
          Free(Pointer(V));
          V := Nextv;
        end;
        Polygon.Active := 0;
      end;
    end;

    Polygon := Polygon.Next;
  end;
end;

procedure AddLeft(P: PPolygonNode; X, Y: Double);
var
  Nv: PVertexNode;
begin
  { Create a new Vertex node and set its fields }
  MALLOC(Pointer(Nv), Sizeof(TVertexNode), 'vertex node creation');
  Nv.X := X;
  Nv.Y := Y;

  { Add Vertex nv to the CLeft end of the Polygon's vertex list }
  Nv.Next := P.Proxy.V[CLeft];

  { Update Proxy[CLeft] to point to nv }
  P.Proxy.V[CLeft] := Nv;
end;

procedure MergeLeft(P: PPolygonNode; Q: PPolygonNode; List: PPolygonNode);
var
  Target: PPolygonNode;
begin
  { Label Contour as a Hole }
  Q.Proxy.Hole := FTRUE;

  if P.Proxy <> Q.Proxy then
  begin
    { Assign P's vertex list to the left end of Q's list }
    P.Proxy.V[CRight].Next := Q.Proxy.V[CLeft];
    Q.Proxy.V[CLeft] := P.Proxy.V[CLeft];

    { Redirect any P->Proxy references to Q->Proxy }
    Target := P.Proxy;
    while List <> nil do
    begin
      if List.Proxy = Target then
      begin
        List.Active := FFALSE;
        List.Proxy := Q.Proxy;
      end;
      List := List.Next;
    end;
  end;
end;

procedure AddRight(P: PPolygonNode; X, Y: Double);
var
  Nv: PVertexNode;
begin
  { Create a new Vertex node and set its fields }
  MALLOC(Pointer(Nv), Sizeof(TVertexNode), 'vertex node creation');
  Nv.X := X;
  Nv.Y := Y;
  Nv.Next := nil;

  { Add Vertex nv to the CRight end of the Polygon's vertex list }
  P.Proxy.V[CRight].Next := Nv;

  { Update Proxy.v[CRight] to point to nv }
  P.Proxy.V[CRight] := Nv;
end;

procedure MergeRight(P: PPolygonNode; Q: PPolygonNode; List: PPolygonNode);
var
  Target: PPolygonNode;
begin
  { Label Contour as external }
  Q.Proxy.Hole := FFALSE;

  if P.Proxy <> Q.Proxy then
  begin
    { Assign P's vertex list to the right end of Q's list }
    Q.Proxy.V[CRight].Next := P.Proxy.V[CLeft];
    Q.Proxy.V[CRight] := P.Proxy.V[CRight];

    { Redirect any P->Proxy references to Q->Proxy }
    Target := P.Proxy;
    while List <> nil do
    begin
      if List.Proxy = Target then
      begin
        List.Active := FFALSE;
        List.Proxy := Q.Proxy;
      end;
      List := List.Next;
    end;
  end;
end;

procedure AddLocalMin(P: PPpolygonNode; Edge: PEdgeNode; X, Y: Double);
var
  Nv: PVertexNode;
  Existing_min: PPolygonNode;
begin
  Existing_min := P^;

  MALLOC(Pointer(P^), Sizeof(TPolygonNode), 'polygon node creation');

  { Create a new Vertex node and set its fields }
  MALLOC(Pointer(Nv), Sizeof(TVertexNode), 'vertex node creation');
  Nv.X := X;
  Nv.Y := Y;
  Nv.Next := nil;

  { Initialise Proxy to point to p itself }
  P^.Proxy := P^;
  P^.Active := FTRUE;
  P^.Next := Existing_min;

  { Make v[CLeft] and v[CRight] point to new Vertex nv }
  P^.V[CLeft] := Nv;
  P^.V[CRight] := Nv;

  { Assign Polygon p to the edge }
  Edge.Outp[CAbove] := P^;
end;

function CountTristrips(Tn: PPolygonNode): Integer;
begin
  Result := 0;

  while Tn <> nil do
  begin
    if Tn.Active > 2 then
      Inc(Result);
    Tn := Tn.Next;
  end;
end;

(* procedure AddVertex(t : PPVertexNode; x, y : Double);  overload;
  begin
  if t^ <> nil then
  begin
  MALLOC(Pointer(t^), sizeof(TVertexNode), 'tristrip vertex creation');
  t^.x := x;
  t^.y := y;
  t^.next := nil;
  end
  else
  { Head further down the list }
  AddVertex(@t^.next, x, y);
  end; *)

procedure NewTristrip(var Tn: PPolygonNode; Edge: PEdgeNode; X, Y: Double);
begin
  if Tn = nil then
  begin
    MALLOC(Pointer(Tn), Sizeof(TPolygonNode), 'tristrip node creation');
    Tn.Next := nil;
    Tn.V[CLeft] := nil;
    Tn.V[CRight] := nil;
    Tn.Active := 1;
    AddVertex(Tn.V[CLeft], X, Y);
    Edge.Outp[CAbove] := Tn;
  end
  else
    { Head further down the list }
    NewTristrip(Tn.Next, Edge, X, Y);
end;

function CreateContourBoundingBoxes(P: PGpcPolygon): PBoundingBoxArray;
var
  C, V: Integer;
begin
  MALLOC(Pointer(Result), P.NumContours * Sizeof(TBoundingBox),
    'Bounding box creation');

  { Construct Contour bounding boxes }
  for C := 0 to P.NumContours - 1 do
  begin
    { Initialise bounding box extent }
    Result[C].Xmin := CDoubleMax;
    Result[C].Ymin := CDoubleMax;
    Result[C].Xmax := -CDoubleMax;
    Result[C].Ymax := -CDoubleMax;

    for V := 0 to P.Contour[C].NumVertices - 1 do
    begin
      { Adjust bounding Result }
      if (P.Contour[C].Vertex[V].X < Result[C].Xmin) then
        Result[C].Xmin := P.Contour[C].Vertex[V].X;
      if (P.Contour[C].Vertex[V].Y < Result[C].Ymin) then
        Result[C].Ymin := P.Contour[C].Vertex[V].Y;
      if (P.Contour[C].Vertex[V].X > Result[C].Xmax) then
        Result[C].Xmax := P.Contour[C].Vertex[V].X;
      if (P.Contour[C].Vertex[V].Y > Result[C].Ymax) then
        Result[C].Ymax := P.Contour[C].Vertex[V].Y;
    end;
  end;
end;

procedure MinimaxTest(CSubj: PGpcPolygon; CClip: PGpcPolygon; Op: TGpcOp);
var
  S_bbox, C_bbox: PBoundingBoxArray;
  S, C          : Integer;
  OverlapTable       : PIntegerArray;
  Overlap       : Integer;
begin
  S_bbox := CreateContourBoundingBoxes(CSubj);
  C_bbox := CreateContourBoundingBoxes(CClip);

  MALLOC(Pointer(OverlapTable), CSubj.NumContours * CClip.NumContours *
    Sizeof(Integer), 'overlap table creation');

  { Check all subject Contour bounding boxes against CClip boxes }
  for S := 0 to CSubj.NumContours - 1 do
    for C := 0 to CClip.NumContours - 1 do
      OverlapTable[C * CSubj.NumContours + S] :=
        Integer((not((S_bbox[S].Xmax < C_bbox[C].Xmin) or
        (S_bbox[S].Xmin > C_bbox[C].Xmax))) and
        (not((S_bbox[S].Ymax < C_bbox[C].Ymin) or
        (S_bbox[S].Ymin > C_bbox[C].Ymax))));

  { For each CClip Contour, search for any subject Contour overlaps }
  for C := 0 to CClip.NumContours - 1 do
  begin
    Overlap := 0;
    S := 0;
    while (Overlap = 0) and (S < CSubj.NumContours) do
    begin
      Overlap := OverlapTable[C * CSubj.NumContours + S];
      Inc(S);
    end;

    if Overlap = 0 then
      { Flag non contributing status by negating Vertex count }
      CClip.Contour[C].NumVertices := -CClip.Contour[C].NumVertices;
  end;

  if (Op = goInt) then
  begin
    { For each subject Contour, search for any CClip Contour overlaps }
    for S := 0 to CSubj.NumContours - 1 do
    begin
      Overlap := 0;
      C := 0;
      while (Overlap = 0) and (C < CClip.NumContours) do
      begin
        Overlap := OverlapTable[C * CSubj.NumContours + S];
        Inc(C);
      end;

      if Overlap = 0 then
        { Flag non contributing status by negating Vertex count }
        CSubj.Contour[S].NumVertices := -CSubj.Contour[S].NumVertices;
    end;
  end;

  Free(Pointer(S_bbox));
  Free(Pointer(C_bbox));
  Free(Pointer(OverlapTable));
end;


// ===========================================================================
// Public Functions
// ===========================================================================

procedure GpcFreePolygon(Polygon: PGpcPolygon);
var
  C: Integer;
begin
  for C := 0 to Polygon.NumContours - 1 do
    CFree(Pointer(Polygon.Contour[C].Vertex));

  CFree(Pointer(Polygon.Hole));
  CFree(Pointer(Polygon.Contour));
  Polygon.NumContours := 0;
end;

procedure GpcReadPolygon(var F: Text; ReadHoleFlags: Integer; P: PGpcPolygon);
var
  C, V: Integer;
begin
  Readln(F, P.NumContours);
  MALLOC(Pointer(P.Hole), P.NumContours * Sizeof(Integer),
    'hole flag array creation');
  MALLOC(Pointer(P.Contour), P.NumContours * Sizeof(TGpcVertexList),
    'contour creation');
  for C := 0 to P.NumContours - 1 do
  begin
    Readln(F, P.Contour[C].NumVertices);

    if (ReadHoleFlags = 1) then
      Readln(F, P.Hole[C])
    else
      P.Hole[C] := FFALSE; // * Assume all contours to be external */

    MALLOC(Pointer(P.Contour[C].Vertex), P.Contour[C].NumVertices *
      Sizeof(TGpcVertex), 'vertex creation');
    for V := 0 to P.Contour[C].NumVertices - 1 do
    begin
      read(F, P.Contour[C].Vertex[V].X);
      Readln(F, P.Contour[C].Vertex[V].Y);
    end;
  end;
end;

procedure GpcWritePolygon(var F: Text; WriteHoleFlags: Integer; P: PGpcPolygon);
var
  C, V: Integer;
begin
  Writeln(F, P.NumContours);
  for C := 0 to P.NumContours - 1 do
  begin
    Writeln(F, P.Contour[C].NumVertices);

    if (WriteHoleFlags = 1) then
      Writeln(F, P.Hole[C]);

    for V := 0 to P.Contour[C].NumVertices - 1 do
      Writeln(F, P.Contour[C].Vertex[V].X:20:CDoubleDig, ' ',
        P.Contour[C].Vertex[V].Y:20:CDoubleDig);
  end;
end;

procedure GpcAddContour(Polygon: PGpcPolygon; Contour: PGpcVertexList;
  Hole: Integer);
var
  C, V            : Integer;
  Extended_hole   : PIntegerArray;
  Extended_contour: PGpcVertexListArray;
begin

  { Create an extended Hole array }
  MALLOC(Pointer(Extended_hole), (Polygon.NumContours + 1) * Sizeof(Integer),
    'contour hole addition');

  { Create an extended Contour array }
  MALLOC(Pointer(Extended_contour), (Polygon.NumContours + 1) *
    Sizeof(TGpcVertexList), 'contour addition');

  { Copy the old Contour into the extended Contour array }
  for C := 0 to Polygon.NumContours - 1 do
  begin
    Extended_hole[C] := Polygon.Hole[C];
    Extended_contour[C] := Polygon.Contour[C];
  end;

  { Copy the new Contour onto the end of the extended Contour array }
  C := Polygon.NumContours;
  Extended_hole[C] := Hole;
  Extended_contour[C].NumVertices := Contour.NumVertices;
  MALLOC(Pointer(Extended_contour[C].Vertex), Contour.NumVertices *
    Sizeof(TGpcVertex), 'contour addition');
  for V := 0 to Contour.NumVertices - 1 do
    Extended_contour[C].Vertex[V] := Contour.Vertex[V];

  { Dispose of the old Contour }
  CFREE(Pointer(Polygon.Contour));
  CFREE(Pointer(Polygon.Hole));

  { Update the Polygon information }
  Inc(Polygon.NumContours);
  Polygon.Hole := Extended_hole;
  Polygon.Contour := Extended_contour;
end;

procedure GpcPolygonClip(SetOperation: TGpcOp; SubjectPolygon: PGpcPolygon;
  ClipPolygon: PGpcPolygon; ResultPolygon: PGpcPolygon);
var
  SbTree: PScanBeamTree;
  It, Intersect: PIntersectionNode;
  Edge, PrevEdge, NextEdge, SuccEdge: PEdgeNode;
  E0, E1: PEdgeNode;
  Aet: PEdgeNode;
  C_heap, S_heap                    : PEdgeNodeArray;
  LocalMinimaTable, LocalMin: PLocalMinimaTableNode;
  OutPoly, P, Q, Poly, Npoly, Cf: PPolygonNode;
  Vtx, Nv: PVertexNode;
  Horiz: array [0..1] of THorizontalEdgeState;
  Inn, Exists, Parity: array [0..1] of Integer;
  C, V, Contributing, Search, Scanbeam: Integer;
  SbtEntries, _class, Bl, Br, Tl, Tr: Integer;
  Sbt: PDoubleArray;
  Xb, Px, Yb, Yt, Dy, Ix, Iy: Double;
begin
  Edge := nil;
  SbTree := nil;
  It := nil;
  Aet := nil;
  LocalMinimaTable := nil;
  OutPoly := nil;
  Cf := nil;
  Inn[0] := CLeft;
  Inn[1] := CLeft;
  Exists[0] := CLeft;
  Exists[1] := CLeft;
  Parity[0] := CLeft;
  Parity[1] := CLeft;
  Scanbeam := 0;
  SbtEntries := 0;
  Sbt := nil;
  C_heap := nil;
  S_heap := nil;

  { Test for trivial NULL Result cases }
  if ((SubjectPolygon.NumContours = 0) and (ClipPolygon.NumContours = 0)) or
    ((SubjectPolygon.NumContours = 0) and ((SetOperation = goInt) or
    (SetOperation = goDiff))) or ((ClipPolygon.NumContours = 0) and
    (SetOperation = goInt)) then
  begin
    ResultPolygon.NumContours := 0;
    ResultPolygon.Hole := nil;
    ResultPolygon.Contour := nil;
    Exit;
  end;

  { Identify potentialy contributing contours }
  if (((SetOperation = goInt) or (SetOperation = goDiff)) and
    (SubjectPolygon.NumContours > 0) and (ClipPolygon.NumContours > 0)) then
    MinimaxTest(SubjectPolygon, ClipPolygon, SetOperation);

  { Build LocalMinimaTable }
  if SubjectPolygon.NumContours > 0 then
    S_heap := BuildLocalMinimaTable(LocalMinimaTable, SbTree, SbtEntries, SubjectPolygon, CSubj,
      SetOperation);
  if ClipPolygon.NumContours > 0 then
    C_heap := BuildLocalMinimaTable(LocalMinimaTable, SbTree, SbtEntries, ClipPolygon, CClip,
      SetOperation);

  { Return a NULL Result if no contours contribute }
  if LocalMinimaTable = nil then
  begin
    ResultPolygon.NumContours := 0;
    ResultPolygon.Hole := nil;
    ResultPolygon.Contour := nil;
    ResetLocalMinimaTable(LocalMinimaTable);
    Free(Pointer(S_heap));
    Free(Pointer(C_heap));
    Exit;
  end;

  { Build scanbeam table from scanbeam tree }
  MALLOC(Pointer(Sbt), SbtEntries * Sizeof(Double), 'sbt creation');
  BuildScanBeamTree(Scanbeam, Sbt, SbTree);
  Scanbeam := 0;
  FreeScanBeamTree(SbTree);

  { Allow pointer re-use without causing memory leak }
  if SubjectPolygon = ResultPolygon then
    GpcFreePolygon(SubjectPolygon);
  if ClipPolygon = ResultPolygon then
    GpcFreePolygon(ClipPolygon);

  { Invert CClip Polygon for difference operation }
  if SetOperation = goDiff then
    Parity[CClip] := CRight;

  LocalMin := LocalMinimaTable;

  { Process each scanbeam }
  while (Scanbeam < SbtEntries) do
  begin
    { Set yb and yt to the bottom and top of the scanbeam }
    Yb := Sbt[Scanbeam];
    Inc(Scanbeam);
    if Scanbeam < SbtEntries then
    begin
      Yt := Sbt[Scanbeam];
      Dy := Yt - Yb;
    end;

    { === SCANBEAM BOUNDARY PROCESSING ================================ }

    { If LocalMinimaTable node corresponding to yb exists }
    if LocalMin <> nil then
    begin
      if (LocalMin.Y = Yb) then
      begin
        { Add edges starting at this local minimum to the AET }
        Edge := LocalMin.FirstBound;
        while Edge <> nil do
        begin
          AddEdgeToAET(Aet, Edge, nil);
          Edge := Edge.Next_bound;
        end;
        LocalMin := LocalMin.Next;
      end;
    end;

    { Set dummy previous x value }
    Px := -CDoubleMax;

    { Create bundles within AET }
    E0 := Aet;
    E1 := Aet;

    { Set up bundle fields of first edge }
    Aet.Bundle[CAbove][Integer(Aet.Typ <> 0)] := Integer((Aet.Top.Y <> Yb));
    Aet.Bundle[CAbove][Integer(Aet.Typ = 0)] := FFALSE;
    Aet.Bstate[CAbove] := UNBUNDLED;

    NextEdge := Aet.Next;

    while NextEdge <> nil do
    begin
      { Set up bundle fields of next edge }
      NextEdge.Bundle[CAbove][NextEdge.Typ] :=
        Integer((NextEdge.Top.Y <> Yb));
      NextEdge.Bundle[CAbove][Integer(NextEdge.Typ = 0)] := FFALSE;
      NextEdge.Bstate[CAbove] := UNBUNDLED;

      { Bundle edges CAbove the scanbeam boundary if they coincide }
      if NextEdge.Bundle[CAbove][NextEdge.Typ] <> 0 then
      begin
        if (EQ(E0.Xb, NextEdge.Xb) and EQ(E0.Dx, NextEdge.Dx) and
          (E0.Top.Y <> Yb)) then
        begin
          NextEdge.Bundle[CAbove][NextEdge.Typ] := NextEdge.Bundle[CAbove]
            [NextEdge.Typ] xor E0.Bundle[CAbove][NextEdge.Typ];
          NextEdge.Bundle[CAbove][Integer(NextEdge.Typ = 0)] :=
            E0.Bundle[CAbove][Integer(NextEdge.Typ = 0)];
          NextEdge.Bstate[CAbove] := BUNDLE_HEAD;
          E0.Bundle[CAbove][CClip] := FFALSE;
          E0.Bundle[CAbove][CSubj] := FFALSE;
          E0.Bstate[CAbove] := BUNDLE_TAIL;
        end;
        E0 := NextEdge;
      end;
      NextEdge := NextEdge.Next;
    end;

    Horiz[CClip] := hesNH;
    Horiz[CSubj] := hesNH;

    { Process each edge at this scanbeam boundary }
    Edge := Aet;
    while Edge <> nil do
    begin
      Exists[CClip] := Edge.Bundle[CAbove][CClip] +
        (Edge.Bundle[CBelow][CClip] shl 1);
      Exists[CSubj] := Edge.Bundle[CAbove][CSubj] +
        (Edge.Bundle[CBelow][CSubj] shl 1);

      if (Exists[CClip] <> 0) or (Exists[CSubj] <> 0) then
      begin
        { Set bundle side }
        Edge.Bside[CClip] := Parity[CClip];
        Edge.Bside[CSubj] := Parity[CSubj];

        { Determine contributing status and quadrant occupancies }
        case SetOperation of
          goDiff, goInt:
            begin
              Contributing :=
                Integer(((Exists[CClip] <> 0) and ((Parity[CSubj] <> 0) or
                (Horiz[CSubj] <> hesNH))) or ((Exists[CSubj] <> 0) and
                ((Parity[CClip] <> 0) or (Horiz[CClip] <> hesNH))) or
                ((Exists[CClip] <> 0) and (Exists[CSubj] <> 0) and
                (Parity[CClip] = Parity[CSubj])));
              Br := Integer((Parity[CClip] <> 0) and (Parity[CSubj] <> 0));
              Bl := Integer(((Parity[CClip] xor Edge.Bundle[CAbove][CClip]) <> 0)
                and ((Parity[CSubj] xor Edge.Bundle[CAbove][CSubj]) <> 0));
              Tr := Integer(((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH)) <> 0)
                and ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH)) <> 0));
              Tl := Integer
                (((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH) xor Edge.Bundle
                [CBelow][CClip]) <> 0) and
                ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH) xor Edge.Bundle
                [CBelow][CSubj]) <> 0));
            end;

          goXor:
            begin
              Contributing := Integer((Exists[CClip] <> 0) or
                (Exists[CSubj] <> 0));
              Br := Integer(Parity[CClip] xor Parity[CSubj]);
              Bl := Integer(((Parity[CClip] xor Edge.Bundle[CAbove][CClip]) <> 0)
                xor ((Parity[CSubj] xor Edge.Bundle[CAbove][CSubj]) <> 0));
              Tr := Integer(((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH)) <> 0)
                xor ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH)) <> 0));
              Tl := Integer
                (((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH) xor Edge.Bundle
                [CBelow][CClip]) <> 0)
                xor ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH)
                xor Edge.Bundle[CBelow][CSubj]) <> 0));
            end;

          goUnion:
            begin
              Contributing :=
                Integer(((Exists[CClip] <> 0) and ((Parity[CSubj] = 0) or
                (Horiz[CSubj] <> hesNH))) or ((Exists[CSubj] <> 0) and
                ((Parity[CClip] = 0) or (Horiz[CClip] <> hesNH))) or
                ((Exists[CClip] <> 0) and (Exists[CSubj] <> 0) and
                (Parity[CClip] = Parity[CSubj])));

              Br := Integer((Parity[CClip] <> 0) or (Parity[CSubj] <> 0));
              Bl := Integer(((Parity[CClip] xor Edge.Bundle[CAbove][CClip]) <> 0)
                or ((Parity[CSubj] xor Edge.Bundle[CAbove][CSubj]) <> 0));
              Tr := Integer(((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH)) <> 0)
                or ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH)) <> 0));
              Tl := Integer
                (((Parity[CClip] xor Integer(Horiz[CClip] <> hesNH) xor Edge.Bundle
                [CBelow][CClip]) <> 0) or
                ((Parity[CSubj] xor Integer(Horiz[CSubj] <> hesNH) xor Edge.Bundle
                [CBelow][CSubj]) <> 0));
            end;
        end; { case }

        { Update parity }
        (* parity[CClip] := Integer((parity[CClip] <> 0) xor (edge.bundle[CAbove][CClip] <> 0));
          parity[CSubj] := Integer((parity[CSubj] <> 0) xor (edge.bundle[CAbove][CSubj] <> 0));
        *)
        Parity[CClip] := Parity[CClip] xor Edge.Bundle[CAbove][CClip];
        Parity[CSubj] := Parity[CSubj] xor Edge.Bundle[CAbove][CSubj];

        { Update horizontal state }
        if Exists[CClip] <> 0 then
          Horiz[CClip] := CNextHorizontalEdgeState[Integer(Horiz[CClip])
            ][((Exists[CClip] - 1) shl 1) + Parity[CClip]];
        if Exists[CSubj] <> 0 then
          Horiz[CSubj] := CNextHorizontalEdgeState[Integer(Horiz[CSubj])
            ][((Exists[CSubj] - 1) shl 1) + Parity[CSubj]];

        _class := Tr + (Tl shl 1) + (Br shl 2) + (Bl shl 3);

        if Contributing <> 0 then
        begin
          Xb := Edge.Xb;

          case TVertexType(_class) of
            vtEMN, vtIMN:
              begin
                AddLocalMin(@OutPoly, Edge, Xb, Yb);
                Px := Xb;
                Cf := Edge.Outp[CAbove];
              end;
            vtERI:
              begin
                if (Xb <> Px) then
                begin
                  AddRight(Cf, Xb, Yb);
                  Px := Xb;
                end;
                Edge.Outp[CAbove] := Cf;
                Cf := nil;
              end;
            vtELI:
              begin
                AddLeft(Edge.Outp[CBelow], Xb, Yb);
                Px := Xb;
                Cf := Edge.Outp[CBelow];
              end;
            vtEMX:
              begin
                if (Xb <> Px) then
                begin
                  AddLeft(Cf, Xb, Yb);
                  Px := Xb;
                end;
                MergeRight(Cf, Edge.Outp[CBelow], OutPoly);
                Cf := nil;
              end;
            vtILI:
              begin
                if (Xb <> Px) then
                begin
                  AddLeft(Cf, Xb, Yb);
                  Px := Xb;
                end;
                Edge.Outp[CAbove] := Cf;
                Cf := nil;
              end;
            vtIRI:
              begin
                AddRight(Edge.Outp[CBelow], Xb, Yb);
                Px := Xb;
                Cf := Edge.Outp[CBelow];
                Edge.Outp[CBelow] := nil;
              end;
            vtIMX:
              begin
                if (Xb <> Px) then
                begin
                  AddRight(Cf, Xb, Yb);
                  Px := Xb;
                end;
                MergeLeft(Cf, Edge.Outp[CBelow], OutPoly);
                Cf := nil;
                Edge.Outp[CBelow] := nil;
              end;
            vtIMM:
              begin
                if (Xb <> Px) then
                begin
                  AddRight(Cf, Xb, Yb);
                  Px := Xb;
                end;
                MergeLeft(Cf, Edge.Outp[CBelow], OutPoly);
                Edge.Outp[CBelow] := nil;
                AddLocalMin(@OutPoly, Edge, Xb, Yb);
                Cf := Edge.Outp[CAbove];
              end;
            vtEMM:
              begin
                if (Xb <> Px) then
                begin
                  AddLeft(Cf, Xb, Yb);
                  Px := Xb;
                end;
                MergeRight(Cf, Edge.Outp[CBelow], OutPoly);
                Edge.Outp[CBelow] := nil;
                AddLocalMin(@OutPoly, Edge, Xb, Yb);
                Cf := Edge.Outp[CAbove];
              end;
            vtLED:
              begin
                if (Edge.Bot.Y = Yb) then
                  AddLeft(Edge.Outp[CBelow], Xb, Yb);
                Edge.Outp[CAbove] := Edge.Outp[CBelow];
                Px := Xb;
              end;
            vtRED:
              begin
                if (Edge.Bot.Y = Yb) then
                  AddRight(Edge.Outp[CBelow], Xb, Yb);
                Edge.Outp[CAbove] := Edge.Outp[CBelow];
                Px := Xb;
              end;
          else
          end; { End of case }
        end; { End of contributing conditional }
      end; { End of edge exists conditional }
      Edge := Edge.Next;
    end; { End of AET loop }

    { Delete terminating edges from the AET, otherwise compute xt }
    Edge := Aet;
    while Edge <> nil do
    begin
      if (Edge.Top.Y = Yb) then
      begin
        PrevEdge := Edge.Prev;
        NextEdge := Edge.Next;
        if PrevEdge <> nil then
          PrevEdge.Next := NextEdge
        else
          Aet := NextEdge;
        if NextEdge <> nil then
          NextEdge.Prev := PrevEdge;

        { Copy bundle head state to the adjacent tail edge if required }
        if (Edge.Bstate[CBelow] = BUNDLE_HEAD) and (PrevEdge <> nil) then
        begin
          if PrevEdge.Bstate[CBelow] = BUNDLE_TAIL then
          begin
            PrevEdge.Outp[CBelow] := Edge.Outp[CBelow];
            PrevEdge.Bstate[CBelow] := UNBUNDLED;
            if PrevEdge.Prev <> nil then
              if PrevEdge.Prev.Bstate[CBelow] = BUNDLE_TAIL then
                PrevEdge.Bstate[CBelow] := BUNDLE_HEAD;
          end;
        end;
      end
      else
      begin
        if (Edge.Top.Y = Yt) then
          Edge.Xt := Edge.Top.X
        else
          Edge.Xt := Edge.Bot.X + Edge.Dx * (Yt - Edge.Bot.Y);
      end;

      Edge := Edge.Next;
    end;

    if Scanbeam < SbtEntries then
    begin
      { === SCANBEAM INTERIOR PROCESSING ============================== }

      BuildIntersectionTable(It, Aet, Dy);

      { Process each node in the intersection table }
      Intersect := It;
      while Intersect <> nil do
      begin
        E0 := Intersect.Ie[0];
        E1 := Intersect.Ie[1];

        { Only generate output for contributing intersections }
        if ((E0.Bundle[CAbove][CClip] <> 0) or (E0.Bundle[CAbove][CSubj] <> 0)) and
          ((E1.Bundle[CAbove][CClip] <> 0) or (E1.Bundle[CAbove][CSubj] <> 0)) then
        begin
          P := E0.Outp[CAbove];
          Q := E1.Outp[CAbove];
          Ix := Intersect.Point.X;
          Iy := Intersect.Point.Y + Yb;

          Inn[CClip] :=
            Integer(((E0.Bundle[CAbove][CClip] <> 0) and (E0.Bside[CClip] = 0)) or
            ((E1.Bundle[CAbove][CClip] <> 0) and (E1.Bside[CClip] <> 0)) or
            ((E0.Bundle[CAbove][CClip] = 0) and (E1.Bundle[CAbove][CClip] = 0) and
            (E0.Bside[CClip] <> 0) and (E1.Bside[CClip] <> 0)));

          Inn[CSubj] :=
            Integer(((E0.Bundle[CAbove][CSubj] <> 0) and (E0.Bside[CSubj] = 0)) or
            ((E1.Bundle[CAbove][CSubj] <> 0) and (E1.Bside[CSubj] <> 0)) or
            ((E0.Bundle[CAbove][CSubj] = 0) and (E1.Bundle[CAbove][CSubj] = 0) and
            (E0.Bside[CSubj] <> 0) and (E1.Bside[CSubj] <> 0)));

          { Determine quadrant occupancies }
          case SetOperation of

            goDiff, goInt:
              begin
                Tr := Integer((Inn[CClip] <> 0) and (Inn[CSubj] <> 0));
                Tl := Integer(((Inn[CClip] xor E1.Bundle[CAbove][CClip]) <> 0) and
                  ((Inn[CSubj] xor E1.Bundle[CAbove][CSubj]) <> 0));
                Br := Integer(((Inn[CClip] xor E0.Bundle[CAbove][CClip]) <> 0) and
                  ((Inn[CSubj] xor E0.Bundle[CAbove][CSubj]) <> 0));
                Bl := Integer
                  (((Inn[CClip] xor E1.Bundle[CAbove][CClip] xor E0.Bundle[CAbove]
                  [CClip]) <> 0) and
                  ((Inn[CSubj] xor E1.Bundle[CAbove][CSubj] xor E0.Bundle[CAbove]
                  [CSubj]) <> 0));
              end;

            goXor:
              begin
                Tr := Integer((Inn[CClip] <> 0) xor (Inn[CSubj] <> 0));
                Tl := Integer((Inn[CClip] xor E1.Bundle[CAbove][CClip])
                  xor (Inn[CSubj] xor E1.Bundle[CAbove][CSubj]));
                Br := Integer((Inn[CClip] xor E0.Bundle[CAbove][CClip])
                  xor (Inn[CSubj] xor E0.Bundle[CAbove][CSubj]));
                Bl := Integer
                  ((Inn[CClip] xor E1.Bundle[CAbove][CClip] xor E0.Bundle[CAbove]
                  [CClip]) xor (Inn[CSubj] xor E1.Bundle[CAbove][CSubj]
                  xor E0.Bundle[CAbove][CSubj]));
              end;

            goUnion:
              begin
                Tr := Integer((Inn[CClip] <> 0) or (Inn[CSubj] <> 0));
                Tl := Integer(((Inn[CClip] xor E1.Bundle[CAbove][CClip]) <> 0) or
                  ((Inn[CSubj] xor E1.Bundle[CAbove][CSubj]) <> 0));
                Br := Integer(((Inn[CClip] xor E0.Bundle[CAbove][CClip]) <> 0) or
                  ((Inn[CSubj] xor E0.Bundle[CAbove][CSubj]) <> 0));
                Bl := Integer
                  (((Inn[CClip] xor E1.Bundle[CAbove][CClip] xor E0.Bundle[CAbove]
                  [CClip]) <> 0) or
                  ((Inn[CSubj] xor E1.Bundle[CAbove][CSubj] xor E0.Bundle[CAbove]
                  [CSubj]) <> 0));
              end;
          end; { case }

          _class := Tr + (Tl shl 1) + (Br shl 2) + (Bl shl 3);

          case TVertexType(_class) of
            vtEMN:
              begin
                AddLocalMin(@OutPoly, E0, Ix, Iy);
                E1.Outp[CAbove] := E0.Outp[CAbove];
              end;
            vtERI:
              begin
                if P <> nil then
                begin
                  AddRight(P, Ix, Iy);
                  E1.Outp[CAbove] := P;
                  E0.Outp[CAbove] := nil;
                end;
              end;
            vtELI:
              begin
                if Q <> nil then
                begin
                  AddLeft(Q, Ix, Iy);
                  E0.Outp[CAbove] := Q;
                  E1.Outp[CAbove] := nil;
                end;
              end;
            vtEMX:
              begin
                if (P <> nil) and (Q <> nil) then
                begin
                  AddLeft(P, Ix, Iy);
                  MergeRight(P, Q, OutPoly);
                  E0.Outp[CAbove] := nil;
                  E1.Outp[CAbove] := nil;
                end;
              end;
            vtIMN:
              begin
                AddLocalMin(@OutPoly, E0, Ix, Iy);
                E1.Outp[CAbove] := E0.Outp[CAbove];
              end;
            vtILI:
              begin
                if P <> nil then
                begin
                  AddLeft(P, Ix, Iy);
                  E1.Outp[CAbove] := P;
                  E0.Outp[CAbove] := nil;
                end;
              end;
            vtIRI:
              begin
                if Q <> nil then
                begin
                  AddRight(Q, Ix, Iy);
                  E0.Outp[CAbove] := Q;
                  E1.Outp[CAbove] := nil;
                end;
              end;
            vtIMX:
              begin
                if (P <> nil) and (Q <> nil) then
                begin
                  AddRight(P, Ix, Iy);
                  MergeLeft(P, Q, OutPoly);
                  E0.Outp[CAbove] := nil;
                  E1.Outp[CAbove] := nil;
                end;
              end;
            vtIMM:
              begin
                if (P <> nil) and (Q <> nil) then
                begin
                  AddRight(P, Ix, Iy);
                  MergeLeft(P, Q, OutPoly);
                  AddLocalMin(@OutPoly, E0, Ix, Iy);
                  E1.Outp[CAbove] := E0.Outp[CAbove];
                end;
              end;
            vtEMM:
              begin
                if (P <> nil) and (Q <> nil) then
                begin
                  AddLeft(P, Ix, Iy);
                  MergeRight(P, Q, OutPoly);
                  AddLocalMin(@OutPoly, E0, Ix, Iy);
                  E1.Outp[CAbove] := E0.Outp[CAbove];
                end;
              end;
          else
          end; { End of case }
        end; { End of contributing intersection conditional }

        { Swap bundle sides in response to edge crossing }
        if (E0.Bundle[CAbove][CClip] <> 0) then
          E1.Bside[CClip] := Integer(E1.Bside[CClip] = 0);
        if (E1.Bundle[CAbove][CClip] <> 0) then
          E0.Bside[CClip] := Integer(E0.Bside[CClip] = 0);
        if (E0.Bundle[CAbove][CSubj] <> 0) then
          E1.Bside[CSubj] := Integer(E1.Bside[CSubj] = 0);
        if (E1.Bundle[CAbove][CSubj] <> 0) then
          E0.Bside[CSubj] := Integer(E0.Bside[CSubj] = 0);

        { Swap e0 and e1 bundles in the AET }
        PrevEdge := E0.Prev;
        NextEdge := E1.Next;
        if NextEdge <> nil then
          NextEdge.Prev := E0;

        if E0.Bstate[CAbove] = BUNDLE_HEAD then
        begin
          Search := FTRUE;
          while Search <> 0 do
          begin
            PrevEdge := PrevEdge.Prev;
            if PrevEdge <> nil then
            begin
              if PrevEdge.Bstate[CAbove] <> BUNDLE_TAIL then
                Search := FFALSE;
            end
            else
              Search := FFALSE;
          end;
        end;
        if PrevEdge = nil then
        begin
          Aet.Prev := E1;
          E1.Next := Aet;
          Aet := E0.Next;
        end
        else
        begin
          PrevEdge.Next.Prev := E1;
          E1.Next := PrevEdge.Next;
          PrevEdge.Next := E0.Next;
        end;
        E0.Next.Prev := PrevEdge;
        E1.Next.Prev := E1;
        E0.Next := NextEdge;

        Intersect := Intersect.Next;
      end; { End of IT loop }

      { Prepare for next scanbeam }
      Edge := Aet;
      while Edge <> nil do
      begin
        NextEdge := Edge.Next;
        SuccEdge := Edge.Succ;

        if (Edge.Top.Y = Yt) and (SuccEdge <> nil) then
        begin
          { Replace AET edge by its successor }
          SuccEdge.Outp[CBelow] := Edge.Outp[CAbove];
          SuccEdge.Bstate[CBelow] := Edge.Bstate[CAbove];
          SuccEdge.Bundle[CBelow][CClip] := Edge.Bundle[CAbove][CClip];
          SuccEdge.Bundle[CBelow][CSubj] := Edge.Bundle[CAbove][CSubj];
          PrevEdge := Edge.Prev;
          if PrevEdge <> nil then
            PrevEdge.Next := SuccEdge
          else
            Aet := SuccEdge;
          if NextEdge <> nil then
            NextEdge.Prev := SuccEdge;
          SuccEdge.Prev := PrevEdge;
          SuccEdge.Next := NextEdge;
        end
        else
        begin
          { Update this edge }
          Edge.Outp[CBelow] := Edge.Outp[CAbove];
          Edge.Bstate[CBelow] := Edge.Bstate[CAbove];
          Edge.Bundle[CBelow][CClip] := Edge.Bundle[CAbove][CClip];
          Edge.Bundle[CBelow][CSubj] := Edge.Bundle[CAbove][CSubj];
          Edge.Xb := Edge.Xt;
        end;
        Edge.Outp[CAbove] := nil;
        Edge := NextEdge;
      end;
    end;
  end; { === END OF SCANBEAM PROCESSING ================================== }

  { Generate Result Polygon from OutPoly }
  ResultPolygon.Contour := nil;
  ResultPolygon.Hole := nil;
  ResultPolygon.NumContours := CountContours(OutPoly);
  if ResultPolygon.NumContours > 0 then
  begin
    MALLOC(Pointer(ResultPolygon.Hole), ResultPolygon.NumContours *
      Sizeof(Integer), 'hole flag table creation');
    MALLOC(Pointer(ResultPolygon.Contour), ResultPolygon.NumContours *
      Sizeof(TGpcVertexList), 'contour creation');
    Poly := OutPoly;
    C := 0;

    while Poly <> nil do
    begin
      Npoly := Poly.Next;
      if Poly.Active <> 0 then
      begin
        ResultPolygon.Hole[C] := Poly.Proxy.Hole;
        ResultPolygon.Contour[C].NumVertices := Poly.Active;
        MALLOC(Pointer(ResultPolygon.Contour[C].Vertex),
          ResultPolygon.Contour[C].NumVertices * Sizeof(TGpcVertex),
          'vertex creation');

        V := ResultPolygon.Contour[C].NumVertices - 1;
        Vtx := Poly.Proxy.V[CLeft];
        while Vtx <> nil do
        begin
          Nv := Vtx.Next;
          ResultPolygon.Contour[C].Vertex[V].X := Vtx.X;
          ResultPolygon.Contour[C].Vertex[V].Y := Vtx.Y;
          Free(Pointer(Vtx));
          Dec(V);
          Vtx := Nv;
        end;
        Inc(C);
      end;
      Free(Pointer(Poly));
      Poly := Npoly;
    end;
  end
  else
  begin
    Poly := OutPoly;
    while Poly <> nil do
    begin
      Npoly := Poly.Next;
      Free(Pointer(Poly));
      Poly := Npoly;
    end;

  end;

  { Tidy up }
  Reset_it(It);
  ResetLocalMinimaTable(LocalMinimaTable);
  Free(Pointer(C_heap));
  Free(Pointer(S_heap));
  Free(Pointer(Sbt));
end;

procedure GpcFreeTristrip(TriStrip: PGpcTristrip);
var
  S: Integer;
begin
  for S := 0 to TriStrip.NumStrips - 1 do
    CFREE(Pointer(TriStrip.Strip[S].Vertex));
  CFREE(Pointer(TriStrip.Strip));
  TriStrip.NumStrips := 0;
end;

procedure GpcPolygonToTristrip(S: PGpcPolygon; T: PGpcTristrip);
var
  C: TGpcPolygon;
begin
  C.NumContours := 0;
  C.Hole := nil;
  C.Contour := nil;
  GpcTristripClip(goDiff, S, @C, T);
end;

procedure GpcTristripClip(Op: TGpcOp; CSubj: PGpcPolygon; CClip: PGpcPolygon;
  Result: PGpcTristrip);
var
  SbTree: PScanBeamTree;
  It: PIntersectionNode;
  Intersect: PIntersectionNode;
  Edge, Prev_edge, Next_edge, Succ_edge, E0, E1: PEdgeNode;
  Aet: PEdgeNode;
  C_heap, S_heap: PEdgeNodeArray;
  Cf: PEdgeNode;
  LocalMinimaTable, Local_min: PLocalMinimaTableNode;
  Tlist, Tn, Tnn, P, Q: PPolygonNode;
  Lt, Ltn, Rt, Rtn: PVertexNode;
  Horiz: array [0..1] of THorizontalEdgeState;
  Cft: TVertexType;
  InArray: array [0..1] of Integer;
  Exists: array [0..1] of Integer;
  Parity: array [0..1] of Integer;
  S, V, Contributing, Search, Scanbeam, Sbt_entries: Integer;
  Vclass, Bl, Br, Tl, Tr        : Integer;
  Sbt: PDoubleArray;
  Xb, Px, Nx, Yb, Yt, Dy, Ix, Iy: Double;
begin
  SbTree := nil;
  It := nil;
  Aet := nil;
  C_heap := nil;
  S_heap := nil;
  LocalMinimaTable := nil;
  Tlist := nil;
  Parity[0] := CLeft;
  Parity[1] := CLeft;
  Scanbeam := 0;
  Sbt_entries := 0;
  Sbt := nil;

  // * Test for trivial NULL Result cases */
  if (((CSubj.NumContours = 0) and (CClip.NumContours = 0)) or
    ((CSubj.NumContours = 0) and ((Op = goInt) or (Op = goDiff))) or
    ((CClip.NumContours = 0) and (Op = goInt))) then
  begin
    Result.NumStrips := 0;
    Result.Strip := nil;
    Exit;
  end;

  // * Identify potentialy contributing contours */
  if (((Op = goInt) or (Op = goDiff)) and (CSubj.NumContours > 0) and
    (CClip.NumContours > 0)) then
  begin
    MinimaxTest(CSubj, CClip, Op);
  end;

  // * Build LocalMinimaTable */
  if (CSubj.NumContours > 0) then
    S_heap := BuildLocalMinimaTable(LocalMinimaTable, SbTree, Sbt_entries, CSubj, GPC.CSubj, Op);
  if (CClip.NumContours > 0) then
    C_heap := BuildLocalMinimaTable(LocalMinimaTable, SbTree, Sbt_entries, CClip, GPC.CClip, Op);

  // * Return a NULL Result if no contours contribute */
  if (LocalMinimaTable = nil) then
  begin
    Result.NumStrips := 0;
    Result.Strip := nil;
    ResetLocalMinimaTable(LocalMinimaTable);
    Free(Pointer(S_heap));
    Free(Pointer(C_heap));
    Exit;
  end;

  // * Build scanbeam table from scanbeam tree */
  MALLOC(Pointer(Sbt), Sbt_entries * Sizeof(Double), 'sbt creation');
  BuildScanBeamTree(Scanbeam, Sbt, SbTree);
  Scanbeam := 0;
  FreeScanBeamTree(SbTree);

  // * Invert CClip Polygon for difference operation */
  if (Op = goDiff) then
    Parity[GPC.CClip] := CRight;

  Local_min := LocalMinimaTable;

  // * Process each scanbeam */
  while (Scanbeam < Sbt_entries) do
  begin
    // * Set yb and yt to the bottom and top of the scanbeam */
    Yb := Sbt[Scanbeam];
    Inc(Scanbeam);
    if (Scanbeam < Sbt_entries) then
    begin
      Yt := Sbt[Scanbeam];
      Dy := Yt - Yb;
    end;

    // * === SCANBEAM BOUNDARY PROCESSING ================================ */

    // * If LocalMinimaTable node corresponding to yb exists */
    if (Local_min <> nil) then
    begin
      if (Local_min.Y = Yb) then
      begin
        // * Add edges starting at this local minimum to the AET */
        Edge := Local_min.FirstBound;
        while Edge <> nil do
        begin
          AddEdgeToAET(Aet, Edge, nil);
          Edge := Edge.Next_bound;
        end;
        Local_min := Local_min.Next;
      end;
    end;

    // * Set dummy previous x value */
    Px := -CDoubleMax;

    // * Create bundles within AET */
    E0 := Aet;
    E1 := Aet;

    // * Set up bundle fields of first edge */
    Aet.Bundle[CAbove][Aet.Typ] := Ord(Aet.Top.Y <> Yb);
    Aet.Bundle[CAbove][Ord(Aet.Typ = 0)] := FFALSE;
    Aet.Bstate[CAbove] := UNBUNDLED;

    Next_edge := Aet.Next;
    while Next_edge <> nil do
    begin

      // * Set up bundle fields of next edge */
      Next_edge.Bundle[CAbove][Next_edge.Typ] := Ord(Next_edge.Top.Y <> Yb);
      Next_edge.Bundle[CAbove][Ord(Next_edge.Typ = 0)] := FFALSE;
      Next_edge.Bstate[CAbove] := UNBUNDLED;

      // * Bundle edges CAbove the scanbeam boundary if they coincide */
      if (Next_edge.Bundle[CAbove][Next_edge.Typ] <> 0) then
      begin
        if (EQ(E0.Xb, Next_edge.Xb) and EQ(E0.Dx, Next_edge.Dx) and
          (E0.Top.Y <> Yb)) then
        begin
          Next_edge.Bundle[CAbove][Next_edge.Typ] := Next_edge.Bundle[CAbove]
            [Next_edge.Typ] xor E0.Bundle[CAbove][Next_edge.Typ];
          Next_edge.Bundle[CAbove][Ord(Next_edge.Typ = 0)] :=
            E0.Bundle[CAbove][Ord(Next_edge.Typ = 0)];
          Next_edge.Bstate[CAbove] := BUNDLE_HEAD;
          E0.Bundle[CAbove][GPC.CClip] := FFALSE;
          E0.Bundle[CAbove][GPC.CSubj] := FFALSE;
          E0.Bstate[CAbove] := BUNDLE_TAIL;
        end;
        E0 := Next_edge;
      end;
      Next_edge := Next_edge.Next;
    end;

    Horiz[GPC.CClip] := hesNH;
    Horiz[GPC.CSubj] := hesNH;

    // * Process each edge at this scanbeam boundary */
    Edge := Aet;
    while Edge <> nil do
    begin
      Exists[GPC.CClip] := Edge.Bundle[CAbove][GPC.CClip] +
        (Edge.Bundle[CBelow][GPC.CClip] shl 1);
      Exists[GPC.CSubj] := Edge.Bundle[CAbove][GPC.CSubj] +
        (Edge.Bundle[CBelow][GPC.CSubj] shl 1);

      if ((Exists[GPC.CClip] <> 0) or (Exists[GPC.CSubj] <> 0)) then
      begin
        // * Set bundle side */
        Edge.Bside[GPC.CClip] := Parity[GPC.CClip];
        Edge.Bside[GPC.CSubj] := Parity[GPC.CSubj];

        // * Determine contributing status and quadrant occupancies */
        case (Op) of

          goDiff, goInt:
            begin
              Contributing :=
                Ord(((Exists[GPC.CClip] <> 0) and ((Parity[GPC.CSubj] <> 0) or
                (Horiz[GPC.CSubj] <> hesNH))) or ((Exists[GPC.CSubj] <> 0) and
                ((Parity[GPC.CClip] <> 0) or (Horiz[GPC.CClip] <> hesNH))) or
                ((Exists[GPC.CClip] <> 0) and (Exists[GPC.CSubj] <> 0) and
                (Parity[GPC.CClip] = Parity[GPC.CSubj])));
              Br := (Parity[GPC.CClip]) and (Parity[GPC.CSubj]);
              Bl := (Parity[GPC.CClip] xor Edge.Bundle[CAbove][GPC.CClip]) and
                (Parity[GPC.CSubj] xor Edge.Bundle[CAbove][GPC.CSubj]);
              Tr := (Parity[GPC.CClip] xor Ord(Horiz[GPC.CClip] <> hesNH)) and
                (Parity[GPC.CSubj] xor Ord(Horiz[GPC.CSubj] <> hesNH));
              Tl := (Parity[GPC.CClip] xor (Ord(Horiz[GPC.CClip] <> hesNH)
                xor Edge.Bundle[CBelow][GPC.CClip])) and
                (Parity[GPC.CSubj] xor (Ord(Horiz[GPC.CSubj] <> hesNH)
                xor Edge.Bundle[CBelow][GPC.CSubj]));
            end;
          goXor:
            begin
              Contributing := Exists[GPC.CClip] or Exists[GPC.CSubj];
              Br := (Parity[GPC.CClip]) xor (Parity[GPC.CSubj]);
              Bl := (Parity[GPC.CClip] xor Edge.Bundle[CAbove][GPC.CClip])
                xor (Parity[GPC.CSubj] xor Edge.Bundle[CAbove][GPC.CSubj]);
              Tr := (Parity[GPC.CClip] xor Ord(Horiz[GPC.CClip] <> hesNH))
                xor (Parity[GPC.CSubj] xor Ord(Horiz[GPC.CSubj] <> hesNH));
              Tl := (Parity[GPC.CClip] xor (Ord(Horiz[GPC.CClip] <> hesNH)
                xor Edge.Bundle[CBelow][GPC.CClip]))
                xor (Parity[GPC.CSubj] xor (Ord(Horiz[GPC.CSubj] <> hesNH)
                xor Edge.Bundle[CBelow][GPC.CSubj]));
            end;
          goUnion:
            begin
              Contributing :=
                Ord(((Exists[GPC.CClip] <> 0) and ((Parity[GPC.CSubj] = 0) or
                (Horiz[GPC.CSubj] <> hesNH))) or ((Exists[GPC.CSubj] <> 0) and
                ((Parity[GPC.CClip] = 0) or (Horiz[GPC.CClip] <> hesNH))) or
                ((Exists[GPC.CClip] <> 0) and (Exists[GPC.CSubj] <> 0) and
                (Parity[GPC.CClip] = Parity[GPC.CSubj])));
              Br := (Parity[GPC.CClip]) or (Parity[GPC.CSubj]);
              Bl := (Parity[GPC.CClip] xor Edge.Bundle[CAbove][GPC.CClip]) or
                (Parity[GPC.CSubj] xor Edge.Bundle[CAbove][GPC.CSubj]);
              Tr := (Parity[GPC.CClip] xor Ord(Horiz[GPC.CClip] <> hesNH)) or
                (Parity[GPC.CSubj] xor Ord(Horiz[GPC.CSubj] <> hesNH));
              Tl := (Parity[GPC.CClip] xor (Ord(Horiz[GPC.CClip] <> hesNH)
                xor Edge.Bundle[CBelow][GPC.CClip])) or
                (Parity[GPC.CSubj] xor (Ord(Horiz[GPC.CSubj] <> hesNH)
                xor Edge.Bundle[CBelow][GPC.CSubj]));
            end;
        end;

        // * Update parity */
        Parity[GPC.CClip] := Parity[GPC.CClip] xor Edge.Bundle[CAbove][GPC.CClip];
        Parity[GPC.CSubj] := Parity[GPC.CSubj] xor Edge.Bundle[CAbove][GPC.CSubj];

        // * Update horizontal state */
        if (Exists[GPC.CClip] <> 0) then
          Horiz[GPC.CClip] := CNextHorizontalEdgeState[Ord(Horiz[GPC.CClip])]
            [((Exists[GPC.CClip] - 1) shl 1) + Parity[GPC.CClip]];
        if (Exists[GPC.CSubj] <> 0) then
          Horiz[GPC.CSubj] := CNextHorizontalEdgeState[Ord(Horiz[GPC.CSubj])]
            [((Exists[GPC.CSubj] - 1) shl 1) + Parity[GPC.CSubj]];

        Vclass := Tr + (Tl shl 1) + (Br shl 2) + (Bl shl 3);

        if (Contributing <> 0) then
        begin
          Xb := Edge.Xb;

          case TVertexType(Vclass) of

            vtEMN:
              begin
                NewTristrip(Tlist, Edge, Xb, Yb);
                Cf := Edge;
              end;
            vtERI:
              begin
                Edge.Outp[CAbove] := Cf.Outp[CAbove];
                if (Xb <> Cf.Xb) then
                  Vertex(Edge, CAbove, CRight, Xb, Yb);
                Cf := nil;
              end;
            vtELI:
              begin
                Vertex(Edge, CBelow, CLeft, Xb, Yb);
                Edge.Outp[CAbove] := nil;
                Cf := Edge;
              end;
            vtEMX:
              begin
                if (Xb <> Cf.Xb) then
                  Vertex(Edge, CBelow, CRight, Xb, Yb);
                Edge.Outp[CAbove] := nil;
                Cf := nil;
              end;
            vtIMN:
              begin
                if (Cft = vtLED) then
                begin
                  if (Cf.Bot.Y <> Yb) then
                    Vertex(Cf, CBelow, CLeft, Cf.Xb, Yb);
                  NewTristrip(Tlist, Cf, Cf.Xb, Yb);
                end;
                Edge.Outp[CAbove] := Cf.Outp[CAbove];
                Vertex(Edge, CAbove, CRight, Xb, Yb);
              end;
            vtILI:
              begin
                NewTristrip(Tlist, Edge, Xb, Yb);
                Cf := Edge;
                Cft := vtILI;
              end;
            vtIRI:
              begin
                if (Cft = vtLED) then
                begin
                  if (Cf.Bot.Y <> Yb) then
                    Vertex(Cf, CBelow, CLeft, Cf.Xb, Yb);
                  NewTristrip(Tlist, Cf, Cf.Xb, Yb);
                end;
                Vertex(Edge, CBelow, CRight, Xb, Yb);
                Edge.Outp[CAbove] := nil;
              end;
            vtIMX:
              begin
                Vertex(Edge, CBelow, CLeft, Xb, Yb);
                Edge.Outp[CAbove] := nil;
                Cft := vtIMX;
              end;
            vtIMM:
              begin
                Vertex(Edge, CBelow, CLeft, Xb, Yb);
                Edge.Outp[CAbove] := Cf.Outp[CAbove];
                if (Xb <> Cf.Xb) then
                  Vertex(Cf, CAbove, CRight, Xb, Yb);
                Cf := Edge;
              end;
            vtEMM:
              begin
                Vertex(Edge, CBelow, CRight, Xb, Yb);
                Edge.Outp[CAbove] := nil;
                NewTristrip(Tlist, Edge, Xb, Yb);
                Cf := Edge;
              end;
            vtLED:
              begin
                if (Edge.Bot.Y = Yb) then
                  Vertex(Edge, CBelow, CLeft, Xb, Yb);
                Edge.Outp[CAbove] := Edge.Outp[CBelow];
                Cf := Edge;
                Cft := vtLED;
              end;
            vtRED:
              begin
                Edge.Outp[CAbove] := Cf.Outp[CAbove];
                if (Cft = vtLED) then
                begin
                  if (Cf.Bot.Y = Yb) then
                    Vertex(Edge, CBelow, CRight, Xb, Yb)
                  else if (Edge.Bot.Y = Yb) then
                  begin
                    Vertex(Cf, CBelow, CLeft, Cf.Xb, Yb);
                    Vertex(Edge, CBelow, CRight, Xb, Yb);
                  end;
                end
                else
                begin
                  Vertex(Edge, CBelow, CRight, Xb, Yb);
                  Vertex(Edge, CAbove, CRight, Xb, Yb);
                end;
                Cf := nil;
              end;
            // * End of switch */
          end;
          // * End of contributing conditional */
        end;
        // * End of edge exists conditional */

      end;
      // * End of AET loop */

      Edge := Edge.Next
    end;

    // * Delete terminating edges from the AET, otherwise compute xt */
    Edge := Aet;
    while Edge <> nil do
    begin
      if (Edge.Top.Y = Yb) then
      begin
        Prev_edge := Edge.Prev;
        Next_edge := Edge.Next;
        if (Prev_edge <> nil) then
          Prev_edge.Next := Next_edge
        else
          Aet := Next_edge;
        if (Next_edge <> nil) then
          Next_edge.Prev := Prev_edge;

        // * Copy bundle head state to the adjacent tail edge if required */
        if ((Edge.Bstate[CBelow] = BUNDLE_HEAD) and (Prev_edge <> nil)) then
        begin
          if (Prev_edge.Bstate[CBelow] = BUNDLE_TAIL) then
          begin
            Prev_edge.Outp[CBelow] := Edge.Outp[CBelow];
            Prev_edge.Bstate[CBelow] := UNBUNDLED;
            if (Prev_edge.Prev <> nil) then
              if (Prev_edge.Prev.Bstate[CBelow] = BUNDLE_TAIL) then
                Prev_edge.Bstate[CBelow] := BUNDLE_HEAD;
          end;
        end;
      end
      else
      begin
        if (Edge.Top.Y = Yt) then
          Edge.Xt := Edge.Top.X
        else
          Edge.Xt := Edge.Bot.X + Edge.Dx * (Yt - Edge.Bot.Y);
      end;

      Edge := Edge.Next
    end;

    if (Scanbeam < Sbt_entries) then
    begin
      // * === SCANBEAM INTERIOR PROCESSING ============================== */

      BuildIntersectionTable(It, Aet, Dy);

      // * Process each node in the intersection table */
      Intersect := It;
      while (Intersect <> nil) do
      begin
        E0 := Intersect.Ie[0];
        E1 := Intersect.Ie[1];

        // * Only generate output for contributing intersections */
        if (((E0.Bundle[CAbove][GPC.CClip] <> 0) or (E0.Bundle[CAbove][GPC.CSubj] <>
          0)) and ((E1.Bundle[CAbove][GPC.CClip] <> 0) or
          (E1.Bundle[CAbove][GPC.CSubj] <> 0))) then
        begin
          P := E0.Outp[CAbove];
          Q := E1.Outp[CAbove];
          Ix := Intersect.Point.X;
          Iy := Intersect.Point.Y + Yb;

          InArray[GPC.CClip] :=
            Ord(((E0.Bundle[CAbove][GPC.CClip] <> 0) and (E0.Bside[GPC.CClip] = 0))
            or ((E1.Bundle[CAbove][GPC.CClip] <> 0) and (E1.Bside[GPC.CClip] <> 0))
            or ((E0.Bundle[CAbove][GPC.CClip] = 0) and
            (E1.Bundle[CAbove][GPC.CClip] = 0) and (E0.Bside[GPC.CClip] <> 0) and
            (E1.Bside[GPC.CClip] <> 0)));
          InArray[GPC.CSubj] :=
            Ord(((E0.Bundle[CAbove][GPC.CSubj] <> 0) and (E0.Bside[GPC.CSubj] = 0))
            or ((E1.Bundle[CAbove][GPC.CSubj] <> 0) and (E1.Bside[GPC.CSubj] <> 0))
            or ((E0.Bundle[CAbove][GPC.CSubj] = 0) and
            (E1.Bundle[CAbove][GPC.CSubj] = 0) and (E0.Bside[GPC.CSubj] <> 0) and
            (E1.Bside[GPC.CSubj] <> 0)));

          // * Determine quadrant occupancies */
          case (Op) of

            goDiff, goInt:
              begin
                Tr := (InArray[GPC.CClip]) and (InArray[GPC.CSubj]);
                Tl := (InArray[GPC.CClip] xor E1.Bundle[CAbove][GPC.CClip]) and
                  (InArray[GPC.CSubj] xor E1.Bundle[CAbove][GPC.CSubj]);
                Br := (InArray[GPC.CClip] xor E0.Bundle[CAbove][GPC.CClip]) and
                  (InArray[GPC.CSubj] xor E0.Bundle[CAbove][GPC.CSubj]);
                Bl := (InArray[GPC.CClip] xor E1.Bundle[CAbove][GPC.CClip]
                  xor E0.Bundle[CAbove][GPC.CClip]) and
                  (InArray[GPC.CSubj] xor E1.Bundle[CAbove][GPC.CSubj]
                  xor E0.Bundle[CAbove][GPC.CSubj]);

              end;
            goXor:
              begin
                Tr := (InArray[GPC.CClip]) xor (InArray[GPC.CSubj]);
                Tl := (InArray[GPC.CClip] xor E1.Bundle[CAbove][GPC.CClip])
                  xor (InArray[GPC.CSubj] xor E1.Bundle[CAbove][GPC.CSubj]);
                Br := (InArray[GPC.CClip] xor E0.Bundle[CAbove][GPC.CClip])
                  xor (InArray[GPC.CSubj] xor E0.Bundle[CAbove][GPC.CSubj]);
                Bl := (InArray[GPC.CClip] xor E1.Bundle[CAbove][GPC.CClip]
                  xor E0.Bundle[CAbove][GPC.CClip])
                  xor (InArray[GPC.CSubj] xor E1.Bundle[CAbove][GPC.CSubj]
                  xor E0.Bundle[CAbove][GPC.CSubj]);
              end;
            goUnion:
              begin
                Tr := (InArray[GPC.CClip]) or (InArray[GPC.CSubj]);
                Tl := (InArray[GPC.CClip] xor E1.Bundle[CAbove][GPC.CClip]) or
                  (InArray[GPC.CSubj] xor E1.Bundle[CAbove][GPC.CSubj]);
                Br := (InArray[GPC.CClip] xor E0.Bundle[CAbove][GPC.CClip]) or
                  (InArray[GPC.CSubj] xor E0.Bundle[CAbove][GPC.CSubj]);
                Bl := (InArray[GPC.CClip] xor E1.Bundle[CAbove][GPC.CClip]
                  xor E0.Bundle[CAbove][GPC.CClip]) or
                  (InArray[GPC.CSubj] xor E1.Bundle[CAbove][GPC.CSubj]
                  xor E0.Bundle[CAbove][GPC.CSubj]);
              end;
          end;

          Vclass := Tr + (Tl shl 1) + (Br shl 2) + (Bl shl 3);

          case TVertexType(Vclass) of
            vtEMN:
              begin
                NewTristrip(Tlist, E1, Ix, Iy);
                E0.Outp[CAbove] := E1.Outp[CAbove];
              end;
            vtERI:
              begin
                if (P <> nil) then
                begin
                  P_EDGE(Prev_edge, E0, CAbove, Px, Iy);
                  Vertex(Prev_edge, CAbove, CLeft, Px, Iy);
                  Vertex(E0, CAbove, CRight, Ix, Iy);
                  E1.Outp[CAbove] := E0.Outp[CAbove];
                  E0.Outp[CAbove] := nil;
                end;
              end;
            vtELI:
              begin
                if (Q <> nil) then
                begin
                  N_EDGE(Next_edge, E1, CAbove, Nx, Iy);
                  Vertex(E1, CAbove, CLeft, Ix, Iy);
                  Vertex(Next_edge, CAbove, CRight, Nx, Iy);
                  E0.Outp[CAbove] := E1.Outp[CAbove];
                  E1.Outp[CAbove] := nil;
                end
              end;
            vtEMX:
              begin
                if ((P <> nil) and (Q <> nil)) then
                begin
                  Vertex(E0, CAbove, CLeft, Ix, Iy);
                  E0.Outp[CAbove] := nil;
                  E1.Outp[CAbove] := nil;
                end
              end;
            vtIMN:
              begin
                P_EDGE(Prev_edge, E0, CAbove, Px, Iy);
                Vertex(Prev_edge, CAbove, CLeft, Px, Iy);
                N_EDGE(Next_edge, E1, CAbove, Nx, Iy);
                Vertex(Next_edge, CAbove, CRight, Nx, Iy);
                NewTristrip(Tlist, Prev_edge, Px, Iy);
                E1.Outp[CAbove] := Prev_edge.Outp[CAbove];
                Vertex(E1, CAbove, CRight, Ix, Iy);
                NewTristrip(Tlist, E0, Ix, Iy);
                Next_edge.Outp[CAbove] := E0.Outp[CAbove];
                Vertex(Next_edge, CAbove, CRight, Nx, Iy);
              end;
            vtILI:
              begin
                if (P <> nil) then
                begin
                  Vertex(E0, CAbove, CLeft, Ix, Iy);
                  N_EDGE(Next_edge, E1, CAbove, Nx, Iy);
                  Vertex(Next_edge, CAbove, CRight, Nx, Iy);
                  E1.Outp[CAbove] := E0.Outp[CAbove];
                  E0.Outp[CAbove] := nil;
                end;
              end;
            vtIRI:
              begin
                if (Q <> nil) then
                begin
                  Vertex(E1, CAbove, CRight, Ix, Iy);
                  P_EDGE(Prev_edge, E0, CAbove, Px, Iy);
                  Vertex(Prev_edge, CAbove, CLeft, Px, Iy);
                  E0.Outp[CAbove] := E1.Outp[CAbove];
                  E1.Outp[CAbove] := nil;
                end;
              end;
            vtIMX:
              begin
                if ((P <> nil) and (Q <> nil)) then
                begin
                  Vertex(E0, CAbove, CRight, Ix, Iy);
                  Vertex(E1, CAbove, CLeft, Ix, Iy);
                  E0.Outp[CAbove] := nil;
                  E1.Outp[CAbove] := nil;
                  P_EDGE(Prev_edge, E0, CAbove, Px, Iy);
                  Vertex(Prev_edge, CAbove, CLeft, Px, Iy);
                  NewTristrip(Tlist, Prev_edge, Px, Iy);
                  N_EDGE(Next_edge, E1, CAbove, Nx, Iy);
                  Vertex(Next_edge, CAbove, CRight, Nx, Iy);
                  Next_edge.Outp[CAbove] := Prev_edge.Outp[CAbove];
                  Vertex(Next_edge, CAbove, CRight, Nx, Iy);
                end;
              end;
            vtIMM:
              begin
                if ((P <> nil) and (Q <> nil)) then
                begin
                  Vertex(E0, CAbove, CRight, Ix, Iy);
                  Vertex(E1, CAbove, CLeft, Ix, Iy);
                  P_EDGE(Prev_edge, E0, CAbove, Px, Iy);
                  Vertex(Prev_edge, CAbove, CLeft, Px, Iy);
                  NewTristrip(Tlist, Prev_edge, Px, Iy);
                  N_EDGE(Next_edge, E1, CAbove, Nx, Iy);
                  Vertex(Next_edge, CAbove, CRight, Nx, Iy);
                  E1.Outp[CAbove] := Prev_edge.Outp[CAbove];
                  Vertex(E1, CAbove, CRight, Ix, Iy);
                  NewTristrip(Tlist, E0, Ix, Iy);
                  Next_edge.Outp[CAbove] := E0.Outp[CAbove];
                  Vertex(Next_edge, CAbove, CRight, Nx, Iy);
                end;
              end;
            vtEMM:
              begin
                if ((P <> nil) and (Q <> nil)) then
                begin
                  Vertex(E0, CAbove, CLeft, Ix, Iy);
                  NewTristrip(Tlist, E1, Ix, Iy);
                  E0.Outp[CAbove] := E1.Outp[CAbove];
                end;
              end;
          end; // * End of switch */
        end; // * End of contributing intersection conditional */

        // * Swap bundle sides in response to edge crossing */
        if (E0.Bundle[CAbove][GPC.CClip] <> 0) then
          E1.Bside[GPC.CClip] := Ord(E1.Bside[GPC.CClip] = 0);
        if (E1.Bundle[CAbove][GPC.CClip] <> 0) then
          E0.Bside[GPC.CClip] := Ord(E0.Bside[GPC.CClip] = 0);
        if (E0.Bundle[CAbove][GPC.CSubj] <> 0) then
          E1.Bside[GPC.CSubj] := Ord(E1.Bside[GPC.CSubj] = 0);
        if (E1.Bundle[CAbove][GPC.CSubj] <> 0) then
          E0.Bside[GPC.CSubj] := Ord(E0.Bside[GPC.CSubj] = 0);

        // * Swap e0 and e1 bundles in the AET */
        Prev_edge := E0.Prev;
        Next_edge := E1.Next;
        if (E1.Next <> nil) then
          E1.Next.Prev := E0;

        if (E0.Bstate[CAbove] = BUNDLE_HEAD) then
        begin
          Search := FTRUE;
          while (Search <> FFALSE) do
          begin
            Prev_edge := Prev_edge.Prev;
            if (Prev_edge <> nil) then
            begin
              if ((Prev_edge.Bundle[CAbove][GPC.CClip] <> 0) or
                (Prev_edge.Bundle[CAbove][GPC.CSubj] <> 0) or
                (Prev_edge.Bstate[CAbove] = BUNDLE_HEAD)) then
                Search := FFALSE;
            end
            else
              Search := FFALSE;
          end;
        end;
        if (Prev_edge = nil) then
        begin
          E1.Next := Aet;
          Aet := E0.Next;
        end
        else
        begin
          E1.Next := Prev_edge.Next;
          Prev_edge.Next := E0.Next;
        end;
        E0.Next.Prev := Prev_edge;
        E1.Next.Prev := E1;
        E0.Next := Next_edge;
        Intersect := Intersect.Next;
      end; // * End of IT loop*/

      // * Prepare for next scanbeam */
      Edge := Aet;
      while (Edge <> nil) do
      begin
        Next_edge := Edge.Next;
        Succ_edge := Edge.Succ;

        if ((Edge.Top.Y = Yt) and (Succ_edge <> nil)) then
        begin
          // * Replace AET edge by its successor */
          Succ_edge.Outp[CBelow] := Edge.Outp[CAbove];
          Succ_edge.Bstate[CBelow] := Edge.Bstate[CAbove];
          Succ_edge.Bundle[CBelow][GPC.CClip] := Edge.Bundle[CAbove][GPC.CClip];
          Succ_edge.Bundle[CBelow][GPC.CSubj] := Edge.Bundle[CAbove][GPC.CSubj];
          Prev_edge := Edge.Prev;
          if (Prev_edge <> nil) then
            Prev_edge.Next := Succ_edge
          else
            Aet := Succ_edge;
          if (Next_edge <> nil) then
            Next_edge.Prev := Succ_edge;
          Succ_edge.Prev := Prev_edge;
          Succ_edge.Next := Next_edge;
        end
        else
        begin
          // * Update this edge */
          Edge.Outp[CBelow] := Edge.Outp[CAbove];
          Edge.Bstate[CBelow] := Edge.Bstate[CAbove];
          Edge.Bundle[CBelow][GPC.CClip] := Edge.Bundle[CAbove][GPC.CClip];
          Edge.Bundle[CBelow][GPC.CSubj] := Edge.Bundle[CAbove][GPC.CSubj];
          Edge.Xb := Edge.Xt;
        end;
        Edge.Outp[CAbove] := nil;
        Edge := Next_edge;
      end;
    end;
  end; // * === END OF SCANBEAM PROCESSING ================================== */

  // * Generate Result TriStrip from tlist */
  Result.Strip := nil;
  Result.NumStrips := CountTristrips(Tlist);
  if (Result.NumStrips > 0) then
  begin
    MALLOC(Pointer(Result.Strip), Result.NumStrips * Sizeof(TGpcVertexList),
      'tristrip list creation');

    S := 0;
    Tn := Tlist;
    while (Tn <> nil) do
    begin
      Tnn := Tn.Next;

      if (Tn.Active > 2) then
      begin
        // * Valid TriStrip: copy the vertices and Free the heap */
        Result.Strip[S].NumVertices := Tn.Active;
        MALLOC(Pointer(Result.Strip[S].Vertex), Tn.Active * Sizeof(TGpcVertex),
          'tristrip creation');
        V := 0;
        if (CInvertTriStrips <> 0) then
        begin
          Lt := Tn.V[CRight];
          Rt := Tn.V[CLeft];
        end
        else
        begin
          Lt := Tn.V[CLeft];
          Rt := Tn.V[CRight];
        end;
        while ((Lt <> nil) or (Rt <> nil)) do
        begin
          if (Lt <> nil) then
          begin
            Ltn := Lt.Next;
            Result.Strip[S].Vertex[V].X := Lt.X;
            Result.Strip[S].Vertex[V].Y := Lt.Y;
            Inc(V);
            Free(Pointer(Lt));
            Lt := Ltn;
          end;
          if (Rt <> nil) then
          begin
            Rtn := Rt.Next;
            Result.Strip[S].Vertex[V].X := Rt.X;
            Result.Strip[S].Vertex[V].Y := Rt.Y;
            Inc(V);
            Free(Pointer(Rt));
            Rt := Rtn;
          end;
        end;
        Inc(S);
      end
      else
      begin
        // * Invalid TriStrip: just Free the heap */
        Lt := Tn.V[CLeft];
        while (Lt <> nil) do
        begin
          Ltn := Lt.Next;
          Free(Pointer(Lt));
          Lt := Ltn
        end;
        Rt := Tn.V[CRight];
        while (Rt <> nil) do
        begin
          Rtn := Rt.Next;
          Free(Pointer(Rt));
          Rt := Rtn
        end;
      end;
      Free(Pointer(Tn));

      Tn := Tnn;
    end;
  end;

  // * Tidy up */
  Reset_it(It);
  ResetLocalMinimaTable(LocalMinimaTable);
  Free(Pointer(C_heap));
  Free(Pointer(S_heap));
  Free(Pointer(Sbt));
end;

end.
