unit RedFox.NodeEditor.NodeEditDisplay;

interface

uses
  Types, Classes, Controls,
  RedFox,
  RedFoxCustomControl,
  RedFox.NodeEditor.Base;

type
  TGetFunctionValueAtX = function(const PixelPosX:single):single of object;
  TCheckNodePosition = function(const NodeIndex : integer; const OriginalPosition, TargetPosition:TPointF):TPointF of object;
  TCanDeleteNode = procedure(const NodeIndex : integer; var Allowed:boolean) of object;

  TGrabbedNodeDetails = record
    Index : integer; //Node Index
    Offset : TPointF;
    OriginalPosition : TPointF;
  end;


  TRedFoxNodeEditDisplay = class(TRedFoxCustomControl)
  private
    fNodes: TRedFoxNodeList;
    fNodeDisplayLimits: TRectF;
    fOnNewNodePosition: TCheckNodePosition;
    fOnGetFunctionValueAtX: TGetFunctionValueAtX;
    fOnCanDeleteNode: TCanDeleteNode;
    procedure SetNodeDisplayLimits(const Value: TRectF);
    function GetNodeColor: TRedFoxColorString;
    function GetNodeColorMouseOver: TRedFoxColorString;
    procedure SetNodeColor(const Value: TRedFoxColorString);
    procedure SetNodeColorMouseOver(const Value: TRedFoxColorString);
    function GetLineColor: TRedFoxColorString;
    procedure SetLineColor(const Value: TRedFoxColorString);
    function GetNodeColorGrabbed: TRedFoxColorString;
    procedure SetNodeColorGrabbed(const Value: TRedFoxColorString);
  protected
    Colors : record
      NodeHandle          : TRedFoxColor;
      NodeHandleMouseOver : TRedFoxColor;
      NodeHandleGrabbed   : TRedFoxColor;
      Line : TRedFoxColor;
    end;

    DisplayNodeCount : integer;
    NodeDisplayPositions : array of TPointF;
    NodeCurveHandlePositions : array of TPointF;

    IsNodeMouseOver : boolean;
    MouseOverNodeIndex : integer;
    LastMousePos : TPointF;

    IsNodeGrabbed : boolean;
    GrabbedNode : TGrabbedNodeDetails;

    function CheckNewNodePosition(const NodeIndex : integer; const OriginalPosition, TargetPosition:TPointF):TPointF;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    procedure DblClick; override;

    function CanDeleteNode(const NodeIndex : integer):boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Nodes : TRedFoxNodeList read fNodes write fNodes;

    procedure DoPaintBuffer; override;

    property NodeDisplayLimits : TRectF read fNodeDisplayLimits;

    function NodeToDisplayPos(aPoint:TPointF):TPointF;
    function DisplayToNodePos(aPoint:TPointF):TPointF;

    function GetNodeIndexAt(const DisplayX, DisplayY:single):integer;
  published
    property LineColor          : TRedFoxColorString read GetLineColor          write SetLineColor;
    property NodeColor          : TRedFoxColorString read GetNodeColor          write SetNodeColor;
    property NodeColorMouseOver : TRedFoxColorString read GetNodeColorMouseOver write SetNodeColorMouseOver;
    property NodeColorGrabbed   : TRedFoxColorString read GetNodeColorGrabbed   write SetNodeColorGrabbed;

    property OnDblClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;

    property OnNewNodePosition : TCheckNodePosition read fOnNewNodePosition write fOnNewNodePosition;
    property OnGetFunctionValueAtX : TGetFunctionValueAtX read fOnGetFunctionValueAtX write fOnGetFunctionValueAtX;

    property OnCanDeleteNode : TCanDeleteNode read fOnCanDeleteNode write fOnCanDeleteNode;
  end;

implementation

const
  kNodePixelWidth = 5;

{ TRedFoxNodeEditDisplay }

constructor TRedFoxNodeEditDisplay.Create(AOwner: TComponent);
begin
  inherited;

  Nodes := TRedFoxNodeList.Create;
  Nodes.OwnsObjects := true;

  fNodeDisplayLimits.Left   := 0;
  fNodeDisplayLimits.Right  := 1;
  fNodeDisplayLimits.Top    := 1;
  fNodeDisplayLimits.Bottom := -1;

  MouseOverNodeIndex := -1;

  IsNodeGrabbed := false;
  IsNodeMouseOver := false;

  //Colors.NodeHandle.SetColor(0,0,255,100);
  Colors.NodeHandle.SetColor('$0000FF99');
end;

destructor TRedFoxNodeEditDisplay.Destroy;
begin
  setLength(NodeDisplayPositions, 0);
  setLength(NodeCurveHandlePositions, 0);
  Nodes.Free;
  inherited;
end;

function TRedFoxNodeEditDisplay.NodeToDisplayPos(aPoint: TPointF): TPointF;
begin
  result.X := (aPoint.X - fNodeDisplayLimits.Left) / (fNodeDisplayLimits.Right - fNodeDisplayLimits.Left) * Width;
  result.Y := Height - ((aPoint.Y - fNodeDisplayLimits.Bottom) / (fNodeDisplayLimits.Top - fNodeDisplayLimits.Bottom) * Height);
end;

function TRedFoxNodeEditDisplay.DisplayToNodePos(aPoint: TPointF): TPointF;
begin
  result.X := (aPoint.X / Width) * (fNodeDisplayLimits.Right - fNodeDisplayLimits.Left) + fNodeDisplayLimits.Left;
  result.Y := ((Height - aPoint.Y) / Height) * (fNodeDisplayLimits.Top - fNodeDisplayLimits.Bottom) + fNodeDisplayLimits.Bottom;
end;



procedure TRedFoxNodeEditDisplay.SetLineColor(const Value: TRedFoxColorString);
begin
  Colors.Line.SetColor(Value);
  Invalidate;
end;

procedure TRedFoxNodeEditDisplay.SetNodeColor(const Value: TRedFoxColorString);
begin
  Colors.NodeHandle.SetColor(Value);
  Invalidate;
end;

procedure TRedFoxNodeEditDisplay.SetNodeColorGrabbed(const Value: TRedFoxColorString);
begin
  Colors.NodeHandleGrabbed.SetColor(Value);
  Invalidate;
end;

procedure TRedFoxNodeEditDisplay.SetNodeColorMouseOver(const Value: TRedFoxColorString);
begin
  Colors.NodeHandleMouseOver.SetColor(Value);
  Invalidate;
end;

procedure TRedFoxNodeEditDisplay.SetNodeDisplayLimits(const Value: TRectF);
begin
  fNodeDisplayLimits := Value;
  Invalidate;
end;


procedure TRedFoxNodeEditDisplay.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pt : TPointF;
  NodeIndex : integer;
begin
  inherited;

  if (Button = TMouseButton.mbLeft) and ((ssCtrl in Shift) = false) and (IsNodeMouseOver) then
  begin
    IsNodeGrabbed := true;
    GrabbedNode.Index := MouseOverNodeIndex;
    GrabbedNode.Offset.X := X - NodeDisplayPositions[GrabbedNode.Index].X;
    GrabbedNode.Offset.Y := Y - NodeDisplayPositions[GrabbedNode.Index].Y;
    GrabbedNode.OriginalPosition := Nodes[GrabbedNode.Index].Position;

    Invalidate;
    Repaint;
  end;

  if (Button = TMouseButton.mbRight) and (IsNodeMouseOver = false) then
  begin
    pt.X := X;
    pt.Y := y;
    pt := DisplayToNodePos(pt);
    NodeIndex := Nodes.InsetNodeAt(pt.X, pt.Y);
    pt := CheckNewNodePosition(NodeIndex, pt, pt);
    Nodes[NodeIndex].X := pt.X;
    Nodes[NodeIndex].Y := pt.Y;

    Invalidate;
    Repaint;
  end;

  if (Button = TMouseButton.mbRight) and (IsNodeMouseOver = true) then
  begin
    NodeIndex := GetNodeIndexAt(X, Y);
    if CanDeleteNode(NodeIndex) then Nodes.Delete(NodeIndex);

    Invalidate;
    Repaint;
  end;

end;

procedure TRedFoxNodeEditDisplay.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  c1:integer;
  Index : integer;
  NewNodePos : TPointF;
begin
  inherited;

  LastMousePos.X := X;
  LastMousePos.Y := Y;

  if IsNodeGrabbed = false then
  begin
    //=== Check to see if the mouse is over any nodes ====
    Index := -1;

    for c1 := 0 to DisplayNodeCount-1 do
    begin
      if DistanceBetweenPoints(NodeDisplayPositions[c1].X, NodeDisplayPositions[c1].Y, x, y) < kNodePixelWidth then
      begin
        Index := c1;
        break;
      end;
    end;

    if MouseOverNodeIndex <> Index then
    begin
      if Index <> - 1
        then IsNodeMouseOver := true
        else IsNodeMouseOver := false;

      MouseOverNodeIndex := Index;

      Invalidate;
    end;
  end;


  if IsNodeGrabbed = true then
  begin
    NewNodePos.X := x - GrabbedNode.Offset.X;
    NewNodePos.Y := y - GrabbedNode.Offset.Y;
    NewNodePos := self.DisplayToNodePos(NewNodePos);

    NewNodePos := CheckNewNodePosition(GrabbedNode.Index, GrabbedNode.OriginalPosition, NewNodePos);


    Nodes[GrabbedNode.Index].Position := NewNodePos;



    Invalidate;
  end;

end;

procedure TRedFoxNodeEditDisplay.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = TMouseButton.mbLeft) and (IsNodeGrabbed) then
  begin
    IsNodeGrabbed := false;
    Invalidate;
  end;

end;



procedure TRedFoxNodeEditDisplay.DblClick;
var
  pt : TPointF;
  NodeIndex : integer;
begin
  inherited;

  NodeIndex := GetNodeIndexAt(LastMousePos.X, LastMousePos.Y);

  if NodeIndex = -1 then
  begin
    //Create new node
    pt := DisplayToNodePos(LastMousePos);
    NodeIndex := Nodes.InsetNodeAt(pt.X, pt.Y);
    pt := CheckNewNodePosition(NodeIndex, pt, pt);
    Nodes[NodeIndex].X := pt.X;
    Nodes[NodeIndex].Y := pt.Y;

    Invalidate;
    Repaint;
  end;

  if NodeIndex <> -1 then
  begin
    //Delete the double clicked node.
    if CanDeleteNode(NodeIndex) then Nodes.Delete(NodeIndex);

    Invalidate;
    Repaint;
  end;



end;



function TRedFoxNodeEditDisplay.CanDeleteNode(const NodeIndex: integer): boolean;
var
  IsDeleteAllowed : boolean;
begin
  IsDeleteAllowed := true;
  if assigned(OnCanDeleteNode) then OnCanDeleteNode(NodeIndex, IsDeleteAllowed);
  result := IsDeleteAllowed;
end;

function TRedFoxNodeEditDisplay.CheckNewNodePosition(const NodeIndex : integer; const OriginalPosition, TargetPosition: TPointF): TPointF;
var
  TempTargetPosition : TPointF;
  OtherNodePosition : TPointF;
begin
  TempTargetPosition := TargetPosition;

  //=== Limit node position to node display boundarys ===
  if TempTargetPosition.X < NodeDisplayLimits.Left  then TempTargetPosition.X := NodeDisplayLimits.Left;
  if TempTargetPosition.X > NodeDisplayLimits.Right then TempTargetPosition.X := NodeDisplayLimits.Right;

  if TempTargetPosition.Y > NodeDisplayLimits.Top  then TempTargetPosition.Y := NodeDisplayLimits.Top;
  if TempTargetPosition.Y < NodeDisplayLimits.Bottom then TempTargetPosition.Y := NodeDisplayLimits.Bottom;


  //=== Limit node position to node display boundarys ===
  if (NodeIndex > 0) then
  begin
    OtherNodePosition := Nodes[NodeIndex-1].Position;
    if TempTargetPosition.X < OtherNodePosition.X then TempTargetPosition.X := OtherNodePosition.X;
  end;

  if (NodeIndex < Nodes.Count - 1) then
  begin
    OtherNodePosition := Nodes[NodeIndex+1].Position;
    if TempTargetPosition.X > OtherNodePosition.X then TempTargetPosition.X := OtherNodePosition.X;
  end;


  //=== Call the NewNodePosition event to allow the application developer to add additional handling ===
  if assigned(OnNewNodePosition) then TempTargetPosition := OnNewNodePosition(NodeIndex, OriginalPosition, TempTargetPosition);

  result := TempTargetPosition;
end;







function TRedFoxNodeEditDisplay.GetLineColor: TRedFoxColorString;
begin
  result := Colors.Line.AsString;
end;

function TRedFoxNodeEditDisplay.GetNodeColor: TRedFoxColorString;
begin
  result := Colors.NodeHandle.AsString;
end;

function TRedFoxNodeEditDisplay.GetNodeColorGrabbed: TRedFoxColorString;
begin
  result := Colors.NodeHandleGrabbed.AsString;
end;

function TRedFoxNodeEditDisplay.GetNodeColorMouseOver: TRedFoxColorString;
begin
  result := Colors.NodeHandleMouseOver.AsString;
end;

function TRedFoxNodeEditDisplay.GetNodeIndexAt(const DisplayX, DisplayY: single): integer;
var
  c1 : integer;
begin
  for c1 := 0 to DisplayNodeCount-1 do
  begin
    if DistanceBetweenPoints(NodeDisplayPositions[c1].X, NodeDisplayPositions[c1].Y, DisplayX, DisplayY) < kNodePixelWidth then
    begin
      result := c1;
      exit; //===================>> exit >>=============>>
    end;
  end;

  //If we've made it this far, no node has been found.
  result := -1;

end;

procedure TRedFoxNodeEditDisplay.DoPaintBuffer;
var
  c1: Integer;
  aPoint : TPointF;
  pt1, pt2 : TPointF;
  HandleRect : TRectF;
  xValue, yValue : single;
begin
  //=== Clear the back Buffer ====
  BackBuffer.RedFoxInterface.ClearAll(255,255,255,0);

  //Translate the node position to display drawing positions.
  DisplayNodeCount := Nodes.Count;
  SetLength(NodeDisplayPositions, Nodes.Count);
  SetLength(NodeCurveHandlePositions, Nodes.Count);
  for c1 := 0 to Nodes.Count-1 do
  begin
    aPoint := Nodes[c1].Position;
    NodeDisplayPositions[c1] := NodeToDisplayPos(aPoint);
  end;

  if not assigned(OnGetFunctionValueAtX) then
  begin
    //Draw the lines between node positions.
    BackBuffer.BufferInterface.LineColor := Colors.Line.AsAggRgba8;
    BackBuffer.BufferInterface.LineWidth := 2;

    for c1 := 1 to Nodes.Count-1 do
    begin
      pt1 := NodeDisplayPositions[c1-1];
      pt2 := NodeDisplayPositions[c1];

      self.BackBuffer.BufferInterface.Line(pt1.X, pt1.Y, pt2.X, pt2.Y);
    end;
  end;

  if assigned(OnGetFunctionValueAtX) then
  begin
    //=== Draw interpolated Line ===
    BackBuffer.BufferInterface.LineColor := Colors.Line.AsAggRgba8;
    BackBuffer.BufferInterface.NoFill;
    BackBuffer.BufferInterface.LineWidth := 2;

    pt1.X := 0;
    pt1.Y := OnGetFunctionValueAtX(0);

    for c1 := 1 to self.Width do
    begin
      pt2.X := c1;
      pt2.Y := OnGetFunctionValueAtX(c1);

      self.BackBuffer.BufferInterface.Line(pt1.X, pt1.Y, pt2.X, pt2.Y);

      pt1 := pt2;
    end;
  end;



  //Draw the node handles.
  //BackBuffer.BufferInterface.SetLineColor(0,0,0,255);
  //BackBuffer.BufferInterface.SetFillColor(0,0,0,255);
  BackBuffer.BufferInterface.LineColor := Colors.NodeHandle.AsAggRgba8;
  BackBuffer.BufferInterface.FillColor := Colors.NodeHandle.AsAggRgba8;

  for c1 := 0 to Nodes.Count-1 do
  begin
    HandleRect.Left   := NodeDisplayPositions[c1].X - 2.5;
    HandleRect.Right  := NodeDisplayPositions[c1].X + 2.5;
    HandleRect.Top    := NodeDisplayPositions[c1].Y - 2.5;
    HandleRect.Bottom := NodeDisplayPositions[c1].Y + 2.5;

    BackBuffer.BufferInterface.Rectangle(HandleRect.Left, HandleRect.Top, HandleRect.Right, HandleRect.Bottom);
  end;


  //=== Draw the mouse over node ===

  if IsNodeMouseOver then
  begin
    HandleRect.Left   := NodeDisplayPositions[MouseOverNodeIndex].X - 2.5;
    HandleRect.Right  := NodeDisplayPositions[MouseOverNodeIndex].X + 2.5;
    HandleRect.Top    := NodeDisplayPositions[MouseOverNodeIndex].Y - 2.5;
    HandleRect.Bottom := NodeDisplayPositions[MouseOverNodeIndex].Y + 2.5;

    BackBuffer.BufferInterface.LineColor := Colors.NodeHandleMouseOver.AsAggRgba8;
    BackBuffer.BufferInterface.FillColor := Colors.NodeHandleMouseOver.AsAggRgba8;

    BackBuffer.BufferInterface.Rectangle(HandleRect.Left, HandleRect.Top, HandleRect.Right, HandleRect.Bottom);
  end;


  if IsNodeGrabbed then
  begin
    HandleRect.Left   := NodeDisplayPositions[GrabbedNode.Index].X - 2.5;
    HandleRect.Right  := NodeDisplayPositions[GrabbedNode.Index].X + 2.5;
    HandleRect.Top    := NodeDisplayPositions[GrabbedNode.Index].Y - 2.5;
    HandleRect.Bottom := NodeDisplayPositions[GrabbedNode.Index].Y + 2.5;

    BackBuffer.BufferInterface.LineColor := Colors.NodeHandleGrabbed.AsAggRgba8;
    BackBuffer.BufferInterface.FillColor := Colors.NodeHandleGrabbed.AsAggRgba8;

    BackBuffer.BufferInterface.Rectangle(HandleRect.Left, HandleRect.Top, HandleRect.Right, HandleRect.Bottom);
  end;
end;

end.
