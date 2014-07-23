unit VamTreeView;

interface

uses
  Types,
  Graphics,
  Contnrs,
  Classes, Controls, WinApi.Messages,
  Agg2D, AggColor,
  RedFoxImageBuffer,
  VamTreeViewNode,
  VamCustomTreeView;


type
  TNodeEvent = VamCustomTreeView.TNodeEvent;
  TBooleanNodeEvent = VamCustomTreeView.TNodeEvent;

  PVamTreeViewNode = VamTreeViewNode.PVamTreeViewNode;
  TVamTreeViewNode = VamTreeViewNode.TVamTreeViewNode;

  TNodeBitmapEvent = procedure(Sender:TObject; Node:TVamTreeViewNode; var Bitmap:TBitmap) of object;

  TBitmapEvent = procedure(Sender:TObject; var Bitmap:TBitmap; var Src:TRect) of object;

  TETOption = (etoMultiSelect, etoAutoExpand, etoAutoSelect);
  TETOptions = set of TETOption;

  TVamTreeView = class(TVamCustomTreeView)
  private
    fDefaultNodeHeight: integer;
    fChildIndent: integer;
    fStagingBuffer: TRedFoxImageBuffer;
    fOptions: TETOptions;
    fRootIndent: integer;
    fSelectedList: TObjectList;
    fFocusedNode: TVamTreeViewNode;
    fFrom:TVamTreeViewNode;
    fTreeOffsetX: integer;
    fTreeOffsetY: integer;
    fScrollRangeX: single;
    fScrollRangeY: single;
    fOnScrollXChange: TNotifyEvent;
    fOnScrollYChange: TNotifyEvent;
    fOnGetNodeBitmap: TNodeBitmapEvent;
    fBufferNeedsUpdate: boolean;
    fOnGetBackground: TBitmapEvent;
    fSelectedNodeColor: TColor;
    fSelectedNodeAlpha: byte;
    fOnBeginNodeDrag: TNotifyEvent;
    fOnTreeRightClicked: TNotifyEvent;
    fOnNodeRightClicked: TNodeEvent;
    procedure SetOptions(const Value: TETOptions);
    procedure SetTreeOffsetX(Value: integer);
    procedure SetTreeOffsetY(Value: integer);
    procedure SetScrollPosX(Value: single);
    procedure SetScrollPosY(Value: single);
    function GetScrollPosX: single;
    function GetScrollPosY: single;
    procedure SetFocusedNode(const Value: TVamTreeViewNode);
    function GetSelectedCount: integer;
    procedure SetSelectedNodeColor(const Value: TColor);
    procedure SetSelectedNodeAlpha(const Value: byte);
    procedure SetDefaultNodeHeight(const Value: integer);

    //procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    //procedure CMMouseWheel(var Message: TCMMouseWheel); message CM_MOUSEWHEEL;
    //procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;
  protected
    WatchForDrag : boolean;
    IsDragActive : boolean;
    MouseDownPos : TPoint;
    LeftClickedNode : TVamTreeViewNode;
    RightClickedNode : TVamTreeViewNode;

    function FindNodeWithTopValue(NodeTopValue:integer):TVamTreeViewNode;
    procedure GetBackground(var Background:TBitmap; var Src:TRect);
    function GetNodeBitmap(aNode:TVamTreeViewNode):TBitmap;
    procedure ScrollXChange;
    procedure ScrollYChange;



    procedure SelectVisibleNodesBetween(NodeA, NodeB:TVamTreeViewNode);

    procedure MouseWheelUp(Shift : TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;
    procedure MouseWheelDown(Shift : TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DoNodeMultiSelect(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    property StagingBuffer:TRedFoxImageBuffer read fStagingBuffer write fStagingBuffer;
    //property BufferInterface:TAgg2D read fBufferInterface write fBufferInterface;
    property SelectedList:TObjectList read fSelectedList write fSelectedList;
    property BufferNeedsUpdate:boolean read fBufferNeedsUpdate write fBufferNeedsUpdate;
    property From:TVamTreeViewNode read fFrom write fFrom;


    //==== Drawing Methods =====================================================
    procedure Paint; override;

    procedure DrawTreeView(const DestBuffer : TRedFoxImageBuffer);

    procedure RenderNode(const DestBuffer : TRedFoxImageBuffer; const Node:TVamTreeViewNode; var vOffset, hOffset:integer);
    //==========================================================================
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CalcTreeDimensions; override;

    procedure SetFontSize(Size:integer);

    procedure PageDown;
    procedure PageUp;
    procedure SelectDown;
    procedure SelectUp;

    //procedure MouseWheelHandler(var Message: TMessage); override;

    procedure SelectNone;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    procedure MoveTreeUp;
    procedure MoveTreeDown;
    procedure ScrollIntoView(Node:TVamTreeViewNode);
    function IsNodeInView(Node:TVamTreeViewNode):boolean;

    function GetNodeHeight(aNode:TVamTreeViewNode):integer;
    function GetNodeWidth(aNode:TVamTreeViewNode):integer;
    function GetNodeAt(x,y:integer):TVamTreeViewNode;

    function GetFirstSelected:TVamTreeViewNode;
    function GetSelected(Index:integer):TVamTreeViewNode;

    procedure SaveToStream(Stream:TStream); override;
    procedure LoadFromStream(Stream:TStream); override;

    property TreeOffsetX :integer read fTreeOffsetX write SetTreeOffsetX;
    property TreeOffsetY :integer read fTreeOffsetY write SetTreeOffsetY;

    property ScrollPosX   :single read GetScrollPosX write SetScrollPosX;
    property ScrollPosY   :single read GetScrollPosY write SetScrollPosY;
    property ScrollRangeX :single read fScrollRangeX write fScrollRangeX;
    property ScrollRangeY :single read fScrollRangeY write fScrollRangeY;

    property FocusedNode :TVamTreeViewNode read fFocusedNode write SetFocusedNode;
    property Selected[Index:integer] : TVamTreeViewNode read GetSelected;
    property SelectedCount :integer read GetSelectedCount;
  published
    property SelectedNodeColor :TColor read fSelectedNodeColor write SetSelectedNodeColor;
    property SelectedNodeAlpha :byte  read fSelectedNodeAlpha  write SetSelectedNodeAlpha;

    property ChildIndent       :integer    read fChildIndent       write fChildIndent;
    property DefaultNodeHeight :integer    read fDefaultNodeHeight write SetDefaultNodeHeight;
    property Options           :TETOptions read fOptions           write SetOptions;
    property RootIndent        :integer    read fRootIndent        write fRootIndent;

    property OnGetBackground :TBitmapEvent     read fOnGetBackground write fOnGetBackground;
    property OnGetNodeBitmap :TNodeBitmapEvent read fOnGetNodeBitmap write fOnGetNodeBitmap;
    property OnScrollXChange :TNotifyEvent     read fOnScrollXChange write fOnScrollXChange;
    property OnScrollYChange :TNotifyEvent     read fOnScrollYChange write fOnScrollYChange;

    property OnBeginNodeDrag : TNotifyEvent read fOnBeginNodeDrag write fOnBeginNodeDrag;


    property OnNodeRightClicked : TNodeEvent read fOnNodeRightClicked write fOnNodeRightClicked;
    property OnTreeRightClicked : TNotifyEvent read fOnTreeRightClicked write fOnTreeRightClicked;

    property Font;

    {$INCLUDE TControlProperties.inc}
  end;

implementation

uses
  SysUtils, Dialogs, RedFoxColor, AggPixelFormat, RedFox;

function ToPAnsiChar(Text:string):PAnsiChar;
begin
  result := PAnsiChar(PAnsiString(AnsiString(Text)));
end;



{ TEasyTreeView }

{
procedure TEasyTreeView.CMMouseEnter(var Message: TMessage);
begin

end;

procedure TEasyTreeView.CMMouseWheel(var Message: TCMMouseWheel);
begin

end;

procedure TEasyTreeView.WMMouseWheel(var Message: TWMMouseWheel);
begin


end;
}


constructor TVamTreeView.Create(AOwner: TComponent);
begin
  inherited;

  fTreeOffsetX := 0;
  fTreeOffsetY := 0;

  StagingBuffer := TRedFoxImageBuffer.Create;
  StagingBuffer.Width := 10;
  StagingBuffer.Height := 10;


  BufferNeedsUpdate := false;

  DefaultNodeHeight := 16;
  ChildIndent := 12;
  RootIndent := 4;

  SelectedList := TObjectList.Create;
  SelectedList.OwnsObjects := false;

  FocusedNode := nil;

  SelectedNodeColor := clBlack;
  SelectedNodeAlpha := 35;

end;

destructor TVamTreeView.Destroy;
begin
  SelectedList.Free;
  StagingBuffer.Free;
  inherited;
end;

function TVamTreeView.GetNodeHeight(aNode: TVamTreeViewNode): integer;
var
  c1, nh:integer;
begin
  nh := DefaultNodeHeight;

  if (aNode.HasChildren) and (aNode.Expanded) then
  begin
    for c1 := 0 to aNode.ChildCount - 1 do
    begin
      nh := nh + GetNodeHeight(aNode.Child[c1]);
    end;
  end;

  result := nh;
end;

function TVamTreeView.GetNodeWidth(aNode: TVamTreeViewNode): integer;
var
  c1 : integer;
  tw, nw:double;
begin
  BackBuffer.UpdateFont(Font);

  nw := RootIndent + (ChildIndent * aNode.Depth);
  //TODO: BUG: There is a problem with this line. When running on the host OS it
  // returns a incorrect value.

  nw := nw + BackBuffer.TextWidth(aNode.Caption);
  nw := nw + DefaultNodeHeight + 4;  //Take the node bitmap into account.

  if (aNode.HasChildren) and (aNode.Expanded) then
  begin
    for c1 := 0 to aNode.ChildCount - 1 do
    begin
      tw := GetNodeWidth(aNode.Child[c1]);
      if tw > nw then nw := tw;
    end;
  end;

  //TODO: Change GetNodeWidth result type to double to avoid the round() call.
  result := round(nw);
end;



function TVamTreeView.GetScrollPosX: single;
var
  sp : single;
begin
  if TreeWidth <= Width then
  begin
    result := 0
  end else
  begin
    sp := TreeOffsetX;
    result := (-sp) / (TreeWidth - Width);
  end;

  assert(result >= 0);
  assert(result <= 1);
end;

function TVamTreeView.GetScrollPosY: single;
var
  sp : single;
begin
  if TreeHeight <= Height then
  begin
    result := 0;
  end else
  begin
    sp := TreeOffsetY;
    result := (-sp) / (TreeHeight - Height);
  end;

  assert(result >= 0);
  assert(result <= 1);
end;

function TVamTreeView.GetSelected(Index: integer): TVamTreeViewNode;
begin
  if (SelectedList.Count = 0) and (Index = 0) then
  begin
    result := FocusedNode;
  end else
  begin
    if SelectedList.Count <= Index
      then result := nil
      else result := SelectedList[Index] as TVamTreeViewNode;
  end;

end;

function TVamTreeView.GetSelectedCount: integer;
begin
  result := 0;

  if (SelectedList.Count = 0) and (FocusedNode = nil)
    then result := 0;

  if (SelectedList.Count = 0) and (FocusedNode <> nil)
    then result := 1;

  if (SelectedList.Count > 0)
    then result := SelectedList.Count;


end;

function TVamTreeView.IsNodeInView(Node: TVamTreeViewNode): boolean;
begin
  if (Node.Top < 0) or (Node.Top + DefaultNodeHeight > Height)
    then result := false
    else result := true;

end;

procedure TVamTreeView.MouseWheelDown(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  //TreeOffsetY := TreeOffsetY - DefaultNodeHeight;
end;

procedure TVamTreeView.MouseWheelUp(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  //TreeOffsetY := TreeOffsetY + DefaultNodeHeight;
end;

procedure TVamTreeView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  //aNode : TVamTreeViewNode;
  ClickedNode : TVamTreeViewNode;
  ModifiersPresent : boolean;
begin
  WatchForDrag := false;
  MouseDownPos := Point(x, y);

  if (ssShift in Shift) or (ssCtrl in Shift)
    then ModifiersPresent := true
    else ModifiersPresent := false;

  //==================================================
  //  Left Button
  //==================================================
  if Button = mbLeft then
  begin
    IsDragActive := false;
    LeftClickedNode := nil;
    ClickedNode := GetNodeAt(x,y);

    if ClickedNode = nil then
    begin
      FocusedNode := nil;
      BufferNeedsUpdate := true;
      Invalidate;
    end else
    if (ClickedNode <> nil) and (ModifiersPresent = false) then
    begin
      WatchForDrag := true;
      LeftClickedNode := ClickedNode;
      FocusedNode := ClickedNode;

      BufferNeedsUpdate := true;
      Invalidate;
    end else
    if (ClickedNode <> nil) and (ssShift in Shift) then
    begin
      if assigned(From)
        then SelectVisibleNodesBetween(From,ClickedNode)
        else FocusedNode := ClickedNode;
      BufferNeedsUpdate := true;
      Invalidate;
    end else
    if (ClickedNode <> nil) and (ssCtrl in Shift) then
    begin
      SelectedList.Add(ClickedNode);
      FocusedNode := ClickedNode;
      BufferNeedsUpdate := true;
      Invalidate;
    end;

  end;


  //==================================================
  //  Right Button
  //==================================================
  if Button= mbRight then
  begin
    ClickedNode := GetNodeAt(x,y);
    if ClickedNode = nil then
    begin
      SelectedList.Clear;
      FocusedNode := nil;
      BufferNeedsUpdate := true;
      Invalidate;

      if assigned(OnTreeRightClicked) then OnTreeRightClicked(self);
    end;

    if ClickedNode <> nil then
    begin
      SelectedList.Clear;
      FocusedNode := ClickedNode;
      BufferNeedsUpdate := true;
      Invalidate;

      if assigned(OnNodeRightClicked) then OnNodeRightClicked(self, ClickedNode);
    end;

  end;




  {
  if (etoMultiSelect in Options) and (etoAutoSelect in Options) and ((ssShift in Shift) or (ssAlt in Shift)) then
  begin
    DoNodeMultiSelect(Button, Shift, X, Y);
  end else
  begin
    aNode := GetNodeAt(x,y);

    if (Button = mbLeft) and (etoAutoSelect in Options) then
    begin
      if (etoAutoExpand in Options) and (aNode <> nil) then
      begin
        if (aNode.Expanded = false)
          then DoExpand(aNode)
          else DoCollapse(aNode);

        Invalidate;
      end;

      Selected := aNode;
      Invalidate;
    end;
  end;
  }


  inherited;
end;

procedure TVamTreeView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (WatchForDrag) and ((abs(MouseDownPos.X-X) > 10) or (abs(MouseDownPos.Y-Y) > 10)) then
  begin
    WatchForDrag := false;
    IsDragActive := true;
    if assigned(OnBeginNodeDrag) then OnBeginNodeDrag(self);
  end;


end;

procedure TVamTreeView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button = mbLeft) and (assigned(LeftClickedNode)) and (IsDragActive = false) then
  begin
    WatchForDrag := false;
    FocusedNode := nil;
    FocusedNode := LeftClickedNode;

    if LeftClickedNode.Expanded = false
      then DoExpand(LeftClickedNode)
      else DoCollapse(LeftClickedNode);

    BufferNeedsUpdate := true;
    Invalidate;
  end;

end;



procedure TVamTreeView.DoNodeMultiSelect(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ClickedNode:TVamTreeViewNode;
  InList:boolean;
  NoModifiers:boolean;
begin
  ClickedNode := GetNodeAt(x,y);

  if ClickedNode = nil then
  begin
    SelectedList.Clear;
    FocusedNode := nil;
    BufferNeedsUpdate := true;
    Invalidate;
    exit; //  <----------------------------------------------
  end;

  if SelectedList.IndexOf(ClickedNode) = -1
    then InList := false
    else InList := true;

  if (not(ssShift in Shift)) and (not(ssCtrl in Shift))
    then NoModifiers := true
    else NoModifiers := false;


  //Clicked node is NOT already selected....

  if (InList = false) and (Button = mbRight) then
  begin
    SelectedList.Clear;
    FocusedNode := ClickedNode;
    BufferNeedsUpdate := true;
    Invalidate;
  end;

  if (InList = false) and (Button = mbLeft) then
  begin
    //Left button, not selected.
    if (NoModifiers) then
    begin
      SelectedList.Clear;

      SelectedList.Add(ClickedNode);
      FocusedNode := ClickedNode;

      if (etoAutoExpand in Options) then
      begin
        if (ClickedNode.Expanded = false)
          then DoExpand(ClickedNode)
          else DoCollapse(ClickedNode);
      end;

      BufferNeedsUpdate := true;
      Invalidate;
    end;

    //Left button, not selected.
    if (ssShift in Shift) and (not(ssCtrl in Shift)) then
    begin
      //SelectVisibleNodesBetween(Selected, ClickedNode);
      SelectVisibleNodesBetween(From,ClickedNode);
      BufferNeedsUpdate := true;
      Invalidate;
    end;

    //Left button, not selected.
    if (ssCtrl in Shift) and (not(ssShift in Shift)) then
    begin
      SelectedList.Add(ClickedNode);
      FocusedNode := ClickedNode;
      BufferNeedsUpdate := true;
      Invalidate;
    end;

  end;



  //Clicked node is already selected....
  if (InList) and (Button = mbRight) then
  begin
    FocusedNode := ClickedNode;
    BufferNeedsUpdate := true;
    Invalidate;
  end;

  if (InList) and (Button = mbLeft) then
  begin
    //Left button, selected.
    if (NoModifiers) then
    begin
      if (etoAutoExpand in Options) then
      begin
        if (ClickedNode.Expanded = false)
          then DoExpand(ClickedNode)
          else DoCollapse(ClickedNode);
      end;

      //select the node.
      FocusedNode := ClickedNode;
      BufferNeedsUpdate := true;
      Invalidate;
    end;

    //Left button, selected.
    if (ssShift in Shift) and (not(ssCtrl in Shift)) then
    begin
      SelectVisibleNodesBetween(FocusedNode, ClickedNode);
      BufferNeedsUpdate := true;
      Invalidate;
    end;

    //Left button, selected.
    if (ssCtrl in Shift) and (not(ssShift in Shift)) then
    begin
      //deselect the node.
      SelectedList.Remove(ClickedNode);

      if (FocusedNode = ClickedNode) then
      begin
        if SelectedList.Count > 0
          then FocusedNode := SelectedList[0] as TVamTreeViewNode
          else FocusedNode := nil;
      end;

      BufferNeedsUpdate := true;
      Invalidate;
    end;

  end;

  BufferNeedsUpdate := true;
  Invalidate;
end;

function TVamTreeView.FindNodeWithTopValue(NodeTopValue: integer): TVamTreeViewNode;
var
  Node:TVamTreeViewNode;
begin
  //result := nil;

  Node := GetFirstNode;

  while (Node <> nil) and (Node.Top <> NodeTopValue) do Node := GetNextNode(Node);

  result := Node;
end;


procedure TVamTreeView.SelectNone;
begin
  SelectedList.Clear;
  FocusedNode := nil;
  BufferNeedsUpdate := true;
end;

procedure TVamTreeView.SelectUp;
var
  nn:TVamTreeViewNode;
begin
  nn := GetPrevSibling(FocusedNode);
  if nn <> nil then
  begin
    SelectVisibleNodesBetween(From,nn);
    ScrollIntoView(nn);
    BufferNeedsUpdate := true;
    Invalidate;
  end;
end;

procedure TVamTreeView.SelectDown;
var
  nn:TVamTreeViewNode;
begin
  nn := GetNextSibling(FocusedNode);
  if nn <> nil then
  begin
    SelectVisibleNodesBetween(From,nn);
    ScrollIntoView(nn);
    BufferNeedsUpdate := true;
    Invalidate;
  end;
end;


procedure TVamTreeView.SelectVisibleNodesBetween(NodeA, NodeB: TVamTreeViewNode);
var
  Node:TVamTreeViewNode;
begin
  SelectedList.Clear;

  if (not assigned(NodeA)) or (not assigned(NodeB)) then exit;


  if NodeA.Top < NodeB.Top then
  begin
    Node := NodeA;

    while (Node <> NodeB) and (Node <> nil) do
    begin
      SelectedList.Add(Node);
      Node := GetNextVisible(Node);
    end;
    SelectedList.Add(Node);

  end else
  //if NodeA.Top > NodeB.Top then
  begin
    Node := NodeA;

    while (Node <> NodeB) and (Node <> nil) do
    begin
      SelectedList.Add(Node);
      Node := GetPrevVisible(Node);
    end;
    SelectedList.Add(Node);

  end;

  fFocusedNode := NodeB;
  DoSelectionChange(fFocusedNode);
  BufferNeedsUpdate := true;

  BufferNeedsUpdate := true;
  Invalidate;





end;





procedure TVamTreeView.GetBackground(var Background: TBitmap; var Src: TRect);
begin
  Background := nil;
  if assigned(fOnGetBackground) then OnGetBackground(self, Background, Src);

end;

function TVamTreeView.GetFirstSelected: TVamTreeViewNode;
begin
  if SelectedList.Count > 0
    then result := SelectedList[0] as TVamTreeViewNode
    else result := FocusedNode;

end;

function TVamTreeView.GetNodeAt(x, y: integer): TVamTreeViewNode;
var
  aNode:TVamTreeViewNode;
begin
  //x := x - TreeOffsetX;
  //y := y - TreeOffsetY;

  result := nil;

  aNode := GetFirstNode;

  while (aNode <> nil) do
  begin
    if (aNode.Visible) and (aNode.Top + DefaultNodeHeight> Y) then
    begin
      if x < aNode.Width
        then result := aNode
        else result := nil;
      exit; // <--------------------------------------------------
    end else
    begin
      aNode := GetNextNode(aNode);
    end;
  end;

end;


function TVamTreeView.GetNodeBitmap(aNode: TVamTreeViewNode): TBitmap;
var
  aBitmap:TBitmap;
begin
  aBitmap := nil;
  if assigned(fOnGetNodeBitmap) then OnGetNodeBitmap(self, aNode, aBitmap);
  result := aBitmap;
end;

procedure TVamTreeView.ScrollIntoView(Node: TVamTreeViewNode);
begin
  if Node = nil then exit;

  if Node.Top < 0 then
  begin
    TreeOffsetY := TreeOffsetY - Node.Top;
    BufferNeedsUpdate := true;
    Invalidate;
  end;

  if Node.Top + DefaultNodeHeight * 3 > Height then
  begin
    TreeOffsetY := TreeOffsetY - (Node.Top + DefaultNodeHeight * 3 - Height);
    BufferNeedsUpdate := true;
    Invalidate;
  end;


end;

procedure TVamTreeView.MoveTreeDown;
begin
  TreeOffsetY := TreeOffsetY - self.DefaultNodeHeight;
  BufferNeedsUpdate := true;
  Invalidate;
end;

procedure TVamTreeView.MoveTreeUp;
begin
  TreeOffsetY := TreeOffsetY + self.DefaultNodeHeight;
  BufferNeedsUpdate := true;
  Invalidate;
end;



procedure TVamTreeView.ScrollXChange;
begin
  if assigned(fOnScrollXChange) then OnScrollXChange(self);
end;

procedure TVamTreeView.ScrollYChange;
begin
  if assigned(fOnScrollYChange) then OnScrollYChange(self);
end;

procedure TVamTreeView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  if (AWidth <> 0) and (AHeight <> 0) then
  begin
    StagingBuffer.Width  := AWidth;
    StagingBuffer.Height := AHeight;
    CalcTreeDimensions;
  end;
end;


procedure TVamTreeView.SetDefaultNodeHeight(const Value: integer);
begin
  fDefaultNodeHeight := Value;
end;

// procedure TEasyTreeView.SetFontSize(Size: integer);
// Is for setting the font size at run time. It recalcuates the minimum node
// height as well as change the font size.
procedure TVamTreeView.SetFontSize(Size: integer);
begin
  DefaultNodeHeight := round(BackBuffer.TextHeight);
end;

procedure TVamTreeView.SetOptions(const Value: TETOptions);
begin
  fOptions := Value;
end;

procedure TVamTreeView.SetScrollPosX(Value: single);
var
  sp : integer;
begin
  assert(Value >= 0);
  assert(Value <= 1);

  if Value > 1 then value := 1
  else if Value < 0 then Value := 0;

  sp := -(round((TreeWidth - Width) * Value));

  if sp <> TreeOffsetX then
  begin
    TreeOffsetX := sp;
    BufferNeedsUpdate := true;
    Invalidate;
  end;
end;

procedure TVamTreeView.SetScrollPosY(Value: single);
var
  sp : integer;
begin
  if Value > 1 then value := 1
  else if Value < 0 then Value := 0;

  sp := -(round((TreeHeight - Height) * Value));

  if sp <> TreeOffsetY then
  begin
    TreeOffsetY := sp;
    BufferNeedsUpdate := true;
    Invalidate;
  end;
end;

procedure TVamTreeView.SetFocusedNode(const Value: TVamTreeViewNode);
begin
  if fFocusedNode <> Value then
  begin
    From := Value;
    fFocusedNode := Value;

    if assigned(Value) then
    begin
      if SelectedList.IndexOf(Value) = -1 then
      begin
        SelectedList.Clear;
        SelectedList.Add(Value);
        fFocusedNode := Value;
      end else
      begin
        fFocusedNode := Value;
      end;
    end else
    begin
      SelectedList.Clear;
      FocusedNode := nil;
    end;


    BufferNeedsUpdate := true;
    DoSelectionChange(fFocusedNode);
  end;

end;

procedure TVamTreeView.SetSelectedNodeAlpha(const Value: byte);
begin
  if fSelectedNodeAlpha <> Value then
  begin
    fSelectedNodeAlpha := Value;
    BufferNeedsUpdate := true;
    Invalidate;
  end;
end;

procedure TVamTreeView.SetSelectedNodeColor(const Value: TColor);
begin
  if fSelectedNodeColor <> Value then
  begin
    fSelectedNodeColor := Value;
    BufferNeedsUpdate := true;
    Invalidate;
  end;
end;

procedure TVamTreeView.PageDown;
var
  NodesToJump:integer;
  Node:TVamTreeViewNode;
  c1: Integer;
begin

  NodesToJump := Height div DefaultNodeHeight;

  Node := FocusedNode; //Get current position in tree.

  SelectNone; // de-select all nodes.

  if Node = nil then
  begin
    FocusedNode := GetFirstNode;
    ScrollIntoView(FocusedNode);
  end;

  if Node <> nil then
  begin
    for c1 := 0 to NodesToJump - 1 do
    begin
      if GetNextVisible(Node) <> nil
        then Node := GetNextVisible(Node);
    end;

    FocusedNode := Node;
    ScrollIntoView(FocusedNode);
  end;

  BufferNeedsUpdate := true;
  Invalidate;
end;

procedure TVamTreeView.PageUp;
var
  NodesToJump:integer;
  Node:TVamTreeViewNode;
  c1: Integer;
begin

  NodesToJump := Height div DefaultNodeHeight;

  Node := FocusedNode; //Get current node position in tree.

  SelectNone;  //de-select all selected nodes.

  if Node = nil then
  begin
    FocusedNode := GetFirstNode;
    ScrollIntoView(FocusedNode);
  end;

  if Node <> nil then
  begin
    for c1 := 0 to NodesToJump - 1 do
    begin
      if GetPrevVisible(Node) <> nil
        then Node := GetPrevVisible(Node);
    end;

    FocusedNode := Node;
    ScrollIntoView(FocusedNode);
  end;

  BufferNeedsUpdate := true;
  Invalidate;
end;

procedure TVamTreeView.SetTreeOffsetX(Value: integer);
begin
  if Value > 0 then Value := 0
  else if Value + (TreeWidth - Width) < 0 then Value := -(TreeWidth - Width);

  if Value <> fTreeOffsetX then
  begin
    fTreeOffsetX := Value;
    BufferNeedsUpdate := true;
    Invalidate;
    ScrollXChange;
  end;
end;

procedure TVamTreeView.SetTreeOffsetY(Value: integer);
begin
  if Value > 0 then Value := 0
  else if Value + (TreeHeight - Height) < 0 then Value := -(TreeHeight - Height);

  if Value <> fTreeOffsetY then
  begin
    fTreeOffsetY := Value;
    BufferNeedsUpdate := true;
    Invalidate;
    ScrollYChange;
  end;
end;




procedure TVamTreeView.CalcTreeDimensions;
var
  c1, th,tw:integer;
  NodeWidth:integer;
begin
  if not assigned(StagingBuffer) then exit;

  BufferNeedsUpdate := true;

  th := 0;
  tw := Width;
  for c1 := 0 to RootNodeCount - 1 do
  begin
    th := th + GetNodeHeight(RootNodes[c1]);

    // HACK: Error can arrise here.
    NodeWidth := GetNodeWidth(RootNodes[c1]) + 15;
    if NodeWidth > tw then tw := NodeWidth;

  end;

  th := th + 30;

  if th < Height then th := Height;

  TreeHeight := th;
  TreeWidth  := tw;  //Temp.

  //HACK: There is something wrong with the GetNodeWidth() function.
  // It returns a much too wide value, resulting in errors etc when
  // trying to create the large background bitmap. This hack
  // limits the maximum size of the bitmap size with complete disregard for
  // how this impacts functionality.
  if TreeWidth > Width * 3 then TreeWidth := Width * 3;

  if TreeOffsetX < Width - TreeWidth then TreeOffsetX := Width - TreeWidth;

  if TreeOffsetY < Height - TreeHeight then TreeOffsetY := Height - TreeHeight;

  fScrollRangeX := Width / TreeWidth;
  fScrollRangeY := Height / TreeHeight;
  ScrollXChange;
  ScrollYChange;
end;

procedure TVamTreeView.LoadFromStream(Stream: TStream);
var
  i:integer;
  b:boolean;
begin
  inherited;

  CalcTreeDimensions;

  //Load tree offsets
  Stream.Read(i,SizeOf(i));
  TreeOffsetX := i;
  Stream.Read(i,SizeOf(i));
  TreeOffsetY := i;

  //Selected node.
  Stream.Read(b,SizeOf(b));

  if b = true then
  begin
    Stream.Read(i,SizeOf(i));
    FocusedNode := FindNodeWithTopValue(i);
  end;


end;

procedure TVamTreeView.SaveToStream(Stream: TStream);
var
  i:integer;
  b:boolean;
begin
  inherited;

  //Save tree offsets.
  i := TreeOffsetX;
  Stream.Write(i,SizeOf(i));
  i := TreeOffsetY;
  Stream.Write(i,SizeOf(i));

  //Selected node.
  if FocusedNode <> nil
    then b := true
    else b := false;

  Stream.Write(b,SizeOf(b));

  if b = true then
  begin
    i := FocusedNode.Top;
    Stream.Write(i,SizeOf(i));
  end;


end;

procedure TVamTreeView.Paint;
//var
  //c1:integer;
  //CurVertOffset:integer;
  //CurHorzOffset:integer;
  //Node:TVamTreeViewNode;
  //Background:TBitmap;
  //Src,Dest:TRect;
begin
  inherited;

  BackBuffer.BufferInterface.ClearAll(255,255,255,555);

  if BufferNeedsUpdate then
  begin
    BufferNeedsUpdate := false;
    DrawTreeView(StagingBuffer);
  end;

  StagingBuffer.RedFoxInterface.DrawTo(BackBuffer.RedFoxInterface, 0, 0);

  // TODO:MED Previously the tree view would be drawn into a another
  // bitmap buffer. That buffer would then be blitted to the control
  // backbuffer when required. Consider doing that again.
  // I've kept the code below as a reference as to what used to happen.


  {
  BackBuffer.BufferInterface.ClearAll(255,255,255,0);
  BackBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
  StagingBuffer.UpdateFont(Font);

  //=== Update Font =======
  //Buffer.UpdateFont(Font);
  //Buffer.BufferInterface.TextAlignment(tahLeft, tavCenter);
  //=======================

  GetBackground(Background, Src);

  if assigned(Background) then
  begin
    Src.Left   := Left;
    Src.Top    := Top;
    Src.Right  := Left + Width;
    Src.Bottom := Top + Height;

    Dest.Left   := 0;
    Dest.Right  := Width;
    Dest.Top    := 0;
    Dest.Bottom := Height;

    StagingBuffer.TransformImage(Background, -Left, -Top);

    BufferNeedsUpdate := true;
  end else
  begin
    //Buffer.BufferInterface.ClearAll(0,0,0,0);
    StagingBuffer.BufferInterface.ClearAll(255,255,255,0);
    StagingBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSourceOver;
    StagingBuffer.BufferInterface.AntiAliasGamma := 10000;

    BufferNeedsUpdate := true;
  end;

  if BufferNeedsUpdate then
  begin
    ResetNodeVisibility(MasterNode);

    CurVertOffset := RootIndent + TreeOffsetY;
    CurHorzOffset := RootIndent + TreeOffsetX;

    //Paint the nodes.
    BufferInterface.FillColor := GetAggColor(clSilver);

    for c1 := 0 to RootNodeCount - 1 do
    begin
      Node := RootNodes[c1];
      RenderNode(Node, CurVertOffset, CurHorzOffSet);
    end;

    BufferNeedsUpdate := false;
  end;

  //copy the back buffer to the display.
  BackBuffer.BufferInterface.CopyImage(StagingBuffer.AsImage,0,0);
  }

end;

procedure TVamTreeView.DrawTreeView(const DestBuffer: TRedFoxImageBuffer);
var
  c1:integer;
  CurVertOffset:integer;
  CurHorzOffset:integer;
  Node:TVamTreeViewNode;
begin
  DestBuffer.BufferInterface.ClearAll(255,255,255,0);
  DestBuffer.BufferInterface.BlendMode := TAggBlendMode.bmSource;
  DestBuffer.UpdateFont(Font);

  ResetNodeVisibility(MasterNode);

  CurVertOffset := RootIndent + TreeOffsetY;
  CurHorzOffset := RootIndent + TreeOffsetX;

  //Paint the nodes.
  DestBuffer.BufferInterface.FillColor := GetAggColor(clSilver);

  for c1 := 0 to RootNodeCount - 1 do
  begin
    Node := RootNodes[c1];
    RenderNode(DestBuffer, Node, CurVertOffset, CurHorzOffSet);
  end;
end;

procedure TVamTreeView.RenderNode(const DestBuffer: TRedFoxImageBuffer; const Node: TVamTreeViewNode; var vOffset, hOffset: integer);
var
  Text:string;
  TextPosX,TextPosY:integer;
  c1:integer;
  NodeBitmap:TBitmap;
  Src,Dest:TRect;
  NodeIsSelected:boolean;
  Selection:TRect;
  TextBounds : TRect;
  tw : single;
begin
  assert(assigned(Node));

  //Setup some values.
  Text := Node.Caption;

  NodeBitmap := GetNodeBitmap(Node);

  DestBuffer.BufferInterface.NoFill;
  if (FocusedNode = Node) or (SelectedList.IndexOf(Node) <> -1)
    then NodeIsSelected := true
    else NodeIsSelected := false;

  //render the node
  if (vOffset > (0-DefaultNodeHeight * 2)) and (vOffset < self.Height + DefaultNodeHeight * 2) then
  begin
    if NodeBitmap <> nil then
    begin
      Src.Left    := 0;
      Src.Top     := 0;
      Src.Right   := NodeBitmap.Width;
      Src.Bottom  := NodeBitmap.Height;

      Dest.Left   := hOffset;
      Dest.Right  := hOffset + DefaultNodeHeight;
      Dest.Top    := vOffset;
      Dest.Bottom := vOffset + DefaultNodeHeight;

      DestBuffer.TransformImage(NodeBitmap, Dest.Left, Dest.Top);

      TextPosX := hOffset + DefaultNodeHeight + 2;
      TextPosY := vOffset;
    end else
    begin
      TextPosX := hOffset;
      TextPosY := vOffset;
    end;

    if NodeIsSelected then
    begin
      //tw := BufferInterface.TextWidth(AnsiString(Text));
      tw := StagingBuffer.TextWidth(Text, Font);
      Selection.Left   := TextPosX;
      Selection.Right  := round(TextPosX + tw);
      Selection.Top    := TextPosY;
      Selection.Bottom := TextPosY + DefaultNodeHeight;

      DestBuffer.BufferInterface.LineColor := GetAggColor(SelectedNodeColor, SelectedNodeAlpha);
      DestBuffer.BufferInterface.FillColor := GetAggColor(SelectedNodeColor, SelectedNodeAlpha);

      DestBuffer.BufferInterface.Rectangle(Selection.Left,
                                Selection.Top,
                                Selection.Right,
                                Selection.Bottom);
    end;


    //DestBuffer.BufferInterface.LineColor := GetAggColor(Font.Color);
    //DestBuffer.BufferInterface.FillColor := GetAggColor(Font.Color);
    //Buffer.TextOut(TextPosX, (TextPosY + DefaultNodeHeight * 0.5), Text);



    TextBounds.Left := TextPosX;
    TextBounds.Right := round(TextPosX + DestBuffer.TextWidth(Text));
    TextBounds.Top  := TextPosY;
    TextBounds.Bottom := TextPosY + DefaultNodeHeight;

    DestBuffer.DrawText(Text, Font, TRedFoxAlign.AlignNear, TRedFoxAlign.AlignCenter, TextBounds, GetAggColor(Font.Color));

    //TextBounds := Rect(0,0, Width, Height);
    //BackBuffer.DrawText(Text, Font, TextAlign, TextVAlign, TextBounds);

    Node.Width := round(TextPosX + DestBuffer.TextWidth(Text));
  end;

  //fill in some values.
  Node.Left    := hOffset;
  Node.Top     := vOffset;
  Node.Visible := true;

  inc(vOffset, DefaultNodeHeight);

  //Render child nodes if needed..
  if (Node.HasChildren) and (Node.Expanded) then
  begin
    inc(hOffset, ChildIndent);

    for c1 := 0 to Node.ChildCount - 1 do
    begin
      RenderNode(DestBuffer, Node.Child[c1],vOffset,hOffset);
    end;

    dec(hOffset, ChildIndent);
  end;
end;







end.
