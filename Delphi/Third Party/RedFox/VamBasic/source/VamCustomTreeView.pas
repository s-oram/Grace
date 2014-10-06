unit VamCustomTreeView;

interface

uses
  Classes, Controls,
  VamWinControl,
  VamTreeViewNode;

type
  TNodeIOEvent = procedure(Sender : TObject; Node:TVamTreeViewNode; var NodeData : RawByteString);
  TNodeEvent = procedure(Sender:TObject; Node:TVamTreeViewNode) of object;
  TBooleanNodeEvent = procedure(Sender:TObject; Node:TVamTreeViewNode; var Allowed:boolean) of object;

  TVamCustomTreeView = class(TVamWinControl)
  private
    fMasterNode: TVamTreeViewNode;
    fOnAllowExpand: TBooleanNodeEvent;
    fOnExpanded: TNodeEvent;
    fOnAllowCollapse: TBooleanNodeEvent;
    fOnCollapsed: TNodeEvent;
    fOnSelectionChange: TNodeEvent;
    fTreeHeight: integer;
    fTreeWidth: integer;
    fOnIntializeNode: TNodeEvent;
    fOnFinalizeNode: TNodeEvent;
    function GetRootNodeCount: integer;
    function GetRootNode(Index: integer): TVamTreeViewNode;
    procedure SetRootNode(Index: integer; const Value: TVamTreeViewNode);
  protected
    procedure CalcTreeDimensions; virtual;
    procedure RemoveChildNode(aParent,aChild:TVamTreeViewNode);
    procedure ResetNodeVisibility(aNode:TVamTreeViewNode);

    procedure SaveNodeToStream(Stream:TStream; Node:TVamTreeViewNode); virtual;
    procedure LoadNodeFromStream(Stream:TStream; Node:TVamTreeViewNode); virtual;

    procedure DoSelectionChange(aNode:TVamTreeViewNode);
    property MasterNode:TVamTreeViewNode read fMasterNode write fMasterNode;

    // InitializeNode() is called after a new node has been created and added to the tree structure.
    procedure InitializeNode(aNode:TVamTreeViewNode); virtual;
    // FinalizeNode() is called just before a node is removed from the tree structure and deleted.
    procedure FinalizeNode(aNode:TVamTreeViewNode); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear; virtual;
    function CreateNode(aParent:TVamTreeViewNode):TVamTreeViewNode; virtual;
    function CreateRootNode:TVamTreeViewNode; virtual;

    function GetFirstNode:TVamTreeViewNode;
    function GetNextNode(aNode:TVamTreeViewNode):TVamTreeViewNode;
    function GetNextVisible(aNode:TVamTreeViewNode):TVamTreeViewNode;
    function GetNextSibling(aNode:TVamTreeViewNode):TVamTreeViewNode;

    function GetPrevSibling(aNode:TVamTreeViewNode):TVamTreeViewNode;
    function GetPrevVisible(aNode:TVamTreeViewNode):TVamTreeViewNode;

    function GetFirstChild(aNode:TVamTreeViewNode):TVamTreeViewNode;
    function GetLastChild(aNode:TVamTreeViewNode):TVamTreeViewNode;

    function GetParent(aNode:TVamTreeViewNode):TVamTreeViewNode;

    procedure DeleteNode(Node:TVamTreeViewNode; UpdateTreeDimensions:boolean = true);
    procedure DeleteChildNodes(Node:TVamTreeViewNode; UpdateTreeDimensions:boolean = true);

    procedure DoExpand(aNode:TVamTreeViewNode); virtual;
    procedure DoCollapse(aNode:TVamTreeViewNode); virtual;

    procedure SaveToStream(Stream:TStream); virtual;
    procedure LoadFromStream(Stream:TStream); virtual;

    property TreeHeight:integer read fTreeHeight write fTreeHeight;
    property TreeWidth:integer read fTreeWidth write fTreeWidth;

    property RootNodeCount:integer read GetRootNodeCount;
    property RootNodes[Index:integer]:TVamTreeViewNode read GetRootNode write SetRootNode;
  published
    property OnAllowExpand:TBooleanNodeEvent read fOnAllowExpand write fOnAllowExpand;
    property OnExpanded:TNodeEvent read fOnExpanded write fOnExpanded;
    property OnAllowCollapse:TBooleanNodeEvent read fOnAllowCollapse write fOnAllowCollapse;
    property OnCollapsed:TNodeEvent read fOnCollapsed write fOnCollapsed;

    property OnSelectionChange:TNodeEvent read fOnSelectionChange write fOnSelectionChange;


    // OnIntializeNode is called after a node is created, but before it is used in the tree. It can
    // be used to initalize any data that is attached to the node.
    property OnInitializeNode : TNodeEvent read fOnIntializeNode write fOnIntializeNode;
    // OnFinalizeNode is called just before a node is free'ed. It should be used to 'free' any
    // related node data.
    property OnFinalizeNode  : TNodeEvent read fOnFinalizeNode  write fOnFinalizeNode;
  end;


  TVamTreeViewNodeHack = class(TVamTreeViewNode);

implementation

uses
  SysUtils, Dialogs;

{ TCustomEasyTreeView }


constructor TVamCustomTreeView.Create(AOwner: TComponent);
begin
  inherited;
  MasterNode := TVamTreeViewNode.Create;
  MasterNode.Depth := -1;
end;

destructor TVamCustomTreeView.Destroy;
begin
  // An access violation occurs here somewhere....
  if MasterNode.ChildCount > 0
    then DeleteChildNodes(MasterNode, false);
  MasterNode.Free;
  inherited;
end;


procedure TVamCustomTreeView.Clear;
begin
  if MasterNode.HasChildren then DeleteChildNodes(MasterNode, false);
  CalcTreeDimensions;
  Invalidate;
end;



function TVamCustomTreeView.CreateRootNode: TVamTreeViewNode;
begin
  result := CreateNode(MasterNode);
  CalcTreeDimensions;
end;

procedure TVamCustomTreeView.DeleteChildNodes(Node: TVamTreeViewNode; UpdateTreeDimensions:boolean = true);
var
  c1:integer;
  aNode : TVamTreeViewNode;
begin
  assert(Node.HasChildren);
  for c1 := Node.ChildCount-1 downto 0 do
  begin
    aNode := Node.Child[c1];
    DeleteNode(aNode, false);
  end;

  if UpdateTreeDimensions then CalcTreeDimensions;
end;

procedure TVamCustomTreeView.DeleteNode(Node: TVamTreeViewNode; UpdateTreeDimensions:boolean = true);
begin
  FinalizeNode(Node);

  if (Node.ParentNode <> nil)
    then RemoveChildNode(Node.ParentNode, Node);

  if Node.HasChildren then DeleteChildNodes(Node, false);

  Node.Free;

  if UpdateTreeDimensions then CalcTreeDimensions;
end;


procedure TVamCustomTreeView.DoCollapse(aNode: TVamTreeViewNode);
var
  Allowed:boolean;
begin
  if aNode.Expanded = true then
  begin
    Allowed := (aNode.ChildCount > 0);
    if assigned(fOnAllowCollapse) then OnAllowCollapse(Self, aNode, Allowed);

    if (Allowed) then
    begin
      aNode.Expanded := false;
      if assigned(fOnCollapsed) then OnCollapsed(Self, aNode);
      CalcTreeDimensions;
      Invalidate;
    end;
  end;
end;

procedure TVamCustomTreeView.DoExpand(aNode: TVamTreeViewNode);
var
  Allowed:boolean;
begin
  if aNode.Expanded = false then
  begin
    Allowed := (aNode.ChildCount > 0);
    if assigned(fOnAllowExpand) then OnAllowExpand(Self, aNode, Allowed);
    if (Allowed) then
    begin
      aNode.Expanded := true;
      if assigned(fOnExpanded) then OnExpanded(Self, aNode);
      CalcTreeDimensions;
      Invalidate;
    end;
  end;
end;

procedure TVamCustomTreeView.DoSelectionChange(aNode: TVamTreeViewNode);
begin
  if assigned(fOnSelectionChange) then OnSelectionChange(self, aNode);
end;

procedure TVamCustomTreeView.FinalizeNode(aNode: TVamTreeViewNode);
begin
  if assigned(OnFinalizeNode) then OnFinalizeNode(self, aNode);
end;

function TVamCustomTreeView.GetFirstChild(aNode: TVamTreeViewNode): TVamTreeViewNode;
begin
  if (aNode.HasChildren)
    then result := aNode.Child[0]
    else result := nil;
end;

function TVamCustomTreeView.GetFirstNode: TVamTreeViewNode;
begin
  if RootNodeCount > 0
    then result := RootNodes[0]
    else result := nil;

end;

function TVamCustomTreeView.GetLastChild(aNode: TVamTreeViewNode): TVamTreeViewNode;
var
  Index:integer;
begin
  if (aNode.HasChildren) then
  begin
    Index := aNode.ChildCount-1;
    result := aNode.Child[Index];
  end else
  begin
    result := nil;
  end;
end;

function TVamCustomTreeView.GetNextNode(aNode: TVamTreeViewNode): TVamTreeViewNode;
var
  n:TVamTreeViewNode;
begin
  //result := nil;

  //if possible choose first child.
  if aNode.ChildCount > 0 then
  begin
    result := aNode.Child[0];
    exit; //    <------------------------------------------------
  end;

  //if possible choose next sibling.
  n := GetNextSibling(aNode);
  if n <> nil then
  begin
    result := n;
    exit; //    <------------------------------------------------
  end;

  //finally check for parent's siblings.
  while (n = nil) and (aNode.ParentNode <> nil) do
  begin
    aNode := aNode.ParentNode;
    n := GetNextSibling(aNode);
  end;
  result := n;

end;

function TVamCustomTreeView.GetNextVisible(aNode: TVamTreeViewNode): TVamTreeViewNode;
var
  n:TVamTreeViewNode;
begin
  //result := nil;

  //if this node isn't visible, move up through the parents.
  if aNode.Visible = false then
  begin
    while (aNode <> nil) and (aNode.Visible = false)
      do aNode := aNode.ParentNode;

    result := aNode;
    exit; //--------------------------------->> exit >>----------->>
  end;


  //if possible choose first child.
  if (aNode.Expanded) and (aNode.ChildCount > 0) then
  begin
    result := aNode.Child[0];
    exit; //--------------------------------->> exit >>----------->>
  end;

  //if possible choose next sibling.
  n := GetNextSibling(aNode);
  if n <> nil then
  begin
    result := n;
    exit; //--------------------------------->> exit >>----------->>
  end;

  //finally check for parent's siblings.
  while (n = nil) and (aNode.ParentNode <> nil) do
  begin
    aNode := aNode.ParentNode;
    n := GetNextSibling(aNode);
  end;
  result := n;

end;



function TVamCustomTreeView.GetParent(aNode: TVamTreeViewNode): TVamTreeViewNode;
begin
  if aNode.ParentNode = MasterNode
    then result := nil
    else result := aNode.ParentNode;

end;

function TVamCustomTreeView.GetPrevSibling(aNode: TVamTreeViewNode): TVamTreeViewNode;
var
  aParent:TVamTreeViewNode;
  Index:integer;
begin
  if aNode.ParentNode = nil then
  begin
    result := nil;
    exit; //  <------------------------------------------------
  end;

  aParent := aNode.ParentNode;

  Index := aParent.ChildList.IndexOf(aNode);

  if Index > 0
    then result := aParent.Child[Index - 1]
    else result := nil;

end;

function TVamCustomTreeView.GetPrevVisible(aNode: TVamTreeViewNode): TVamTreeViewNode;
var
  n:TVamTreeViewNode;
begin
  assert(aNode <> nil);

  if aNode = nil then raise Exception.Create('Node not assigned.');


  //result := nil;

  //if this node isn't visible, move up through the parents.
  if aNode.Visible = false then
  begin
    while (aNode <> nil) and (aNode.Visible = false)
      do aNode := aNode.ParentNode;

    result := aNode;
    exit; // <--------------------------------------------------
  end;

  //if possible choose prev sibling.
  n := GetPrevSibling(aNode);
  if n <> nil then
  begin
    //If this sibling has child nodes visible need to choose
    //the last visbible child.
    while (n.HasChildren) and (n.Expanded) do n := GetLastChild(n);

    result := n;
    exit; //    <------------------------------------------------
  end;


  //finally, choose the parent node (if it has one);
  if (aNode.ParentNode <> MasterNode) and (aNode.ParentNode <> nil)
    then result := aNode.ParentNode
    else result := nil;



end;

function TVamCustomTreeView.GetNextSibling(aNode: TVamTreeViewNode): TVamTreeViewNode;
var
  aParent:TVamTreeViewNode;
  Index:integer;
begin
  if aNode.ParentNode = nil then
  begin
    result := nil;
    exit; //  <------------------------------------------------
  end;

  aParent := aNode.ParentNode;

  Index := aParent.ChildList.IndexOf(aNode);

  if Index < aParent.ChildCount-1
    then result := aParent.Child[Index + 1]
    else result := nil;

end;

function TVamCustomTreeView.GetRootNode(Index: integer): TVamTreeViewNode;
begin
  if Index >= MasterNode.childList.count  then assert(false);

  result := MasterNode.Child[Index];
end;

procedure TVamCustomTreeView.SetRootNode(Index: integer; const Value: TVamTreeViewNode);
begin
  MasterNode.Child[Index] := Value;
end;

function TVamCustomTreeView.GetRootNodeCount: integer;
begin
  result := MasterNode.ChildCount;
end;

procedure TVamCustomTreeView.InitializeNode(aNode: TVamTreeViewNode);
begin
  if assigned(OnInitializeNode) then OnInitializeNode(self, aNode);
end;

function TVamCustomTreeView.CreateNode(aParent: TVamTreeViewNode): TVamTreeViewNode;
var
  aNode:TVamTreeViewNode;
begin
  aNode := TVamTreeViewNode.Create;

  if aParent = fMasterNode
    then TVamTreeViewNodeHack(aNode).SetIsRootNode(true);

  TETNodeEx(aParent).ChildList.Add(aNode);
  aNode.ParentNode := aParent;
  aNode.Depth := aParent.Depth + 1;

  InitializeNode(aNode);

  result := aNode;
end;

procedure TVamCustomTreeView.RemoveChildNode(aParent, aChild: TVamTreeViewNode);
var
  c1:integer;
begin
  assert(aChild.ParentNode = aParent);

  aChild.ParentNode := nil;

  for c1 := 0 to aParent.ChildCount - 1 do
  begin
    if aParent.Child[c1] = aChild then
    begin
      aParent.ChildList.Delete(c1);
      Break;
    end;
  end;

end;

procedure TVamCustomTreeView.ResetNodeVisibility(aNode:TVamTreeViewNode);
var
  c1:integer;
begin
  aNode.Visible := false;
  for c1 := 0 to aNode.ChildCount - 1 do
  begin
    ResetNodeVisibility(aNode.Child[c1]);
  end;
end;

procedure TVamCustomTreeView.CalcTreeDimensions;
begin
  //This method should be over-ridden to by the descendent
  //class which handles drawing the tree.
  TreeHeight := 0;
  TreeWidth := 0;
end;



procedure TVamCustomTreeView.LoadFromStream(Stream: TStream);
begin
  Clear;
  LoadNodeFromStream(Stream, MasterNode);
end;

procedure TVamCustomTreeView.SaveToStream(Stream: TStream);
begin
  SaveNodeToStream(Stream, MasterNode);
end;

procedure TVamCustomTreeView.LoadNodeFromStream(Stream: TStream;
  Node: TVamTreeViewNode);
var
  c1,ChildCount:integer;
  ChildNode:TVamTreeViewNode;
begin
  Node.LoadFromStream(Stream);

  //read the child count
  Stream.Read(ChildCount, SizeOf(ChildCount));

  //Load the child nodes.
  for c1 := 0 to ChildCount - 1 do
  begin
    ChildNode := CreateNode(Node);
    LoadNodeFromStream(Stream,ChildNode);
    ChildNode.ParentNode := Node;
  end;

end;

procedure TVamCustomTreeView.SaveNodeToStream(Stream: TStream; Node: TVamTreeViewNode);
var
  c1,ChildCount:integer;
begin
  Node.SaveToStream(Stream);




  //Save node child count.
  ChildCount := Node.ChildCount;
  Stream.Write(ChildCount,SizeOf(ChildCount));

  //Save the child nodes.
  for c1 := 0 to ChildCount - 1 do
  begin
    SaveNodeToStream(Stream,Node.Child[c1]);
  end;


end;





end.
