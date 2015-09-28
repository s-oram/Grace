{
  RedFoxAddonFileBrowser adds File Browser functionality to the RedFoxTreeView
  component.

  The "FileBrowser" is intended for use in plugins which need an inbuilt browser.
}

unit eeFileBrowserAddon;

interface

uses
  Types, Graphics,
  Classes, Controls,
  VamTreeViewNode,
  VamTreeView,
  DropSource,
  DragDropFile,
  DragDrop,
  DropTarget;

type
  TNodeType = (ntFolder, ntFile, ntSpecial);

  PNodeData = ^TNodeData;
  TNodeData = record
    CanExpand : boolean;
    NodeType  : TNodeType;
    FileName  : string;
  end;

  TRootNode = record
    Dir  : string;
    Name : string;
  end;

  TOnFilterNodesEvent = procedure(Sender : TObject; const RootDir : string; var FolderNodes, FileNodes : TStringList) of object;
  TGetNodeBitmapEvent = procedure(Sender : TObject; const NodeFileName : string; var Bitmap : TBitmap) of object;

  TFileBrowserAddon = class
  private
    fOnNodeFocusChanged: TNotifyEvent;
    fOnFilterNodes: TOnFilterNodesEvent;
    fOnGetNodeBitmap: TGetNodeBitmapEvent;
  protected
    DropFileSource: TDropFileSource;

    RootNodeCount : integer;
    RootNodes : array of TRootNode;
    TreeView : TVamTreeView;

    procedure EventHandle_BrowserMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EventHandle_BrowserMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure EventHandle_BrowserMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);

    procedure EventHandle_BrowserAllowExpand(Sender: TObject; Node: TVamTreeViewNode; var Allowed: Boolean);
    procedure EventHandle_BrowserAllowCollapse(Sender: TObject; Node: TVamTreeViewNode; var Allowed: Boolean);
    procedure EventHandle_NodeCollapsed(Sender:TObject; Node:TVamTreeViewNode);

    procedure EventHandle_OnInitializeNode(Sender:TObject; Node:TVamTreeViewNode);
    procedure EventHandle_OnFinalizeNode(Sender:TObject; Node:TVamTreeViewNode);

    procedure EventHandle_GetNodeBitmap(Sender: TObject; Node: TVamTreeViewNode; var Bitmap: TBitmap);

    procedure EventHandle_OnBeginNodeDrag(Sender : TObject);

    procedure NodeFocusedChanged;

    procedure RefreshNode(Node : TVamTreeViewNode);
  public
    constructor Create(aTreeView:TVamTreeView);
    destructor Destroy; override;

    procedure RefreshTreeView;

    procedure AddRootNode(Dir, Name : string);
    procedure ClearRootNodes;
    procedure UpdateRootNodes;

    procedure Command_BrowserUp;
    procedure Command_BrowserDown;
    procedure Command_BrowserLeft;
    procedure Command_BrowserRight;
    procedure Command_PageUp;
    procedure Command_PageDown;
    procedure Command_SelectUp;
    procedure Command_SelectDown;

    function GetFocusedNodeData : PNodeData;

    property OnNodeFocusChanged : TNotifyEvent read fOnNodeFocusChanged write fOnNodeFocusChanged;
    property OnFilterNodes : TOnFilterNodesEvent read fOnFilterNodes write fOnFilterNodes;
    property OnGetNodeBitmap : TGetNodeBitmapEvent read fOnGetNodeBitmap write fOnGetNodeBitmap;
  end;

implementation

uses
  Dialogs,
  SysUtils, VamLib.Utils, uFindFiles;

{ TFileBrowserAddon }

constructor TFileBrowserAddon.Create(aTreeView: TVamTreeView);
begin
  TreeView := aTreeView;
  TreeView.OnMouseDown      := self.EventHandle_BrowserMouseDown;
  TreeView.OnMouseWheelDown := self.EventHandle_BrowserMouseWheelDown;
  TreeView.OnMouseWheelUp   := self.EventHandle_BrowserMouseWheelUp;
  TreeView.OnInitializeNode := self.EventHandle_OnInitializeNode;
  TreeView.OnFinalizeNode   := self.EventHandle_OnFinalizeNode;
  TreeView.OnAllowExpand    := self.EventHandle_BrowserAllowExpand;
  TreeView.OnAllowCollapse  := self.EventHandle_BrowserAllowCollapse;
  TreeView.OnCollapsed      := self.EventHandle_NodeCollapsed;
  TreeView.OnBeginNodeDrag  := self.EventHandle_OnBeginNodeDrag;
  TreeView.OnGetNodeBitmap  := self.EventHandle_GetNodeBitmap;
  RootNodeCount := 0;

  DropFileSource := TDropFileSource.Create(nil);
  DropFileSource.DragTypes := [dtCopy, dtLink];
end;

destructor TFileBrowserAddon.Destroy;
begin
  DropFileSource.Free;
  RootNodeCount := 0;
  SetLength(RootNodes, 0);
  inherited;
end;



procedure TFileBrowserAddon.AddRootNode(Dir, Name: string);
begin
  Name := Trim(Name);
  if Name = '' then Name := '(No Name)';

  inc(RootNodeCount);
  SetLength(RootNodes, RootNodeCount);
  RootNodes[RootNodeCount-1].Dir  := Dir;
  RootNodes[RootNodeCount-1].Name := Name;
end;

procedure TFileBrowserAddon.ClearRootNodes;
begin
  RootNodeCount := 0;
  SetLength(RootNodes, 0);
end;

procedure TFileBrowserAddon.UpdateRootNodes;
var
  nd : PNodeData;
  aNode : TVamTreeViewNode;
  c1: Integer;
begin
  TreeView.Clear;

  for c1 := 0 to RootNodeCount-1 do
  begin
    aNode := TreeView.CreateRootNode;
    aNode.Caption := RootNodes[c1].Name;

    nd := aNode.Data;
    nd^.FileName := RootNodes[c1].Dir;
    nd^.NodeType := TNodeType.ntFolder;
    nd^.CanExpand := true;
  end;

  //TODO:
  // If root node count = 1, then expand the root node.


end;

procedure TFileBrowserAddon.EventHandle_BrowserAllowCollapse(Sender: TObject; Node: TVamTreeViewNode; var Allowed: Boolean);
var
  Data : PNodeData;
begin
  Data := Node.Data;

  if Data^.CanExpand
    then Allowed := true
    else Allowed := false;

end;

procedure TFileBrowserAddon.EventHandle_BrowserAllowExpand(Sender: TObject; Node: TVamTreeViewNode; var Allowed: Boolean);
var
  c1 : integer;
  s : string;
  ChildNode : TVamTreeViewNode;
  Data, ChildData : PNodeData;
  FolderResults, FileResults : TStringList;
  Dir : string;
  NodePath : string;
begin
  Data := Node.Data;

  if Data^.CanExpand
    then Allowed := true
    else Allowed := false;

  NodePath := Data^.FileName;

  if (Data^.CanExpand) and (DirectoryExists(NodePath)) then
  begin
    FolderResults  := TStringList.Create;
    FolderResults.Sorted := true;
    FolderResults.CaseSensitive := false;
    AutoFree(@FolderResults);

    FileResults := TStringList.Create;
    FileResults.Sorted := true;
    FileResults.CaseSensitive := false;
    AutoFree(@FileResults);

    Dir := includeTrailingPathDelimiter(Data^.FileName);

    // First find the folders.
    FindOnlyFolders(Dir, FolderResults);
    FindOnlyFiles(Dir, FileResults,'*.*');

    if assigned(OnFilterNodes) then
    begin
      OnfilterNodes(self, Dir, FolderResults, FileResults);
    end;

    for c1 := 0 to FolderResults.Count - 1 do
    begin
      ChildNode := TreeView.CreateNode(Node);
      ChildNode.Caption := FolderResults[c1];

      ChildData              := ChildNode.Data;
      s := IncludeTrailingPathDelimiter(Dir + FolderResults[c1]);
      ChildData^.FileName    := s;
      ChildData^.NodeType    := ntFolder;
      ChildData^.CanExpand   := true;
    end;

    for c1 := 0 to FileResults.Count - 1 do
    begin
      ChildNode := TreeView.CreateNode(Node);
      ChildNode.Caption := FileResults[c1];

      ChildData              := ChildNode.Data;
      ChildData^.FileName    := Dir + FileResults[c1];
      ChildData^.NodeType    := ntFile;
      ChildData^.CanExpand   := false;
    end;

    if (FileResults.Count = 0) and (FolderResults.Count = 0) then
    begin
      ChildNode := TreeView.CreateNode(Node);
      ChildNode.Caption := '(empty)';

      ChildData              := ChildNode.Data;
      ChildData^.FileName    := '';
      ChildData^.NodeType    := ntSpecial;
      ChildData^.CanExpand   := false;
    end;
  end;


  if (Data^.CanExpand) and (DirectoryExists(NodePath) = false) then
  begin
    ChildNode := TreeView.CreateNode(Node);
    ChildNode.Caption := 'Error: Path not found.';

    ChildData              := ChildNode.Data;
    ChildData^.FileName    := '';
    ChildData^.NodeType    := ntSpecial;
    ChildData^.CanExpand   := false;
  end;

end;



procedure TFileBrowserAddon.EventHandle_NodeCollapsed(Sender: TObject; Node: TVamTreeViewNode);
begin
  if (Node.HasChildren) then TreeView.DeleteChildNodes(Node);
end;

procedure TFileBrowserAddon.EventHandle_OnInitializeNode(Sender: TObject; Node: TVamTreeViewNode);
var
  NodeData : PNodeData;
begin
  GetMem(NodeData, SizeOf(NodeData^));
  Initialize(NodeData^);
  Node.Data := NodeData;
end;

function TFileBrowserAddon.GetFocusedNodeData: PNodeData;
begin
  if TreeView.FocusedNode <> nil
    then result := TreeView.FocusedNode.Data
    else result := nil;
end;

procedure TFileBrowserAddon.NodeFocusedChanged;
begin
  if assigned(OnNodeFocusChanged) then OnNodeFocusChanged(self);

end;

procedure TFileBrowserAddon.EventHandle_OnFinalizeNode(Sender: TObject; Node: TVamTreeViewNode);
var
  NodeData : PNodeData;
begin
  NodeData := Node.Data;
  Finalize(NodeData^);
  FreeMem(NodeData, SizeOf(NodeData^));
  Node.Data := nil;
end;

procedure TFileBrowserAddon.EventHandle_OnBeginNodeDrag(Sender: TObject);
var
  c1: Integer;
  NodeData : PNodeData;
begin
  DropFileSource.Files.Clear;

  for c1 := 0 to TreeView.SelectedCount-1 do
  begin
    NodeData := TreeView.Selected[c1].Data;
    if FileExists(NodeData^.FileName) then
    begin
      DropFileSource.Files.Add(NodeData^.FileName);
    end;
  end;

  if DropFileSource.Files.Count > 0 then
  begin
    DropFileSource.Execute(true);
  end;

end;

procedure TFileBrowserAddon.Command_BrowserUp;
var
  Node:TVamTreeViewNode;
  PrevNode:TVamTreeViewNode;
begin
  Node := TreeView.GetFirstSelected;

  if Node = nil then
  begin
    TreeView.SelectNone;
    TreeView.FocusedNode := TreeView.GetFirstNode;
  end else
  begin
    PrevNode := TreeView.GetPrevVisible(Node);
    if PrevNode <> nil then
    begin
      TreeView.SelectNone;
      TreeView.FocusedNode := PrevNode;
    end;
  end;

  TreeView.ScrollIntoView(TreeView.FocusedNode);

  TreeView.Invalidate;
  NodeFocusedChanged;
end;

procedure TFileBrowserAddon.Command_BrowserDown;
var
  Node:TVamTreeViewNode;
  NextNode:TVamTreeViewNode;
begin
  Node := TreeView.FocusedNode;

  if Node = nil then
  begin
    TreeView.SelectNone;
    TreeView.FocusedNode := TreeView.GetFirstNode;
  end else
  begin
    NextNode := TreeView.GetNextVisible(Node);
    if NextNode <> nil then
    begin
      TreeView.SelectNone;
      TreeView.FocusedNode := NextNode;
    end;
  end;

  TreeView.ScrollIntoView(TreeView.FocusedNode);

  TreeView.Invalidate;
  NodeFocusedChanged;
end;


procedure TFileBrowserAddon.Command_BrowserLeft;
var
  Node:TVamTreeViewNode;
begin
  Node := TreeView.FocusedNode;

  if Node = nil then
  begin
    TreeView.SelectNone;
    TreeView.FocusedNode := TreeView.GetFirstNode;
  end else
  if (Node.Expanded) then
  begin
    TreeView.SelectNone;
    TreeView.DoCollapse(Node);
  end else
  begin
    Node := TreeView.GetParent(Node);
  end;

  if Node <> nil then
  begin
    TreeView.SelectNone;
    TreeView.FocusedNode := Node;
  end;

  TreeView.ScrollIntoView(TreeView.FocusedNode);

  TreeView.Invalidate;
  NodeFocusedChanged;
end;


procedure TFileBrowserAddon.Command_BrowserRight;
var
  Node:TVamTreeViewNode;
  NodeData:PNodeData;
begin
  Node := TreeView.FocusedNode;

  if Node = nil then
  begin
    TreeView.SelectNone;
    TreeView.FocusedNode := TreeView.GetFirstNode;
    TreeView.Invalidate;

    NodeFocusedChanged;
    exit; //  <-------------------------------------------------
  end;

  NodeData := Node.Data;

  if (NodeData^.CanExpand = true) and (Node.Expanded = false) then
  begin
    TreeView.SelectNone;
    TreeView.DoExpand(Node);
    TreeView.FocusedNode := Node;
    TreeView.Invalidate;

    NodeFocusedChanged;
    exit; //  <-------------------------------------------------
  end;

  if (NodeData^.CanExpand = false) then
  begin
    TreeView.SelectNone;
    TreeView.FocusedNode := Node;
    TreeView.Invalidate;

    NodeFocusedChanged;
    exit; //  <-------------------------------------------------
  end;

end;

procedure TFileBrowserAddon.Command_PageDown;
begin
  TreeView.PageDown;
  NodeFocusedChanged;
end;

procedure TFileBrowserAddon.Command_PageUp;
begin
  TreeView.PageUp;
  NodeFocusedChanged;
end;

procedure TFileBrowserAddon.Command_SelectUp;
begin
  TreeView.SelectUp;
  NodeFocusedChanged;
end;

procedure TFileBrowserAddon.Command_SelectDown;
begin
  TreeView.SelectDown;
  NodeFocusedChanged;
end;

procedure TFileBrowserAddon.EventHandle_BrowserMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  NodeFocusedChanged;
end;

procedure TFileBrowserAddon.EventHandle_BrowserMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TreeView.MoveTreeDown;
  Command_BrowserDown;
  Handled := true;
end;

procedure TFileBrowserAddon.EventHandle_BrowserMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TreeView.MoveTreeUp;
  Command_BrowserUp;
  Handled := true;
end;

procedure TFileBrowserAddon.EventHandle_GetNodeBitmap(Sender: TObject; Node: TVamTreeViewNode; var Bitmap: TBitmap);
var
  NodeData:PNodeData;
begin
  if assigned(OnGetNodeBitmap) then
  begin
    NodeData := Node.Data;

    // NOTE: HACK: At the moment we aren't getting bitmaps for ntSpecial types of nodes.
    // Ideally the OnGetNodeBitmap() function should have more information about the node
    // and be able to make the decision itself. This method here is just an event handler.
    if NodeData.NodeType <> ntSpecial
      then OnGetNodeBitmap(self, NodeData.FileName, Bitmap);
  end;
end;

procedure TFileBrowserAddon.RefreshTreeView;
var
  c1: Integer;
begin
  for c1 := 0 to TreeView.RootNodeCount-1 do
  begin
    if TreeView.RootNodes[c1].Expanded then
    begin
      RefreshNode(TreeView.RootNodes[c1]);
    end;
  end;

  TreeView.CalcTreeDimensions;
  TreeView.Invalidate;
end;

procedure TFileBrowserAddon.RefreshNode(Node: TVamTreeViewNode);
var
  c1: Integer;
  nd, Data, ChildData : PNodeData;
  ChildNode : TVamTreeViewNode;
  FolderResults, FileResults : TStringList;
  Dir : string;
begin
  for c1 := Node.ChildCount-1 downto 0 do
  begin
    if Node.Child[c1].Expanded
      then RefreshNode(Node.Child[c1]);
  end;

  //TODO:HIGH currently the refresh node only refreshes files. it should be updated to refresh
  // folders as well.
  // Maybe a simple way to go about refreshing the folder list would be to search
  // for folders, filter etc. Then check if the folder nodes are different to the
  // actual found folder count and folder names. If they are different then we
  // refresh the folders as well.

  // === First delete all the "file" nodes ====

  for c1 := Node.ChildCount-1 downto 0 do
  begin
    ChildNode := Node.Child[c1];

    nd := ChildNode.Data;
    if nd^.NodeType = TNodeType.ntFile then
    begin
      TreeView.DeleteNode(ChildNode, false);
    end;
  end;

  // === Look for files on disk. ====

  FolderResults  := TStringList.Create;
  FolderResults.Sorted := true;
  FolderResults.CaseSensitive := false;
  AutoFree(@FolderResults);

  FileResults := TStringList.Create;
  FileResults.Sorted := true;
  FileResults.CaseSensitive := false;
  AutoFree(@FileResults);

  Data := Node.Data;
  Dir := includeTrailingPathDelimiter(Data^.FileName);



  // First find the folders.
  FindOnlyFolders(Dir, FolderResults);
  FindOnlyFiles(Dir, FileResults,'*.*');

  if assigned(OnFilterNodes) then
  begin
    OnfilterNodes(self, Dir, FolderResults, FileResults);
  end;

  {
  for c1 := 0 to FolderResults.Count - 1 do
  begin
    ChildNode := TreeView.CreateNode(Node);
    ChildNode.Caption := FolderResults[c1];

    ChildData              := ChildNode.Data;
    s := IncludeTrailingPathDelimiter(Dir + FolderResults[c1]);
    ChildData^.FileName    := s;
    ChildData^.NodeType    := ntFolder;
    ChildData^.CanExpand   := true;
  end;
  }

  for c1 := 0 to FileResults.Count - 1 do
  begin
    ChildNode := TreeView.CreateNode(Node);
    ChildNode.Caption := FileResults[c1];

    ChildData              := ChildNode.Data;
    ChildData^.FileName    := Dir + FileResults[c1];
    ChildData^.NodeType    := ntFile;
    ChildData^.CanExpand   := false;
  end;

end;





end.
