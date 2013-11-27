unit NativeXmlEx;

interface

uses
  Classes,
  NativeXml;

type
  INodeWiz = interface
    ['{AF6571F5-A2AB-410D-A322-A2C8220AEC67}']
    function FindOrCreateNode(ChildPath : string):TXmlNode;
    function CreateNode(ChildPath : string):TXmlNode;

    function Child(ChildPath:string):TXmlNode;
    function ValueUnicode(ChildPath : string):string;
  end;

  TNodeWiz = class(TInterfacedObject, INodeWiz)
  private
    AnchorNode : TXmlNode;
  public
    constructor Create(aAnchorNode : TXmlNode);


    function Child(ChildPath:string):TXmlNode;

    // Navigates to the final child node, giving preference to existing nodes.
    // Non-existing nodes will be created.
    function FindOrCreateNode(ChildPath : string):TXmlNode;

    //function CreateNode()
    // Navigates to the final child node, giving preference to existing nodes.
    // Non-existing nodes will be created.
    // The final node will always be created.
    function CreateNode(ChildPath : string):TXmlNode;

    function ValueUnicode(ChildPath : string):string;
  end;


function NodeWiz(AnchorNode : TXmlNode):INodeWiz;


procedure FindNodes(const Node : TXmlNode; const NodeName : UTF8String; const List : TList);

implementation

uses
  Types,
  StrUtils;


function NodeWiz(AnchorNode : TXmlNode):INodeWiz;
begin
  assert(assigned(AnchorNode));
  result := TNodeWiz.Create(AnchorNode);
end;

{ NodeWiz }

constructor TNodeWiz.Create(aAnchorNode: TXmlNode);
begin
  AnchorNode := aAnchorNode;
end;

function TNodeWiz.CreateNode(ChildPath: string): TXmlNode;
var
  Elements : TStringDynArray;
  c1: Integer;
  aNode : TXmlNode;
  NodeName : UTF8String;
  RefNode : TXmlNode;
  Index : integer;
begin
  RefNode := AnchorNode;

  Elements := SplitString(ChildPath, '/');

  for c1 := 0 to Length(Elements)-2 do
  begin
    NodeName := UTF8String(Elements[c1]);
    aNode := RefNode.FindNode(NodeName);

    if not assigned(aNode) then
    begin
      aNode := RefNode.NodeNew(NodeName);
    end;

    RefNode := aNode;
  end;

  if Length(Elements) > 0 then
  begin
    Index := Length(Elements)-1;
    NodeName := UTF8String(Elements[Index]);
    aNode := RefNode.NodeNew(NodeName);
    RefNode := aNode;
  end;

  result := RefNode;
end;

function TNodeWiz.FindOrCreateNode(ChildPath: string): TXmlNode;
var
  Elements : TStringDynArray;
  c1: Integer;
  aNode : TXmlNode;
  NodeName : UTF8String;
  RefNode : TXmlNode;
begin
  RefNode := AnchorNode;

  Elements := SplitString(ChildPath, '/');

  for c1 := 0 to Length(Elements)-1 do
  begin
    NodeName := UTF8String(Elements[c1]);
    aNode := RefNode.FindNode(NodeName);

    if not assigned(aNode) then
    begin
      aNode := RefNode.NodeNew(NodeName);
    end;

    RefNode := aNode;
  end;

  result := RefNode;
end;

function TNodeWiz.Child(ChildPath: string): TXmlNode;
var
  Elements : TStringDynArray;
  RefNode : TXmlNode;
  c1 : integer;
  NodeName : UTF8String;
begin
  RefNode := AnchorNode;

  Elements := SplitString(ChildPath, '/');

  for c1 := 0 to Length(Elements)-1 do
  begin
    NodeName := UTF8String(Elements[c1]);
    RefNode := RefNode.FindNode(NodeName);
    if not assigned(RefNode)
      then break;
  end;

  result := RefNode;
end;



function TNodeWiz.ValueUnicode(ChildPath: string): string;
var
  aNode : TXmlNode;
begin
  aNode := Child(ChildPath);
  if assigned(aNode)
    then result := aNode.ValueUnicode
    else result := '';
end;


procedure FindNodes(const Node : TXmlNode; const NodeName : UTF8String; const List : TList);
  // local
  procedure FindNodesRecursive(aNode: TXmlNode; aList: TList);
  var
    i: integer;
    SubNode: TXmlNode;
  begin
    for i := 0 to aNode.NodeCount - 1 do
    begin
      SubNode := aNode.Nodes[i];
      if EndsText(NodeName, SubNode.FullPath)
        then aList.Add(SubNode);
      FindNodesRecursive(SubNode, aList);
    end;
  end;
// main
begin
  List.Clear;
  FindNodesRecursive(Node, List);
end;

end.
