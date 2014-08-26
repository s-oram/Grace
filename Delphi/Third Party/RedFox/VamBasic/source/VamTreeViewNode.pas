unit VamTreeViewNode;

interface

uses
  Classes, Contnrs;

type
  PVamTreeViewNode = ^TVamTreeViewNode;
  TVamTreeViewNode = class
  private
    fChildList:TObjectList;
    fCaption: string;
    fData:pointer;
    fParentNode: TVamTreeViewNode;
    fExpanded: boolean;
    fVisible: boolean;
    fLeft: integer;
    fTop: integer;
    fDepth: integer;
    fWidth: integer;
    fHeight: integer;
    fIsRootNode: boolean;
    function GetData: pointer;
    procedure SetData(const Value: pointer);
    function GetChild(Index: integer): TVamTreeViewNode;
    procedure SetChild(Index: integer; const Value: TVamTreeViewNode);
    function GetChildCount: integer;
    function GetHasChildren: boolean;
    function GetNodeIndex: integer;
  protected
    procedure SetIsRootNode(Value : boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure SaveToStream(Stream:TStream);
    procedure LoadFromStream(Stream:TStream);

    property Caption:string read fCaption write fCaption;
    property Data:pointer read GetData write SetData;

    property ParentNode:TVamTreeViewNode read fParentNode write fParentNode;

    property Child[Index :integer]:TVamTreeViewNode read GetChild write SetChild;
    property ChildCount  :integer read GetChildCount;
    property HasChildren :boolean read GetHasChildren;

    property ChildList:TObjectList read fChildList write fChildList;

    property Expanded :boolean read fExpanded write fExpanded;
    property Visible  :boolean read fVisible  write fVisible;
    property Left     :integer read fLeft     write fLeft;
    property Top      :integer read fTop      write fTop;
    property Width    :integer read fWidth    write fWidth;
    property Height   :integer read fHeight   write fHeight;

    property Depth    :integer read fDepth    write fDepth;
    property IsRootNode : boolean read fIsRootNode;
    property NodeIndex : integer read GetNodeIndex;
  end;

  TETNodeEx = class(TVamTreeViewNode)
  public
    property ChildList;
  end;

implementation

uses
  SysUtils, Dialogs;

{ TETNode }

constructor TVamTreeViewNode.Create;
begin
  Left := 0;
  Top := 0;
  Visible := false;
  Expanded := false;
  Caption := 'Item';
  ParentNode := nil;
  fIsRootNode := false;

  fData := nil;

  ChildList := TObjectList.Create;
  ChildList.OwnsObjects := false;

  Depth := 0;
end;

destructor TVamTreeViewNode.Destroy;
begin
  // NOTE: The node data should be free'ed before freeing the node. As a sanity check,
  // the data pointer should be set to nil as well.
  if assigned(fData) then raise Exception.Create('fData is not nil.');
  ChildList.Free;
  inherited;
end;

function TVamTreeViewNode.GetChild(Index: integer): TVamTreeViewNode;
begin
  result := ChildList[Index] as TVamTreeViewNode;
end;

function TVamTreeViewNode.GetChildCount: integer;
begin
  result := fChildList.Count;
end;

function TVamTreeViewNode.GetData: pointer;
begin
  result := fData;
end;

function TVamTreeViewNode.GetHasChildren: boolean;
begin
  result := (ChildList.Count > 0);
end;

function TVamTreeViewNode.GetNodeIndex: integer;
begin
  if not assigned(fParentNode)
    then result := -1
    else result := fParentNode.ChildList.IndexOf(self);
end;

procedure TVamTreeViewNode.SetChild(Index: integer; const Value: TVamTreeViewNode);
begin
  ChildList[Index] := Value;
end;

procedure TVamTreeViewNode.SetData(const Value: pointer);
begin
  fData := Value;
end;

procedure TVamTreeViewNode.SetIsRootNode(Value: boolean);
begin
  fIsRootNode := Value;
end;

procedure TVamTreeViewNode.LoadFromStream(Stream: TStream);
var
  i:integer;
  s:AnsiString;
  //p:pointer;
  b:boolean;
begin
  //read the caption
  Stream.Read(i,SizeOf(i));
  SetLength(s, i);
  Stream.Read(s[1],i);
  Caption := string(s);

  //Load the node state.
  Stream.Read(b,SizeOf(b));
  Expanded := b;

  //load the node top position.
  Stream.read(i,SizeOf(i));
  Top := i;

end;

procedure TVamTreeViewNode.SaveToStream(Stream: TStream);
var
  i:integer;
  s:AnsiString;
  b:boolean;
begin
  //Store the caption
  s := AnsiString(Caption);
  i := Length(s) * StringElementSize(s);
  Stream.Write(i,SizeOf(i));
  Stream.Write(s[1],i);

  //Store the node state
  b := Expanded;
  Stream.Write(b,SizeOf(b));

  //Store the node top position.
  i := Top;
  Stream.Write(i,SizeOf(i));

end;


end.
