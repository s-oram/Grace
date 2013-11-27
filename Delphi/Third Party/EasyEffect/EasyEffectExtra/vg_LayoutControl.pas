unit vg_LayoutControl;

interface

uses
  vg_scene, NativeXML, Classes, Contnrs, vg_InspectorForm, vam_BaseControls;

type
  TvgLayoutControl = class
  private
    fControlEditEnabled: boolean;
    procedure SetControlEditEnabled(const Value: boolean);
  protected
    EditList:TObjectList;
    LayoutData:TNativeXML;
    InspectorForm : TInspectorForm;
    function FindVgObject(const Name:string; Root:TvgObject):TvgObject;
    procedure UpdateComponent(c:TComponent; LayoutInfo:TXmlNode);

    procedure ControlEditHandler(Sender:TObject);
  public
    constructor Create;
	  destructor Destroy; override;

    procedure ClearLayout;
    procedure StoreObjectProperties(ob:TvgObject);

    procedure LoadLayout(const FileName:string);
    procedure LoadLayoutFromStream(Stream:TStream);
    procedure SaveLayout(const FileName:string);

    procedure UpdateLayout(Root:TvgObject);

    procedure AddObjectToEditList(ob:TvgVamControl);

    property ControlEditEnabled:boolean read fControlEditEnabled write SetControlEditEnabled;
  end;

implementation





{ TvgLayoutControl }

constructor TvgLayoutControl.Create;
var
  Name, Value:string;
  //aNode:TXMLNode;
  //n2 :TXMLNode;
begin
  EditList := TObjectList.Create;
  EditList.OwnsObjects := false;

  LayoutData := TNativeXML.Create;

  LayoutData.CreateName('root');

  Name := 'FileType';
  Value := 'GUI_Layout';
  LayoutData.Root.AttributeAdd(Name, Value);

  Name := 'FileVersion';
  Value := '1';
  LayoutData.Root.AttributeAdd(Name, Value);

  //aNode := LayoutData.Root.NodeNew('Test');
  //n2 := aNode.NodeNew('Height');
  //n2.ValueAsString := '102';

  ControlEditEnabled := false;
end;

destructor TvgLayoutControl.Destroy;
begin
  if assigned(InspectorForm) then InspectorForm.Free;
  
  LayoutData.Free;
  EditList.Free;
  inherited;
end;

procedure TvgLayoutControl.ClearLayout;
var
  Name, Value:string;
begin
  LayoutData.Clear;

  LayoutData.CreateName('root');

  Name := 'FileType';
  Value := 'GUI_Layout';
  LayoutData.Root.AttributeAdd(Name, Value);

  Name := 'FileVersion';
  Value := '1';
  LayoutData.Root.AttributeAdd(Name, Value);
end;



procedure TvgLayoutControl.LoadLayout(const FileName: string);
begin
  LayoutData.LoadFromFile(FileName);
end;

procedure TvgLayoutControl.LoadLayoutFromStream(Stream: TStream);
begin
  LayoutData.LoadFromStream(Stream);
end;

procedure TvgLayoutControl.SaveLayout(const FileName: string);
begin
  LayoutData.XmlFormat := xfReadable;
  LayoutData.SaveToFile(FileName);
end;

procedure TvgLayoutControl.SetControlEditEnabled(const Value: boolean);
var
  c1:integer;
begin
  fControlEditEnabled := Value;


  if (Value = true) and (not assigned(InspectorForm)) then
  begin
    InspectorForm := TInspectorForm.Create(nil);
    InspectorForm.Show;
  end;

  for c1 := 0 to EditList.Count - 1 do
  begin
    (EditList[c1] as TvgVamControl).ShowEditRect := Value;
  end;
  
end;

procedure TvgLayoutControl.UpdateLayout(Root: TvgObject);
var
  c:TComponent;
  c1:integer;
  aNode:TXmlNode;
  Name, Value:string;
begin
  if LayoutData.Root = nil then exit;

  for c1  := 0 to LayoutData.Root.NodeCount-1 do
  begin
    aNode := LayoutData.Root.Nodes[c1];
    Value := aNode.Name;

    if Value <> ''
      then c := FindVgObject(Value, Root)
      else c := nil;


    if assigned(c) then UpdateComponent(c, aNode);
  end;
end;

function TvgLayoutControl.FindVgObject(const Name: string; Root: TvgObject): TvgObject;
var
  c1: Integer;
  xo:TvgObject;
begin
  // WARNING: This is a RECURSIVE function!!

  for c1 := 0 to Root.ChildrenCount - 1 do
  begin
    if Root.Children[c1].Name = Name then
    begin
      result := Root.Children[c1];
      exit; //========================================>>
    end;
  end;


  for c1 := 0 to Root.ChildrenCount - 1 do
  begin
    // WARNING: Recursion here...
    xo := FindVgObject(Name, Root.Children[c1]);

    if assigned(xo) then
    begin
      result := xo;
      exit; //========================================>>
    end;
  end;


  //If the function makes it this far.. the object hasn't been found. return nil.
  result := nil;
end;



procedure TvgLayoutControl.UpdateComponent(c: TComponent; LayoutInfo: TXmlNode);
var
  NodeName : string;
  ChildNode:TXmlNode;
  Value:string;
begin
  if c is TvgVisualObject then
  begin
    NodeName  := 'Height';
    ChildNode := LayoutInfo.NodeByName(NodeName);
    if assigned(ChildNode) then
    begin
      //TODO: It would be could to wrap a 'safe' here...
      (c as TvgVisualObject).Height := ChildNode.ValueAsInteger;
    end;

    NodeName  := 'Width';
    ChildNode := LayoutInfo.NodeByName(NodeName);
    if assigned(ChildNode) then
    begin
      //TODO: It would be could to wrap a 'safe' here...
      (c as TvgVisualObject).Width := ChildNode.ValueAsInteger;
    end;

    NodeName  := 'PosX';
    ChildNode := LayoutInfo.NodeByName(NodeName);
    if assigned(ChildNode) then
    begin
      //TODO: It would be could to wrap a 'safe' here...
      (c as TvgVisualObject).Position.X := ChildNode.ValueAsInteger;
    end;

    NodeName  := 'PosY';
    ChildNode := LayoutInfo.NodeByName(NodeName);
    if assigned(ChildNode) then
    begin
      //TODO: It would be could to wrap a 'safe' here...
      (c as TvgVisualObject).Position.Y := ChildNode.ValueAsInteger;
    end;


  end;

end;

procedure TvgLayoutControl.StoreObjectProperties(ob: TvgObject);
// Writes the object properties to the Layout XML blob.
var
  Node : TXmlNode;
  ChildNode:TXmlNode;
begin
  Node := LayoutData.Root.NodeFindOrCreate(ob.Name);

  if (ob is TvgVisualObject) then
  begin
    ChildNode := Node.NodeFindOrCreate('Height');
    ChildNode.ValueAsInteger := round((ob as TvgVisualObject).Height);

    ChildNode := Node.NodeFindOrCreate('Width');
    ChildNode.ValueAsInteger := round((ob as TvgVisualObject).Width);

    ChildNode := Node.NodeFindOrCreate('PosX');
    ChildNode.ValueAsInteger := round((ob as TvgVisualObject).Position.X);

    ChildNode := Node.NodeFindOrCreate('PosY');
    ChildNode.ValueAsInteger := round((ob as TvgVisualObject).Position.Y);
  end;


end;


procedure TvgLayoutControl.ControlEditHandler(Sender: TObject);
begin
  if ControlEditEnabled then
  begin
    InspectorForm.Inspector1.SelectedObject := (Sender as TComponent);
  end;
end;



procedure TvgLayoutControl.AddObjectToEditList(ob: TvgVamControl);
begin
  EditList.Add(ob);
  (ob as TvgVamControl).OnEdit := ControlEditHandler;
end;





end.
