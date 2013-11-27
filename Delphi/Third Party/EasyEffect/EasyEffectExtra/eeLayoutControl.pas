unit eeLayoutControl;

interface

uses
  TypInfo, NativeXML, Classes, Controls, Contnrs,
  vg_scene;

type
  TLayOutData = class
  private
  protected
    Data : TNativeXML;
    cld  : TXmlNode;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Clear;

    procedure LoadLayout(const FileName:string);
    procedure LoadLayoutFromStream(Stream:TStream);
    procedure SaveLayout(const FileName:string);

    function ControlDataFindOrCreate(aControl:TObject):TXmlNode;
    function ControlDataFindByName(aControlName:string):TXmlNode;

    procedure StoreProperty(c:TObject; PropName:string);
    procedure StoreCompoundProperty(c:TObject; PropName:string);

    procedure StoreLayoutProperties(c:TvgVisualObject; IncludeChildren:boolean = false);
  end;


  // NOTE: This TLayoutControl can not create controls. It can only apply 'layout' data to
  // an existing set of pre-created controls.


  TLayoutControl = class
  private
    fLayoutData : TLayoutData;
  protected
    //ControlList contains references to all controls that layout data will apply itself to.
    ControlList : TObjectList;
  public
    constructor Create;
	  destructor Destroy; override;

    //Updates the form/compoent using layout information from the layout data.
    procedure ApplyLayout;

    procedure AddControl(c:TControl); overload;         //for regular delphi controls
    procedure AddControl(c:TvgVisualObject; AddChildren:boolean = false); overload;  //for VGScene controls

    property LayoutData : TLayoutData read fLayoutData write fLayoutData;
  end;

implementation

uses
  SysUtils;


function GetCompoundPropValue(Instance: TObject; const PropName: string): Variant;
var
  elements:TStringList;
  c1: Integer;
  pName  : string;
  pValue : string;
  pk     : TTypeKind;
  obj    : TObject;
begin
  elements := TStringList.Create;
  try
    //Explode the compound property name
    elements.Delimiter     := '.';
    elements.DelimitedText := PropName;

    c1     := 0;
    pValue := '';
    obj    := Instance;
    while (pValue = '') and (c1 < elements.Count) do
    begin
      pName := elements[c1];
      pk    := PropType(obj, pName);

      if pk = tkClass then
      begin
        obj := GetObjectProp(obj, pName);
        inc(c1);
      end else
      begin
        pValue := GetPropValue(obj, pName);
      end;
    end;
  finally
    result := pValue;
    elements.Free;
  end;
end;



procedure SetCompoundPropValue(Instance: TObject; const PropName: string;  const Value: Variant);
// NOTE: This has not been tested yet!!!!! 
var
  elements :TStringList;
  c1       : Integer;
  pName    : string;
  pk       : TTypeKind;
  obj      : TObject;
  IsSet    : boolean;
begin
  elements := TStringList.Create;
  try
    //Explode the compound property name
    elements.Delimiter     := '.';
    elements.DelimitedText := PropName;

    c1     := 0;
    IsSet  := false;
    obj    := Instance;

    while (IsSet = false) and (c1 < elements.Count) do
    begin
      pName := elements[c1];
      pk    := PropType(obj, pName);

      if pk = tkClass then
      begin
        obj := GetObjectProp(obj, pName);
        inc(c1);
      end else
      begin
        IsSet := true;
        SetPropValue(obj, pName, Value);
      end;
    end;
  finally
    elements.Free;
  end;
end;


{ TLayOutData }

constructor TLayOutData.Create;
begin
  Data := TNativeXML.Create;
  Clear;

end;

destructor TLayOutData.Destroy;
begin
  Data.Free;
  inherited;
end;

procedure TLayOutData.Clear;
var
  Name, Value:string;
begin
  Data.Clear;

  Data.CreateName('root');

  Name := 'FileType';
  Value := 'GUI_Layout';
  Data.Root.AttributeAdd(Name, Value);

  Name := 'FileVersion';
  Value := '1';
  Data.Root.AttributeAdd(Name, Value);


  cld := Data.Root.NodeNew('ControlLayoutData');
end;

procedure TLayOutData.LoadLayout(const FileName: string);
begin
  Data.LoadFromFile(FileName);

  cld := nil;
  if assigned(Data.Root) then cld := Data.Root.FindNode('ControlLayoutData');
  if not assigned(cld) then Self.Clear;
end;

procedure TLayOutData.LoadLayoutFromStream(Stream: TStream);
begin
  Data.LoadFromStream(Stream);

  cld := nil;
  if assigned(Data.Root) then cld := Data.Root.FindNode('ControlLayoutData');
  if not assigned(cld) then Self.Clear;
end;

procedure TLayOutData.SaveLayout(const FileName: string);
begin
  Data.XmlFormat := xfReadable;
  Data.SaveToFile(FileName);
end;



procedure TLayOutData.StoreProperty(c: TObject; PropName: string);
var
  ControlData : TXmlNode;
  Value:string;
begin
  ControlData := ControlDataFindOrCreate(c);
  Value := GetPropValue(c, PropName);
  ControlData.AttributeByName[PropName] := Value;
end;

procedure TLayOutData.StoreCompoundProperty(c: TObject; PropName: string);
var
  ControlData : TXmlNode;
  Value:string;
begin
  ControlData := ControlDataFindOrCreate(c);
  Value := GetCompoundPropValue(c, PropName);
  ControlData.AttributeByName[PropName] := Value;
end;

procedure TLayOutData.StoreLayoutProperties(c: TvgVisualObject; IncludeChildren: boolean);
// StoreLayoutProperties stores all the common properties required to save and restore a layout.
var
  c1: Integer;
  obj  : TvgVisualObject;
begin
  StoreProperty(c, 'Width');
  StoreProperty(c, 'Height');
  StoreCompoundProperty(c, 'Position.X');
  StoreCompoundProperty(c, 'Position.Y');

  if IncludeChildren then
  begin
    for c1 := 0 to c.ChildrenCount - 1 do
    begin
      if c.Children[c1] is TvgVisualObject then
      begin
        obj := c.Children[c1] as TvgVisualObject;
        StoreLayoutProperties(obj, true);
      end;
    end;
  end;             
end;



function TLayOutData.ControlDataFindByName(aControlName: string): TXmlNode;
begin
  result := cld.FindNode(aControlName);
end;

function TLayOutData.ControlDataFindOrCreate(aControl: TObject): TXmlNode;
// NOTE: ControlDataNew() looks for existing control property data, if not
// found, it will create a new node to store the control property values. 
var
  Name : string;
  Node:TXmlNode;
begin
  Name := GetPropValue(aControl, 'Name');
  Node := cld.FindNode(Name);
  if not assigned(Node) then Node := cld.NodeNew(Name);
  result := Node;
end;

{ TvgLayoutControl }

constructor TLayoutControl.Create;
begin
  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;


  LayoutData := TLayoutData.Create;
  LayoutData.Clear;

end;

destructor TLayoutControl.Destroy;
begin
  LayoutData.Free;
  ControlList.Free;
  inherited;
end;



procedure TLayoutControl.AddControl(c:TControl);
begin
  //Check if control is in list, if not add it.
  if ControlList.IndexOf(c) = -1 then ControlList.Add(c);

end;

procedure TLayoutControl.AddControl(c: TvgVisualObject; AddChildren: boolean);
// WARNING: This can be a recursive function. 
var
  c1  : Integer;
  obj : TvgVisualObject;
begin
  //Check if control is in list, if not add it.
  if ControlList.IndexOf(c) = -1 then ControlList.Add(c);

  if AddChildren then
  begin
    for c1 := 0 to c.ChildrenCount - 1 do
    begin
      if (c.Children[c1] is TvgVisualObject) then
      begin
        obj := c.Children[c1] as TvgVisualObject;
        AddControl(obj, true);   // <<-------- recursion here!! ------
      end;
    end;
  end;
end;

procedure TLayoutControl.ApplyLayout;
var
  c1, c2 : integer;
  obj : TObject;
  ObjData : TXmlNode;
  ObjName : string;
  pName   : string;
  pValue  : string;
begin
  for c1 := 0 to ControlList.Count - 1 do
  begin
    //Get the object.
    obj := ControlList[c1];

    //Get the object name.
    ObjName := '';
    if ControlList[c1] is TControl        then ObjName := (obj as TControl).Name;
    if ControlList[c1] is TvgVisualObject then ObjName := (obj as TvgVisualObject).Name;
    if ObjName = '' then raise Exception.Create('Unexpected object type.');

    //Get the object data.
    ObjData := LayoutData.ControlDataFindByName(ObjName); //Get control layout data.

    //Read and apply all object properties...
    if assigned(ObjData) then
    begin
      for c2 := 0 to ObjData.AttributeCount - 1 do
      begin
        pName  := ObjData.AttributeName[c2];
        pValue := ObjData.AttributeValueDirect[c2];
        SetCompoundPropValue(obj, pName, pValue);
      end;
    end;
  end;
end;




end.
