unit FarScape.Scene;

interface

uses
  Contnrs,
  Types,
  FarScape.CustomControl,
  FarScape.ControlHelper,
  FarScape.PureInterfacedObject;

type
  TSceneElement = class
  public
    Control : TFarScapeControl;
    // The scene element caches some commonly used values.
    AbsoluteBoundsRect : TRect; // TODO:HIGH. Think about removing AbsoluteBoundsRect from here. Perhaps it's not needed. If it is needed, it might
                                // be better to add it to the control class itself.
    IsShowing : boolean;
    procedure Update(Source : TFarScapeControl); overload;
    procedure Update; overload;
  end;

  TScene = class(TPureInterfacedObject, IFarScapeScene)
  private
    function GetElementCount: integer;
    function GetElementInfo(Index: integer): TSceneElement;
  protected
    ElementList : TObjectList;
    SceneRoot : TFarScapeControl;
  public
    constructor Create(const aSceneRoot : TFarScapeControl);
    destructor Destroy; override;

    procedure RebuildScene;
    procedure UpdateScene;

    property ElementCount : integer read GetElementCount;
    property Element[Index : integer] : TSceneElement read GetElementInfo;

    function GetElementAt(const X, Y : integer):TSceneElement;

  end;

implementation

procedure AddControlsToList(List : TObjectList; const FSC : TFarScapeControl);
var
  c1: Integer;
  c : TFarScapeControl;
begin
  List.Add(FSC);
  for c1 := 0 to FSC.ControlCount-1 do
  begin
    c := FSC.Control[c1];
    AddControlsToList(List, c);
  end;
end;

{ TSceneElement }

procedure TSceneElement.Update(Source: TFarScapeControl);
begin
  self.Control := Source;
  Update;
end;


procedure TSceneElement.Update;
begin
  self.AbsoluteBoundsRect := self.Control.GetAbsoluteRect;
  self.IsShowing := self.Control.IsShowing;
end;

{ TScene }

constructor TScene.Create(const aSceneRoot : TFarScapeControl);
begin
  assert(assigned(aSceneRoot));

  ElementList := TObjectList.Create;
  ElementList.OwnsObjects := true;

  SceneRoot := aSceneRoot;
end;

destructor TScene.Destroy;
begin
  ElementList.Free;
  inherited;
end;

function TScene.GetElementCount: integer;
begin
  result := ElementList.Count;
end;

function TScene.GetElementInfo(Index: integer): TSceneElement;
begin
  result := ElementList[Index] as TSceneElement;
end;

procedure TScene.RebuildScene;
var
  c : TFarScapeControl;
  c1: Integer;
  ElementInfo : TSceneElement;
  ControlList : TObjectList;
begin
  ElementList.Clear;

  ControlList := TObjectList.Create;
  ControlList.OwnsObjects := false;

  try
    for c1 := 0 to SceneRoot.ControlCount-1 do
    begin
      c := SceneRoot.Control[c1];
      AddControlsToList(ControlList, c);
    end;

    for c1 := 0 to ControlList.Count-1 do
    begin
      ElementInfo := TSceneElement.Create;
      ElementInfo.Update( (ControlList[c1] as TFarScapeControl) );
      ElementList.Add(ElementInfo);
    end;

  finally
    ControlList.Free;
  end;
end;

procedure TScene.UpdateScene;
var
  c1 : integer;
begin
  for c1 := 0 to ElementCount-1
    do Element[c1].Update;
end;

function TScene.GetElementAt(const X, Y: integer): TSceneElement;
var
  el : TSceneElement;
  c1: Integer;
begin
  for c1 := ElementCount-1 downto 0 do
  begin
    el := Element[c1];
    if (el.IsShowing) and (el.AbsoluteBoundsRect.Contains(Point(x,y))) then exit(el);
  end;
  // If we make it this far, no control is found at point.
  result := nil;
end;





end.
