unit FarScape.Scene;

interface

uses
  Contnrs,
  Types,
  FarScape.Event,
  FarScape.Events,
  FarScape.CustomControl,
  FarScape.ControlHelper,
  FarScape.PureInterfacedObject;

type
  // TODO:MED Think about adding TScene to the root control.
  // Is there any reason it needs to be created independently.

  TSceneElement = class
  public
    Control : TFarScapeControl;
    // The scene element caches some commonly used values.
    AbsoluteBoundsRect : TRect; // TODO:HIGH. Think about removing AbsoluteBoundsRect from here. Perhaps it's not needed. If it is needed, it might
                                // be better to add it to the control class itself.
    IsShowing : boolean;
    NamePath : string;
    procedure Update(Source : TFarScapeControl); overload;
    procedure Update; overload;
  end;

  TScene = class(TPureInterfacedObject)
  private
    function GetElementCount: integer;
    function GetElementInfo(Index: integer): TSceneElement;
  protected
    ElementList : TObjectList;
    SceneRoot : TFarScapeControl;

    procedure HandleSceneRootEvents(const ev : TFarScapeEvent);
  public
    constructor Create(const aSceneRoot : TFarScapeControl);
    destructor Destroy; override;

    procedure RebuildScene;
    procedure UpdateScene;

    property ElementCount : integer read GetElementCount;
    property Element[Index : integer] : TSceneElement read GetElementInfo;

    function GetElementAt(const X, Y : integer):TSceneElement;

    function FindElementByNamePath(const NamePath : string):TSceneElement;
    function FindControlByNamePath(const NamePath : string):TFarScapeControl;

  end;

implementation

uses
  StrUtils;

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
  self.IsShowing  := self.Control.IsShowing;
  self.NamePath   := self.Control.GetNamePath;
end;

{ TScene }

constructor TScene.Create(const aSceneRoot : TFarScapeControl);
begin
  assert(assigned(aSceneRoot));

  ElementList := TObjectList.Create;
  ElementList.OwnsObjects := true;

  SceneRoot := aSceneRoot;

  // TODO:MED instead of listening for all event types, limit to required event types.
  SceneRoot.AddEventListener([], HandleSceneRootEvents);
end;

destructor TScene.Destroy;
begin
  SceneRoot.RemoveEventListener(HandleSceneRootEvents);

  ElementList.Free;
  inherited;
end;

function TScene.FindControlByNamePath(const NamePath: string): TFarScapeControl;
var
  el : TSceneElement;
begin
  el := FindElementByNamePath(NamePath);
  if assigned(el)
    then result := el.Control
    else result := nil;

end;

function TScene.FindElementByNamePath(const NamePath: string): TSceneElement;
var
  c1: Integer;
begin
  assert(NamePath <> '');
  for c1 := 0 to ElementList.Count-1 do
  begin
    if EndsText(NamePath, Element[c1].NamePath)
      then exit(Element[c1]);
  end;

  // If we make it this far, no matching element has been found.
  result := nil;
end;

function TScene.GetElementCount: integer;
begin
  result := ElementList.Count;
end;

function TScene.GetElementInfo(Index: integer): TSceneElement;
begin
  result := ElementList[Index] as TSceneElement;
end;

procedure TScene.HandleSceneRootEvents(const ev: TFarScapeEvent);
begin
  {
  if ev.EventClass = TChildAddedEvent then
  begin
    self.RebuildScene;
  end;

  if ev.EventClass = TChildRemovedEvent then
  begin
    self.RebuildScene;
  end;

  if ev.EventClass = TControlBoundsChangedEvent then
  begin
    self.UpdateScene;
  end;

  if ev.EventClass = TControlNameChangedEvent then
  begin
    self.UpdateScene;
  end;
  }
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
    // Add all child controls to control list.
    for c1 := 0 to SceneRoot.ControlCount-1 do
    begin
      c := SceneRoot.Control[c1];
      AddControlsToList(ControlList, c);
    end;

    // Update element info for all controls.
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
