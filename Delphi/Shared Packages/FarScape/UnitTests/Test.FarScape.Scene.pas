unit Test.FarScape.Scene;

interface

uses
  FarScape.CustomControl,
  FarScape.Scene,
  WatchTower;

type
  TTestControl = class(TFarScapeControl)
  end;

  TFarScapeSceneTest = class(TWatchTowerTest)
  protected
    Scene : TScene;
    Root : TFarScapeControl;
    fsc1 : TFarScapeControl;
    fsc2 : TFarScapeControl;
    fsc3 : TFarScapeControl;
    fsc4 : TFarScapeControl;
    fsc5 : TFarScapeControl;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure BuildSceneWithoutFreeingParent;

    [Test]
    procedure CheckElementCountIsCorrectA;

    [Test]
    procedure CheckElementCountIsCorrectB;

    [Test]
    procedure CheckElementOrdering;

    [Test]
    procedure GetElementAt;


  end;



implementation

uses
  WatchTower.Confirm;

{ TFarScapeSceneTest }

procedure TFarScapeSceneTest.Setup;
begin
  inherited;
  Root := TFarScapeControl.Create;
  fsc1 := TFarScapeControl.Create;
  fsc2 := TFarScapeControl.Create;
  fsc3 := TFarScapeControl.Create;
  fsc4 := TFarScapeControl.Create;
  fsc5 := TFarScapeControl.Create;

  Scene := TScene.Create(Root);

  Root.Name := 'Root';
  fsc1.Name := 'FSC1';
  fsc2.Name := 'FSC2';
  fsc3.Name := 'FSC3';
  fsc4.Name := 'FSC4';
  fsc5.Name := 'FSC5';
end;

procedure TFarScapeSceneTest.TearDown;
begin
  inherited;
  Scene.Free;
  Root.Free;
  fsc1.Free;
  fsc2.Free;
  fsc3.Free;
  fsc4.Free;
  fsc5.Free;
end;

procedure TFarScapeSceneTest.BuildSceneWithoutFreeingParent;
begin
  Scene.RebuildScene;
  Confirm.IsTrue(Scene.ElementCount = 0);

  // Confirm the scene can be built without changing the control parent.
  fsc1.Parent := Root;
  Scene.RebuildScene;
  Confirm.IsTrue(fsc1.Parent = Root);
end;

procedure TFarScapeSceneTest.CheckElementCountIsCorrectA;
begin
  fsc1.Parent := Root;
  Scene.RebuildScene;
  Confirm.IsTrue(Scene.ElementCount = 1);

  fsc1.Parent := Root;
  fsc2.Parent := Root;
  Scene.RebuildScene;
  Confirm.IsTrue(Scene.ElementCount = 2);

  fsc1.Parent := Root;
  fsc2.Parent := Root;
  fsc3.Parent := Root;
  Scene.RebuildScene;
  Confirm.IsTrue(Scene.ElementCount = 3);
end;

procedure TFarScapeSceneTest.CheckElementCountIsCorrectB;
begin
  fsc1.Parent := Root;
  fsc2.Parent := fsc1;
  fsc3.Parent := fsc2;
  Scene.RebuildScene;

  Confirm.IsTrue(Scene.ElementCount = 3);

  Confirm.IsTrue(Scene.Element[0].Control = fsc1);
  Confirm.IsTrue(Scene.Element[1].Control = fsc2);
  Confirm.IsTrue(Scene.Element[2].Control = fsc3);
end;

procedure TFarScapeSceneTest.CheckElementOrdering;
begin
  fsc1.Parent := Root;
  fsc4.Parent := Root;

  fsc2.Parent := fsc1;
  fsc3.Parent := fsc2;

  Scene.RebuildScene;

  Confirm.IsTrue(Scene.ElementCount = 4);

  Confirm.IsTrue(Scene.Element[0].Control = fsc1);
  Confirm.IsTrue(Scene.Element[1].Control = fsc2);
  Confirm.IsTrue(Scene.Element[2].Control = fsc3);
  Confirm.IsTrue(Scene.Element[3].Control = fsc4);
end;

procedure TFarScapeSceneTest.GetElementAt;
var
  el : TSceneElement;
begin
  Root.SetBounds(0,0,100,100);
  fsc1.SetBounds(10,10,50,50);
  fsc1.Parent := Root;
  Scene.RebuildScene;

  el := Scene.GetElementAt(12,12);
  Confirm.IsTrue(el.Control = fsc1);
end;

end.
