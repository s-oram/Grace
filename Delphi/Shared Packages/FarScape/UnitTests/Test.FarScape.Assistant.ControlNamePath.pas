unit Test.FarScape.Assistant.ControlNamePath;

interface

uses
  WatchTower;

type
  TControlNamePathTest = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure IsValidNamePath;

    [Test]
    procedure AscendNamePath;

    [Test]
    procedure LastName;
  end;

implementation

uses
  WatchTower.Confirm,
  FarScape.Assistant.ControlNamePath;

{ TControlNamePathTest }

procedure TControlNamePathTest.IsValidNamePath;
begin
  Confirm.IsTrue(  ControlNamePath.IsValidNamePath('Panel') );
  Confirm.IsTrue(  ControlNamePath.IsValidNamePath('Panel.Knob') );
  Confirm.IsTrue(  ControlNamePath.IsValidNamePath('Panel.Knob.Label') );

  Confirm.IsFalse(  ControlNamePath.IsValidNamePath('Panel.Knob.Label ') );
  Confirm.IsFalse(  ControlNamePath.IsValidNamePath(' Panel.Knob.Label') );
  Confirm.IsFalse(  ControlNamePath.IsValidNamePath('.Panel.Knob.Label') );
  Confirm.IsFalse(  ControlNamePath.IsValidNamePath('Panel.Knob.Label.') );
  Confirm.IsFalse(  ControlNamePath.IsValidNamePath('2Panel.Knob.Label') );
  Confirm.IsFalse(  ControlNamePath.IsValidNamePath('_Panel.Knob.Label') );
  Confirm.IsFalse(  ControlNamePath.IsValidNamePath('Panel.2Knob.3Label') );
  Confirm.IsFalse(  ControlNamePath.IsValidNamePath('Panel.Knob.Label:Panda') );
end;

procedure TControlNamePathTest.AscendNamePath;
begin
  Confirm.IsTrue(  ControlNamePath.AscendNamePathByOne('Panel')                = '' );
  Confirm.IsTrue(  ControlNamePath.AscendNamePathByOne('Panel.Knob')           = 'Panel' );
  Confirm.IsTrue(  ControlNamePath.AscendNamePathByOne('Panel.Knob.Pizza')     = 'Panel.Knob' );
  Confirm.IsTrue(  ControlNamePath.AscendNamePathByOne('Panel.Knob.Pizza.Jam') = 'Panel.Knob.Pizza' );
end;

procedure TControlNamePathTest.LastName;
begin
  Confirm.IsTrue(  ControlNamePath.LastName('Panel')                = 'Panel' );
  Confirm.IsTrue(  ControlNamePath.LastName('Panel.Knob')           = 'Knob' );
  Confirm.IsTrue(  ControlNamePath.LastName('Panel.Knob.Pizza')     = 'Pizza' );
  Confirm.IsTrue(  ControlNamePath.LastName('Panel.Knob.Pizza.Jam') = 'Jam' );
end;



end.
