unit Test.AudioPlugin.ProcessController;

interface

uses
  WatchTower;

type
  TProcessControllerTest = class(TWatchTowerTest)
  private
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure Test1;
  end;

implementation

{ TProcessControllerTest }

procedure TProcessControllerTest.Setup;
begin
  inherited;
end;

procedure TProcessControllerTest.TearDown;
begin
  inherited;
end;

procedure TProcessControllerTest.Test1;
begin

end;

end.
