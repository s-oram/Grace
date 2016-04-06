unit Test.AudioPlugin;

interface

uses
  WatchTower,
  Mocks.AudioPlugin;

type
  TAudioPluginTest = class(TWatchTowerTest)
  private
    Plug : TMockAudioPlugin;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [_Test]
    procedure ParameterSetGet;
  end;

implementation

uses
  WatchTower.Confirm;

{ TAudioPluginTest }

procedure TAudioPluginTest.Setup;
begin
  inherited;
  // TODO:MED probably should add a mock globals plugin here.
  Plug := TMockAudioPlugin.Create(nil);
end;

procedure TAudioPluginTest.TearDown;
begin
  inherited;
  if assigned(Plug) then Plug.Free;
end;

procedure TAudioPluginTest.ParameterSetGet;
var
  c1: Integer;
begin

end;

end.
