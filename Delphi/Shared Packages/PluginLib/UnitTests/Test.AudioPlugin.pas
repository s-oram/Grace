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

    [Test]
    procedure ParameterSetGet;
  end;

implementation

uses
  WatchTower.Confirm;

{ TAudioPluginTest }

procedure TAudioPluginTest.Setup;
begin
  inherited;
  Plug := TMockAudioPlugin.Create;
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
  Confirm.IsTrue(Plug.VstParameterCount > 0);

  for c1 := 0 to Plug.VstParameterCount-1 do
  begin
    Plug.VstParameter[c1] := 0.5;
  end;

  for c1 := 0 to Plug.VstParameterCount-1 do
  begin
    Confirm.IsTrue(Plug.VstParameter[c1] = 0.5);
  end;
end;

end.
