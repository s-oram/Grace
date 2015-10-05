unit Test.LucidityBug.SetGuiParameter;

interface

uses
  WatchTower,
  eeGlobals,
  eePlugin;

type
  TLucidityBug_SetGUIPar = class(TWatchTowerTest)
  private
  protected
    Globals : TGlobals;
    Plug : TeePlugin;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure AttempToSetGUIParameter;


  end;

implementation

uses
  SysUtils,
  VamLib.MoreTypes,
  WatchTower.Confirm;

{ TLucidity_SetGUIPar }

procedure TLucidityBug_SetGUIPar.Setup;
begin
  inherited;
  Globals := TGlobals.Create;



end;

procedure TLucidityBug_SetGUIPar.TearDown;
begin
  inherited;
  Globals.Free;
end;

procedure TLucidityBug_SetGUIPar.AttempToSetGUIParameter;
begin
  // BUG:
  // Grace is not setting parameters from the GUI correctly.
  // Reproduce: Create a new
end;



end.
