unit Test.VamLib.ArrayUtils;

interface

uses
  WatchTower,
  VamLib.ArrayUtils;

type
  TArrayUtilTest = class(TWatchTowerTest)
  public
    procedure Setup; override;
    procedure TearDown; override;
  end;

implementation

uses
  VamLib.UniqueID,
  WatchTower.Confirm;

{ TArrayUtilTest }

procedure TArrayUtilTest.Setup;
begin
  inherited;

end;

procedure TArrayUtilTest.TearDown;
begin
  inherited;
end;


end.
