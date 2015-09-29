unit Test.WatchTower.DataDirCollection;

interface

uses
  WatchTower,
  WatchTower.DataDirCollection;

type
  TDataDirCollectionTest = class(TWatchTowerTest)
  private
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure Fakie;
  end;

implementation

{ TDataDirCollectionTest }

procedure TDataDirCollectionTest.Fakie;
begin

end;

procedure TDataDirCollectionTest.Setup;
begin
  inherited;

end;

procedure TDataDirCollectionTest.TearDown;
begin
  inherited;

end;

end.
