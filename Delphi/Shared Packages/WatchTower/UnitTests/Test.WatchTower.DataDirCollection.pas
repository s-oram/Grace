unit Test.WatchTower.DataDirCollection;

interface

uses
  WatchTower,
  WatchTower.DataDirCollection;

type
  TDataDirCollection_BasicTest = class(TWatchTowerTest)
  private
    DataDirCollection : TDataDirCollection;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure AddingDirectories;
  end;

implementation

uses
  WatchTower.Confirm;

{ TDataDirCollectionTest }

procedure TDataDirCollection_BasicTest.Setup;
begin
  inherited;
  DataDirCollection := TDataDirCollection.Create;
end;

procedure TDataDirCollection_BasicTest.TearDown;
begin
  inherited;
  if assigned(DataDirCollection) then DataDirCollection.Free;
end;


procedure TDataDirCollection_BasicTest.AddingDirectories;
begin
  Confirm.IsTrue(DataDirCollection.Count = 0);

  DataDirCollection.AddDataDir('JamesBrownIsDead');
  Confirm.IsTrue(DataDirCollection.Count = 1);

  // Adding a direction multiple times shouldn't create duplicate entries.
  DataDirCollection.AddDataDir('JamesBrownIsDead');
  Confirm.IsTrue(DataDirCollection.Count = 1);

  DataDirCollection.AddDataDir('JimiHedrixIsTheBest');
  Confirm.IsTrue(DataDirCollection.Count = 2);
end;


end.
