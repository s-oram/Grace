unit Test.VamLib.Win.Links;

interface

uses
  WatchTower;

type
  TVamLib_Win_Links_Test = class(TWatchTowerTest)
  private
    TestDataDirectory : string;
  public
    procedure Setup; override;
    procedure TearDown; override;

    [Test]
    procedure GetLinkTarget_Test;

    [Test]
    procedure ResolveLinkTarget_Test;
  end;

implementation

uses
  SysUtils,
  WatchTower.Confirm,
  VamLib.Win.Links;

{ TVamLib_Win_Links_Test }

procedure TVamLib_Win_Links_Test.Setup;

begin
  inherited;
  TestDataDirectory := self.FindTestDataDir('\VamLib\Test.VamLib.Win.Links.pas\');
  Confirm.IsTrue(DirectoryExists(TestDataDirectory),  'Cannot locate test data directory.');
end;

procedure TVamLib_Win_Links_Test.TearDown;
begin
  inherited;
end;

procedure TVamLib_Win_Links_Test.GetLinkTarget_Test;
var
  LinkFile : string;
  TargetFile : string;
  x : string;
begin
  LinkFile   := TestDataDirectory + '\Link To Target File.lnk';
  TargetFile := 'Target File.txt';

  Confirm.IsTrue(FileExists(LinkFile));

  x := GetLinkTarget(LinkFile);

  Confirm.EndsWith(TargetFile, x, true, 'Target File is not returned correctly.');
end;

procedure TVamLib_Win_Links_Test.ResolveLinkTarget_Test;
var
  LinkFile : string;
  TargetFile : string;
  x : string;
begin
  LinkFile   := TestDataDirectory + '\Original Target.lnk';
  TargetFile := 'Original Target Renamed.txt';

  Confirm.IsTrue(FileExists(LinkFile));

  x := ResolveLinkTarget(LinkFile);
  Confirm.IsTrue(x <> '');

  x := ExtractFileName(x);
  Confirm.IsTrue((x = 'Original Target Renamed.txt') or (x = 'Original Target.txt'));
end;






end.
