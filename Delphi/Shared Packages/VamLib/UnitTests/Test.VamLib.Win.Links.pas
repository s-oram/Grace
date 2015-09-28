unit Test.VamLib.Win.Links;

interface

uses
  WatchTower;

type
  TVamLib_Win_Links_Test = class(TWatchTowerTest)
  private
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
  Confirm.IsTrue(DirectoryExists(TestDataDirectory), 'Test data directory not found.');
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
  LinkFile   := TestDataDirectory + '\VamLib\Test.VamLib.Win.Links.pas\Link To Target File.lnk';
  TargetFile := 'S:\Delphi\Shared Packages\WatchTowerTestData' + '\VamLib\Test.VamLib.Win.Links.pas\Target File.txt';

  Confirm.IsTrue(FileExists(LinkFile));

  x := GetLinkTarget(LinkFile);

  Confirm.IsTrue(SameText(x, TargetFile), 'Target File is not returned correctly.');
end;

procedure TVamLib_Win_Links_Test.ResolveLinkTarget_Test;
var
  LinkFile : string;
  TargetFile : string;
  x : string;
begin
  LinkFile   := TestDataDirectory + '\VamLib\Test.VamLib.Win.Links.pas\Original Target.lnk';
  TargetFile := 'Original Target Renamed.txt';

  Confirm.IsTrue(FileExists(LinkFile));

  x := ResolveLinkTarget(LinkFile);
  Confirm.IsTrue(x <> '');

  x := ExtractFileName(x);
  Confirm.IsTrue((x = 'Original Target Renamed.txt') or (x = 'Original Target.txt'));
end;






end.
