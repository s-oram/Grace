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
  LinkFile   := 'S:\Delphi\Shared Packages\VamLib\UnitTests\Test.VamLib.Win.Links\Link To Target File.lnk';
  TargetFile := 'S:\Delphi\Shared Packages\VamLib\UnitTests\Test.VamLib.Win.Links\Target File.txt';

  Confirm.IsTrue(FileExists(LinkFile));
  Confirm.IsTrue(FileExists(TargetFile));

  x := GetLinkTarget(LinkFile);


end;



end.
