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
    procedure GetLinkTarget;
  end;

implementation

uses
  VamLib.Win.Links;

{ TVamLib_Win_Links_Test }

procedure TVamLib_Win_Links_Test.GetLinkTarget;
begin

end;

procedure TVamLib_Win_Links_Test.Setup;
begin
  inherited;

end;

procedure TVamLib_Win_Links_Test.TearDown;
begin
  inherited;

end;

end.
