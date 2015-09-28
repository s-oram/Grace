unit Test.Lucidity.ProgramFileUtils;

interface

uses
  WatchTower;

type
  TLucidity_ProgramFileUtils = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure FindPossibleSamplePaths_TestA;

    [Test]
    procedure FindPossibleSamplePaths_TestB;

    [Test]
    procedure FindPossibleSamplePaths_TestC;

    [Test]
    procedure FindPossibleSamplePaths_TestD;
  end;

implementation

uses
  SysUtils,
  WatchTower.Confirm,
  VamLib.MoreTypes,
  Lucidity.ProgramFileUtils;

{ TLucidity_ProgramFileUtils }

procedure TLucidity_ProgramFileUtils.FindPossibleSamplePaths_TestA;
var
  Paths : TArrayOfString;
begin
  Paths := FindPossibleSamplePaths('C:\MyDrumKit.lpg','sample.wav');
  Confirm.IsTrue(SameText(Paths[0], 'sample.wav'));
  Confirm.IsTrue(SameText(Paths[1], 'C:\MyDrumKit samples\sample.wav'));
  Confirm.IsTrue(SameText(Paths[2], 'C:\sample.wav'));
  Confirm.IsTrue(SameText(Paths[3], ''));
end;

procedure TLucidity_ProgramFileUtils.FindPossibleSamplePaths_TestB;
var
  Paths : TArrayOfString;
begin
  Paths := FindPossibleSamplePaths('C:\MyDrumKit.lpg','..\JB\sample.wav');
  Confirm.IsTrue(SameText(Paths[0], '..\JB\sample.wav'));
  Confirm.IsTrue(SameText(Paths[1], 'C:\MyDrumKit samples\sample.wav'));
  Confirm.IsTrue(SameText(Paths[2], 'C:\JB\sample.wav'));
  Confirm.IsTrue(SameText(Paths[3], ''));
end;

procedure TLucidity_ProgramFileUtils.FindPossibleSamplePaths_TestC;
var
  Paths : TArrayOfString;
begin
  Paths := FindPossibleSamplePaths('C:\MyDrumKit.lpg','\..\JB\sample.wav');
  Confirm.IsTrue(SameText(Paths[0], '\..\JB\sample.wav'));
  Confirm.IsTrue(SameText(Paths[1], 'C:\MyDrumKit samples\sample.wav'));
  Confirm.IsTrue(SameText(Paths[2], 'C:\JB\sample.wav'));
  Confirm.IsTrue(SameText(Paths[3], ''));
end;

procedure TLucidity_ProgramFileUtils.FindPossibleSamplePaths_TestD;
var
  Paths : TArrayOfString;
begin
  Paths := FindPossibleSamplePaths('C:\MyDrumKit.lpg','/../JB\sample.wav');
  Confirm.IsTrue(SameText(Paths[0], '\..\JB\sample.wav'));
  Confirm.IsTrue(SameText(Paths[1], 'C:\MyDrumKit samples\sample.wav'));
  Confirm.IsTrue(SameText(Paths[2], 'C:\JB\sample.wav'));
  Confirm.IsTrue(SameText(Paths[3], ''));
end;

end.
