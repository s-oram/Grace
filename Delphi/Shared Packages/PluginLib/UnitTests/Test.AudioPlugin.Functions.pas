unit Test.AudioPlugin.Functions;

interface

uses
  WatchTower,
  AudioPlugin.Functions;


type
  TAudioPluginFunctions = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure CalcNewEditorSize_TestA;

    [Test]
    procedure CalcNewEditorSize_TestB;

    [Test]
    procedure CalcNewEditorSize_TestC;
  end;


implementation

uses
  Types,
  WatchTower.Confirm;

{ TAudioPluginFunctions }

procedure TAudioPluginFunctions.CalcNewEditorSize_TestA;
var
  InitialRatio : double;
  NewRatio : double;
  CurrentWidth, CurrentHeight, OffsetX, OffsetY : integer;
  NewSize : TSize;
begin
  CurrentWidth  := 300;
  CurrentHeight := 400;
  OffsetX := 50;
  OffsetY := 50;
  InitialRatio := CurrentWidth / CurrentHeight;
  NewSize := CalcNewEditorSize(InitialRatio, CurrentWidth, CurrentHeight, OffsetX, OffsetY);

  NewRatio := NewSize.cx / NewSize.cy;

  if abs(NewRatio - InitialRatio) > 0.1 then Confirm.Fail( 'Something went wrong. Ratio is not maintained.' );
end;

procedure TAudioPluginFunctions.CalcNewEditorSize_TestB;
var
  InitialRatio : double;
  NewRatio : double;
  CurrentWidth, CurrentHeight, OffsetX, OffsetY : integer;
  NewSize : TSize;
begin
  CurrentWidth  := 400;
  CurrentHeight := 300;
  OffsetX := 50;
  OffsetY := 50;
  InitialRatio := CurrentWidth / CurrentHeight;
  NewSize := CalcNewEditorSize(InitialRatio, CurrentWidth, CurrentHeight, OffsetX, OffsetY);

  NewRatio := NewSize.cx / NewSize.cy;

  if abs(NewRatio - InitialRatio) > 0.1 then Confirm.Fail( 'Something went wrong. Ratio is not maintained.' );
end;

procedure TAudioPluginFunctions.CalcNewEditorSize_TestC;
var
  InitialRatio : double;
  NewRatio : double;
  CurrentWidth, CurrentHeight, OffsetX, OffsetY : integer;
  NewSize : TSize;
begin
  CurrentWidth  := 400;
  CurrentHeight := 300;
  OffsetX := -50;
  OffsetY := -50;
  InitialRatio := CurrentWidth / CurrentHeight;
  NewSize := CalcNewEditorSize(InitialRatio, CurrentWidth, CurrentHeight, OffsetX, OffsetY);

  NewRatio := NewSize.cx / NewSize.cy;

  if abs(NewRatio - InitialRatio) > 0.1 then Confirm.Fail( 'Something went wrong. Ratio is not maintained.' );
end;

end.
