unit Test.VamLib.Utils;

interface

uses
  WatchTower,
  VamLib.Utils;

type
  TVamLibUtilsTest = class(TWatchTowerTest)
  private
  public
    [Test]
    procedure VstFloatToIntConversion;

    [Test]
    procedure MidiNoteToTextConversion;
  end;


implementation

uses
  WatchTower.Confirm;

{ TVamLibUtilsTest }

procedure TVamLibUtilsTest.VstFloatToIntConversion;
var
  x1 : single;
  fr : integer;
  c1: Integer;
begin
  for c1 := 0 to 1000 do
  begin
    x1 := IntToVstFloat(c1, 0, 1000);
    fr := VstFloatToInt(x1, 0, 1000);
    Confirm.IsTrue( fr = c1  );
  end;


  for c1 := 0 to 100 do
  begin
    x1 := IntToVstFloat(c1, -100, 3500);
    fr := VstFloatToInt(x1, -100, 3500);
    Confirm.IsTrue( fr = c1  );
  end;
end;

procedure TVamLibUtilsTest.MidiNoteToTextConversion;
var
  Text : string;
begin
  Text := MidiNoteToText(0);
  Confirm.IsTrue(  Text = 'C-2'  );

  Text := MidiNoteToText(127);
  Confirm.IsTrue(  Text = 'G8'  );
end;



end.
