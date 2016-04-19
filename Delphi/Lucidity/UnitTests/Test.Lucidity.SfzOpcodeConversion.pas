unit Test.Lucidity.SfzOpcodeConversion;

interface

uses
  WatchTower,
  eeGlobals;

type
  TSFZOpcodeConversion = class(TWatchTowerTest)
  private
  protected
  public
    [Test]
    procedure BasicTests;


  end;

implementation

uses
  SysUtils,
  WatchTower.Confirm,
  Lucidity.SfzOpcodeConversion,
  SfzParser.SfzOpcodes,
  VamLib.Utils;



{ TSFZOpcodeConversion }

procedure TSFZOpcodeConversion.BasicTests;
var
  ConvertResult : string;
  x : single;
begin
  ConvertResult := ConvertOpcodeToVoiceParameterValue(TSfzOpcode.volume, '0');
  Confirm.IsTrue(ConvertResult = '0.5');

  ConvertResult := ConvertOpcodeToVoiceParameterValue(TSfzOpcode.volume, '-12');
  x := StrToFloat(ConvertResult);
  Confirm.IsTrue(InRange(x, 0.24, 0.26 ));

  ConvertResult := ConvertOpcodeToVoiceParameterValue(TSfzOpcode.volume, '-24');
  x := StrToFloat(ConvertResult);
  Confirm.IsTrue(InRange(x, 0.124, 0.126 ));
end;

end.
