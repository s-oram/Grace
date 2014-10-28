unit RedFoxReg;

interface

uses
  RedFoxContainer;

procedure Register;

implementation

uses
  Classes, DesignIntf;

procedure Register;
begin
  RegisterComponents('Red Fox Basic', [TRedFoxContainer]);

  //RegisterComponents('Red Fox', [TRedFoxControl]);
  //RegisterComponents('Red Fox Basic', [TRedFoxMyFirstControl]);
  //RegisterComponents('Red Fox Basic', [TRedFoxLabel]);
  //RegisterComponents('Red Fox Basic', [TRedFoxVectorScrollBar]);
  //RegisterComponents('Red Fox Basic', [TRedFoxTreeView]);
  //RegisterComponents('Red Fox Basic', [TRedFoxPaintBox]);
  //RegisterComponents('Red Fox Basic', [TRedFoxDiv]);
  //RegisterComponents('Red Fox Basic', [TRedFoxBasicWinControl]);
  //RegisterComponents('Red Fox Basic', [TRedFoxWinScrollBar]);

  //RegisterComponents('Red Fox VST', [TRedFoxSampleMap]);
  //RegisterComponents('Red Fox VST', [TRedFoxSamplerKeys]);
  //RegisterComponents('Red Fox VST', [TRedFoxNodeEditDisplay]);
  //RegisterComponents('Red Fox VST', [TRedFoxSampleDisplay]);

end;

end.
