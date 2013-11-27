unit LucidityGlobals;

interface

var
  LastProgramLoadDir : string;
  LastProgramSaveDir : string;

implementation

initialization
  LastProgramLoadDir := '';
  LastProgramSaveDir := '';

finalization

end.
