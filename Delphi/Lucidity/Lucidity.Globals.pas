unit Lucidity.Globals;

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
