unit eeStringConversion;

interface


// This function converts a string to a PAnsiChar
// If the output is not the same, an exception is raised
// Author: nogabel@hotmail.com
// Source: http://stackoverflow.com/questions/283759/convert-string-to-pansichar-in-delphi-2009
function ToPAnsiChar(stringVar : string) : PAnsiChar;

implementation

uses
  SysUtils;

function ToPAnsiChar(stringVar : string) : PAnsiChar;
Var
  AnsString : AnsiString;
  InternalError : Boolean;
begin
  InternalError := false;
  Result := '';
  try
    if stringVar <> '' Then
    begin
       AnsString := AnsiString(StringVar);
       Result := PAnsiChar(PAnsiString(AnsString));
    end;
  Except
    InternalError := true;
  end;
  if InternalError or (String(Result) <> stringVar) then
  begin
    Raise Exception.Create('Conversion from string to PAnsiChar failed!');
  end;
end;

end.
