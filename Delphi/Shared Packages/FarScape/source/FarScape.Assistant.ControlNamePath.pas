unit FarScape.Assistant.ControlNamePath;

interface

type
  // Name paths are used to locate named controls.
  // Valid name path examples:
  //   Panel
  //   Panel.Knob
  //   Panel.Knob.Label
  ControlNamePath = record
  private
  public
    // check if a string is a valid name path.
    class function IsValidNamePath(const NamePath : string):boolean; static;

    // Returns the entire name path minus the last name. Returns an empty string if there is only one name in the path.
    class function AscendNamePathByOne(const NamePath : string):string; static;

    // Returns the last name in the path. Returns the input if there is only one name in the path.
    class function LastName(const NamePath : string):string; static;
  end;

implementation

uses
  RegularExpressions,
  StrUtils;

// PCRE Regex Tester:
// https://regex101.com/

// Go Regex Tester:
// https://regex-golang.appspot.com/


{ ControlNamePath }

class function ControlNamePath.IsValidNamePath(const NamePath: string): boolean;
const
  rex_ValidNamePath = '^(?:[a-zA-Z]+\w*)(?:\.[a-zA-Z]+\w*)*$';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_ValidNamePath);
  mr := rex.Match(NamePath);
  result := mr.Success;
end;

class function ControlNamePath.AscendNamePathByOne(const NamePath: string): string;
const
  rex_AllExceptLastName = '^((?:[a-zA-Z]+\w*)(?:\.[a-zA-Z]+\w*)*?)(?:\.[a-zA-Z]+\w*)$';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_AllExceptLastName);
  mr := rex.Match(NamePath);
  if (mr.Success)
    then result := mr.Groups[1].Value
    else result := '';
end;

class function ControlNamePath.LastName(const NamePath: string): string;
const
  rex_LastName = '(?:^|\w\.)((?:[a-zA-Z]+\w*))$';
var
  rex : TRegEx;
  mr : TMatch;
begin
  rex := TRegEx.Create(rex_LastName);
  mr := rex.Match(NamePath);
  if (mr.Success)
    then result := mr.Groups[1].Value
    else result := '';
end;




end.
