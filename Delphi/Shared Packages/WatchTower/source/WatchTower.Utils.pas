unit WatchTower.Utils;

interface

function DoesPathUseCorrectSeperators(const Path : string):boolean;

implementation

uses
  SysUtils;

function DoesPathUseCorrectSeperators(const Path : string):boolean;
begin
  if (SysUtils.PathDelim = '\') then
  begin
    if Pos('/', Path) = 0
      then exit(true)
      else exit(false);
  end;

  if (SysUtils.PathDelim = '/') then
  begin
    if Pos('\', Path) = 0
      then exit(true)
      else exit(false);
  end;

  // If we make it this far...
  result := false;
end;

end.
