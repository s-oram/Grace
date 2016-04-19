unit VamLib.Wine;

interface

// How to get Wine version.
// https://www.winehq.org/pipermail/wine-devel/2008-September/069387.html
function IsRunningUnderWine:boolean;

implementation

uses
  SysUtils, Windows;

function IsRunningUnderWine:boolean;
var
  H: cardinal;
begin
  result := false;

  try
    H := SysUtils.SafeLoadLibrary('ntdll.dll')
  except
    H := 0;
  end;

  if H <> 0 then
  begin
    Result := Assigned(GetProcAddress(H, 'wine_get_version'));
    FreeLibrary(H);
  end;
end;

end.
