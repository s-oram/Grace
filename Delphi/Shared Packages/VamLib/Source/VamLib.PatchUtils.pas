unit VamLib.PatchUtils;

interface

function GetActualAddr(Proc: Pointer): Pointer;

implementation

uses
  SysUtils, Windows;


type
  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: PPointer;
  end;


//get the address of a procedure or method of a function
function GetActualAddr(Proc: Pointer): Pointer;
begin
  if Proc <> nil then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT) and (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;



end.
