unit eeSimpleLog;

{$INCLUDE Defines.inc}

{$IFNDEF SimpleLog}
  // Turn off hints if we're not using the simple log class.
  // Leaving hints on when we disable SimpleLog will cause a few
  // "variable not used" hints.
  {$HINTS OFF}
{$ENDIF}

interface

type
  TSimpleLog = class
  public
    class procedure Clear(LogFileName:string);
    class procedure Append(LogFileName:string; Text:string);
  end;

implementation

uses
  Windows, SysUtils, Classes;

{ TSimpleLog }

class procedure TSimpleLog.Clear(LogFileName:string);
var
  TextFile : TFileStream;
begin
  {$IFDEF SimpleLog}
  if FileExists(LogFileName) then
  begin
    TextFile := TFileStream.Create(LogFileName, fmCreate);
    TextFile.Free;
  end;
  {$ENDIF}
end;

class procedure TSimpleLog.Append(LogFileName:string; Text:string);
const
  LineBreak = string(#13#10);
var
  Flags : word;
  TextFile : TFileStream;
begin
  {$IFDEF SimpleLog}
  OutputDebugString(PWideChar(Text));

  //prepend date and time.
  Text := DateTimeToStr(Now) + ' - ' + Text;

  if FileExists(LogFileName) = false
    then Flags := fmCreate
    else Flags := fmOpenReadWrite;

  TextFile := TFileStream.Create(LogFileName, Flags);
  try
    TextFile.Seek(0,TSeekOrigin.soEnd);
    TextFile.Write(Text[1], Length(Text) * SizeOf(Char));
    TextFile.Write(LineBreak[1], Length(LineBreak) * SizeOf(Char));
  finally
    TextFile.Free;
  end;
  {$ENDIF}
end;




end.
