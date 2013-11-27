unit eeClipBoard;

interface

uses
  Classes;



// GetFilesOnClipBoard() returns a list of all file names on the clip board.
// Source: http://delphi.about.com/cs/adptips2001/a/bltip1001_3.htm
procedure GetFilesOnClipBoard(var Files : TStringList);

implementation

uses
  SysUtils, ClipBrd, Windows, ShellApi;

procedure GetFilesOnClipBoard(var Files : TStringList);
var
  cbHandle: THandle;
  Buffer : array[0..Max_Path] of WideChar;
  BufferLength : integer;
  c1, numFiles: Integer;
  s : string;
begin
  if not assigned(Files) then raise Exception.Create('Files variable is not assigned.');

  Files.Clear;

  Clipboard.Open;
  try
    cbHandle := Clipboard.GetAsHandle( CF_HDROP ) ;
    if cbHandle <> 0 then
    begin
      BufferLength := Length(Buffer);

      NumFiles := DragQueryFile(cbHandle, $FFFFFFFF, nil, 0 ) ;
      for c1 := 0 to numfiles - 1 do
      begin
        buffer[0] := #0;
        DragQueryFile( cbHandle, c1, Buffer, BufferLength) ;
        s := Buffer;
        Files.Add(s);
      end;
    end;
  finally
    Clipboard.close;
  end;
end;

end.
