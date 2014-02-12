unit VamLib.DebugString;

{ Copyright (c) 1995-2011 Jeroen Wiert Pluimers for BeSharp.net and better office benelux.
Full BSD License is available at http://besharp.codeplex.com/license and http://bo.codeplex.com/license }

{ better than OutputDebugString which:
 1. has a bug in Windows 95 that prevents OutputDebugString to be visible in DBWIN32/DbgView/DebugView
 2. if a process runs under the debugger, only outputs to the Debugger and not to DBWIN32/DbgView/DebugView

 See http://www.unixwiz.net/techtips/outputdebugstring.html
}

interface

procedure DbWin__OutputDebugStringA(lpOutputString: PAnsiChar); stdcall;

procedure DbWin__OutputDebugStringW(lpOutputString: PWideChar); stdcall;
{$ifdef UNICODE}
procedure DbWin__OutputDebugStringU(lpOutputString: PWideChar); stdcall;
{$endif UNICODE}
procedure DbWin__OutputDebugString(lpOutputString: PAnsiChar); stdcall;

implementation

uses
 Windows,
 SysUtils;

const
 FourK = 4096; //size for DBWIN_BUFFER buffer below
 AlmostFourK = FourK - SizeOf(DWord);

type
 PSharedMem = ^TSharedMem;

 TSharedMem = record
   ProcessID: DWord;
{$ifdef UNICODE}
   CharBuffer: array[0..AlmostFourK-1] of AnsiChar;
{$else}
   CharBuffer: array[0..AlmostFourK-1] of Char;
{$endif UNICODE}
 end;

procedure DbWin__OutputDebugStringA(lpOutputString: PAnsiChar); stdcall;
var
 Utf8String: string;
begin
 Utf8String := UTF8ToString(lpOutputString);
{$ifdef UNICODE}
 DbWin__OutputDebugStringU(PChar(Utf8String));
{$else}
 DbWin__OutputDebugString(PChar(Utf8String));
{$endif UNICODE}
end;

procedure DbWin__OutputDebugStringW(lpOutputString: PWideChar); stdcall;
begin
{$ifdef UNICODE}
 DbWin__OutputDebugStringU(lpOutputString);
{$else}
 DbWin__OutputDebugString(PChar(WideCharToString(lpOutputString)));
{$endif UNICODE}
end;

{$ifdef UNICODE}
procedure DbWin__OutputDebugStringU(lpOutputString: PWideChar); stdcall;
var
 Utf8String: RawByteString;
begin
 Utf8String := UTF8Encode(lpOutputString);
 DbWin__OutputDebugString(PAnsiChar(Utf8String));
end;
{$endif UNICODE}

procedure DbWin__OutputDebugString(lpOutputString: PAnsiChar); stdcall;
{$ifdef win32}
var
 heventDBWIN: THandle;  { DBWIN32 synchronization object }
 heventData: THandle;   { data passing synch object }
 hSharedFile: THandle;  { memory mapped file shared data }
 SharedMem: PSharedmem; { shared data }
begin
{ protocol: http://unixwiz.net/techtips/outputdebugstring.html }
{ Do a regular OutputDebugString so that the output is
 still seen in the debugger window if it exists. }
{$ifdef UNICODE}
 Windows.OutputDebugStringA(lpOutputString);
{$else}
 Windows.OutputDebugString(lpOutputString);
{$endif UNICODE}
{ bail if OutputDebugString is supposed to work }
 //if (DoesOutputDebugStringRedirectToDebugView) then
 //  Exit; // we exit when Windows.OutputDebugString would have done its' job.

{ make sure DBWIN is open and waiting }
 heventDBWIN := OpenEvent(EVENT_MODIFY_STATE, FALSE, 'DBWIN_BUFFER_READY');
 try
   if heventDBWIN = 0 then
   begin
     // MessageBox(0, 'DBWIN_BUFFER_READY nonexistent', nil, MB_OK);
     Exit;
   end;
 { get a handle to the data synch object }
   heventData := OpenEvent(EVENT_MODIFY_STATE, FALSE, 'DBWIN_DATA_READY');
   try
     if heventData = 0 then
     begin
       // MessageBox(0, 'DBWIN_DATA_READY nonexistent', nil, MB_OK);
       Exit;
     end;
   { get a handle to the memory mapped file }
     hSharedFile := CreateFileMapping(THANDLE(-1), nil, PAGE_READWRITE, 0, FourK, 'DBWIN_BUFFER');
     try
       if hSharedFile = 0 then
       begin
         // MessageBox(0, 'DebugTrace: Unable to create file mapping object DBWIN_BUFFER', 'Error', MB_OK);
         Exit;
       end;
     { get a pointer to the memory mapped file
       (this points to the shared memory between our process and the DBWIN32 process) }
       SharedMem := MapViewOfFile(hSharedFile, FILE_MAP_WRITE, 0, 0, 512);
       if SharedMem = nil then
       begin
         // MessageBox(0, 'DebugTrace: Unable to map shared memory', 'Error', MB_OK);
         Exit;
       end;
     { wait for buffer event }
       WaitForSingleObject(heventDBWIN, INFINITE);
     { write it to the shared memory }
       SharedMem.ProcessId := GetCurrentProcessId;
       StrLCopy(SharedMem.CharBuffer, lpOutputString, 500);
     { signal data ready event }
       SetEvent(heventData);
     { clean up handles }
       if not UnMapViewOfFile(SharedMem) then
         MessageBox(0, 'DebugTrace: Unable to un-map shared memory', 'Error', MB_OK);
     finally
       CloseHandle(hSharedFile);
     end;
   finally
     CloseHandle(heventData);
   end;
 finally
   CloseHandle(heventDBWIN);
 end;
end;
{$else}
begin
 WinProcs.OutputDebugString(lpOutputString);
end;
{$endif}

end.
