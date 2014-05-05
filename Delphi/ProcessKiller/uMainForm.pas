unit uMainForm;

interface

uses
  VamLib.Collections.Lists,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TCurrentProcessInfo = record
  public
    Name : string;
    ProcessID : DWORD;
  end;

  TForm2 = class(TForm)
    ListBox1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }

    CurrentProcessList : TSimpleRecordList<TCurrentProcessInfo>;

  public
    procedure UpdateListBox;
  end;

var
  Form2: TForm2;

implementation

uses
  PsApi,
  TlHelp32,
  VamLib.Utils;

{$R *.dfm}


function GetProcessName(PID: DWORD; var ProcessName: string): DWORD;
var
  dwReturn     : DWORD;
  hProc        : Cardinal;
  buffer       : array[0..MAX_PATH - 1] of Char;
begin
  dwReturn := 0;
  Zeromemory(@buffer, sizeof(buffer));
  hProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, FALSE, PID);
  if hProc <> 0 then
  begin
    GetModulebaseName(hProc, 0, buffer, sizeof(buffer));
    ProcessName := (string(buffer));
    CloseHandle(hProc);
  end
  else
    dwReturn := GetLastError;
  result := dwReturn;
end;

type
  TPIDList = array of DWORD;

function GetProcessList(var ProcessList: TPIDList): DWORD;

  function GetOSVersionInfo(var Info: TOSVersionInfo): Boolean;
  begin
    FillChar(Info, SizeOf(TOSVersionInfo), 0);
    Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    Result := GetVersionEx(TOSVersionInfo(Addr(Info)^));
    if (not Result) then
    begin
      FillChar(Info, SizeOf(TOSVersionInfo), 0);
      Info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
      Result := GetVersionEx(TOSVersionInfo(Addr(Info)^));
      if (not Result) then
        Info.dwOSVersionInfoSize := 0;
    end;
  end;

var
  dwReturn     : DWORD;
  OS           : TOSVersionInfo;
  // EnumProcesses
  PidProcesses : PDWORD;
  PidWork      : PDWORD;
  BufferSize   : Cardinal;
  Needed       : DWORD;
  cntProcesses : Cardinal;
  i            : Cardinal;
  // CreateToolhelp32Snapshot
  hProcSnapShot: THandle;
  pe32         : TProcessEntry32;
  j            : Cardinal;

begin
  dwReturn := 0;
  // What OS are we running on?
  if GetOSVersionInfo(OS) then
  begin
    if (OS.dwPlatformId = VER_PLATFORM_WIN32_NT) and (OS.dwMajorVersion = 4) then
    // WinNT and higher
    begin
      Needed := 0;
      BufferSize := 1024;
      GetMem(PidProcesses, BufferSize);
      // make sure memory is allocated
      if Assigned(PidProcesses) then
      begin
        try
          // enumerate the processes
          if EnumProcesses(PidProcesses, BufferSize, Needed) then
          begin
            dwReturn := 0;
            cntProcesses := Needed div sizeof(DWORD) - 1;
            PidWork := PidProcesses;
            setlength(ProcessList, cntProcesses);
            // walk the processes
            for i := 0 to cntProcesses - 1 do
            begin
              ProcessList[i] := PidWork^;
              Inc(PidWork);
            end;
          end
          else // EnumProcesses = False
            dwReturn := GetLastError;
        finally
          // clean up no matter what happend
          FreeMem(PidProcesses, BufferSize);
        end;
      end
      else // GetMem = nil
        dwReturn := GetLastError;
    end
    // Win 9x and higher except WinNT
    else
    begin
      // make the snapshot
      hProcSnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
      if hProcSnapShot <> INVALID_HANDLE_VALUE then
      begin
        pe32.dwSize := sizeof(TProcessEntry32);
        j := 0;
        setlength(ProcessList, j + 1);
        if Process32First(hProcSnapShot, pe32) then
        begin
          // first process
          ProcessList[j] := pe32.th32ProcessID;
          // walk the processes
          while Process32Next(hProcSnapShot, pe32) do
          begin
            Inc(j);
            setlength(ProcessList, j + 1);
            ProcessList[j] := pe32.th32ProcessID;
          end;
        end
        else // Process32First = False
          dwReturn := GetLastError;
        CloseHandle(hProcSnapShot);
      end
      else // hSnapShot = INVALID_HANDLE_VALUE
        dwReturn := GetLastError;
    end;
  end;
  result := dwReturn;
end;

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  CurrentProcessList := TSimpleRecordList<TCurrentProcessInfo>.Create;


  UpdateListBox;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  CurrentProcessList.Free;
end;

procedure TForm2.UpdateListBox;
var
  Text : TStringList;
  retValue     : DWORD;
  ProcessList  : TPIDList;
  i            : Integer;
  ProcessName  : string;
  PID          : DWORD;
  c1: Integer;
  s : string;
resourcestring
  rsUnknown    = 'unbekannt';
begin
  Text := TStringList.Create;
  AutoFree(@Text);



  // VCL causes last error to be set, even nothing has already happend :-/
  SetLastError(0);
  retValue := GetProcessList(ProcessList);
  if retValue = 0 then
  begin
    CurrentProcessList.Count := Length(ProcessList);

    for i := 0 to length(ProcessList) - 1 do
    begin
      PID := ProcessList[i];
      if GetProcessName(ProcessList[i], ProcessName) <> 0 then
        ProcessName := rsUnknown;

      CurrentProcessList.Raw[i].Name := ProcessName;
      CurrentProcessList.Raw[i].ProcessID := PID;
    end;
  end
  else
    ShowMessage(SysErrorMessage(retValue));


  //======= Delete unnamed processes ====================

  for c1 := CurrentProcessList.Count-1 downto 0 do
  begin
    if CurrentProcessList[c1].Name = ''
      then CurrentProcessList.Delete(c1);
  end;


  //======= Update the list box ====================
  ListBox1.Clear;
  for c1 := 0 to CurrentProcessList.Count-1 do
  begin
    PID         := CurrentProcessList[c1].ProcessID;
    Processname := CurrentProcessList[c1].Name;
    s := IntToStr(PID) + ' - ' + ProcessName;
    ListBox1.Items.Add(s);
  end;
  ListBox1.Invalidate;
  //==============================================
end;

end.
