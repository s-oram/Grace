unit
    AsioList;

interface

uses
    Classes, ActiveX;

type
    TAsioDriverDesc = packed record
      id   : TCLSID;
      name : array[0..511] of ansichar;
      path : array[0..511] of ansichar;
    end;
    PAsioDriverDesc = ^TAsioDriverDesc;

    TAsioDriverList = array of TAsioDriverDesc;


procedure ListAsioDrivers(var List: TAsioDriverList);


implementation

uses
    Windows, Registry, SysUtils, ComObj;

const
     ASIODRV_DESC  = 'description';
     INPROC_SERVER = 'InprocServer32';
     ASIO_PATH     = 'software\asio';
     COM_CLSID     = 'clsid';

// ******************************************************************
// Local Functions
// ******************************************************************
function findDrvPath(const clsidstr: string; var dllpath: string): longint;
var
   reg     : TRegistry;
   success : boolean;
   buf     : array[0..1024] of char;
   s       : string;
   temps   : string;
begin
  Result := -1;

  //CharLowerBuff(clsidstr,strlen(clsidstr));
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    success := reg.OpenKeyReadOnly(COM_CLSID + '\' + clsidstr + '\' + INPROC_SERVER);
    if success then
    begin
      dllpath := reg.ReadString('');
      if (ExtractFilePath(dllpath) = '') and (dllpath <> '') then
      begin
        buf[0] := #0;
        temps := dllpath;   // backup the value
        if GetSystemDirectory(buf, 1023) <> 0 then   // try the system directory first
        begin
          s := buf;
          dllpath := s + '\' + temps;
        end;

        if not FileExists(dllpath) then              // try the windows dir if necessary
        begin
          buf[0] := #0;
          if GetWindowsDirectory(buf, 1023) <> 0 then   // try the system directory first
          begin
            s := buf;
            dllpath := s + '\' + temps;
          end;
        end;
      end;

      if FileExists(dllpath) then
        Result := 0;
    end;
  finally
    reg.Free;
  end;
end;

procedure ListAsioDrivers(var List: TAsioDriverList);
var
   r       : TRegistry;
   keys    : TStringList;
   success : boolean;
   i       : integer;
   id      : string;
   dllpath : string;
   count   : integer;
begin
  SetLength(List, 0);

  keys := TStringList.Create;
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    success := r.OpenKeyReadOnly(ASIO_PATH);
    if success then
    begin
      r.GetKeyNames(keys);
      r.CloseKey;
    end;

    count := 0;
    for i := 0 to keys.Count-1 do
    begin
      success := r.OpenKeyReadOnly(ASIO_PATH + '\' + keys[i]);
      if success then
      begin
        id := r.ReadString(COM_CLSID);
        if findDrvPath(id, dllpath) = 0 then  // check if the dll exists
        begin
          SetLength(List, count+1);
          List[count].id := StringToGUID(id);
          {$WARNINGS OFF}
          StrPLCopy(List[count].name, keys[i], 512);
          StrPLCopy(List[count].path, dllpath, 512);
          {$WARNINGS ON}
          inc(count);
        end;
        r.CloseKey;
      end;
    end;
  finally
    keys.Free;
    r.Free;
  end;
end;

end.
