{
********************************************************************
DVSTExtra.pas
A unit to extend the VST SDK with some useful functions!

(C)opyright 2001-2004 by Tobybear (tobybear@web.de)
Get the latest version at my site: www.tobybear.de

I AM NOT RESPONSIBLE FOR ANYTHING! USE AT YOUR OWN RISK!

VST is a registered trademark of Steinberg Hard- and Software GmbH
********************************************************************
}

// NOTE: This code has been built to compile with Delphi 2007 or Delphi XE2. There may be issues with
// string conversions when using intermediate versions.

unit eeVSTExtra;

interface

function GetDLLFilename:string;
function GetDLLDirectory:string;

function CreateApplicationDataFolder(CompanyName, ProductName, ProductVersion:string):string;
function FindApplicationDataFolder(const CompanyName, ProductName, ProductVersion:string; Out DataFolder:string):boolean;

function GetWindowsFolderPath(CSIDL_FOLDER_ID:integer):string;
function GetEnvVarValue(const VarName: string): string;

function DispatcherOpcodeToStr(Opcode:integer):string;

function RemoveFileExt(FileName:string):string;


procedure GetBuildInfo(var V1, V2, V3, V4: word);
function GetBuildInfoAsString: string;



implementation

uses
  Classes, Math, SysUtils, Windows, SHFolder;

function GetDLLFilename:string;
{$IFDEF VER230}
var
  s:array[0..1500] of WideChar;
  st:string;
begin
  getmodulefilename(hinstance,s,sizeof(s));
  st:=strpas(s);
  st:=extractfilename(st);
  result:=st;
end;
{$ELSE}
var
  s:array[0..1500] of ansichar;
  st:string;
begin
  getmodulefilename(hinstance,s,sizeof(s));
  st:=strpas(s);
  st:=extractfilename(st);
  result:=st;
end;
{$ENDIF}

function GetDLLDirectory:string;
{$IFDEF VER230}
var
  s:array[0..1500] of WideChar;
  st:string;
begin
  getmodulefilename(hinstance,s,sizeof(s));
  st:=strpas(s);
  st:=extractfilepath(st);
  result:=st;
end;
{$ELSE}
var
  s:array[0..1500] of AnsiChar;
  st:string;
begin
  getmodulefilename(hinstance,s,sizeof(s));
  st:=strpas(s);
  st:=extractfilepath(st);
  result:=st;
end;
{$ENDIF}


function GetWindowsFolderPath(CSIDL_FOLDER_ID:integer):string;
const
  //NOTE: Constant values sourced from http://www.koders.com/delphi/fid3153506EB59DBB44695B8FAFA71D6FD326F5F995.aspx
  SHGFP_TYPE_CURRENT  = 0;   // current value for user, verify it exists
  SHGFP_TYPE_DEFAULT  = 1;   // default value, may not exist
var
  {$IFDEF VER230}
  path: array [0..MAX_PATH] of WideChar;
  {$ELSE}
  path: array [0..MAX_PATH] of AnsiChar;
  {$ENDIF}
begin
  //Look for root app data folder.
  if Succeeded(SHGetFolderPath(0,CSIDL_LOCAL_APPDATA,0,SHGFP_TYPE_CURRENT,@path[0]))
    then result := path
    else result := '';
end;

function GetEnvVarValue(const VarName: string): string;
var
  BufSize: Integer;  // buffer size required for value
begin
  // Get required buffer size (inc. terminal #0)
  BufSize := GetEnvironmentVariable(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    // Read env var value into result string
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariable(PChar(VarName),
      PChar(Result), BufSize);
  end
  else
    // No such environment variable
    Result := '';
end;



function CreateApplicationDataFolder(CompanyName, ProductName, ProductVersion:string):string;
const
  //NOTE: Constant values sourced from http://www.koders.com/delphi/fid3153506EB59DBB44695B8FAFA71D6FD326F5F995.aspx
  SHGFP_TYPE_CURRENT  = 0;   // current value for user, verify it exists
  SHGFP_TYPE_DEFAULT  = 1;   // default value, may not exist
var
  {$IFDEF VER230}
  path: array [0..MAX_PATH] of WideChar;
  {$ELSE}
  path: array [0..MAX_PATH] of AnsiChar;
  {$ENDIF}
  DataFolder:string;
begin

  //Look for root app data folder.
  if not Succeeded(SHGetFolderPath(0,CSIDL_LOCAL_APPDATA,0,SHGFP_TYPE_CURRENT,@path[0])) then
  begin
    raise Exception.Create('Couldn''t find application data folder.');
  end;

  DataFolder := Path;

  //check for and create COMPANY folder if it doesn't exist.
  DataFolder := IncludeTrailingPathDelimiter(DataFolder);
  DataFolder := DataFolder + CompanyName;

  if not DirectoryExists(DataFolder) then
  begin
    if not CreateDir(DataFolder)
      then raise Exception.Create('Couldn''t create application data folder.');
  end;

  //check for and create PRODUCT folder if it doesn't exist.
  DataFolder := IncludeTrailingPathDelimiter(DataFolder);
  DataFolder := DataFolder + ProductName;

  if not DirectoryExists(DataFolder) then
  begin
    if not CreateDir(DataFolder)
      then raise Exception.Create('Couldn''t create application data folder.');
  end;

  //check for and create VERSION folder if it doesn't exist.
  DataFolder := IncludeTrailingPathDelimiter(DataFolder);
  DataFolder := DataFolder + ProductVersion;

  if not DirectoryExists(DataFolder) then
  begin
    if not CreateDir(DataFolder)
      then raise Exception.Create('Couldn''t create application data folder.');
  end;


  //report data folder.
  DataFolder := IncludeTrailingPathDelimiter(DataFolder);
  result     := DataFolder;

end;

function FindApplicationDataFolder(const CompanyName, ProductName, ProductVersion:string; Out DataFolder:string):boolean;
const
  //NOTE: Constant values sourced from http://www.koders.com/delphi/fid3153506EB59DBB44695B8FAFA71D6FD326F5F995.aspx
  SHGFP_TYPE_CURRENT  = 0;   // current value for user, verify it exists
  SHGFP_TYPE_DEFAULT  = 1;   // default value, may not exist
var
  {$IFDEF VER230} //Delphi XE2
  path : array [0..MAX_PATH] of WideChar;
  {$ELSE}
  path : array [0..MAX_PATH] of AnsiChar;
  {$ENDIF}
  Dir  : string;
begin
  //Look for root app data folder.
  if Succeeded(SHGetFolderPath(0,CSIDL_LOCAL_APPDATA,0,SHGFP_TYPE_CURRENT,@path[0])) then
  begin
    Dir := IncludeTrailingPathDelimiter(Path)
           + IncludeTrailingPathDelimiter(CompanyName)
           + IncludeTrailingPathDelimiter(ProductName)
           + IncludeTrailingPathDelimiter(ProductVersion);

    Dir := IncludeTrailingPathDelimiter(Dir);

    if DirectoryExists(Dir) then
    begin
      DataFolder := Dir;
      result := true;
    end else
    begin
      DataFolder := '';
      result     := false;
    end;
  end else
  begin
    //Root app data folder could not be found for some reason... :(
    DataFolder := '';
    result     := false;
  end;
end;

const
     effOpen            = 0;
     effClose           = 1;
     effSetProgram	= 2;
     effGetProgram      = 3;
     effSetProgramName  = 4;
     effGetProgramName  = 5;
     effGetParamLabel   = 6;
     effGetParamDisplay = 7;
     effGetParamName    = 8;
     effGetVu           = 9;
     effSetSampleRate   = 10;
     effSetBlockSize    = 11;
     effMainsChanged    = 12;
     effEditGetRect     = 13;
     effEditOpen        = 14;
     effEditClose       = 15;
     effEditDraw        = 16;
     effEditMouse       = 17;
     effEditKey         = 18;
     effEditIdle        = 19;
     effEditTop         = 20;
     effEditSleep       = 21;
     effIdentify        = 22;
     effGetChunk        = 23;
     effSetChunk        = 24;
     effNumOpcodes      = 25;
     effProcessEvents   = effSetChunk + 1;
     effCanBeAutomated = effSetChunk + 2;
     effString2Parameter = effSetChunk + 3;
     effGetNumProgramCategories = effSetChunk + 4;
     effGetProgramNameIndexed = effSetChunk + 5;
     effCopyProgram = effSetChunk + 6;
     effConnectInput = effSetChunk + 7;
     effConnectOutput = effSetChunk + 8;
     effGetInputProperties = effSetChunk + 9;
     effGetOutputProperties = effSetChunk + 10;
     effGetPlugCategory = effSetChunk + 11;
     effGetCurrentPosition = effSetChunk + 12;
     effGetDestinationBuffer = effSetChunk + 13;
     effOfflineNotify = effSetChunk + 14;
     effOfflinePrepare = effSetChunk + 15;
     effOfflineRun = effSetChunk + 16;
     effProcessVarIo = effSetChunk + 17;
     effSetSpeakerArrangement = effSetChunk + 18;

     effSetBlockSizeAndSampleRate = effSetChunk + 19;
     effSetBypass = effSetChunk + 20;
     effGetEffectName = effSetChunk + 21;
     effGetErrorText = effSetChunk + 22;
     effGetVendorString = effSetChunk + 23;
     effGetProductString = effSetChunk + 24;
     effGetVendorVersion = effSetChunk + 25;
     effVendorSpecific = effSetChunk + 26;
     effCanDo = effSetChunk + 27;
     effGetTailSize = effSetChunk + 28;
     effIdle = effSetChunk + 29;
     effGetIcon = effSetChunk + 30;
     effSetViewPosition = effSetChunk + 31;
     effGetParameterProperties = effSetChunk + 32;
     effKeysRequired = effSetChunk + 33;
     effGetVstVersion = effSetChunk + 34;
     effNumV2Opcodes = effSetChunk + 35;
     //---from here VST 2.1 extension opcodes---------------------------------------------------------
     effEditKeyDown = effNumV2Opcodes;
     effEditKeyUp = effEditKeyDown+1;
     effSetEditKnobMode = effEditKeyDown+2;
     effGetMidiProgramName = effEditKeyDown+3;
     effGetCurrentMidiProgram = effEditKeyDown+4;
     effGetMidiProgramCategory = effEditKeyDown+5;
     effHasMidiProgramsChanged = effEditKeyDown+6;
     effGetMidiKeyName = effEditKeyDown+7;
     effBeginSetProgram = effEditKeyDown+8;
     effEndSetProgram = effEditKeyDown+9;
     effNumV2_1Opcodes = effEditKeyDown+10;
     //---from here VST 2.3 extension opcodes---------------------------------------------------------
     effGetSpeakerArrangement = effNumV2_1Opcodes;
     effShellGetNextPlugin = effGetSpeakerArrangement+1;
     effStartProcess = effGetSpeakerArrangement+2;
     effStopProcess = effGetSpeakerArrangement+3;
     effSetTotalSampleToProcess = effGetSpeakerArrangement+4;
     effSetPanLaw = effGetSpeakerArrangement+5;
     effBeginLoadBank = effGetSpeakerArrangement+6;
     effBeginLoadProgram = effGetSpeakerArrangement+7;
     effNumV2_3Opcodes = effGetSpeakerArrangement+8;



function DispatcherOpcodeToStr(Opcode:integer):string;
begin
  case Opcode  of
    //-- VST 1.0 opcodes ---------------------------------------
    effOpen:                       result := 'effOpen';
    effClose:                      result := 'effClose';
    effSetProgram:                 result := 'effSetProgram';
    effGetProgram:                 result := 'effGetProgram';
    effSetProgramName:             result := 'effSetProgramName';
    effGetProgramName:             result := 'effGetProgramName';
    effGetParamLabel:              result := 'effGetParamLabel';
    effGetParamDisplay:            result := 'effGetParamDisplay';
    effGetParamName:               result := 'effGetParamName';
    effGetVu:                      result := 'effGetVu';
    effSetSampleRate:              result := 'effSetSampleRate';
    effSetBlockSize:               result := 'effSetBlockSize';
    effMainsChanged:               result := 'effMainsChanged';
    effEditGetRect:                result := 'effEditGetRect';
    effEditOpen:                   result := 'effEditOpen';
    effEditClose:                  result := 'effEditClose';
    effEditDraw:                   result := 'effEditDraw';
    effEditMouse:                  result := 'effEditMouse';
    effEditKey:                    result := 'effEditKey';
    effEditIdle:                   result := 'effEditIdle';
    effEditTop:                    result := 'effEditTop';
    effEditSleep:                  result := 'effEditSleep';
    effIdentify:                   result := 'effIdentify';
    effGetChunk:                   result := 'effGetChunk';
    //---from here VST 2.0 extension opcodes---------------------------------------------------------
    effProcessEvents:              result := 'effProcessEvents';
    effCanBeAutomated:             result := 'effCanBeAutomated';
    effString2Parameter:           result := 'effString2Parameter';
    effGetNumProgramCategories:    result := 'effGetNumProgramCategories';
    effGetProgramNameIndexed:      result := 'effGetProgramNameIndexed';
    effCopyProgram:                result := 'effCopyProgram';
    effConnectInput:               result := 'effConnectInput';
    effConnectOutput:              result := 'effConnectOutput';
    effGetInputProperties:         result := 'effGetInputProperties';
    effGetOutputProperties:        result := 'effGetOutputProperties';
    effGetPlugCategory:            result := 'effGetPlugCategory';
    effGetCurrentPosition:         result := 'effGetCurrentPosition';
    effGetDestinationBuffer:       result := 'effGetDestinationBuffer';
    effOfflineNotify:              result := 'effOfflineNotify';
    effOfflinePrepare:             result := 'effOfflinePrepare';
    effOfflineRun:                 result := 'effOfflineRun';
    effProcessVarIo:               result := 'effProcessVarIo';
    effSetSpeakerArrangement:      result := 'effSetSpeakerArrangement';
    effSetBlockSizeAndSampleRate:  result := 'effSetBlockSizeAndSampleRate';
    effSetBypass:                  result := 'effSetBypass';
    effGetEffectName:              result := 'effGetEffectName';
    effGetErrorText:               result := 'effGetErrorText';
    effGetVendorString:            result := 'effGetVendorString';
    effGetProductString:           result := 'effGetProductString';
    effGetVendorVersion:           result := 'effGetVendorVersion';
    effVendorSpecific:             result := 'effVendorSpecific';
    effCanDo:                      result := 'effCanDo';
    effGetTailSize:                result := 'effGetTailSize';
    effIdle:                       result := 'effIdle';
    effGetIcon:                    result := 'effGetIcon';
    effSetViewPosition:            result := 'effSetViewPosition';
    effGetParameterProperties:     result := 'effGetParameterProperties';
    effKeysRequired:               result := 'effKeysRequired';
    effGetVstVersion:              result := 'effGetVstVersion';
    //---from here VST 2.1 extension opcodes---------------------------------------------------------
    effEditKeyDown:                result := 'effEditKeyDown';
    effEditKeyUp:                  result := 'effEditKeyUp';
    effSetEditKnobMode:            result := 'effSetEditKnobMode';
    effGetMidiProgramName:         result := 'effGetMidiProgramName';
    effGetCurrentMidiProgram:      result := 'effGetCurrentMidiProgram';
    effGetMidiProgramCategory:     result := 'effGetMidiProgramCategory';
    effHasMidiProgramsChanged:     result := 'effHasMidiProgramsChanged';
    effGetMidiKeyName:             result := 'effGetMidiKeyName';
    effBeginSetProgram:            result := 'effBeginSetProgram';
    effEndSetProgram:              result := 'effEndSetProgram';
    //---from here VST 2.3 extension opcodes---------------------------------------------------------
    effGetSpeakerArrangement:      result := 'effGetSpeakerArrangement';
    effShellGetNextPlugin:         result := 'effShellGetNextPlugin';
    effStartProcess:               result := 'effStartProcess';
    effStopProcess:                result := 'effStopProcess';
    effSetTotalSampleToProcess:    result := 'effSetTotalSampleToProcess';
    effSetPanLaw:                  result := 'effSetPanLaw';
    effBeginLoadBank:              result := 'effBeginLoadBank';
    effBeginLoadProgram:           result := 'effBeginLoadProgram';
  else
    result := 'Unknown: Opcode = ' + IntToStr(Opcode);
  end;

end;

function RemoveFileExt(FileName:string):string;
var
  Ext:string;
  Index:integer;
  cc:integer;
begin
  FileName := ExtractFileName(FileName); //Remove path information, if there is any.

  Ext := ExtractFileExt(FileName);

  if Ext ='' then
  begin
    //There is no extension.
    result := FileName;
    exit; //==================================================>
  end;

  cc := Length(Ext);
  Index := Pos(Ext,FileName);

  Delete(FileName,Index,cc);

  result := FileName;
end;





procedure GetBuildInfo(var V1, V2, V3, V4: word);
var
  verblock:PVSFIXEDFILEINFO;
  versionMS,versionLS:cardinal;
  verlen:cardinal;
  rs:TResourceStream;
  m:TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    rs := TResourceStream.CreateFromID(HInstance,1,RT_VERSION);
    try
      m.CopyFrom(rs,rs.Size);
    finally
      rs.Free;
    end;

    m.Position := 0;

    if VerQueryValue(m.Memory,'\',pointer(verblock),verlen) then
    begin
      VersionMS:=verblock.dwFileVersionMS;
      VersionLS:=verblock.dwFileVersionLS;
      V1 := versionMS shr 16;
      V2 := versionMS and $FFFF;
      V3 := VersionLS shr 16;
      V4 := VersionLS and $FFFF;
    end;

  finally
    m.Free;
  end;
end;

function GetBuildInfoAsString: string;
var
  V1, V2, V3, V4: word;
begin
  GetBuildInfo(V1, V2, V3, V4);
  Result := IntToStr(V1) + '.' + IntToStr(V2) + '.' +
    IntToStr(V3) + '.' + IntToStr(V4);
end;



end.




