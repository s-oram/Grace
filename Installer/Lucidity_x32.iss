[Setup]
#define VersionReference "S:\Installer\Vst32 Dir\Lucidity.dll"
#define MyAppPublisher "One Small Clue"
#define MyAppName "Lucidity"
#define MyAppURL "http://www.onesmallclue.com/"
#define MyAppPlatform "32bit"

//===Automatice defines====
#define MyAppVersion GetFileVersion(VersionReference)
#define MyAppVersionAlt StringChange(GetFileVersion(VersionReference),'.','-')




OutputBaseFilename=Setup_{#MyAppName}_{#MyAppPlatform}_Full_{#MyAppVersionAlt}_Windows
AppCopyright={#MyAppPublisher}
AppPublisher={#MyAppPublisher}
AppName={#MyAppName}
AppVerName={#MyAppName} {#MyAppPlatform} {#MyAppVersion}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={reg:HKLM\Software\VST,VSTPluginsPath|{pf}\Steinberg\Vstplugins}
RestartIfNeededByRun=false
ChangesEnvironment=true
CreateAppDir=false
OutputDir=S:\Bin\Installers 
Compression=lzma/Ultra64
InternalCompressLevel=Ultra64
ShowLanguageDialog=no
UsePreviousGroup=false
SetupLogging=true
DisableProgramGroupPage=yes

[Dirs]
Name: "{code:GetDataDir|DataDir}"; Permissions: everyone-modify;

[Files]
Source: "Vst32 Dir\Lucidity.dll"; DestDir: "{code:GetDataDir|VstDir}";  Flags: ignoreversion;
Source: "Data Dir\*";             DestDir: "{code:GetDataDir|DataDir}"; Flags: ignoreversion recursesubdirs;
Source: "System32\*";             DestDir: "{sys}"; Flags: 32bit;

[Icons]
Name: "{code:GetDataDir|VstDir}\{#MyAppName} Data"; Filename: {code:GetDataDir|DataDir}; Flags: UseAppPaths; 
Name: "{code:GetDataDir|DataDir}\Uninstall {#MyAppName} {#MyAppPlatform}.exe"; Filename: {uninstallexe}; 

[Registry]
Root: HKLM; Subkey: "Software\{#MyAppPublisher}\{#MyAppName}"; Flags: uninsdeletekey


[code]
//#include "Install Script Code.txt"

var  
  VstDirPage  : TInputDirWizardPage; 
  Text1 : string;
  Text2 : string;
  Text3 : string;  

  VstDirCaption : string;
  DataDirCaption : string;
  VstDir   : string;
  DataDir  : string; 
procedure InitializeWizard;
var
  SubKeyName : string; 
  ValueName  : string;
begin
  //========================================================
  //  Custom Page: VST Directory
  //========================================================
  Text1 := 'Installation Paths'; 
  Text2 := 'Choose where to install files for Lucidity.';
  Text3 := '';
  
  //Get the VST plugin directory as specified in the Windows registery.
  if '{#MyAppPlatform}' = '32bit' then
  begin
    VstDirCaption  := '32-Bit VST 2 Plugins Directory';
    DataDirCaption := 'Lucidity Data Directory'#13#10'(For skins, patches, configuration files etc.)'; 

    VstDir := ''; 
    RegQueryStringValue(HKEY_LOCAL_MACHINE, 'Software\VST', 'VSTPluginsPath', VstDir);;
    RegQueryStringValue(HKEY_LOCAL_MACHINE, 'Software\{#MyAppPublisher}\{#MyAppName}', 'Vstx86Dir', VstDir);

    DataDir := ExpandConstant('{commonappdata}{\}{#MyAppPublisher}{\}{#MyAppName} x32');    
    RegQueryStringValue(HKEY_LOCAL_MACHINE, 'Software\{#MyAppPublisher}\{#MyAppName}', 'DataPath', DataDir);
  end;
  
  if '{#MyAppPlatform}' = '64bit' then
  begin
    VstDirCaption  := '64-Bit VST 2 Plugins Directory';
    DataDirCaption := 'Lucidity Data Directory (For skins, patches, configuration files etc.)'; 

    VstDir := ''; 
    RegQueryStringValue(HKEY_LOCAL_MACHINE, 'Software\VST', 'VSTPluginsPath', VstDir);;
    RegQueryStringValue(HKEY_LOCAL_MACHINE, 'Software\{#MyAppPublisher}\{#MyAppName}', 'Vstx64Dir',VstDir);

    DataDir := ExpandConstant('{commonappdata}{\}{#MyAppPublisher}{\}{#MyAppName} x64');    
    RegQueryStringValue(HKEY_LOCAL_MACHINE, 'Software\{#MyAppPublisher}\{#MyAppName}', 'DataPath', DataDir);
  end;
  
  VstDirPage := CreateInputDirPage(wpWelcome, Text1, Text2,  Text3, False, '{#MyAppName}');

  VstDirPage.Add(VstDirCaption);  
  VstDirPage.Values[0] := VstDir;

  VstDirPage.Add(DataDirCaption);  
  VstDirPage.Values[1] := DataDir;
end; 


function GetDataDir(Param: String): String;
begin  
  if Param = 'VstDir' 
    then Result := VstDirPage.Values[0]
  else if (Param = 'DataDir') 
    then Result := VstDirPage.Values[1];
end;

procedure CurStepChanged(CurStep: TSetupStep);
var
  DataDir    : string;
  s          : string;
  ReplacementString  : string;
  SourceFileFullPath : string;
  SourceFileName     : string;
  DestFileFullPath   : string;     
begin
  if (CurStep = ssDone)  and (IsUninstaller = false) then
  begin
    //Copy the log file.   
    SourceFileFullPath := ExpandConstant('{log}');
    SourceFileName     := ExtractFileName(SourceFileFullPath);
    DestFileFullPath := GetDataDir('VstDir') + ExpandConstant('{\}{#MyAppName} ') + 'Setup Log.txt';    
    FileCopy(SourceFileFullPath, DestFileFullPath, false);
    
    if '{#MyAppPlatform}' = '32bit' then
    begin
      RegWriteStringValue(HKEY_LOCAL_MACHINE, 'Software\{#MyAppPublisher}\{#MyAppName}', 'DataPath', GetDataDir('DataDir'));
      RegWriteStringValue(HKEY_LOCAL_MACHINE, 'Software\{#MyAppPublisher}\{#MyAppName}', 'Vstx86Dir', GetDataDir('VstDir'));
    end;
    
    if '{#MyAppPlatform}' = '64bit' then
    begin
      RegWriteStringValue(HKEY_LOCAL_MACHINE, 'Software\{#MyAppPublisher}\{#MyAppName}', 'DataPath', GetDataDir('DataDir'));
      RegWriteStringValue(HKEY_LOCAL_MACHINE, 'Software\{#MyAppPublisher}\{#MyAppName}', 'Vstx64Dir', GetDataDir('VstDir'));
    end;   
  end;       
end;

function UpdateReadyMemo(Space, NewLine, MemoUserInfoInfo, MemoDirInfo, MemoTypeInfo, MemoComponentsInfo, MemoGroupInfo, MemoTasksInfo: String): String;
var
  s : string;
begin
  s := 'VST Plugin Directory' + ExpandConstant(#13#10); 
  s := s + '   ' + GetDataDir('VstDir') + ExpandConstant(#13#10);
  s := s + ExpandConstant(#13#10);
  s := s + 'Plugin Data Directory'  + ExpandConstant(#13#10);
  s := s + '   ' + GetDataDir('DataDir') + ExpandConstant(#13#10);
  s := s + ExpandConstant(#13#10);  

  result := s;
end;






