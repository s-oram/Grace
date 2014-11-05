[Setup]
#define VersionReference "S:\Installer\Vst64 Dir\Grace.dll"
#define MyAppPublisher "One Small Clue"
#define MyAppName "Grace"
#define MyAppURL "http://www.onesmallclue.com/"
#define MyAppPlatform "64bit"

//===Automatice defines====
#define MyAppVersion GetFileVersion(VersionReference)
#define MyAppVersionAlt StringChange(GetFileVersion(VersionReference),'.','-')

OutputBaseFilename=Setup_{#MyAppName}_{#MyAppPlatform}_Demo_{#MyAppVersionAlt}_Windows
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
ArchitecturesInstallIn64BitMode=x64
ArchitecturesAllowed=x64

[Dirs]
Name: "{code:GetDataDir|DataDir}"; Permissions: everyone-modify;

[Files]
Source: "Vst64 Dir\Grace.dll";      DestDir: "{code:GetDataDir|VstDir}";  Flags: ignoreversion;
Source: "Data Dir\*";               DestDir: "{code:GetDataDir|DataDir}"; Flags: ignoreversion recursesubdirs;
Source: "Data Dir Demo Extras\*";   DestDir: "{code:GetDataDir|DataDir}"; Flags: ignoreversion recursesubdirs;
Source: "System64\*";               DestDir: "{sys}"; Flags: 64bit;

[Icons]
Name: "{code:GetDataDir|VstDir}\{#MyAppName} Data"; Filename: {code:GetDataDir|DataDir}; Flags: UseAppPaths; 
Name: "{code:GetDataDir|DataDir}\Uninstall {#MyAppName} {#MyAppPlatform}.exe"; Filename: {uninstallexe}; 

[Registry]
Root: HKLM; Subkey: "Software\{#MyAppPublisher}\{#MyAppName}"; Flags: uninsdeletekey


[code]
#include "Install Script Code.txt"
