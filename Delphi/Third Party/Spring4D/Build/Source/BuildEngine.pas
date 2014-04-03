{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit BuildEngine;

interface

uses
  Classes,
  Registry,
  SysUtils,
  Spring.Collections;

type
  {$SCOPEDENUMS ON}

  TConfigurationType = (
    Debug,
    Release
  );

  TCompilerTargetBase = class
  private
    fBrowsingPaths: TStrings;
    fDisplayName: string;
    fEnvironmentVariables: TStrings;
    fExists: Boolean;
    fLibraryPaths: TStrings;
    fRootDir: string;
    fTargetPlatform: string;
    fTypeName: string;
  public
    constructor Create;
    destructor Destroy; override;

    property BrowsingPaths: TStrings read fBrowsingPaths;
    property DisplayName: string read fDisplayName;
    property EnvironmentVariables: TStrings read fEnvironmentVariables;
    property Exists: Boolean read fExists;
    property LibraryPaths: TStrings read fLibraryPaths;
    property RootDir: string read fRootDir;
    property TargetPlatform: string read fTargetPlatform;
    property TypeName: string read fTypeName;
  end;

  TCompilerTarget = class(TCompilerTargetBase)
  private
    type
      TKeys = record
        BDS: string;
        LibraryKey: string;
        Globals: string;
        EnvironmentVariables: string;
      end;

      TNames = record
        RootDir: string;
        LibraryPath: string;
        BrowsingPath: string;
      end;
  private
    fRegistry: TRegistry;
    fKeys: TKeys;
    fNames: TNames;
  protected
    procedure EnsureOpenKey(const key: string; canCreate: Boolean = False);
    function GetDccFileName(const targetPlatform: string): string;
    function IdeAndDccExist(const bdsFilePath, targetPlatform: string): Boolean;
    procedure LoadEnvironmentVariables(const environmentVariables: TStrings);
    procedure SaveEnvironmentVariables(const environmentVariables: TStrings);

    property Keys: TKeys read fKeys;
    property Names: TNames read fNames;
  public
    constructor Create(const typeName: string; const properties: TStrings); overload;
    destructor Destroy; override;

    procedure LoadOptions;
    procedure SaveOptions;
  end;

  TBuildTask = class
  private
    fCompiler: TCompilerTarget;
    fProjects: TStrings;
    fUnitOutputPath: string;
  public
    constructor Create;
    destructor Destroy; override;

    function CanBuild: Boolean;
    function Name: string;
    property Compiler: TCompilerTarget read fCompiler write fCompiler;
    property Projects: TStrings read fProjects;
    property UnitOutputPath: string read fUnitOutputPath write fUnitOutputPath;
  end;

  TBuildEngineBase = class
  private
    fConfigurationType: TConfigurationType;
    fModifyDelphiRegistrySettings: Boolean;
    FPauseAfterEachStep: Boolean;
    fRunTests: Boolean;
    fSelectedTasks: IList<TBuildTask>;
    fSourcePaths: TStrings;
    fTargets: IList<TCompilerTarget>;
    fTasks: IList<TBuildTask>;
  public
    constructor Create;
    destructor Destroy; override;

    property ConfigurationType: TConfigurationType
      read fConfigurationType write fConfigurationType;
    property ModifyDelphiRegistrySettings: Boolean
      read fModifyDelphiRegistrySettings write fModifyDelphiRegistrySettings;
    property PauseAfterEachStep: Boolean read FPauseAfterEachStep write FPauseAfterEachStep;
    property RunTests: Boolean read fRunTests write fRunTests;
    property SelectedTasks: IList<TBuildTask> read fSelectedTasks;
    property SourcePaths: TStrings read fSourcePaths;
    property Targets: IList<TCompilerTarget> read fTargets;
    property Tasks: IList<TBuildTask> read fTasks;
  end;

  TBuildEngine = class(TBuildEngineBase)
  private
    fSourceBaseDir: string;
  protected
    procedure BuildTarget(const task: TBuildTask);
    procedure ExecuteCommandLine(const applicationName, commandLine: string;
      var exitCode: Cardinal; const workingDir: string = ''); virtual;
    procedure RemoveRelatedEntries(const baseDir: string; const entries: TStrings); virtual;
  public
    constructor Create;

    procedure ConfigureCompilers(const fileName: string);
    procedure LoadSettings(const fileName: string);
    procedure SaveSettings(const fileName: string);

    procedure BuildAll;
    procedure CleanUp;
  end;

  ECommandLineException = class(Exception);
  EBuildException = class(Exception);

resourcestring
  SFailedToOpenRegistryKey = 'Failed to open the registry key: "%s".';
  SFailedToCreateProcess = 'Failed to create the process: "%s".';
  SBuildFailed = 'Failed to build the task: "%s"';

implementation

uses
  IniFiles,
  IOUtils,
  Windows,
  Spring,
  Spring.Utils;


type
  // Spelling is exactly as used in *.dproj files for "Platform" and $(Platform) entries.
  TKnownPlatforms = (
    Win32,
    Win64,
    OSX32,
    iOSSimulator,
    iOSDevice,
    Android
  );

const
  SPause = ' pause';
  ConfigurationNames: array[TConfigurationType] of string = (
    'Debug',
    'Release'
  );

procedure Log(const aLine: string); overload;
begin
{$IFDEF DEBUG}
  OutputDebugString(PChar(aLine));
{$ENDIF DEBUG}
end;

procedure Log(const aFormat: string; const aArguments: array of const); overload;
begin
{$IFDEF DEBUG}
  Log(Format(aFormat, aArguments));
{$ENDIF DEBUG}
end;

type
  TStringsHelper = class helper for TStrings
  public
    function AddIfNotContains(const S: string): Integer; overload;
    procedure AddIfNotContains(const values: TStrings); overload;
    function GetValueOrDefault(const name, defaultValue: string): string;
  end;

{$REGION 'TStringsHelper'}

function TStringsHelper.AddIfNotContains(const S: string): Integer;
begin
  Result := IndexOf(S);
  if Result = -1 then
    Result := Add(S);
end;

procedure TStringsHelper.AddIfNotContains(const values: TStrings);
var
  I: Integer;
  S: string;
begin
  BeginUpdate;
  try
    for I := 0 to values.Count - 1 do
    begin
      S := values[I];
      if IndexOf(S) = -1 then
        AddObject(S, values.Objects[I]);
    end;
  finally
    EndUpdate;
  end;
end;

function TStringsHelper.GetValueOrDefault(const name, defaultValue: string): string;
var
  index: Integer;
begin
  index := IndexOfName(name);
  if index > -1 then
    Result := ValueFromIndex[index]
  else
    Result := defaultValue;
end;

{$ENDREGION}


{$REGION 'TCompilerTargetBase'}

constructor TCompilerTargetBase.Create;
begin
  inherited Create;

  fBrowsingPaths := TStringList.Create;
  fBrowsingPaths.Delimiter := ';';
  fBrowsingPaths.StrictDelimiter := True;

  fEnvironmentVariables := TStringList.Create;
  fEnvironmentVariables.Delimiter := ';';
  fEnvironmentVariables.StrictDelimiter := True;

  fLibraryPaths := TStringList.Create;
  fLibraryPaths.Delimiter := ';';
  fLibraryPaths.StrictDelimiter := True;
end;

destructor TCompilerTargetBase.Destroy;
begin
  fBrowsingPaths.Free;
  fEnvironmentVariables.Free;
  fLibraryPaths.Free;

  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'TCompilerTarget'}

constructor TCompilerTarget.Create(const typeName: string; const properties: TStrings);
var
  bdsFilePath: string;
begin
  Guard.CheckTrue(typeName <> '', 'typeName');
  Guard.CheckNotNull(properties, 'properties');

  inherited Create;

  fDisplayName := properties.GetValueOrDefault('DisplayName', '');
  fTypeName := typeName;

  fRegistry := TRegistry.Create;
  fRegistry.RootKey := HKEY_CURRENT_USER;

  fTargetPlatform := properties.GetValueOrDefault('Platform', 'Win32');
  fKeys.BDS := properties.GetValueOrDefault('Keys.BDS', '');
  fKeys.LibraryKey := IncludeTrailingPathDelimiter(fKeys.BDS) + properties.GetValueOrDefault('Keys.Library', 'Library');
  fKeys.Globals := IncludeTrailingPathDelimiter(fKeys.BDS) + properties.GetValueOrDefault('Keys.Globals', 'Globals');
  fKeys.EnvironmentVariables := IncludeTrailingPathDelimiter(fKeys.BDS) + properties.GetValueOrDefault('Keys.EnvironmentVariables', 'Environment Variables');
  fNames.LibraryPath := properties.GetValueOrDefault('Names.LibraryPath', 'Search Path');
  fNames.BrowsingPath := properties.GetValueOrDefault('Names.BrowsingPath', 'Browsing Path');
  fNames.RootDir := properties.GetValueOrDefault('Names.RootDir', 'RootDir');

  fExists := fRegistry.KeyExists(fKeys.BDS);
  if fExists then
  begin
    EnsureOpenKey(fKeys.BDS);
    try
      bdsFilePath := fRegistry.ReadString('App');

      fExists := IdeAndDccExist(bdsFilePath, fTargetPlatform);

      if fExists then
        fRootDir := fRegistry.ReadString(fNames.RootDir)
      else
        fRootDir := '';
    finally
      fRegistry.CloseKey;
    end;
  end;

  if fExists then
    LoadOptions;
end;

destructor TCompilerTarget.Destroy;
begin
  fRegistry.Free;
  inherited Destroy;
end;

procedure TCompilerTarget.EnsureOpenKey(const key: string; canCreate: Boolean);
begin
  if not fRegistry.OpenKey(key, canCreate) then
    raise ERegistryException.CreateResFmt(@SFailedToOpenRegistryKey, [key]);
end;

procedure TCompilerTarget.LoadEnvironmentVariables(const environmentVariables: TStrings);
var
  i: Integer;
  name, value: string;
  separator: Char;
begin
  if fRegistry.KeyExists(Keys.EnvironmentVariables) then
  begin
    EnsureOpenKey(Keys.EnvironmentVariables);
    try
      fRegistry.GetValueNames(environmentVariables);
      separator := environmentVariables.NameValueSeparator;

      for i := 0 to environmentVariables.Count - 1 do
      begin
        name := environmentVariables.Strings[i];
        value := fRegistry.ReadString(name);
        environmentVariables.Strings[i] := name + separator + value;
      end;
    finally
      fRegistry.CloseKey;
    end;
  end;
end;

procedure TCompilerTarget.SaveEnvironmentVariables(const environmentVariables: TStrings);
var
  i: Integer;
begin
  EnsureOpenKey(Keys.EnvironmentVariables, True);
  try
    for i := 0 to environmentVariables.Count - 1 do
      fRegistry.WriteString(environmentVariables.Names[i],
        environmentVariables.ValueFromIndex[i]);
  finally
    fRegistry.CloseKey;
  end;
end;

procedure TCompilerTarget.LoadOptions;
var
  path: string;
begin
  EnsureOpenKey(Keys.LibraryKey);
  try
    path := fRegistry.ReadString(Names.LibraryPath);
    LibraryPaths.DelimitedText := path;
    path := fRegistry.ReadString(Names.BrowsingPath);
    BrowsingPaths.DelimitedText := path;
  finally
    fRegistry.CloseKey;
  end;

  LoadEnvironmentVariables(EnvironmentVariables);
end;

procedure TCompilerTarget.SaveOptions;
begin
  EnsureOpenKey(Keys.LibraryKey);
  try
    fRegistry.WriteString(Names.LibraryPath, LibraryPaths.DelimitedText);
    fRegistry.WriteString(Names.BrowsingPath, BrowsingPaths.DelimitedText);
  finally
    fRegistry.CloseKey;
  end;

  SaveEnvironmentVariables(EnvironmentVariables);

  EnsureOpenKey(Keys.Globals);
  try
    fRegistry.WriteString('ForceEnvOptionsUpdate', '1');
  finally
    fRegistry.CloseKey;
  end;
end;

function TCompilerTarget.GetDccFileName(const targetPlatform: string): string;
const // luckily, the compiler file names have not changed over the Delphi versions.
  CCommandLineCompilers: array[TKnownPlatforms] of string = (
    'dcc32.exe',
    'dcc64.exe',
    'dccosx.exe',
    'dccios32.exe',
    'dcciosarm.exe',
    'dccaarm.exe'
  );
var
  knownPlatform: TKnownPlatforms;
  knownPlatformName: string;
begin
  for knownPlatform := Low(TKnownPlatforms) to High(TKnownPlatforms) do
  begin
    knownPlatformName := TEnum.GetName(knownPlatform);
    if SameText(knownPlatformName, targetPlatform) then
    begin
      Result := CCommandLineCompilers[knownPlatform];
      Exit;
    end;
  end;
  Guard.RaiseArgumentException('targetPlatform');
end;

function TCompilerTarget.IdeAndDccExist(const bdsFilePath, targetPlatform: string): Boolean;
var
  bdsDirectory: string;
  dccFilePath: string;
  dccExists: Boolean;
  ideExists: Boolean;
begin
  if bdsFilePath = '' then
    Exit(False);

  ideExists := FileExists(bdsFilePath);
  Log('%d=Exists(%s)', [Ord(ideExists), bdsFilePath]);

  if ideExists then
  begin
    bdsDirectory := TPath.GetDirectoryName(bdsFilePath);

    dccFilePath := GetDccFileName(targetPlatform);
    dccFilePath := TPath.Combine(bdsDirectory, dccFilePath);
    dccExists := FileExists(dccFilePath);
    Log('%d=Exists(%s)', [Ord(dccExists), dccFilePath]);

    Result := ideExists and dccExists;
  end
  else
    Result := False;
end;

{$ENDREGION}


{$REGION 'TBuildTask'}

constructor TBuildTask.Create;
begin
  inherited Create;

  fProjects := TStringList.Create;
  fProjects.Delimiter := ';';
  fProjects.StrictDelimiter := True;
end;

destructor TBuildTask.Destroy;
begin
  fProjects.Free;

  inherited Destroy;
end;

function TBuildTask.Name: string;
begin
  Result := fCompiler.DisplayName;
end;

function TBuildTask.CanBuild: Boolean;
begin
  Result := fCompiler.Exists;
end;

{$ENDREGION}


{$REGION 'TBuildEngineBase'}

constructor TBuildEngineBase.Create;
begin
  inherited Create;

  fSelectedTasks := TCollections.CreateList<TBuildTask>;
  fTargets := TCollections.CreateObjectList<TCompilerTarget>;
  fTasks := TCollections.CreateObjectList<TBuildTask>;

  fSourcePaths := TStringList.Create;
  fSourcePaths.Delimiter := ';';
  fSourcePaths.StrictDelimiter := True;
end;

destructor TBuildEngineBase.Destroy;
begin
  fSourcePaths.Free;

  inherited Destroy;
end;

{$ENDREGION}


{$REGION 'TBuildEngine'}

constructor TBuildEngine.Create;
begin
  inherited Create;

  fConfigurationType := TConfigurationType.Release;
end;

procedure TBuildEngine.ExecuteCommandLine(const applicationName, commandLine: string;
  var exitCode: Cardinal; const workingDir: string);
var
  localCommandLine: string;
  startupInfo: TStartupInfo;
  processInfo: TProcessInformation;
  currentDir: PChar;
begin
  startupInfo := Default(TStartupInfo);
  processInfo := Default(TProcessInformation);
  startupInfo.cb := SizeOf(startupInfo);
  localCommandLine := commandLine;
  UniqueString(localCommandLine);
  if workingDir <> '' then
    currentDir := PChar(workingDir)
  else
    currentDir := nil;
  if not CreateProcess(PChar(applicationName), PChar(localCommandLine),
    nil, nil, True, 0, nil, currentDir, startupInfo, processInfo) then
  begin
    raise ECommandLineException.CreateResFmt(@SFailedToCreateProcess, [applicationName]);
  end;
  try
    WaitForSingleObject(processInfo.hProcess, INFINITE);
    GetExitCodeProcess(processInfo.hProcess, exitCode);
  finally
    CloseHandle(processInfo.hProcess);
    CloseHandle(processInfo.hThread);
  end;
end;

procedure TBuildEngine.BuildAll;
var
  task: TBuildTask;
begin
  for task in SelectedTasks do
    BuildTarget(task);
end;

procedure TBuildEngine.BuildTarget(const task: TBuildTask);
var
  projectPath: string;
  unitOutputPath: string;
  configurationName: string;
  projectName: string;
  cmdFileName: string;
  commandLine: string;
  exitCode: Cardinal;
  targetPlatform: string;
  rsVars: string;
  target: TCompilerTarget;
begin
  Guard.CheckNotNull(task, 'task');

  target := task.Compiler;

  projectPath := ExtractFilePath(ParamStr(0));
  configurationName := ConfigurationNames[ConfigurationType];

  RemoveRelatedEntries(projectPath, target.LibraryPaths);
  RemoveRelatedEntries(projectPath, target.BrowsingPaths);

  unitOutputPath := projectPath + task.UnitOutputPath;
  unitOutputPath := StringReplace(unitOutputPath, '$(Config)', configurationName, [rfIgnoreCase, rfReplaceAll]);
  targetPlatform := target.TargetPlatform;
  unitOutputPath := StringReplace(unitOutputPath, '$(Platform)', targetPlatform, [rfIgnoreCase, rfReplaceAll]);

  target.LibraryPaths.AddIfNotContains(unitOutputPath);
  target.BrowsingPaths.AddIfNotContains(SourcePaths);
  if fModifyDelphiRegistrySettings then
    target.SaveOptions;

  cmdFileName := IncludeTrailingPathDelimiter(TEnvironment.GetFolderPath(sfSystem)) + 'cmd.exe';
  rsVars := IncludeTrailingPathDelimiter(target.RootDir) + 'bin\rsvars.bat';
  for projectName in task.Projects do
  begin
    commandLine := Format('/C BuildHelper "%0:s" "%1:s" "Config=%2:s" "Platform=%3:s"', [
      rsVars, projectName, configurationName, targetPlatform]);
    if fPauseAfterEachStep then
      commandLine := commandLine + SPause;
    ExecuteCommandLine(cmdFileName, commandLine, exitCode);
    if exitCode <> 0 then
      raise EBuildException.CreateResFmt(@SBuildFailed, [projectName]);
  end;

  if fRunTests then
  begin
    if (targetPlatform = 'Win32') or (targetPlatform = 'Win64') then
    begin
      commandLine := Format('%0:s\Tests\Bin\%1:s\Spring.Tests.exe', [
        ExcludeTrailingPathDelimiter(projectPath),
        StringReplace(task.Compiler.TypeName, '.', '\', [])]);
      ExecuteCommandLine(commandLine, '', exitCode, ExtractFileDir(commandLine));
    end;
  end;
end;

procedure TBuildEngine.CleanUp;
var
  cmdFileName: string;
  commandLine: string;
  exitCode: Cardinal;
begin
  cmdFileName := IncludeTrailingPathDelimiter(TEnvironment.GetFolderPath(sfSystem)) + 'cmd.exe';
  commandLine := '/C Clean.bat';
  if PauseAfterEachStep then
    commandLine := commandLine + SPause;
  ExecuteCommandLine(cmdFileName, commandLine, exitCode);
end;

procedure TBuildEngine.ConfigureCompilers(const fileName: string);
var
  iniFile: TCustomIniFile;
  sections: TStrings;
  properties: TStrings;
  sectionName: string;
  target: TCompilerTarget;
begin
  CheckFileExists(fileName);

  Targets.Clear;

  iniFile := TIniFile.Create(fileName);
  sections := TStringList.Create;
  properties := TStringList.Create;
  try
    iniFile.ReadSections(sections);

    for sectionName in sections do
    begin
      iniFile.ReadSectionValues(sectionName, properties);
      target := TCompilerTarget.Create(sectionName, properties);
      Targets.Add(target);
    end;
  finally
    properties.Free;
    sections.Free;
    iniFile.Free;
  end;
end;

procedure TBuildEngine.LoadSettings(const fileName: string);
var
  iniFile: TCustomIniFile;
  sections: TStrings;
  sectionName: string;
  config: string;
  target: TCompilerTarget;
  task: TBuildTask;
  i: Integer;
  selectedTasks: TStrings;
begin
  iniFile := TIniFile.Create(fileName);
  sections := TStringList.Create;
  selectedTasks := TStringList.Create;
  selectedTasks.Delimiter := ';';
  try
    config := iniFile.ReadString('Globals', 'Config', 'Debug');
    fConfigurationType := TEnum.Parse<TConfigurationType>(config);
    fSourceBaseDir := iniFile.ReadString('Globals', 'SourceBaseDir', '');
    fSourceBaseDir := ApplicationPath + fSourceBaseDir;
    fSourcePaths.DelimitedText := iniFile.ReadString('Globals', 'SourcePaths', '');
    for i := 0 to fSourcePaths.Count - 1 do
      fSourcePaths[i] := IncludeTrailingPathDelimiter(fSourceBaseDir) + fSourcePaths[i];
    selectedTasks.DelimitedText := iniFile.ReadString('Globals', 'SelectedTasks', '');
    fPauseAfterEachStep := iniFile.ReadBool('Globals', 'PauseAfterEachStep', False);
    fRunTests := iniFile.ReadBool('Globals', 'RunTests', False);
    fModifyDelphiRegistrySettings := iniFile.ReadBool('Globals', 'ModifyDelphiRegistrySettings', False);

    for target in Targets do
    begin
      { The sections in Build.Settings.ini and Build.Settings.Compilers.ini }
      sectionName := target.TypeName;
      if iniFile.SectionExists(sectionName) then
      begin
        task := TBuildTask.Create;
        Tasks.Add(task);
        task.Compiler := target;
        task.Projects.DelimitedText := iniFile.ReadString(sectionName, 'Projects', '');
        task.UnitOutputPath := iniFile.ReadString(sectionName, 'UnitOutputPaths', '');
        if task.CanBuild and ((selectedTasks.Count = 0) or (selectedTasks.IndexOf(sectionName) > -1)) then
          fSelectedTasks.Add(task);
      end;
    end;
  finally
    selectedTasks.Free;
    sections.Free;
    iniFile.Free;
  end;
end;

procedure TBuildEngine.RemoveRelatedEntries(const baseDir: string; const entries: TStrings);
var
  s: string;
  i: Integer;
begin
  Guard.CheckNotNull(entries, 'entries');

  for i := entries.Count - 1 downto 0 do
  begin
    s := entries[i];
    if (Pos(baseDir, s) > 0) {or (Pos('$(SPRING)', entry) > 0)} then
      entries.Delete(i);
  end;
end;

procedure TBuildEngine.SaveSettings(const fileName: string);
var
  iniFile: TCustomIniFile;
  selectedTasks: TStrings;
  task: TBuildTask;
begin
  iniFile := TIniFile.Create(fileName);
  selectedTasks := TStringList.Create;
  selectedTasks.Delimiter := ';';
  try
    for task in fSelectedTasks do
      selectedTasks.Add(task.Compiler.TypeName);
    iniFile.WriteString('Globals', 'Config', ConfigurationNames[ConfigurationType]);
    iniFile.WriteString('Globals', 'SelectedTasks', selectedTasks.DelimitedText);
    iniFile.WriteBool('Globals', 'PauseAfterEachStep', PauseAfterEachStep);
    iniFile.WriteBool('Globals', 'RunTests', RunTests);
    iniFile.WriteBool('Globals', 'ModifyDelphiRegistrySettings', ModifyDelphiRegistrySettings);
  finally
    selectedTasks.Free;
    iniFile.Free;
  end;
end;

{$ENDREGION}


end.
