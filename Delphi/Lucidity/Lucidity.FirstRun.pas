unit Lucidity.FirstRun;

interface

{$INCLUDE Defines.inc}

procedure FirstRunSetup;

procedure InstallFont(FontFile : string);

implementation

uses
  {$IFDEF Logging}VamLib.LoggingProxy,{$ENDIF}
  WinApi.Windows,
  WinApi.Messages,
  SysUtils,
  eePluginDataDir,
  uLucidityData,
  VamLib.Utils;

procedure InstallFont(FontFile : string);
var
  fn : string absolute FontFile;
  r : integer;
begin
  {$IFDEF Logging}Log.TrackMethod('InstallFont()');{$ENDIF}
  {$IFDEF Logging}Log.LogMessage('FontFile = ' + fn);{$ENDIF}

  if FileExists(fn) then
  begin
    r := AddFontResource(pWideChar(fn)) ;

    {$IFDEF Logging}
    if r <> 0
      then Log.LogMessage('Font successfully added.')
      else Log.LogMessage('Font install failed.');
    {$ENDIF}
    //if r <> 0 then SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
  end;
end;

procedure UninstallFont(FontFile : string);
var
  fn : string absolute FontFile;
  r : boolean;
begin
  {$IFDEF Logging}Log.TrackMethod('UninstallFont()');{$ENDIF}
  {$IFDEF Logging}Log.LogMessage('FontFile = ' + fn);{$ENDIF}

  if FileExists(fn) then
  begin
    r := RemoveFontResource(pWideChar(fn)) ;
    {$IFDEF Logging}
    if r = true
      then Log.LogMessage('Font successfully removed.')
      else Log.LogMessage('Font not removed. (FAIL)');
    {$ENDIF}
    //if r = true then SendMessage(HWND_BROADCAST, WM_FONTCHANGE, 0, 0);
  end;
end;


procedure FirstRunSetup;
var
  SampleDirectories: TSampleDirectories;
  DataFileName : string;
  DataDir : string;
  fn : string;
begin
  {$IFDEF Logging}Log.TrackMethod('FirstRunSetup');{$ENDIF}

  if (PluginDataDir^.Exists) then
  begin
    //=== Check that the 'User\Patches' directory exists ===
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Patches';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Config Default';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Config User Override';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);


    //=== Check that the samples directories config file exists ===
    DataFileName := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Config User Override') + 'Sample Directories.xml';
    if not FileExists(DataFilename) then
    begin
      SampleDirectories := TSampleDirectories.Create;
      AutoFree(@SampleDirectories);

      SampleDirectories.DataFileName := DataFileName;

      DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Patches') + 'Factory';
      SampleDirectories.AddSampleDirectory('Factory Patches', DataDir);

      DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Patches') + 'User';
      SampleDirectories.AddSampleDirectory('User Patches', DataDir);

      //TODO: add the samples directory here...
      SampleDirectories.WriteDirectoryInfoToFile(DataFilename);
    end;

    fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Resources') + 'LiberationSans-Regular.ttf';
    InstallFont(fn);

    fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Resources') + 'LiberationSans-Bold.ttf';
    InstallFont(fn);


  end;
end;


var
  fn : string;

initialization


finalization
  fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Resources') + 'LiberationSans-Regular.ttf';
  UninstallFont(fn);

  fn := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Resources') + 'LiberationSans-Bold.ttf';
  UninstallFont(fn);


end.
