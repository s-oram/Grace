unit Lucidity.FirstRun;

interface

procedure FirstRunSetup;

implementation

uses
  SysUtils,
  eePluginDataDir,
  uLucidityData,
  VamLib.Utils;

procedure FirstRunSetup;
var
  SampleDirectories: TSampleDirectories;
  DataFileName : string;
  DataDir : string;
begin
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





  end;
end;

end.
