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
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'User';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Patches';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);



    //===  Check that the 'Factory\Patches' directory exists ===
    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + 'Factory';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);

    DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Factory') + 'Patches';
    if (DirectoryExists(DataDir) = false) then CreateDir(DataDir);


    //=== Check that the samples directories config file exists ===
    DataFileName := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Sample Directories.xml';
    if not FileExists(DataFilename) then
    begin
      SampleDirectories := TSampleDirectories.Create;
      AutoFree(@SampleDirectories);

      SampleDirectories.DataFileName := DataFileName;

      DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('Factory') + 'Patches';
      SampleDirectories.AddSampleDirectory('Factory Patches', DataDir);

      DataDir := IncludeTrailingPathDelimiter(PluginDataDir^.Path) + IncludeTrailingPathDelimiter('User') + 'Patches';
      SampleDirectories.AddSampleDirectory('User Patches', DataDir);


      //TODO: add the samples directory here...

      SampleDirectories.WriteDirectoryInfoToFile(DataFilename);
    end;





  end;
end;

end.
