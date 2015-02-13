unit ee3AddOn.PluginPaths;

interface

function GetPluginDllFileName  : string;
function GetPluginDllDirectory : string;
function GetPluginDataDirectory : string;

implementation

uses
  IOUtils,
  SysUtils,
  VamLib.Logging,
  VamLib.Utils,
  VamLib.Win.Links,
  ee3.Utils;

function GetPluginDllFileName  : string;
var
  s : string;
begin
  s := ee3.Utils.GetDLLFilename;
  result := s;
end;

function GetPluginDLLDirectory : string;
var
  s : string;
begin
  s := ee3.Utils.GetDLLDirectory;
  if not DirectoryExists(s) then raise Exception.Create('Plugin DLL Directory doesn''t exist.'); //This would be a highly unexpected error!!
  result := s;
end;

function GetPluginDataDirectory : string;
var
  VstDir : string;
  VstFn  : string;
  DataDirLinkFileName : string;
  DataDir : string;
  TestDir : string;
begin
  DataDir := '';
  VstDir := GetPluginDLLDirectory;
  VstFn  := TPath.GetFileNameWithoutExtension(GetPluginDllFileName);

  //=== Look for a link to the data directory. "<plugin name> Data.lnk" =====
  DataDirLinkFileName := IncludeTrailingPathDelimiter(VstDir) + VstFn + ' Data.lnk';
  if FileExists(DataDirLinkFileName) then
  begin
    TestDir := GetLinkTarget(DataDirLinkFileName);
    if DirectoryExists(TestDir) then
    begin
      DataDir := TestDir;
    end;
  end;

  //===== Look for the data directory in the same location as the plugin dll ======
  if (DataDir = '') or (DirectoryExists(DataDir) = false) then
  begin
    TestDir := IncludeTrailingPathDelimiter(VstDir) + VstFn + ' Data';
    if DirectoryExists(TestDir) then
    begin
      DataDir := TestDir;
    end;
  end;

  //==== Set fields based on whether the data directory has been found or not =====
  if (DataDir <> '') and (DirectoryExists(DataDir))
    then result := DataDir
    else result := '';
end;

end.
