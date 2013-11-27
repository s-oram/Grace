{
  Unit to encapsulate finding the plugin data directory.
}

unit eePluginDataDir;


interface

const
  kMidiMapsDirName = 'Midi Maps';
  kSkinsDirName    = 'Skins';

type
  // NOTE: All directory path string should be returned without a trailing path delimiter.
  PPluginDataDirInfo = ^TPluginDataDirInfo;
  TPluginDataDirInfo = record
    Path   : string;
    Exists : boolean;
    function MidiMapsDir : string;
    function SkinsDir    : string;
  end;

function PluginDataDir : PPluginDataDirInfo;

function PluginDataSubDirExists(SubDirName:string):boolean;
function GetPluginDataSubDir(SubDirName:string):string;

implementation

uses
  SysUtils, eeVstExtra, vam.Win.Links;

var
  IsDataDirInfoValid : boolean;
  DataDirInfo : TPluginDataDirInfo;


function PluginDataDir : PPluginDataDirInfo;
var
  VstDir : string;
  VstFn  : string;
  DataDirLinkFileName : string;
  DataDir : string;
  TestDir : string;
begin
  if IsDataDirInfoValid = false then
  begin
    DataDir := '';
    VstDir := GetDLLDirectory;
    VstFn  := GetDLLFilename;
    VstFn  := RemoveFileExt(VstFn);

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
    if (DataDir <> '') and (DirectoryExists(DataDir)) then
    begin
      DataDirInfo.Path := DataDir;
      DataDirInfo.Exists := true;
      IsDataDirInfoValid := true;
    end else
    begin
      DataDirInfo.Path := '';
      DataDirInfo.Exists := false;
      IsDataDirInfoValid := true;
    end;

  end;

  result := @DataDirInfo;
end;



{ TPluginDataDirInfo }

function TPluginDataDirInfo.MidiMapsDir: string;
var
  Dir : string;
begin
  if Path = '' then raise Exception.Create('Data dir path not set.');
  if DirectoryExists(Path) = false then raise Exception.Create('Data dir not found.');

  Dir := IncludeTrailingPathDelimiter(Path) + kMidiMapsDirName;

  if (DirectoryExists(Dir) = false) and (CreateDir(Dir) = false) then raise Exception.Create('Midi Maps dir not found and couldn''t be created.');

  result := Dir;
end;

function TPluginDataDirInfo.SkinsDir: string;
var
  Dir : string;
begin
  if Path = '' then raise Exception.Create('Data dir path not set.');
  if DirectoryExists(Path) = false then raise Exception.Create('Data dir not found.');

  Dir := IncludeTrailingPathDelimiter(Path) + kSkinsDirName;

  if (DirectoryExists(Dir) = false) and (CreateDir(Dir) = false) then raise Exception.Create('Skins dir not found and couldn''t be created.');

  result := Dir;
end;


function PluginDataSubDirExists(SubDirName:string):boolean;
var
  Dir : string;
begin
  if PluginDataDir^.Exists = false then
  begin
    result := false;
    exit;
  end;

  Dir := IncludeTrailingPathDelimiter(PluginDataDir.Path) + SubDirName;
  result := DirectoryExists(Dir)
end;

function GetPluginDataSubDir(SubDirName:string):string;
var
  Dir : string;
begin
  if PluginDataDir^.Exists = false then raise Exception.Create('Can not locate plugin data directory.');

  Dir := IncludeTrailingPathDelimiter(PluginDataDir.Path) + SubDirName;

  if (DirectoryExists(Dir) = false) and (CreateDir(Dir) = false)
    then raise Exception.Create('Data directory (' + SubDirName + ') not found and could not be created.');

  result := Dir;
end;



initialization
  IsDataDirInfoValid := false;
finalization

end.
