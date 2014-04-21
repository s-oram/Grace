unit uLucidityData;

interface

uses
  Generics.Collections,
  VamLib.ZeroObject;



type
  {
    Lucidity uses two data directories.
    - \Data\Factory
    - \Data\User
    The FACTORY directory contains all data files that are default to
    a installation. They will sometimes be deleted or over-written if
    Lucidity is re-installed.

    The USER directory can duplicate the contents FACTORY directory.
    When a file is found in both the USER and FACTORY directory, the USER
    file takes precedence and overrides the FACTORY file.

    Files in the USER directory will never be deleted or over-written by
    the application or application installer.
  }


  TLucidityData = class
  private
    // FixFilePath() replaces '\' with the correct path delimiter for the system.
    class function FixFilePath(FileName : string):string;
  public
    // Checks if a data file exists in either the FACTORY or USER directories.
    class function DataFileExists(RelativeFileName : string):boolean;

    // if the data file exists (in the FACTORY or USER directories),
    // this function expands the relative filename.
    // If the data file doesn't exist, the expanded filename uses
    // the FACTORY directory.
    class function ExpandFileName(RelativeFileName : string):string;
  end;



  TDirectoryInfo = record
    Name : string;
    Path : string;
  end;

  TDirectoryInfoList = TList<TDirectoryInfo>;

  TSampleDirectories = class(TZeroObject)
  private
    // TODO: It could be possible to make this class
    // use a single class variable to hold the directory list.
    // That way all instances of lucidity would use the same directory list.
    DirectoryList : TDirectoryInfoList;
    fDataFileName: string;
    procedure SetDataFileName(const Value: string);
    function GetCount: integer;
    function GetDirectoryInfo(Index: integer): TDirectoryInfo;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RefreshDirectoryList;

    procedure MoveDirectoryUp(Index : integer);
    procedure MoveDirectoryDown(Index : integer);

    procedure AddSampleDirectory(const Name, Path : string);
    procedure RemoveSampleDirectory(const Index : integer);

    procedure WriteDirectoryInfoToFile(const FileName : string);
    procedure ReadDirectoryInfoFromfile(const FileName : string);

    //DataFileName is the data file that holds all the sample directories.
    property DataFileName : string read fDataFileName write SetDataFileName;

    property Count : integer read GetCount;
    property DirectoryInfo[Index : integer]: TDirectoryInfo read GetDirectoryInfo; default;

  end;







implementation

uses
  uAutoFree,
  SysUtils, uDataFolderUtils,
  eeSaveLoadFunctions,
  NativeXML;


type
  TLucidityDataConfig = record
    IsInitialized : boolean;
    RootDataDir : string;
  end;

var
  GlobalDataConfig : TLucidityDataConfig;


procedure InitGlobalData;
var
  Dir : string;
begin
  GlobalDataConfig.IsInitialized := true;

  Dir := LocateDataFolder('One Small Clue', 'Lucidity x32');

  if DirectoryExists(Dir) = false then
  begin
    raise Exception.Create('Lucidity: Can not find data folder. Please re-install or contact support.');
  end else
  begin
    GlobalDataConfig.RootDataDir := Dir;
  end;

end;



{ TLucidityData }

class function TLucidityData.FixFilePath(FileName: string): string;
var
  PathDelimiter : string;
begin
  PathDelimiter := IncludeTrailingPathDelimiter('');

  if PathDelimiter <> '\'
    then result := StringReplace(FileName, '\', PathDelimiter, [rfReplaceAll])
    else result := FileName;
end;



class function TLucidityData.DataFileExists(RelativeFileName: string): boolean;
var
  fnA, fnB : string;
begin
  if GlobalDataConfig.IsInitialized = false then InitGlobalData;

  RelativeFileName := FixFilePath(RelativeFileName);

  fnA := IncludeTrailingPathDelimiter(GlobalDataConfig.RootDataDir) + 'Factory' + RelativeFileName;
  fnB := IncludeTrailingPathDelimiter(GlobalDataConfig.RootDataDir) + 'User'    + RelativeFileName;

  if FileExists(fnA) or FileExists(fnB)
    then result := true
    else result := false;

end;

class function TLucidityData.ExpandFileName(RelativeFileName: string): string;
var
  fnA, fnB : string;
begin
  if GlobalDataConfig.IsInitialized = false then InitGlobalData;

  RelativeFileName := FixFilePath(RelativeFileName);

  fnA := IncludeTrailingPathDelimiter(GlobalDataConfig.RootDataDir) + 'Factory' + RelativeFileName;
  fnB := IncludeTrailingPathDelimiter(GlobalDataConfig.RootDataDir) + 'User'    + RelativeFileName;

  if FileExists(fnB)
    then result := fnB
    else result := fnA;

end;

{ TLucidityDirectories }

constructor TSampleDirectories.Create;
begin
  DirectoryList := TDirectoryInfoList.Create;
end;

destructor TSampleDirectories.Destroy;
begin
  DirectoryList.Free;
  inherited;
end;

function TSampleDirectories.GetCount: integer;
begin
  result := DirectoryList.Count;
end;

function TSampleDirectories.GetDirectoryInfo(Index: integer): TDirectoryInfo;
begin
  result := DirectoryList[Index];
end;




procedure TSampleDirectories.SetDataFileName(const Value: string);
begin
  fDataFileName := Value;
end;

procedure TSampleDirectories.RefreshDirectoryList;
begin
  if (fDataFileName <> '') and (FileExists(fDataFileName)) then
  begin
    ReadDirectoryInfoFromfile(fDataFileName);
  end;
end;

procedure TSampleDirectories.AddSampleDirectory(const Name, Path: string);
var
  DirInfo : TDirectoryInfo;
begin
  DirInfo.Name := Name;
  DirInfo.Path := Path;

  DirectoryList.Add(DirInfo);

  if (fDataFileName <> '') then
  begin
    WriteDirectoryInfoToFile(fDataFileName);
  end;
end;

procedure TSampleDirectories.RemoveSampleDirectory(const Index: integer);
begin
  DirectoryList.Delete(Index);

  if (fDataFileName <> '') then
  begin
    WriteDirectoryInfoToFile(fDataFileName);
  end;
end;

procedure TSampleDirectories.MoveDirectoryUp(Index: integer);
begin
  if Index > 0 then
  begin
    DirectoryList.Move(Index, Index-1);
  end;

  if (fDataFileName <> '') then
  begin
    WriteDirectoryInfoToFile(fDataFileName);
  end;
end;

procedure TSampleDirectories.MoveDirectoryDown(Index: integer);
begin
  if Index < DirectoryList.Count-1 then
  begin
    DirectoryList.Move(Index, Index+1);
  end;

  if (fDataFileName <> '') then
  begin
    WriteDirectoryInfoToFile(fDataFileName);
  end;
end;

procedure TSampleDirectories.WriteDirectoryInfoToFile(const FileName: string);
var
  XML : TNativeXML;
  RootNode : TXmlNode;
  DataDirNode : TXmlNode;
  c1: Integer;
begin
  Xml := TNativeXml.CreateName('root');
  try
    RootNode := Xml.Root;

    for c1 := 0 to self.Count-1 do
    begin
      DataDirNode := RootNode.NodeNew('SampleDir');
      DataDirNode.NodeNew('Name').ValueUnicode := DirectoryInfo[c1].Name;
      DataDirNode.NodeNew('Path').ValueUnicode := DirectoryInfo[c1].Path;
    end;

    XML.XmlFormat := xfReadable;
    XML.SaveToFile(FileName);
  finally
    Xml.Free;
  end;
end;

procedure TSampleDirectories.ReadDirectoryInfoFromfile(const FileName: string);
var
  XML : TNativeXML;
  RootNode : TXmlNode;
  DataDirNode : TXmlNode;
  c1: Integer;
  Name, Path : string;
  DirInfo : TDirectoryInfo;
  SampleDirNodes : TSdNodeList;
begin
  if FileExists(FileName) = false then raise Exception.Create('File doesn''t exist. (' + FileName + ')');

  DirectoryList.Clear;

  SampleDirNodes := TsdNodeList.Create;
  AutoFree(@SampleDirNodes);

  Xml := TNativeXml.Create(nil);
  try
    Xml.LoadFromFile(FileName);

    if assigned(Xml.Root) then
    begin
      RootNode := Xml.Root;

      RootNode.FindNodes('SampleDir', SampleDirNodes);

      for c1 := 0 to SampleDirNodes.Count-1 do
      begin
        DataDirNode := SampleDirNodes[c1];

        ReadNodeValue(DataDirNode, 'Name', Name);
        ReadNodeValue(DataDirNode, 'Path', Path);

        if (Name <> '') and (Path <> '') then
        begin
          DirInfo.Name := Name;
          DirInfo.Path := Path;
          DirectoryList.Add(DirInfo);
        end;
      end;
    end;

  finally
    Xml.Free;
  end;
end;


initialization
  GlobalDataConfig.IsInitialized := false;

finalization

end.
