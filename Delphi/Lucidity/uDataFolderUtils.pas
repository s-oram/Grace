unit uDataFolderUtils;

interface

//===== Public Function Declarations ========================
function LocateDataFolder(CompanyName, ProductName:string):string;
//===========================================================



//===== Private Function Declarations - Do Not Use! =========
function GetDataDirFromXML(XmlFileName:string):string;
//===========================================================


implementation

uses
  SysUtils, WinShell, Windows, eeVstExtra, NativeXml, uAutoFree;


function LocateDataFolder(CompanyName, ProductName:string):string;
var
  DataFileName : string;
  xml : TNativeXml;
  VstDir : string;
  XmlDataDir : string;
  TempDir    : string;
  RegKey     : string;
begin

  VstDir := GetDllDirectory;
  VstDir := IncludeTrailingPathDelimiter(VstDir);

  // Load the xml data file, normally in the same location as the VST plugin.
  // The xml data file should tell us the location of the data folder.
  DataFileName := VstDir + ProductName + ' Data.xml';
  try
    XmlDataDir := GetDataDirFromXml(DataFileName);
  except
    result := '';
    exit; //=============================>> exit >>===========>>
  end;


  if DirectoryExists(XmlDataDir) then
  begin
    result := XmlDataDir;
    exit; //=============================>> exit >>===========>>
  end;

  if DirectoryExists(VstDir + XmlDataDir) then
  begin
    result := VstDir + XmlDataDir;
    exit; //=============================>> exit >>===========>>
  end;

  try
    RegKey := '\Software\' + CompanyName + '\' + ProductName;
    TempDir := GetRegistryData(HKEY_LOCAL_MACHINE, RegKey, 'DataPath');
    if DirectoryExists(TempDir) then
    begin
      result := TempDir;
      exit; //=============================>> exit >>===========>>
    end;
  except
    // NOTE: Querying the registry may raise exceptions, but we don't care if
    // it does. Just catch the exceptions and move on.
  end;



  //If we've made it this far, something has gone wrong.
  result := '';
  exit;
end;


function GetDataDirFromXML(XmlFileName:string):string;
var
  xml : TNativeXml;
  Node : TXmlNode;
  Dir  : string;
begin
  Dir := '';

  if FileExists(XmlFilename) then
  begin
    try
      xml := TNativeXML.Create(nil);
      AutoFree(@xml);

      xml.LoadFromFile(XmlFileName);
      if assigned(xml.Root) then
      begin
        Node := xml.Root.FindNode('datadir');
        if assigned(Node) then
        begin
          Dir := string(Node.ValueUnicode);
        end;
      end;
    except
      Dir := '';
    end;
  end;

  result := dir;
end;


end.
