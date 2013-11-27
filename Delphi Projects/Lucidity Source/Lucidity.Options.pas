unit Lucidity.Options;

interface

uses
  VamLib.Collections.RecordArray;

type
  PSoundEditorInfo = ^TSoundEditorInfo;
  TSoundEditorInfo = record
    ApplicationName : string; // Name of the application to show in menus etc.
    ApplicationExe  : string; // Filepath to executable.
  end;

  TSoundEditors = TRecordArray<TSoundEditorInfo>;

  TOptions = class
  private
    fSoundEditors: TSoundEditors;
    fAutoSaveFileName: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ReadFromFile(FileName : string);
    procedure WriteToFile(FileName : string);

    property SoundEditors : TSoundEditors read fSoundEditors;

    procedure AddNewSoundEditor(const AppExe : string);

    property AutoSaveFileName : string read fAutoSaveFileName write fAutoSaveFileName;
  end;

implementation

uses
  uAutoFree,
  NativeXml,
  NativeXmlEx,
  eeFunctions;

{ TOptions }

constructor TOptions.Create;
begin
  AutoSaveFileName := '';
end;

destructor TOptions.Destroy;
begin

  inherited;
end;

procedure TOptions.AddNewSoundEditor(const AppExe: string);
var
  IsSoundEditorInList : boolean;
  InfoIndex : integer;
  c1: Integer;
  appName : string;
  s : string;

  Info : PSoundEditorInfo;
begin
  IsSoundEditorInList := false;

  appName := RemoveFileExt(AppExe);

  for c1 := 0 to SoundEditors.Count-1 do
  begin
    if SoundEditors[c1].ApplicationName = AppName then
    begin
      IsSoundEditorInList := true;
      Info := @SoundEditors.Raw[c1];
      break;
    end;
  end;


  if IsSoundEditorInList = false then
  begin
    Info := SoundEditors.New;
  end;

  Info^.ApplicationName := AppName;
  Info^.ApplicationExe  := AppExe;


  if AutoSaveFileName <> '' then
  begin
    WriteToFile(AutoSaveFileName);
  end;
end;

procedure TOptions.ReadFromFile(FileName: string);
var
  XML : TNativeXML;
  NodeList : TsdNodeList;
  c1: Integer;
  AppName, AppExe : string;
begin
  XML := TNativeXML.Create(nil);
  try
    XML.LoadFromFile(FileName);
    //==================================

    if assigned(xml.Root) then
    begin
      NodeList := TsdNodeList.Create;
      AutoFree(@NodeList);

      xml.Root.FindNodes('SoundEditor', NodeList);

      SoundEditors.Count := NodeList.Count;

      for c1 := 0 to NodeList.Count-1 do
      begin
        // TODO: There will likely be an exception raised if the value doesn't translate to unicode.
        // Maybe there needs to be a 'Safe' value, or DataIO_ValueAs... method.
        SoundEditors.Raw[c1].ApplicationName := NodeWiz(NodeList[c1]).ValueUnicode('AppName');
        SoundEditors.Raw[c1].ApplicationExe  := NodeWiz(NodeList[c1]).ValueUnicode('AppExe');
      end;
    end;

    //===================================
  finally
    XML.Free;
  end;
end;

procedure TOptions.WriteToFile(FileName: string);
var
  XML : TNativeXML;
  Node : TXmlNode;
  ChildNode : TXmlNode;
  c1: Integer;
begin
  XML := TNativeXML.CreateName('root');
  try
    Node := xml.Root;

    for c1 := 0 to SoundEditors.Count-1 do
    begin
      Node := NodeWiz(xml.Root).CreateNode('SoundEditor');

      Node.NodeNew('AppName').ValueUnicode := SoundEditors[c1].ApplicationName;
      Node.NodeNew('AppExe').ValueUnicode := SoundEditors[c1].ApplicationExe;
    end;


    XML.XmlFormat := xfReadable;
    XML.SaveToFile(FileName);
  finally
    XML.Free;
  end;
end;

end.
