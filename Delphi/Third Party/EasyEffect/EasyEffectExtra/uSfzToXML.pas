unit uSfzToXML;

interface

uses
  uSfzParser, NativeXML;


procedure LoadSfzAsXML(const SfzFileName : string; XML:TNativeXML);

type
  TSfzToXmlLoader = class
  private
  protected
    SfzParser  : TSfzParser;
    XmlDoc     : TNativeXml;
    ActiveNode : TXmlNode;
    LastRegion : TXmlNode;
    LastGroup  : TXmlNode;
    procedure FileStart(Sender:TObject);
    procedure FileEnd(Sender:TObject);
    procedure RegionStart(Sender:TObject);
    procedure RegionEnd(Sender:TObject);
    procedure GroupStart(Sender:TObject);
    procedure GroupEnd(Sender:TObject);
    procedure Opcode(Sender:TObject; const Opcode, Value:string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Load(const SfzFileName : string; XML:TNativeXML);
  end;

implementation

uses
  VamLib.Utils;

procedure LoadSfzAsXML(const SfzFileName : string; XML:TNativeXML);
var
  SfzLoader : TSfzToXmlLoader;
begin
  SfzLoader := TSfzToXmlLoader.Create;
  AutoFree(@SfzLoader);

  SfzLoader.Load(SfzFileName, XML);
end;

{ TSfzToXmlLoader }

constructor TSfzToXmlLoader.Create;
begin
  SfzParser := TSfzParser.Create;
  SfzParser.OnFileStart   := FileStart;
  SfzParser.OnFileEnd     := FileEnd;
  SfzParser.OnGroupStart  := GroupStart;
  SfzParser.OnGroupEnd    := GroupEnd;
  SfzParser.OnRegionStart := RegionStart;
  SfzParser.OnRegionEnd   := RegionEnd;
  SfzParser.OnOpcode      := Opcode;
end;

destructor TSfzToXmlLoader.Destroy;
begin
  SfzParser.Free;
  inherited;
end;

procedure TSfzToXmlLoader.Load(const SfzFileName: string; XML: TNativeXML);
begin
  if not assigned(XML.Root) then XML.CreateName('root');

  xmlDoc := xml;
  ActiveNode := nil;
  LastGroup  := nil;
  LastRegion := nil;

  SfzParser.Parse(SfzFileName);
end;

procedure TSfzToXmlLoader.FileStart(Sender: TObject);
begin

end;

procedure TSfzToXmlLoader.FileEnd(Sender: TObject);
begin

end;

procedure TSfzToXmlLoader.GroupStart(Sender: TObject);
begin
  LastGroup  := xmlDoc.Root.NodeNew('Group');
  ActiveNode := LastGroup;
end;

procedure TSfzToXmlLoader.GroupEnd(Sender: TObject);
begin
  LastGroup := nil;
end;

procedure TSfzToXmlLoader.RegionStart(Sender: TObject);
begin
  if assigned(LastGroup)
    then LastRegion := LastGroup.NodeNew('Region')
    else LastRegion := xmlDoc.Root.NodeNew('Region');

  ActiveNode := LastRegion;
end;

procedure TSfzToXmlLoader.RegionEnd(Sender: TObject);
begin
  LastRegion := nil;
end;

procedure TSfzToXmlLoader.Opcode(Sender: TObject; const Opcode, Value: string);
begin
  if assigned(ActiveNode) then
  begin
    ActiveNode.NodeNew(Opcode).ValueAsString := Value;
  end;

end;



end.
