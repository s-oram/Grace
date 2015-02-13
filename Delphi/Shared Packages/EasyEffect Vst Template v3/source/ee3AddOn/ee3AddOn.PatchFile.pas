unit ee3AddOn.PatchFile;

interface

uses
  Classes,
  NativeXml, // NOTE: Requires NativeXml Version 4
  SysUtils;

type
  TCustomPatchFile = class
  private
  protected
    fPatchFileType : string;
    fCurrentPatchFileVersion : integer;

    procedure Clear; virtual; abstract; //clear any current data.

    function Exists(const Key : string):boolean; virtual; abstract;
    function ExistsAsInt(const Key : string):boolean; virtual; abstract;
    function ExistsAsBool(const Key : string):boolean; virtual; abstract;
    function ExistsAsFloat(const Key : string):boolean; virtual; abstract;

    procedure Update(const Key, Value : string); overload; virtual; abstract;
    procedure Update(const Key : string; const Value : integer); overload; virtual; abstract;
    procedure Update(const Key : string; const Value : boolean); overload; virtual; abstract;
    procedure Update(const Key : string; const Value : single); overload; virtual; abstract;

    function ValueAsString(const Key : string):string; overload; virtual; abstract;
    function ValueAsInt(const Key : string):integer; overload; virtual; abstract;
    function ValueAsBool(const Key : string):boolean; overload; virtual; abstract;
    function ValueAsFloat(const Key : string):single; overload; virtual; abstract;

    procedure SaveToFile(const FileName : string); virtual; abstract;
    procedure SaveToMemoryStream(const ms : TMemoryStream); virtual; abstract;

    procedure LoadFromFile(const FileName : string); virtual; abstract;
    procedure LoadFromMemoryStream(const ms : TMemoryStream); virtual; abstract;

    property PatchFileType           : string  read fPatchFileType;
    property CurrentPatchFileVersion : integer read fCurrentPatchFileVersion;
  public
    constructor Create(const aPatchFileType : string; const aCurrentPatchFileVersion : integer); virtual;
    destructor Destroy; override;
  end;

  TXmlPatchFile = class(TCustomPatchFile)
  protected
    Data : TNativeXML;
  protected const
    PatchInfoPath  = 'PatchInfo';
    PatchValuePath = 'PatchValue';
  public
    constructor Create(const aPatchFileType : string; const aCurrentPatchFileVersion : integer); override;
    destructor Destroy; override;

    procedure Clear; override;

    function Exists(const Key : string):boolean; override;
    function ExistsAsInt(const Key : string):boolean; override;
    function ExistsAsBool(const Key : string):boolean; override;
    function ExistsAsFloat(const Key : string):boolean; override;

    // Update() will add the key if it doesn't already exist. It will update the key value if it does exist.
    procedure Update(const Key, Value : string); overload; override;
    procedure Update(const Key : string; const Value : integer); overload; override;
    procedure Update(const Key : string; const Value : boolean); overload; override;
    procedure Update(const Key : string; const Value : single); overload; override;

    function ValueAsString(const Key : string):string; overload; override;
    function ValueAsInt(const Key : string):integer; overload; override;
    function ValueAsBool(const Key : string):boolean; overload; override;
    function ValueAsFloat(const Key : string):single; overload; override;

    procedure SaveToFile(const FileName : string); override;
    procedure LoadFromFile(const FileName : string); override;
    procedure SaveToMemoryStream(const ms : TMemoryStream); override;
    procedure LoadFromMemoryStream(const ms : TMemoryStream); override;
  end;



implementation

uses
  VamLib.Utils,
  NativeXmlEx;

{ TCustomPatchFile }

constructor TCustomPatchFile.Create(const aPatchFileType: string; const aCurrentPatchFileVersion: integer);
begin
  fPatchFileType           := aPatchFileType;
  fCurrentPatchFileVersion := aCurrentPatchFileVersion;
end;

destructor TCustomPatchFile.Destroy;
begin

  inherited;
end;




{ TXmlPatchFile }

constructor TXmlPatchFile.Create(const aPatchFileType: string; const aCurrentPatchFileVersion: integer);
begin
  inherited;
  Clear;
end;

destructor TXmlPatchFile.Destroy;
begin
  if assigned(Data) then FreeAndNil(Data);
  inherited;
end;



procedure TXmlPatchFile.Clear;
var
  NodePath : string;
begin
  if assigned(Data) then FreeAndNil(Data);
  Data := TNativeXML.CreateName('root');

  NodePath := PatchInfoPath + '/' + 'PatchFileType';
  NodeWiz(Data.Root).FindOrCreateNode(NodePath).ValueUnicode := PatchFileType;

  NodePath := PatchInfoPath + '/' + 'PatchFileVersion';
  NodeWiz(Data.Root).FindOrCreateNode(NodePath).ValueUnicode := IntToStr(CurrentPatchFileVersion);
end;

function TXmlPatchFile.Exists(const Key: string): boolean;
var
  NodePath : string;
begin
  assert(Key <> '');
  NodePath := PatchValuePath + '/' + Key;
  result := NodeWiz(Data.Root).Exists(NodePath);
end;

function TXmlPatchFile.ExistsAsBool(const Key: string): boolean;
var
  NodePath : string;
  NodeValue : string;
begin
  assert(Key <> '');
  NodePath := PatchValuePath + '/' + Key;
  if NodeWiz(Data.Root).Exists(NodePath) then
  begin
    NodeValue := NodeWiz(Data.Root).Find(NodePath).ValueUnicode;
    result := DataIO_StrIsBool(NodeValue);
  end else
  begin
    result := false;
  end;
end;

function TXmlPatchFile.ExistsAsFloat(const Key: string): boolean;
var
  NodePath : string;
  NodeValue : string;
begin
  assert(Key <> '');
  NodePath := PatchValuePath + '/' + Key;
  if NodeWiz(Data.Root).Exists(NodePath) then
  begin
    NodeValue := NodeWiz(Data.Root).Find(NodePath).ValueUnicode;
    result := DataIO_StrIsFloat(NodeValue);
  end else
  begin
    result := false;
  end;
end;

function TXmlPatchFile.ExistsAsInt(const Key: string): boolean;
var
  NodePath : string;
  NodeValue : string;
begin
  assert(Key <> '');
  NodePath := PatchValuePath + '/' + Key;
  if NodeWiz(Data.Root).Exists(NodePath) then
  begin
    NodeValue := NodeWiz(Data.Root).Find(NodePath).ValueUnicode;
    result := DataIO_StrIsInt(NodeValue);
  end else
  begin
    result := false;
  end;
end;

procedure TXmlPatchFile.Update(const Key: string; const Value: integer);
var
  NodePath : string;
begin
  assert(Key <> '');
  NodePath := PatchValuePath + '/' + Key;
  NodeWiz(Data.Root).FindOrCreateNode(NodePath).ValueUnicode := IntToStr(Value);
end;

procedure TXmlPatchFile.Update(const Key, Value: string);
var
  NodePath : string;
begin
  assert(Key <> '');
  NodePath := PatchValuePath + '/' + Key;
  NodeWiz(Data.Root).FindOrCreateNode(NodePath).ValueUnicode := Value;
end;

procedure TXmlPatchFile.Update(const Key: string; const Value: boolean);
var
  NodePath : string;
begin
  assert(Key <> '');
  NodePath := PatchValuePath + '/' + Key;
  NodeWiz(Data.Root).FindOrCreateNode(NodePath).ValueUnicode := DataIO_BoolToStr(Value);
end;

procedure TXmlPatchFile.Update(const Key: string; const Value: single);
var
  NodePath : string;
begin
  assert(Key <> '');
  NodePath := PatchValuePath + '/' + Key;
  NodeWiz(Data.Root).FindOrCreateNode(NodePath).ValueUnicode := DataIO_FloatToStr(Value);
end;

function TXmlPatchFile.ValueAsBool(const Key: string): boolean;
var
  NodePath : string;
  NodeValue : string;
begin
  assert(Key <> '');
  assert(Exists(Key));
  NodePath  := PatchValuePath + '/' + Key;
  NodeValue := NodeWiz(Data.Root).Find(NodePath).ValueUnicode;
  result := DataIO_StrToBool(NodeValue); // NOTE: This method could raise an exception.
end;

function TXmlPatchFile.ValueAsFloat(const Key: string): single;
var
  NodePath : string;
  NodeValue : string;
begin
  assert(Key <> '');
  assert(Exists(Key));
  NodePath  := PatchValuePath + '/' + Key;
  NodeValue := NodeWiz(Data.Root).Find(NodePath).ValueUnicode;
  result := DataIO_StrToFloat(NodeValue); // NOTE: This method could raise an exception.
end;

function TXmlPatchFile.ValueAsInt(const Key: string): integer;
var
  NodePath : string;
  NodeValue : string;
begin
  assert(Key <> '');
  assert(Exists(Key));
  NodePath  := PatchValuePath + '/' + Key;
  NodeValue := NodeWiz(Data.Root).Find(NodePath).ValueUnicode;
  result := DataIO_StrToInt(NodeValue); // NOTE: This method could raise an exception.
end;

function TXmlPatchFile.ValueAsString(const Key: string): string;
var
  NodePath : string;
  NodeValue : string;
begin
  assert(Key <> '');
  assert(Exists(Key));
  NodePath  := PatchValuePath + '/' + Key;
  NodeValue := NodeWiz(Data.Root).Find(NodePath).ValueUnicode;
end;

procedure TXmlPatchFile.LoadFromFile(const FileName: string);
begin
  if assigned(Data) then FreeAndNil(Data);
  Data := TNativeXML.CreateName('root');
  Data.LoadFromFile(FileName);
  // Descendents should override this method and check if the patch file is out of date and
  // update accordingly if so.
end;

procedure TXmlPatchFile.LoadFromMemoryStream(const ms: TMemoryStream);
begin
  if assigned(Data) then FreeAndNil(Data);
  Data := TNativeXML.CreateName('root');
  Data.LoadFromStream(ms);
  // Descendents should override this method and check if the patch file is out of date and
  // update accordingly if so.
end;

procedure TXmlPatchFile.SaveToFile(const FileName: string);
begin
  Data.XmlFormat := xfReadable;
  Data.SaveToFile(FileName);
end;

procedure TXmlPatchFile.SaveToMemoryStream(const ms: TMemoryStream);
begin
  Data.XmlFormat := xfCompact;
  Data.SaveToStream(ms);
end;



end.
