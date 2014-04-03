unit uIniFileService;

interface

uses

       Classes
     ;

type
  IIniFileService = interface
    ['{457DB193-8B7D-43B6-85EB-7CAF57949FEF}']
    function GetFilename: string;
    function SectionExists(const Section: string): Boolean;
    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
    function ReadFloat(const Section, Name: string; Default: Double): Double;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream);
    procedure WriteDate(const Section, Name: string; Value: TDateTime);
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime);
    procedure WriteFloat(const Section, Name: string; Value: Double);
    procedure WriteTime(const Section, Name: string; Value: TDateTime);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings); overload;
    procedure ReadSections(const Section: string; Strings: TStrings); overload;
    procedure ReadSubSections(const Section: string; Strings: TStrings; Recurse: Boolean = False);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);
    procedure UpdateFile;
    function ValueExists(const Section, Ident: string): Boolean;
    property FileName: string read GetFilename;
  end;

type
  TIniFileType = (iftFileBased, iftMemoryBased);

  procedure RegisterIniFileService(aServiceName: string; aFilename: string; aIniFileType: TIniFileType = iftFileBased);


implementation

uses
         IniFiles
       , Spring.Container
       ;

type

  TIniFileImpl = class(TInterfacedObject, IIniFileService)
  private
    FIniFile: TCustomIniFile;
  public
    constructor Create(const aFilename: string; aIniFileType: TIniFileType);
    function GetFilename: string;
    function SectionExists(const Section: string): Boolean;
    function ReadString(const Section, Ident, Default: string): string;
    procedure WriteString(const Section, Ident, Value: String);
    function ReadInteger(const Section, Ident: string; Default: Longint): Longint;
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    function ReadBinaryStream(const Section, Name: string; Value: TStream): Integer;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
    function ReadFloat(const Section, Name: string; Default: Double): Double;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
    procedure WriteBinaryStream(const Section, Name: string; Value: TStream);
    procedure WriteDate(const Section, Name: string; Value: TDateTime);
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime);
    procedure WriteFloat(const Section, Name: string; Value: Double);
    procedure WriteTime(const Section, Name: string; Value: TDateTime);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings); overload;
    procedure ReadSections(const Section: string; Strings: TStrings); overload;
    procedure ReadSubSections(const Section: string; Strings: TStrings; Recurse: Boolean = False);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);
    procedure UpdateFile;
    function ValueExists(const Section, Ident: string): Boolean;
    property FileName: string read GetFilename;
  end;

  { TIniFileImpl }

constructor TIniFileImpl.Create(const aFilename: string; aIniFileType: TIniFileType);
begin
  inherited Create;
  case aIniFiletype of
    iftFileBased: FIniFile := TIniFile.Create(aFilename);
    iftMemoryBased: FIniFile := TMemIniFile.Create(aFilename);
  end;

end;

procedure TIniFileImpl.DeleteKey(const Section, Ident: String);
begin
  FIniFile.DeleteKey(Section, Ident);
end;

procedure TIniFileImpl.EraseSection(const Section: string);
begin
    fIniFile.EraseSection(Section);
end;

function TIniFileImpl.GetFilename: string;
begin
  Result := FIniFile.FileName;
end;

function TIniFileImpl.ReadBinaryStream(const Section, Name: string; Value: TStream): Integer;
begin
  Result := FIniFile.ReadBinaryStream(Section, Name, Value);
end;

function TIniFileImpl.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result := FIniFile.ReadBool(Section, Ident, Default);
end;

function TIniFileImpl.ReadDate(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDate(Section, Name, Default);
end;

function TIniFileImpl.ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDateTime(Section, Name, Default);
end;

function TIniFileImpl.ReadFloat(const Section, Name: string; Default: Double): Double;
begin
  Result := FIniFile.ReadFloat(Section, Name, Default);
end;

function TIniFileImpl.ReadInteger(const Section, Ident: string; Default: Integer): Longint;
begin
  Result := FIniFile.ReadInteger(Section, Ident, Default);
end;

procedure TIniFileImpl.ReadSection(const Section: string; Strings: TStrings);
begin
  FIniFile.ReadSection(Section, Strings);
end;

procedure TIniFileImpl.ReadSections(const Section: string; Strings: TStrings);
begin
  FIniFile.ReadSections(Section, Strings);
end;

procedure TIniFileImpl.ReadSections(Strings: TStrings);
begin
  FIniFile.ReadSections(Strings);
end;

procedure TIniFileImpl.ReadSectionValues(const Section: string; Strings: TStrings);
begin
  FIniFile.ReadSectionValues(Section, Strings);
end;

function TIniFileImpl.ReadString(const Section, Ident, Default: string): string;
begin
  Result := FIniFile.ReadString(Section, Ident, Default);
end;

procedure TIniFileImpl.ReadSubSections(const Section: string; Strings: TStrings; Recurse: Boolean);
begin
   FIniFile.ReadSubSections(Section, Strings, Recurse);
end;

function TIniFileImpl.ReadTime(const Section, Name: string; Default: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadTime(Section, Name, Default);
end;

function TIniFileImpl.SectionExists(const Section: string): Boolean;
begin
  Result := FIniFile.SectionExists(Section);
end;

procedure TIniFileImpl.UpdateFile;
begin
  FIniFile.UpdateFile;
end;

function TIniFileImpl.ValueExists(const Section, Ident: string): Boolean;
begin
 Result := FIniFile.ValueExists(Section, Ident)
end;

procedure TIniFileImpl.WriteBinaryStream(const Section, Name: string; Value: TStream);
begin
  FIniFile.WriteBinaryStream(Section, Name, Value);
end;

procedure TIniFileImpl.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  FIniFile.WriteBool(Section, Ident, Value);
end;

procedure TIniFileImpl.WriteDate(const Section, Name: string; Value: TDateTime);
begin
  FIniFile.WriteDate(Section, Name, Value);
end;

procedure TIniFileImpl.WriteDateTime(const Section, Name: string; Value: TDateTime);
begin
  FIniFile.WriteDateTime(Section, Name, Value);
end;

procedure TIniFileImpl.WriteFloat(const Section, Name: string; Value: Double);
begin
  FIniFile.WriteFloat(Section, Name, Value);
end;

procedure TIniFileImpl.WriteInteger(const Section, Ident: string; Value: Integer);
begin
  FIniFile.WriteInteger(Section, Ident, Value);
end;

procedure TIniFileImpl.WriteString(const Section, Ident, Value: String);
begin
  FIniFile.WriteString(Section, Ident, Value);
end;

procedure TIniFileImpl.WriteTime(const Section, Name: string; Value: TDateTime);
begin
  FIniFile.WriteTime(Section, Name, Value);
end;

procedure RegisterIniFileService(aServiceName: string; aFilename: string; aIniFileType: TIniFileType = iftFileBased);
begin
  GlobalContainer.RegisterType<TIniFileImpl>.Implements<IIniFileService>(aServicename).AsSingleton.DelegateTo(
    function: TIniFileImpl
    begin
      Result := TIniFileImpl.Create(aFilename, aIniFileType);
    end
  );
  GlobalContainer.Build;
end;

initialization



end.
