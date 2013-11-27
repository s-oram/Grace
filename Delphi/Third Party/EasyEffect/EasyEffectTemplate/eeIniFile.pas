unit eeIniFile;

interface

uses
  Classes, Contnrs;

type
  TValuePair = class
  public
    Ident:string;
    Value:string;
  end;

  TSection = class
  private
    fValueList:TObjectList;
    fName: string;
    function GetValueCount: integer;
    procedure SetValueCount(const Value: integer);
    function GetValue(index: integer): TValuePair;
    procedure SetValue(index: integer; const Value: TValuePair);
  public
    constructor Create;
	  destructor Destroy; override;

    function Exists(Ident:string):boolean;

    procedure AddValue(vp:TValuePair); overload;
    procedure AddValue(Ident,Value:string); overload;
    procedure DeleteValue(Index:integer);

    function ReadValue(Ident,Default:string):string; overload;
    function ReadValue(Index:integer; Default:string):string; overload;


    procedure Clear;

    property Name:string read fName write fName;
    property ValueCount:integer read GetValueCount write SetValueCount;
    property Values[index:integer]:TValuePair read GetValue write SetValue; default;
  end;


  TSectionList = class
  private
    fSectionList:TObjectList;
    function GetCount: integer;
    procedure SetCount(const Value: integer);
    function GetValue(index: integer): TSection;
    procedure SetValue(index: integer; const Value: TSection);
  public
    constructor Create;
	  destructor Destroy; override;



    procedure AddSection(aSection:TSection);
    procedure DeleteSection(SectionID:string); overload;
    procedure DeleteSection(Index:integer); overload;

    procedure Clear;

    property Count:integer read GetCount write SetCount;
    property Sections[index:integer]:TSection read GetValue write SetValue; default;
  end;

  TIniFile = class
  private
    fSections: TSectionList;
  protected
    procedure FlushSection(Text:TStrings; aSection:TSection);
    procedure SaveToStringList(Text:TStringList);
    procedure LoadFromStringList(Text:TStringList);
    function GetSection(SectionID:string):TSection;
    property Sections:TSectionList read fSections write fSections;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Clear;

    function Exists(SectionID:string):boolean; overload;
    function Exists(SectionID, Ident:string):boolean; overload;

    function ValueCount(SectionID:string):integer;

    function GetValuePair(SectionID:string; Index:integer):TValuePair;

    procedure LoadFromFile(aFileName:string);
    procedure LoadFromStream(Stream:TStream);
    procedure SaveToFile(aFileName:string);
    procedure SaveToStream(Stream:TStream);

    function ReadString(const SectionID:string; const Index:integer; const Default: string = ''): string; overload;
    function ReadString(const SectionID, Ident:string; Default: string = ''): string; overload;
    procedure WriteString(const SectionID, Ident, Value: String);

    function ReadInteger(const SectionID:string; Index:integer; Default:integer): integer; overload;
    function ReadInteger(const SectionID, Ident:string; Default:integer): integer; overload;
    procedure WriteInteger(const SectionID, Ident:string; Value:integer);

    function ReadFloat(const SectionID, Ident:string; DefaultValue:single = 0):single; //NOTE: Precision isn't good.
    procedure WriteFloat(const SectionID, Ident:string; Value:single);                 //NOTE: Precision isn't good.

    procedure ReadSectionValues(const SectionID: string; Strings: TStrings);
    procedure DeleteSection(const aSectionID: string);
    procedure DeleteKey(const Section, Ident: String);
  end;


implementation

uses
  SysUtils;

{ TSection }

constructor TSection.Create;
begin
  fValueList := TObjectList.Create;
end;

destructor TSection.Destroy;
begin
  fValueList.Free;
  inherited;
end;

function TSection.Exists(Ident: string): boolean;
var
  c1:integer;
begin
  result := false;

  for c1 := 0 to ValueCount - 1 do
  begin
    if Values[c1].Ident = Ident then
    begin
      result := true;
      exit;  //===========================================================>
    end;
  end;

end;


procedure TSection.Clear;
begin
  fValueList.Clear;
end;

procedure TSection.AddValue(Ident, Value: string);
var
  c1:integer;
  vp:TValuePair;
begin
  vp := nil;

  //Find the name value pair if it exists, if not create it and add it to the end.
  if Ident <> '' then
  begin
    for c1 := 0 to ValueCount - 1 do
    begin
      if Values[c1].Ident = Ident then
      begin
        vp := Values[c1];
        Continue; {correct value pair found so skip out of loop}
      end;
    end;

    if vp = nil then
    begin
      vp := TValuePair.Create;
      vp.Ident := Ident;
      AddValue(vp);
    end;

    vp.Value := Value;
  end;


  // if no identifier just add it to the end.
  if Ident = '' then
  begin
    vp := TValuePair.Create;
    vp.Ident := Ident;
    AddValue(vp);
    vp.Value := Value;
  end;


end;





function TSection.GetValueCount: integer;
begin
  result := fValueList.Count;
end;

function TSection.ReadValue(Ident,Default: string): string;
var
  c1:integer;
begin
  result := Default;

  for c1 := 0 to ValueCount - 1 do
  begin
    if Values[c1].Ident = Ident then
    begin
      result := Values[c1].Value;
      exit;  //===========================================================>
    end;
  end;

end;

function TSection.ReadValue(Index: integer; Default: string): string;
begin
  assert(Index >= 0);
  
  if (Index >= ValueCount)
    then result := Default
    else result := Values[Index].Value;
  
end;



function TSection.GetValue(index: integer): TValuePair;
begin
  result := fValueList[Index] as TValuePair;
end;

procedure TSection.SetValueCount(const Value: integer);
begin
  fValueList.Count := value;
end;

procedure TSection.SetValue(index: integer; const Value: TValuePair);
begin
  fValueList[Index] := Value;
end;

procedure TSection.AddValue(vp: TValuePair);
begin
  fValueList.Add(vp);
end;

procedure TSection.DeleteValue(Index: integer);
begin
  fValueList.Delete(Index);
end;



{TSectionList}

constructor TSectionList.Create;
begin
  fSectionList := TObjectList.Create;
end;

procedure TSectionList.DeleteSection(SectionID: string);
var
  c1:integer;
begin
  for c1 := Count-1 DownTo 0 do
  begin
    if Sections[c1].Name = SectionID
      then DeleteSection(c1);
  end;
    
end;

destructor TSectionList.Destroy;
begin
  fSectionList.Free;
  inherited;
end;

procedure TSectionList.Clear;
begin
  fSectionList.Clear;
end;

function TSectionList.GetCount: integer;
begin
  result := fSectionList.Count;
end;

function TSectionList.GetValue(index: integer): TSection;
begin
  result := fSectionList[Index] as TSection;
end;

procedure TSectionList.SetCount(const Value: integer);
begin
  fSectionList.Count := value;
end;

procedure TSectionList.SetValue(index: integer; const Value: TSection);
begin
  fSectionList[Index] := Value;
end;

procedure TSectionList.AddSection(aSection: TSection);
begin
  fSectionList.Add(aSection);
end;

procedure TSectionList.DeleteSection(Index: integer);
begin
  fSectionList.Delete(Index);
end;




{ TsoIniFile }

procedure TIniFile.Clear;
begin
  Sections.Clear;
end;

constructor TIniFile.Create;
begin
  Sections := TSectionList.Create;
end;

destructor TIniFile.Destroy;
begin
  Sections.Free;
  inherited;
end;

procedure TIniFile.DeleteKey(const Section, Ident: String);
begin
  assert(false, 'TODO');
end;

procedure TIniFile.DeleteSection(const aSectionID: string);
begin
  Sections.DeleteSection(aSectionID);
end;


function TIniFile.Exists(SectionID: string): boolean;
var
  aSection:TSection;
begin
  aSection := GetSection(SectionID);

  if aSection = nil
    then result := false
    else result := true;

end;

function TIniFile.Exists(SectionID, Ident: string): boolean;
var
  aSection:TSection;
begin
  aSection := GetSection(SectionID);

  if aSection = nil then
  begin
    result := false;
    exit; //=======================================>
  end;

  result := aSection.Exists(Ident);

end;

function TIniFile.GetSection(SectionID: string): TSection;
var
  c1:integer;
begin
  result := nil;

  for c1 := 0 to Sections.Count - 1 do
  begin
    if Sections[c1].Name = SectionID then
    begin
      result := Sections[c1];
      exit; //====================================>
    end;
  end;


end;

function TIniFile.GetValuePair(SectionID: string; Index: integer): TValuePair;
var
  aSection:TSection;

begin
  aSection := GetSection(SectionID);

  if (aSection = nil) then
  begin
    result := nil;
  end else
  begin
    if Index >= aSection.ValueCount
      then result := nil
      else result := aSection.GetValue(Index);
      
  end;

end;

function TIniFile.ReadInteger(const SectionID, Ident: string;  Default: integer): integer;
var
  r:string;
  ds:string;
begin
  ds := IntToStr(Default);

  r := ReadString(SectionID,Ident,ds);

  result := StrToInt(r);
end;

function TIniFile.ReadInteger(const SectionID: string; Index, Default: integer): integer;
var
  r:string;
  ds:string;
begin
  ds := IntToStr(Default);

  r := ReadString(SectionID,Index,ds);

  result := StrToInt(r);
end;

procedure TIniFile.ReadSectionValues(const SectionID: string; Strings: TStrings);
begin

end;

function TIniFile.ReadString(const SectionID: string; const Index: integer; const Default: string = ''): string;
var
  aSection:TSection;
begin
  assert(SectionID <> '');

  aSection := GetSection(SectionID);

  if aSection = nil
    then result := Default
    else result := aSection.ReadValue(Index, Default);


end;

function TIniFile.ReadString(const SectionID, Ident:string; Default: string = ''): string;
var
  aSection:TSection;
begin
  assert(SectionID <> '');

  aSection := GetSection(SectionID);

  if aSection = nil
    then result := Default
    else result := aSection.ReadValue(Ident, Default);
end;


function TIniFile.ReadFloat(const SectionID, Ident: string; DefaultValue: single): single;
var
  IntValue:integer;
begin
  IntValue := round(DefaultValue * 100);

  result := ReadInteger(SectionId, Ident, IntValue);
  result := result * 0.01;
end;


procedure TIniFile.WriteFloat(const SectionID, Ident: string; Value: single);
var
  IntValue:integer;
begin
  IntValue := round(Value * 100);

  WriteInteger(SectionID, Ident, IntValue);
end;

procedure TIniFile.WriteInteger(const SectionID, Ident: string; Value: integer);
var
  ValueString:String;
begin
  ValueString := IntToStr(Value);

  WriteString(SectionID, Ident, ValueString);
end;

procedure TIniFile.WriteString(const SectionID, Ident, Value: String);
var
  aSection:TSection;
begin
  assert(SectionID <> '');

  //Find the section, function returns nil if the section doesn't exist.
  aSection := GetSection(SectionID);

  //If the section doesn't exist, create it.
  if aSection = nil then
  begin
    aSection := TSection.Create;
    aSection.Name := SectionID;
    Sections.AddSection(aSection);
  end;

  aSection.AddValue(Ident,Value);

end;

procedure TIniFile.LoadFromFile(aFileName: string);
var
  Text:TStringList;
begin
  assert(FileExists(aFileName));

  Clear;

  Text := TStringList.Create;
  Text.LoadFromFile(aFileName);

  LoadFromStringList(Text);

  Text.Free;
end;

procedure TIniFile.LoadFromStream(Stream: TStream);
var
  Text:TStringList;
begin
  Clear;

  Text := TStringList.Create;
  Text.LoadFromStream(Stream);

  LoadFromStringList(Text);

  Text.Free;
end;


procedure TIniFile.LoadFromStringList(Text: TStringList);
var
  s:string;
  c1,Len:integer;
  aSectionName,aIdent,aValue:string;
  Index:integer;
begin
  //remove all comments
  for c1 := Text.Count - 1 downto 0 do
  begin
    s := Text[c1];
    if (Length(s) = 0) or (s[1] = '#') then Text.Delete(c1);
  end;

  //remove all empty lines.
  for c1 := Text.Count - 1 downto 0 do
  begin
    s := Text[c1];
    if (Length(s) = 0) then Text.Delete(c1);
  end;

  //remove white space and control charactors are start/end of lines
  for c1 := 0 to Text.Count - 1 do
  begin
    s := Text[c1];
    s := Trim(Text[c1]);
    Text[c1] := s;
  end;


  //Parse remaining lines.
  aSectionName := '';
  aIdent := '';
  aValue := '';
  for c1 := 0 to Text.Count - 1 do
  begin
    s := Text[c1];
    Len := Length(s);
    if (s[1] = '[') and (s[Len] = ']') then
    begin
      //Get the new section name.
      aSectionName := Copy(s,2,Len-2);
    end else
    begin
      //get the new value.
      Index := Pos('=',s);
      if (Index >= 2)
        then aIdent := Copy(s,1,Index-1)
        else aIdent := '';

      aValue := Copy(s,Index+1,Len-Index);

      //Write the value, but only if it follows a section ID.
      if aSectionName <> '' then WriteString(aSectionName, aIdent, aValue);

    end;
  end;
end;

procedure TIniFile.SaveToFile(aFileName: string);
var
  Text:TStringList;
begin
  Text := TStringList.Create;
  try
  //NOTE: This line was commented out because I haven't really implemented it properly. :(
  //if FileExists(aFileName) then Text.LoadFromFile(aFileName);
  SaveToStringList(Text);
  Text.SaveToFile(aFileName);
  finally
    Text.Free;
  end;

end;


procedure TIniFile.SaveToStream(Stream: TStream);
var
  Text:TStringList;
begin
  Text := TStringList.Create;
  try
    SaveToStringList(Text);
    Text.SaveToStream(Stream);
  finally
    Text.Free;
  end;

end;

procedure TIniFile.SaveToStringList(Text: TStringList);
var
  c1: Integer;
  aSection:TSection;
begin
  //remove white space and control charactors are start/end of lines
  for c1 := 0 to Text.Count - 1 do
  begin
    Text[c1] := Trim(Text[c1]);;
  end;

  //TODO: need to scan text and delete any sections which don't exist.

  //Flush all sections to file.
  for c1 := 0 to Sections.Count - 1 do
  begin
    aSection := Sections[c1];
    assert(aSection <> nil);
    FlushSection(Text, aSection);
  end;
end;

function TIniFile.ValueCount(SectionID: string): integer;
var
  aSection:TSection;
begin
  aSection := GetSection(SectionID);

  if aSection = nil
    then result := 0
    else result := aSection.ValueCount;

end;

procedure TIniFile.FlushSection(Text: TStrings; aSection: TSection);
var
  s:string;
  Line:integer;
  c1: Integer;
  DeleteLine:boolean;
begin

  s := '[' + aSection.Name + ']';
  Line := Text.IndexOf(s);

  //----------------------------------------------------------
  //  If the section doesn't exist in the text.
  //----------------------------------------------------------
  if Line = -1 then
  begin
    //Section doesn't exist, so just append the section to the end of the file.
    Text.Add(s);

    //add section values.
    for c1 := 0 to aSection.ValueCount - 1 do
    begin
      if aSection.Values[c1].Ident <> ''
        then s := aSection.Values[c1].Ident + '=' + aSection.Values[c1].Value
        else s := aSection.Values[c1].Value;
      Text.Add(s);
    end;

    //finally add a empty line.
    Text.Add('');
  end;


  //----------------------------------------------------------
  //  If the sections already exists in the text.
  //----------------------------------------------------------

  if Line >= 0 then
  begin
    //First delete all section values.
    inc(Line);

    DeleteLine := true;
    while (DeleteLine) and (Line < Text.Count) do
    begin
      if Text[Line] = '' then DeleteLine := false;

      if Length(Text[Line]) > 0 then
      begin
        if (Text[Line][1] = ' ') then DeleteLine := false;
        if (Text[Line][1] = '[') then DeleteLine := false;
        if (Text[Line][1] = '#') then DeleteLine := false;
      end;

      if DeleteLine then Text.Delete(Line);
    end;


    //add section values.
    for c1 := 0 to aSection.ValueCount - 1 do
    begin
      if aSection.Values[c1].Ident <> ''
        then s := aSection.Values[c1].Ident + '=' + aSection.Values[c1].Value
        else s := aSection.Values[c1].Value;
      Text.Insert(Line, s);
      inc(Line);
    end;

  end;



end;









end.
