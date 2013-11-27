unit Lucidity.CopyProtection;

interface

uses
  Classes;

const
  KeySalt1 = 'ARdumNeCTaGFJ8wRJBdVD2LL';
  KeySalt2 = '8qJ8gcvnJn8FDSDFUqAytF2qF2Qvg';
  KeySalt3 = 'QY8MqC2Dx5HE4E4EtAN43@#$%#%XGQ6Y';
  KeySalt4 = 'gvf4EDM####d3y9sFRRYR5aKKvxb';
  KeySalt5 = '2vjJkGaxpCkeYmRg*Zu%NxPemH';
  KeySalt6 = 'LX3UwutytnQjcRJ5hEzmqKpv';
  KeySalt7 = '6zCa@@#NVMLsWB5f4ZnXUt)(9Tb39';
  KeySalt8 = 'R4uHNNt$#$@523%#%#$bRJftur4cLXs3BFSS';

type
  TLucidityKey = record
  private
    fIsKeyChecksumValid: boolean;
  public
    UserName  : string;   //This is an encoded user name.
    UserEmail : string;   //This is an encoded user email.
    Sections  : array[0..7] of string;
    DataCheck : string;

    procedure AssignFrom(const Source : TLucidityKey);
    procedure Clear;
    function LoadFromFile(const FileName: string):boolean;

    property IsKeyChecksumValid : boolean read fIsKeyChecksumValid;
  end;


function GetUserNameFromKey(const Key:TLucidityKey):string;
function GetUserEmailFromKey(const Key:TLucidityKey):string;

function ExtractKeyFromFileContents(FileContents, KeyData : TStringList):boolean;

function IsKeyValid_ChecksumOnly(const KeyData : TStrings):boolean; overload;
function IsKeyValid_ChecksumOnly(const Key : TLucidityKey):boolean; overload;



implementation

uses
  uAutoFree,
  eeHashes,
  EncdDecd,
  SysUtils,
  PunyCode;

function GetUserNameFromKey(const Key:TLucidityKey):string;
var
  Data : string;
begin
  Data := Key.UserName;
  Data := DecodeString(Data);
  Data := TPunyCode.Decode(Data);
  result := Data;
end;

function GetUserEmailFromKey(const Key:TLucidityKey):string;
var
  Data : string;
begin
  Data := Key.UserEmail;
  Data := DecodeString(Data);
  Data := TPunyCode.Decode(Data);
  result := Data;
end;


// IsKeyValid() Checks if the key file format matches the key for lucidity
// and whether the key matches the key checksum. It doesn't check
// whether the key is will allow the product to registered.
function IsKeyValid_ChecksumOnly(const KeyData: TStrings): boolean;
var
  TestData, TestResult : string;
begin
  TestData := KeyData.Create[0];            //name
  TestData := TestData + KeyData.Create[1]; //email
  TestData := TestData + KeyData.Create[2]; //sections one to eight...
  TestData := TestData + KeyData.Create[3];
  TestData := TestData + KeyData.Create[4];
  TestData := TestData + KeyData.Create[5];
  TestData := TestData + KeyData.Create[6];
  TestData := TestData + KeyData.Create[7];
  TestData := TestData + KeyData.Create[8];
  TestData := TestData + KeyData.Create[9];

  TestResult := KeyData[10];

  if SameText(md5(TestData), TestResult)
    then result := true
    else result := false;

end;

function IsKeyValid_ChecksumOnly(const Key : TLucidityKey):boolean; overload;
var
  TestData, TestResult : string;
begin
  TestData := Key.UserName;
  TestData := TestData + Key.UserEmail;
  TestData := TestData + Key.Sections[0];
  TestData := TestData + Key.Sections[1];
  TestData := TestData + Key.Sections[2];
  TestData := TestData + Key.Sections[3];
  TestData := TestData + Key.Sections[4];
  TestData := TestData + Key.Sections[5];
  TestData := TestData + Key.Sections[6];
  TestData := TestData + Key.Sections[7];

  TestResult := Key.DataCheck;

  if SameText(md5(TestData), TestResult)
    then result := true
    else result := false;

end;


function ExtractKeyFromFileContents(FileContents, KeyData: TStringList): boolean;
var
  KeyStartIndex, KeyEndIndex : integer;
  c1: Integer;
  s : string;
  KeyLineCount : integer;
begin
  assert(assigned(FileContents));
  assert(assigned(KeyData));


  KeyStartIndex := FileContents.IndexOf('BEGINKEY>>>>');
  KeyEndIndex := FileContents.IndexOf('<<<<ENDKEY');


  if (KeyStartIndex = -1) or (KeyEndIndex = -1)
    then exit(false);


  for c1 := KeyStartIndex+1 to KeyEndIndex-1 do
  begin
    s := FileContents[c1];
    KeyData.Add(s);
  end;

  KeyLineCount := KeyEndIndex - KeyStartIndex - 1;

  if KeyLineCount <> 11
    then exit(false)
    else exit(true);

end;




{ TLucidityKey }

procedure TLucidityKey.AssignFrom(const Source: TLucidityKey);
var
  c1 : integer;
begin
  self.UserName := Source.UserName;
  self.UserEmail := Source.UserEmail;

  for c1 := 0 to 7 do
  begin
    Self.Sections[c1] := Source.Sections[c1];
  end;

  self.DataCheck := Source.DataCheck;
end;

procedure TLucidityKey.Clear;
var
  c1: Integer;
begin
  self.fIsKeyChecksumValid := false;

  self.UserName := '';
  self.UserEmail := '';

  for c1 := 0 to 7 do
  begin
    Sections[c1] := '';
  end;

  DataCheck := '';
end;

function TLucidityKey.LoadFromFile(const FileName: string):boolean;
var
  FileData : TStringList;
  KeyData  : TStringList;
begin
  FileData := TStringList.Create;
  AutoFree(@FileData);

  KeyData := TStringList.Create;
  AutoFree(@KeyData);

  Clear;

  FileData.Clear;
  FileData.LoadFromFile(FileName);

  ExtractKeyFromFileContents(FileData, KeyData);

  if IsKeyValid_ChecksumOnly(KeyData)
    then result := true
    else result := false;

  if IsKeyValid_ChecksumOnly(KeyData) then
  begin
    self.fIsKeyChecksumValid := true;

    self.UserName    := KeyData[0];
    self.UserEmail   := KeyData[1];
    self.Sections[0] := KeyData[2];
    self.Sections[1] := KeyData[3];
    self.Sections[2] := KeyData[4];
    self.Sections[3] := KeyData[5];
    self.Sections[4] := KeyData[6];
    self.Sections[5] := KeyData[7];
    self.Sections[6] := KeyData[8];
    self.Sections[7] := KeyData[9];
    self.DataCheck   := KeyData[10];
  end else
  begin
    self.fIsKeyChecksumValid := false;
  end;

end;


end.
