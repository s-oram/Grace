unit Lucidity.KeyGenMaster;

interface

uses
  Classes;

const
  KeySaltCount = 25;
  KeySalts : array [0..KeySaltCount-1] of string = (
  'fQK8g4t97GLTWwp_IPjymy6MVKpd3H5G',
  'f9SnzEcqi4sYuE9GLTWwpIUp5Z45kP5S6LjDQu',
  'yGOThXI6D5L0uE9duATLydZBF3VnLLX1T0H',
  'FW1kxT3oTGqdjcXRNzBWO4OATLydZCFPbNkwjD',
  'wXG7rNtGcWNC1OZUtuV_jEnkrrHWqGXD',
  'ZaKV_jJizx0OZEVbZHw56lurHWqsvr58KhYA',
  'l6gpzsZI04EUvaxFpMVosSs_R8nctHz9',
  '5lk88c9k3DK9GnTTcOgLBr79t8nk9HebAd',
  'vQ4BRPiQrrtc2LKnyYh7UrQji5bMoCA_',
  '8ZfAxhGFPFsF3U3yYh7UrQji5bzw9Hd6C0Cn0SkOOd1',
  'AF1qmVA9S5Wh0SYc49h69huT_IRXURSW',
  'cYVvu6lJvT_Iq3tuNmy9Z3oxT2T_IG6eq3WgeL',
  'IYZkTza3xsXvSbyzBNI74eP2fsQwZOCM',
  'S2yDItKivoFBuS_sszCAIK0zBNI7Fvc837oxE',
  '0PYG110ij9JES1Q7PsPkT0duGFHACzSf',
  'QbyHpxLXBXnlbSl1hxPkT0d1HV62Jfbmw7na6',
  'kt1OD7k2AxTFcnrqAO_v4fOt5mKZvxtP',
  '5WDIxdIugdB6z4epr4juOOzWvGSnt5mKZe1_n',
  'hSQdZRrcXrJ2ZL5EJd1ZF3Q3UEkUw7v5',
  'THySTeddq2SoZL5EJd1ZF3Q3UU8oulNG1jCMOqQIrvHBt',
  'sIu4nt2dq54zh0mjJxlGMRMBZL5EJd1ZF3Q3U7Fqed6n',
  'fqBNNVa7DernVuhlUwdDlWRTVnja77ejrva0',
  'MNvJRJOjMuhlU8R_A0pJ_9k6c3d59Acl',
  'ZOZX4DJV8JmnnObzSuhlUuhl6c3hlUoIicH9LfZwC',
  'Wd0U6c3dLpZUVUvE8NupniaEzuH6c3J1IFpuLm'
  );


type
  TLucidityKey = record
  private
  public
    UserName  : string;
    UserEmail : string;
    Sections  : array[0..KeySaltCount-1] of string;
    DataCheck : string; //Checksum
    procedure AssignFrom(const Source : TLucidityKey);
    procedure Clear;
    function LoadFromFile(const FileName: string):boolean;
  end;

function ExtractKeyFromFileContents(FileContents, KeyData : TStringList):boolean;

function IsKeyValid_ChecksumOnly(const Key : TLucidityKey):boolean;

procedure SaveKeyToFile(const FileName : string; const Key:TLucidityKey);
function IsKeyValid_FullCheck(const Key : TLucidityKey):boolean;

function CreateLucidityKey(UserName, UserEmail : string):TLucidityKey;

function GetKeyCheckRawString(const KeyCheckIndex : integer; const UserName, UserEmail : string):string;

implementation

uses
  VamLib.Utils,
  eeHashes,
  EncdDecd,
  SysUtils,
  PunyCode;

function GetKeyCheckRawString(const KeyCheckIndex : integer; const UserName, UserEmail : string):string;
begin
  result := KeySalts[KeyCheckIndex] + UserName + KeySalts[KeyCheckIndex] + UserEmail + KeySalts[KeyCheckIndex];
end;

function IsKeyValid_ChecksumOnly(const Key : TLucidityKey):boolean; overload;
var
  c1 : integer;
  TestData, TestResult : string;
begin
  TestData := Key.UserName + Key.UserEmail;

  for c1 := 0 to KeySaltCount-1 do
  begin
    TestData := TestData + Key.Sections[c1];
  end;

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

  if KeyLineCount = 28
    then result := true
    else result := false;
end;

{ TLucidityKey }

procedure TLucidityKey.AssignFrom(const Source: TLucidityKey);
var
  c1 : integer;
begin
  self.UserName := Source.UserName;
  self.UserEmail := Source.UserEmail;

  for c1 := 0 to KeySaltCount-1 do
  begin
    Self.Sections[c1] := Source.Sections[c1];
  end;

  self.DataCheck := Source.DataCheck;
end;

procedure TLucidityKey.Clear;
var
  c1: Integer;
begin
  self.UserName := '';
  self.UserEmail := '';

  for c1 := 0 to KeySaltCount-1 do
  begin
    Sections[c1] := '';
  end;

  DataCheck := '';
end;

function TLucidityKey.LoadFromFile(const FileName: string):boolean;
var
  FileData : TStringList;
  KeyData  : TStringList;
  c1: Integer;
begin
  FileData := TStringList.Create;
  AutoFree(@FileData);

  KeyData := TStringList.Create;
  AutoFree(@KeyData);

  Clear;

  FileData.Clear;
  FileData.LoadFromFile(FileName);

  if ExtractKeyFromFileContents(FileData, KeyData) = false
    then exit(false);


  self.UserName := KeyData[0];
  self.UserName := KeyData[1];
  for c1 := 0 to KeySaltCount-1 do
  begin
    Self.Sections[c1] := KeyData[2 + c1];
  end;
  self.DataCheck := KeyData[27];

  if IsKeyValid_ChecksumOnly(self)
    then result := true
    else result := false;
end;


procedure SaveKeyToFile(const FileName : string; const Key:TLucidityKey);
var
  FileData : TStringList;
  UserName, UserEmail : string;
  c1: Integer;
begin
  FileData := TStringList.Create;
  AutoFree(@FileData);

  FileData.Add('License Key File for Lucidity 1.0');
  FileData.Add('BEGINKEY>>>>');
  FileData.Add(Key.UserName);
  FileData.Add(Key.UserEmail);
  for c1 := 0 to KeySaltCount-1 do
  begin
    FileData.Add(Key.Sections[c1])
  end;
  FileData.Add(Key.DataCheck);
  FileData.Add('<<<<ENDKEY');

  FileData.SaveToFile(FileName);
end;

function CreateLucidityKey(UserName, UserEmail : string):TLucidityKey;
var
  c1 : integer;
  Key : TLucidityKey;
  DataCheck : string;
begin
  Key.UserName  := UserName;
  Key.UserEmail := UserEmail;

  for c1 := 0 to KeySaltCount-1 do
  begin
    Key.Sections[c1] := md5(GetKeyCheckRawString(c1, UserName, UserEmail));
  end;

  DataCheck := UserName + UserEmail;
  for c1 := 0 to KeySaltCount-1 do
  begin
    DataCheck := DataCheck + Key.Sections[c1];
  end;

  Key.DataCheck := md5(DataCheck);

  result.AssignFrom(Key);
end;

function IsKeyValid_FullCheck(const Key : TLucidityKey):boolean;
var
  UserName, UserEmail : string;
  TestData, TestResult : string;
  c1: Integer;
  DataCheck : string;
begin
  if IsKeyValid_ChecksumOnly(Key) = false
    then exit(false);

  UserName  := Key.UserName;
  UserEmail := Key.UserEmail;

  for c1 := 0 to KeySaltCount-1 do
  begin
    TestResult := Key.Sections[c1];
    TestData   := md5(GetKeyCheckRawString(c1, UserName, UserEmail));
    if SameText(TestResult, TestData) = false then exit(false);
  end;


  //finally check the checksum
  DataCheck := UserName + UserEmail;
  for c1 := 0 to KeySaltCount-1 do
  begin
    DataCheck := DataCheck + Key.Sections[c1];
  end;

  TestResult := Key.DataCheck;
  TestData   := md5(DataCheck);
  if SameText(TestResult, TestData) = false then exit(false);


  // if we make it this far, the key is valid.
  result := true;
end;


end.
