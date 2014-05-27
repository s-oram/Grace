unit Lucidity.CopyProtection;

interface

uses
  Classes;

const
  KeySalt1 = 'fQK8g4t97GLTWwp_IPjymy6MVKpd3H5G';
  KeySalt2 = 'f9SnzEcqi4sYuE9GLTWwpIUp5Z45kP5S6LjDQu';
  KeySalt3 = 'yGOThXI6D5L0uE9duATLydZBF3VnLLX1T0H';
  KeySalt4 = 'FW1kxT3oTGqdjcXRNzBWO4OATLydZCFPbNkwjD';
  KeySalt5 = 'wXG7rNtGcWNC1OZUtuV_jEnkrrHWqGXD';
  KeySalt6 = 'ZaKV_jJizx0OZEVbZHw56lurHWqsvr58KhYA';
  KeySalt7 = 'l6gpzsZI04EUvaxFpMVosSs_R8nctHz9';
  KeySalt8 = '5lk88c9k3DK9GnTTcOgLBr79t8nk9HebAd';
  KeySalt9 = 'vQ4BRPiQrrtc2LKnyYh7UrQji5bMoCA_';
  KeySalt10 = '8ZfAxhGFPFsF3U3yYh7UrQji5bzw9Hd6C0Cn0SkOOd1';
  KeySalt11 = 'AF1qmVA9S5Wh0SYc49h69huT_IRXURSW';
  KeySalt12 = 'cYVvu6lJvT_Iq3tuNmy9Z3oxT2T_IG6eq3WgeL';
  KeySalt13 = 'IYZkTza3xsXvSbyzBNI74eP2fsQwZOCM';
  KeySalt14 = 'S2yDItKivoFBuS_sszCAIK0zBNI7Fvc837oxE';
  KeySalt15 = '0PYG110ij9JES1Q7PsPkT0duGFHACzSf';
  KeySalt16 = 'QbyHpxLXBXnlbSl1hxPkT0d1HV62Jfbmw7na6';
  KeySalt17 = 'kt1OD7k2AxTFcnrqAO_v4fOt5mKZvxtP';
  KeySalt18 = '5WDIxdIugdB6z4epr4juOOzWvGSnt5mKZe1_n';
  KeySalt19 = 'hSQdZRrcXrJ2ZL5EJd1ZF3Q3UEkUw7v5';
  KeySalt20 = 'THySTeddq2SoZL5EJd1ZF3Q3UU8oulNG1jCMOqQIrvHBt';
  KeySalt21 = 'sIu4nt2dq54zh0mjJxlGMRMBZL5EJd1ZF3Q3U7Fqed6n';
  KeySalt22 = 'fqBNNVa7DernVuhlUwdDlWRTVnja77ejrva0';
  KeySalt23 = 'MNvJRJOjMuhlU8R_A0pJ_9k6c3d59Acl';
  KeySalt24 = 'ZOZX4DJV8JmnnObzSuhlUuhl6c3hlUoIicH9LfZwC';
  KeySalt25 = 'Wd0U6c3dLpZUVUvE8NupniaEzuH6c3J1IFpuLm';



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
    fIsKeyChecksumValid: boolean;
  public
    UserName  : string;   //This is an encoded user name.
    UserEmail : string;   //This is an encoded user email.
    Sections  : array[0..24] of string;
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


procedure SaveKeyToFile(const FileName : string; const Key:TLucidityKey);
function IsKeyValid_FullCheck(const Key : TLucidityKey):boolean;



function CreateLucidityKey(UserName, UserEmail : string):TLucidityKey;

implementation

uses
  VamLib.Utils,
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


procedure SaveKeyToFile(const FileName : string; const Key:TLucidityKey);
var
  FileData : TStringList;
  UserName, UserEmail : string;
begin
  FileData := TStringList.Create;
  AutoFree(@FileData);

  UserName := GetUserNameFromKey(Key);
  UserEmail := GetUserEmailFromKey(Key);

  FileData.Add('License Key File for Lucidity 1.0');
  FileData.Add(TPunyCode.Encode(UserName));
  FileData.Add(TPunyCode.Encode(UserEmail));
  FileData.Add('');

  FileData.Add('BEGINKEY>>>>');
  FileData.Add(Key.UserName);
  FileData.Add(Key.UserEmail);
  FileData.Add(Key.Sections[0]);
  FileData.Add(Key.Sections[1]);
  FileData.Add(Key.Sections[2]);
  FileData.Add(Key.Sections[3]);
  FileData.Add(Key.Sections[4]);
  FileData.Add(Key.Sections[5]);
  FileData.Add(Key.Sections[6]);
  FileData.Add(Key.Sections[7]);
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
  UserName  := TPunyCode.Encode(UserName);
  UserEmail := TPunyCode.Encode(UserEmail);

  UserName  := EncodeString(UserName);
  UserEmail := EncodeString(UserEmail);

  Key.UserName  := UserName;
  Key.UserEmail := UserEmail;

  Key.Sections[0]  := md5(UserName + UserEmail + KeySalt1);
  Key.Sections[1]  := md5(UserName + KeySalt2);
  Key.Sections[2]  := md5(UserName + UserName + KeySalt3);
  Key.Sections[3]  := md5(KeySalt4 + UserName + UserEmail + KeySalt4);
  Key.Sections[4]  := md5(UserEmail + KeySalt5 + UserEmail);
  Key.Sections[5]  := md5(KeySalt6 + UserEmail + UserName + KeySalt6);
  Key.Sections[6]  := md5(KeySalt7 + UserEmail + KeySalt7 + UserEmail + KeySalt7);
  Key.Sections[7]  := md5(KeySalt8 + UserName + UserEmail + KeySalt8);
  Key.Sections[8]  := md5(UserName + UserEmail + KeySalt1);
  Key.Sections[9]  := md5(UserName + KeySalt2);
  Key.Sections[10] := md5(UserName + UserName + KeySalt3);
  Key.Sections[11] := md5(KeySalt4 + UserName + UserEmail + KeySalt4);
  Key.Sections[12] := md5(UserEmail + KeySalt5 + UserEmail);
  Key.Sections[13] := md5(KeySalt6 + UserEmail + UserName + KeySalt6);
  Key.Sections[14] := md5(KeySalt7 + UserEmail + KeySalt7 + UserEmail + KeySalt7);
  Key.Sections[15] := md5(KeySalt8 + UserName + UserEmail + KeySalt8);
  Key.Sections[16] := md5(UserName + UserEmail + KeySalt1);
  Key.Sections[17] := md5(UserName + KeySalt2);
  Key.Sections[18] := md5(UserName + UserName + KeySalt3);
  Key.Sections[19] := md5(KeySalt4 + UserName + UserEmail + KeySalt4);
  Key.Sections[20] := md5(UserEmail + KeySalt5 + UserEmail);
  Key.Sections[21] := md5(KeySalt6 + UserEmail + UserName + KeySalt6);
  Key.Sections[22] := md5(KeySalt7 + UserEmail + KeySalt7 + UserEmail + KeySalt7);
  Key.Sections[23] := md5(KeySalt8 + UserName + UserEmail + KeySalt8);
  Key.Sections[24] := md5(KeySalt8 + UserName + UserEmail + KeySalt8);

  DataCheck := UserName + UserEmail;
  for c1 := 0 to 7 do
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
begin
  if IsKeyValid_ChecksumOnly(Key) = false
    then exit(false);

  UserName  := Key.UserName;
  UserEmail := Key.UserEmail;

  TestResult := Key.Sections[0];
  TestData   := md5(UserName + UserEmail + KeySalt1);
  if SameText(TestResult, TestData) = false then exit(false);

  TestResult := Key.Sections[1];
  TestData   := md5(UserName + KeySalt2);
  if SameText(TestResult, TestData) = false then exit(false);

  TestResult := Key.Sections[2];
  TestData   := md5(UserName + UserName + KeySalt3);
  if SameText(TestResult, TestData) = false then exit(false);

  TestResult := Key.Sections[3];
  TestData   := md5(KeySalt4 + UserName + UserEmail + KeySalt4);
  if SameText(TestResult, TestData) = false then exit(false);

  TestResult := Key.Sections[4];
  TestData   := md5(UserEmail + KeySalt5 + UserEmail);
  if SameText(TestResult, TestData) = false then exit(false);

  TestResult := Key.Sections[5];
  TestData   := md5(KeySalt6 + UserEmail + UserName + KeySalt6);
  if SameText(TestResult, TestData) = false then exit(false);

  TestResult := Key.Sections[6];
  TestData   := md5(KeySalt7 + UserEmail + KeySalt7 + UserEmail + KeySalt7);
  if SameText(TestResult, TestData) = false then exit(false);

  TestResult := Key.Sections[7];
  TestData   := md5(KeySalt8 + UserName + UserEmail + KeySalt8);
  if SameText(TestResult, TestData) = false then exit(false);


  // if we make it this far, the key is valid.
  result := true;
end;


end.
