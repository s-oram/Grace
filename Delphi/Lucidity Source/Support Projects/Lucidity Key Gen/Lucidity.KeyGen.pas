unit Lucidity.KeyGen;

interface

uses
  Lucidity.CopyProtection;

function CreateLucidityKey(UserName, UserEmail : string):TLucidityKey;
procedure SaveKeyToFile(const FileName : string; const Key:TLucidityKey);

function IsKeyValid_FullCheck(const Key : TLucidityKey):boolean;

implementation

uses
  SysUtils,
  Classes,
  uAutoFree,
  eeHashes,
  EncdDecd,
  Punycode;

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

  Key.UserName := UserName;
  Key.UserEmail := UserEmail;

  Key.Sections[0] := md5(UserName + UserEmail + KeySalt1);
  Key.Sections[1] := md5(UserName + KeySalt2);
  Key.Sections[2] := md5(UserName + UserName + KeySalt3);
  Key.Sections[3] := md5(KeySalt4 + UserName + UserEmail + KeySalt4);
  Key.Sections[4] := md5(UserEmail + KeySalt5 + UserEmail);
  Key.Sections[5] := md5(KeySalt6 + UserEmail + UserName + KeySalt6);
  Key.Sections[6] := md5(KeySalt7 + UserEmail + KeySalt7 + UserEmail + KeySalt7);
  Key.Sections[7] := md5(KeySalt8 + UserName + UserEmail + KeySalt8);

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
