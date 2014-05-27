unit Lucidity.CopyProtection;

interface

{$INCLUDE Defines.inc}

uses
  Classes;

const
  KeySaltCount = 25;

  {$IFDEF KeySalt1}  KeySalt1 = 'fQK8g4t97GLTWwp_IPjymy6MVKpd3H5G';                {$ENDIF}
  {$IFDEF KeySalt2}  KeySalt2 = 'f9SnzEcqi4sYuE9GLTWwpIUp5Z45kP5S6LjDQu';          {$ENDIF}
  {$IFDEF KeySalt3}  KeySalt3 = 'yGOThXI6D5L0uE9duATLydZBF3VnLLX1T0H';             {$ENDIF}
  {$IFDEF KeySalt4}  KeySalt4 = 'FW1kxT3oTGqdjcXRNzBWO4OATLydZCFPbNkwjD';          {$ENDIF}
  {$IFDEF KeySalt5}  KeySalt5 = 'wXG7rNtGcWNC1OZUtuV_jEnkrrHWqGXD';                {$ENDIF}
  {$IFDEF KeySalt6}  KeySalt6 = 'ZaKV_jJizx0OZEVbZHw56lurHWqsvr58KhYA';            {$ENDIF}
  {$IFDEF KeySalt7}  KeySalt7 = 'l6gpzsZI04EUvaxFpMVosSs_R8nctHz9';                {$ENDIF}
  {$IFDEF KeySalt8}  KeySalt8 = '5lk88c9k3DK9GnTTcOgLBr79t8nk9HebAd';              {$ENDIF}
  {$IFDEF KeySalt9}  KeySalt9 = 'vQ4BRPiQrrtc2LKnyYh7UrQji5bMoCA_';                {$ENDIF}
  {$IFDEF KeySalt10} KeySalt10 = '8ZfAxhGFPFsF3U3yYh7UrQji5bzw9Hd6C0Cn0SkOOd1';    {$ENDIF}
  {$IFDEF KeySalt11} KeySalt11 = 'AF1qmVA9S5Wh0SYc49h69huT_IRXURSW';               {$ENDIF}
  {$IFDEF KeySalt12} KeySalt12 = 'cYVvu6lJvT_Iq3tuNmy9Z3oxT2T_IG6eq3WgeL';         {$ENDIF}
  {$IFDEF KeySalt13} KeySalt13 = 'IYZkTza3xsXvSbyzBNI74eP2fsQwZOCM';               {$ENDIF}
  {$IFDEF KeySalt14} KeySalt14 = 'S2yDItKivoFBuS_sszCAIK0zBNI7Fvc837oxE';          {$ENDIF}
  {$IFDEF KeySalt15} KeySalt15 = '0PYG110ij9JES1Q7PsPkT0duGFHACzSf';               {$ENDIF}
  {$IFDEF KeySalt16} KeySalt16 = 'QbyHpxLXBXnlbSl1hxPkT0d1HV62Jfbmw7na6';          {$ENDIF}
  {$IFDEF KeySalt17} KeySalt17 = 'kt1OD7k2AxTFcnrqAO_v4fOt5mKZvxtP';               {$ENDIF}
  {$IFDEF KeySalt18} KeySalt18 = '5WDIxdIugdB6z4epr4juOOzWvGSnt5mKZe1_n';          {$ENDIF}
  {$IFDEF KeySalt19} KeySalt19 = 'hSQdZRrcXrJ2ZL5EJd1ZF3Q3UEkUw7v5';               {$ENDIF}
  {$IFDEF KeySalt20} KeySalt20 = 'THySTeddq2SoZL5EJd1ZF3Q3UU8oulNG1jCMOqQIrvHBt';  {$ENDIF}
  {$IFDEF KeySalt21} KeySalt21 = 'sIu4nt2dq54zh0mjJxlGMRMBZL5EJd1ZF3Q3U7Fqed6n';   {$ENDIF}
  {$IFDEF KeySalt22} KeySalt22 = 'fqBNNVa7DernVuhlUwdDlWRTVnja77ejrva0';           {$ENDIF}
  {$IFDEF KeySalt23} KeySalt23 = 'MNvJRJOjMuhlU8R_A0pJ_9k6c3d59Acl';               {$ENDIF}
  {$IFDEF KeySalt24} KeySalt24 = 'ZOZX4DJV8JmnnObzSuhlUuhl6c3hlUoIicH9LfZwC';      {$ENDIF}
  {$IFDEF KeySalt25} KeySalt25 = 'Wd0U6c3dLpZUVUvE8NupniaEzuH6c3J1IFpuLm';         {$ENDIF}


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

    function IsKeyChecksumValid:boolean;
  end;


function IsKeyValid_ChecksumOnly(const Key : TLucidityKey):boolean;
function ExtractKeyFromFileContents(FileContents, KeyData : TStringList):boolean;

implementation

uses
  VamLib.Utils,
  eeHashes,
  EncdDecd,
  SysUtils,
  PunyCode;

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

function TLucidityKey.IsKeyChecksumValid: boolean;
begin
  result := IsKeyValid_ChecksumOnly(self);
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


  self.UserName  := KeyData[0];
  self.UserEmail := KeyData[1];
  for c1 := 0 to KeySaltCount-1 do
  begin
    Self.Sections[c1] := KeyData[2 + c1];
  end;
  self.DataCheck := KeyData[27];

  if IsKeyValid_ChecksumOnly(self)
    then result := true
    else result := false;
end;




end.
