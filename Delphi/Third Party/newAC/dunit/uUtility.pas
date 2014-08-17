unit uUtility;

{ Title: uUtility
    All common functions (and some not so common) go in here. This unit can be
    used by every unit, and functions can be called regardless of state. }

interface

uses

  ACS_Classes,
  ACS_smpeg,
  ACS_WavPack,
  ACS_Wave,
  ACS_FLAC,
  ACS_MAC,
  ACS_Vorbis,

  Windows,
  SysUtils,
  Classes;

{ Procedure: FindFiles
    Populates a TStringList with files given a directory and filemask I believe
    this is another one of Zarko Gajic's from http://delphi.about.com.

  Parameters:

    FilesList - TStringList; A pre-existing TStringList for output
    StartDir - string; The directory to start in
    FileMask - string; The filemask to use
}

procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string);
function ProgPath: string;
{ Function: SetFileDate
  Another gem from Zarko Gajic

  Parameters:

    FileName - string; the name of the file to modify
    FileDate - TDateTime; The date to set

  Returns:

    Success - Boolean
}
function SetFileDate(const FileName: string; const FileDate: TDateTime):
  Boolean;

function CalcFileCRC32(sFilename: string): string;
procedure CalcCRC32(pData: Pointer; dwByteCount: DWORD; var dwCRC: DWORD);
function FilesAreIdentical(const sFile1, sFile2: string): Boolean;
function FileSizeInt64(const sPath: WideString): Int64;

 const
  CRCTable: array[0..255] of DWORD =
    ($00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);


implementation

function ProgPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: Integer;
begin
  if StartDir[length(StartDir)] <> '\' then
    StartDir := StartDir + '\';

  { Build a list of the files in directory StartDir
     (not the directories!) }

  IsFound :=
    FindFirst(StartDir + FileMask, faAnyFile - faDirectory, SR) = 0;
  while IsFound do
  begin
    FilesList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Build a list of subdirectories
  DirList := TStringList.Create;
  IsFound := FindFirst(StartDir + '*.*', faAnyFile, SR) = 0;
  while IsFound do
  begin
    if ((SR.Attr and faDirectory) <> 0) and
      (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Scan the list of subdirectories
  for i := 0 to DirList.Count - 1 do
    FindFiles(FilesList, DirList[i], FileMask);

  DirList.Free;
end;

function SetFileDate(const FileName: string; const FileDate: TDateTime):
  Boolean;
var
  FileHandle: THandle;
  FileSetDateResult: Integer;
begin
  FileHandle := 0;
  Result := false;
  try
    try
      FileHandle := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
      if FileHandle > 0 then
      begin
        FileSetDateResult := FileSetDate(FileHandle,
          DateTimeToFileDate(FileDate));
        Result := (FileSetDateResult = 0);
      end;
    except
      Result := false;
    end;
  finally
    FileClose(FileHandle);
  end;
end;

function CreateInputFromExt(sFilename: string): TAuFileIn;
var
  sExt: string;
begin
  sExt := ExtractFileExt(sFilename);
  if sExt = '.wav' then
    Result := TWaveIn.Create(nil)
  else if sExt = '.wv' then
    Result := TWVIn.Create(nil)
  else if sExt = '.mp3' then
    Result := TMP3In.Create(nil)
  else if sExt = '.flac' then
    Result := TFLACIn.Create(nil)
  else if sExt = '.ape' then
    Result := TMACIn.Create(nil)
  else if sExt = '.ogg' then
    Result := TVorbisIn.Create(nil)
  else
    Result := nil;
end;

function CreateOutputFromExt(sFilename: string): TAuFileOut;
var
  sExt: string;
begin
  sExt := ExtractFileExt(sFilename);
  if sExt = '.wav' then
    Result := TWaveOut.Create(nil)
  else if sExt = '.wv' then
    Result := TWVOut.Create(nil)
//  else if sExt = '.mp3' then
//    Result := TMP3Out.Create(nil)
  else if sExt = '.flac' then
    Result := TFLACOut.Create(nil)
  else if sExt = '.ape' then
    Result := TMACOut.Create(nil)
  else if sExt = '.ogg' then
    Result := TVorbisOut.Create(nil)
  else
    Result := nil;
end;

procedure CalcCRC32(pData: Pointer; dwByteCount: DWORD; var dwCRC: DWORD);
var
  i: DWORD;
  pByte: ^byte;
begin
  pByte := pData;
  for i := 0 to dwByteCount - 1 do
  begin
    dwCRC := (dwCRC shr 8) xor
      CRCTable[pByte^ xor (dwCRC and $000000FF)];
    Inc(pByte)
  end
end;

function CalcFileCRC32(sFilename: string): string;
var
  msFile: TMemoryStream;
  dwCRC: DWORD;
  wError: Word;
begin
  wError := 0;
  dwCRC := $FFFFFFFF;
  msFile := TMemoryStream.Create;
  try
    try
      msFile.LoadFromFile(sFilename);
      if msFile.Size > 0 then CalcCRC32(msFile.Memory, msFile.Size, dwCRC)
      except
        on E: EReadError do
          wError := 1
    end;
    dwCRC := not dwCRC
  finally
    msFile.Free;
    if wError = 0 then
      Result := Format('%x', [dwCRC])
    else
      Result := Format('%x', [0]);
  end;
end;

function FilesAreIdentical(const sFile1, sFile2: string): Boolean;
var
  s1, s2: string;
begin
  s1 := CalcFileCRC32(sFile1);
  s2 := CalcFileCRC32(sFile2);
  Result := s1 = s2;
end;

function FileSizeInt64(const sPath: WideString): Int64;
var
  h: Thandle;
  f: TWin32FindDataW;
begin
  Result := -1;
  h := FindFirstFileW(PWideChar(sPath), f);
  if h = INVALID_HANDLE_VALUE then
    Exit;
  Result := Int64(f.nFileSizeLow) + f.nFileSizeHigh * High(f.nFileSizeLow);
  Windows.FindClose(h);
end;

end.

