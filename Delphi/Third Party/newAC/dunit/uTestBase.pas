(*
  Title: uTestBase
    A collection of ancestor classes for testcases.
*)

{$M+} // RTTI for DUnit

{$Include dunit_options.inc}

unit uTestBase;

interface

uses
  uUtility,

  ACS_WavPack,
  ACS_Classes,
  ACS_Tags,
  ACS_Wave,

  Classes,
  SysUtils,
  Windows,

  TestFramework;


type

  (* Class: TTestFileCode
      Ancestor class for TestCases involving transcoding from a list of
      files. *)
  TTestFileCode = class(TTestCase)
  protected
    FInput,
      FOutput: string;
    FInputFiles: TStringList;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure OnFileDone(Sender: TComponent);
  end;

  (* Class: TTestFileDecode
      Ancestor class for TestCases using any <TAuFileIn> and decoding to WAV.
  *)
  TTestFileDecode = class(TTestFileCode)
  protected
    FWaveOut: TWaveOut;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  (* Procedure: DecodeFiles
      Opens the file specified in sListFile and uses aufInput to decode the
      listed files to temp\<filename>, changing their extension to '.wav'.
      Please note that the resulting WAV headers will *not* be of the Microsoft
      format (ie. CreateMSHeaders = false). This is opposite to the default. *)
    procedure DecodeFiles(sListFile: string; aufInput: TauFileIn);
  published
    procedure TestDecode; virtual; abstract;
  end;

  (* Class: TTestFileEncode
      Ancestor class for TestCases using any <TAuFileOut> and encoding from WAV.
  *)
  TTestFileEncode = class(TTestFileCode)
  protected
    FWaveIn: TWaveIn;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  (* Procedure: EncodeFiles
      Opens the file specified in sListFiles and uses aufOutput to
      encode the listed files to temp\<filename>, changing their extention to
      sExt. *)
    procedure EncodeFiles(sListFile, sExt: string; aufOutput: TauFileOut);
  published
    procedure TestEncode; virtual; abstract;
  end;

implementation

{ TTestInput }

procedure TTestFileDecode.DecodeFiles(sListFile: string; aufInput: TauFileIn);
const
  sHybridExt = ' - hybrid';
var
  i, x: integer;
begin
  FInputFiles.LoadFromFile(sListFile);
  for i := 0 to Pred(FInputFiles.Count) do
  begin
    FInput := FInputFiles[i];
    x := Pos(sHybridExt, FInput);
    if x = 0 then
      FOutput := 'temp\' + ChangeFileExt(ExtractFileName(FInputFiles[i]), '.wav')
    else
    begin
      FOutput := Copy(FInput, 1, Pred(x)) + Copy(FInput, x + Length(sHybridExt), MAXINT);
      FOutput := 'temp\' + ChangeFileExt(ExtractFileName(FOutput), '.wav')
    end;
    {$IFNDEF GenerateSources}
    Assert(FileExists(FInput));
    {$ENDIF}
    aufInput.FileName := FInput;
    FWaveOut.FileName := FOutput;
    FWaveOut.BlockingRun;
    OnFileDone(FWaveOut);
  end;
end;

procedure TTestFileDecode.SetUp;
begin
  inherited;
  FWaveOut := TWaveOut.Create(nil);
  FWaveOut.WavType := wtPCM;
  FWaveOut.CreateNonMsHeaders := true;
end;

procedure TTestFileDecode.TearDown;
begin
  FWaveOut.Free;
  FWaveOut := nil;
  inherited;
end;


{ TTestCode }

procedure TTestFileCode.OnFileDone(Sender: TComponent);
var
  aufInput: TAuFileIn;
  aufOutput: TAuFileOut;
  sReference: string;
begin
  aufOutput := Sender as TAuFileOut;
  aufInput := aufOutput.Input as TAuFileIn;
  sReference := ExtractFilePath(aufInput.FileName) + ExtractFileName(aufOutput.FileName);
  {$IFDEF GenerateSources}
   CopyFile(PChar(FOutput), PChar(sReference), false);
  {$ELSE}
  Check(FileSizeInt64(FOutput) <> 0);
  Check(FilesAreIdentical(sReference, FOutput), 'File Mismatch: ' + aufInput.FileName + ' - ' + sReference);
  {$ENDIF}
end;

procedure TTestFileCode.SetUp;
begin
  inherited;
  FInputFiles := TStringList.Create;
end;

procedure TTestFileCode.TearDown;
var
  i: Integer;
begin
//co-opt FInputFiles for file deleting, comment out the FindFiles line to generate references
  FInputFiles.Clear;
  FindFiles(FInputFiles, 'temp\', '*.*');
  for i := 0 to Pred(FInputFiles.Count) do
    DeleteFile(PChar(FInputFiles[i]));
  FInputFiles.Destroy;
end;

{ TTestEncode }

procedure TTestFileEncode.EncodeFiles(sListFile, sExt: string;
  aufOutput: TauFileOut);
var
  i: integer;
begin
  FInputFiles.LoadFromFile(sListFile);
  for i := 0 to Pred(FInputFiles.Count) do
  begin
    FInput := FInputFiles[i];
    FOutput := 'temp\' + ChangeFileExt(ExtractFileName(FInputFiles[i]), sExt);
    {$IfNDef GenerateSources}
    Assert(FileExists(FInput));
    {$ENDIF}
    FWaveIn.FileName := FInput;
    aufOutput.FileName := FOutput;
    aufOutput.BlockingRun;
    OnFileDone(aufOutput);
    {$IfDef GenerateSources}
    if Pos(' - hybrid', FOutput) > 0 then
    begin
      CopyFile(PChar(FOutput), PChar('media\' + ExtractFilename(FOutput)), false);
      CopyFile(PChar(ChangeFileExt(FOutput, '.wvc')), PChar('media\' + ChangeFileExt(ExtractFilename(FOutput), '.wvc')), false);
    end;
    {$EndIf}
  end;
end;

procedure TTestFileEncode.SetUp;
begin
  inherited;
  FWaveIn := TWaveIn.Create(nil);
end;

procedure TTestFileEncode.TearDown;
begin
  FWaveIn.Destroy;
  inherited;
end;

end.
