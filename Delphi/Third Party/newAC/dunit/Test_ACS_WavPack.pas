(*
  This file is a part of New Audio Components package v 1.6
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

  ****************************************************************************

  TWVIn and TWVOut components are written by Sergei Borisov, <jr_ross@mail.ru>
*)

(* $Id:$ *)

(* Title: ACS_WavPack TestCases
    This unit contains TestCases for <ACS_WavPack>. More information about DUnit
    can be found at http://dunit.sourceforge.net. *)

unit Test_ACS_WavPack;

{$Include dunit_options.inc}

interface

uses
  uUtility,
  uTestBase,
  uLog,

  ACS_WavPack,
  ACS_Classes,
  ACS_Tags,
  ACS_Wave,
  WavPackDLL,

  TestFramework,

  Classes,
  Dialogs,
  SysUtils,
  Windows;

type

  (* Class: TestWVDecode
      Tests decoding of WavPack files to WAV by comparing the output to
      reference files using CRC32. *)
  TestWVDecode = class(TTestFileDecode)
  strict protected
    FWVIn: TWVIn;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  (* Procedure: TestDecode
      Decodes all WV files specified in decode_wv.txt .*)
    procedure TestDecode; override;
  (* Procedure: TestFileNotFound
      Attempts to open a file which doesn't exist. *)
    procedure TestFileNotFound;
  end;

  (* Class: TestWVEncode
      Tests encoding of WAV files to WavPack by comparing the output to
      reference files using CRC32. *)
  TestWVEncode = class(TTestFileEncode)
  protected
    FWVOut: TWVOut;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  (* Procedure: TestEncode
      Encodes all WAV files specified in encode_wv.txt. *)
    procedure TestEncode; override;
  (* Procedure: TestEncodeHybrid
      Encodes all WAV files specified in encode_wv.txt using the WavPack's
      Hybrid mode. *)
    procedure TestEncodeHyrbid;
  end;

implementation

procedure TestWVDecode.SetUp;
begin
  inherited;
  FWVIn := TWVIn.Create(nil);
  FWaveOut.Input := FWVIn;
end;

procedure TestWVDecode.TearDown;
begin
  inherited;
  FWVIn.Free;
  FWVIn := nil;
end;

procedure TestWVDecode.TestDecode;
begin
{$IfDef GenerateSources}
  DecodeFiles('decode_wv_reference.txt', FWVIn);
{$Else}
  DecodeFiles('decode_wv.txt', FWVIn);
{$EndIf}
end;

// leaks a TAuFileStream created at Line 1280 in ACS_Wave
// this actually ends up testing TWaveOut, so a new unit should be made
// with a testcase for ACS_Wave
// this procedure would then be moved to TestWVEncode and used with a
// nonexistent wav
procedure TestWVDecode.TestFileNotFound;
begin
  FWVIn.FileName := 'not.found.wv';
  FWaveOut.FileName := 'temp\output.wav';
  try
    FWaveOut.BlockingRun;
  except
    on E: Exception do
    begin
      LogIt('File Not Found Message: ' + E.Message);
      Check(E.Message = 'The system cannot find the file specified', 'Bad File Not Found Message: ' + E.Message);
    end;
  end;
end;

procedure TestWVEncode.SetUp;
begin
  inherited;
  FWVOut := TWVOut.Create(nil);
  FWVOut.Input := FWaveIn;
end;

procedure TestWVEncode.TearDown;
begin
  FWVOut.Free;
  FWVOut := nil;
  inherited;
end;

procedure TestWVEncode.TestEncode;
begin
  EncodeFiles('encode_wv.txt', '.wv', FWVOut);
end;

procedure TestWVEncode.TestEncodeHyrbid;
var
  slCorrections: TStringList;
  i: integer;
begin
  FWVOut.HybridMode := true;
  EncodeFiles('encode_wv.txt', ' - hybrid.wv', FWVOut);
  {$IfNDef GenerateSources}
  slCorrections := TStringList.Create;
  try
    slCorrections.LoadFromFile('encode_wv.txt');
    for i := 0 to Pred(slCorrections.Count) - 1 do
      Check(FilesAreIdentical(
        ChangeFileExt(slCorrections[i], ' - hybrid.wvc'),
        'temp\' + ExtractFilename(ChangeFileExt(slCorrections[i], ' - hybrid.wvc'))),
        'Corrections file mismatch');
  finally
    slCorrections.Destroy;
  end;
  {$EndIf}
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestWVDecode.Suite);
  RegisterTest(TestWVEncode.Suite);

end.

