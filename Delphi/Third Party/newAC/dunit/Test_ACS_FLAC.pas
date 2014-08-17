(*
  This file is a part of New Audio Components package v 1.6
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

  ****************************************************************************
*)

(* $Id: Test_ACS_FLAC.pas 890 2008-06-22 20:10:50Z bob $ *)

(* ACS_FLAC TestCases
    This unit contains TestCases for <ACS_FLAC>. More information about DUnit
    can be found at http://dunit.sourceforge.net. *)

unit Test_ACS_FLAC;

interface

uses
  uTestBase,
  TestFramework, ACS_FLAC, ACS_Classes, ACS_Types, Classes, ACS_Tags, SysUtils, FLAC,
  Windows;
type

  (* Class: TestFLACEncode
      Tests encoding of WAV files to FLAC by comparing the output to
      reference files using CRC32. *)
  TestFLACEncode = class(TTestFileEncode)
  strict private
    FFLACOut: TFLACOut;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  (* Procedure: TestEncode
      Encodes all WAV files specified in encode_flac.txt. *)
    procedure TestEncode; override;
  end;

  (* Class: TestFLACDecode
      Tests decoding of FLAC files to WAV by comparing the output to
      reference files using CRC32. *)
  TestFLACDecode = class(TTestFileDecode)
  strict private
    FFLACIn: TFLACIn;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  (* Procedure: TestDecode
      Decodes all FLAC files specified in decode_flac.txt .*)
    procedure TestDecode; override;
  end;

implementation

procedure TestFLACEncode.SetUp;
begin
  inherited;
  FFLACOut := TFLACOut.Create(nil);
  FFLACOut.Input := FWaveIn;
end;

procedure TestFLACEncode.TearDown;
begin
  FFLACOut.Free;
  FFLACOut := nil;
  inherited;
end;

procedure TestFLACEncode.TestEncode;
begin
  EncodeFiles('encode_flac.txt', '.flac', FFLACOut);
end;

procedure TestFLACDecode.SetUp;
begin
  inherited;
  FFLACIn := TFLACIn.Create(nil);
  FWaveOut.Input := FFLACIn;
end;

procedure TestFLACDecode.TearDown;
begin
  FFLACIn.Free;
  FFLACIn := nil;
  inherited;
end;

procedure TestFLACDecode.TestDecode;
begin
  DecodeFiles('decode_flac.txt', FFLACIn);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestFLACEncode.Suite);
  RegisterTest(TestFLACDecode.Suite);
end.

