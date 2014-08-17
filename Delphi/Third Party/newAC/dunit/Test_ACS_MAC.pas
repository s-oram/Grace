(*
  This file is a part of New Audio Components package v 1.6
  Copyright (c) 2002-2007, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net

  ****************************************************************************
*)

(* $Id: Test_ACS_MAC.pas 890 2008-06-22 20:10:50Z bob $ *)

(* Title: ACS_MAC TestCases
    This unit contains TestCases for <ACS_MAC>. More information about DUnit
    can be found at http://dunit.sourceforge.net. *)

unit Test_ACS_MAC;

interface

uses
  uTestBase,
  TestFramework, ACS_MAC, ACS_Classes, ACS_Types, Classes, ACS_Tags, SysUtils, MACDll,
  Windows;
type

  (* Class: TestMACEncode
      Tests encoding of WAV files to MAC by comparing the output to
      reference files using CRC32. *)
  TestMACEncode = class(TTestFileEncode)
  strict private
    FMACOut: TMACOut;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  (* Procedure: TestEncode
      Encodes all WAV files specified in encode_MAC.txt. *)
    procedure TestEncode; override;
  end;

  (* Class: TestMACDecode
      Tests decoding of MAC files to WAV by comparing the output to
      reference files using CRC32. *)
  TestMACDecode = class(TTestFileDecode)
  strict private
    FMACIn: TMACIn;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
  (* Procedure: TestDecode
      Decodes all MAC files specified in decode_MAC.txt .*)
    procedure TestDecode; override;
  end;

implementation

procedure TestMACEncode.SetUp;
begin
  inherited;
  FMACOut := TMACOut.Create(nil);
  FMACOut.Input := FWaveIn;
end;

procedure TestMACEncode.TearDown;
begin
  FMACOut.Free;
  FMACOut := nil;
  inherited;
end;

procedure TestMACEncode.TestEncode;
begin
  EncodeFiles('encode_mac.txt', '.ape', FMACOut);
end;

procedure TestMACDecode.SetUp;
begin
  inherited;
  FMACIn := TMACIn.Create(nil);
  FWaveOut.Input := FMACIn;
end;

procedure TestMACDecode.TearDown;
begin
  FMACIn.Free;
  FMACIn := nil;
  inherited;
end;

procedure TestMACDecode.TestDecode;
begin
  DecodeFiles('decode_mac.txt', FMACIn);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestMACEncode.Suite);
  RegisterTest(TestMACDecode.Suite);
end.

