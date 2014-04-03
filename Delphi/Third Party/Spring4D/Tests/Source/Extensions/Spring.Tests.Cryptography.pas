{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}

unit Spring.Tests.Cryptography;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils,
  TestFramework,
  Spring,
  Spring.Cryptography,
  Spring.Cryptography.Base;

type
//  [Ignore]
  TCryptoTestCase = class abstract(TTestCase)
  protected
    procedure CheckEquals(const expected, actual: TBuffer; const msg: string = ''); overload;
  end;

//  [Ignore]
  THashAlgorithmTestCase = class abstract(TCryptoTestCase)
  protected
    const
      fCData: UnicodeString = 'Delphi Spring Framework';  // fCDefaultDataString
      fBytesAbc : array[0..2] of Byte = (Byte('a'), Byte('b'), Byte('c'));

  protected
    fActual: TBuffer;
    fExpected: TBuffer;
  end;

  TTestCRC16 = class(THashAlgorithmTestCase)
  private
    const
{$IFNDEF NEXTGEN}
      fCData: AnsiString = '123456789';
{$ELSE}
      fCData = '123456789';
{$ENDIF}
      fCExpectedHashOfEmptyBuffer = '0000';
      fCExpectedHashOfData = 'BB3D';
  private
    fCRC16: ICRC16;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyBuffer;
    procedure TestNonEmptyBuffer;
    procedure TestABC;
  end;

  TTestCRC32 = class(THashAlgorithmTestCase)
  private
    const
{$IFNDEF NEXTGEN}
      fCData: AnsiString = '123456789';
{$ELSE}
      fCData = '123456789';
{$ENDIF}
      fCExpectedHashOfEmptyBuffer = '00000000';
      fCExpectedHashOfData = 'CBF43926';
  private
    fCRC32: ICRC32;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyBuffer;
    procedure TestNonEmptyBuffer;
    procedure TestABC;
  end;

  TTestMD5 = class(THashAlgorithmTestCase)
  private
    const
      fCExpectedHashOfEmptyBuffer  = 'D41D8CD98F00B204E9800998ECF8427E';
      fCExpectedHashOfData         = '0x489677428096293B28F845955503B1E5';
  private
    fMD5: IMD5;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyBuffer;
    procedure TestNonEmptyBuffer;
    procedure TestEmptyStream;
  end;

  TTestSHA1 = class(THashAlgorithmTestCase)
  private
    const
      fCExpectedHashOfEmptyBuffer = 'da39a3ee5e6b4b0d3255bfef95601890afd80709';
      fCExpectedHashOfData        = 'bca8bac7e322b4262e55fc3b4554c57c20bf426e';
  private
    fSHA1: ISHA1;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyBuffer;
    procedure TestNonEmptyBuffer;
  end;

  TTestSHA256 = class(THashAlgorithmTestCase)
  private
    const
      fCExpectedHashOfEmptyBuffer = 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855';
      fCExpectedHashOfData        = '3f0cd24238e25399b9d722ad366244d4e79be0f47f37da0312a325a81e03a620';
  private
    fSHA256: ISHA256;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyBuffer;
    procedure TestNormalData;
  end;

  TTestSHA384 = class(THashAlgorithmTestCase)
  private
    const
      fCExpectedHashOfEmptyBuffer = '38b060a751ac96384cd9327eb1b1e36a21fdb71114be07434c0cc7bf63f6e1da274edebfe76f65fbd51ad2f14898b95b';
      fCExpectedHashOfData        = 'e2b2372309461dfd765098165c6dee2518b3193a84938c113d5a947dae30e032c5b33cb4015119dfc7821fc72ae02dc9';
  private
    fSHA384: ISHA384;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyBuffer;
    procedure TestNormalData;
  end;

  TTestSHA512 = class(THashAlgorithmTestCase)
  private
    const
      fCExpectedHashOfEmptyBuffer = 'cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e';
      fCExpectedHashOfData        = 'f1d91db9bb299887508f83eaa521a76cd79e87e24e2dd678ff589886ad76034f312fc5e8bff6fb598e8ebd5e1a4fdaa9f25ed5a85eb093d47a7258e3948142c8';
  private
    fSHA512: ISHA512;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestEmptyBuffer;
    procedure TestNormalData;
  end;

  TMockSymmetricAlgorithm = class(TSymmetricAlgorithmBase)
  protected
    procedure DoEncryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
    procedure DoDecryptBlock(const inputBuffer: TBytes; var outputBuffer: TBytes); override;
  end;

//  [Ignore]
  TSymmetricAlgorithmTestCase = class abstract(TCryptoTestCase)
  protected
    fInputBuffer: TBuffer;
    fOutputBuffer: TBuffer;
    fActualBuffer: TBuffer;
    fExpectedBuffer: TBuffer;
    fKey: TBuffer;
  end;

//  [Ignore]
  TTestSymmetricAlgorithmBase = class(TSymmetricAlgorithmTestCase)
  private
    fAlgorithm: ISymmetricAlgorithm;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure CheckResult(const inputBuffer, outputBuffer: TBuffer); virtual;
  published
    procedure TestEmptyInputBuffer;
  end;

  TTestPaddingModeIsNone = class(TTestSymmetricAlgorithmBase)
  protected
    procedure SetUp; override;
  published
    procedure TestOneBlock;
    procedure TestTwoBlocks;
    procedure TestExceptions;
  end;

  TTestPaddingModeIsPKCS7 = class(TTestSymmetricAlgorithmBase)
  protected
    procedure SetUp; override;
  published
    procedure TestPaddingSizeIsFull;
    procedure TestPaddingSizeIsOne;
    procedure TestPaddingSizeIsSeven;
  end;

  TTestPaddingModeIsZeros = class(TTestSymmetricAlgorithmBase)
  protected
    procedure SetUp; override;
  published
    procedure TestPaddingSizeIsFull;
    procedure TestPaddingSizeIsOne;
    procedure TestPaddingSizeIsSeven;
  end;

  TTestPaddingModeIsANSIX923 = class(TTestSymmetricAlgorithmBase)
  protected
    procedure SetUp; override;
  published
    procedure TestPaddingSizeIsFull;
    procedure TestPaddingSizeIsOne;
    procedure TestPaddingSizeIsSeven;
  end;

  TTestPaddingModeIsISO10126 = class(TTestSymmetricAlgorithmBase)
  protected
    procedure SetUp; override;
  published
    procedure TestPaddingSizeIsFull;
    procedure TestPaddingSizeIsOne;
    procedure TestPaddingSizeIsSeven;
  end;

  TTestDES = class(TSymmetricAlgorithmTestCase)
  private
    fDES: IDES;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
//    procedure TestDefaultProperties;
    procedure TestCase1;
    procedure TestCase2;
    procedure TestCBC;
    procedure TestDecrypt;
  end;

  TTestTripleDES = class(TSymmetricAlgorithmTestCase)
  private
    fTripleDES: ITripleDES;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestECB;
    procedure TestCBC;
  end;

implementation

{ TCryptoTestCase }

procedure TCryptoTestCase.CheckEquals(const expected, actual: TBuffer;
  const msg: string);
begin
  FCheckCalled := True;
  if not expected.Equals(actual) then
    FailNotEquals(expected.ToHexString, actual.ToHexString, msg);
end;

{ TTestCRC16 }

procedure TTestCRC16.SetUp;
begin
  inherited;
  fCRC16 := CreateCRC16;
end;

procedure TTestCRC16.TearDown;
begin
  fCRC16 := nil;
  inherited;
end;

procedure TTestCRC16.TestEmptyBuffer;
begin
  fActual := fCRC16.ComputeHash([]);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfEmptyBuffer);
  CheckEquals(fExpected, fActual);
end;

procedure TTestCRC16.TestNonEmptyBuffer;
begin
{$IFNDEF NEXTGEN}
  fActual := fCRC16.ComputeHash(fCData);
{$ELSE}
  fActual := fCRC16.ComputeHash(TEncoding.ANSI.GetBytes(fCData));
{$ENDIF}
  fExpected := TBuffer.FromHexString(fCExpectedHashOfData);
  CheckEquals(fExpected, fActual);
end;

procedure TTestCRC16.TestABC;
begin
  fActual := fCRC16.ComputeHash(fBytesAbc);
  fExpected := TBuffer.FromHexString('9738');
  CheckEquals(fExpected, fActual);
end;

{ TTestCRC32 }

procedure TTestCRC32.SetUp;
begin
  inherited;
  fCRC32 := CreateCRC32;
end;

procedure TTestCRC32.TearDown;
begin
  fCRC32 := nil;
  inherited;
end;

procedure TTestCRC32.TestEmptyBuffer;
begin
  fActual := fCRC32.ComputeHash([]);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfEmptyBuffer);
  CheckEquals(fExpected, fActual);
end;

procedure TTestCRC32.TestNonEmptyBuffer;
begin
{$IFNDEF NEXTGEN}
  fActual := fCRC32.ComputeHash(fCData);
{$ELSE}
  fActual := fCRC32.ComputeHash(TEncoding.ANSI.GetBytes(fCData));
{$ENDIF}
  fExpected := TBuffer.FromHexString(fCExpectedHashOfData);
  CheckEquals(fExpected, fActual);
end;

procedure TTestCRC32.TestABC;
begin
  fActual := fCRC32.ComputeHash(fBytesAbc);
  fExpected := TBuffer.FromHexString('352441C2');
  CheckEquals(fExpected, fActual);
end;


{$IFDEF SUPPORTS_REGION}{$REGION 'TTestMD5'}{$ENDIF}

procedure TTestMD5.SetUp;
begin
  inherited;
  fMD5 := CreateMD5;
end;

procedure TTestMD5.TearDown;
begin
  fMD5 := nil;
  inherited;
end;

procedure TTestMD5.TestEmptyBuffer;
begin
  fActual := fMD5.ComputeHash([]);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfEmptyBuffer);
  CheckEquals(fExpected, fActual);
end;

procedure TTestMD5.TestNonEmptyBuffer;
begin
  fActual := fMD5.ComputeHash(fCData);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfData);
  CheckEquals(fExpected, fActual);
end;

procedure TTestMD5.TestEmptyStream;
var
  stream: TStream;
begin
  stream := TMemoryStream.Create;
  try
    fActual := fMD5.ComputeHash(stream);
    fExpected := TBuffer.FromHexString(fCExpectedHashOfEmptyBuffer);
    CheckEquals(fExpected, fActual);
  finally
    stream.Free;
  end;
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{ TTestSHA1 }

procedure TTestSHA1.SetUp;
begin
  inherited;
  fSHA1 := CreateSHA1;
end;

procedure TTestSHA1.TearDown;
begin
  fSHA1 := nil;
  inherited;
end;

procedure TTestSHA1.TestEmptyBuffer;
begin
  fActual := fSHA1.ComputeHash([]);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfEmptyBuffer);
  CheckEquals(fExpected, fActual, '');
end;

procedure TTestSHA1.TestNonEmptyBuffer;
begin
  fActual := fSHA1.ComputeHash(fCData);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfData);
  CheckEquals(fExpected, fActual);
end;

{ TTestSHA256 }

procedure TTestSHA256.SetUp;
begin
  inherited;
  fSHA256 := CreateSHA256;
end;

procedure TTestSHA256.TearDown;
begin
  fSHA256 := nil;
  inherited;
end;

procedure TTestSHA256.TestEmptyBuffer;
begin
  fActual := fSHA256.ComputeHash([]);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfEmptyBuffer);
  CheckEquals(fExpected, fActual, '');
end;

procedure TTestSHA256.TestNormalData;
begin
  fActual := fSHA256.ComputeHash(fCData);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfData);
  CheckEquals(fExpected, fActual);
end;

{ TTestSHA384 }

procedure TTestSHA384.SetUp;
begin
  inherited;
  fSHA384 := CreateSHA384;
end;

procedure TTestSHA384.TearDown;
begin
  fSHA384 := nil;
  inherited;
end;

procedure TTestSHA384.TestEmptyBuffer;
begin
  fActual := fSHA384.ComputeHash([]);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfEmptyBuffer);
  CheckEquals(fExpected, fActual, '');
end;

procedure TTestSHA384.TestNormalData;
begin
  fActual := fSHA384.ComputeHash(fCData);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfData);
  CheckEquals(fExpected, fActual);
end;

{ TTestSHA512 }

procedure TTestSHA512.SetUp;
begin
  inherited;
  fSHA512 := CreateSHA512;
end;

procedure TTestSHA512.TearDown;
begin
  fSHA512 := nil;
  inherited;
end;

procedure TTestSHA512.TestEmptyBuffer;
begin
  fActual := fSHA512.ComputeHash([]);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfEmptyBuffer);
  CheckEquals(fExpected, fActual, '');
end;

procedure TTestSHA512.TestNormalData;
begin
  fActual := fSHA512.ComputeHash(fCData);
  fExpected := TBuffer.FromHexString(fCExpectedHashOfData);
  CheckEquals(fExpected, fActual);
end;


{$IFDEF SUPPORTS_REGION}{$REGION 'TTestDES'}{$ENDIF}

procedure TTestDES.SetUp;
begin
  inherited;
  fDES := CreateDES;
  fDES.CipherMode := TCipherMode.ECB;
  fDES.PaddingMode := TPaddingMode.None;
end;

procedure TTestDES.TearDown;
begin
  fDES := nil;
  inherited;
end;

procedure TTestDES.TestCase1;
const
  inputData: array[0..7] of Byte = ($07, $56, $D8, $E0, $77, $47, $61, $D2);
  outputData: array[0..7] of Byte = ($0C, $D3, $DA, $02, $00, $21, $DC, $09);
  key: array[0..7] of Byte = ($01, $70, $F1, $75, $46, $8F, $B5, $E6);
begin
  fInputBuffer := TBuffer.Create(inputData);
  fOutputBuffer := TBuffer.Create(outputData);
  fKey := TBuffer.Create(key);
  fDES.Key := fKey;

  fActualBuffer := fDES.Encrypt(fInputBuffer);
  CheckEquals(fOutputBuffer, fActualBuffer);

  fActualBuffer := fDES.Decrypt(fOutputBuffer);
  CheckEquals(fInputBuffer, fActualBuffer);
end;

procedure TTestDES.TestCase2;
const
  inputData: array[0..7] of Byte = ($48, $0D, $39, $00, $6E, $E7, $62, $F2);
  outputData: array[0..7] of Byte = ($A1, $F9, $91, $55, $41, $02, $0B, $56);
  key: array[0..7] of Byte = ($02, $58, $16, $16, $46, $29, $B0, $07);
begin
  fInputBuffer := TBuffer.Create(inputData);
  fOutputBuffer := TBuffer.Create(outputData);
  fKey := TBuffer.Create(key);
  fDES.Key := fKey;

  fActualBuffer := fDES.Encrypt(fInputBuffer);
  CheckEquals(fOutputBuffer, fActualBuffer);

  fActualBuffer := fDES.Decrypt(fOutputBuffer);
  CheckEquals(fInputBuffer, fActualBuffer);
end;

procedure TTestDES.TestCBC;
begin
{$IFNDEF NEXTGEN}
  fInputBuffer := TBuffer.Create(AnsiString('Now is the time for all '));
{$ELSE}
  fInputBuffer := TBuffer.Create(TEncoding.ANSI.GetBytes('Now is the time for all '));
{$ENDIF}
  fOutputBuffer := TBuffer.FromHexString('e5c7cdde872bf27c 43e934008c389c0f 683788499a7c05f6');
  fKey := TBuffer.FromHexString('0123456789abcdef');
  fDES.IV := TBuffer.FromHexString('1234567890abcdef');
  fDES.Key := fKey;
  fDES.CipherMode := TCipherMode.CBC;

  fActualBuffer := fDES.Encrypt(fInputBuffer);
  CheckEquals(fOutputBuffer, fActualBuffer, 'Encryption: ');

  fActualBuffer := fDES.Decrypt(fOutputBuffer);
  CheckEquals(fInputBuffer, fActualBuffer, 'Decryption: ');
end;

procedure TTestDES.TestDecrypt;
begin
  fInputBuffer := TBuffer.Create('Hello');
  fDES.PaddingMode := TPaddingMode.PKCS7;
  fDES.Key := TBuffer.FromHexString('0123456789abcdef');
  fDES.IV := TBuffer.FromHexString('1234567890abcdef');
  fOutputBuffer := fDES.Encrypt(fInputBuffer);
  fActualBuffer := fDES.Decrypt(fOutputBuffer);

  CheckEquals(fInputBuffer, fActualBuffer);
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{$IFDEF SUPPORTS_REGION}{$REGION 'TTestTripleDES'}{$ENDIF}

procedure TTestTripleDES.SetUp;
begin
  inherited;
  fTripleDES := CreateTripleDES;
end;

procedure TTestTripleDES.TearDown;
begin
  fTripleDES := nil;
  inherited;
end;

procedure TTestTripleDES.TestECB;
const
  key: array[0..23] of Byte = ($01, $23, $45, $67, $89, $AB, $CD, $EF, $FE,
    $DC, $BA, $98, $76, $54, $32, $10, $89, $AB, $CD, $EF, $01, $23, $45, $67);
  inputBuffer: array[0..7] of Byte = ($01, $23, $45, $67, $89, $AB, $CD, $E7);
  outputBuffer: array[0..7] of Byte = ($DE, $0B, $7C, $06, $AE, $5E, $0E, $D5);
begin
  fTripleDES.CipherMode := TCipherMode.ECB;
  fTripleDES.PaddingMode := TPaddingMode.None;
  fTripleDES.Key := TBuffer.Create(key);
  fInputBuffer := TBuffer.Create(inputBuffer);
  fOutputBuffer := TBuffer.Create(outputBuffer);
  fActualBuffer := fTripleDES.Encrypt(fInputBuffer);
  CheckEquals(fOutputBuffer, fActualBuffer);
  fActualBuffer := fTripleDES.Decrypt(fOutputBuffer);
  CheckEquals(fInputBuffer, fActualBuffer);
end;

procedure TTestTripleDES.TestCBC;
begin
  fKey := TBuffer.FromHexString('01 23 45 67 89 ab cd ef 23 45 67 89 ab cd ef 01 01 23 45 67 89 ab cd ef');
  fTripleDES.CipherMode := TCipherMode.CBC;
  fTripleDES.PaddingMode := TPaddingMode.PKCS7;
  fTripleDES.Key := fKey;
  fTripleDES.IV := TBuffer.BytesOf($0, fTripleDES.BlockSize div 8);
  fInputBuffer := TBuffer.Create([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]);
  fExpectedBuffer := TBuffer.FromHexString('23 61 AC E6 C5 17 10 51 D9 CB 92 8C 76 89 35 84');
  fActualBuffer := fTripleDES.Encrypt(fInputBuffer);
  CheckEquals(fExpectedBuffer, fActualBuffer, 'Encryption');
end;

{$IFDEF SUPPORTS_REGION}{$ENDREGION}{$ENDIF}


{ TMockSymmetricAlgorithm }

procedure TMockSymmetricAlgorithm.DoEncryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
begin
  Assert(Length(inputBuffer) = Length(outputBuffer));
  Move(inputBuffer[0], outputBuffer[0], Length(inputBuffer));
end;

procedure TMockSymmetricAlgorithm.DoDecryptBlock(const inputBuffer: TBytes;
  var outputBuffer: TBytes);
begin
  Assert(Length(inputBuffer) = Length(outputBuffer));
  Move(inputBuffer[0], outputBuffer[0], Length(inputBuffer));
end;

{ TTestSymmetricAlgorithmBase }

procedure TTestSymmetricAlgorithmBase.SetUp;
begin
  inherited;
  fAlgorithm := TMockSymmetricAlgorithm.Create([8 * 8], [8 * 8]);
  fAlgorithm.BlockSize := 8 * 8;
  fAlgorithm.KeySize := 8 * 8;
  fAlgorithm.CipherMode := TCipherMode.ECB;
  fAlgorithm.PaddingMode := TPaddingMode.None;
end;

procedure TTestSymmetricAlgorithmBase.TearDown;
begin
  fAlgorithm := nil;
  inherited;
end;

procedure TTestSymmetricAlgorithmBase.CheckResult(const inputBuffer,
  outputBuffer: TBuffer);
begin
  fActualBuffer := fAlgorithm.Encrypt(inputBuffer);
  CheckEquals(outputBuffer, fActualBuffer, 'Encryption: ');

  fActualBuffer := fAlgorithm.Decrypt(outputBuffer);
  CheckEquals(inputBuffer, fActualBuffer, 'Decryption: ');
end;

procedure TTestSymmetricAlgorithmBase.TestEmptyInputBuffer;
begin
  CheckResult(TBuffer.Empty, TBuffer.Empty);
end;

{ TTestPaddingModeIsNone }

procedure TTestPaddingModeIsNone.SetUp;
begin
  inherited;
  fAlgorithm.CipherMode := TCipherMode.ECB;
  fAlgorithm.PaddingMode := TPaddingMode.None;
end;

procedure TTestPaddingModeIsNone.TestOneBlock;
begin
  fInputBuffer := TBuffer.BytesOf(1, 8);
  CheckResult(fInputBuffer, fInputBuffer);
end;

procedure TTestPaddingModeIsNone.TestTwoBlocks;
begin
  fInputBuffer := TBuffer.BytesOf(1, 16);
  CheckResult(fInputBuffer, fInputBuffer);
end;

procedure TTestPaddingModeIsNone.TestExceptions;
begin
  fInputBuffer := TBuffer.BytesOf(1, 7);
  ExpectedException := ECryptographicException;
  fOutputBuffer := fAlgorithm.Encrypt(fInputBuffer);
end;

{ TTestPaddingModeIsPKCS7 }

procedure TTestPaddingModeIsPKCS7.SetUp;
begin
  inherited;
  fAlgorithm.PaddingMode := TPaddingMode.PKCS7;
end;

procedure TTestPaddingModeIsPKCS7.TestPaddingSizeIsFull;
begin
  fInputBuffer := TBuffer.BytesOf($01, 8);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($08, 8);
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 16);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($08, 8);
  CheckResult(fInputBuffer, fOutputBuffer);
end;

procedure TTestPaddingModeIsPKCS7.TestPaddingSizeIsOne;
begin
  fInputBuffer := TBuffer.BytesOf($01, 7);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($01, 1);
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 15);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($01, 1);
  CheckResult(fInputBuffer, fOutputBuffer);
end;

procedure TTestPaddingModeIsPKCS7.TestPaddingSizeIsSeven;
begin
  fInputBuffer := TBuffer.BytesOf($01, 1);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($07, 7);
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 9);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($07, 7);
  CheckResult(fInputBuffer, fOutputBuffer);
end;

{ TTestPaddingModeIsZeros }

procedure TTestPaddingModeIsZeros.SetUp;
begin
  inherited;
  fAlgorithm.PaddingMode := TPaddingMode.Zeros;
end;

procedure TTestPaddingModeIsZeros.TestPaddingSizeIsFull;
begin
  fInputBuffer := TBuffer.BytesOf($01, 8);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 8);
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 16);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 8);
  CheckResult(fInputBuffer, fOutputBuffer);
end;

procedure TTestPaddingModeIsZeros.TestPaddingSizeIsOne;
begin
  fInputBuffer := TBuffer.BytesOf($01, 7);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 1);
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 15);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 1);
  CheckResult(fInputBuffer, fOutputBuffer);
end;

procedure TTestPaddingModeIsZeros.TestPaddingSizeIsSeven;
begin
  fInputBuffer := TBuffer.BytesOf($01, 1);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 7);
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 9);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 7);
  CheckResult(fInputBuffer, fOutputBuffer);
end;

{ TTestPaddingModeIsANSIX923 }

procedure TTestPaddingModeIsANSIX923.SetUp;
begin
  inherited;
  fAlgorithm.PaddingMode := TPaddingMode.ANSIX923;
end;

procedure TTestPaddingModeIsANSIX923.TestPaddingSizeIsFull;
begin
  fInputBuffer := TBuffer.BytesOf($01, 8);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 7) + $08;
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 16);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 7) + $08;
  CheckResult(fInputBuffer, fOutputBuffer);
end;

procedure TTestPaddingModeIsANSIX923.TestPaddingSizeIsOne;
begin
  fInputBuffer := TBuffer.BytesOf($01, 7);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($01, 1);
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 15);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($01, 1);
  CheckResult(fInputBuffer, fOutputBuffer);
end;

procedure TTestPaddingModeIsANSIX923.TestPaddingSizeIsSeven;
begin
  fInputBuffer := TBuffer.BytesOf($01, 1);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 6) + $07;
  CheckResult(fInputBuffer, fOutputBuffer);

  fInputBuffer := TBuffer.BytesOf($02, 9);
  fOutputBuffer := fInputBuffer + TBuffer.BytesOf($00, 6) + $07;
  CheckResult(fInputBuffer, fOutputBuffer);
end;

{ TTestPaddingModeIsISO10126 }

procedure TTestPaddingModeIsISO10126.SetUp;
begin
  inherited;
  fAlgorithm.PaddingMode := TPaddingMode.ANSIX923;
end;

procedure TTestPaddingModeIsISO10126.TestPaddingSizeIsFull;
begin
  fInputBuffer := TBuffer.BytesOf($01, 8);
  fActualBuffer := fAlgorithm.Encrypt(fInputBuffer);
  CheckEquals(16, fActualBuffer.Size);
  CheckEquals($08, fActualBuffer.Last);

  fInputBuffer := TBuffer.BytesOf($02, 16);
  fActualBuffer := fAlgorithm.Encrypt(fInputBuffer);
  CheckEquals(24, fActualBuffer.Size);
  CheckEquals($08, fActualBuffer.Last);
end;

procedure TTestPaddingModeIsISO10126.TestPaddingSizeIsOne;
begin
  fInputBuffer := TBuffer.BytesOf($01, 7);
  fActualBuffer := fAlgorithm.Encrypt(fInputBuffer);
  CheckEquals(8, fActualBuffer.Size);
  CheckEquals($01, fActualBuffer.Last);

  fInputBuffer := TBuffer.BytesOf($02, 15);
  fActualBuffer := fAlgorithm.Encrypt(fInputBuffer);
  CheckEquals(16, fActualBuffer.Size);
  CheckEquals($01, fActualBuffer.Last);
end;

procedure TTestPaddingModeIsISO10126.TestPaddingSizeIsSeven;
begin
  fInputBuffer := TBuffer.BytesOf($01, 1);
  fActualBuffer := fAlgorithm.Encrypt(fInputBuffer);
  CheckEquals(8, fActualBuffer.Size);
  CheckEquals($07, fActualBuffer.Last);

  fInputBuffer := TBuffer.BytesOf($02, 9);
  fActualBuffer := fAlgorithm.Encrypt(fInputBuffer);
  CheckEquals(16, fActualBuffer.Size);
  CheckEquals($07, fActualBuffer.Last);
end;

end.
