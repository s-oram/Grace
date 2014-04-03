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

///	<preliminary />
///	<seealso href="http://msdn.microsoft.com/en-us/library/92f9ye3s(VS.71).aspx" />
///	<seealso href="http://msdn.microsoft.com/en-us/library/system.security.cryptography.aspx" />
///	<seealso href="http://en.wikipedia.org/wiki/Cryptography" />
unit Spring.Cryptography;

{$I Spring.inc}
{$R-}
{$IFDEF DELPHIXE4_UP}
  {$ZEROBASEDSTRINGS OFF}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  Spring,
  Spring.Collections;

type
  {$SCOPEDENUMS ON}

  {$REGION 'Core Types'}

  ///	<summary>
  ///	  TCipherMode
  ///	</summary>
  ///	<remarks>
  ///	  The cipher modes cmOFB, cmCFB and cmCTS have not been supported yet.
  ///	</remarks>
  ///	<seealso href="http://en.wikipedia.org/wiki/Block_cipher_mode_of_operation">
  ///	  Block Cipher Mode of Operation
  ///	</seealso>
  TCipherMode = (
    ///	<summary>Cipher-Block Chaining</summary>
    CBC,

    ///	<summary>Electronic Codebook</summary>
    ECB{,

    ///	<summary>Output Feedback (<b>Not yet implemented</b>)</summary>
    OFB,

    ///	<summary>Cipher Feedback (<b>Not yet implemented</b>)</summary>
    CFB,

    ///	<summary>Cipher Text Stealing (<b>Not yet implemented</b>)</summary>
    CTS}
  );

  ///	<summary>
  ///	  Specifies the type of padding to apply when the message data block is
  ///	  shorter than the full number of bytes needed for a cryptographic
  ///	  operation.
  ///	</summary>
  ///	<seealso href="http://msdn.microsoft.com/en-us/library/system.security.cryptography.paddingmode.aspx" />
  ///	<seealso href="http://en.wikipedia.org/wiki/Padding_(cryptography)" />
  TPaddingMode = (
    /// <summary>
    /// No padding is done.
    /// </summary>
    None,

    /// <summary>
    /// The PKCS #7 padding string consists of a sequence of bytes, each of
    /// which is equal to the total number of padding bytes added.
    /// </summary>
    /// <remarks>
    /// Data: FF FF FF FF FF FF FF FF FF
    /// PKCS7 padding: FF FF FF FF FF FF FF FF FF 07 07 07 07 07 07 07
    /// </remarks>
    PKCS7,

    /// <summary>
    /// The padding string consists of bytes set to zero.
    /// </summary>
    /// <remarks>
    /// Data: FF FF FF FF FF FF FF FF FF
    /// Zeros padding: FF FF FF FF FF FF FF FF FF 00 00 00 00 00 00 00
    /// </remarks>
    Zeros,

    /// <summary>
    /// The ANSIX923 padding string consists of a sequence of bytes filled with
    /// zeros before the length.
    /// </summary>
    /// <remarks>
    /// Data: FF FF FF FF FF FF FF FF FF
    /// X923 padding: FF FF FF FF FF FF FF FF FF 00 00 00 00 00 00 07
    /// </remarks>
    ANSIX923,

    /// <summary>
    /// The ISO10126 padding string consists of random data before the length.
    /// </summary>
    /// <remarks>
    /// Data: FF FF FF FF FF FF FF FF FF
    /// ISO10126 padding: FF FF FF FF FF FF FF FF FF 7D 2A 75 EF F8 EF 07
    /// </remarks>
    ISO10126
  );

  ISizeCollection = IEnumerable<Integer>;

  ///	<summary>
  ///	  Represents a series of bytes in memory.
  ///	</summary>
  ///	<remarks>
  ///	  The <c>TBuffer</c> structure is actually a wrapper of a value of
  ///	  <c>TBytes</c>and provides some easy-going methods and properties.
  ///	</remarks>
  TBuffer = record
  strict private
    fBytes: TBytes;
    function GetIsEmpty: Boolean;
    function GetMemory: PByte; inline;
    function GetSize: Integer; inline;
    function GetByteItem(const index: Integer): Byte;
    procedure SetSize(const value: Integer);
    procedure SetByteItem(const index: Integer; const value: Byte);
  private
    class var fEmpty: TBuffer;
  public
    constructor Create(size: Integer); overload;
    constructor Create(const buffer: Pointer; count: Integer); overload;
    constructor Create(const buffer: Pointer; startIndex, count: Integer); overload;
    constructor Create(const buffer: array of Byte); overload;
    constructor Create(const buffer: array of Byte; startIndex, count: Integer); overload;
    constructor Create(const buffer: array of Char); overload;
    constructor Create(const buffer: array of Char; startIndex, count: Integer); overload;
    constructor Create(const s: string); overload;
{$IFNDEF NEXTGEN}
    constructor Create(const s: WideString); overload;
    constructor Create(const s: RawByteString); overload;
{$ENDIF}
    constructor Create(stream: TStream); overload;

    class function FromHexString(const s: string): TBuffer; static;

    ///	<seealso cref="FromHexString(string)" />
    class function ConvertToHexString(const buffer: Pointer; count: Integer): string; overload; static;
    class function ConvertToHexString(const buffer: Pointer; count: Integer;
      const prefix: string; const delimiter: string = ' '): string; overload; static;

    class function BytesOf(const value: Byte; count: Integer): TBytes; static;
    class function GetByte(const buffer; const index: Integer): Byte; static;
    class procedure SetByte(var buffer; const index: Integer; const value: Byte); static;

    function Clone: TBuffer;
    function Copy(startIndex, count: Integer): TBytes;
    function Reverse: TBuffer;

    function Left(count: Integer): TBuffer;
    function Mid(startIndex, count: Integer): TBuffer;
    function Right(count: Integer): TBuffer;

    function First: Byte;
    function Last: Byte;

    function EnsureSize(size: Integer): TBuffer; overload;
    function EnsureSize(size: Integer; value: Byte): TBuffer; overload;
{$IFNDEF NEXTGEN}
    function EnsureSize(size: Integer; value: AnsiChar): TBuffer; overload;
{$ENDIF}

    function Equals(const buffer: TBuffer): Boolean; overload;
    function Equals(const buffer: array of Byte): Boolean; overload;
    function Equals(const buffer: Pointer; count: Integer): Boolean; overload;
    function Equals(const hexString: string): Boolean; overload;

    procedure LoadFromStream(stream: TStream);
    procedure SaveToStream(stream: TStream);

    function ToBytes: TBytes;
    function ToString: string;

    function ToHexString: string; overload;
    function ToHexString(const prefix: string; const delimiter: string = ' '): string; overload;

    property AsBytes: TBytes read fBytes;
    property IsEmpty: Boolean read GetIsEmpty;
    property Memory: PByte read GetMemory;
    property Size: Integer read GetSize write SetSize;
    property Bytes[const index: Integer]: Byte read GetByteItem write SetByteItem; default;

    class property Empty: TBuffer read fEmpty;

    { Operator Overloads }
    class operator Implicit(const value: TBytes): TBuffer;
    class operator Implicit(const value: TBuffer): TBytes;
    class operator Implicit(const value: TBuffer): PByte;
    class operator Explicit(const value: TBytes): TBuffer;
    class operator Explicit(const value: TBuffer): TBytes;
    class operator Explicit(const value: TBuffer): PByte;

    class operator Add(const left, right: TBuffer): TBuffer;
    class operator Add(const left: TBuffer; const right: Byte): TBuffer; overload;
    class operator Add(const left: Byte; const right: TBuffer): TBuffer; overload;

    class operator Equal(const left, right: TBuffer): Boolean;
    class operator NotEqual(const left, right: TBuffer): Boolean;

    class operator BitwiseAnd(const left, right: TBuffer): TBuffer;
    class operator BitwiseOr(const left, right: TBuffer): TBuffer;
    class operator BitwiseXor(const left, right: TBuffer): TBuffer;
  end;

  ///	<summary>
  ///	  Defines the basic operations of hash algorithms.
  ///	</summary>
  IHashAlgorithm = interface
    ['{D33C6DB1-7C51-4DDE-BB7D-ACE98BB61EBE}']
  {$REGION 'Property Getters and Setters'}
    function GetHashSize: Integer;
  {$ENDREGION}

    function ComputeHash(const buffer: array of Byte): TBuffer; overload;
    function ComputeHash(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function ComputeHash(const buffer: Pointer; count: Integer): TBuffer; overload;
    function ComputeHash(const inputString: string): TBuffer; overload;
{$IFNDEF NEXTGEN}
    function ComputeHash(const inputString: WideString): TBuffer; overload;
    function ComputeHash(const inputString: RawByteString): TBuffer; overload;
{$ENDIF}
    function ComputeHash(const inputStream: TStream): TBuffer; overload;  // experimental
    function ComputeHashOfFile(const fileName: string): TBuffer;  // callback?

    ///	<summary>
    ///	  Gets the hash size, <b>in bits</b>, of the algorithm.
    ///	</summary>
    property HashSize: Integer read GetHashSize;
  end;

  ///	<summary>
  ///	  IKeyedHashAlgorithm
  ///	</summary>
  ///	<remarks>
  ///	  From MSDN: A keyed hash algorithm is a key-dependent, one-way hash
  ///	  function used as a message authentication code. Only someone who knows
  ///	  the key can verify the hash. Keyed hash algorithms provide authenticity
  ///	  without secrecy.
  ///	</remarks>
  IKeyedHashAlgorithm = interface(IHashAlgorithm)
    ['{0D6838E7-05C0-4874-86B0-732DF42105F5}']
  {$REGION 'Property Getters and Setters'}
    function GetKey: TBuffer;
    procedure SetKey(const value: TBuffer);
  {$ENDREGION}

    ///	<summary>
    ///	  Gets or sets the key to use in the hash algorithm.
    ///	</summary>
    property Key: TBuffer read GetKey write SetKey;
  end;

  ///	<summary>
  ///	  Performs a transformation on data to keep it from being read by third
  ///	  parties. This type of encryption uses a single shared, secret key to
  ///	  encrypt and decrypt data.
  ///	</summary>
  ///	<seealso href="http://en.wikipedia.org/wiki/Cipher" />
  ///	<seealso href="http://en.wikipedia.org/wiki/Block_cipher_mode_of_operation" />
  ISymmetricAlgorithm = interface
    ['{98E0E218-2BD4-4AFA-87B2-E8C4812B2105}']
  {$REGION 'Property Getters and Setters'}
    function GetBlockSize: Integer;
    function GetKeySize: Integer;
    function GetLegalBlockSizes: ISizeCollection;
    function GetLegalKeySizes: ISizeCollection;
    function GetCipherMode: TCipherMode;
    function GetPaddingMode: TPaddingMode;
    function GetKey: TBuffer;
    function GetIV: TBuffer;
    procedure SetBlockSize(const value: Integer);
    procedure SetKeySize(const value: Integer);
    procedure SetCipherMode(const value: TCipherMode);
    procedure SetPaddingMode(const value: TPaddingMode);
    procedure SetKey(const value: TBuffer);
    procedure SetIV(const value: TBuffer);
  {$ENDREGION}

    function  Encrypt(const buffer: TBuffer): TBuffer; overload;
    function  Encrypt(const buffer: array of Byte): TBuffer; overload;
    function  Encrypt(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function  Encrypt(const buffer: Pointer; count: Integer): TBuffer; overload;
    function  Encrypt(const inputString: string): TBuffer; overload;
{$IFNDEF NEXTGEN}
    function  Encrypt(const inputString: WideString): TBuffer; overload;
    function  Encrypt(const inputString: RawByteString): TBuffer; overload;
{$ENDIF}
    procedure Encrypt(inputStream, outputStream: TStream); overload;  // experimental

    function  Decrypt(const buffer: TBuffer): TBuffer; overload;
    function  Decrypt(const buffer: array of Byte): TBuffer; overload;
    function  Decrypt(const buffer: array of Byte; startIndex, count: Integer): TBuffer; overload;
    function  Decrypt(const buffer: Pointer; count: Integer): TBuffer; overload;
    function  Decrypt(const inputString: string): TBuffer; overload;
{$IFNDEF NEXTGEN}
    function  Decrypt(const inputString: WideString): TBuffer; overload;
    function  Decrypt(const inputString: RawByteString): TBuffer; overload;
{$ENDIF}
    procedure Decrypt(inputStream, outputStream: TStream); overload; // experimental

    ///	<summary>
    ///	  Gets or sets the cipher mode for operation of the symmetric
    ///	  algorithm.
    ///	</summary>
    property CipherMode: TCipherMode read GetCipherMode write SetCipherMode;

    ///	<summary>
    ///	  Gets or sets the padding mode used in the symmetric algorithm.
    ///	</summary>
    property PaddingMode: TPaddingMode read GetPaddingMode write SetPaddingMode;

    ///	<summary>
    ///	  Gets or sets the secret key for the symmetric algorithm.
    ///	</summary>
    property Key: TBuffer read GetKey write SetKey;

    ///	<summary>
    ///	  Gets or sets the value of initialization vector.
    ///	</summary>
    property IV: TBuffer read GetIV write SetIV;

    ///	<summary>
    ///	  Gets or sets the block size, in bits, of the cryptographic operation.
    ///	</summary>
    property BlockSize: Integer read GetBlockSize write SetBlockSize;

    ///	<summary>
    ///	  Gets or sets the size, in bits, of the secret key used by the
    ///	  symmetric algorithm.
    ///	</summary>
    property KeySize: Integer read GetKeySize write SetKeySize;

    ///	<summary>
    ///	  Gets the block sizes, in bits, that are supported by the symmetric
    ///	  algorithm.
    ///	</summary>
    property LegalBlockSizes: ISizeCollection read GetLegalBlockSizes;

    ///	<summary>
    ///	  Gets the key sizes, in bits, that are supported by the symmetric
    ///	  algorithm.
    ///	</summary>
    property LegalKeySizes: ISizeCollection read GetLegalKeySizes;
  end;

  ///	<summary>
  ///	  Generates cryptographically strong random values.
  ///	</summary>
  IRandomNumberGenerator = interface
    ['{64B180B9-E192-4542-A45D-4E7402ED7BA8}']
    procedure GetBytes(var data: TBytes);
    procedure GetNonZeroBytes(var data: TBytes);
  end;


  {$REGION 'Hash Algorithms'}

  ICRC16 = interface(IHashAlgorithm)
    ['{7FEC815E-52E3-4C48-AAC1-7DEE905A6C1F}']
    function GetCrcValue: UInt16;
    property CrcValue: UInt16 read GetCrcValue;
  end;

  ICRC32 = interface(IHashAlgorithm)
    ['{96581075-EC4C-4C3F-A031-88FCD4D9F3EA}']
    function GetCrcValue: UInt32;
    property CrcValue: UInt32 read GetCrcValue;
  end;

  IMD5 = interface(IHashAlgorithm)
    ['{84E96BE7-0959-490D-9CF0-A62FEF72BFE7}']
  end;

  ISHA1 = interface(IHashAlgorithm)
    ['{FB202EDF-7846-4F3C-9088-D581CC8E1BC0}']
  end;

  ISHA256 = interface(IHashAlgorithm)
    ['{1DD90B12-CB33-44F2-A996-E7A0B5F0C541}']
  end;

  ISHA384 = interface(IHashAlgorithm)
    ['{B8CFF3B2-D319-4D21-89E5-5A1E0DB540B5}']
  end;

  ISHA512 = interface(IHashAlgorithm)
    ['{986B1C68-C156-46B8-A36C-822E7E5BC35E}']
  end;

  {$ENDREGION}


  {$REGION 'Symmetric Algorithms'}

  IDES = interface(ISymmetricAlgorithm)
    ['{2123E0C7-A747-49D4-A7CD-A2A9BC1A0042}']
  end;

  ITripleDES = interface(ISymmetricAlgorithm)
    ['{81D5101D-B3EA-437D-8A1A-80E74A9EDCDF}']
  end;

  {$ENDREGION}


  ECryptographicException = class(Exception);


{$REGION 'Routines'}

function CreateCRC16: ICRC16;

function CreateCRC32: ICRC32;

function CreateMD5: IMD5;

function CreateSHA1: ISHA1;

function CreateSHA256: ISHA256;

function CreateSHA384: ISHA384;

function CreateSHA512: ISHA512;

function CreateDES: IDES;

function CreateTripleDES: ITripleDES;

function CreateRandomNumberGenerator: IRandomNumberGenerator;

function CreateSizeCollection(value: Integer): ISizeCollection; overload;

function CreateSizeCollection(const values: array of Integer): ISizeCollection; overload;

{$ENDREGION}

const
  cmCBC = TCipherMode.CBC deprecated;
  cmECB = TCipherMode.ECB deprecated;
  pmNone = TPaddingMode.None deprecated;
  pmPKCS7 = TPaddingMode.PKCS7 deprecated;
  pmZeros = TPaddingMode.Zeros deprecated;
  pmANSIX923 = TPaddingMode.ANSIX923 deprecated;
  pmISO10126 = TPaddingMode.ISO10126 deprecated;

implementation

uses
  Spring.Cryptography.Base,
  Spring.Cryptography.CRC,
  Spring.Cryptography.MD5,
  Spring.Cryptography.DES,
  Spring.Cryptography.SHA,
  Spring.ResourceStrings;

{$REGION 'Routines'}

function CreateCRC16: ICRC16;
begin
  Result := TCRC16.Create;
end;

function CreateCRC32: ICRC32;
begin
  Result := TCRC32.Create;
end;

function CreateMD5: IMD5;
begin
  Result := TMD5.Create;
end;

function CreateSHA1: ISHA1;
begin
  Result := TSHA1.Create;
end;

function CreateSHA256: ISHA256;
begin
  Result := TSHA256.Create;
end;

function CreateSHA384: ISHA384;
begin
  Result := TSHA384.Create;
end;

function CreateSHA512: ISHA512;
begin
  Result := TSHA512.Create;
end;

function CreateDES: IDES;
begin
  Result := TDES.Create;
end;

function CreateTripleDES: ITripleDES;
begin
  Result := TTripleDES.Create;
end;

function CreateRandomNumberGenerator: IRandomNumberGenerator;
begin
  Result := TRandomNumberGenerator.Create;
end;

function CreateSizeCollection(value: Integer): ISizeCollection;
begin
  Result := CreateSizeCollection([value]);
end;

function CreateSizeCollection(const values: array of Integer): ISizeCollection;
var
  list: IList<Integer>;
begin
  list := TCollections.CreateList<Integer>;
  list.AddRange(values);
  Result := list;
end;

{$ENDREGION}


{$REGION 'TBuffer'}

constructor TBuffer.Create(size: Integer);
begin
  Guard.CheckRange(size >= 0, 'size');

  SetLength(fBytes, size);
end;

constructor TBuffer.Create(const buffer: Pointer; count: Integer);
begin
  Guard.CheckRange(count >= 0, 'count');

  SetLength(fBytes, count);
  Move(buffer^, fBytes[0], count);
end;

constructor TBuffer.Create(const buffer: Pointer; startIndex, count: Integer);
begin
  Guard.CheckRange(startIndex >= 0, 'startIndex');
  Guard.CheckRange(count >= 0, 'count');

  SetLength(fBytes, count);
  Move(PByte(buffer)[startIndex], fBytes[0], count);
end;

constructor TBuffer.Create(const buffer: array of Byte);
begin
  Create(@buffer[0], Length(buffer));
end;

constructor TBuffer.Create(const buffer: array of Byte; startIndex, count: Integer);
begin
  Guard.CheckRange(buffer, startIndex, count);

  Create(@buffer[startIndex], count);
end;

constructor TBuffer.Create(const s: string);
begin
  Create(PByte(s), Length(s) * SizeOf(Char));
end;

{$IFNDEF NEXTGEN}
constructor TBuffer.Create(const s: WideString);
begin
  Create(PByte(s), Length(s) * SizeOf(Char));
end;

constructor TBuffer.Create(const s: RawByteString);
begin
  Create(PByte(s), Length(s));
end;
{$ENDIF}

constructor TBuffer.Create(const buffer: array of Char);
begin
  Create(@buffer[0], Length(buffer) * SizeOf(Char));
end;

constructor TBuffer.Create(const buffer: array of Char; startIndex, count: Integer);
begin
  Guard.CheckRange(buffer, startIndex, count);

  Create(@buffer[startIndex], count * SizeOf(Char));
end;

constructor TBuffer.Create(stream: TStream);
begin
  LoadFromStream(stream);
end;

class function TBuffer.BytesOf(const value: Byte; count: Integer): TBytes;
begin
  Guard.CheckRange(count >= 0, 'count');

  SetLength(Result, count);
  FillChar(Result[0], count, value);
end;

class function TBuffer.GetByte(const buffer; const index: Integer): Byte;
begin
  Guard.CheckRange(index >= 0, 'index');

  Result := PByte(@buffer)[index];
end;

procedure TBuffer.LoadFromStream(stream: TStream);
begin
  Guard.CheckNotNull(stream, 'stream');

  SetLength(fBytes, stream.Size - stream.Position);
  stream.ReadBuffer(fBytes[0], Length(fBytes));
end;

procedure TBuffer.SaveToStream(stream: TStream);
begin
  Guard.CheckNotNull(stream, 'stream');

  stream.WriteBuffer(fBytes[0], Length(fBytes));
end;

class procedure TBuffer.SetByte(var buffer; const index: Integer;
  const value: Byte);
begin
  Guard.CheckRange(index >= 0, 'index');

  PByte(@buffer)[index] := value;
end;

class function TBuffer.FromHexString(const s: string): TBuffer;
var
  buffer: string;
{$IFNDEF NEXTGEN}
  text: string;
{$ELSE}
  text: TBytes;
{$ENDIF}
  bytes: TBytes;
  index: Integer;
  i: Integer;
const
  HexCharSet: TSysCharSet = ['0'..'9', 'a'..'f', 'A'..'F'];
begin
  buffer := StringReplace(s, '0x', '', [rfIgnoreCase, rfReplaceAll]);
  SetLength(text, Length(buffer));
  index := 0;
  for i := 1 to Length(buffer) do
  begin
    if CharInSet(buffer[i], HexCharSet) then
    begin
      Inc(index);
{$IFNDEF NEXTGEN}
      text[index] := buffer[i];
{$ELSE}
      text[index - 1] := Ord(buffer[i]);
{$ENDIF}
    end;
  end;
  SetLength(bytes, index div 2);
{$IFNDEF NEXTGEN}
  Classes.HexToBin(PChar(text), PByte(bytes), Length(bytes));
{$ELSE}
  Classes.HexToBin(text, 0, bytes, 0, Length(bytes));
{$ENDIF}
  Result := TBuffer.Create(bytes);
end;

class function TBuffer.ConvertToHexString(const buffer: Pointer;
  count: Integer): string;
{$IFNDEF NEXTGEN}
begin
  SetLength(Result, count * 2);
  Classes.BinToHex(buffer, PChar(Result), count);
end;
{$ELSE}
var
  buff: TBytes;
  text: TBytes;
  i: Integer;
begin
  if (count = 0) then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  SetLength(buff, count);
  Move(buffer^, buff[0], count);
  SetLength(text, count * 2);
  Classes.BinToHex(text, 0, buff, 0, count);
  SetLength(Result, count * 2);
  for i := 1 to Length(Result) do
    Result[i]:=Char(text[i - 1]);
end;
{$ENDIF}

class function TBuffer.ConvertToHexString(const buffer: Pointer; count: Integer;
  const prefix, delimiter: string): string;
const
  Convert: array[0..15] of Char = '0123456789ABCDEF';
var
  p: PByte;
  stringBuilder: TStringBuilder;
  captacity: Integer;
  text: array[0..1] of Char;
  i: Integer;
begin
  if count = 0 then Exit('');
  p := buffer;
  captacity := (Length(prefix) + 2 + Length(delimiter)) * count;
  stringBuilder := TStringBuilder.Create(captacity);
  try
    stringBuilder.Append(prefix);
    text[0] := Convert[p[0] shr 4];
    text[1] := Convert[p[0] and $0F];
    stringBuilder.Append(text);
    for i := 1 to count - 1 do
    begin
      stringBuilder.Append(delimiter);
      stringBuilder.Append(prefix);
      text[0] := Convert[p[i] shr 4];
      text[1] := Convert[p[i] and $0F];
      stringBuilder.Append(text);
    end;
    Result := stringBuilder.ToString;
  finally
    stringBuilder.Free;
  end;
end;

function TBuffer.Clone: TBuffer;
begin
  Result := ToBytes;
end;

function TBuffer.Reverse: TBuffer;
var
  i: Integer;
  p: PByte;
begin
  SetLength(Result.fBytes, Size);
  p := @Result.fBytes[Size - 1];
  for i := 0 to Size - 1 do
  begin
    p^ := fBytes[i];
    Dec(p);
  end;
end;

function TBuffer.Copy(startIndex, count: Integer): TBytes;
begin
  Guard.CheckRange(fBytes, startIndex, count);

  SetLength(Result, count);
  Move(fBytes[startIndex], Result[0], count);
end;

function TBuffer.First: Byte;
begin
  Result := Self[0];
end;

function TBuffer.Last: Byte;
begin
  Result := Self[Size-1];
end;

function TBuffer.Left(count: Integer): TBuffer;
begin
  Guard.CheckRange((count >= 0) and (count <= Size), 'count');

  Result := Mid(0, count);
end;

function TBuffer.Mid(startIndex, count: Integer): TBuffer;
begin
  Result := Self.Copy(startIndex, count);
end;

function TBuffer.Right(count: Integer): TBuffer;
begin
  Guard.CheckRange((count >= 0) and (count <= Size), 'count');

  Result := Mid(Size - count, count);
end;

function TBuffer.EnsureSize(size: Integer): TBuffer;
begin
  Result := Self.EnsureSize(size, 0);
end;

function TBuffer.EnsureSize(size: Integer; value: Byte): TBuffer;
var
  data: TBytes;
begin
  if Self.Size < size then
  begin
    SetLength(data, size);
    Move(fBytes[0], data[0], Self.Size);
    FillChar(data[Self.Size], size - Self.Size, value);
  end
  else
  begin
    data := Self.ToBytes;
  end;
  Result := data;
end;

{$IFNDEF NEXTGEN}
function TBuffer.EnsureSize(size: Integer; value: AnsiChar): TBuffer;
begin
  Result := Self.EnsureSize(size, Byte(value));
end;
{$ENDIF}

function TBuffer.Equals(const buffer: TBuffer): Boolean;
begin
  Result := Equals(buffer.fBytes);
end;

function TBuffer.Equals(const buffer: array of Byte): Boolean;
begin
  Result := (Size = Length(buffer)) and
    ((Size = 0) or CompareMem(Memory, @buffer[0], Size));
end;

function TBuffer.Equals(const buffer: Pointer; count: Integer): Boolean;
begin
  Guard.CheckRange(count >= 0, 'count');

  Result := (count = Self.Size) and CompareMem(Self.Memory, buffer, count);
end;

function TBuffer.Equals(const hexString: string): Boolean;
var
  buffer: TBuffer;
begin
  buffer := TBuffer.FromHexString(hexString);
  Result := Equals(buffer);
end;

function TBuffer.ToString: string;
begin
  Result := TBuffer.ConvertToHexString(Memory, Size);
end;

function TBuffer.ToBytes: TBytes;
begin
  Result := fBytes;
end;

function TBuffer.ToHexString: string;
begin
  Result := TBuffer.ConvertToHexString(Memory, Size);
end;

function TBuffer.ToHexString(const prefix: string; const delimiter: string): string;
begin
  Result := TBuffer.ConvertToHexString(Memory, Size, prefix, delimiter);
end;

function TBuffer.GetSize: Integer;
begin
  Result := Length(fBytes);
end;

function TBuffer.GetIsEmpty: Boolean;
begin
  Result := Length(fBytes) = 0;
end;

function TBuffer.GetMemory: PByte;
begin
  Result := PByte(fBytes);
end;

function TBuffer.GetByteItem(const index: Integer): Byte;
begin
  Guard.CheckRange((index >= 0) and (index < Size), 'index');

  Result := fBytes[index];
end;

procedure TBuffer.SetByteItem(const index: Integer; const value: Byte);
begin
  Guard.CheckRange((index >= 0) and (index < Size), 'index');

  fBytes[index] := value;
end;

procedure TBuffer.SetSize(const value: Integer);
begin
  SetLength(fBytes, value);
end;

class operator TBuffer.Implicit(const value: TBytes): TBuffer;
begin
  Result.fBytes := value;
end;

class operator TBuffer.Implicit(const value: TBuffer): TBytes;
begin
  Result := value.fBytes;
end;

class operator TBuffer.Explicit(const value: TBuffer): PByte;
begin
  Result := PByte(value.fBytes);
end;

class operator TBuffer.Explicit(const value: TBytes): TBuffer;
begin
  Result.fBytes := value;
end;

class operator TBuffer.Explicit(const value: TBuffer): TBytes;
begin
  Result := value.fBytes;
end;

class operator TBuffer.Implicit(const value: TBuffer): PByte;
begin
  Result := PByte(value.fBytes);
end;

class operator TBuffer.Add(const left, right: TBuffer): TBuffer;
begin
  SetLength(Result.fBytes, left.Size + right.Size);
  Move(left.fBytes[0], Result.fBytes[0], left.Size);
  Move(right.fBytes[0], Result.fBytes[left.Size], right.Size);
end;

class operator TBuffer.Add(const left: TBuffer; const right: Byte): TBuffer;
begin
  Result.Size := left.Size + 1;
  Move(left.Memory^, Result.Memory^, left.Size);
  Result[Result.Size-1] := right;
end;

class operator TBuffer.Add(const left: Byte; const right: TBuffer): TBuffer;
begin
  Result.Size := right.Size + 1;
  Move(right.Memory^, Result.Memory[1], right.Size);
  Result[0] := left;
end;

class operator TBuffer.Equal(const left, right: TBuffer): Boolean;
begin
  Result := left.Equals(right);
end;

class operator TBuffer.NotEqual(const left, right: TBuffer): Boolean;
begin
  Result := not left.Equals(right);
end;

class operator TBuffer.BitwiseAnd(const left, right: TBuffer): TBuffer;
var
  i: Integer;
begin
  if left.Size <> right.Size then
  begin
    raise EInvalidOperationException.CreateRes(@SInvalidOperationBufferSizeShouldBeSame);
  end;
  Result.Size := left.Size;
  for i := 0 to Result.Size - 1 do
  begin
    Result[i] := left[i] and right[i];
  end;
end;

class operator TBuffer.BitwiseOr(const left, right: TBuffer): TBuffer;
var
  i: Integer;
begin
  if left.Size <> right.Size then
  begin
    raise EInvalidOperationException.CreateRes(@SInvalidOperationBufferSizeShouldBeSame);
  end;
  Result.Size := left.Size;
  for i := 0 to Result.Size - 1 do
  begin
    Result[i] := left[i] or right[i];
  end;
end;

class operator TBuffer.BitwiseXor(const left, right: TBuffer): TBuffer;
var
  i: Integer;
begin
  if left.Size <> right.Size then
  begin
    raise EInvalidOperationException.CreateRes(@SInvalidOperationBufferSizeShouldBeSame);
  end;
  Result.Size := left.Size;
  for i := 0 to Result.Size - 1 do
  begin
    Result[i] := left[i] xor right[i];
  end;
end;

{$ENDREGION}

end.
