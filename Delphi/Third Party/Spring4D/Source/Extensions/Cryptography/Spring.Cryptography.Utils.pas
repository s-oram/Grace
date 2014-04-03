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

unit Spring.Cryptography.Utils;

{$I Spring.inc}

interface

uses
  Classes,
  SysUtils;

function Endian(x: LongWord): LongWord;
function Endian64(x: Int64): Int64;
function Rol(x: LongWord; y: Byte): LongWord;
function Ror(x: LongWord; y: Byte): LongWord;
function Ror64(x: Int64; y: Byte): Int64;

implementation

function Endian(x: LongWord): LongWord;
{$IFDEF CPUX86}
asm
  bswap eax
end;
{$ELSE}
begin
  x := (x shr 16) + (x shl 16);
  Result := ((x shl 8) and $FF00FF00) + ((x shr 8) and $00FF00FF);
end;
{$ENDIF}

function Endian64(x: Int64): Int64;
begin
  Result := (x and $00000000000000FF) shl 56;
  Result := Result + (x and $000000000000FF00) shl 40;
  Result := Result + (x and $0000000000FF0000) shl 24;
  Result := Result + (x and $00000000FF000000) shl 8;
  Result := Result + (x and $000000FF00000000) shr 8;
  Result := Result + (x and $0000FF0000000000) shr 24;
  Result := Result + (x and $00FF000000000000) shr 40;
  Result := Result + (x and $FF00000000000000) shr 56;
end;

function Rol(x: LongWord; y: Byte): LongWord;
{$IFDEF CPUX86}
asm
  mov   cl,dl
  Rol   eax,cl
end;
{$ELSE}
begin
  Result := (x shl y) OR (x shr (32 - y));
end;
{$ENDIF}

function Ror(x: LongWord; y: Byte): LongWord;
{$IFDEF CPUX86}
asm
  mov   cl,dl
  Ror   eax,cl
end;
{$ELSE}
begin
  Result := (x shr y) OR (x shl (32 - y));
end;
{$ENDIF}

function Ror64(x: Int64; y: Byte): Int64;
begin
  Result := (x shr y) or (x shl (64 - y));
end;

end.
