{ *********************************************************** }
{ *                    ksTools Library                      * }
{ *       Copyright (c) Sergey Kasandrov 1997, 2010         * }
{ *       -----------------------------------------         * }
{ *         http://sergworks.wordpress.com/kstools          * }
{ *********************************************************** }

unit ksCompress;

interface

uses SysUtils, Classes, ksUtils, ksClasses, ksShrink;

{ Shrink Algorithm }

function ShrinkBytes(const Source: TBytes; Count: LongWord = 0): TBytes;
function UnshrinkBytes(const Source: TBytes; Count: LongWord): TBytes;

procedure ShrinkStream(Source, Target: TStream; Count: LongWord);
procedure UnshrinkStream(Source, Target: TStream; Count: LongWord);

procedure ShrinkFile(const Source, Target: string; Count: LongWord = 0);
procedure UnshrinkFile(const Source, Target: string; Count: LongWord);

implementation

function ShrinkBytes(const Source: TBytes; Count: LongWord): TBytes;
var
  SourceStream,
  TargetStream: TksBytesStream;

begin
  if Count = 0 then Count:= Length(Source);
  SourceStream:= TksBytesStream.Create(Source);
  try
    TargetStream:= TksBytesStream.Create;
    try
      ShrinkStream(SourceStream, TargetStream, Count);
      Result:= TargetStream.Bytes;
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

function UnshrinkBytes(const Source: TBytes; Count: LongWord): TBytes;
var
  SourceStream,
  TargetStream: TksBytesStream;

begin
  SourceStream:= TksBytesStream.Create(Source);
  try
    TargetStream:= TksBytesStream.Create;
    try
      UnshrinkStream(SourceStream, TargetStream, Count);
      Result:= Copy(TargetStream.Bytes);
    finally
      TargetStream.Free;
    end;
  finally
    SourceStream.Free;
  end;
end;

procedure ShrinkStream(Source, Target: TStream; Count: LongWord);
var
  Reader: TksReader;
  Writer: TksWriter;

begin
  Reader:= TksReader.Create(Source, 16 * 1024);
  try
    Writer:= TksWriter.Create(Target, 16 * 1024);
    try
      Shrink(Reader.ReadByte, Writer.WriteBits, Count);
    finally
      Writer.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure UnshrinkStream(Source, Target: TStream; Count: LongWord);
var
  Reader: TksReader;
  Writer: TksWriter;

begin
  Reader:= TksReader.Create(Source, 16 * 1024);
  try
    Writer:= TksWriter.Create(Target, 16 * 1024);
    try
      Unshrink(Reader.ReadBits, Writer.WriteByte, Count);
    finally
      Writer.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure ShrinkFile(const Source, Target: string; Count: LongWord);
var
  Src, Tgt: TFileStream;

begin
  Src:= TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    if Count = 0 then Count:= Src.Size;
    Tgt:= TFileStream.Create(Target, fmCreate);
    try
      ShrinkStream(Src, Tgt, Count);
    finally
      Tgt.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure UnshrinkFile(const Source, Target: string; Count: LongWord);
var
  Src, Tgt: TFileStream;

begin
  Src:= TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    Tgt:= TFileStream.Create(Target, fmCreate);
    try
      UnshrinkStream(Src, Tgt, Count);
    finally
      Tgt.Free;
    end;
  finally
    Src.Free;
  end;
end;

end.
