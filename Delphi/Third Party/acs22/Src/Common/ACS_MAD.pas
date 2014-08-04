(*
  This file is a part of Audio Components Suite v 2.2.
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_MAD;

interface

uses

  ACS_Types, Classes, SysUtils, Math, MAD;

type

  TRawPCMWaveHeader = record
    RIFF: array [0..3] of Char;
    FileSize: Integer;
    RIFFType: array [0..3] of Char;
    FmtChunkId: array [0..3] of Char;
    FmtChunkSize: Integer;
    FormatTag: Word;
    Channels: Word;
    SampleRate: Integer;
    BytesPerSecond: Integer;
    BlockAlign: Word;
    BitsPerSample: Word;
    DataChunkId: array [0..3] of Char;
    DataSize: Integer;
  end;

  TMADProgressEvent = procedure(Sender : TComponent) of object;
  TMADDoneEvent = procedure(Sender : TComponent; Success : Boolean) of object;

  TMADThread = class(TThread)
  private
    _Free : Boolean;
    Progr : Integer;
    Owner : TComponent;
    FDecoder :  mad_decoder;
    FInputStream : TStream;
    FOutputStream : TStream;
    HasFirstFrame : Boolean;
    FSR : Integer;
    FChan : Integer;
    FBitrate : Integer;
    FValid : Boolean;
    Data : PBuffer8;
    InputDone : Boolean;
    WaveHdr : TRawPCMWaveHeader;
    FSize : Integer;
    FMADProgress : TMADProgressEvent;
    FMADDone : TMADDoneEvent;
    WhenDone : procedure of object;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner : TComponent; InputStream, OutputStream : TStream);
    destructor Destroy; override;
  end;

  TMP3ToWav = class(TComponent)
  private
    Thread : TMADThread;
    FMADProgress : TMADProgressEvent;
    FMADDone : TMADDoneEvent;
    FInputStreamAssigned, FOutputStreamAssigned : Boolean;
    FInputStream, FOutputStream : TStream;
    FInputFile, FOutputFile : TFileName;
//    FBuisy : Boolean;
    function GetProgress : Integer;
    function GetBuisy : Boolean;
    procedure SetInputStream(aStream : TStream);
    procedure SetOutputStream(aStream : TStream);
    procedure WhenDone;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Run;
    procedure Stop;
    property Buisy : Boolean read GetBuisy;
    property InputStream : TStream read FInputStream write SetInputStream;
    property OutputStream : TStream read FOutputStream write SetOutputStream;
    property Progress : Integer read GetProgress;
  published
    property InputFile : TFileName read FInputFile write FInputFile;
    property OutputFile : TFileName read FOutputFile write FOutputFile;
    property OnDone : TMADDoneEvent read FMADDone write FMADDone;
    property OnProgress : TMADProgressEvent read FMADProgress write FMADProgress;
  end;

implementation

   function InputFunc(CData : Pointer; Stream : p_mad_stream) : Integer; cdecl;
   var
     MT : TMADThread;
     Len : Integer;
   begin

     MT := TMADThread(CData);

     if MT.InputDone then
     begin
       Result := MAD_FLOW_STOP;
       Exit;
     end;
     MT.InputDone := True;
//     Len := MT.FInputStream.Read(Data^, MT.FInputStream.Size);
     Len := MT.FInputStream.Size;
     mad_stream_buffer(Stream, MT.Data, Len);
     if not MT.Terminated then Result := MAD_FLOW_CONTINUE
     else Result := MAD_FLOW_STOP;
   end;

   function OutputFunc(CData : Pointer; Header : p_mad_header; pcm : p_mad_pcm) : Integer; cdecl;
   var
     MT : TMADThread;
     i, framesize : Integer;
     outsamples : array[0..2303] of SmallInt;
     text : array[0..4] of Char;
     CProgr : Integer;
   begin
     MT := TMADThread(CData);
     if not MT.HasFirstFrame then
     begin
       MT.FSR := pcm.samplerate;
       MT.FChan := pcm.channels;
       MT.FBitrate := Header.bitrate;
       framesize := Ceil(144*MT.FBitrate/MT.FSR);
       MT.FSize := Round(MT.FInputStream.Size/framesize*1152)*MT.FChan*2;
       MT.FValid := True;
       text := 'RIFF';
       Move(text[0], MT.WaveHdr.RIFF[0], 4);
       MT.WaveHdr.FileSize := MT.FSize + 44;
       text := 'WAVE';
       Move(text[0], MT.WaveHdr.RIFFType[0], 4);
       text := 'fmt ';
       Move(text[0], MT.WaveHdr.FmtChunkId[0], 4);
       MT.WaveHdr.FmtChunkSize := 16;
       MT.WaveHdr.FormatTag := 1;
       MT.WaveHdr.Channels := MT.FChan;
       MT.WaveHdr.SampleRate := MT.FSR;
       MT.WaveHdr.BitsPerSample := 16;
       MT.WaveHdr.BlockAlign := 2*MT.FChan;
       MT.WaveHdr.BytesPerSecond := MT.FSR * MT.WaveHdr.BlockAlign;
       text := 'data';
       Move(text[0], MT.WaveHdr.DataChunkId[0], 4);
       MT.WaveHdr.DataSize := MT.FSize;
       if MT.FOutputStream is TMemoryStream then
       begin
         MT.FOutputStream.Size :=MT.FSize + 44;
         MT.FOutputStream.Seek(0, soFromBeginning);
       end;
       MT.FOutputStream.Write(MT.WaveHdr, 44);
       MT.HasFirstFrame := True;
     end;
     if pcm.channels = 2 then
     begin
       for i := 0 to pcm.length -1 do
       begin
         if pcm.samples[0][i] >= MAD_F_ONE then
         pcm.samples[0][i] := MAD_F_ONE - 1;
         if pcm.samples[0][i] < -MAD_F_ONE then
         pcm.samples[0][i] := -MAD_F_ONE;
         pcm.samples[0][i] := pcm.samples[0][i] shr (MAD_F_FRACBITS + 1 - 16);
         outsamples[i shl 1] := pcm.samples[0][i];
         if pcm.samples[1][i] >= MAD_F_ONE then
         pcm.samples[1][i] := MAD_F_ONE - 1;
         if pcm.samples[1][i] < -MAD_F_ONE then
         pcm.samples[1][i] := -MAD_F_ONE;
         pcm.samples[1][i] := pcm.samples[1][i] shr (MAD_F_FRACBITS + 1 - 16);
         outsamples[(i shl 1)+1] := pcm.samples[1][i];
       end;
       MT.FOutputStream.Write(outsamples[0], pcm.length*4);
     end else
     begin
       for i := 0 to pcm.length -1 do
       begin
         if pcm.samples[0][i] >= MAD_F_ONE then
         pcm.samples[0][i] := MAD_F_ONE - 1;
         if pcm.samples[0][i] < -MAD_F_ONE then
         pcm.samples[0][i] := -MAD_F_ONE;
         pcm.samples[0][i] := pcm.samples[0][i] shr (MAD_F_FRACBITS + 1 - 16);
         outsamples[i] := pcm.samples[0][i];
       end;
       MT.FOutputStream.Write(outsamples[0], pcm.length*2);
     end;
     if MT.FSize <> 0 then
     begin
       CProgr := Round(MT.FOutputStream.Position/MT.FSize*100);
       if MT.Progr <> CProgr then
       begin
         MT.Progr := CProgr;
         if Assigned(MT.FMADProgress) then
         MT.FMADProgress(MT.Owner);
       end;
     end;
     if not MT.Terminated then Result := MAD_FLOW_CONTINUE
     else Result := MAD_FLOW_STOP;
   end;

   function ErrorFunc(CData : Pointer; Stream : p_mad_stream; Frame : p_mad_frame) : Integer; cdecl;
   begin
     Result := MAD_FLOW_CONTINUE;
   end;

   constructor TMADThread.Create;
   begin
     inherited Create(True);
     Owner := AOwner;
     FInputStream := InputStream;
     FOutputStream := OutputStream;
     FreeOnTerminate := False;
   end;

   destructor TMADThread.Destroy;
   begin
     if not _Free then
     begin
       Terminate;
       {$IFDEF WIN32}
       while not _Free do;
       {$ENDIF}
     end;
     inherited Destroy;
   end;

   procedure TMADThread.Execute;
   begin
     try
       GetMem(Data, FInputStream.Size);
       FInputStream.Read(Data[0], FInputStream.Size);
       mad_decoder_init(@FDecoder, Self, InputFunc, nil, nil, OutputFunc, ErrorFunc, nil);
       mad_decoder_run(@FDecoder, MAD_DECODER_MODE_SYNC);
       mad_decoder_finish(@FDecoder);
       FreeMem(Data);
       WhenDone;
       if Assigned(FMADDone) then FMADDone(Owner, FValid);
       _Free := True;
     except
       FreeMem(Data);
       WhenDone;
       _Free := True;
     end;
   end;

   constructor TMP3ToWav.Create;
   begin
     inherited Create(AOwner);
     if not (csDesigning in ComponentState) then
     if not MADLibLoaded then
     raise Exception.Create(MADLibPath + ' library could not be loaded.');
   end;

   destructor TMP3ToWav.Destroy;
   begin
     if Assigned(Thread) then Thread.Free;
     inherited Destroy;
   end;

   procedure TMP3ToWav.SetInputStream;
   begin
     if AStream = nil then FInputStreamAssigned := False
     else FInputStreamAssigned := True;
     FInputStream := AStream;
   end;

   procedure TMP3ToWav.SetOutputStream;
   begin
     if AStream = nil then FOutputStreamAssigned := False
     else FOutputStreamAssigned := True;
     FOutputStream := AStream;
   end;

   function TMP3ToWav.GetProgress;
   begin
     if Assigned(Thread) then
     if not Thread._Free then
     Result := Thread.Progr;
   end;

   function TMP3ToWav.GetBuisy;
   begin
     if Assigned(Thread) then
     begin
       if not Thread._Free then Result := True
       else Result := False;
     end else Result := False;
   end;

   procedure TMP3ToWav.WhenDone;
   begin
     if not FInputStreamAssigned then FinputStream.Free;
     if not FOutputStreamAssigned then FOutputStream.Free;
   end;

   procedure TMP3ToWav.Run;
   begin
     if Assigned(Thread) then
     begin
       if not Thread._Free then
       raise Exception.Create('Component is buisy.')
       else Thread.Free;
     end;
     if not FInputStreamAssigned then
     begin
       if FInputFile = '' then
       raise Exception.Create('Neither input stream not file name is assigned.');
       FInputStream := TFileStream.Create(FInputFile, fmOpenRead);
     end;
     if not FOutputStreamAssigned then
     begin
       if FOutputFile = '' then
       begin
         if not FInputStreamAssigned then FInputStream.Free;
         raise Exception.Create('Neither output stream not file name is assigned.');
       end;
       FOutputStream := TFileStream.Create(FOutputFile, fmCreate);
     end;
     Thread := TMADThread.Create(Self, FInputStream, FOutputStream);
     Thread.FMADProgress := FMADProgress;
     Thread.FMADDone := FMADDone;
     Thread.WhenDone := WhenDone;
     Thread.Resume;
   end;

   procedure TMP3ToWav.Stop;
   begin
     if Assigned(Thread) then
     Thread.Terminate;
   end;

end.
