(*
  This file is a part of New Audio Components package v 2.2
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: DMXStreams.pas 1167 2010-02-01 14:54:09Z andrei.borovsky $ *)

unit DMXStreams;

interface

uses
  Windows, Classes, SysUtils, math, ACS_Classes, ACS_Procs, ACS_Types;

type

  TAC3VOBStream = (acvStreamFirst, acvStreamSecond);

  TAuVOBAC3Demuxer = class(TAuFileStream)
  private
    Block : PBuffer8;
    InBuff : array [0..2047] of Byte;
    DataSize : LongWord;
    FStream : Byte;
    function IsAudioPacket : Boolean;
    function IsAc3AudioStream(StreamID : Byte) : Boolean;
    procedure ReadBlock;
  public
    procedure Init(Stream : TAC3VOBStream = acvStreamFirst);
 //   function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

(*  TAuAOBDemuxer = class(TAuFileStream)
  private
    F : TFileStream;
    Block : PBuffer8;
    InBuff : array [0..2047] of Byte;
    DataSize : LongWord;
    FStream : Byte;
    FAudioDataSize : Word;
    FSampleRate : LongWord;
    FBitsPerSample : Word;
    FChannels : Word;
    FFrameSize : Word;
    FOffset : Word;
    FSamplesCount : Word;
    OutBuf : array[0..4095] of Byte;
    function IsAudioPacket : Boolean;
    function IsAudioPES(Ptr : PLongWord) : Boolean;
    procedure ParseLPCMHeader(Offset : Integer);
    procedure ReadBlock;
  public
    procedure Init(Stream : TAC3VOBStream = acvStreamFirst);
    function Read(var Buffer; Count: Longint): Longint; override;
    property Channels : Word read FChannels;
    property SampleRate : LongWord read FSampleRate;
    property BitsPerSample : Word read FBitsPerSample;
  end; *)



implementation

const
  SECTOR_SIZE = 2048;
  AC3_PACK_HEADER_LENGTH = 14;
  AC3_PRIVATE_DATA_LENGTH = 4;
  AC3_FIRST_STREAM =	$80;
  AC3_SECOND_STREAM	= $81;

  function TAuVOBAC3Demuxer.IsAudioPacket : Boolean;
  begin
    Result := (PLongWord(@InBuff[0])^ = $BA010000) and  (PLongWord(@InBuff[AC3_PACK_HEADER_LENGTH])^ = $BD010000);
  end;

  function TAuVOBAC3Demuxer.IsAc3AudioStream(StreamID : Byte) : Boolean;
  begin
    Result := (StreamID = FStream);
  end;

   procedure TAuVOBAC3Demuxer.Init(Stream : TAC3VOBStream = acvStreamFirst);
   begin
     DataSize := 0;
     if Stream = acvStreamFirst then
       FStream := AC3_FIRST_STREAM
     else
       FStream := AC3_SECOND_STREAM;
   end;

   procedure TAuVOBAC3Demuxer.ReadBlock;
   var
     DataStart, AudioDataStart : Integer;
     Modulo : Integer;
   begin
     while Position < Size do
     begin
       Modulo := Position mod 2048;
       if Modulo > 0 then
       begin
         Position := Position - Modulo ;
         inherited Read(InBuff[0], 2048);
         if not IsAudioPacket then
         begin
          inherited Read(InBuff[0], 2048);
         end;
       end else
         inherited Read(InBuff[0], 2048);
       if not IsAudioPacket then
         continue;
    	 DataStart := AC3_PACK_HEADER_LENGTH + InBuff[AC3_PACK_HEADER_LENGTH + 8]+9;
       if not IsAc3AudioStream(InBuff[DataStart]) then
         continue;
    	 AudioDataStart  := DataStart + AC3_PRIVATE_DATA_LENGTH;
       Block := @InBuff[AudioDataStart];    //FastCopyMem(@Block[0], @InBuff[AudioDataStart], SECTOR_SIZE-AudioDataStart);
       DataSize := SECTOR_SIZE-AudioDataStart;
       Break;
     end;
   end;

   function TAuVOBAC3Demuxer.Read(var Buffer; Count: Longint): Longint;
   var
     C : LongInt;
     b : array of Byte;
   begin
     Result := 0;
     C := Count;
     SetLength(b, C);
     while (Position < Size) do
     begin
       while C > 0 do
       begin
         {$WARNINGS OFF}
         if DataSize >= C then
         {$WARNINGS ON}
         begin
           Move(Block[0], b[Count - C], C);
           {$WARNINGS OFF}
           DataSize := DataSize - C;
           {$WARNINGS ON}
           Move(Block[C], Block[0], DataSize);
           Move(b[0], Buffer, Count);
           Result := Count;
           C := 0;
         end else
         begin
           Move(Block[0], b[Count - C], DataSize);

           {$WARNINGS OFF}
           C := C - DataSize;
           {$WARNINGS ON}
           ReadBlock;
           if DataSize = 0 then
           begin
             Result := 0;
             Exit;
           end;
         end;
       end;
       Break;
     end;
   end;

(*   function TAuVOBAC3Demuxer.Seek(const Offset: Int64; Origin: TSeekOrigin) : Int64;
   var
     i : Integer;
   begin
     for i := 0 to 2045 do
       if PWord(@InBuff[i])^ = $770B then
       begin
         FastCopyMem(@Block[0], @InBuff[i], 2047 - i);
         DataSize := DataSize - i;
         Break;
       end;
   end;*)

(*     function TAuAOBDemuxer.IsAudioPacket : Boolean;
  begin
    Result := (PLongWord(@InBuff[0])^ = $BA010000) and  (PLongWord(@InBuff[AC3_PACK_HEADER_LENGTH])^ = $BB010000);
  end;

  function TAuAOBDemuxer.IsAudioPES(Ptr : PLongWord) : Boolean;
  begin
    Result := (Ptr^ = $BD010000);
  end;

   procedure TAuAOBDemuxer.Init(Stream : TAC3VOBStream = acvStreamFirst);
   begin
     DataSize := 0;
     if Stream = acvStreamFirst then
       FStream := AC3_FIRST_STREAM
     else
       FStream := AC3_SECOND_STREAM;
   end;

   procedure TAuAOBDemuxer.ReadBlock;
   var
     DataStart, AudioDataStart : Integer;
     Modulo, i : Integer;
   begin
     while Position < Size do
     begin
       Modulo := Position mod 2048;
       if Modulo > 0 then
       begin
         Position := Position - Modulo ;
         inherited Read(InBuff[0], 2048);
         if not IsAudioPacket then
         begin
          inherited Read(InBuff[0], 2048);
         end;
       end else
         inherited Read(InBuff[0], 2048);
       if not IsAudioPacket then
         continue;
    	 DataStart := AC3_PACK_HEADER_LENGTH + InBuff[AC3_PACK_HEADER_LENGTH + 5]+6;
       if not IsAudioPES(@InBuff[DataStart]) then
         continue;
    	 AudioDataStart  := DataStart + 4;
       FAudioDataSize := PWord(@InBuff[AudioDataStart])^;
       AudioDataStart  := DataStart + 2; // + AC3_PRIVATE_DATA_LENGTH;
       for i := 0 to 255 do
          if InBuff[AudioDataStart+i] = 10 then
          begin
            AudioDataStart := AudioDataStart+i;
            Break;
          end;
       if InBuff[AudioDataStart] <> 10 then
         Continue;
       ParseLPCMHeader(AudioDataStart);
       AudioDataStart := AudioDataStart + FOffset;
       DataSize := SECTOR_SIZE-AudioDataStart;
       DataSize := DataSize - (DataSize mod FFrameSize);
       Move(InBuff[AudioDataStart], OutBuf[0], DataSize);
       Block := @OutBuf[0];
       Break;
     end;
   end;

   procedure TAuAOBDemuxer.ParseLPCMHeader(Offset : Integer);
   var
     IntOffset : Integer;
     hl, fs : Integer;
     chmask, ss, sr : Byte;
   begin
     IntOffset := Offset + 2;
     hl := InBuff[IntOffset+1]*256 + InBuff[IntOffset];
     IntOffset := Offset + 4;
     FOffset := InBuff[IntOffset+1]*256 + InBuff[IntOffset];
     chmask := InBuff[Offset + 6];
     ss := InBuff[Offset + 7] shr 5;
     sr := InBuff[Offset + 8] shr 5;
     case ss of
       0 : FBitsPerSample := 16;
       1 : FBitsPerSample := 20;
       2 : FBitsPerSample := 24;
       else
         FBitsPerSample := 0;
     end;
     case sr of
       0 : FSampleRate := 48000;
       1 : FSampleRate := 96000;
       2 : FSampleRate := 192000;
       8 : FSampleRate := 44100;
       9 : FSampleRate := 88200;
       10 : FSampleRate := 176400;
         else
         FSampleRate := 0;
     end;
     if FSampleRate < 88200 then
       FSamplesCount := 40
     else
     if FSampleRate < 176400 then
       FSamplesCount := 80
     else
       FSamplesCount := 160;
     if chmask = 0 then
        FChannels := 6
     else
       FChannels := 2;
     FFrameSize := (FBitsPerSample div 8)*FChannels*FSamplesCount;
   end;

   function TAuAOBDemuxer.Read(var Buffer; Count: Longint): Longint;
   var
     C : LongInt;
     b : array of Byte;
     i : Integer;
   begin
     Result := 0;
     C := Count;
     SetLength(b, C);
     while (Position < Size) do
     begin
       while C > 0 do
       begin
         if DataSize >= C then
         begin
           FastCopyMem(@b[Count - C], @Block[0], C);
           DataSize := DataSize - C;
           FastCopyMem(@Block[0], @Block[C], DataSize);
           Move(b[0], Buffer, Count);
           Result := Count;
           C := 0;
         end else
         begin
           FastCopyMem(@b[Count - C], @Block[0], DataSize);
           C := C - DataSize;
           ReadBlock;
           if DataSize = 0 then
           begin
             Result := 0;
             Exit;
           end;
         end;
       end;
       Break;
     end;
   end; *)


end.
