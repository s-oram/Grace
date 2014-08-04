(*
  This file is a part of Audio Components Suite v 2.2.
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_LAME;

interface

uses
  Classes, SysUtils, ACS_Classes, lame,
{$IFDEF LINUX}
  libc;
{$ENDIF}

{$IFDEF WIN32}
  Windows;
{$ENDIF}

const
  OUT_BUF_SIZE = $4000;

type

  TMP3BitRate = (br8, br16, br24, br32, br40, br48, br56, br64, br80, br96,
                 br112, br128, br144, br160, br192, br224, br256, br320);

  TMP3Out = class(TACSFileOut)
  private
    Buffer : array [1..(OUT_BUF_SIZE shr 1)] of SmallInt;
    FBitRate : TMP3BitRate;
    _plgf : PLame_global_flags;
    mp3buf : PByte;
    mp3buf_size : Integer;
    FTitle : String;
    FArtist : String;
    FAlbum : String;
    FYear : String;
    FTrack : String;
    FComment : String;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BitRate : TMP3BitRate read FBitRate write FBitRate stored True;
    property Id3TagTitle : String read FTitle write FTitle;
    property Id3TagArtist : String read FArtist write FArtist;
    property Id3TagAlbum : String read FAlbum write FAlbum;
    property Id3TagYear : String read FYear write FYear;
    property Id3TagTrack : String read FTrack write FTrack;
    property Id3TagComment : String read FComment write FComment;
  end;

implementation

  constructor TMP3Out.Create;
  begin
    inherited Create(AOwner);
    LoadLAME;
    FBitRate := br128;
    if not (csDesigning in ComponentState) then
    if not LameLoaded then
    raise EACSException.Create(LAME_PATH + ' library could not be loaded.');
  end;

  destructor TMP3Out.Destroy;
  begin
    UnloadLAME;
    inherited Destroy;
  end;

  procedure TMP3Out.Prepare;
  var
    samples, br : Integer;
  begin
    if not FStreamAssigned then
    begin
      if FFileName = '' then raise EACSException.Create('File name is not assigned.');
      FStream := TFileStream.Create(FFileName, fmCreate or fmShareExclusive, FAccessMask);
    end;
    FInput.Init;
    _plgf := lame_init;
    if FInput.Size > 0 then
    samples := FInput.Size div ((Finput.BitsPerSample shr 3)*Finput.Channels);
    lame_set_num_samples(_plgf, samples);
    lame_set_in_samplerate(_plgf, Finput.SampleRate);
    lame_set_num_channels(_plgf, 2); // not Finput.Channels see the note below
    case FBitRate of
      br8 : br := 8;
      br16 : br := 16;
      br24 : br := 24;
      br32 : br := 32;
      br40 : br := 40;
      br48 : br := 48;
      br56 : br := 56;
      br64 : br := 64;
      br80 : br := 80;
      br96 : br := 96;
      br112 : br := 112;
      br128 : br := 128;
      br144 : br := 144;
      br160 : br := 160;
      br192 : br := 192;
      br224 : br := 224;
      br256 : br := 256;
      br320 : br := 320;
    end;
    lame_set_brate(_plgf, br);
    lame_init_params(_plgf);
    mp3buf_size := (OUT_BUF_SIZE shr 1) + (OUT_BUF_SIZE shr 3) + 7200;
    GetMem(mp3buf, mp3buf_size);
  end;

  procedure TMP3Out.Done;
  var
    res : Integer;
  begin
    id3tag_init(_plgf);
    id3tag_set_title(_plgf, PChar(FTitle));
    id3tag_set_artist(_plgf, PChar(FArtist));
    id3tag_set_album(_plgf, PChar(FAlbum));
    id3tag_set_year(_plgf, PChar(FYear));
    id3tag_set_track(_plgf, PChar(FTrack));
    id3tag_set_comment(_plgf, PChar(FComment));
    res := lame_encode_flush(_plgf, mp3buf, mp3buf_size);
    FStream.Write(mp3buf^, res);
    if not FStreamAssigned then FStream.Free;
    lame_close(_plgf);
    FreeMem(mp3buf);
    FInput.Flush;
  end;

  function TMP3Out.DoOutput;
  var
    Len, res, ns : Integer;
  begin
    // No exceptions Here
    Result := True;
    if not CanOutput then Exit;
    if Progress <> CurProgr then
    begin
      CurProgr := Progress;
      if Assigned(FOnProgress) then FOnProgress(Self);
    end;
    if Abort then
    begin
      Result := False;
      Exit;
    end;
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(@Buffer[1], OUT_BUF_SIZE);
    InputLock := False;
    if Len <> 0 then
    begin
      if FInput.Channels = 2 then
      begin
        ns := Len shr 2;
        res := lame_encode_buffer_interleaved(_plgf, @Buffer[1], ns, mp3buf, mp3buf_size);
      end else
      begin
        (* If the input stream is mono we turn it into stereo here.
           This is the way to bypass some pour performance on mono streams,
           maybe not the best way, but for a while ...*)
        ns := Len shr 1;
        res := lame_encode_buffer(_plgf, @Buffer[1], @Buffer[1], ns, mp3buf, mp3buf_size);
      end;
      if res < 0 then
      begin
        Result := False;
        Exit;
      end;
      FStream.Write(mp3buf^, res);
    end else
    begin
      Result := False;
      Exit;
    end;
  end;
end.
