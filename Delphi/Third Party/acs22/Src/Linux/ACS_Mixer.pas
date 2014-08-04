(*
  This file is a part of Audio Components Suite v 2.2 (Kylix Edition).
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at borovsky@pochtamt.ru
*)

unit ACS_Mixer;

interface

uses
  Soundcard, Libc, Classes, SysUtils, ACS_Classes;

type
  TMixerChannel = (mcVolume, mcTreble, mcBass, mcSynth,
  mcPCM, mcSpeaker, mcLine, mcMic, mcCD, mcIMix, mcAltPCM, mcRecLev, mcUnknown);

  TMixerChannelSet = set of TMixerChannel;

  TMixerLevel = record
  case Word of
    1 :
    (
      Left, Right : Byte;
    );
    2 : (Main : Byte;);
  end;

  TMixer = class(TComponent)
  private
    FDevNum : Integer;
    _mix_fd : Integer;
    FChannels : TMixerChannelSet;
    FFileName : String;
    FMixerName : String;
    function GetRecSource : TMixerChannel;
    function GetVolume(const vChannel : TMixerChannel) : TMixerLevel;
    procedure SetRecSource(vChannel : TMixerChannel);
    procedure SetVolume(const vChannel : TMixerChannel;  vLevel : TMixerLevel);
    procedure SetDevNum(Num : Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsStereo(vChannel : TMixerChannel) : Boolean;
    function IsRecordable(vChannel : TMixerChannel) : Boolean;
    property Channels : TMixerChannelSet read FChannels;
    property Level[const vChannel : TMixerChannel] : TMixerLevel read GetVolume write SetVolume;
    property RecordSource : TMixerChannel read GetRecSource write SetRecSource;
  published
    property DevNum : Integer read FDevNum write SetDevNum stored True;
    property MixerName : String read FMixerName;
  end;

var
  MixersCount : Byte;

  function ChannelToStr(ch : TMixerChannel) : String;

implementation

type
  TMixerInfo = record
    Path : String;
    Name : String;
  end;

const
  MAX_MIXERS = 5; (* There shouldn't be more than
                     5 valid mixers in the system.
                     Right? *)

var
  Mixers : array[0..MAX_MIXERS] of TMixerInfo; // one extra slot for /dev/mixer device

  function ChannelToStr(ch : TMixerChannel) : String;
  begin
    case ch of
      mcVolume:  Result := 'Master output';
      mcTreble:  Result := 'Treble output';
      mcBass:    Result := 'Bass output';
      mcSynth:   Result := 'Synthesizer input';
      mcPCM:     Result := 'Audio output';
      mcSpeaker: Result := 'Speaker output';
      mcLine:    Result := 'Line input';
      mcMic:     Result := 'Michrophone input';
      mcCD:      Result := 'CD input';
      mcIMix:    Result := 'Record monitor';
      mcAltPCM:  Result := 'Alternate output';
      mcRecLev:  Result := 'Record level';
      mcUnknown: Result := 'Unknown channel';
    end;
  end;

  constructor TMixer.Create;
  begin
    inherited Create(AOwner);
    if MixersCount > 0 then
    SetDevNum(0);
  end;

  destructor TMixer.Destroy;
  begin
    inherited Destroy;
  end;

  function GetChannelMask(Ch : TMixerChannel; Request : Integer): LongWord;
  begin
    Result := 0;
    case Request of
      0:
      case Ch of
        mcVolume:  Result := SOUND_MIXER_VOLUME;
        mcTreble:  Result := SOUND_MIXER_TREBLE;
        mcBass:    Result := SOUND_MIXER_BASS;
        mcSynth:   Result := SOUND_MIXER_SYNTH;
        mcPCM:     Result := SOUND_MIXER_PCM;
        mcSpeaker: Result := SOUND_MIXER_SPEAKER;
        mcLine:    Result := SOUND_MIXER_LINE;
        mcMic:     Result := SOUND_MIXER_MIC;
        mcCD:      Result := SOUND_MIXER_CD;
        mcIMix:    Result := SOUND_MIXER_IMIX;
        mcAltPCM:  Result := SOUND_MIXER_ALTPCM;
        mcRecLev:  Result := SOUND_MIXER_RECLEV;
        mcUnknown: Result := 0;
      end;
      1:
      case Ch of
        mcVolume:  Result := SOUND_MIXER_WRITE_VOLUME;
        mcTreble:  Result := SOUND_MIXER_WRITE_TREBLE;
        mcBass:    Result := SOUND_MIXER_WRITE_BASS;
        mcSynth:   Result := SOUND_MIXER_WRITE_SYNTH;
        mcPCM:     Result := SOUND_MIXER_WRITE_PCM;
        mcSpeaker: Result := SOUND_MIXER_WRITE_SPEAKER;
        mcLine:    Result := SOUND_MIXER_WRITE_LINE;
        mcMic:     Result := SOUND_MIXER_WRITE_MIC;
        mcCD:      Result := SOUND_MIXER_WRITE_CD;
        mcIMix:    Result := SOUND_MIXER_WRITE_IMIX;
        mcAltPCM:  Result := SOUND_MIXER_WRITE_ALTPCM;
        mcRecLev:  Result := SOUND_MIXER_WRITE_RECLEV;
        mcUnknown: Result := 0;
      end;
      2:
      case Ch of
        mcVolume:  Result := SOUND_MIXER_READ_VOLUME;
        mcTreble:  Result := SOUND_MIXER_READ_TREBLE;
        mcBass:    Result := SOUND_MIXER_READ_BASS;
        mcSynth:   Result := SOUND_MIXER_READ_SYNTH;
        mcPCM:     Result := SOUND_MIXER_READ_PCM;
        mcSpeaker: Result := SOUND_MIXER_READ_SPEAKER;
        mcLine:    Result := SOUND_MIXER_READ_LINE;
        mcMic:     Result := SOUND_MIXER_READ_MIC;
        mcCD:      Result := SOUND_MIXER_READ_CD;
        mcIMix:    Result := SOUND_MIXER_READ_IMIX;
        mcAltPCM:  Result := SOUND_MIXER_READ_ALTPCM;
        mcRecLev:  Result := SOUND_MIXER_READ_RECLEV;
        mcUnknown: Result := 0;
      end;
    end;
  end;

  function GetChannel(Mask : Integer) : TMixerChannel;
  begin
    case Mask of
      SOUND_MIXER_VOLUME:  Result := mcVolume;
      SOUND_MIXER_TREBLE:  Result := mcTreble;
      SOUND_MIXER_BASS:    Result := mcBass;
      SOUND_MIXER_SYNTH:   Result := mcSynth;
      SOUND_MIXER_PCM:     Result := mcPCM;
      SOUND_MIXER_SPEAKER: Result := mcSpeaker;
      SOUND_MIXER_LINE:    Result := mcLine;
      SOUND_MIXER_MIC:     Result := mcMic;
      SOUND_MIXER_CD:      Result := mcCD;
      SOUND_MIXER_IMIX:    Result := mcIMix;
      SOUND_MIXER_ALTPCM:  Result := mcAltPCM;
      SOUND_MIXER_RECLEV:  Result := mcRecLev;
      else                 Result := mcUnknown;
    end;
  end;

  procedure TMixer.SetDevNum(Num : Integer);
  var
    DevMask, i : Integer;
    Channel : TMixerChannel;
  begin
    if Num in [0..MixersCount - 1] then // check [0..0] [0..-1]
    begin
      FFileName := Mixers[Num].Path;
      FMixerName := Mixers[Num].Name;
      FChannels := [];
      _mix_fd := open(PChar(FFileName), O_RDONLY);
      ioctl(_mix_fd, SOUND_MIXER_READ_DEVMASK, @DevMask);
      __close(_mix_fd);
      for i:=0 to 31 do
      begin
        if (DevMask and (1 shl i)) <> 0 then
        begin
          Channel := GetChannel(i);
          if Channel <> mcUnknown then
          FChannels := FChannels + [Channel];
        end;
      end;
    end;
  end;

  function TMixer.GetRecSource;
  var
    rs, pow : Integer;
  begin
    _mix_fd := open(PChar(FFileName), O_RDONLY);
    ioctl(_mix_fd, SOUND_MIXER_READ_RECSRC, @rs);
    __close(_mix_fd);
    pow := 0;
    while rs <> 1 do
    begin
      rs := rs shr 1;
      Inc(pow);
    end;
    Result := GetChannel(pow);
  end;

  function TMixer.GetVolume;
  var
    vol, chan : Integer;
  begin
    _mix_fd := open(PChar(FFileName), O_RDONLY);
    chan := GetChannelMask(vChannel, 2);
    ioctl(_mix_fd, chan, @vol);
    __close(_mix_fd);
    if vol > 255 then
    begin
      Result.Left := Lo(vol);
      Result.Right := Lo(vol shr 8);
    end else Result.Main := vol;
  end;

  function TMixer.IsStereo;
  var
    mask, chan : Integer;
  begin
    _mix_fd := open(PChar(FFileName), O_RDONLY);
    ioctl(_mix_fd, SOUND_MIXER_READ_STEREODEVS, @mask);
    chan := GetChannelMask(vChannel, 0);
    __close(_mix_fd);
    Result := (mask and (1 shl chan))<>0;
  end;

  function TMixer.IsRecordable;
  var
    mask, chan : Integer;
  begin
    _mix_fd := open(PChar(FFileName), O_RDONLY);
    ioctl(_mix_fd, SOUND_MIXER_READ_RECMASK, @mask);
    chan := GetChannelMask(vChannel, 0);
    __close(_mix_fd);
    Result := (mask and (1 shl chan))<>0;
  end;

  procedure TMixer.SetRecSource;
  var
    chan : Integer;
  begin
    chan := 1 shl GetChannelMask(vChannel, 0);
    _mix_fd := open(PChar(FFileName), O_WRONLY);
    ioctl(_mix_fd, SOUND_MIXER_WRITE_RECSRC, @chan);
    __close(_mix_fd);
    if chan <> (1 shl GetChannelMask(vChannel, 0)) then
    raise EACSException.Create(ChannelToStr(vChannel)+' channel is not recordable');
  end;

  procedure TMixer.SetVolume;
  var
    vol, chan : Integer;
  begin
    chan := GetChannelMask(vChannel, 1);
    if IsStereo(vChannel) then
    vol := vLevel.Left + (vLevel.Right shl 8)
    else vol := vLevel.Main;
    _mix_fd := open(PChar(FFileName), O_WRONLY);
    ioctl(_mix_fd, chan, @vol);
    __close(_mix_fd);
  end;

  function CountMixers : Byte;
  var
    fd, i, DevMask : Integer;
    fname : String;
    mi : mixer_info;
  begin
    Result := 0;
    for i := 0 to MAX_MIXERS-1 do
    begin
      fname := '/dev/mixer'+IntToStr(i-1);
      try
        fd := open(PChar(fname), O_RDONLY);
      except
        Break;
      end;
      if fd = -1 then Break;
      DevMask := 0;
      ioctl(fd, SOUND_MIXER_READ_DEVMASK, @DevMask);
      if DevMask <> 0 then
      begin
        Mixers[Result].Path := fname;
        ioctl(fd, SOUND_MIXER_INFO, @mi);
        Mixers[Result].Name := String(mi.name);
        Inc(Result);
      end;
      __close(fd);
    end;
    fname := '/dev/mixer';
    try
      fd := open(PChar(fname), O_RDONLY);
    except
      Exit;
    end;
    if fd = -1 then Exit;
    ioctl(fd, SOUND_MIXER_READ_DEVMASK, @DevMask);
    if DevMask <> 0 then
    begin
      Mixers[Result].Path := fname;
      ioctl(fd, SOUND_MIXER_INFO, @mi);
      Mixers[Result].Name := String(mi.name);
    end;
    __close(fd);
    Inc(Result);
  end;

initialization
  MixersCount := CountMixers;
end.
