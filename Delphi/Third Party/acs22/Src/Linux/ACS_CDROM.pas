(*
  This file is a part of Audio Components Suite v 2.2 (Kylix Edition).
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_CDROM;

interface


uses
  Classes, SysUtils, Libc, ACS_Classes, cd_rom;

const
  BUF_SIZE = 80 * CD_FRAMESIZE_RAW;  // 80 frames

type

  TCDStatus = (cdsNotReady, cdsReady, cdsPlaying, cdsPaused);
  TTrackType = (ttAudio, ttData);
  TCDInfo = (cdiNoDisc, cdiDiscAudio, cdiDiscData, cdiDiscMixed, cdiUnknown);
  TMCN = array[0..13] of Char;

  TCDMSF = record
    Minute : Byte;
    Second : Byte;
    Frame : Byte;
  end;

  TCDTrackInfo = record
    TrackLength : TCDMSF;
    TrackType : TTrackType;
  end;

  TCDPosition = record
    Track : Integer;
    MSF : TCDMSF;
  end;

  TCDPlayer = class(TComponent)
  private
    FCDPath : String;
    FOpened : Integer;
    _cd_fd : Integer;
    procedure OpenCD;
    procedure CloseCD;
    procedure ForceClose;
    function GetDiscInfo : TCDInfo;
    procedure GetTrackStart(Track : Integer; var MSF : TCDMSF);
    function GetMCN : TMCN;
    function GetMediaChanged : Boolean;
    function GetNumTracks : Integer;
    function GetPosition : TCDPosition;
    function GetTrackInfo(const vIndex : Integer) : TCDTrackInfo;
    function GetStatus : TCDStatus;
    function GetLVolume : Integer;
    procedure SetLVolume(vVolume: Integer);
    function GetRVolume : Integer;
    procedure SetRVolume(vVolume: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseTray;
    procedure Eject;
    procedure Pause;
    procedure Play(Track : Integer); overload;
    procedure Play(Track, Index : Integer); overload;
    procedure Play(PlayFrom, PlayTo : TCDPosition); overload;
    procedure Resume;
    procedure Stop;
    property DiscInfo : TCDInfo read GetDiscInfo;
    property MCN : TMCN read GetMCN;
    property MediaChanged : Boolean read GetMediaChanged;
    property Position : TCDPosition read GetPosition;
    property Status : TCDStatus read GetStatus;
    property TracksCount : Integer read GetNumTracks;
    property Tracks[const vIndex : Integer] : TCDTrackInfo read GetTrackInfo;
  published
    property CDPath : String read FCDPath write FCDPath stored True;
    property LVolume : Integer read GetLVolume write SetLVolume;
    property RVolume : Integer read GetRVolume write SetRVolume;
  end;

  TCDIn = class(TACSInput)
  private
    FCDPath : String;
    FOpened : Integer;
    _cd_fd : Integer;
    FStartTrack, FEndTrack : Integer;
    FStartPos, FEndPos: TCDPosition;
    FCurPos, FEndMSF : TCDMSF;
    buf : array[1..BUF_SIZE] of Byte;  // ring buffer
    procedure OpenCD;
    procedure CloseCD;
    function GetStatus : TCDStatus;
    function GetNumTracks : Integer;
    function GetTrackInfo(const vIndex : Integer) : TCDTrackInfo;
    procedure SetST(Track : Integer);
    procedure SetET(Track : Integer);
    procedure SetSP(Pos : TCDPosition);
    procedure SetEP(Pos : TCDPosition);
    function GetSize : Integer;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetInfo : TCDInfo;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    procedure Eject;
    procedure CloseTray;
    property DiscInfo: TCDInfo read GetInfo;
    property Status: TCDStatus read GetStatus;
    property Tracks[const vIndex : Integer] : TCDTrackInfo read GetTrackInfo;
    property TracksCount : Integer read GetNumTracks;
  published
    property CDPath : String read FCDPath write FCDPath stored True;
    property StartTrack: Integer read FStartTrack write SetSt;
    property EndTrack: Integer read FEndTrack write SetET;
    property StartPos : TCDPosition read FStartPos write SetSP;
    property EndPos : TCDPosition read FEndPos write SetEP;
  end;

  const
    EndOfDisc : TCDPosition = (Track : CDROM_LEADOUT; MSF : (Minute : 0; Second : 0; Frame : 0));

  function MSFToStr(const MSF : TCDMSF) : String;

implementation

  function MSF2Frames(const MSF : TCDMSF) : Integer;
  begin
    Result := ((MSF.Minute * 60) + MSF.Second) * 75 + MSF.Frame;
  end;

  procedure Frames2MSF(Frames : Integer; var MSF : TCDMSF);
  var
    Temp : Integer;
  begin
    Temp := Frames div 75;
    MSF.Minute := Temp div 60;
    MSF.Second := Temp mod 60;
    MSF.Frame := Frames mod 75;
  end;

  function GetTocEntry(cd_fd, Track : Integer): TCDTrackInfo;
  var
    Entry : cdrom_tocentry;
    toc : cdrom_tochdr;
    frames1, frames2 : Integer;
  begin
    ioctl(cd_fd, CDROMREADTOCHDR, @toc);
    Entry.cdte_format := CDROM_MSF;
    Entry.cdte_track := Track+toc.cdth_trk0-1;
    ioctl(cd_fd, CDROMREADTOCENTRY, @Entry);
    frames1 := MSF2Frames(TCDMSF(Entry.cdte_addr.msf));
    if (Entry.cdte_adr_ctrl and CDROM_DATA_TRACK) <> 0 then
    Result.TrackType := ttData
    else Result.TrackType := ttAudio;
    if Entry.cdte_track < toc.cdth_trk1 then Inc(Entry.cdte_track)
    else Entry.cdte_track := CDROM_LEADOUT;
    ioctl(cd_fd, CDROMREADTOCENTRY, @Entry);
    frames2 := MSF2Frames(TCDMSF(Entry.cdte_addr.msf));
    Frames2MSF(frames2-frames1, Result.TrackLength);
  end;

  function GetCDStatus(cd_fd : Integer) : TCDStatus;
  (* not all drivers support the CDROM_DRIVE_STATUS ioctl
   we use this ioctl first and then some other tecnique
   if it is not supported. *)
  var
    sci : cdrom_subchnl;
    res :Integer;
  begin
    res := ioctl(cd_fd, CDROM_DRIVE_STATUS, CDSL_CURRENT);
    case res of
      CDS_TRAY_OPEN, CDS_NO_DISC, CDS_DRIVE_NOT_READY:
      begin
        Result := cdsNotReady;
        Exit;
      end;
    end;
    (* Either the disc is ok or no information
     from the driver. Trying CDROMSUBCHNL.*)
    sci.cdsc_format := CDROM_MSF;
    if ioctl(cd_fd, CDROMSUBCHNL, @sci) < 0 then
    begin
      Result := cdsNotReady;
      Exit;
    end;
    case sci.cdsc_audiostatus of
      CDROM_AUDIO_PLAY : Result := cdsPlaying;
      CDROM_AUDIO_PAUSED : Result := cdsPaused;
      CDROM_AUDIO_ERROR  : Result := cdsNotReady;
      else Result := cdsReady;
    end;
  end;

  function GetCDInfo(cd_fd : Integer) : TCDInfo;
  var
    res : Integer;
  begin
    Result := cdiUnknown;
    res := ioctl(cd_fd, CDROM_DRIVE_STATUS, CDSL_CURRENT);
    case res of
      CDS_TRAY_OPEN, CDS_NO_DISC: Result := cdiNoDisc;
      CDS_DISC_OK :
      begin
        res := ioctl(cd_fd, CDROM_DISC_STATUS, CDSL_CURRENT);
        case res of
          CDS_AUDIO : Result := cdiDiscAudio;
          CDS_MIXED : Result := cdiDiscMixed;
          else Result :=  cdiDiscData;
        end;
      end;
    end;
  end;

  constructor TCDPlayer.Create(AOwner: TComponent);
  begin
    inherited Create(AOwner);
    FCDPath := '/dev/cdrom';
    FOpened := 0; // For the sake of style
  end;

  destructor TCDPlayer.Destroy;
  begin
    //FCDPath := '';
    if FOpened > 0 then
    __close(_cd_fd);
    inherited Destroy;
  end;

  procedure TCDPlayer.OpenCD;
  begin
    if FOpened = 0 then
    begin
      _cd_fd := open(PChar(FCDPath), O_RDONLY or O_NONBLOCK);
      if _cd_fd < 0 then
      raise EACSException.Create(strerror(errno));
    end;
    Inc(FOpened);
  end;

  procedure TCDPlayer.CloseCD;
  begin
    if FOpened <= 1 then
    __close(_cd_fd);
    Dec(FOpened);
  end;

  procedure TCDPlayer.ForceClose;
  begin
    __close(_cd_fd);
    FOpened:=0;
  end;

  procedure TCDPlayer.GetTrackStart;
  var
    Entry : cdrom_tocentry;
    toc : cdrom_tochdr;
  begin
    OpenCD;
    Entry.cdte_format := CDROM_MSF;
    ioctl(_cd_fd, CDROMREADTOCHDR, @toc);
    Entry.cdte_track := Track+toc.cdth_trk0-1;
    ioctl(_cd_fd, CDROMREADTOCENTRY, @Entry);
    MSF := TCDMSF(Entry.cdte_addr.msf);
    CloseCD;
  end;

  function TCDPlayer.GetNumTracks;
  var
    toc : cdrom_tochdr;
  begin
    OpenCD;
    if GetStatus <> cdsNotReady then
    begin
      ioctl(_cd_fd, CDROMREADTOCHDR, @toc);
      Result := toc.cdth_trk1 - toc.cdth_trk0 + 1;
    end else Result := 0;
    CloseCD;
  end;

  procedure TCDPlayer.CloseTray;
  begin
    OpenCD;
    ioctl(_cd_fd, CDROMCLOSETRAY);
    CloseCD;
  end;

  procedure TCDPlayer.Eject;
  begin
    OpenCD;
    if GetStatus in [cdsPlaying, cdsPaused] then
    begin
      ForceClose;
      raise EACSException.Create('Drive is not ready.');
    end;
    ioctl(_cd_fd, CDROMEJECT);
    CloseCD;
  end;

  procedure TCDPlayer.Pause;
  begin
    OpenCD;
    if GetStatus = cdsPlaying then
    ioctl(_cd_fd, CDROMPAUSE);
    CloseCD;
  end;

  procedure TCDPlayer.Play(Track : Integer);
  var
    toc : cdrom_tochdr;
    ti : cdrom_ti;
  begin
    OpenCD;
    if GetStatus = cdsNotReady then
    begin
      ForceClose;
      raise EACSException.Create('Device is not ready');
    end;
    if GetTrackInfo(Track).TrackType <> ttAudio then
    begin
      ForceClose;
      raise EACSException.Create('Trying to play a data track.');
    end;
    ioctl(_cd_fd, CDROMREADTOCHDR, @toc);
    ti.cdti_trk0 := toc.cdth_trk0 + Track - 1;
    ti.cdti_ind0 := 0;
    ti.cdti_trk1 := toc.cdth_trk0 + Track - 1;
    ti.cdti_ind1 := 255;
    ioctl(_cd_fd, CDROMPLAYTRKIND, @ti);
    CloseCD;
  end;

  procedure TCDPlayer.Play(Track, Index : Integer);
  var
    toc : cdrom_tochdr;
    ti : cdrom_ti;
  begin
    OpenCD;
    if GetStatus = cdsNotReady  then
    begin
      ForceClose;
      raise EACSException.Create('Device is not ready');
    end;
    if GetTrackInfo(Track).TrackType <> ttAudio then
    begin
      ForceClose;
      raise EACSException.Create('Trying to play a data track.');
    end;
    ioctl(_cd_fd, CDROMREADTOCHDR, @toc);
    ti.cdti_trk0 := toc.cdth_trk0 + Track - 1;
    ti.cdti_ind0 := Index;
    ti.cdti_trk1 := toc.cdth_trk0 + Track - 1;
    ti.cdti_ind1 := Index;
    ioctl(_cd_fd, CDROMPLAYTRKIND, @ti);
    CloseCD;
  end;

  procedure TCDPlayer.Play(PlayFrom, PlayTo : TCDPosition);
  var
    frames1, frames2 : Integer;
    _msf : cdrom_msf_t;
    MSF : TCDMSF;
  begin
    OpenCD;
    if GetStatus = cdsNotReady then
    begin
      ForceClose;
      raise EACSException.Create('Device is not ready');
    end;
    if (PlayFrom.Track in [1..GetNumTracks]) = False then
    begin
      ForceClose;
      raise EACSException.Create('Track out of range.');
    end;
    if PlayTo.Track < PlayFrom.Track then
    begin
      ForceClose;
      raise EACSException.Create('Track out of range.');
    end;
    GetTrackStart(PlayFrom.Track, MSF);
    frames1 := MSF2Frames(MSF) + MSF2Frames(PlayFrom.MSF);
    GetTrackStart(PlayTo.Track, MSF);
    frames2 := MSF2Frames(MSF) + MSF2Frames(PlayTo.MSF);
    Frames2MSF(frames1, MSF);
    _msf.cdmsf_min0 := MSF.Minute;
    _msf.cdmsf_sec0 := MSF.Second;
    _msf.cdmsf_frame0 := MSF.Frame;
    Frames2MSF(frames2, MSF);
    _msf.cdmsf_min1 := MSF.Minute;
    _msf.cdmsf_sec1 := MSF.Second;
    _msf.cdmsf_frame1 := MSF.Frame;
    ioctl(_cd_fd, CDROMPLAYMSF, @_msf);
    CloseCD;
  end;

  procedure TCDPlayer.Resume;
  begin
    OpenCD;
    if GetStatus = cdsPaused then
    ioctl(_cd_fd, CDROMRESUME);
    CloseCD;
  end;

  procedure TCDPlayer.Stop;
  begin
    OpenCD;
    ioctl(_cd_fd, CDROMSTOP);
    CloseCD;
  end;

  function TCDPlayer.GetPosition;
  var
    sci : cdrom_subchnl;
    toc : cdrom_tochdr;
  begin
    OpenCD;
    case GetStatus of
    cdsPlaying, cdsPaused :
    begin
      sci.cdsc_format := CDROM_MSF;
      ioctl(_cd_fd, CDROMSUBCHNL, @sci);
      ioctl(_cd_fd, CDROMREADTOCHDR, @toc);
      Result.Track := sci.cdsc_trk-toc.cdth_trk0+1;
      Result.MSF.Minute := sci.cdsc_reladdr.msf.minute;
      Result.MSF.Second := sci.cdsc_reladdr.msf.second;
      Result.MSF.Frame := sci.cdsc_reladdr.msf.frame;
    end;
    else begin
      Result.Track := 0;
      Result.MSF.Minute := 0;
      Result.MSF.Second := 0;
      Result.MSF.Frame := 0;
    end;
    end;
    CloseCD;
  end;

  function TCDPlayer.GetTrackInfo;
  begin
    OpenCD;
    if (vIndex in [1..GetNumTracks]) = False then
    begin
      ForceClose;
      raise EACSException.Create('Track out of range.');
    end;
    Result := GetTocEntry(_cd_fd, vIndex);
    CloseCD;
  end;

  function TCDPlayer.GetStatus;
  begin
    if Fopened = 0 then
    _cd_fd := open(PChar(FCDPath), O_RDONLY or O_NONBLOCK);
    if _cd_fd < 0 then
    begin
      Result := cdsNotReady;
      Exit;
    end;
    Inc(FOpened);
    Result := GetCDStatus(_cd_fd);
    CloseCD;
  end;

  function TCDPlayer.GetLVolume;
  var
    vc : cdrom_volctrl;
  begin
   OpenCD;
   ioctl(_cd_fd, CDROMVOLREAD, @vc);
   Result := vc.channel0;
   CloseCD;
  end;

  procedure TCDPlayer.SetLVolume;
    var
    vc : cdrom_volctrl;
  begin
   OpenCD;
   ioctl(_cd_fd, CDROMVOLREAD, @vc);
   vc.channel0 := vVolume;
   ioctl(_cd_fd, CDROMVOLCTRL, @vc);
   CloseCD;
  end;

  function TCDPlayer.GetRVolume;
  var
    vc : cdrom_volctrl;
  begin
   OpenCD;
   ioctl(_cd_fd, CDROMVOLREAD, @vc);
   Result := vc.channel1;
   CloseCD;
  end;

  procedure TCDPlayer.SetRVolume;
    var
    vc : cdrom_volctrl;
  begin
   OpenCD;
   ioctl(_cd_fd, CDROMVOLREAD, @vc);
   vc.channel1 := vVolume;
   ioctl(_cd_fd, CDROMVOLCTRL, @vc);
   CloseCD;
  end;

  function TCDPlayer.GetDiscInfo;
  begin
    OpenCD;
    Result := GetCDInfo(_cd_fd);
    CloseCD;
  end;

  function TCDPlayer.GetMCN;
  begin
    OpenCD;
    if GetStatus = cdsNotReady then
    begin
      CloseCD;
      raise EACSException.Create('Drive is not ready.');
    end;
    ioctl(_cd_fd, CDROM_GET_MCN, @Result);
    CloseCD;
  end;

  function TCDPlayer.GetMediaChanged;
  begin
    OpenCD;
    Result := ioctl(_cd_fd, CDROM_MEDIA_CHANGED, CDSL_CURRENT) > 0;
    CloseCD;
  end;

  function MSFToStr(const MSF : TCDMSF) : String;
  var
    sep : String;
    sec, min : Integer;
  begin
    min := MSF.Minute;
    if MSF.Frame > 37 then
    begin
      sec := MSF.Second + 1;
      if sec = 60 then
      begin
        Inc(min);
        sec := 0;
      end;
    end
    else sec := MSF.Second;
    if sec<10 then sep := ':0'
    else sep := ':';
    Result := IntToStr(min) + sep + IntToStr(sec);
  end;

  procedure TCDIn.OpenCD;
  begin
    if FOpened = 0 then
    begin
      _cd_fd := open(PChar(FCDPath), O_RDONLY or O_NONBLOCK);
      if _cd_fd < 0 then
      raise EACSException.Create(strerror(errno));
    end;
    Inc(FOpened);
  end;


  procedure TCDIn.CloseCD;
  begin
    if FOpened = 1 then __close(_cd_fd);
    if FOpened > 0 then Dec(FOpened);
  end;

  function TCDIn.GetInfo;
  begin
    if Buisy then raise EACSException.Create('The component is buisy');
    OpenCD;
    Result := GetCDInfo(_cd_fd);
    CloseCD;
  end;

  function TCDIn.GetStatus;
  begin
    if Buisy then raise EACSException.Create('The component is buisy');
    if Fopened = 0 then
    _cd_fd := open(PChar(FCDPath), O_RDONLY or O_NONBLOCK);
    if _cd_fd < 0 then
    begin
      Result := cdsNotReady;
      Exit;
    end;
    Inc(FOpened);
    Result := GetCDStatus(_cd_fd);
    CloseCD;
  end;

  function TCDIn.GetNumTracks;
  var
    toc : cdrom_tochdr;
  begin
    if Buisy then raise EACSException.Create('The component is buisy');
    OpenCD;
    if GetStatus <> cdsNotReady then
    begin
      ioctl(_cd_fd, CDROMREADTOCHDR, @toc);
      Result := toc.cdth_trk1 - toc.cdth_trk0 + 1;
    end else Result := 0;
    CloseCD;
  end;

  function TCDIn.GetTrackInfo;
  begin
    if Buisy then raise EACSException.Create('The component is buisy');
    OpenCD;
    if (vIndex in [1..GetNumTracks]) = False then
    begin
      __close(_cd_fd);
      FOpened := 0;
      raise EACSException.Create('Track out of range.');
    end;
    Result := GetTocEntry(_cd_fd, vIndex);
    CloseCD;
  end;

  function GetTrackMSF(cd_fd, Track : Integer) : TCDMSF;
  var
    entry : cdrom_tocentry;
    hdr : cdrom_tochdr;
  begin
    ioctl(cd_fd, CDROMREADTOCHDR, @hdr);
    entry.cdte_format := CDROM_MSF;
    entry.cdte_track := Track + hdr.cdth_trk0 - 1;
    if entry.cdte_track > hdr.cdth_trk1 then
    entry.cdte_track := CDROM_LEADOUT;
    ioctl(cd_fd, CDROMREADTOCENTRY, @entry);
    Result := TCDMSF(entry.cdte_addr.msf);
  end;

  function GetPosMSF(cd_fd : Integer; Pos : TCDPosition) : TCDMSF;
  var
    msf1 : TCDMSF;
    frames : Integer;
  begin
    msf1 := TCDMSF(GetTrackMSF(cd_fd, Pos.Track));
    frames := MSF2Frames(msf1);
    frames := frames + MSF2Frames(Pos.MSF);
    Frames2MSF(frames, msf1);
    Result := msf1;
  end;

  procedure TCDIn.SetST;
  begin
    if Self.Buisy then raise EACSException.Create('The component is buisy');
    FStartTrack := Track;
    FStartPos.Track := FStartTrack;
    FillChar(FStartPos.MSF, SizeOf(FStartPos.MSF), 0);
  end;

  procedure TCDIn.SetET;
  begin
    if Self.Buisy then raise EACSException.Create('The component is buisy');
    FEndTrack := Track;
    OpenCD;
    FEndPos.Track := FEndTrack + 1;
    FillChar(FEndPos.MSF, SizeOf(FEndPos.MSF), 0);
    CloseCD;
  end;

  procedure TCDIn.SetSP;
  begin
    if Self.Buisy then raise EACSException.Create('The component is buisy');
    Self.FStartPos := Pos;
  end;

  procedure TCDIn.SetEP;
  begin
    if Self.Buisy then raise EACSException.Create('The component is buisy');
    Self.FEndPos := Pos;
  end;

  constructor TCDIn.Create;
  begin
    inherited Create(AOwner);
    Self.FCDPath := '/dev/cdrom';
  end;

  destructor TCDIn.Destroy;
  begin
    inherited Destroy;
  end;

  function TCDIn.GetBPS : Integer;
  begin
    Result := 16;
  end;

  function TCDIn.GetCh;
  begin
    Result := 2;
  end;

  function TCDIn.GetSR;
  begin
    Result := 44100;
  end;

  function TCDIn.GetSize;
  var
    msf1, msf2 : TCDMSF;
  begin
    if Buisy then raise EACSException.Create('The component is buisy');
    OpenCD;
    msf1 := GetPosMSF(_cd_fd, FStartPos);
    msf2 := GetPosMSF(_cd_fd, FEndPos);
    CloseCD;
    Result := ((msf2.minute*60 + msf2.second)*75 + msf2.frame -
      ((msf1.minute*60 + msf1.second)*75 + msf1.frame))*CD_FRAMESIZE_RAW;
  end;

  procedure TCDIn.Init;
  begin
    if Buisy then raise EACSException.Create('The component is buisy');
    if not (DiscInfo in [cdiDiscAudio, cdiDiscMixed]) then
    raise EACSException.Create('Not an audio disc');
    FSize := GetSize;
    Buisy := True;
    BufStart := 1;
    BufEnd := 0;
    FPosition := 0;
    OpenCD;
    FCurPos := GetPosMSF(_cd_fd, FStartPos);
    FEndMSF := GetPosMSF(_cd_fd, FEndPos);
  end;

  procedure TCDIn.Flush;
  begin
    CloseCD;
    Buisy := False;
    FSize := 0;
  end;

  function TCDIn.GetData;
  var
    StartFrame, EndFrame : Integer;
    cdaudio : cdrom_read_audio;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    StartFrame := MSF2Frames(FCurPos);
    EndFrame := MSF2Frames(FEndMSF);
    if BufStart > BufEnd then
    begin
      if EndFrame = StartFrame then
      begin
        Result := 0;
        Exit;
      end;
      BufStart := 1;
      if (EndFrame - StartFrame) > (BUF_SIZE div CD_FRAMESIZE_RAW)
      then  cdaudio.nframes := BUF_SIZE div CD_FRAMESIZE_RAW
      else cdaudio.nframes := EndFrame - StartFrame;
      cdaudio.addr_format := CDROM_MSF;
      cdaudio.addr.msf := cdrom_msf0(FCurPos);
      cdaudio.buf := @Buf[1];
      ioctl(_cd_fd, CDROMREADAUDIO, @cdaudio);
      BufEnd := cdaudio.nframes * CD_FRAMESIZE_RAW;
      StartFrame := MSF2Frames(FCurPos) + cdaudio.nframes;
      Frames2MSF(StartFrame, FCurPos);
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(Buf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
  end;

  procedure TCDIn.Eject;
  begin
    if Buisy then raise EACSException.Create('The component is buisy.');
    OpenCD;
    ioctl(_cd_fd, CDROMEJECT);
    CloseCD;
  end;

  procedure TCDIn.CloseTray;
  begin
    OpenCD;
    ioctl(_cd_fd, CDROMCLOSETRAY);
    CloseCD;
  end;

end.
