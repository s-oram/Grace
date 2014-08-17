(*
  This file is a part of New Audio Components package v 2.4
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: ACS_Misc.pas 1127 2010-01-23 18:35:12Z andrei.borovsky $ *)

unit ACS_Misc;

(* Title: ACS_Misc
    Miscellaneous converter and utility components.  *)

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes, ACS_Procs, SyncObjs, GainAnalysis, taglib

 {$IFDEF WIN32}
  , Windows
  {$ENDIF}

  {$IFDEF LINUX}
  , libc, smpeg, LibAO
  {$ENDIF};

const
  BUF_SIZE = $4000;

type

  TBuffer = array[0..0] of Byte;
  PBuffer = ^TBuffer;

  TOnBufferDone = procedure(Sender : TComponent; var DataBuffer : Pointer; var DataSize : LongWord; var RepeatCount : Integer) of object;

  TAudioProcessorInitEvent = procedure(Sender : TComponent; var TotalSize : Int64) of object;
  TAudioProcessorFlushEvent = procedure(Sender : TComponent) of object;

  TGetParameterEvent64 = procedure(Sender : TComponent; var Param : Int64) of object;
  TGetParameterEvent32 = procedure(Sender : TComponent; var Param : LongWord) of object;

  TGetDataEvent = procedure(Sender : TComponent; var Buffer : Pointer; var Bytes : LongWord) of object;

  (* Class: TMemoryIn
    A descendant of <TAuInput> which reads audio data from a memory block that
    you provide. It is analogous to TStreamIn when reading from TMemoryStream,
    the only difference is that a pointer to a memory block is used instead of
    a TMemoryStream object. *)

  TMemoryIn = class(TAuInput)
  private
    FBuffer : PBuffer;
    FDataSize : LongWord;
    Busy : Boolean;
    BufStart, BufEnd : LongWord;
    FBPS, FSR, FChan : LongWord;
    FRepeatCount : Integer;
    FOnBufferDone : TOnBufferDone;
    function GetBuffer : Pointer;
    procedure SetBuffer(v : Pointer);
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    (* Property: DataBuffer
      Use this property to assign a pointer pointing to a data block, audio
      data will be read from. The data block IS NOT created by this component.
      You create it and fill it with data. You must not free the memory block
      pointed to by DataBuffer until you get an OnDone event from the output
      component that reads from this input. The memory block pointed to by
      DataBuffer should be <DataSize> in length. *)
    property DataBuffer : Pointer read GetBuffer write SetBuffer;
    (* Property: DataSize
      Use this property to set the size of the <DataBuffer> in bytes. *)
    property DataSize : LongWord read FDataSize write FDataSize;
  published
    (* Property: InBitsPerSample
      Use this property to tell the component the number of bits per sample
      for the audio data stored in the <DataBuffer>. *)
    property InBitsPerSample : LongWord read GetBPS write FBPS;
    (* Property: InChannels
      Use this property to tell the component the number of channels for the
      audio data stored in the <DataBuffer>. *)
    property InChannels : LongWord read GetCh write FChan;
    (* Property: InSampleRate
      Use this property to tell the component the sample rate of the audio
      data stored in the <DataBuffer>. *)
    property InSampleRate : LongWord read GetSR write FSR;
    (* Property: RepeatCount
      Use this property to tell the component how many times the contents of
      the <DataBuffer> should be replayed before the component reports the end
      of data. The default value for this property is 1. If this property is
      set to -1 the component will replay the buffer endlessly until it is
      stopped. *)
    property RepeatCount : Integer read FRepeatCount write FRepeatCount;
    (* Property: OnBufferDone
      This event is raised when the comoponent has played its current buffer contents and is about to report the end of input.
      Using this property you can renew data buffer so that the component continues playback.
      The event hander arguments are the pointer to the new buffer, its length and the repeat count.
      You can assign new values to these arguments or leave the previous ones.
      If <RepeatCount> is greater than 1, the OnBufferDone event handler is called only after the contents of the buffer has been repeated <RepeatCount> number of times.
      The event handler will never be called if <RepeatCount> is -1. *)
    property OnBufferDone : TOnBufferDone read FOnBufferDone write FOnBufferDone;
  end;

    (* Class: TAudioProcessor
    This component allows you to perform your own audio processing without writing  your own converter component.
    You put TAudioProcessor exemplar into audio-processing chain just like any other converter, and assign handlers to its events to perform processing.
    You don't have to assign handler to all events, but only to those that are actually needed.
     See AudioProcessorDemo for an example of its usage.
    *)
  TAudioProcessor = class(TAuConverter)
  private
    FOnInit : TAudioProcessorInitEvent;
    FOnFlush : TAudioProcessorFlushEvent;
    FOnGetData : TGetDataEvent;
    FOnGetSampleRate : TGetParameterEvent32;
    FOnGetBitsPerSample : TGetParameterEvent32;
    FOnGetChannels : TGetParameterEvent32;
    FOnGetTotalTime : TGetParameterEvent32;
    FOnGetSize : TGetParameterEvent64;
  protected
    function GetBPS : LongWord; override;
    function GetCh : LongWord; override;
    function GetSR : LongWord; override;
    function GetTotalTime : LongWord; override;
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
  published
  (* Property: OnFlush
    This event is raised when the audio chain is about to close. In this event handler you can free any additional resources you have aloocated in the <OnInit> event handler.
    The minimal example of this event handler may look like this:
    > procedure TForm1.AudioProcessor1Flush(Sender: TComponent);
    > begin
    >   TAudioProcessor(Sender).Input.Flush;
    > end;
    Here we just call the TAudioProcessor's input component's Flush method.
    *)
    property OnFlush : TAudioProcessorFlushEvent read FOnFlush write FOnFlush;
    (* Property: OnGetBitsPerSample
        This event is raised when the component for which TAudioProcessor is an input wants to know
        the number of bits per sample provided by the TAudioProcessor. Possible values are 8, 16, 24, and 32.
        You should set this event handler only if your TAudioProcessor changes the number of bits per sample regarding its input.
        For example, if your TAudioProcessor converts all of its input to 32 bps then the handler may look like this:
        > procedure TForm1.AudioProcessor1GetBitsPerSample(Sender: TComponent;
        >   var Param: Cardinal);
        > begin
        >   Param := 32;
        > end;
    *)
    property OnGetBitsPerSample : TGetParameterEvent32 read FOnGetBitsPerSample write FOnGetBitsPerSample;
    property OnGetChannels : TGetParameterEvent32 read FOnGetChannels write FOnGetChannels;
   (* Property: OnGetData
      This event is raised when the component for which TAudioProcessor is an input wants to get some data from it.
      The Buffer variable is the one where you should place the pointer to the data buffer, and the Bytes variable is the one where
      you should return the number of bytes in the buffer. When event is called the Bytes variable contains the number of bytes the output component wants from TaudioProcessor.
      The actual number of bytes returned by this handler (and stored in the Bytes variable) may be smaller (but not larger) than the number requested.
      Set the Buffer to nil and the Bytes to 0 if you want to indicate that there is no more data.
      Remember, that after you have set the Buffer to nil and the Bytes to 0 the event handler may be called again, and you should always reutn nil/0 until
      <OnFlush> event is called.
      The minimal OngetDataHandler could look like this:
      > procedure TForm1.AudioProcessor1GetData(Sender: TComponent;
      >    var Buffer: Pointer; var Bytes: Cardinal);
      > begin
      >   TAudioProcessor(Sender).Input.GetData(Buffer, Bytes);
      > end;
      In this example we just read the data from TAudioProcessor's input and pass it along the audio processing chain.
      In real life you will want to perform some modifications of the obtained data.
      Note that InputComponent.GetData may set Buffer to nil and Bytes to 0, indicating that the input component has no more data.
    *)
    property OnGetData : TGetDataEvent read FOnGetData write FOnGetData;
    property OnGetSampleRate : TGetParameterEvent32 read FOnGetSampleRate write FOnGetSampleRate;
    property OnGetSize : TGetParameterEvent64 read FOnGetSize write FOnGetSize;
    property OnGetTotalTime : TGetParameterEvent32 read FOnGetTotalTime write FOnGetTotalTime;
   (* Property: OnInit
    This event is raised when audio-chain needs to initialize the TAudioProcessor. In the event's handler you can perform whatever is necessry to do before
    the TAudioProcessor can process data.
    The minimal example of this event handler may look like this:
    > procedure TForm1.AudioProcessor1Init(Sender: TComponent; var TotalSize: Int64);
    > begin
    >   TAudioProcessor(Sender).Input.Init;
    >    TotalSize := TAudioProcessor(Sender).Input.Size
    > end;
    Here we initialize the TAudioProcessor's input component and pass its total data size to the TotalSize parameter.
    If the TAudioProcessor changes the data size regarding its input (as for example, a resampler or a stereo to mono converter would do)
    Write the modified value to the TotalSize.
    If you don't know the TAudioProcessor's total data output size, store -1 in the TotalSize parameter.
    *)
    property OnInit : TAudioProcessorInitEvent read FOnInit write FOnInit;
  end;

  {$IFDEF LINUX}

  TMPEGIn = class(TACSFileIn)
  private
    _M : Pointer;
    buf : array[1..BUF_SIZE] of Byte;  // ring buffer
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
  end;

  TAOLive = class(TACSOutput)
  private
    _device : PAODevice;
    Buffer : array [0..BUF_SIZE-1] of Byte;
    FVolume : Byte;
    FDrivers : TStringList;
    FCurrentDriver, FDefaultDriver : String;
    procedure SetDriver(const aDriver : String);
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsDevicePlayable(const Dev : String) : Boolean;
    property DefaultDriver : String read FDefaultDriver;
    property Driver : String read FCurrentDriver write SetDriver;
    property Drivers : TStringList read FDrivers;
  published
    property Volume : Byte read FVolume write FVolume stored True;
  end;

{$ENDIF}

  TNULLOut = class(TAuOutput)
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  end;


  TPlayItemChangedEvent = procedure(Sender : TComponent) of object;

 (* Class: TAudioPlayList
    A descendant of <TAuConverter> which enables input comonents to play series of files without gaps.
    Suppose you want to build a chain that plays several mp3 files from a predefined list.
    Your chain may look like this:

    TMP3In - >> TAudioPlayList - >> TDxAudioOut

    Now you assign the list of mp3s to be played to TAudioPlayList's <Files> property and start playback.
    TAudioPlayList will manage the files that TMP3In will play.
    There are some limitations with this component however.
    All the input files should have the same audio data parameters (sample rates, bits per sample, number of channels).
    No file name is assigned to the input component before you start playback, so you should not enquire about any properties of the file input being playd before you have started the playback.
    It is ok to check the input file properties from the <OnPlayItemChanged> event handler.
    TAudioPlayList allows you to construct play lists from the files of the same format. See AudioPlayer demo on how to make play lists of the files of different formats. *)

  TAudioPlayList = class(TAuConverter)
  private
    FFiles : TStringList;
    FCurrentItem : Word;
    FOnPlayItemChanged : TPlayItemChangedEvent;
    procedure SetFiles(f : TStringList);
    procedure SetCurrentItem(ci : Word);
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    (* Property: Files
      The list of files to be played. The list elements should be full file paths. You can edit this list on the fly while performing playback. *)
    property Files : TStringList read FFiles write SetFiles stored True;
    (* Property: CurrentItem
      The index of the play list element currently being played (or the element that will be played when you start playback).
      The first item in the play list has an index of zero.
      This value changes when the play list switches to the new element.
      You can change this value to switch the file being played. *)
    property CurrentItem : Word read FCurrentItem write SetCurrentItem;
    (* Property: OnPlayItemChanged
      This event is triggered when the new item from the playlist starts to play.
      It is safe to get input file properties from this event's handler.  *)
     property OnPlayItemChanged : TPlayItemChangedEvent read FOnPlayItemChanged write FOnPlayItemChanged;
  end;

  TCueItem = record
    Performer : AnsiString;
    Title : AnsiString;
    BeginIndex : LongWord;
    EndIndex : LongWord;
  end;

 (* Class: TCueSplitter
    Descends from <TAuConverter>.
    This class splits long audiotracks containing many compositions into fragments according to the information obtained
    from a cue-sheet file. The TCueSplitter should be put into audio-processing chain after the input component that reads the audiofile to be split.
    For example, the audio chain to split a long flac into separate flac-encoded fragments foloowing the cue-sheeit file might look like this:

    TFLACIn - >> TCueSplitter - >> TFLACOut

    This component expects ints input to be a seekable TAuFileIn descending component.
    Set  the input component's <FileName> property to the name of the multi-composition file described by the cue-sheet.
    Set TCueSplitter's <CueFile> property to the name of the appropriaate cue-sheet file and set the composition to be extracted to the <CurrentItem> property.
  *)

  TCueSplitter = class(TAuConverter)
  private
    FItems : array of TCueItem;
    FCueFile : AnsiString;
    FCurrentItem : Byte;
    FAlbum : AnsiString;
    FPerformer : AnsiString;
    FYear : AnsiString;
    FGenre : AnsiString;
    FItemsCount : Byte;
    procedure ParseCue;
    procedure SetCurrentItem(ci : Byte);
    function GetAlbum : AnsiString;
    function GetPerformer : AnsiString;
    function GetTitle : AnsiString;
    function GetItemsCount : Byte;
    function GetTime : LongWord;
    function GetYear : AnsiString;
    function GetGenre : AnsiString;
    procedure SetCueFile(const fn : AnsiString);
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    destructor Destroy; override;
    (* Property: ItemsCount
      The total number of tracks in the cue-sheet.
   *)
    property ItemsCount : Byte read GetItemsCount;
    (* Property: CurrentItem
      The index of the item to be extracted.
      The valid values range from 0 to ItemsCount-1.
   *)
    property CurrentItem : Byte read FCurrentItem write SetCurrentItem;
    (* Property: Album
      Returns the name of the album (The cue-sheet header title)
   *)
    property Album : AnsiString read GetAlbum;
    (* Property: Performer
      Returns the name of the current performer
   *)
    property Performer : AnsiString read GetPerformer;
    (* Property: Performer
      Returns the name of the current composition
   *)
    property Title : AnsiString read GetTitle;
    (* Property: Time
      Returns the current composition duration in seconds.
   *)
    property Time : LongWord read GetTime;
    (* Property: Year
      Returns the current composition duration in seconds.
   *)
    property Year : AnsiString read GetYear;
    (* Property: Year
      Returns the current composition duration in seconds.
   *)
    property Genre : AnsiString read GetGenre;
  published
    (* Property: CueFile
      The name of the cue-sheet file. *)
    property CueFile : AnsiString read FCueFile write SetCueFile;
  end;

  (* Class: TGainAnalysis
    Descends from <TAuConverter>.
    This component calculates its input's replay gain (like in mp3gain utility) and peak values. The component relies on libgain.dll which uses the same code as mp3gain does.
    This is a converter componen that you can connect to some output component or to TNULLOutput. See the ReplayGain demo on how to use it.
  *)

  TGainAnalysis = class(TAuConverter)
  private
    FTitleGain : Double;
    FAlbumGain : Double;
    FTitlePeak : Double;
    FAlbumPeak : Double;
    FNewAlbum : Boolean;
    InBuffer : array of Single;
    LBuffer, RBuffer : array of Double;
    FBufferSize : LongWord;
    FSampleSize : Word;
    function GetAlbumGain : Double;
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    (* Function: NewAlbum
      Call this method before starting to process a new batch of audio files. You must call this method before performing any other operation on the component. *)
    procedure NewAlbum;
    (* Property: TitleGain
      Returns the replay gain (in dB) for the last processed file. The best place to analyze this value is the output component's OnDone event handler. *)
    property TitleGain : Double read FTitleGain;
    (* Property: AlbumGain
      Returns the replay gain (in dB) for the batch of files processed since the last call to <NewAlbum>.  *)
    property AlbumGain : Double read GetAlbumGain;
    (* Property: TitlePeak
      Returns the peak value for the last processed file in percents of the maximum possible value. The best place to analyze this value is the output component's OnDone event handler. *)
    property TitlePeak : Double read FTitlePeak;
    (* Property: AlbumPeak
      Returns the peak value for the batch of files processed since the last call to <NewAlbum>. *)
    property AlbumPeak : Double read FAlbumPeak;
  end;

  (* Class: TTagEditor
    Descends from <TComponent>.
    This component allows you to create, read, and edit tags for a wide variety of audio files. Supported file formts include mp3, ogg, FLAC, a4m, wavpack, wma (reading only). The file type is guessed by its extension.
    The component requires libtag.dll and libtag_c.dll.
  *)

  TTagEditor = class(TComponent)
  private
    FFileName : AnsiString;
    FTitle : WideString;
    FAlbum : WideString;
    FArtist : WideString;
    FGenre : WideString;
    FYear : WideString;
    FTrack : WideString;
    FDuration : LongWord;
    FBitrate : LongWord;
    FValid : Boolean;
    procedure SetFileName(const aFileName : AnsiString);
    function GetFileExt : ANSIString;
  published
    (* Property: FileName
      Sets the name of the file for editing. The file must exist and have a valid format.  *)
    property FileName : AnsiString read FFileName write SetFileName;
  public
    (* Property: FileName
      Read or sets the Title tag.  *)
    property Title : WideString read FTitle write FTitle;
    (* Property: FileName
      Read or sets the Album tag.  *)
    property Album : WideString read FAlbum write FAlbum;
    (* Property: FileName
      Read or sets the Artist tag.  *)
    property Artist : WideString read FArtist write FArtist;
    (* Property: FileName
      Read or sets the Genre tag.  *)
    property Genre : WideString read FGenre write FGenre;
    (* Property: FileName
      Read or sets the Year tag.  *)
    property Year : WideString read FYear write FYear;
    (* Property: FileName
      Read or sets the Track tag.  *)
    property Track : WideString read FTrack write FTrack;
    (* Property: FileName
      Returns the file duration in seconds.  *)
    property Duration : LongWord read FDuration;
    (* Property: FileName
      Returns the file bitrate.  *)
    property Bitrate : LongWord read FBitrate;
    (* Property: FileName
      Returns True if the file is valid.  *)
    property Valid : Boolean read FValid;
    (* Function: Save
      Saves the edited tags to the file. *)
    procedure Save;
  end;


implementation

{$IFDEF LINUX}

var
  AOInitialized : Integer = 0;

{$ENDIF}

  constructor TMemoryIn.Create;
  begin
    inherited Create(AOwner);
    FRepeatCount := 1;
  end;

  destructor TMemoryIn.Destroy;
  begin
    inherited Destroy;
  end;

  function TMemoryIn.GetBPS;
  begin
    if (FBPS in [8, 16, 24, 32]) = False  then FBPS := 16;
    Result := FBPS;
  end;

  function TMemoryIn.GetCh;
  begin
    Result := FChan;
  end;

  function TMemoryIn.GetSR;
  begin
    if (FSR < 2000) or (FSR > 96000) then FSR := 8000;
    Result := FSR;
  end;

  procedure TMemoryIn.InitInternal;
  begin
    FPosition := 0;
    BufEnd := FDataSize;
    BufStart := 0;
    if FRepeatCount >= 0 then
    {$WARNINGS OFF}
      FSize := FDataSize*FRepeatCount
    else
    {$WARNINGS ON}
      FSize := -1;
    Busy := True;
  end;

  procedure TMemoryIn.FlushInternal;
  begin
    Busy := False;
    FDataSize := 0;
  end;

  procedure TMemoryIn.GetDataInternal;
  begin
    if not Busy then  raise EAuException.Create('The Stream is not opened');
    if not Assigned(FBuffer) then
    begin
      Bytes := 0;
      Buffer := nil;
      Exit;
    end;
    if BufStart >= BufEnd then
    begin
      if (FDataSize = 0) or ((FSize > 0) and (FPosition >= FSize)) then
      begin
        if Assigned(FOnBufferDone) then
        begin
          FOnBufferDone(Self, Pointer(FBuffer), FDataSize, FRepeatCount);
          {$WARNINGS OFF}
          if (FBuffer = nil) or (FDataSize*FRepeatCount = 0) then
          {$WARNINGS ON}
          begin
            Bytes := 0;
            Buffer := nil;
            Exit;
          end;
        end else
        begin
          Bytes := 0;
          Buffer := nil;
          Exit;
        end;
      end;
      BufStart := 0;
    end;
    if Bytes > (BufEnd - BufStart) then
      Bytes := BufEnd - BufStart;
    Buffer := @FBuffer[BufStart];
    Inc(BufStart, Bytes);
    Inc(FPosition, Bytes);
  end;

  function TMemoryIn.GetBuffer;
  begin
    Result := Pointer(FBuffer);
  end;

  procedure TMemoryIn.SetBuffer;
  begin
    FBuffer := PBuffer(v);
  end;


{$IFDEF LINUX}

  constructor TMPEGIn.Create;
  begin
    inherited Create(AOwner);
  end;

  destructor TMPEGIn.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TMPEGIn.Init;
  begin
    inherited Init;
    SMPEG_play(_M);
  end;

  procedure TMPEGIn.OpenFile;
  var
    info : SMPEG_info;
    spec : SDL_AudioSpec;
  begin
    if FOpened = 0 then
    begin
      LoadLibrary;
      (* the next call is needed just to make sure
        the SDL library is loaded *)
      _M := SMPEG_new(PChar(FFileName), info, 1);
      SMPEG_delete(_M);
      FValid := True;
      _M := SMPEG_new(PChar(FFileName), info, 0);
      if info.has_audio <> 1 then
      begin
        FValid := False;
        Exit;
      end;
      FTime := Round(info.total_time);
      SMPEG_wantedSpec(_M, spec);
      FSR := spec.freq;
      FBPS := 16;
      FChan := spec.channels;
      FSize := FTime*2*FChan*FSR;
    end;
    Inc(FOpened);
  end;

  procedure TMPEGIn.CloseFile;
  begin
    if FOpened = 1 then
    begin
      if SMPEG_status(_M) = SMPEG_PLAYING then
      SMPEG_stop(_M);
      SMPEG_delete(_M);
      UnloadLibrary;
    end;
    if FOpened > 0 then Dec(FOpened);
  end;

  function TMPEGIn.GetData;
  var
    l, offs : Integer;
    tmp : Single;
  begin
    if not Busy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      if FOffset <> 0 then
      begin
        offs := Round((FOffset/100)*FSize);
        FPosition := FPosition + offs;
        if FPosition < 0 then FPosition := 0
        else if FPosition > FSize then FPosition := FSize;
        if FOffset < 0 then
        begin
          SMPEG_rewind(_M);
          SMPEG_play(_M);
          tmp := (FPosition/FSize)*FTime;
          SMPEG_skip(_M, tmp);
        end;
        tmp := (FOffset/100)*FTime;
        SMPEG_skip(_M, tmp);
        FOffset := 0;
      end;
      BufStart := 1;
      FillChar(Buf, SizeOf(Buf), 0);
      l := SMPEG_playAudio(_M, @Buf[1], BUF_SIZE);
      if l = 0 then
      begin
        if FLoop then
        begin
          Flush;
          Init;
//          SMPEG_rewind(_M);
//          SMPEG_play(_M);
//          FPosition := 0;
          l := SMPEG_playAudio(_M, @Buf[1], BUF_SIZE);
        end else
        begin
          Result := 0;
          Exit;
        end;
      end;
      BufEnd := l;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(Buf[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
  end;

  constructor TAOLive.Create;
  var
    DrList : PPAOInfo;
    DrCount, i : Integer;
    Info : PAOInfo;
  begin
    if not LibaoLoaded then
    raise EACSException.Create(LibaoPath + ' library could not be loaded.');
    inherited Create(AOwner);
    if AOInitialized = 0 then
    ao_initialize;
    Inc(AOInitialized);
    FDrivers := TStringList.Create;
    DrList := ao_driver_info_list(DrCount);
    for i := 0 to DrCount-1 do
    begin
      if DrList^._type = AO_TYPE_LIVE then
      begin
        FDrivers.Add(String(DrList^.short_name));
      end;
      Inc(DrList);
    end;
    Info := ao_driver_info(ao_default_driver_id);
    FDefaultDriver := Info.short_name;
    FVolume := 255;
  end;

  destructor TAOLive.Destroy;
  begin
    FDrivers.Free;
    if AOInitialized = 1 then
    ao_shutdown;
    Dec(AOInitialized);
    inherited Destroy;
  end;

  procedure TAOLive.Prepare;
  var
    did : Integer;
    sf : ao_sample_format;
    opt : PAOOption;
    Info : PAOInfo;
  begin
    FInput.Init;
    if FCurrentDriver = '' then
    begin
      did := ao_default_driver_id;
      Info := ao_driver_info(did);
      FCurrentDriver := Info.short_name;
    end
    else did := ao_driver_id(@FCurrentDriver[1]);
    opt := nil;
    sf.bits := Finput.BitsPerSample;
    sf.rate := Finput.SampleRate;
    sf.channels := Finput.Channels;
    sf.byte_format := AO_FMT_NATIVE;
    _device := ao_open_live(did, @sf, opt);
    FreeOptionsList(Opt);
    if _device = nil then
    raise EACSException.Create('Cannot play on the "'+FCurrentDriver+'" device.');
  end;

  procedure TAOLive.Done;
  begin
    Finput.Flush;
    if _device <> nil then
    ao_close(_device);
  end;

function TAOLive.DoOutput;
var
  Len, i : Integer;
  P : Pointer;
  P1 : PBuffer8;
  P2 : PBuffer16;
begin
  // No exceptions Here
  Result := True;
  if not CanOutput then Exit;
  Len := 0;
  if Abort then
  begin
    ao_close(_device);
    _device := nil;
    Result := False;
    Exit;
  end;
  try
    P := @Buffer[0];
    Len := Finput.GetData(P, BUF_SIZE);
    if FVolume < 255 then
    begin
      if FInput.BitsPerSample = 16 then
      begin
        P2 := @Buffer[0];
        for i := 0 to (Len shr 1) -1 do
        P2[i] := Round(P2[i]*(FVolume/255));
      end else
      begin
        P1 := @Buffer[0];
        for i := 0 to Len - 1 do
        P1[i] := Round(P1[i]*(FVolume/255));
      end;
    end;
    ao_play(_device, P, Len);
  except
  end;
  if Len > 0 then Result := True
  else Result := False;
end;

  procedure TAOLive.SetDriver;
  begin
    if IsDevicePlayable(aDriver) then
    FCurrentDriver := aDriver;
  end;

  function TAOLive.IsDevicePlayable;
  var
    i, did : Integer;
    sf : ao_sample_format;
    opt : PAOOption;
  begin
    Result := True;
    if Dev = '' then Exit;
    if Busy then
    raise EACSException.Create('Component is busy.');
    for i := 0 to FDrivers.Count-1 do
    if FDrivers.Strings[i] = Dev then
    begin
      did := ao_driver_id(@Dev[1]);
      sf.bits := 16;
      sf.rate := 22050;
      sf.channels := 2;
      sf.byte_format := AO_FMT_NATIVE;
      opt := nil;
      _device := ao_open_live(did, @sf, opt);
      if _device <> nil then
      begin
        ao_close(_device);
        FreeOptionsList(Opt);
        Exit;
      end else Break;
    end;
    Result := False;
  end;

{$ENDIF}


  function TAudioProcessor.GetBPS;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetBitsPerSample) then FOnGetBitsPerSample(Self, Result) else
    Result := FInput.BitsPerSample;
  end;

  function TAudioProcessor.GetSR;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetSampleRate) then FOnGetSampleRate(Self, Result) else
    Result := FInput.SampleRate;
  end;

  function TAudioProcessor.GetCh;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetChannels) then FOnGetChannels(Self, Result) else
    Result := FInput.Channels;
  end;

  function TAudioProcessor.GetTotalTime;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetTotalTime) then FOnGetTotalTime(Self, Result) else
    Result := FInput.TotalTime;
  end;

  procedure TAudioProcessor.GetDataInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnGetData) then FOnGetData(Self, Buffer, Bytes)
    else
    FInput.GetData(Buffer, Bytes);
  end;

  procedure TAudioProcessor.InitInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnInit) then FOnInit(Self, FSize)
    else
    begin
      FInput.Init;
      if Assigned(FOnGetSize) then FOnGetSize(Self, FSize)
      else FSize := Finput.Size;
    end;
    Busy := True;
    FPosition := 0;
  end;

  procedure TAudioProcessor.FlushInternal;
  begin
    if not Assigned(FInput) then
    raise EAuException.Create('Input is not assigned.');
    if Assigned(FOnFlush) then FOnFlush(Self)
    else FInput.Flush;
    Busy := False;
  end;

procedure TNULLOut.Prepare;
begin
  if not Assigned(FInput) then
  raise EAuException.Create('Input is not assigned.');
  FInput.Init;
end;

function TNULLOut.DoOutput;
var
  Res : LongWord;
  Ptr : Pointer;
begin
  Result := True;
  if not Busy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  Res := BUF_SIZE;
  Finput.GetData(Ptr, Res);
  if Res > 0 then Result := True
  else
  begin
    Result := False;
    Exit;
  end;
end;

procedure TNULLOut.Done;
begin
  FInput.Flush;
end;

constructor TAudioPlayList.Create(AOwner: TComponent);
begin
  inherited;
  FFiles := TStringList.Create;
end;

destructor TAudioPlayList.Destroy;
begin
  FFiles.Free;
  inherited;
end;

procedure TAudioPlayList.SetFiles(f: TStringList);
begin
  FFiles.Assign(f);
end;

procedure TAudioPlayList.SetCurrentItem(ci: Word);
begin
  if (ci <> 0) and (ci > FFiles.Count - 1) then Exit;
  FCurrentItem := ci;
  if Busy then
  begin
    _Lock;
    FInput.Flush;
    TAuFileIn(Finput).FileName := FFiles.Strings[FCurrentItem];
    FInput.Init;
    _Unlock;
    if Assigned(FOnPlayItemChanged) then
      EventHandler.PostGenericEvent(Self, FOnPlayItemChanged);
  end;
end;

procedure TAudioPlayList.InitInternal;
begin
  if FInput = nil then
    raise EAuException.Create('Input is not assigned.');
  if not (Finput is TAuFileIn) then
    raise EAuException.Create('PlayList input should be a file-reading component.');
  Busy := True;
  if FCurrentItem < FFiles.Count - 1 then
        TAuFileIn(Finput).FileName := FFiles.Strings[FCurrentItem]
  else
       TAuFileIn(Finput).FileName := FFiles.Strings[0];
  FInput.Init;
  if Assigned(FOnPlayItemChanged) then
      EventHandler.PostGenericEvent(Self, FOnPlayItemChanged);
  FSize:= -1;
end;

procedure TAudioPlayList.FlushInternal;
begin
  Finput.Flush;
  Busy := False;
end;

procedure TAudioPlayList.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
var
  TmpBytes : LongWord;
begin
  TmpBytes := Bytes;
  FInput.GetData(Buffer, Bytes);
  if Bytes = 0 then
  begin
    if FCurrentItem < FFiles.Count - 1 then
    begin
      Finput.Flush;
      Inc(FCurrentItem);
      TAuFileIn(Finput).FileName := FFiles.Strings[FCurrentItem];
      Finput.Init;
      Bytes := TmpBytes;
      FInput.GetData(Buffer, Bytes);
      if Assigned(FOnPlayItemChanged) then
        EventHandler.PostGenericEvent(Self, FOnPlayItemChanged);
    end;
  end;
end;

destructor TCueSplitter.Destroy;
begin
  FItems := nil;
  inherited;
end;

procedure TCueSplitter.ParseCue;
const
  beforeTracks = 1;
  NewTrack = 2;
var
  SL : TStringList;
  State : Integer;
  i, p, pos : Integer;
  Min, sec, millisec : Integer;
function ExtractString(const S : String; pos : Integer) : String;
var
  i : Integer;
  ksp : Boolean;
begin
  Result := '';
  ksp := False;
  for i:= pos to Length(S) do
  begin
    if S[i] < Char(33) then
    begin
      if ksp then Result := Result + S[i];
    end else
    begin
      if S[i] = '"' then ksp := True
      else Result := Result + S[i];
    end;
  end;
end;

function ExtractDigits(const S : String; var pos : Integer) : String;
var
  i : Integer;
  ind : Boolean;
begin
  Result := '';
  ind := False;
  for i:= pos to Length(S) do
  begin
    if
    {$IFDEF UNICODE}
     CharInSet(S[i], ['0'..'9'])
    {$ELSE}
     S[i] in ['0'..'9']
    {$ENDIF}
    then
    begin
      ind := True;
      Result := Result + S[i];
    end else
    begin
      if ind then
      begin
        pos := i;
        Exit;
      end;
    end;
  end;
end;

begin
  if FItemsCount > 0 then
    Exit;
  FPerformer := '';
  FAlbum := '';
  FYear := '';
  FGenre := '';
  SetLength(FItems, 255);
  FItemsCount := 0;
  SL := TStringList.Create;
  if FCueFile = '' then
     raise EAuException.Create('Cue-file not set');
  SL.LoadFromFile(String(FCueFile));
  State := beforeTracks;
  for i := 0 to SL.Count - 1 do
  begin
    case State of
      beforeTracks:
      begin
        p := AnsiPos('PERFORMER', SL.Strings[i]);
        if p > 0 then
        begin
          FPerformer := AnsiString(ExtractString(SL.Strings[i], p + 9));
        end;
        p := AnsiPos('TITLE', SL.Strings[i]);
        if p > 0 then
        begin
          FAlbum := AnsiString(ExtractString(SL.Strings[i], p + 5));
        end;
        p := AnsiPos('GENRE', SL.Strings[i]);
        if p > 0 then
        begin
          FGenre := AnsiString(ExtractString(SL.Strings[i], p + 5));
        end;
        p := AnsiPos('DATE', SL.Strings[i]);
        if p > 0 then
        begin
          pos := p + 4;
          FYear := AnsiString(ExtractDigits(SL.Strings[i], pos));
        end;
        p := AnsiPos('YEAR', SL.Strings[i]);
        if p > 0 then
        begin
          pos := p + 4;
          FYear := AnsiString(ExtractDigits(SL.Strings[i], pos));
        end;
        p := AnsiPos('TRACK', SL.Strings[i]);
        if p > 0 then
        begin
          State := NewTrack;
          FItems[FItemsCount].BeginIndex := 0;
          FItems[FItemsCount].EndIndex := 0;
          FItems[FItemsCount].Performer := FPerformer;
          Inc(FItemsCount);
        end;
      end;
      NewTrack :
      begin
        p := AnsiPos('INDEX', SL.Strings[i]);
        if p > 0 then
        begin
          pos := p + 5;
          if StrToInt(ExtractDigits(SL.Strings[i], pos)) = 0 then
          begin
            if FItemsCount > 1 then
            begin
              Inc(pos);
              Min := StrToInt(ExtractDigits(SL.Strings[i], pos));
              Inc(pos);
              Sec := StrToInt(ExtractDigits(SL.Strings[i], pos));
              Inc(pos);
              MilliSec := Round(StrToInt(ExtractDigits(SL.Strings[i], pos))*1000/75);
              MilliSec := Min*60000 + Sec*1000 + MilliSec;
              FItems[FItemsCount-2].EndIndex := MilliSec;
            end;
          end else
          begin
            Inc(pos);
            Min := StrToInt(ExtractDigits(SL.Strings[i], pos));
            Inc(pos);
            Sec := StrToInt(ExtractDigits(SL.Strings[i], pos));
            Inc(pos);
            MilliSec := Round(StrToInt(ExtractDigits(SL.Strings[i], pos))*1000/75);
            MilliSec := Min*60000 + Sec*1000 + MilliSec;
            FItems[FItemsCount-1].BeginIndex := MilliSec;
            if FItemsCount > 1 then
              if FItems[FItemsCount-2].EndIndex = 0 then
                FItems[FItemsCount-2].EndIndex := MilliSec;
          end;
        end;
        p := AnsiPos('PERFORMER', SL.Strings[i]);
        if p > 0 then
        begin
          FItems[FItemsCount-1].Performer := AnsiString(ExtractString(SL.Strings[i], p + 9));
        end;
        p := AnsiPos('TITLE', SL.Strings[i]);
        if p > 0 then
        begin
          FItems[FItemsCount-1].Title := AnsiString(ExtractString(SL.Strings[i], p + 5));
        end;
        p := AnsiPos('TRACK', SL.Strings[i]);
        if p > 0 then
        begin
          FItems[FItemsCount].BeginIndex := 0;
          FItems[FItemsCount].EndIndex := 0;
          FItems[FItemsCount].Performer := FPerformer;
          Inc(FItemsCount);
        end;
      end;
   end;
  end;
  SL.Free;
end;

procedure TCueSplitter.SetCurrentItem(ci: Byte);
begin
  ParseCue;
  if (ci = 0) or (ci < ItemsCount)  then
    FCurrentItem := ci
  else
    raise EAuException.Create('Item index out of bounds');
end;

function TCueSplitter.GetItemsCount;
begin
  ParseCue;
  Result :=  FItemsCount;
end;

function TCueSplitter.GetAlbum;
begin
  ParseCue;
  Result := FAlbum;
end;

function TCueSplitter.GetYear;
begin
  ParseCue;
  Result := FYear;
end;

function TCueSplitter.GetGenre;
begin
  ParseCue;
  Result := FGenre;
end;



function TCueSplitter.GetPerformer;
begin
  ParseCue;
  Result := FItems[FCurrentItem].Performer;
end;

function TCueSplitter.GetTitle;
begin
  ParseCue;
  Result := FItems[FCurrentItem].Title;
end;

function TCueSplitter.GetTime;
var
  EI : LongWord;
begin
  ParseCue;
  if FItems[FCurrentItem].EndIndex = 0 then
  begin
    if Assigned(Finput) then
    begin
      EI := (Finput.TotalSamples div Finput.SampleRate)*1000;
      Result := (EI - FItems[FCurrentItem].BeginIndex) div 1000;
    end else
      Result := 0;
  end else
    Result := (FItems[FCurrentItem].EndIndex - FItems[FCurrentItem].BeginIndex) div 1000;
end;

procedure TCueSplitter.SetCueFile(const fn: AnsiString);
begin
  FItems := nil;
  FItemsCount := 0;
  FCurrentItem := 0;
  FCueFile := fn;
end;

procedure TCueSplitter.InitInternal;
var
  FSR : LongWord;
begin
  if not Assigned(FInput) then
  raise EAuException.Create('Input is not assigned.');
  ParseCue;
  Busy := True;
  FPosition := 0;
  FSR := FInput.SampleRate;
  TAuSeekableStreamedInput(Finput).StartSample := Round(FItems[FCurrentItem].BeginIndex/1000*FSR);
  if FCurrentItem = FItemsCount -1 then
     TAuSeekableStreamedInput(Finput).EndSample := -1
  else
     TAuSeekableStreamedInput(Finput).EndSample := Round(FItems[FCurrentItem].EndIndex/1000*FSR);
  FInput.Init;
    if TAuSeekableStreamedInput(Finput).EndSample = -1 then
      FSize := FInput.Size - TAuSeekableStreamedInput(Finput).StartSample*(Finput.BitsPerSample div 8)*Finput.Channels + (Finput.BitsPerSample div 8)*Finput.Channels
    else
      FSize := TAuSeekableStreamedInput(Finput).EndSample*(Finput.BitsPerSample div 8)*Finput.Channels - TAuSeekableStreamedInput(Finput).StartSample*(Finput.BitsPerSample div 8)*Finput.Channels + (Finput.BitsPerSample div 8)*Finput.Channels;
end;

procedure TCueSplitter.GetDataInternal;
begin
   FInput.GetData(Buffer, Bytes);
//   Inc(FPosition, Bytes);
end;

procedure TCueSplitter.FlushInternal;
begin
  FInput.Flush;
  Busy := False;
end;

procedure TGainAnalysis.NewAlbum;
begin
  LoadLibGain;
  if not LibGainLoaded then
    raise EAuException.Create(Format('Could not load the %s library.', [LibGainPath]));
  InitGainAnalysis(44100);
  FAlbumGain := 0;
  FAlbumPeak := 0;
  FNewAlbum := True;
end;

procedure TGainAnalysis.InitInternal;
var
  SR : LongWord;
begin
  Busy := True;
  FPosition := 0;
  FInput.Init;
  if FInput.Channels > 2 then
  begin
    FInput.Flush;
    Busy := False;
    raise EAuException.Create('Only mono or stereo soures are supported.');
  end;
  SR := FInput.SampleRate;
  if ResetSampleFrequency(SR) <> GAIN_ANALYSIS_OK then
  begin
    FInput.Flush;
    Busy := False;
    raise EAuException.Create(Format('Failed to set up gain analysis. Possible cause: sample rate %d is not supported.', [SR]));
  end;
  FTitleGain := 0;
  FTitlePeak := 0;
  FSampleSize := FInput.BitsPerSample div 8;
end;

procedure TGainAnalysis.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
var
  i : Integer;
  SamplesRead, FramesRead : LongWord;
  AbsPeak : Single;
begin
  Finput.GetData(Buffer, Bytes);
  if (Buffer = nil) or (Bytes = 0) then
    Exit;
  SamplesRead := Bytes div FSampleSize;
  if SamplesRead > FBufferSize then
  begin
    FBufferSize := SamplesRead;
    SetLength(InBuffer, FBufferSize);
    SetLength(LBuffer, FBufferSize);
    SetLength(RBuffer, FBufferSize);
  end;
  case FSampleSize of
    1 : ByteToSingle(PBuffer8(Buffer), @InBuffer[0], SamplesRead);
    2 : SmallIntToSingle(PBuffer16(Buffer), @InBuffer[0], SamplesRead);
    3 : Int24ToSingle(PBuffer8(Buffer), @InBuffer[0], SamplesRead);
    4 : Int32ToSingle(PBuffer32(Buffer), @InBuffer[0], SamplesRead);
  end;
  for i := 0 to SamplesRead - 1 do
  begin
    AbsPeak := Abs(LBuffer[i]);
    if AbsPeak > FTitlePeak then
      FTitlePeak := AbsPeak;
  end;
  FramesRead := SamplesRead div FInput.Channels;
  if Finput.Channels = 2 then
  begin
    for i := 0 to FramesRead - 1 do
    begin
      RBuffer[i] := InBuffer[i*2]*$8000;
      LBuffer[i] := InBuffer[i*2+1]*$8000;
    end;
  end else
    for i := 0 to FramesRead - 1 do
      RBuffer[i] := InBuffer[i]*$8000;

  if AnalyzeSamples(@LBuffer[0], @RBuffer[0], FramesRead, FInput.Channels) = GAIN_ANALYSIS_ERROR then
    raise EAuException.Create('Gain analysis failed');
end;

procedure TGainAnalysis.FlushInternal;
begin
  FTitlePeak := FTitlePeak/327.68;
  if FTitlePeak > FAlbumPeak then
    FAlbumPeak := FTitlePeak;
  FTitleGain := GetTitleGain;
  Finput.Flush;
  Busy := False;
end;

function TGainAnalysis.GetAlbumGain;
begin
  if FNewAlbum then
  begin
    FNewAlbum := False;
    FAlbumGain := GetAlbumGain;
  end;
  Result := FAlbumGain;
end;

function TTagEditor.GetFileExt;
begin
  Result := AnsiStrLower(PAnsiChar(AnsiString(ExtractFileExt(String(FFileName)))));
end;

procedure TTagEditor.SetFileName(const aFileName: AnsiString);
const
  MaxAllowedExtIndex = 5;
  AllowedExtensions : array[0..5] of AnsiString = ('.mp3', '.wma', '.m4a', '.flac', '.wv', '.ogg');
var
  _File : PTagLib_File;
  _Tag : PTagLib_Tag;
  _AudioProperties : PTagLib_AudioProperties;
  S : Utf8String;
  Ext : ANSIString;
  sb, db : LongWord;
  Dest : WideString;
//  sr : wma_sync_reader;
  i : Integer;
begin
  FFileName := aFileName;
  if not (csDesigning in ComponentState) then
  begin
    LoadLibtag;
    if not LibtagLoaded then
      raise EAuException.Create(LibtagPath + ' could not be loaded');
    FTitle := '';
    FAlbum := '';
    FArtist := '';
    FGenre := '';
    FYear := '';
    FTrack := '';
    FDuration := 0;
    FBitrate := 0;
    if aFileName = '' then
      Exit;
    Ext := GetFileExt;
    FValid := False;
    for i := 0 to MaxAllowedExtIndex do
      if AllowedExtensions[i] = Ext then
      begin
        FValid := True;
        Break;
      end;
    if not FValid then Exit;
    taglib_id3v2_set_default_text_encoding(Ord(TagLib_ID3v2_UTF16));
    _File := taglib_file_new(PAnsiChar(FFileName));
    FValid := taglib_file_is_valid(_File);
    if FValid  then
    begin
 (*     if AnsiStrLower(PAnsiChar(AnsiString(ExtractFileExt(FFileName)))) = '.wma' then
      begin
        taglib_file_free(_File);
        Stream := TFileStream.Create(FFileName, fmOpenRead);
        lwma_reader_init1(sr, Stream);
        if sr.has_audio then
        begin
          FTitle := lwma_reader_get_title(sr);
          FAlbum := lwma_reader_get_album(sr);
          FArtist := lwma_reader_get_author(sr);
          FGenre := lwma_reader_get_genre(sr);
          FTrack :=  lwma_reader_get_track(sr);
          FYear := lwma_reader_get_year(sr);
          FDuration := lwma_reader_get_duration(sr) div 100;
          FBitrate := lwma_reader_get_bitrate(sr) div 1000;
        end;
        lwma_reader_free(sr);
        Stream.Free;
        Exit;
      end; *)
      _AudioProperties := taglib_file_audioproperties(_File);
      _Tag := taglib_file_tag(_File);
      S := taglib_tag_title(_Tag);
      sb := Length(S);
      Dest := '';
      db := 0;
      if sb <> 0 then db := sb + 2;
      SetLength(Dest, db);
      Utf8ToUnicode(@Dest[1], db, @S[1], sb);
      FTitle := Dest;
      S := taglib_tag_album(_Tag);
      sb := Length(S);
      Dest := '';
      db := 0;
      if sb <> 0 then db := sb + 2;
      SetLength(Dest, db);
      Utf8ToUnicode(@Dest[1], db, @S[1], sb);
      FAlbum := Dest;
      S := taglib_tag_artist(_Tag);
      sb := Length(S);
      Dest := '';
      db := 0;
      if sb <> 0 then db := sb + 2;
      SetLength(Dest, db);
      Utf8ToUnicode(@Dest[1], db, @S[1], sb);
      FArtist := Dest;
      S := taglib_tag_genre(_Tag);
      sb := Length(S);
      Dest := '';
      db := 0;
      if sb <> 0 then db := sb + 2;
      SetLength(Dest, db);
      Utf8ToUnicode(@Dest[1], db, @S[1], sb);
      FGenre := Dest;
      {$WARNINGS OFF}
      S := IntToStr(taglib_tag_year(_Tag));
      {$WARNINGS ON}
      sb := Length(S);
      Dest := '';
      db := 0;
      if sb <> 0 then db := sb + 2;
      SetLength(Dest, db);
      Utf8ToUnicode(@Dest[1], db, @S[1], sb);
      FYear := Dest;
      {$WARNINGS OFF}
      S := IntToStr(taglib_tag_track(_Tag));
      {$WARNINGS ON}
      sb := Length(S);
      Dest := '';
      db := 0;
      if sb <> 0 then db := sb + 2;
      SetLength(Dest, db);
      Utf8ToUnicode(@Dest[1], db, @S[1], sb);
      FTrack := Dest;
      FDuration := taglib_audioproperties_length(_AudioProperties);
      FBitrate := taglib_audioproperties_bitrate(_AudioProperties);
      taglib_tag_free_strings;
    end;
    taglib_file_free(_File);
  end;
end;

procedure TTagEditor.Save;
var
  _File : PTagLib_File;
  _Tag : PTagLib_Tag;
//  sr : wma_writer;
begin
  if FFileName <> '' then
  begin
    if GetFileExt = '.wma' then Exit;
    taglib_id3v2_set_default_text_encoding(Ord(TagLib_ID3v2_UTF16));
    _File := taglib_file_new(PAnsiChar(FFileName));
    _Tag := taglib_file_tag(_File);
    if taglib_file_is_valid(_File) then
    begin
(*      if AnsiStrLower(PAnsiChar(AnsiString(ExtractFileExt(FFileName)))) = '.wma' then
      begin
        taglib_file_free(_File);
        lwma_writer_init(sr, PWideChar(WideString(FFileName)), Self, nil);
        begin
          lwma_writer_set_title(sr, FTitle);
          lwma_writer_set_album(sr, FAlbum);
          lwma_writer_set_author(sr, FArtist);
          lwma_writer_set_genre(sr, FGenre);
          lwma_writer_set_track(sr, FTrack);
          lwma_writer_set_year(sr, FYear);
        end;
        sr.writer.Flush;
        lwma_writer_free(sr);
        Exit;
      end;*)
      taglib_tag_set_title(_Tag, PAnsiChar(Utf8Encode(FTitle)));
      taglib_tag_set_artist(_Tag, PAnsiChar(Utf8Encode(FArtist)));
      taglib_tag_set_album(_Tag, PAnsiChar(Utf8Encode(FAlbum)));
      taglib_tag_set_genre(_Tag, PAnsiChar(Utf8Encode(FGenre)));
      if FYear <> '' then
      begin
        try
          taglib_tag_set_year(_Tag, StrToInt(FYear));
        except
        end;
      end;
      if FTrack <> '' then
      begin
        try
          taglib_tag_set_track(_Tag, StrToInt(FTrack));
        except
        end;
      end;
      if not taglib_file_save(_File) then
        raise EAuException.Create('Failed to save tags');
      taglib_file_free(_File);
    end;
  end;
end;

end.
