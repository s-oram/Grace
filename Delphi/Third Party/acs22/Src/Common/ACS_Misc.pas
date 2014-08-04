(*
  This file is a part of Audio Components Suite v 2.2,
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_Misc;

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes
  {$IFDEF LINUX}
  , libc, smpeg, LibAO
  {$ENDIF};

const
  BUF_SIZE = $4000;

type

  TBuffer = array[0..0] of Byte;
  PBuffer = ^TBuffer;

  TOnBufferDone = procedure(Sender : TComponent) of object;

  TAudioProcessorInitEvent = procedure(Sender : TComponent; var TotalSize : Integer) of object;
  TAudioProcessorFlushEvent = procedure(Sender : TComponent) of object;

  TGetParameterEvent = procedure(Sender : TComponent; var Param : Integer) of object;

  TGetDataEvent = procedure(Sender : TComponent; Data : Pointer; var n : Integer) of object;

  TMemoryIn = class(TACSInput)
  private
    FBuffer : PBuffer;
    FDataSize : Integer;
    FOnBufferDone : TOnBufferDone;
    Buisy : Boolean;
    BufStart, BufEnd : Integer;
    FBPS, FSR, FChan : Integer;
    function GetBuffer : Pointer;
    procedure SetBuffer(v : Pointer);
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    property DataBuffer : Pointer read GetBuffer write SetBuffer;
    property DataSize : Integer read FDataSize write FDataSize;
  published
    property GlobalSize : Integer read FSize write FSize;
    property InBitsPerSample : Integer read GetBPS write FBPS;
    property InChannels : Integer read GetCh write FChan;
    property InSampleRate : Integer read GetSR write FSR;
    property OnBufferDone : TOnBufferDone read FOnBufferDone write FOnBufferDone;
  end;

  TAudioProcessor = class(TACSConverter)
  private
    FOnInit : TAudioProcessorInitEvent;
    FOnFlush : TAudioProcessorFlushEvent;
    FOnGetData : TGetDataEvent;
    FOnGetSampleRate : TGetParameterEvent;
    FOnGetBitsPerSample : TGetParameterEvent;
    FOnGetChannels : TGetParameterEvent;
    FOnGetTotalTime : TGetParameterEvent;
    FOnGetSize : TGetParameterEvent;
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    function GetTotalTime : Integer; override;
  public
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
  published
    property OnFlush : TAudioProcessorFlushEvent read FOnFlush write FOnFlush;
    property OnGetBitsPerSample : TGetParameterEvent read FOnGetBitsPerSample write FOnGetBitsPerSample;
    property OnGetChannels : TGetParameterEvent read FOnGetChannels write FOnGetChannels;
    property OnGetData : TGetDataEvent read FOnGetData write FOnGetData;
    property OnGetSampleRate : TGetParameterEvent read FOnGetSampleRate write FOnGetSampleRate;
    property OnGetSize : TGetParameterEvent read FOnGetSize write FOnGetSize;
    property OnGetTotalTime : TGetParameterEvent read FOnGetTotalTime write FOnGetTotalTime;
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

  TNULLOut = class(TACSOutput)
  private
    Buf : array[0..BUF_SIZE-1] of Byte;
  protected
    procedure Done; override;
    function DoOutput(Abort : Boolean):Boolean; override;
    procedure Prepare; override;
  end;

  TInputItem = class(TCollectionItem)
  protected
    FInput : TACSInput;
    function GetOwner : TPersistent; override;
  published
    property Input : TACSInput read FInput write FInput;
  end;

  TInputItems = class(TOwnedCollection)
  end;

  TInputChangedEvent = procedure(Sender : TComponent; var Index : Integer; var Continue : Boolean) of object;

  TInputList = class(TACSInput)
  private
    FCurrentInput : Integer;
    FInputItems : TInputItems;
    Lock : Boolean;
    FOnInputChanged : TInputChangedEvent;
    FIndicateProgress : Boolean;
    procedure SetCurrentInput(aInput : Integer);
    procedure SetInputItems(aItems : TInputItems);
  protected
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    property CurrentInput : Integer read FCurrentInput write SetCurrentInput;
  published
    property IndicateProgress : Boolean read FIndicateProgress write FIndicateProgress;
    property InputItems : TInputItems read FInputItems write SetInputItems;
    property OnInputChanged : TInputChangedEvent read FOnInputChanged write FOnInputChanged;
  end;


implementation

{$IFDEF LINUX}

var
  AOInitialized : Integer = 0;

{$ENDIF}

  constructor TMemoryIn.Create;
  begin
    inherited Create(AOwner);
    FSize := -1;
  end;

  destructor TMemoryIn.Destroy;
  begin
    inherited Destroy;
  end;

  function TMemoryIn.GetBPS;
  begin
    if (FBPS in [8, 16]) = False  then FBPS := 16;
    Result := FBPS;
  end;

  function TMemoryIn.GetCh;
  begin
    if (FChan in [1..2]) = False then FChan := 1;
    Result := FChan;
  end;

  function TMemoryIn.GetSR;
  begin
    if (FSR < 4000) or (FSR > 48000) then FSR := 8000;
    Result := FSR;
  end;

  procedure TMemoryIn.Init;
  begin
    FPosition := 0;
    BufEnd := FDataSize;
    BufStart := 1;
    Buisy := True;
  end;

  procedure TMemoryIn.Flush;
  begin
    Buisy := False;
    FDataSize := 0;
  end;

  function TMemoryIn.GetData;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if not Assigned(FBuffer) then
    begin
      Result := 0;
      Exit;
    end;
    if BufStart > BufEnd then
    begin
      BufStart := 1;
      if FDataSize = 0 then
      begin
        if Assigned(FOnBufferDone) then FOnBufferDone(Self)
        else
        begin
          Result := 0;
          Exit;
        end;
      end;
      BufEnd := FDataSize;
      if FDataSize = 0 then
      begin
        Result := 0;
        Exit;
      end;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(FBuffer[BufStart-1], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
    Dec(FDataSize, Result);
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
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
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
  if Progress <> CurProgr then
  begin
    CurProgr := Progress;
    if Assigned(FOnProgress) then FonProgress(Self);
  end;
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
    while InputLock do;
    InputLock := True;
    Len := Finput.GetData(P, BUF_SIZE);
    InputLock := False;
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
    if Buisy then
    raise EACSException.Create('Component is buisy.');
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
    raise EACSException.Create('Input is not assigned.');
    if Assigned(FOnGetBitsPerSample) then FOnGetBitsPerSample(Self, Result) else
    Result := FInput.BitsPerSample;
  end;

  function TAudioProcessor.GetSR;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    if Assigned(FOnGetSampleRate) then FOnGetSampleRate(Self, Result) else
    Result := FInput.SampleRate;
  end;

  function TAudioProcessor.GetCh;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    if Assigned(FOnGetChannels) then FOnGetChannels(Self, Result) else
    Result := FInput.Channels;
  end;

  function TAudioProcessor.GetTotalTime;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    if Assigned(FOnGetTotalTime) then FOnGetTotalTime(Self, Result) else
    Result := FInput.TotalTime;
  end;

  function TAudioProcessor.GetData;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    Result := BufferSize;
    if Assigned(FOnGetData) then FOnGetData(Self, Buffer, Result)
    else Result := FInput.GetData(Buffer, BufferSize);
   Inc(FPosition, Result);
//   if Result = 0 then
//   Result := Result shl 1;
  end;

  procedure TAudioProcessor.Init;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    if Assigned(FOnInit) then FOnInit(Self, FSize)
    else
    begin
      FInput.Init;
      if Assigned(FOnGetSize) then FOnGetSize(Self, FSize)
      else FSize := Finput.Size;
    end;
    Buisy := True;
    FPosition := 0;
  end;

  procedure TAudioProcessor.Flush;
  begin
    if not Assigned(FInput) then
    raise EACSException.Create('Input is not assigned.');
    if Assigned(FOnFlush) then FOnFlush(Self)
    else FInput.Flush;
    Buisy := False;
  end;

procedure TNULLOut.Prepare;
begin
  if not Assigned(FInput) then
  raise EACSException.Create('Input is not assigned.');
  FInput.Init;
end;

function TNULLOut.DoOutput;
begin
  Result := True;
  if not Buisy then Exit;
  if Abort or (not CanOutput) then
  begin
    Result := False;
    Exit;
  end;
  if Finput.GetData(@Buf[0], BUF_SIZE) > 0 then Result := True
  else
  begin
    Result := False;
    Exit;
  end;
  if Assigned(FOnProgress) then
  begin
    if FInput.Size > 0 then
    if CurProgr <> GetProgress then
    begin
      CurProgr := GetProgress;
      FOnProgress(Self);
    end;
  end;
end;

procedure TNULLOut.Done;
begin
  FInput.Flush;
end;

function TInputItem.GetOwner;
begin
  Result := Collection;
end;

constructor TInputList.Create;
begin
  inherited Create(AOwner);
  FInputItems := TInputItems.Create(Self, TInputItem);
  FPosition := 0;
  FSize := -1;
  FIndicateProgress := True;
end;

destructor TInputList.Destroy;
begin
  FInputItems.Free;
  Inherited Destroy;
end;

procedure TInputList.SetCurrentInput;
var
  I : TInputItem;
begin
  if aInput <> 0 then
  if (aInput < 0) or (aInput >= FInputItems.Count) then
  raise EACSException.Create('List index out of bounds: ' + IntToStr(aInput));
  if Buisy then
  begin
    while Lock do;
    Lock := True;
    I := TInputItem(InputItems.Items[FCurrentInput]);
    I.Input.Flush;
    I := TInputItem(InputItems.Items[aInput]);
    I.Input.Init;
    if FIndicateProgress then
    FSize := I.Input.Size
    else FSize := -1;
    FPosition := 0;
    Lock := False;
  end;
  FCurrentInput := aInput;
end;

function TInputList.GetBPS;
var
  I : TInputItem;
begin
  if Buisy then
  begin
    I := TInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.BitsPerSample;
  end else
  if InputItems.Count > 0 then
  begin
    I := TInputItem(InputItems.Items[0]);
    Result := I.Input.BitsPerSample;
  end;
end;

function TInputList.GetCh;
var
  I : TInputItem;
begin
  if Buisy then
  begin
    I := TInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.Channels;
  end else
  if InputItems.Count > 0 then
  begin
    I := TInputItem(InputItems.Items[0]);
    Result := I.Input.Channels;
  end;
end;

function TInputList.GetSR;
var
  I : TInputItem;
begin
  if Buisy then
  begin
    I := TInputItem(InputItems.Items[FCurrentInput]);
    Result := I.Input.SampleRate;
  end else
  if InputItems.Count > 0 then
  begin
    I := TInputItem(InputItems.Items[0]);
    Result := I.Input.SampleRate;
  end;
end;

procedure TInputList.Init;
var
  I : TInputItem;
begin
  if Buisy then
  raise EACSException.Create('The component is buisy.');
  if InputItems.Count = 0 then
  raise EACSException.Create('No input items in the list.');
  I := TInputItem(InputItems.Items[FCurrentInput]);
  if not Assigned(I.Input) then
  raise EACSException.Create('No input assigned to the input item '+IntToStr(FCurrentInput));
  Buisy := True;
  I.Input.Init;
  if FIndicateProgress then
  FSize := I.Input.Size
  else FSize := -1;
  FPosition := 0;
end;

procedure TInputList.Flush;
var
  I : TInputItem;
begin
  I := TInputItem(InputItems.Items[FCurrentInput]);
  I.Input.Flush;
  FCurrentInput := 0;
  Lock := False;
  Buisy := False;
end;

function TInputList.GetData;
var
  I : TInputItem;
  Continue : Boolean;
begin
  while Lock do;
  Lock := True;
  I := TInputItem(InputItems.Items[FCurrentInput]);
  Result :=  I.Input.GetData(Buffer, BufferSize);
  while Result = 0 do
  begin
    if FCurrentInput < InputItems.Count -1 then
    begin
      I.Input.Flush;
      Inc(FCurrentInput);
      Continue := True;
      if Assigned(FonInputChanged) then
      FonInputChanged(Self, FCurrentInput, Continue);
      if Continue then
      begin
        I := TInputItem(InputItems.Items[FCurrentInput]);
        if not Assigned(I.Input) then
        raise EACSException.Create('No input assigned to the input item '+IntToStr(FCurrentInput));
        I.Input.Init;
        if FIndicateProgress then
        FSize := I.Input.Size
        else FSize := -1;
        FPosition := 0;
        Result :=  I.Input.GetData(Buffer, BufferSize);
      end else Break;
    end else Break;
  end;
  if FIndicateProgress then
  FPosition := I.Input.Position;
  Lock := False;
end;

procedure TInputList.SetInputItems;
begin
  FInputItems.Assign(aItems);
end;
end.
