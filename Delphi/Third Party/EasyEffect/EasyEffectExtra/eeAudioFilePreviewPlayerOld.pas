unit eeAudioFilePreviewPlayerOld;

interface

uses
  MoreTypes, eeSampleInt, OTLTask, OTLTaskControl, OTLEventMonitor, SyncObjs, eeAudioFilePreviewPlayerVoice;

type
  TAudioFilePreviewPlayer = class
  private
    fSampleRate: integer;
    fVolume: single;
    procedure SetSampleRate(const Value: integer);
    procedure SetVolume(const Value: single);
  protected
    aVoice:TAudioFilePreviewPlayerVoice;

    Dispatcher:TOmniEventMonitor;
    SampleLoadingLock:TCriticalSection;

    Time:cardinal;
    TimeCounter:cardinal;
    TimeCounterActive:boolean;

    Active:boolean;

    Sample:TSampleInt;
    SampleFileName:string;

    ReadPos:cardinal;
    ReadEnd:cardinal;
    ReadStepSize:integer;

    procedure LoadSample(const task: IOmniTask);
  public
    constructor Create;
	  destructor Destroy; override;

    procedure Trigger(fn:string; Delay:integer);
    procedure Stop;
    procedure Kill;

    procedure Process(In1,In2:PSingle; Sampleframes:integer); inline;

    property SampleRate:integer read fSampleRate write SetSampleRate;
    property Volume    :single  read fVolume     write SetVolume;     //range 0..1
  end;

implementation

uses
  SysUtils;


{ TAudioFilePreviewPlayer }

constructor TAudioFilePreviewPlayer.Create;
begin
  aVoice := TAudioFilePreviewPlayerVoice.Create;

  Volume     := 1;
  Active     := false;
  SampleRate := 44100;
  Sample     := TSampleInt.Create;
  Dispatcher := TOmniEventMonitor.Create(nil);

  SampleLoadingLock := TCriticalSection.Create;
end;

destructor TAudioFilePreviewPlayer.Destroy;
begin
  SampleLoadingLock.Free;

  Sample.Free;
  Dispatcher.Free;

  aVoice.Free;
  inherited;
end;

procedure TAudioFilePreviewPlayer.Trigger(fn: string; Delay: integer);
begin
  SampleFileName := fn;

  Time := Delay * SampleRate div 1000; //Set time in samples.
  TimeCounter := 0;                    //reset the time counter.
  TimeCounterActive := true;
end;

procedure TAudioFilePreviewPlayer.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
  aVoice.SampleRate := Value;
end;

procedure TAudioFilePreviewPlayer.SetVolume(const Value: single);
begin
  fVolume := Value;
  aVoice.Volume := Value;
end;

procedure TAudioFilePreviewPlayer.Stop;
begin
  TimeCounterActive := false;
  Active := false;
end;

procedure TAudioFilePreviewPlayer.Kill;
begin
  TimeCounterActive := false;
  Active := false;
end;



procedure TAudioFilePreviewPlayer.LoadSample(const task: IOmniTask);
begin
  if Sample.LoadFromFile(SampleFileName) then
  begin
    if Sample.Properties.SampleFrames > 2 then
    begin
      ReadPos      := 0;
      ReadStepSize := round(Sample.Properties.SampleRate / SampleRate * 1024);
      ReadEnd      := (Sample.Properties.SampleFrames-2) * 1024;

      Active := true;
    end;
  end;

  SampleLoadingLock.Leave;
end;

procedure TAudioFilePreviewPlayer.Process(In1, In2: PSingle; Sampleframes: integer);
const
  OneOver1024 = 1 / 1024;
var
  c1: Integer;
  r1,r2:integer;
  f:single;
  x1,x2:single;
begin
  if TimeCounterActive then
  begin
    inc(TimeCounter, SampleFrames);
    if TimeCounter > Time then
    begin
      Active := false;
      TimeCounterActive := false;
      aVoice.Trigger(SampleFileName);
    end;
  end;

  if aVoice.Active then aVoice.Process(In1, In2, SampleFrames);


  //==============================================================================
  // Old process method
  //==============================================================================

  {
  if TimeCounterActive then
  begin
    inc(TimeCounter, SampleFrames);
    if TimeCounter > Time then
    begin
      if SampleLoadingLock.TryEnter then
      begin
        Active := false;
        TimeCounterActive := false;
        Dispatcher.Monitor(CreateTask(LoadSample, 'Load Sample')).Run;
      end;
    end;
  end;

  if (Active) and (Sample.Properties.IsValid) and (Sample.Properties.ChannelCount = 1) then
  begin
    try
      for c1 := 0 to SampleFrames - 1 do
      begin
        r1 := ReadPos shr 10; //r1 := ReadPos div 1024;
        r2 := r1 + 1;
        f  := (ReadPos * OneOver1024) - r1;
        x1 := (Sample.Ch1[r1] * OneOver32767) * (1 - f) + (Sample.Ch1[r2] * OneOver32767) * (f);

        if (ReadPos + ReadStepSize < ReadEnd)
          then ReadPos := ReadPos + ReadStepSize
          else Active  := false;

        In1^ := In1^ + (x1 * Volume);
        In2^ := In2^ + (x1 * Volume);
        inc(In1);
        Inc(In2);
      end;
    except
      // NOTE: This section of code would sometimes trigger an access violation but I don't understand why.
      // The Sample loading is protected by a critical section so shouldn't be getting modified unexpectedly.
      on EAccessViolation do Active := false;
    end;
  end;

  if (Active) and  (Sample.Properties.IsValid) and (Sample.Properties.ChannelCount = 2) then
  begin
    try
      for c1 := 0 to SampleFrames - 1 do
      begin
        r1 := ReadPos shr 10; //r1 := ReadPos div 1024;
        r2 := r1 + 1;
        f  := (ReadPos * OneOver1024) - r1;
        x1 := (Sample.Ch1[r1] * OneOver32767) * (1 - f) + (Sample.Ch1[r2] * OneOver32767) * (f);
        x2 := (Sample.Ch2[r1] * OneOver32767) * (1 - f) + (Sample.Ch2[r2] * OneOver32767) * (f);

        if (ReadPos + ReadStepSize < ReadEnd)
          then ReadPos := ReadPos + ReadStepSize
          else Active  := false;

        In1^ := In1^ + (x1 * Volume);
        In2^ := In2^ + (x2 * Volume);
        inc(In1);
        Inc(In2);
      end;
    except
      // NOTE: This section of code would sometimes trigger an access violation but I don't understand why.
      // The Sample loading is protected by a critical section so shouldn't be getting modified unexpectedly.
      on EAccessViolation do Active := false;

    end;


  end;



  }
end;



end.
