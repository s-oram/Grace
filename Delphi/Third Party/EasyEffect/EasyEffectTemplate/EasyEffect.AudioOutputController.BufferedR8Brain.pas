unit EasyEffect.AudioOutputController.BufferedR8Brain;

interface

{$INCLUDE Defines.inc}

uses
  EasyEffect.AudioOutputController.Custom,
  R8BrainWrapper, eeMultiChannelBuffers,
  DAEffect, VamLib.MoreTypes;

const
  kBufferSizeFactor = 4;

type
  TVstAudioOutputController = class(TCustomVstAudioOutputController, IVstAudioOutputController)
  private
  protected
    PluginOutputBufferPointers : PArrayOfPSingle;

    UpSampleBuffersA : TMultiSingleBuffer;
    UpSampleBuffersB : TMultiDoubleBuffer;
    InputBuffer : record
      MaxSize      : integer;
      SampleFrames : integer; //Total number of samples in the Upsample buffer.
      ReadIndex    : integer; //current read index.
    end;

    Resamplers : TArray<TR8BrainResampler>;

    OutputBuffer : record
      SampleFrames : integer; //Samples in the buffer.
      ReadCount    : integer; //samples that have been read from the buffer.
      ReadPointers : array of PDouble; //pointers to the output buffer data.
    end;

    //OutputBufferSampleFrames : integer;
    //OutputBuffer


    TempPluginOutputBuffers : TArray<PSingle>;

    procedure ReadFromOutputBuffers(var PluginOutputBuffers : TArray<PSingle>; const SampleFrames : integer);
    procedure RefreshOutputBuffers;


    procedure UpdateInternalBuffers; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Setup_PluginOutputBuffers(Outputs:PArrayOfPSingle);
    procedure PreProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
    procedure PostProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);

    property UseSilentBuffers;
    property OutputChannelCount;
    property BlockSize;
    property OverSampleFactor;
  end;



implementation

uses
  Math,
  eeFunctions;

{ TVstAudioOutputController }

constructor TVstAudioOutputController.Create;
begin
  inherited;

  UpSampleBuffersA := TMultiSingleBuffer.Create;
  UpSampleBuffersB := TMultiDoubleBuffer.Create;
end;

destructor TVstAudioOutputController.Destroy;
var
  c1 : integer;
begin
  for c1 := 0 to Length(Resamplers)-1 do Resamplers[c1].Free;
  SetLength(Resamplers, 0);


  UpSampleBuffersA.Free;
  UpSampleBuffersB.Free;

  SetLength(OutputBuffer.ReadPointers, 0);


  inherited;
end;

procedure TVstAudioOutputController.Setup_PluginOutputBuffers(Outputs: PArrayOfPSingle);
begin
  PluginOutputBufferPointers := Outputs;
end;

procedure TVstAudioOutputController.UpdateInternalBuffers;
var
  c1: Integer;
  smpsA : integer;
  smpsB : integer;

  InPtr  : PDouble;
  OutPtr : PDouble;
  InputSampleFrames : integer;
begin
  if OutputChannelCount = 0 then exit;
  if BlockSize          = 0 then exit;
  if OverSampleFactor   = 0 then exit;

  InputBuffer.MaxSize := OverSampleFactor * BlockSize * kBufferSizeFactor;

  SetLength(OutputBuffer.ReadPointers, OutputChannelCount);
  SetLength(TempPluginOutputBuffers, OutputChannelCount);


  for c1 := 0 to Length(Resamplers)-1 do Resamplers[c1].Free;
  SetLength(Resamplers, OutputChannelCount);
  for c1 := 0 to Length(Resamplers)-1 do
  begin
    Resamplers[c1] := TR8BrainResampler.Create;
    //Resamplers[c1].Setup(44100 * OverSampleFactor, 44100, OverSampleFactor * BlockSize, 5, res24bit);
    Resamplers[c1].Setup(OverSampleFactor, 1, InputBuffer.MaxSize, 5, res24bit);
  end;

  UpSampleBuffersA.SetSize(OutputChannelCount, InputBuffer.MaxSize);
  UpSampleBuffersA.Clear;
  UpSampleBuffersB.SetSize(OutputChannelCount, InputBuffer.MaxSize);
  UpSampleBuffersB.Clear;


  //InputSampleFrames := BlockSize * OverSampleFactor;
  InputSampleFrames := 16 * OverSampleFactor;
  InPtr := @UpSampleBuffersB.Data[0,0];

  for c1 := 0 to OutputChannelCount-1 do
  begin
    smpsA := 0;
    smpsB := -1;

    while smpsA <> smpsB do
    begin
      smpsA := smpsB;
      smpsB := Resamplers[c1].Process(InPtr, InputSampleFrames, OutPtr);
    end;

    OutputBuffer.ReadPointers[c1] := OutPtr;
    OutputBuffer.SampleFrames := SmpsB;
    OutputBuffer.ReadCount    := 0;
  end;

  InputBuffer.SampleFrames := 0;
  InputBuffer.ReadIndex    := 0;

end;

procedure TVstAudioOutputController.PreProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
var
  c1,c2:integer;
begin
  //TODO: we should zero the buffers here maybe?
  {
  //Zero the buffers if required.
  if UseSilentBuffers then
  begin
    for c1 := 0 to OutputChannelCount-1 do
    begin
      for c2 := 0 to (SampleFrames * OverSampleFactor)-1 do
      begin
        UpSampleBuffersA.Data[c1,c2] := 0;
      end;
    end;
  end;
  }

  //Point plugin to the temporary output buffers.
  for c1 := 0 to OutputChannelCount-1 do
  begin
    PluginOutputBufferPointers^[c1] := @UpSampleBuffersA.Data[c1,InputBuffer.SampleFrames];
  end;

  inc(InputBuffer.SampleFrames, SampleFrames * OverSampleFactor);
end;


procedure TVstAudioOutputController.PostProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
var
  c1: Integer;
  Smps : integer;
  c2: Integer;
  SmpsProcessed : integer;
  smpsToDo : integer;
  smpsReady : integer;
  smpsRemaining : integer;
begin
  // Get the output pointers...
  for c1 := 0 to OutputChannelCount-1 do
  begin
    TempPluginOutputBuffers[c1] := PSingle(VstOutputs^);
    inc(VstOutputs);
  end;

  // TODO: We do a couple needless loops and if/then checks. I think
  // with some careful reorganising it might be possible to optimise
  // this slightly.
  smpsProcessed := 0;

  while (smpsProcessed < SampleFrames) do
  begin
    smpsReady := OutputBuffer.SampleFrames - OutputBuffer.ReadCount;
    smpsRemaining := SampleFrames - SmpsProcessed;
    smpsToDo := min(smpsRemaining, smpsReady);

    if smpsToDo > 0
      then ReadFromOutputBuffers(TempPluginOutputBuffers, SmpsToDo);

    if smpsReady - SmpsToDo = 0
      then RefreshOutputBuffers;

    inc(smpsProcessed, smpsToDO);
  end;
end;


procedure TVstAudioOutputController.ReadFromOutputBuffers(var PluginOutputBuffers: TArray<PSingle>; const SampleFrames: integer);
var
  c1: Integer;
  c2: Integer;
begin
  for c1 := 0 to OutputChannelCount-1 do
  begin
    for c2 := 0 to SampleFrames-1 do
    begin
      PluginOutputBuffers[c1]^ := OutputBuffer.ReadPointers[c1]^;
      inc(PluginOutputBuffers[c1]);
      inc(OutputBuffer.ReadPointers[c1]);
    end;
  end;

  inc(OutputBuffer.ReadCount, SampleFrames);
  assert(OutputBuffer.ReadCount <= OutputBuffer.SampleFrames);
end;







procedure TVstAudioOutputController.RefreshOutputBuffers;
var
  c1: Integer;
  c2: Integer;

  InPtr  : PDouble;
  OutPtr : PDouble;
  InputSampleFrames : integer;

  TempBufferSize : integer;

  SmpsReceived : integer;
begin


  //copy the raw upsample data to a temporary buffer of double.
  for c1 := 0 to OutputChannelCount-1 do
  begin
    for c2 := InputBuffer.ReadIndex to InputBuffer.SampleFrames-1 do
    begin
      UpsampleBuffersB.Data[c1,c2] := UpsampleBuffersA.Data[c1,c2+InputBuffer.ReadIndex];
    end;
  end;

  TempBufferSize := InputBuffer.SampleFrames;

  // we've copied the data so reset the input buffer readIndex etc.
  InputBuffer.SampleFrames := 0;
  InputBuffer.ReadIndex    := 0;

  // Now do the downsampling.
  InputSampleFrames := TempBufferSize;

  //assert(InputSampleFrames <= TempBufferSize);

  for c1 := 0 to OutputChannelCount-1 do
  begin
    InPtr  := @UpsampleBuffersB.Data[c1,0];
    SmpsReceived := Resamplers[c1].Process(InPtr, InputSampleFrames, OutPtr);

    OutputBuffer.ReadPointers[c1] := OutPtr;
  end;

  OutputBuffer.ReadCount := 0;
  OutputBuffer.SampleFrames := SmpsReceived;

  //assert(SmpsReceived >= SampleFrames);
end;

end.
