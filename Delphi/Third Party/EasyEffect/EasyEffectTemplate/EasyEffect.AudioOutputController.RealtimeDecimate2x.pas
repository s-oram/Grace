unit EasyEffect.AudioOutputController.RealtimeDecimate2x;

interface

uses
  VamLib.MoreTypes,
  VamVst2.DAEffect,
  VamAudio.RealTimeDecimationFilter,
  EasyEffect.AudioOutputController.Custom;

type
  TDownSampleFilter = TDecimationFilter2x8thOrder;

  TVstAudioOutputController = class(TCustomVstAudioOutputController, IVstAudioOutputController)
  private
  protected
    PluginOutputBufferPointers : PArrayOfPSingle;

    Resamplers : array of TDownSampleFilter;
    Buffers : array of array of Single;

    procedure UpdateInternalBuffers; override;
    function GetLatency: integer; override;

    // Hide the OverSampleFactor property. It should always be 2.
    property OverSampleFactor;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Setup_PluginOutputBuffers(Outputs:PArrayOfPSingle);
    procedure PreProcessVstOutputs(VstOutputs: VamVst2.DAEffect.PPSingle; const SampleFrames:integer);
    procedure PostProcessVstOutputs(VstOutputs: VamVst2.DAEffect.PPSingle; const SampleFrames:integer);

    property UseSilentBuffers;
    property OutputChannelCount;
    property BlockSize;
    property Latency;
  end;

implementation

uses
  VamAudio.Constants;

{ TVstAudioOutputController }

constructor TVstAudioOutputController.Create;
begin
  inherited;
  OverSampleFactor := 2;
end;

destructor TVstAudioOutputController.Destroy;
var
  c1 : integer;
begin

  for c1 := 0 to Length(Resamplers)-1 do Resamplers[c1].Free;
  SetLength(Resamplers, 0);

  for c1 := 0 to Length(Buffers)-1 do
  begin
    SetLength(Buffers[c1], 0);
  end;
  SetLength(Buffers, 0);

  inherited;
end;

function TVstAudioOutputController.GetLatency: integer;
begin
  result := 2;
end;

procedure TVstAudioOutputController.Setup_PluginOutputBuffers(Outputs: PArrayOfPSingle);
begin
  PluginOutputBufferPointers := Outputs;
end;

procedure TVstAudioOutputController.UpdateInternalBuffers;
var
  c1: Integer;
begin
  if OutputChannelCount = 0 then exit;
  if BlockSize          = 0 then exit;
  if OverSampleFactor   = 0 then exit;

  for c1 := 0 to Length(Resamplers)-1 do Resamplers[c1].Free;
  SetLength(Resamplers, OutputChannelCount);
  for c1 := 0 to Length(Resamplers)-1 do
  begin
    Resamplers[c1] := TDownSampleFilter.Create;
    Resamplers[c1].Reset;
  end;

  for c1 := 0 to Length(Buffers)-1 do
  begin
    SetLength(Buffers[c1], 0);
  end;
  SetLength(Buffers, OutputChannelCount);
  for c1 := 0 to Length(Buffers)-1 do
  begin
    SetLength(Buffers[c1], BlockSize * OverSampleFactor);
  end;
end;

procedure TVstAudioOutputController.PreProcessVstOutputs(VstOutputs: VamVst2.DAEffect.PPSingle; const SampleFrames:integer);
var
  c1,c2:integer;
begin
  for c1 := 0 to OutputChannelCount-1 do
  begin
    if UseSilentBuffers then
    begin
      for c2 := 0 to (SampleFrames * OverSampleFactor)-1 do
      begin
        Buffers[c1,c2] := 0;
      end;
    end;
    PluginOutputBufferPointers^[c1] := @Buffers[c1,0];
  end;
end;


procedure TVstAudioOutputController.PostProcessVstOutputs(VstOutputs: VamVst2.DAEffect.PPSingle; const SampleFrames:integer);
var
  c1, c2: Integer;
  Smps : integer;
  OutputBuffer : PSingle;
begin
  assert(OverSampleFactor = 2);

  for c1 := 0 to OutputChannelCount-1 do
  begin
    OutputBuffer := PSingle(VstOutputs^);
    inc(VstOutputs);

    //Smps := Resamplers[c1].Process_Single(@Buffers[c1,0], @Buffers[c1,0], SampleFrames * OverSampleFactor);
    Resamplers[c1].Process(@Buffers[c1,0], @Buffers[c1,0], SampleFrames * OverSampleFactor, Smps);

    assert(Smps = SampleFrames);

    // TODO:HIGH can probably remove this copy operation. The decimation filter can probably copy it directly accress.
    for c2 := 0 to Smps-1 do
    begin
      OutputBuffer^ := Buffers[c1,c2];
      inc(OutputBuffer);
    end;
  end;
end;

end.
