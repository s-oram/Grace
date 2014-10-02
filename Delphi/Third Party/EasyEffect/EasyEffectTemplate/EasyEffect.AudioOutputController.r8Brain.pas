unit EasyEffect.AudioOutputController.r8Brain;

interface

{$INCLUDE Defines.inc}

uses
  EasyEffect.AudioOutputController.Custom,
  R8BrainWrapper,
  DAEffect, VamLib.MoreTypes;

type
  TVstAudioOutputController = class(TCustomVstAudioOutputController, IVstAudioOutputController)
  private
  protected
    PluginOutputBufferPointers : PArrayOfPSingle;

    //Resamplers : array of TR8BrainResampler;
    Resamplers : TArray<TR8BrainResampler>;
    Buffers : array of array of Single;
    
    procedure UpdateInternalBuffers; override;
    function GetLatency: integer; override;
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
    property Latency;
  end;



implementation

uses
  eeFunctions;

{ TVstAudioOutputController }

constructor TVstAudioOutputController.Create;
begin
  inherited;

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
  if Length(Resamplers) > 0
    then result := Resamplers[0].Latency
    else result := 0;
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
    Resamplers[c1] := TR8BrainResampler.Create;
    Resamplers[c1].Setup(OverSampleFactor, 1, OverSampleFactor * BlockSize, 5, res24bit);
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

procedure TVstAudioOutputController.PreProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
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


procedure TVstAudioOutputController.PostProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
var
  c1, c2: Integer;
  Smps : integer;
  OutputBuffer : PSingle;
begin
  for c1 := 0 to OutputChannelCount-1 do
  begin
    OutputBuffer := PSingle(VstOutputs^);
    inc(VstOutputs);

    Smps := Resamplers[c1].Process_Single(@Buffers[c1,0], @Buffers[c1,0], SampleFrames * OverSampleFactor);

    // TODO log exception here:
    //assert(Smps = SampleFrames);

    for c2 := 0 to Smps-1 do
    begin
      OutputBuffer^ := Buffers[c1,c2];
      inc(OutputBuffer);
    end;
  end;
end;





end.
