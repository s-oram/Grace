unit EasyEffect.AudioOutputController.DspMaster;

interface

{$INCLUDE Defines.inc}

uses
  EasyEffect.AudioOutputController.Custom,
  eeOverSamplingFilters,
  DAEffect, VamLib.MoreTypes;

type
  TVstAudioOutputController = class(TCustomVstAudioOutputController, IVstAudioOutputController)
  private
  protected
    PluginOutputBufferPointers : PArrayOfPSingle;
    OutputFilters : TOutputFilters;
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
  eeFunctions;

{ TVstAudioOutputController }

constructor TVstAudioOutputController.Create;
begin
  inherited;
  OutputFilters := TOutputFilters.Create;
end;

destructor TVstAudioOutputController.Destroy;
begin
  OutputFilters.Free;
  inherited;
end;

procedure TVstAudioOutputController.Setup_PluginOutputBuffers(Outputs: PArrayOfPSingle);
begin
  PluginOutputBufferPointers := Outputs;
end;

procedure TVstAudioOutputController.UpdateInternalBuffers;
var
  c1: Integer;
begin
  OutputFilters.FilterCount := OutputChannelCount;
  for c1 := 0 to OutputChannelCount-1 do
  begin
    OutputFilters.Filters[c1].DecimationFactor := OverSampleFactor;
    OutputFilters.Filters[c1].BufferSize := BlockSize * OverSampleFactor;
  end;
end;

procedure TVstAudioOutputController.PreProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
var
  c1:integer;
begin
  for c1 := 0 to OutputChannelCount-1 do
  begin
    if UseSilentBuffers then OutputFilters.Filters[c1].ZeroBuffer;
    PluginOutputBufferPointers^[c1] := OutputFilters.Filters[c1].GetDataPointer;
  end;
end;


procedure TVstAudioOutputController.PostProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
var
  c1: Integer;
  ModSampleFrames : integer;
begin
  ModSampleFrames := SampleFrames * OverSampleFactor;
  for c1 := 0 to OutputChannelCount-1 do
  begin
    OutputFilters.Filters[c1].Process(PSingle(VstOutputs^), ModSampleFrames);
    inc(VstOutputs);
  end;
end;

end.
