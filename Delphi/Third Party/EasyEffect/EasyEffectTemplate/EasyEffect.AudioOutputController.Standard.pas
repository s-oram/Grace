{

  IMPORTANT: This class is untested!
}


unit EasyEffect.AudioOutputController.Standard;


interface

{$INCLUDE Defines.inc}

uses
  EasyEffect.AudioOutputController.Custom,
  DAEffect, MoreTypes;

type
  TVstAudioOutputController = class(TCustomVstAudioOutputController)
  private
  protected
    PluginOutputBufferPointers : PArrayOfPSingle;
    procedure UpdateInternalBuffers; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Setup_PluginOutputBuffers(Outputs:PArrayOfPSingle); override;

    procedure PreProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer); override;
    procedure PostProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer); override;

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
end;

destructor TVstAudioOutputController.Destroy;
begin
  inherited;
end;

procedure TVstAudioOutputController.Setup_PluginOutputBuffers(Outputs: PArrayOfPSingle);
begin
  PluginOutputBufferPointers := Outputs;
end;

procedure TVstAudioOutputController.UpdateInternalBuffers;
begin
end;

procedure TVstAudioOutputController.PreProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
var
  c1:integer;
begin
  for c1 := 0 to OutputChannelCount-1 do
  begin
    if UseSilentBuffers then ZeroBufferX1(PSingle(VstOutputs^), SampleFrames);
    PluginOutputBufferPointers^[c1] := PSingle(VstOutputs^);
    inc(VstOutputs);
  end;

end;


procedure TVstAudioOutputController.PostProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
begin
end;

end.
