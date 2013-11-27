unit eeVstAudioOutputController;

interface

{$INCLUDE Defines.inc}

uses
  {$IFDEF OverSampleEnabled}eeOverSamplingFilters,{$ENDIF}
  DAEffect, MoreTypes;

type
  TVstAudioOutputController = class
  private
    fBlockSize: integer;
    fOverSampleFactor: integer;
    fOutputChannelCount: integer;
    fUseSilentBuffers: boolean;
    procedure SetBlockSize(const Value: integer);
    procedure SetOutputChannelCount(const Value: integer);
    procedure SetOverSampleFactor(const Value: integer);
  protected
    PluginOutputBufferPointers : PArrayOfPSingle;
    {$IFDEF OverSampleEnabled}OutputFilters : TOutputFilters;{$ENDIF}
    procedure UpdateInternalBuffers;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Setup_PluginOutputBuffers(Outputs:PArrayOfPSingle);

    procedure PreProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
    procedure PostProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);

    property UseSilentBuffers   : boolean read fUseSilentBuffers   write fUseSilentBuffers;
    property OutputChannelCount : integer read fOutputChannelCount write SetOutputChannelCount;
    property BlockSize          : integer read fBlockSize          write SetBlockSize;
    property OverSampleFactor   : integer read fOverSampleFactor   write SetOverSampleFactor;
  end;

implementation

uses
  eeFunctions;

{ TVstAudioOutputController }

constructor TVstAudioOutputController.Create;
begin
  {$IFDEF OverSampleEnabled}OutputFilters := TOutputFilters.Create;{$ENDIF}
end;

destructor TVstAudioOutputController.Destroy;
begin
  {$IFDEF OverSampleEnabled}OutputFilters.Free;{$ENDIF}
  inherited;
end;

procedure TVstAudioOutputController.SetBlockSize(const Value: integer);
begin
  if Value <> fBlockSize then
  begin
    fBlockSize := Value;
    UpdateInternalBuffers;
  end;
end;

procedure TVstAudioOutputController.SetOutputChannelCount(const Value: integer);
begin
  if Value <> fOutputChannelCount then
  begin
    fOutputChannelCount := Value;
    UpdateInternalBuffers
  end;
end;

procedure TVstAudioOutputController.SetOverSampleFactor(const Value: integer);
begin
  if Value <> fOverSampleFactor then
  begin
    fOverSampleFactor := Value;
    UpdateInternalBuffers
  end;
end;

procedure TVstAudioOutputController.Setup_PluginOutputBuffers(Outputs: PArrayOfPSingle);
begin
  PluginOutputBufferPointers := Outputs;
end;

procedure TVstAudioOutputController.UpdateInternalBuffers;
var
  c1: Integer;
begin
  {$IFDEF OverSampleEnabled}OutputFilters.FilterCount := OutputChannelCount;{$ENDIF}
  for c1 := 0 to OutputChannelCount-1 do
  begin
    {$IFDEF OverSampleEnabled}OutputFilters.Filters[c1].DecimationFactor := OverSampleFactor;{$ENDIF}
    {$IFDEF OverSampleEnabled}OutputFilters.Filters[c1].BufferSize := BlockSize * OverSampleFactor;{$ENDIF}
  end;
end;

procedure TVstAudioOutputController.PreProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
var
  c1:integer;
begin
  for c1 := 0 to OutputChannelCount-1 do
  begin
    {$IFDEF OverSampleEnabled}
      if UseSilentBuffers then OutputFilters.Filters[c1].ZeroBuffer;
      PluginOutputBufferPointers^[c1] := OutputFilters.Filters[c1].GetDataPointer;
    {$ELSE}
      if UseSilentBuffers then ZeroBufferX1(PSingle(VstOutputs^), SampleFrames);
      PluginOutputBufferPointers^[c1] := PSingle(VstOutputs^);
      inc(VstOutputs);
    {$ENDIF}
  end;

end;


procedure TVstAudioOutputController.PostProcessVstOutputs(VstOutputs: DAEffect.PPSingle; const SampleFrames:integer);
  {$IFDEF OverSampleEnabled}
  var
    c1: Integer;
    ModSampleFrames : integer;
  {$ENDIF}
begin
  {$IFDEF OverSampleEnabled}
  ModSampleFrames := SampleFrames * OverSampleFactor;
  for c1 := 0 to OutputChannelCount-1 do
  begin
    OutputFilters.Filters[c1].Process(PSingle(VstOutputs^), ModSampleFrames);
    inc(VstOutputs);
  end;
  {$ENDIF}
end;



end.
