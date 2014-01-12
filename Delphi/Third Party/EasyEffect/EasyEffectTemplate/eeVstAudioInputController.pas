unit eeVstAudioInputController;

interface

{$INCLUDE Defines.inc}

uses
  {$IFDEF OverSampleEnabled}eeOverSamplingFilters,{$ENDIF}
  DAEffect, VamLib.MoreTypes;

type
  TVstAudioInputController = class
  private
    fBlockSize: integer;
    fInputChannelCount: integer;
    fOverSampleFactor: integer;
    procedure SetBlockSize(const Value: integer);
    procedure SetInputChannelCount(const Value: integer);
    procedure SetOverSampleFactor(const Value: integer);
  protected
    PluginInputBufferPointers : PArrayOfPSingle;
    {$IFDEF OverSampleEnabled}InputFilters : TInputFilters;{$ENDIF}
    procedure UpdateInternalBuffers;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Setup_PluginInputBuffers(Inputs:PArrayOfPSingle);

    procedure ProcessVstInputs(VstInputs:DAEffect.PPSingle; const SampleFrames:integer);

    property InputChannelCount : integer read fInputChannelCount write SetInputChannelCount;
    property BlockSize         : integer read fBlockSize         write SetBlockSize;
    property OverSampleFactor  : integer read fOverSampleFactor  write SetOverSampleFactor;
  end;

implementation

{ TVstAudioInputController }

constructor TVstAudioInputController.Create;
begin
  {$IFDEF OverSampleEnabled}
  InputFilters := TInputFilters.Create;
  {$ENDIF}
end;

destructor TVstAudioInputController.Destroy;
begin
  {$IFDEF OverSampleEnabled}
  InputFilters.Free;
  {$ENDIF}
  inherited;
end;

procedure TVstAudioInputController.SetBlockSize(const Value: integer);
begin
  if Value <> fBlockSize then
  begin
    fBlockSize := Value;
    UpdateInternalBuffers;
  end;
end;

procedure TVstAudioInputController.SetInputChannelCount(const Value: integer);
begin
  if Value <> fInputChannelCount then
  begin
    fInputChannelCount := Value;
    UpdateInternalBuffers;
  end;
end;

procedure TVstAudioInputController.SetOverSampleFactor(const Value: integer);
begin
  if Value <> fOverSampleFactor then
  begin
    fOverSampleFactor := Value;
    UpdateInternalBuffers;
  end;
end;

procedure TVstAudioInputController.Setup_PluginInputBuffers(Inputs: PArrayOfPSingle);
begin
  PluginInputBufferPointers := Inputs;
end;

procedure TVstAudioInputController.UpdateInternalBuffers;
{$IFDEF OverSampleEnabled}
var
  c1: Integer;
{$ENDIF}
begin
  {$IFDEF OverSampleEnabled}
  InputFilters.FilterCount := InputChannelCount;
  for c1 := 0 to InputChannelCount-1 do
  begin
    InputFilters.Filters[c1].BufferSize := BlockSize;
    InputFilters.Filters[c1].InterpolationFactor := OverSampleFactor;
  end;
  {$ENDIF}
end;

procedure TVstAudioInputController.ProcessVstInputs(VstInputs: DAEffect.PPSingle; const SampleFrames:integer);
var
  c1 : integer;
begin
  //-----------------------------------------------------
  //Setup the input/output pointers.
  //-----------------------------------------------------
{$IFDEF OverSampleEnabled}
  for c1 := 0 to InputChannelCount-1 do
  begin
    InputFilters.Filters[c1].Process(PSingle(VstInputs^), SampleFrames);
    PluginInputBufferPointers^[c1] := InputFilters.Filters[c1].GetDataPointer;
    inc(VstInputs);

    PluginInputBufferPointers^[c1] := PSingle(VstInputs^);
    inc(VstInputs);
  end;
{$ELSE}
  for c1 := 0 to InputChannelCount-1 do
  begin
    PluginInputBufferPointers^[c1] := PSingle(VstInputs^);
    inc(VstInputs);
  end;
{$ENDIF}
end;



end.
