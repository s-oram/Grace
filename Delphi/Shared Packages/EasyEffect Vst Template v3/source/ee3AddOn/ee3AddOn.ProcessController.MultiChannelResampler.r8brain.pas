unit ee3AddOn.ProcessController.MultiChannelResampler.r8brain;

interface

uses
  Classes,
  SysUtils,
  VamLib.Types,
  r8Brain,
  DAEffect,
  DAEffectX,
  DAudioEffectX;

type
  TResamplerArray = TAutomaticObjectArray<TR8BrainSingleChannelResampler>;

  TMultiChannelResampler = class
  private
  protected
    Config : TResampleConfig;
    ChannelCount : integer;
    InputResamplers  : TResamplerArray;
    OutputBufferPointersX32 : array of PSingle;
    OutputBufferPointersX64 : array of PDouble;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(aConfig : PResampleConfig);
    procedure Clear;

    procedure SetChannelCount(aChannelCount : integer);

    procedure ProcessFloat32(InputSamples : DAEffect.PPSingle; const InputSampleFrames : LongInt; out OutputSamples : DAEffect.PPSingle; var OutputSampleFrames : LongInt);
    procedure ProcessFloat64(InputSamples : DAEffect.PPDouble; const InputSampleFrames : LongInt; out OutputSamples : DAEffect.PPDouble; var OutputSampleFrames : LongInt);
  end;

implementation

{ TMultiChannelResampler }

constructor TMultiChannelResampler.Create;
begin
  Config.SetToDefault;
end;

destructor TMultiChannelResampler.Destroy;
begin
  InputResamplers.SetLength(0);
  SetLength(OutputBufferPointersX32, 0);
  SetLength(OutputBufferPointersX64, 0);
  inherited;
end;

procedure TMultiChannelResampler.Init(aConfig: PResampleConfig);
var
  c1 : integer;
begin
  Config.AssignFrom(aConfig);
  for c1 := 0 to ChannelCount-1 do InputResamplers.Raw[c1].Init(@Config);
end;

procedure TMultiChannelResampler.Clear;
var
  c1: Integer;
begin
  for c1 := 0 to ChannelCount-1 do InputResamplers.Raw[c1].Clear;
end;

procedure TMultiChannelResampler.SetChannelCount(aChannelCount: integer);
var
  c1: Integer;
begin
  ChannelCount := aChannelCount;
  InputResamplers.SetLength(aChannelCount);
  for c1 := 0 to ChannelCount-1 do InputResamplers.Raw[c1].Init(@Config);

  SetLength(OutputBufferPointersX32, ChannelCount);
  SetLength(OutputBufferPointersX64, ChannelCount);
end;

procedure TMultiChannelResampler.ProcessFloat32(InputSamples: DAEffect.PPSingle; const InputSampleFrames: Integer; out OutputSamples: DAEffect.PPSingle; var OutputSampleFrames: Integer);
var
  c1: Integer;
  ip0 : PSingle;
  op0 : PSingle;
begin
  for c1 := 0 to ChannelCount-1 do
  begin
    ip0 := InputSamples^;
    inc(InputSamples);

    InputResamplers.Raw[c1].ProcessFloat32(ip0, InputSampleFrames, op0, OutputSampleFrames);
    OutputBufferPointersX32[c1] := op0;
  end;

  OutputSamples := @OutputBufferPointersX32[0];
end;

procedure TMultiChannelResampler.ProcessFloat64(InputSamples: DAEffect.PPDouble; const InputSampleFrames: Integer; out OutputSamples: DAEffect.PPDouble; var OutputSampleFrames: Integer);
begin

end;



end.
