unit EasyEffect.AudioOutputController.Custom;

interface

uses
  VamVst2.DAEffect, VamLib.MoreTypes;

type
  IVstAudioOutputController = interface
    ['{45889513-D156-4CF7-B258-8064FE25C9EB}']

    // TODO: The plugin has an array of "Outputs", a array of PSingle that point to
    // the actual output buffers.
    // This method stores pointers to the plugins pointers and changes them
    // so that the plugin can write the output data.
    // TODO: This all seems a bit needlessly complicated. Perhaps there is a better way.
    procedure Setup_PluginOutputBuffers(Outputs:PArrayOfPSingle);

    // This method is run before the plugin does it's processing. It ready's the sample buffers for data.
    // (Zero's the buffers if needed). It then updates the plugin with pointers to the
    // sample data buffers.
    procedure PreProcessVstOutputs(VstOutputs: VamVst2.DAEffect.PPSingle; const SampleFrames:integer);

    // This method is run after a plugin does it's processing. It will downsample to
    // the host samplerate if needed, then copy the buffer data to the VST output buffers.
    procedure PostProcessVstOutputs(VstOutputs: VamVst2.DAEffect.PPSingle; const SampleFrames:integer);
  end;

  TCustomVstAudioOutputController = class(TInterfacedObject)
  private
    fBlockSize: integer;
    fOverSampleFactor: integer;
    fOutputChannelCount: integer;
    fUseSilentBuffers: boolean;
    procedure SetBlockSize(const Value: integer);
    procedure SetOutputChannelCount(const Value: integer);
    procedure SetOverSampleFactor(const Value: integer);

  protected
    // UpdateInternalBuffers() is called when OutputChannelCount, BlockSize or OverSamplefactor changes.
    procedure UpdateInternalBuffers; virtual; abstract;
    function GetLatency: integer; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property UseSilentBuffers   : boolean read fUseSilentBuffers   write fUseSilentBuffers;
    property OutputChannelCount : integer read fOutputChannelCount write SetOutputChannelCount;
    property BlockSize          : integer read fBlockSize          write SetBlockSize;
    property OverSampleFactor   : integer read fOverSampleFactor   write SetOverSampleFactor;

    // reports latency in samples.
    property Latency            : integer read GetLatency;
  end;

implementation

uses
  eeFunctions;

{ TVstAudioOutputController }

constructor TCustomVstAudioOutputController.Create;
begin
  fBlockSize          := 0;
  fOverSampleFactor   := 0;
  fOutputChannelCount := 0;
end;

destructor TCustomVstAudioOutputController.Destroy;
begin

  inherited;
end;

procedure TCustomVstAudioOutputController.SetBlockSize(const Value: integer);
begin
  if Value <> fBlockSize then
  begin
    fBlockSize := Value;
    UpdateInternalBuffers;
  end;
end;

procedure TCustomVstAudioOutputController.SetOutputChannelCount(const Value: integer);
begin
  if Value <> fOutputChannelCount then
  begin
    fOutputChannelCount := Value;
    UpdateInternalBuffers
  end;
end;

procedure TCustomVstAudioOutputController.SetOverSampleFactor(const Value: integer);
begin
  if Value <> fOverSampleFactor then
  begin
    fOverSampleFactor := Value;
    UpdateInternalBuffers
  end;
end;

end.
