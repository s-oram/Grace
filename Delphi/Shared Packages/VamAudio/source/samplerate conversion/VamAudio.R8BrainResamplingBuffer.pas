unit VamAudio.R8BrainResamplingBuffer;

interface

// https://code.google.com/p/r8brain-free-src/

uses
  VamLib.MoreTypes, r8bsrc;

type
  TResampleResolution = (res16Bit, res16BitIR, res24bit);

  TR8BrainResamplingBuffer = class
  private
    fLatency: integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetResampleParameters(SrcSampleRate: Double; DstSampleRate: Double;	MaxInLen: LongInt; ReqTransBand: Double; Res: TResampleResolution);

    property Latency : integer read fLatency;
  end;

implementation

{ TR8BrainResamplingBuffer }

constructor TR8BrainResamplingBuffer.Create;
begin

end;

destructor TR8BrainResamplingBuffer.Destroy;
begin

  inherited;
end;

end.
