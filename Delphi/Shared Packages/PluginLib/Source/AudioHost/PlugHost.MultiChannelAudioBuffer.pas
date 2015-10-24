unit PlugHost.MultiChannelAudioBuffer;

interface

uses
  VamLib.MoreTypes;

type


  TMultiChannelAudioBuffer32 = class
  private
    FBuffer: T2dArrayOfSingle;
    FBufferPointers : array of PSingle;
    FPPBuffer: PPSingle;
  protected

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetSize(const Channels, SampleFrames : integer);

    property Buffer : T2dArrayOfSingle read FBuffer;
    property PPBuffer : PPSingle read FPPBuffer;
  end;


  TMultiChannelAudioBuffer64 = class
  private
    FBuffer: T2dArrayOfDouble;
    FBufferPointers : array of PDouble;
    FPPBuffer: PPDouble;
  protected

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetSize(const Channels, SampleFrames : integer);

    property Buffer : T2dArrayOfDouble read FBuffer;
    property PPBuffer : PPDouble read FPPBuffer;
  end;

implementation

{ TMultiChannelAudioBufferS }

constructor TMultiChannelAudioBuffer32.Create;
begin

end;

destructor TMultiChannelAudioBuffer32.Destroy;
begin
  SetLength(FBuffer, 0, 0);
  SetLength(FBufferPointers, 0);
  FPPBuffer := nil;
  inherited;
end;

procedure TMultiChannelAudioBuffer32.SetSize(const Channels, SampleFrames: integer);
var
  c1: Integer;
begin
  SetLength(FBuffer, Channels, SampleFrames);

  if (Channels > 0) and (SampleFrames > 0) then
  begin
    SetLength(FBufferPointers, Channels);
    for c1 := 0 to Channels-1 do
    begin
      FBufferPointers[c1] := @FBuffer[c1,0];
    end;
    FPPBuffer := @FBufferPointers[0];
  end else
  begin
    SetLength(FBufferPointers, 0);
    FPPBuffer := nil;
  end;
end;

{ TMultiChannelAudioBuffer64 }

constructor TMultiChannelAudioBuffer64.Create;
begin

end;

destructor TMultiChannelAudioBuffer64.Destroy;
begin
  SetLength(FBuffer, 0, 0);
  SetLength(FBufferPointers, 0);
  FPPBuffer := nil;
  inherited;
end;

procedure TMultiChannelAudioBuffer64.SetSize(const Channels, SampleFrames: integer);
var
  c1: Integer;
begin
  SetLength(FBuffer, Channels, SampleFrames);

  if (Channels > 0) and (SampleFrames > 0) then
  begin
    SetLength(FBufferPointers, Channels);
    for c1 := 0 to Channels-1 do
    begin
      FBufferPointers[c1] := @FBuffer[c1,0];
    end;
    FPPBuffer := @FBufferPointers[0];
  end else
  begin
    SetLength(FBufferPointers, 0);
    FPPBuffer := nil;
  end;
end;


end.
