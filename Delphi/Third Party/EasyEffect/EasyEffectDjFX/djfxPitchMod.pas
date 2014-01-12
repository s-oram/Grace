{
  TODO: I think the DEPTH value will need to be smoothed.

  - Use integer math for the read pos.
  - use a SINE lfo shape.
}

unit djfxPitchMod;

interface

uses
  eeGlobals, VamLib.MoreTypes, djfxBase, eeSimpleLFO;

type
  TDJFXPitchMod = class(TDJFXCustomFX)
  private
    fSyncNumerator, fSyncDenominator:integer;
  protected
    Lfo:TSimpleLfo;

    DBufferL,DBufferR:array of single;
    BufferSize:integer;

    Depth:single;
    ReadPos:single;
    WritePos:integer;
    procedure SetParValue(Index: integer; const Value: single); override;

    procedure OnTempoChanged(Sender:TObject);
    procedure OnPlayStateChanged(Sender:TObject);
    procedure OnSampleRateChanged(Sender:TObject);
  public
    constructor Create(aGlobals:TGlobals); override;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); override;

    property SyncNumerator   :integer read fSyncNumerator;
    property SyncDenominator :integer read fSyncDenominator;
  end;


implementation

uses
  SysUtils, eeDSP, Math;


{ TDJFXPitchMod }

constructor TDJFXPitchMod.Create(aGlobals: TGlobals);
begin
  inherited;

  Lfo := TSimpleLfo.Create;
  //Lfo.Shape := slTriangle;  //should be sine?
  Lfo.Shape := slSine;

  BufferSize := round(Globals.SampleRate * 0.1);
  SetLength(DBufferL, BufferSize);
  SetLength(DBufferR, BufferSize);


  Name          := 'Pitch Mod';
  fFxType       := dxPitchMod;
  ParName[0]    := 'Rate';
  ParName[1]    := 'Depth';
  ParDisplay[0] := '--';
  ParDisplay[1] := '--';
  ParValue[0]   := 0.5;
  ParValue[1]   := 0.3;

  Globals.TempoList.Add(OnTempoChanged);
  Globals.SampleRateList.Add(OnSampleRateChanged);
  Globals.PlayStateChangedList.Add(OnPlayStateChanged);

  WritePos := 0;

end;

destructor TDJFXPitchMod.Destroy;
begin
  Lfo.Free;
  SetLength(DBufferL, 0);
  SetLength(DBufferR, 0);
  inherited;
end;

procedure TDJFXPitchMod.OnPlayStateChanged(Sender: TObject);
begin
  if Globals.TransportPlaying = true then
  begin
    // Reset tempo sync'd LFO on play start. It helps to keep LFO results consistent in the
    // larger context of the song.
    Lfo.ResetPhase(0);
  end;
end;

procedure TDJFXPitchMod.OnSampleRateChanged(Sender: TObject);
begin
  BufferSize := round(Globals.SampleRate * 0.1);
  SetLength(DBufferL, BufferSize);
  SetLength(DBufferR, BufferSize);
end;

procedure TDJFXPitchMod.OnTempoChanged(Sender: TObject);
var
  SyncSamples:single;
begin
  SyncSamples := SyncToSamples(SyncNumerator/SyncDenominator, Globals.Tempo, Globals.SampleRate);
  Lfo.RateSamples := SyncSamples;
end;

procedure TDJFXPitchMod.SetParValue(Index: integer; const Value: single);
var
  SyncSamples:single;
begin
  inherited;

  case Index of
    0:
    begin
      CalcSyncTimeA(Value, fSyncNumerator, fSyncDenominator);
      ParDisplay[0] := IntToStr(SyncNumerator) + '/' + IntToStr(SyncDenominator);

      SyncSamples := SyncToSamples(SyncNumerator/SyncDenominator, Globals.Tempo, Globals.SampleRate);
      Lfo.RateSamples := SyncSamples;
    end;

    1:
    begin
      ParDisplay[1] := IntToStr(Round(Value * 100));
      Depth := Value * Value;
    end;

  end;


end;

procedure TDJFXPitchMod.Process(In1, In2: PSingle; SampleFrames: integer);
var
  c1: Integer;
  x:single;
  ReadPos:single;
  a,b:integer;
  f:single;
  txL, txR:single;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    DBufferL[WritePos] := In1^;
    DBufferR[WritePos] := In2^;

    x := Lfo.Step;

    x := (x + 1) * 0.5 * Depth;

    ReadPos := WritePos - (x * (BufferSize-10)) - 1;
    a := Floor(ReadPos);
    b := a + 1;
    f := ReadPos - a;
    if a < 0 then a := a + BufferSize;
    if b < 0 then b := b + BufferSize;

    txL := (DBufferL[a] * (1 - f)) + (DBufferL[b] * f);
    txR := (DBufferR[a] * (1 - f)) + (DBufferR[b] * f);

    In1^ := txL;
    In2^ := txR;

    inc(In1);
    inc(In2);

    inc(WritePos);
    if WritePos >= BufferSize then WritePos := WritePos - BufferSize;
  end;

end;




end.
