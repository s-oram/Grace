unit djfxAmpMod;

interface

uses
  eeGlobals, MoreTypes, djfxBase, eeSimpleLFO;

type
  TDJFXAmpMod = class(TDJFXCustomFX)
  private
    fSyncNumerator, fSyncDenominator:integer;
  protected
    LFO:TSimpleLfo;
    Depth:single;
    procedure SetParValue(Index: integer; const Value: single); override;

    procedure OnTempoChanged(Sender:TObject);
    procedure OnPlayStateChanged(Sender:TObject);
  public
    constructor Create(aGlobals:TGlobals); override;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); override;

    property SyncNumerator   :integer read fSyncNumerator;
    property SyncDenominator :integer read fSyncDenominator;
  end;

implementation

uses
  eeDSP, SysUtils;

{ TDJFXAmpMod }

constructor TDJFXAmpMod.Create(aGlobals: TGlobals);
begin
  inherited;

  Lfo := TSimpleLfo.Create;
  Lfo.Shape := slTriangle;


  Name          := 'Amp Mod';
  fFxType       := dxAmpMod;
  ParName[0]    := 'Rate';
  ParName[1]    := 'Depth';
  ParDisplay[0] := '--';
  ParDisplay[1] := '--';
  ParValue[0]   := 0.5;
  ParValue[1]   := 0.3;

  Globals.TempoList.Add(OnTempoChanged);
  Globals.PlayStateChangedList.Add(OnPlayStateChanged);
  
end;

destructor TDJFXAmpMod.Destroy;
begin
  Lfo.Free;
  inherited;
end;

procedure TDJFXAmpMod.OnPlayStateChanged(Sender: TObject);
begin
  if Globals.TransportPlaying = true then
  begin
    // Reset tempo sync'd LFO on play start. It helps to keep LFO results consistent in the
    // larger context of the song.
    Lfo.ResetPhase(0.5);
  end;
end;

procedure TDJFXAmpMod.OnTempoChanged(Sender: TObject);
var
  SyncSamples:single;
begin
  SyncSamples := SyncToSamples(SyncNumerator/SyncDenominator, Globals.Tempo, Globals.SampleRate);
  Lfo.RateSamples := SyncSamples;
end;

procedure TDJFXAmpMod.SetParValue(Index: integer; const Value: single);
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
      Depth := Value;
    end;

  end;

end;

procedure TDJFXAmpMod.Process(In1, In2: PSingle; SampleFrames: integer);
var
  c1: Integer;
  x:single;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    x := Lfo.Step;
    x := (x + 1) * 0.5 * Depth;
    x := 1 - x;     

    In1^ := In1^ * x;
    In2^ := In2^ * x;

    inc(In1);
    inc(In2);
  end;

end;




end.
