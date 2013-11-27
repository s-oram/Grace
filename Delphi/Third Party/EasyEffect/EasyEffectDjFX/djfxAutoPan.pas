{

  NOTE: When storing the state of the AutoPan, remember to convert
  the RATE parameter value to Sync Numerator/Denominator value to
  ensure the RATE related methods can be updated whilst maintaining
  backwards compatibility.
}

unit djfxAutoPan;

interface

uses
  eeGlobals, MoreTypes, djfxBase, eeSimpleLFO;

type
  TDJFXAutoPan = class(TDJFXCustomFX)
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
  SysUtils, eeDSP;

{ TDJFXAutoPan }

constructor TDJFXAutoPan.Create(aGlobals: TGlobals);
begin
  inherited;

  LFO := TSimpleLfo.Create;
  LFO.Shape := slTriangle;

  Name          := 'Auto Pan';
  fFxType       := dxAutoPan;
  ParName[0]    := 'Rate';
  ParName[1]    := 'Depth';
  ParDisplay[0] := '--';
  ParDisplay[1] := '--';
  ParValue[0]   := 0.5;
  ParValue[1]   := 0.3;

  Globals.TempoList.Add(OnTempoChanged);
  Globals.PlayStateChangedList.Add(OnPlayStateChanged);


end;

destructor TDJFXAutoPan.Destroy;
begin
  Lfo.Free;
  inherited;
end;

procedure TDJFXAutoPan.SetParValue(Index: integer; const Value: single);
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


procedure TDJFXAutoPan.OnTempoChanged(Sender: TObject);
var
  SyncSamples:single;
begin
  SyncSamples := SyncToSamples(SyncNumerator/SyncDenominator, Globals.Tempo, Globals.SampleRate);
  Lfo.RateSamples := SyncSamples;
end;

procedure TDJFXAutoPan.OnPlayStateChanged(Sender: TObject);
begin
  if Globals.TransportPlaying = true then
  begin
    // Reset tempo sync'd LFO on play start. It helps to keep LFO results consistent in the
    // larger context of the song.
    Lfo.ResetPhase(0.25);            
  end;
end;




procedure TDJFXAutoPan.Process(In1, In2: PSingle; SampleFrames: integer);
var
  c1: Integer;
  x:single;
  VolumeL, VolumeR:single;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    x := Lfo.Step;
    x := x * Depth;

    if x > 0 then
    begin
      // NOTE: The (0.3 * x) is volume compensation so the the center doesn't appear louder then the
      // extreame pan positions.
      VolumeL := 1 - x;
      VolumeR := 1 + (0.3 * x);  
    end else
    if x < 0 then
    begin
      VolumeL := 1 + (0.3 * x * -1);
      VolumeR := 1 - (x * -1);
    end else
    begin
      VolumeL := 1;
      VolumeR := 1;
    end;

    In1^ := In1^ * VolumeL;
    In2^ := In2^ * VolumeR;

    inc(In1);
    inc(In2);
  end;

end;



end.
