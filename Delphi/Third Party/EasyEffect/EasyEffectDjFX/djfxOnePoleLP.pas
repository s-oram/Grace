unit djfxOnePoleLP;

interface

uses
  eeGlobals, VamLib.MoreTypes, djfxBase, eeSimpleLFO, eeZeroDelayFilters;

type
  TDJFXOnePoleLP = class(TDJFXCustomFX)
  private
  protected
    Freq, res:double;
    BiLinT:TBiLinearTransform;
    yL, yR:double;

    LowPass1:TOnePoleLowPassFilter;
    LowPass2:TOnePoleLowPassFilter;
    LowPass3:TOnePoleLowPassFilter;
    LowPass4:TOnePoleLowPassFilter;
    LowPass5:TOnePoleLowPassFilter;
    LowPass6:TOnePoleLowPassFilter;
    LowPass7:TOnePoleLowPassFilter;
    LowPass8:TOnePoleLowPassFilter;

    procedure SetParValue(Index: integer; const Value: single); override;
  public
    constructor Create(aGlobals:TGlobals); override;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); override;
  end;


implementation

{ TDJFXOnePoleLP }

constructor TDJFXOnePoleLP.Create(aGlobals: TGlobals);
begin
  inherited;

  LowPass1 := TOnePoleLowPassFilter.Create;
  LowPass2 := TOnePoleLowPassFilter.Create;
  LowPass3 := TOnePoleLowPassFilter.Create;
  LowPass4 := TOnePoleLowPassFilter.Create;
  LowPass5 := TOnePoleLowPassFilter.Create;
  LowPass6 := TOnePoleLowPassFilter.Create;
  LowPass7 := TOnePoleLowPassFilter.Create;
  LowPass8 := TOnePoleLowPassFilter.Create;

  BiLinT := TBiLinearTransform.Create;
  yL := 0;
  yR := 0;

  Name          := '1xPole LP';
  fFxType       := dxOnePoleLP;
  ParName[0]    := 'Freq';
  ParName[1]    := 'Res';
  ParDisplay[0] := '--';
  ParDisplay[1] := '--';
  ParValue[0]   := 0.5;
  ParValue[1]   := 0.3;
end;

destructor TDJFXOnePoleLP.Destroy;
begin
  BiLinT.Free;
  LowPass1.Free;
  LowPass2.Free;
  LowPass3.Free;
  LowPass4.Free;
  LowPass5.Free;
  LowPass6.Free;
  LowPass7.Free;
  LowPass8.Free;
  inherited;
end;

procedure TDJFXOnePoleLP.SetParValue(Index: integer; const Value: single);
var
  x:double;
begin
  inherited;

  case Index of
    0:
    begin
      Freq := 15 + Value * 17000;


      x := exp((-2) * pi * freq / 44100);

      LowPass1.a0 := 1-x;
      LowPass1.b1 := -x;
      LowPass2.a0 := 1-x;
      LowPass2.b1 := -x;
      LowPass3.a0 := 1-x;
      LowPass3.b1 := -x;
      LowPass4.a0 := 1-x;
      LowPass4.b1 := -x;
      LowPass5.a0 := 1-x;
      LowPass5.b1 := -x;
      LowPass6.a0 := 1-x;
      LowPass6.b1 := -x;
      LowPass7.a0 := 1-x;
      LowPass7.b1 := -x;
      LowPass8.a0 := 1-x;
      LowPass8.b1 := -x;
    end;

    1:
    begin
      Res := Value * 0.9;
    end;
  end;
end;


procedure TDJFXOnePoleLP.Process(In1, In2: PSingle; SampleFrames: integer);
var
  g, s, w2:double;
  x:double;
  c1: Integer;
begin
  inherited;


  // NOTE: Well it seems to work some what. It breaks with high filter frequencies.
  // Check the Reaktor implementation.
  // Mine may need to be clamped.


  for c1 := 0 to SampleFrames - 1 do
  begin
    In1^ := LowPass1.Process(In1^);
//    In1^ := LowPass2.Process(In1^);
//    In1^ := LowPass3.Process(In1^);
//    In1^ := LowPass4.Process(In1^);
//    In1^ := LowPass5.Process(In1^);
//    In1^ := LowPass6.Process(In1^);
//    In1^ := LowPass7.Process(In1^);
//    In1^ := LowPass8.Process(In1^);

    In2^ := In1^;
    inc(In1);
    inc(In2);

    {
    x := In1^;
    g := 1;
    s := 0;
    w2 := PreWarpBT(Freq, Globals.SampleRate);

    BiLinT.Calc(x - yL, g, s, w2);
    yL := FeedbackSolver(x, g, s, 1);

    In1^ := yL;
    In2^ := yL;

    inc(In1);
    inc(In2);
    }
  end;


end;


end.
