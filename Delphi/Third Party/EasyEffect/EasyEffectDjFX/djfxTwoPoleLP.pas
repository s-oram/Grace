unit djfxTwoPoleLP;

interface

uses
  eeGlobals, MoreTypes, djfxBase, eeSimpleLFO, eeZeroDelayFilters;

type
  TDJFXTwoPoleLP = class(TDJFXCustomFX)
  private
  protected
    Freq, res:double;
    BiLinTL1, BiLinTL2:TBiLinearTransform;
    yL, yR:double;
    procedure SetParValue(Index: integer; const Value: single); override;
  public
    constructor Create(aGlobals:TGlobals); override;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); override;
  end;


implementation

{ TDJFXTwoPoleLP }

constructor TDJFXTwoPoleLP.Create(aGlobals: TGlobals);
begin
  inherited;


  BiLinTL1 := TBiLinearTransform.Create;
  BiLinTL2 := TBiLinearTransform.Create;
  yL := 0;
  yR := 0;

  Name          := '2 Pole LP';
  fFxType       := dxOnePoleLP;
  ParName[0]    := 'Freq';
  ParName[1]    := 'Res';
  ParDisplay[0] := '--';
  ParDisplay[1] := '--';
  ParValue[0]   := 0.5;
  ParValue[1]   := 0.3;

end;

destructor TDJFXTwoPoleLP.Destroy;
begin
  BiLinTL1.Free;
  BiLinTL2.Free;
  inherited;
end;

procedure TDJFXTwoPoleLP.SetParValue(Index: integer; const Value: single);
begin
  inherited;

  case Index of
    0:
    begin
      Freq := 15 + Value * 17000;
    end;

    1:
    begin
      Res := Value * 0.9;
    end;
  end;
end;

procedure TDJFXTwoPoleLP.Process(In1, In2: PSingle; SampleFrames: integer);
var
  d, g1, g2, s1, s2, w2:double;
  gx, sx:double;
  x:double;
  c1: Integer;
  OutL,OutR:double;

begin
  inherited;


  // NOTE: This filter doesn't work.


  for c1 := 0 to SampleFrames - 1 do
  begin
    x := In1^;

    g1 := 1;
    s1 := 0;
    w2 := PreWarpBT(Freq, Globals.SampleRate);
    d  := ResToD(Res);

    yL := BiLinTL1.Calc(x - yL, g1, s1, w2);
    g2 := g1;
    s2 := s1;

    OutL := BiLinTL2.Calc(yL, g2, s2, w2);

    gx := MultAdd(g1, d, g2);
    sx := MultAdd(s1, d, s2);

    yL := FeedbackSolver(x, gx, sx, 1);


    In1^ := OutL;
    In2^ := OutL;

    inc(In1);
    inc(In2);
  end;


end;





end.
