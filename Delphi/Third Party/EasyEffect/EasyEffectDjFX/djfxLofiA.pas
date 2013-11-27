{
  A classic lofi effect.
  Parameter 1 - Sample rate reduction
  Parameter 2 - Bit rate reduction.

  It's called LofiA because it probably will not be the only lofi effect.

}

unit djfxLofiA;

interface

uses
  eeGlobals, MoreTypes, djfxBase;

type
  TDJFXLofiA = class(TDJFXCustomFX)
  protected
    x1, x2:single;
    LastSampleL, LastSampleR:single;
    StepPos:double;
    StepSize:double;
    BitDepthFactor:single;
    OneOverBitDepthFactor:single;
    procedure SetParValue(Index: integer; const Value: single); override;
  public
    constructor Create(aGlobals:TGlobals); override;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); override;
  end;

implementation

uses
  Math, SysUtils, eeDSP;

{ TDJFXLofiA }

constructor TDJFXLofiA.Create(aGlobals: TGlobals);
begin
  inherited;

  Name          := 'Lofi A';
  fFxType       := dxLofiA;
  ParName[0]    := 'Samplerate';
  ParName[1]    := 'BitDepth';
  ParDisplay[0] := '--';
  ParDisplay[1] := '--';
  ParValue[0]   := 1;
  ParValue[1]   := 1;

  LastSampleL := 0;
  LastSampleR := 0;
  x1          := 0;
  x2          := 0;

  StepPos := 0;
  StepSize := 1;
end;

destructor TDJFXLofiA.Destroy;
begin

  inherited;
end;

procedure TDJFXLofiA.SetParValue(Index: integer; const Value: single);
var
  tx:single;
  ps:single;

begin
  inherited;

  case Index of
    0:
    begin
      ps := ParScaleSquare(Value);
      ps := ParScaleCustom(ps, 0.6, 0.2, 0.8, 0.4);
      StepSize := 0.001 + ps * 0.999;
      ParDisplay[0] := IntToStr(round(Globals.SampleRate * StepSize));
      assert(StepSize <= 1);
    end;

    1:
    begin
      ParDisplay[1] := IntToStr(round(Value * 1000));
      ps := Value * Value * Value;
      ps := ParScaleCustom(ps, 0.4, 0.1, 0.8, 0.4);
      tx := 1 + ps * 30;
      BitDepthFactor := Power(2, tx);
      OneOverBitDepthFactor := 1 / BitDepthFactor;
    end;
  end;


end;

procedure TDJFXLofiA.Process(In1, In2: PSingle; SampleFrames: integer);
var
  c1: Integer;

begin
  for c1 := 0 to SampleFrames - 1 do
  begin

    StepPos := StepPos + StepSize;

    if StepPos >= 1 then
    begin
      StepPos := StepPos - 1;

      assert(StepPos >= 0);
      assert(StepPos <= 1);

      x1 := (LastSampleL * (1 - StepPos)) + (In1^ * StepPos);
      x2 := (LastSampleR * (1 - StepPos)) + (In2^ * StepPos);

      x1 := round(x1 * BitDepthFactor) * OneOverBitDepthFactor;
      x2 := round(x2 * BitDepthFactor) * OneOverBitDepthFactor;

    end;

    LastSampleL := In1^;
    LastSampleR := In2^;

    In1^ := x1;
    In2^ := x2;

    inc(In1);
    inc(In2);
  end;

end;



end.
