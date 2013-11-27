unit djfxMoogLP;

interface

uses
  eeGlobals, MoreTypes, djfxBase, eeFilters;

type
  TDJFXMoogLP = class(TDJFXCustomFX)
  private
  protected
    MoogLp:TMoogLp;
    procedure SetParValue(Index: integer; const Value: single); override;
  public
    constructor Create(aGlobals:TGlobals); override;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); override;
  end;


implementation

uses
  SysUtils;

{ TDJFXOnePoleLP }

constructor TDJFXMoogLP.Create(aGlobals: TGlobals);
begin
  inherited;

  MoogLp := TMoogLp.Create;
  MoogLp.SampleRate := round(aGlobals.SampleRate);

  Name          := 'Moog LP';
  fFxType       := dxMoogLp;
  ParName[0]    := 'Freq';
  ParName[1]    := 'Res';
  ParDisplay[0] := '--';
  ParDisplay[1] := '--';
  ParValue[0]   := 0.5;
  ParValue[1]   := 0.3;
end;

destructor TDJFXMoogLP.Destroy;
begin
  MoogLp.Free;
  inherited;
end;

procedure TDJFXMoogLP.SetParValue(Index: integer; const Value: single);
var
  Freq:single;
begin
  inherited;

  case Index of
    0:
    begin
      Freq := (Value * Value) * 17000 + 5;
      MoogLp.SetFreq(Freq);
      ParDisplay[0] := IntToStr(round(Value * 100));
    end;

    1:
    begin
      MoogLp.SetRes(Value);
      ParDisplay[1] := IntToStr(round(Value * 100));
    end;
  end;
end;

procedure TDJFXMoogLP.Process(In1, In2: PSingle; SampleFrames: integer);
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames - 1 do
  begin
    In1^ := MoogLp.Process(In1^);
    In2^ := In1^;

    inc(In1);
    inc(In2);
  end;
end;



end.
