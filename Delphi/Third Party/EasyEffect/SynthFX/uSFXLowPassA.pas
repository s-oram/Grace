unit uSFXLowPassA;

interface

uses
  uSFXBase, MoreTypes, eeFilters;

type
  TSFXLowPassA = class(TSFXBase)
  private
  protected
    MoogLp:TMoogLp;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
	  destructor Destroy; override;

    procedure SetPar(Index:integer; Value:single); override;

    procedure Step(var In1, In2:Single); inline;
  end;

implementation

uses
  SysUtils;

{ TSFXLowPassA }

constructor TSFXLowPassA.Create;
begin
  inherited;
  SetParCount(2);

  fPar[0].Name    := 'Freq';
  fPar[1].Name    := 'Res';

  MoogLp := TMoogLp.Create;
  MoogLp.SampleRate := 44100;


  SetPar(0, 0.15);
  SetPar(1, 0.5);

end;

destructor TSFXLowPassA.Destroy;
begin
  MoogLp.Free;
  inherited;
end;

procedure TSFXLowPassA.SampleRateChanged;
begin
  inherited;
  MoogLp.SampleRate := self.SampleRate;
end;

procedure TSFXLowPassA.SetPar(Index: integer; Value: single);
var
  Freq, Res:single;
begin
  inherited;

  case Index of
    0:
    begin
      Freq := (Value * Value) * 17000 + 5;
      MoogLp.SetFreq(Freq);
      fPar[0].ValueDisplay := IntToStr(round(Value * 100));
    end;

    1:
    begin
      MoogLp.SetRes(Value);
      fPar[1].ValueDisplay := IntToStr(round(Value * 100));
    end;
  end;

end;

procedure TSFXLowPassA.Step(var In1, In2: Single);
begin
  In1 := MoogLp.Process(In1);
  In2 := In1;
end;

end.
