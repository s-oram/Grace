unit uSFXBase;

interface

type
  PSFXPar = ^TSFXPar;
  TSFXPar = record
    Name:string;
    Value:single; //range 0..1
    ValueDisplay:string;
  end;


  TSFXBase = class
  private
    fParCount: integer;
    fSampleRate: integer;
    function GetPar(Index: integer): TSFXPar;
    procedure SetSampleRate(const Value: integer);
  protected
    fPar:array of TSFXPar;
    procedure SetParCount(aCount:integer);
    procedure SampleRateChanged; virtual;
  public
    constructor Create; virtual;
	  destructor Destroy; override;
    property ParCount:integer read fParCount;

    procedure SetPar(Index:integer; Value:single); virtual;
    function GetParValue(Index:integer):single;

    property Par[Index:integer]:TSFXPar read GetPar;

    property SampleRate:integer read fSampleRate write SetSampleRate;
  end;

implementation

{ TSFXBase }

constructor TSFXBase.Create;
begin
  fSampleRate := 44100;
end;

destructor TSFXBase.Destroy;
begin
  SetLength(fPar, 0);
  inherited;
end;

function TSFXBase.GetParValue(Index: integer): single;
begin
  result := fPar[Index].Value;
end;

function TSFXBase.GetPar(Index: integer): TSFXPar;
begin
  result := fPar[Index];
end;

procedure TSFXBase.SampleRateChanged;
begin

end;

procedure TSFXBase.SetPar(Index: integer; Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fPar[Index].Value := Value;

end;

procedure TSFXBase.SetParCount(aCount: integer);
begin
  fParCount := 0;
  SetLength(fPar, aCount);
end;

procedure TSFXBase.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
  SampleRateChanged;
end;

end.
