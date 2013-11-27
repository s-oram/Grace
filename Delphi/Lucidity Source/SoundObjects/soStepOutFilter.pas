unit soStepOutFilter;

interface

type
  TStepOutFilter = class
  private
    RC1 : double;
    Oldx1, Oldx2 : single;
    Changex1, Changex2 : single;

    WorkingChangex1, WorkingChangeX2 : double;
    OffsetX1, OffsetX2 : double;
    fSampleRate: integer;
    fDecayTime: double;
    procedure SetDecayTime(const Value: double);
    procedure SetSampleRate(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Trigger;

    procedure Process(var x1, x2 : single);

    property DecayTime  : double read fDecayTime write SetDecayTime; //in milliseconds.
    property SampleRate : integer read fSampleRate write SetSampleRate;
  end;

implementation

uses
  eeDsp;

{ TStepOutFilter }

constructor TStepOutFilter.Create;
begin
  ChangeX1 := 0;
  ChangeX2 := 0;
  OldX1    := 0;
  OldX2    := 0;

  WorkingChangeX1 := 0;
  WorkingChangeX2 := 0;
  OffsetX1 := 0;
  Offsetx2 := 0;

  fDecayTime := 3;
  fSampleRate := 44100;
end;

destructor TStepOutFilter.Destroy;
begin
  inherited;
end;

procedure TStepOutFilter.Process(var x1, x2: single);
begin
  ChangeX1 := x1 - OldX1;
  ChangeX2 := x2 - OldX2;

  OldX1 := x1;
  OldX2 := x2;

  OffSetx1 := OffsetX1 + WorkingChangeX1;
  OffSetx2 := OffsetX2 + WorkingChangeX2;

  x1 := x1 + OffsetX1;
  x2 := x2 + OffsetX2;

  OffsetX1 := OffsetX1 * rc1;
  OffsetX2 := OffsetX2 * rc1;

  WorkingChangeX1 := WorkingChangeX1 * rc1;
  WorkingChangeX2 := WorkingChangeX2 * rc1;

end;

procedure TStepOutFilter.SetDecayTime(const Value: double);
begin
  fDecayTime := Value;
end;

procedure TStepOutFilter.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;
end;

procedure TStepOutFilter.Trigger;
begin
  WorkingChangeX1 := ChangeX1;
  WorkingChangeX2 := ChangeX2;

  OffsetX1 := OldX1;
  Offsetx2 := OldX2;

  rc1 := CalcRcEnvelopeCoefficient(DecayTime, SampleRate);
end;

end.
