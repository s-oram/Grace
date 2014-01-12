unit uBlitOsc;

interface

uses
  VamLib.MoreTypes, uDspFunctions, eeFilters.DcBlocker, eePitch, Math, eeFastDsp;

type
  TBlitOsc = class
  private
    fFreq: single;
    fPulseWidth: single;
    fSampleRate: single;
    procedure SetFreq(const Value: single);
    procedure SetSampleRate(const Value: single);
  protected
    Phase:single;
    StepSize:single;
    Harmonics:single;
    PeriodLength:single;
    Square   : double;
    Triangle : double;
    Saw      : double;
    DcBlocker : TDcBlocker;
    procedure CalcStepSize;
    procedure SampleRateChanged(Sender:TObject);
  public
    constructor Create;
	  destructor Destroy; override;

    function ProcessAsBlit : single;

    function ProcessAsSquare:single; inline;
    function ProcessAsTriangle:single; inline;
    function ProcessAsSaw:single; inline;

    property Freq:single read fFreq write SetFreq;  //hertz
    property PulseWidth:single read fPulseWidth write fPulseWidth; //0..1


    property SampleRate : single read fSampleRate write SetSampleRate;

  end;

implementation




{ TBlitSquareOsc }

constructor TBlitOsc.Create;
begin
  //set Default values
  Freq := 155;
  PulseWidth := 0.5;
  fSampleRate := 44100;


  //Init some storage variables to 0.
  Phase := 0;
  Square := 0;
  Triangle := 0;
  Saw := 0;

  DcBlocker := TDcBlocker.Create;
end;


destructor TBlitOsc.Destroy;
begin
  DcBlocker.Free;
  inherited;
end;

procedure TBlitOsc.SetFreq(const Value: single);
begin
  fFreq := Value;
  CalcStepSize;
end;

procedure TBlitOsc.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
  SampleRateChanged(self);
end;

procedure TBlitOsc.SampleRateChanged(Sender: TObject);
begin
  CalcStepSize;
end;

procedure TBlitOsc.CalcStepSize;
begin
  StepSize := 1;
  StepSize := 1 / CpsToSamples(Freq,SampleRate);

  PeriodLength := (CpsToSamples(Freq,SampleRate));
  Harmonics    := floor( PeriodLength * 0.5 ) - 1;
  //Harmonics    := 10;

end;

function TBlitOsc.ProcessAsSquare: single;
var
  Blit1,Blit2:single;
  BiPolarBlit:single;
begin
  Phase := Phase + StepSize;
  if Phase >= 1 then Phase := Phase - 1;

  Blit1 := Blit((Phase * PeriodLength), Harmonics, PeriodLength);
  Blit2 := Blit((Phase * PeriodLength) + (PeriodLength * PulseWidth), Harmonics, PeriodLength);

  BiPolarBlit := Blit1 - Blit2;

  //Bleed off the DC offset.
  Square := Square * 0.999;
  //Square := Square * 0.99999;

  //TODO: The DC bleed off stage needs to be improved. To much is bleed at low osc frequencies.

  Square := Square + BiPolarBlit;

  result := Square;

end;

function TBlitOsc.ProcessAsTriangle: single;
var
  Blit1,Blit2:single;
  BiPolarBlit:single;
  //k : double;
  //g : double;
  //x1 : single;
begin
  Phase := Phase + StepSize;
  if Phase >= 1 then Phase := Phase - 1;

  Blit1 := Blit((Phase * PeriodLength), Harmonics, PeriodLength);
  Blit2 := Blit((Phase * PeriodLength) + (PeriodLength * 0.5), Harmonics, PeriodLength);

  BiPolarBlit := Blit1 - Blit2;

  //Bleed off the DC offset.
  Square := Square * 0.9999;

  //TODO: The DC bleed off stage needs to be improved. To much is bleed at low osc frequencies.
  Square := Square + BiPolarBlit;

  //k := PeriodLength * PulseWidth;
  //g := 0.99 / (PeriodLength*(k/PeriodLength)*(1-k/PeriodLength));

  //triangle := Triangle + g * (Square+k/PeriodLength);

  Triangle := Triangle + (square * (1 / PeriodLength));

  //Bleed off the DC offset.
  Triangle := Triangle * 0.999;

  Triangle := Fast_tanh(Triangle);

  //DcBlocker.Process(Square);
  //x1 := Square;

  result := Triangle * 12;
  //result := Triangle;

end;

function TBlitOsc.ProcessAsBlit: single;
var
  Blit1,Blit2:single;
begin
  Phase := Phase + StepSize;
  if Phase >= 1 then Phase := Phase - 1;

  assert(Phase <= 1);
  assert(Phase >= 0);

  Blit1 := Blit((Phase * PeriodLength), Harmonics, PeriodLength);
  assert(Blit1 <= 1);
  assert(Blit1 >= -1);
  result := Blit1;
end;

function TBlitOsc.ProcessAsSaw: single;
var
  Blit1,Blit2:single;
  BiPolarBlit:single;
  //k : double;
  //g : double;
  //x1 : single;
begin
  Phase := Phase + StepSize;
  if Phase >= 1 then Phase := Phase - 1;

  Blit1 := Blit((Phase * PeriodLength), Harmonics, PeriodLength);


  Saw := Saw + (Blit1 - 0.5);

  //Bleed off the DC offset.
  Saw := Saw * 0.9999;


  //TODO: The DC bleed off stage needs to be improved. To much is bleed at low osc frequencies.
  Square := Square + BiPolarBlit;

  //k := PeriodLength * PulseWidth;
  //g := 0.99 / (PeriodLength*(k/PeriodLength)*(1-k/PeriodLength));

  //triangle := Triangle + g * (Square+k/PeriodLength);

  Triangle := Triangle + (square * (1 / PeriodLength));

  //Bleed off the DC offset.
  Triangle := Triangle * 0.999;

  Triangle := Fast_tanh(Triangle);

  //DcBlocker.Process(Square);
  //x1 := Square;

  result := Saw;
  //result := Triangle;

end;




end.
