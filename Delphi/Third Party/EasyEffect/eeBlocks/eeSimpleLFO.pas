{
  A very simple LFO class.
}

unit eeSimpleLFO;

interface

const
  UniOutputScaler : double = 1 / 4294967295; //One over High(Cardinal). Use to scale the phase position to 0..1, ie Uni-polar LFO output.
  BiOutputScaler  : double = 2 / 4294967295; //Two over High(Cardinal). Use to scale the phase position to -1..1, ie Bi-polar LFO output.

type
  TSimpleLfoShape = (slRamp, slSaw, slTriangle, slSine);

  TSimpleLfo = class
  private
    fShape: TSimpleLfoShape;
    fRateSamples: single;
    fRate: single;
    fSampleRate: integer;
    procedure SetRateSamples(const Value: single);
    procedure SetRate(const Value: single);
    procedure SetSampleRate(const Value: integer);
  protected
    PhaseCounter: cardinal;
    StepSize    : cardinal;
  public
    constructor Create;
	  destructor Destroy; override;

    procedure ResetPhase(ph:single); //range 0..1

    procedure StepUniPolarQuintet(out x1, x2, x3, x4, x5: single);

    function Step:single; inline; //Process one sample frame.

    property Shape           :TSimpleLfoShape read fShape write fShape;

    property Rate            : single          read fRate        write SetRate;        //In hertz. Requires sampleRate value to be correct.
    property RateSamples     : single          read fRateSamples write SetRateSamples; //In samples. (Depreciate this property perhaps?

    property SampleRate      : integer        read fSampleRate  write SetSampleRate;
  end;

implementation

uses
  eeDsp;



{ TSimpleLfo }

constructor TSimpleLfo.Create;
begin
  fSampleRate      := 44100;

  Shape            := slTriangle;
  PhaseCounter     := 0;
  Rate             := 0.5; //hertz.
end;

destructor TSimpleLfo.Destroy;
begin

  inherited;
end;

procedure TSimpleLfo.ResetPhase(ph: single);
begin
  assert(ph >= 0);
  assert(ph <= 1);

  PhaseCounter := round(High(Cardinal) * ph);
end;

procedure TSimpleLfo.SetRate(const Value: single);
begin
  fRate := Value;

  RateSamples :=  fSampleRate / fRate;
end;

procedure TSimpleLfo.SetRateSamples(const Value: single);
begin
  assert(Value > 1); //not a realistic rate value, but valid none the less.

  fRateSamples := Value;

  StepSize := High(Cardinal) div round(fRateSamples);
end;

procedure TSimpleLfo.SetSampleRate(const Value: integer);
begin
  fSampleRate := Value;

  //Update samplerate dependent properties..
  Rate := fRate;
end;


//Is this Step() result uni-polar or bi-polar??
function TSimpleLfo.Step: single;
var
  PhaseAsFloat : single;
  tx           : single;
begin
  PhaseAsFloat := PhaseCounter * BiOutputScaler - 1;

  case Shape of
    slRamp:     tx := PhaseAsFloat;
    slSaw:      tx := PhaseAsFloat * -1;
    slTriangle: tx := abs(PhaseAsFloat) * 2 - 1;
    slSine:     tx := (PhaseAsFloat * (1 - abs(PhaseAsFloat))) * 4;  //Approximation of a sine. It has overtones.
  end;

  assert(tx <= 1);
  assert(tx >= -1);


  result := tx;

  PhaseCounter := PhaseCounter + StepSize;
end;

procedure TSimpleLfo.StepUniPolarQuintet(out x1, x2, x3, x4, x5: single);
//Outputs five LFO values, phased equally apart from each other...
const
  Offset1 : cardinal = 0;
  Offset2 : cardinal = 858993459;  //High(Cardinal) * 1/5
  Offset3 : cardinal = 1717986918; //High(Cardinal) * 2/5
  Offset4 : cardinal = 2576980377; //High(Cardinal) * 3/5
  Offset5 : cardinal = 3435973836; //High(Cardinal) * 4/5
var
  cx1, cx2, cx3, cx4, cx5 : cardinal;
  fx1, fx2, fx3, fx4, fx5 : single;
begin
  cx1 := PhaseCounter + Offset1;
  cx2 := PhaseCounter + Offset2;
  cx3 := PhaseCounter + Offset3;
  cx4 := PhaseCounter + Offset4;
  cx5 := PhaseCounter + Offset5;

  case Shape of
    slRamp:
    begin
      x1 := cx1 * UniOutputScaler;
      x2 := cx2 * UniOutputScaler;
      x3 := cx3 * UniOutputScaler;
      x4 := cx4 * UniOutputScaler;
      x5 := cx5 * UniOutputScaler;
    end;

    slSaw:
    begin
      x1 := cx1 * UniOutputScaler * -1;
      x2 := cx2 * UniOutputScaler * -1;
      x3 := cx3 * UniOutputScaler * -1;
      x4 := cx4 * UniOutputScaler * -1;
      x5 := cx5 * UniOutputScaler * -1;
    end;

    slTriangle:
    begin
      x1 := abs(cx1 * BiOutputScaler - 1);
      x2 := abs(cx2 * BiOutputScaler - 1);
      x3 := abs(cx3 * BiOutputScaler - 1);
      x4 := abs(cx4 * BiOutputScaler - 1);
      x5 := abs(cx5 * BiOutputScaler - 1);
    end;

    slSine:
    begin
      // This is only an approximation of a sine. It has overtones. It should be fine for a LFO though.

      //Convert integer phase to float phase.
      fx1 := cx1 * BiOutputScaler - 1;
      fx2 := cx2 * BiOutputScaler - 1;
      fx3 := cx3 * BiOutputScaler - 1;
      fx4 := cx4 * BiOutputScaler - 1;
      fx5 := cx5 * BiOutputScaler - 1;

      //Approximate the sine...
      x1 := (fx1 * (1 - abs(fx1))) * 4;
      x2 := (fx2 * (1 - abs(fx2))) * 4;
      x3 := (fx3 * (1 - abs(fx3))) * 4;
      x4 := (fx4 * (1 - abs(fx4))) * 4;
      x5 := (fx5 * (1 - abs(fx5))) * 4;
    end;
  end;



  PhaseCounter := PhaseCounter + StepSize;
end;

end.
