unit SynthModule_LowPass12;

interface

uses
  VamLib.MoreTypes, SynthModule_Custom;

const
  p4 = 1.0e-24;  //a small value to prevent denormals on p4 cpu's.

type
  TLowPass12 = class(TCustomSynthModule)
  private
    fParFreq: single;
    fParRes: single;
    fMinFreq: single;
    fMaxFreq: single;
    fParFreqModDepth: single;
    fParResModDepth: single;
  protected
    Lin1,Lin2,Lou1,Lou2:double;
    Rin1,Rin2,Rou1,Rou2:double;
    b0a0,b1a0,b2a0,a1a0,a2a0:double;

    procedure CalcFilterCoeffs; //inline;

  public
    Par1Mod : TSynthModuleInput;
    Par2Mod : TSynthModuleInput;

    constructor Create; override;
    destructor Destroy; override;

    procedure DisconnectAllInputs; override;

    procedure ControlRateStep; //inline;
    procedure AudioRateStep(var x1,x2:Single); //inline;

    property ParFreq : single read fParFreq write fParFreq; //range 0..1
    property ParRes  : single read fParRes  write fParRes;  //range 0..1
    property ParFreqModDepth : single read fParFreqModDepth write fParFreqModDepth; //range 0..1
    property ParResModDepth  : single read fParResModDepth  write fParResModDepth;

    property MinFreq : single read fMinFreq write fMinFreq; //Hertz
    property MaxFreq : single read fMaxFreq write fMaxFreq; //Hertz

  end;



implementation

uses
  Math;

{ TLowPass12 }

constructor TLowPass12.Create;
begin
  inherited;

  DisconnectAllInputs;

  MinFreq := 10;
  MaxFreq := 20000;

  fParFreq := 0;
  fParRes  := 0;
  fParFreqModDepth := 0;

  CalcFilterCoeffs;
end;

destructor TLowPass12.Destroy;
begin

  inherited;
end;

procedure TLowPass12.DisconnectAllInputs;
begin
  inherited;

  Par1Mod.Disconnect;
  Par2Mod.Disconnect;
end;

procedure TLowPass12.CalcFilterCoeffs;
var
  alpha,a0,a1,a2,b0,b1,b2:single;
  omega,tsin,tcos:single;
  tx : single;
  FreqHz : single;
  Q      : single;
begin
  tx := fParFreq + (Par1Mod.Value^ * fParFreqModDepth);
  if tx > 1 then tx := 1;
  if tx < 0 then tx := 0;
  FreqHz := MinFreq + (MaxFreq - MinFreq) * tx;


  tx := fParRes + Par2Mod.Value^ * fParResModDepth;
  if tx > 1 then tx := 1;
  if tx < 0 then tx := 0;
  Q := (1 - tx) * 0.96;

  omega:=2 * pi * FreqHz / AudioRate;
  tsin:=sin(omega);
  tcos:=cos(omega);
  alpha:=tsin*sinh(log2(2)/2* Q *omega/tsin);

  b0:=(1-tcos)/2;
  b1:=1-tcos;
  b2:=(1-tcos)/2;
  a0:=1+alpha;
  a1:=-2*tcos;
  a2:=1-alpha;

  b0a0:=b0/a0;
  b1a0:=b1/a0;
  b2a0:=b2/a0;
  a1a0:=a1/a0;
  a2a0:=a2/a0;

end;


procedure TLowPass12.ControlRateStep;
begin
  CalcFilterCoeffs;
end;

procedure TLowPass12.AudioRateStep(var x1, x2: Single);
begin
  //Channel 1
  Lin2:=Lin1;
  Lin1:=x1;
  x1:= b0a0 * (x1) + (b1a0 * Lin1) + (b2a0 * Lin2) - (a1a0 * Lou1) - (a2a0 * Lou2);
  Lou2:=Lou1;
  Lou1:=x1 + p4;

  //Channel 2
  Rin2:=Rin1;
  Rin1:=x2;
  x2:= b0a0 * (x2) + (b1a0 * Rin1) + (b2a0 * Rin2) - (a1a0 * Rou1) - (a2a0 * Rou2);
  Rou2:=Rou1;
  Rou1:=x2 + p4;
end;





end.
