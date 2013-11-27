unit eeBiquadFilterCore;

interface

uses
  MoreTypes;


  //NOTES:
  // Digital Biquad Filter
  // http://en.wikipedia.org/wiki/Digital_biquad_filter

  // Earlevel blog. A good artical on transposed IIR filter forms.
  // http://www.earlevel.com/main/2003/02/28/biquads/

  // Example working filter coefficients..
  //Filter.B0_Coeff := 0.034785961822381306;
  //Filter.B1_Coeff := 0.06957192364476261;
  //Filter.B2_Coeff := 0.034785961822381306;
  //Filter.A1_Coeff := -1.407502284220597;
  //Filter.A2_Coeff := 0.5466461315101225;


  // Direct Form I, II and Transposed are implemented with reference to
  // Understanding Digital Signal Processing (2nd Edition)
  // by Richard Lyons page 242 (figure 6-22).

type
  TBiquadFilterCore = class
  private
  protected
    b0: double;
    b1: double;
    b2: double;
    a1: double;
    a2: double;
    z1  : double;
    z2  : double;

    z1A : double;
    z2A : double;
    z1B : double;
    z2B : double;

    In0, In1, In2, Out1, Out2 : double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; overload;

    // Coefficients... NOTE: Depending on where the coefficients are sourced from,
    // the coefficent labels may be swapped. IE. A coefficients are labeled B and
    // vice-versa.
    property B0_Coeff : double read b0 write b0;
    property B1_Coeff : double read b1 write b1;
    property B2_Coeff : double read b2 write b2;

    property A1_Coeff : double read a1 write a1;
    property A2_Coeff : double read a2 write a2;

    function Step_DirectForm1(const x : single):single; inline;
    function Step_DirectForm2(const x : single):single; inline;
    function Step_DirectForm2Transposed(const x : single):single; inline;

    procedure Process_DirectForm1(px : PSingle; const SampleFrames : integer);
    procedure Process_DirectForm2(px : PSingle; const SampleFrames : integer);
    procedure Process_TransposedDirectForm2(px : PSingle; const SampleFrames : integer);
  end;


implementation

{ TBiquadFilterCore }

constructor TBiquadFilterCore.Create;
begin
  Reset;
end;

destructor TBiquadFilterCore.Destroy;
begin

  inherited;
end;

procedure TBiquadFilterCore.Reset;
begin
  z1  := 0;
  z2  := 0;

  In0 := 0;
  In1 := 0;
  In2 := 0;
  Out1 := 0;
  Out2 := 0;
end;

procedure TBiquadFilterCore.Process_DirectForm1(px: PSingle; const SampleFrames: integer);
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    px^ := self.Step_DirectForm1(px^);
    inc(px);
  end;
end;

procedure TBiquadFilterCore.Process_DirectForm2(px: PSingle; const SampleFrames: integer);
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    px^ := self.Step_DirectForm2(px^);
    inc(px);
  end;
end;

procedure TBiquadFilterCore.Process_TransposedDirectForm2(px: PSingle; const SampleFrames: integer);
var
  c1: Integer;
begin
  for c1 := 0 to SampleFrames-1 do
  begin
    px^ := self.Step_DirectForm2Transposed(px^);
    inc(px);
  end;
end;



function TBiquadFilterCore.Step_DirectForm1(const x: single): single;
var
  outx : double;
begin
  In0 := In1;
  In1 := In2;
  In2 := x;

  Outx := (b0 * In0) + (b1 * In1) + (b2 * in2) + (a1 * Out1) + (a2 * Out2);

  Out2 := Out1;
  Out1 := Outx;

  result := Outx;
end;

function TBiquadFilterCore.Step_DirectForm2(const x: single): single;
var
  c1: Integer;
  temp : double;
  outX : double;
begin
  temp := x + (a1 * z1) + (a2 * z2);
  result := (b0 * temp) + (b1 * z1) + (b2 * z2);

  z2 := z1;
  z1 := temp;
end;


function TBiquadFilterCore.Step_DirectForm2Transposed(const x: single): single;
var
  Temp : double;
begin
  temp := (b0 * x) + z1;
  z1   := (b1 * x) + (a1 * temp) + z2;
  z2   := (b2 * x) + (a2 * temp);
  result := temp;
end;

end.
