unit eeDsp.Interpolation;

interface

function Linear(f, y0, y1:double):double; inline;

function Optimal2x2Point3rdOrder(f, y0, y1:single):single; inline;

function Optimal4x3(const f, y0, y1, y2, y3:single):single; inline;

function Optimal6x5(const f, yz2, yz1, y0, y1,y2,y3:single):single; inline;


type
  TAllPassInterpolator = class
  private
  protected
    OldL : double;
    OldR : double;
  public
    constructor Create;

    function Step(const f, y0, y1:double):double; overload;
    procedure Step(const f, y0L, y1L, y0R, y1R:double; out OutL, OutR: double); overload;
  end;

implementation

uses
  eeDsp.Interpolation.Tests;

  
function Linear(f, y0, y1:double):double; inline;
begin
  result := y0 + f * (y1 - y0);
end;


function Optimal2x2Point3rdOrder(f, y0, y1:single):single; inline;
// Optimal 2x (2-point, 3rd-order) (z-form)
// From:
// "Polynomial Interpolators for High-Quality Resampling of Oversampled Audio"
//  by Olli Niemitalo
var
  z     : double;
  even1 : double;
  odd1  : double;
  c0, c1, c2, c3 : double;
begin
  // Optimal 2x (2-point, 3rd-order) (z-form)

  // float z = x - 1/2.0;
  z :=  f - 0.5;

  // float even1 = y[1]+y[0], odd1 = y[1]-y[0];
  even1 := y1 + y0;
  odd1  := y1 - y0;

  // float c0 = even1*0.50037842517188658;
  c0 := even1 * 0.50037842517188658;

  // float c1 = odd1*1.00621089801788210;
  c1 := odd1 * 1.00621089801788210;

  // float c2 = even1*-0.004541102062639801;
  c2 := even1 * -0.004541102062639801;

  // float c3 = odd1*-1.57015627178718420;
  c3 := odd1 * -1.57015627178718420;

  // return ((c3*z+c2)*z+c1)*z+c0;
  result := ((c3 * z + c2) * z + c1) * z + c0;

end;


function Optimal4x3(const f, y0, y1, y2, y3:single):single; inline;
var
  z ,even1, even2, odd1, odd2 : single;
  c0,c1, c2, c3 : single;

  //y0 = y[-1]
  //y1 = y[0]
  //y2 = y[1]
  //y3 = y[2]
begin
  //// Optimal 2x (4-point, 3rd-order) (z-form)
  //float z = x - 1/2.0;
  z := f - 1/2.0;

  //float even1 = y[1]+y[0], odd1 = y[1]-y[0];
  even1 := y2+y1;
  odd1  := y2-y1;

  //float even2 = y[2]+y[-1], odd2 = y[2]-y[-1];
  even2 := y3+y0;
  odd2  := y3-y0;

  //float c0 = even1*0.45868970870461956 + even2*0.04131401926395584;
  c0 := even1*0.45868970870461956 + even2*0.04131401926395584;

  //float c1 = odd1*0.48068024766578432 + odd2*0.17577925564495955;
  c1 := odd1*0.48068024766578432 + odd2*0.17577925564495955;

  //float c2 = even1*-0.246185007019907091 + even2*0.24614027139700284;
  c2 := even1*-0.246185007019907091 + even2*0.24614027139700284;

  //float c3 = odd1*-0.36030925263849456 + odd2*0.10174985775982505;
  c3 := odd1*-0.36030925263849456 + odd2*0.10174985775982505;

  //return ((c3*z+c2)*z+c1)*z+c0;
  result := ((c3*z+c2)*z+c1)*z+c0;
end;


function Optimal6x5(const f, yz2, yz1, y0, y1,y2,y3:single):single; inline;
begin
  //z     := x - 1/2.0;
  //even1 := y[1]+y[0], odd1 = y[1]-y[0];
  //even2 := y[2]+y[-1], odd2 = y[2]-y[-1];
  //even3 := y[3]+y[-2], odd3 = y[3]-y[-2];
  //c0    := even1*0.40513396007145713 + even2*0.09251794438424393 + even3*0.00234806603570670;
  //c1    := odd1*0.28342806338906690 + odd2*0.21703277024054901 + odd3*0.01309294748731515;
  //c2    := even1*-0.191337682540351941 + even2*0.16187844487943592 + even3*0.02946017143111912;
  //c3    := odd1*-0.16471626190554542 + odd2*-0.00154547203542499 + odd3*0.03399271444851909;
  //c4    := even1*0.03845798729588149 + even2*-0.05712936104242644 + even3*0.01866750929921070;
  //c5    := odd1*0.04317950185225609 + odd2*-0.01802814255926417 + odd3*0.00152170021558204;
  //result := ((((c5*z+c4)*z+c3)*z+c2)*z+c1)*z+c0;
  result := 0;
end;



{ TAllPassInterpolator }

constructor TAllPassInterpolator.Create;
begin
  OldL := 0;
  OldR := 0;
end;

function TAllPassInterpolator.Step(const f, y0, y1: double): double;
begin
  OldL := (f * y1) - (f * OldL) + y0 ;
  result := OldL;
end;

procedure TAllPassInterpolator.Step(const f, y0L, y1L, y0R, y1R:double; out OutL, OutR: double);
begin
  OldL := (f * y1L) - (f * OldL) + y0L ;
  OutL := OldL;

  OldR := (f * y1R) - (f * OldR) + y0R ;
  OutR := OldR;
end;


end.
