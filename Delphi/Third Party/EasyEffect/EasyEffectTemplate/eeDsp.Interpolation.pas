unit eeDsp.Interpolation;

{.$INCLUDE Defines.inc}

interface

function Linear(f, y0, y1:double):double; inline;

function Optimal2x2Point3rdOrder(f, y0, y1:single):single; inline;

function Optimal4x3(const f, y0, y1, y2, y3:single):single; //inline;

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


function Optimal4x3(const f, y0, y1, y2, y3:single):single;
{$IF Defined(UseASM) and Defined(CPUX64)}
{$ELSEIF Defined(UseASM) and Defined(CPUX86)}
// f  = [ebp+$18]
// y0 = [ebp+$14]
// y1 = [ebp+$10]
// y2 = [ebp+$0C]
// y3 = [ebp+$08]
type
  float = double;
const
  PointFive : float = 0.5;
  k01 : float = 0.45868970870461956;
  k02 : float = 0.04131401926395584;
  k11 : float = 0.48068024766578432;
  k12 : float = 0.17577925564495955;
  k21 : float = -0.246185007019907091;
  k22 : float = 0.24614027139700284;
  k31 : float = -0.36030925263849456;
  k32 : float = 0.10174985775982505;
asm
  // make space for local variables.
  sub esp, 4

  // z := f - 1/2.0;
  fld dword [[ebp+$18]]
  fsub [[PointFive]]

  //even1 := y2+y1;
  fld dword ptr [ebp+$0C]
  fadd dword ptr [ebp+$10]

  //odd1  := y2-y1;
  fld dword ptr [ebp+$0C]
  fsub dword ptr [ebp+$10]

  //even2 := y3+y0;
  fld dword ptr [ebp+$08]
  fadd dword ptr [ebp+$14]

  //odd2  := y3-y0;
  fld dword ptr [ebp+$08]
  fsub dword ptr [ebp+$14]

  // The floating point stack is now:
  // st0 = odd2
  // st1 = even2
  // st2 = odd1
  // st3 = even1
  // st4 = z


  //c3 := odd1  * -0.36030925263849456  + odd2  * 0.10174985775982505;


  fld qword [k31]
  // The floating point stack is now:
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z
  fmul st(0), st(3)

  fld qword [k32]
  // The floating point stack is now:
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z
  fmul st(0), st(2)
  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = c3
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z


  fmul st(0), st(5)
  // The floating point stack is now:
  // st0 = c3 * z
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z

  //fst dword ptr [esp]


  //c2 := even1 * -0.246185007019907091 + even2 * 0.24614027139700284;

  fld qword [k21]
  // The floating point stack is now:
  // st1 = c3 * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z
  fmul st(0), st(5)


  fld qword [k22]
  // The floating point stack is now:
  // st2 = c3 * z
  // st3 = odd2
  // st4 = even2
  // st5 = odd1
  // st6 = even1
  // st7 = z
  fmul st(0), st(4)

  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = c2
  // st1 = c3 * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z


  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = (c3 * z + c2)
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z

  fmul st(0), st(5)
  // The floating point stack is now:
  // st0 = (c3 * z + c2) * z
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z


  //c1 := odd1  * 0.48068024766578432   + odd2  * 0.17577925564495955;

  fld qword [k11]
  // The floating point stack is now:
  // st1 = (c3 * z + c2) * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z
  fmul st(0), st(4)

  fld qword [k12]
  // The floating point stack is now:
  // st2 = (c3 * z + c2) * z
  // st3 = odd2
  // st4 = even2
  // st5 = odd1
  // st6 = even1
  // st7 = z
  fmul st(0), st(3)

  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = c1
  // st1 = (c3 * z + c2) * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z

  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = ((c3 * z + c2) * z + c1)
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z

  fmul st(0), st(5)
  // The floating point stack is now:
  // st0 = ((c3 * z + c2) * z + c1) * z
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z




  //c0 := even1 * 0.45868970870461956   + even2 * 0.04131401926395584;

  fld qword [k01]
  // The floating point stack is now:
  // st1 = ((c3 * z + c2) * z + c1) * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z
  fmul st(0), st(5)

  fld qword [k02]
  // The floating point stack is now:
  // st2 = ((c3 * z + c2) * z + c1) * z
  // st3 = odd2
  // st4 = even2
  // st5 = odd1
  // st6 = even1
  // st7 = z
  fmul st(0), st(4)

  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = c0
  // st1 = ((c3 * z + c2) * z + c1) * z
  // st2 = odd2
  // st3 = even2
  // st4 = odd1
  // st5 = even1
  // st6 = z


  faddp st(1), st(0)
  // The floating point stack is now:
  // st0 = ((c3 * z + c2) * z + c1) * z + c0
  // st1 = odd2
  // st2 = even2
  // st3 = odd1
  // st4 = even1
  // st5 = z


  fxch st(5)

  fstp dword ptr [esp]
  fstp dword ptr [esp]
  fstp dword ptr [esp]
  fstp dword ptr [esp]
  fstp dword ptr [esp]
  //fstp dword ptr [esp + $04]

  //fld dword ptr [esp]
  //fld dword ptr [esp + $08]

  //=========================================
  // The result is now in st0
  // result := ((c3*z+c2)*z+c1)*z+c0;
  //=========================================





  // free local variable space.
  add esp, 4

end;
{$ELSE}
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
{$IFEND}




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
