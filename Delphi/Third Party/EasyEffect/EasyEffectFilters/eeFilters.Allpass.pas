unit eeFilters.Allpass;

interface

uses
  VamLib.MoreTypes;

type

  // TSecondOrderAllPass implements the block-diagram found in
  // "Streamlining Digital Signal Processing" by richard G.Lyons.
  // Chapter 9.7, Figure 9-12, page 97.
  TSecondOrderAllPass = class
  private
    x1, x2, y1, y2:double;
    c1: double;
    c2: double;
  public
    constructor Create;
    function Step(x0:double):double; inline;
    function Step_asm(x0:double):double;
    function Step_asm3(x0:double):double;

    property Coefficient1 : double read c1 write c1;
    property Coefficient2 : double read c2 write c2;
  end;

implementation

{ TSecondOrderAllPass }

constructor TSecondOrderAllPass.Create;
begin
  x1 := 0;
  x2 := 0;
  y1 := 0;
  y2 := 0;
end;

function TSecondOrderAllPass.Step(x0: double): double;
var
  t1, t2, t3, t4, t5 : double;
begin
  t1 := x0 - y2;
  t2 := t1 * c2;
  t3 := x1 - y1;
  t4 := t3 * c1;
  t5 := x2 + t2 + t4;

  y2 := y1;
  y1 := t5;

  x2 := x1;
  x1 := x0;

  result := t5;
end;

function TSecondOrderAllPass.Step_asm(x0:double):double;
asm
  //t1 := x0 - y2;
  movq  xmm0, x0
  movq  xmm1, [eax].TSecondOrderAllPass.y2
  subpd xmm0, xmm1


  //t2 := t1 * c2;
  mulsd xmm0, [eax].TSecondOrderAllPass.c2


  //t3 := x1 - y1;
  movq  xmm2, [eax].TSecondOrderAllPass.x1
  movq  xmm3, [eax].TSecondOrderAllPass.y1
  subsd xmm2, xmm3

  //t4 := t3 * c1;
  mulsd xmm2, [eax].TSecondOrderAllPass.c2

  //t5 := x2 + t2 + t4;
  movq  xmm3, [eax].TSecondOrderAllPass.x2
  addpd xmm3, xmm0
  addpd xmm3, xmm2

  //y2 := y1;
  movq  xmm4, [eax].TSecondOrderAllPass.y1
  movq  [eax].TSecondOrderAllPass.y2, xmm4

  //y1 := t5;
  movq  [eax].TSecondOrderAllPass.y1, xmm3

  //x2 := x1;
  movq  xmm4, [eax].TSecondOrderAllPass.x1
  movq  [eax].TSecondOrderAllPass.x2, xmm4

  //x1 := x0;
  movq  xmm4, x0
  movq  [eax].TSecondOrderAllPass.x1, xmm4

  //result := t5;
  movq   result, xmm0
end;



function TSecondOrderAllPass.Step_asm3(x0: double): double;
var
  t1, t2, t3, t4, t5 : double;
asm
  //t1 := x0 - y2;
  movq xmm0, x0
  subsd  xmm0, [eax].TSecondOrderAllPass.y2

  //t2 := t1 * c2;
  mulsd xmm0, [eax].TSecondOrderAllPass.c2
  movq t2, xmm0

  //t3 := x1 - y1;
  movq xmm1, [eax].TSecondOrderAllPass.x1
  subsd  xmm1, [eax].TSecondOrderAllPass.y1
  //t4 := t3 * c1;
  mulsd xmm1, [eax].TSecondOrderAllPass.c2
  movq t4, xmm1

  //t5 := x2 + t2 + t4;
  movq xmm2, [eax].TSecondOrderAllPass.x2
  addsd xmm2, t2
  addsd xmm2, t4
  movq t5, xmm2

  //y2 := y1;
  movq xmm6, [eax].TSecondOrderAllPass.y1
  movq [eax].TSecondOrderAllPass.y2, xmm6

  //y2 := y1;
  movq xmm3, [eax].TSecondOrderAllPass.y1
  movq [eax].TSecondOrderAllPass.y2, xmm3

  //y1 := t5;
  movq xmm3, t5
  movq [eax].TSecondOrderAllPass.y1, xmm3

  //x2 := x1;
  movq xmm3, [eax].TSecondOrderAllPass.x1
  movq [eax].TSecondOrderAllPass.x2, xmm3

  //x1 := x0;
  movq xmm3, x0
  movq [eax].TSecondOrderAllPass.x1, xmm3

  //result := t5;
  movq result, xmm2
end;




end.

