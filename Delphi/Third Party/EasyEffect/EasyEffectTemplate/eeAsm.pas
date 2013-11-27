unit eeAsm;

interface

//Call after using MMX instructions to reset x87 FPU registers.
procedure emms;

function Add(x1, x2:double):double;

function Add_asm(x1, x2:double):double;



implementation

procedure emms; 
asm
  emms
end;

function Add(x1, x2:double):double;
begin
  result := x1 * x2;
end;

function Add_asm(x1, x2:double):double;
asm
  movq      xmm0, x2                          //   x2 = memory[x2]
  movq      xmm1, x1                          //   x1 = memory[x1]
  addpd     xmm0, xmm1                        //   x1 = x1 * x2
  movq      result, xmm0                      //   memory[result] = x1
end;


end.
