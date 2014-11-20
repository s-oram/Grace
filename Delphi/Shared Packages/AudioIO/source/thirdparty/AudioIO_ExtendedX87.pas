unit AudioIO_ExtendedX87;


(*  ** uTExtendedX87.pas *****************************************************
    ** (c) Copyright 2011 by Philipp M. Schlüter                            **
    ** Contact: <firstname.lastname*@systbot.uzh.ch> ; *= spell "ü" as "ue" **
    ** Licence terms: Artistic License 2.0                                  **
    ** A copy of the licence terms is available at:                         **
    ** http://www.opensource.org/licenses/artistic-license-2.0.php          **
    **************************************************************************  *)

  (* *** BACKGROUND ***

    Unfortunately, Delphi's 64-bit compiler (dcc64) and RTL do not support 80-bit floating point values (dcc32's Extended/C's
    long double) on Win64 but silently alias Extended = Double on Win64.
    There are situations, however, where this is clearly undesirable, e.g. if the additional precision gained from Extended
    is required.

    *** AIM ***

    The aim of this unit is to provide basic 80-bit extended capabilities for Delphi x64
    by providing
      - a dedicated FPU-backed TExtendedX87 type that behaves as an 80-bit extended value both on
        32-bit and 64-bit platforms
      - re-implementing basic routines from the System unit for this type on 32-bit and 64-bit

    Users should be able to either re-declare their variables explictly as TExtendedX87 or redeclare the type extended as
      Extended = TExtendedX87;

    *** CAUTION ***

      There are some pitfalls due to limitations of dcc64 (or differences among dcc32 and dcc64), especially:

      - Assignments will usually hav a loss of precision, e.g.
          ----------------------
          var
            x1, x2: TExtendedX87;
          const
            y = -666.666;
          begin
            x1:= 123.789;
            x2:= y;
          end;
          ----------------------
          The variables x1 and x2 will be slightly differnt on Win32 and Win64 because at the point of the assignment, it is
          a DOUBLE value gets assigned to x1/x2 via the Implicit Class Operator on Win64, whereas on Win32 a true 80-bit Extended
          value is assigned:

          Variable  Target_Value  Binary_Value_Win32*       Binary_Value_Win64*     (* as TExtended80Rec)
                                  .Frac              ._Exp  .Frac             ._Exp
          x1        +123.789      $F793F7CED916872B  $4005  $F793F7CED9168800 $4005
          x2        -666.666      $A6AA9FBE76C8B439  $C008  $A6AA9FBE76C8B800 $C008;

      - Therefore, any constants the binary exactness of which is essential, need to be declared differently!

        One way of doing this is by use of TExtended80Rec, or by using TExtendedX87 directly:
            Instead of
              y = -666.666;
            the following can be used:
              y: TExtended80Rec  = (Frac: $A6AA9FBE76C8B439; _Exp: $C008);
            However, the 32-bit and 64-bit compilers do not accept the exact same definitions, so a bit of trickery is necessary

        The following points may help:

          -- The binary representation of an 80-bit Extended value on Win32 can be found out in a simple experiment:
             -----------------------------------------------------------------------------------------------------------
             uses
              SysUtils;
             var
              e: extended;
             begin
              e:= -666.666;
              writeln ('Frac: ',TExtended80Rec(e).Frac,' _Exp: ',TExtended80Rec(e)._Exp);
              writeln ('Frac: $',IntToHex(TExtended80Rec(e).Frac,16),' _Exp: $',IntToHex(TExtended80Rec(e)._Exp,4));
             end.
             -----------------------------------------------------------------------------------------------------------

          -- Instead of defining extended constants you can define TExtended80Rec and cast them to achieve an assignment.
             This works on Win32 and Win64:
              const
                  y: TExtended80Rec  = (Frac: $A6AA9FBE76C8B439; _Exp: $C008);
              var
                  x1: TExtendedX87;
              begin
                x1:= TExtendedX87(y);

          -- Instead of a direct assignment, you could assign using a cast to TExtended80Rec (both Win32 and Win64):

              var
                  x1: TExtendedX87
              begin
                //Instead of: x1:= -666.666;
                TExtendedX87(x1).Frac:= $A6AA9FBE76C8B439;
                TExtendedX87(x1)._Exp:= $C008;
              end;

          -- You can use conditional directives to have separate declarations on Win32 and Win64, e.g.
             const
               y: TExtendedX87 =
              {$IFDEF WIN64}
               (AsExtRec80:(Frac: $A6AA9FBE76C8B439; _Exp: $C008));
              {$ELSE}
               -666.666;
              {$ENDIF}

          -- You can use the absolute keyword (on both Win32 and Win64);
              var
                y: TExtended80Rec  = (Frac: $A6AA9FBE76C8B439; _Exp: $C008);
                x3: TExtendedX87 absolute y;
             OR:
              const
                y: TExtended80Rec  = (Frac: $A6AA9FBE76C8B439; _Exp: $C008);
              var
                x3: TExtendedX87 absolute y;
      *** END ***                                   *)


  {$IF (defined(CPU386)=false) AND (defined(CPUX64)=false)}
    // This unit uses the FPU and therefore cannot run on non x86 CPU architectures
    {$MESSAGE FATAL 'This unit is only for x86/x86-64 CPUs'}
  {$IFEND}

  // **** Defines to be set by the user ****
  {$DEFINE SetWin32Default8087Cw}      //Enable this define if you wish the FPU's settings to be as on Win32 (Delphi's Win32 Default)
  {$DEFINE DontUseSystemUnitOn32bit}   //Enable this define if you with to replace System.pas standard rountines with this unit's on 32-bit
  {$DEFINE EnableHelperRoutines}       //Enable this define if you want to access any of the helper functions provided in this unit (on any platform)
  { $DEFINE EnableFWAITsEverywhere}    //Enable this define if you want FWAITs to be generated after all FPU code (similar to what the Win32 compiler does by default).
                                       //This implicitly implies all three of the following: EnableFWAIT_Arithmetic, EnableFWAIT_Division, EnableFWAIT_Transcendental
  { $DEFINE EnableFWAIT_Arithmetic}    //Enable this define if you want FWAITs to be generated after FPU addition, subtraction, and multiplication instructions
  {$DEFINE EnableFWAIT_Division}       //Enable this define if you want FWAITs to be generated after FPU divisions and square root operations
  {$DEFINE EnableFWAIT_Transcendental} //Enable this define if you want FWAITs to be generated after transcendental FPU instructions (logarithm and trigonometry)

  // **** Implicit defines NOT to be set by the user ****
  {$IFDEF EnableFWAITsEverywhere}
    {$DEFINE EnableFWAIT_Arithmetic}
    {$DEFINE EnableFWAIT_Division}
    {$DEFINE EnableFWAIT_Transcendental}
  {$ENDIF}


  //*************************************************************
  (* Changes and Revision History
     Number format = [TExtendedX87Version].[TExtendedX87Revision]
     Version 0.0001    (August 2011) Added documentation
     Version 0.0002    (Sept 2011) Updated documentation
     Version 0.0003    (Dec 2011) Added FWAITs and defines/IFDEFs to control FWAIT instructions
  *)





INTERFACE

const
  ///<summary> Default x87 FPU control word used by Delphi on Win32 </summary>
  Default8087CW_Win32: word = $1332;

  ///<summary> Major TExtendedX87 Version number </summary>
  TExtendedX87Version  = 0;
  ///<summary> TExtendedX87 Revision number within a major version </summary>
  TExtendedX87Revision = 3;

type
  {$IF COMPILERVERSION < 23}         //For all compilers before Delphi XE2
    //As of Delphi XE2, these types are defined in System.pas (and more fully than here)

    ///<summary> 10 bytes of data representing an 80-bit extended floating point value. </summary>
    ///<remarks> Access via .Words, or .Bytes, or .Frac/._Exp </remarks>
    TExtended80Rec = packed record case Integer of
      0: (Words: array [0..4] of UInt16);
      1: (Bytes: array [0..9] of UInt8);
      2: (Frac: UInt64; _Exp: UInt16;);
    end;
    ///<summary> Pointer to TExtended80Rec </summary>
    PExtended80Rec = ^TExtended80Rec;
  {$IFEND}

  {$IFDEF WIN64}
  ///<summary> 80-bit (10 byte) extended floating point value.</summary>
  ///<summary>Replacement type for Win32's Extended on x64, backed by the FPU</summary>
  ///<remarks> Access via .AsBytes or .AsExtRec80 </remarks>
  TExtendedX87 = packed record
    ///<summary>Assigns double value to TExtendedX87</summary>
    class operator Implicit (D: Double): TExtendedX87;
    ///<summary>Assigns TExtendedX87 value to double </summary>
    class operator Implicit (E: TExtendedX87): Double;
    ///<summary>Assigns single value to TExtendedX87</summary>
    class operator Implicit (S: Single): TExtendedX87;
    ///<summary>Assigns TExtendedX87 value to single </summary>
    class operator Implicit (E: TExtendedX87): Single;
    ///<summary>Addition of two TExtendedX87 values </summary>
    class operator Add (X, Y: TExtendedX87): TExtendedX87;
    ///<summary>Substraction of two TExtendedX87 values </summary>
    class operator Subtract (X, Y: TExtendedX87): TExtendedX87;
    ///<summary>Multiplication of two TExtendedX87 values </summary>
    class operator Multiply (X, Y: TExtendedX87): TExtendedX87;
    ///<summary>Division of two TExtendedX87 values </summary>
    class operator Divide (X, Y: TExtendedX87): TExtendedX87;
    ///<summary>Changes the sign of a TExtendedX87 value </summary>
    class operator Negative (E: TExtendedX87) : TExtendedX87;
    ///<summary> = Comparison of two TExtendedX87 values </summary>
    class operator Equal(X, Y: TExtendedX87): Boolean;
    ///<summary> Checks two TExtendedX87 values for inequality</summary>
    class operator NotEqual(X, Y: TExtendedX87): Boolean;
    ///<summary> > Comparison of two TExtendedX87 values </summary>
    class operator GreaterThan(X, Y: TExtendedX87): Boolean;
    ///<summary> >= Comparison of two TExtendedX87 values </summary>
    class operator GreaterThanOrEqual(X, Y: TExtendedX87): Boolean;
    ///<summary> Less-Than Comparison of two TExtendedX87 values </summary>
    class operator LessThan(X, Y: TExtendedX87): Boolean;
    ///<summary> Less-Than-Or-Equal Comparison of two TExtendedX87 values </summary>
    class operator LessThanOrEqual(X, Y: TExtendedX87): Boolean;
  case byte of
      0: (AsBytes: packed array [0..9] of byte);
      1: (AsExtRec80: System.TExtended80Rec);
  end;
 {$ELSE}
  ///<summary> 80-bit extended floating point value</summary>
  TExtendedX87 = extended;
 {$ENDIF}
  ///<summary> Pointer to TExtendedX87</summary>
  PExtendedX87 = ^TExtendedX87;

  {$IFDEF EnableHelperRoutines}
    ///<summary>Resets FPU to hardware default state</summary>
    ///<remarks>Executes FINIT instruction</remarks>
    procedure ResetFPU;

    ///<summary>Clears FPU exceptions</summary>
    ///<remarks>Executes FNCLEX instruction</remarks>
    procedure ClearFPUExceptions;

    ///<summary>Asks CPU to check for and handle pending unmasked FPU exceptions</summary>
    ///<remarks>Executes FWAIT instruction</remarks>
    procedure CheckForFPUExceptions;

    ///<summary>Exits FPU's MMX state, readying FPU for floating-point work</summary>
    ///<remarks>Executes EMMS instruction</remarks>
    procedure ExitMMX;

    ///<summary>Checks whether the ST(0) FPU register contains anything (i.e. is 'valid') </summary>
    ///<returns>TRUE if ST(0) is valid/contains a value; FALSE otherwise </returns>
    ///<remarks>If FALSE, the FPU stack is empty</remarks>
    function  IsST0Valid: boolean;

    ///<summary>Pops topmost value from FPU stack</summary>
    ///<remarks>Handle with care! Only exectue if ST(0) is valid (you can check e.g. by using IsST0Valid). Keeping track of the FPU stack is the user's responsibility. Incorrect handling can lead to exceptions.</remarks>
    procedure PopST0;

    ///<summary>Sets a TExtendedX87 from a memory location </summary>
    ///<param name="[var] SrcExt87">Variable which contains 10 bytes of data representing an 80-bit extended value</param>
    ///<param name="[out] DstExt87: TExtendedX87">80-bit extended value copied from SrcExt87</param>
    procedure ExtX87SetFromMemory (var SrcExt87; out DstExt87: TExtendedX87);

    ///<summary>Sets a TExtendedX87 from the value contained in the FPU's ST(0) register, removing the value from the FPU stack by popping it</summary>
    ///<param name="[out] DstExt87: TExtendedX87">80-bit extended value from ST(0)</param>
    ///<remarks>Handle with care! It is the user's responsibility to ensure that ST(0) actually contains a value (use e.g. IsST0Valid to check!). If the FPU stack is empty, this will result in an exception.</remarks>
    procedure ExtX87FetchST0AndPop (out DstExt: TExtendedX87);

    ///<summary>Copies a TExtendedX87 value to the FPU's ST(0) register </summary>
    ///<param name="[var] SrcExt87: TExtendedX87">Variable which contains 10 bytes of data representing an 80-bit extended value</param>
    ///<remarks>Handle with care! It is the user's responsibility to keep track of the FPU's stack. If the FPU stack overflows, this will result in an exception.</remarks>
    procedure ExtX87PushOntoST0 (var SrcExt87: TExtendedX87);

    ///<summary>Obtains the 80-bit constant pi from the FPU hardware</summary>
    ///<param name="[out] Ext87: TExtendedX87">Output variable containing the 80-bit floating point value</param>
    procedure GetExtX87Pi (out Ext87: TExtendedX87);

    ///<summary>Obtains the 80-bit constant 1.0 from the FPU hardware</summary>
    ///<param name="[out] Ext87: TExtendedX87">Output variable containing the 80-bit floating point value</param>
    procedure GetExtX87One (out Ext87: TExtendedX87);

    ///<summary>Obtains the 80-bit constant 0.0 from the FPU hardware</summary>
    ///<param name="[out] Ext87: TExtendedX87">Output variable containing the 80-bit floating point value</param>
    procedure GetExtX87Zero (out Ext87: TExtendedX87);

    ///<summary>Obtains the 80-bit constant ld(10)=log2(10) from the FPU hardware</summary>
    ///<param name="[out] Ext87: TExtendedX87">Output variable containing the 80-bit floating point value</param>
    procedure GetExtX87Ld10 (out Ext87: TExtendedX87);

    ///<summary>Obtains the 80-bit constant lg(e)=log2(e) from the FPU hardware</summary>
    ///<param name="[out] Ext87: TExtendedX87">Output variable containing the 80-bit floating point value</param>
    procedure GetExtX87LdE (out Ext87: TExtendedX87);

    ///<summary>Obtains the 80-bit constant lg(e)=log10(e) from the FPU hardware</summary>
    ///<param name="[out] Ext87">Output variable containing the 80-bit floating point value</param>
    procedure GetExtX87Lg2 (out Ext87: TExtendedX87);

    ///<summary>Obtains the 80-bit constant ln(2)=log_e(2) from the FPU hardware</summary>
    ///<param name="[out] Ext87: TExtendedX87">Output variable containing the 80-bit floating point value</param>
    procedure GetExtX87Ln2 (out Ext87: TExtendedX87);

    ///<summary>Converts 80-bit TExtendedX87 to double</summary>
    ///<param name="Ext87: TExtendedX87">Value to be converted</param>
    ///<returns>double value</returns>
    function Ext2Dbl (const Ext87: TExtendedX87): double;

    ///<summary>Converts 80-bit TExtendedX87 to single</summary>
    ///<param name="Ext87: TExtendedX87">Value to be converted</param>
    ///<returns>single value</returns>
    function Ext2Sgl (const Ext87: TExtendedX87): single;

    ///<summary>Converts double to 80-bit TExtendedX87</summary>
    ///<param name="Dbl: Double">Value to be converted</param>
    ///<returns> TExtendedX87 value</returns>
    function Dbl2Ext (const Dbl: double): TExtendedX87;

    ///<summary>Converts single to 80-bit TExtendedX87</summary>
    ///<param name="Sgl: Single">Value to be converted</param>
    ///<returns> TExtendedX87 value</returns>
    function Sgl2Ext (const Sgl: single): TExtendedX87;
  {$ENDIF}

   {$IF defined(CPUX64) or defined(DontUseSystemUnitOn32bit)}
    ///<summary>Returns the absolute value </summary>
    ///<param name="Val: TExtendedX87">Input value</param>
    ///<returns> TExtendedX87 value</returns>
    function abs (const Val: TExtendedX87): TExtendedX87;

    ///<summary>Rounds input value to Int64 </summary>
    ///<param name="Val: TExtendedX87">Input value</param>
    ///<returns> Int64 value</returns>
    function round (const Val: TExtendedX87): Int64;

    ///<summary>Truncates input value to Int64 </summary>
    ///<param name="Val: TExtendedX87">Input value</param>
    ///<returns> Int64 value</returns>
    function trunc (const Val: TExtendedX87): Int64;

    ///<summary>Returns the integer part of an TExtendedX87 80-bit floating point value </summary>
    ///<param name="X: TExtendedX87">Input value</param>
    ///<returns> TExtendedX87 value</returns>
    function int (const X: TExtendedX87): TExtendedX87;

    ///<summary>Returns  the fractional part of an TExtendedX87 80-bit floating point value </summary>
    ///<param name="X: TExtendedX87">Input value</param>
    ///<returns> TExtendedX87 value</returns>
    function frac (const X: TExtendedX87): TExtendedX87;

    ///<summary>Returns e^X (i.e., the base of natural logarithms, e, raised to the power X </summary>
    ///<param name="X: TExtendedX87">Power to which e shall be raised</param>
    ///<returns> TExtendedX87 value</returns>
    function exp (const X: TExtendedX87): TExtendedX87;

    ///<summary>Returns the natural logarithm of X </summary>
    ///<param name="X: TExtendedX87">Input value</param>
    ///<returns> TExtendedX87 value</returns>
    function ln (const X: TExtendedX87): TExtendedX87;

    ///<summary>Returns the square root of X </summary>
    ///<param name="X: TExtendedX87">Input value</param>
    ///<returns> TExtendedX87 value</returns>
    function sqrt (const X: TExtendedX87): TExtendedX87;

    ///<summary>Returns the cosine of X </summary>
    ///<param name="X: TExtendedX87">Input number, given in radians</param>
    ///<returns> TExtendedX87 value</returns>
    function cos (const X: TExtendedX87): TExtendedX87;

    ///<summary>Returns the sine of X </summary>
    ///<param name="X: TExtendedX87">Input number, given in radians</param>
    ///<returns> TExtendedX87 value</returns>
    function sin (const X: TExtendedX87): TExtendedX87;

    ///<summary>Returns the arc tangent of X </summary>
    ///<param name="X: TExtendedX87">Input number</param>
    ///<returns> TExtendedX87 value, in radians</returns>
    ///<remarks>Special floating point values INF, -INF yield +PI/2 and -PI/2 respectively.</remarks>
    function arctan (const X: TExtendedX87): TExtendedX87;
  {$IFEND}



IMPLEMENTATION

{ ***** TExtendedX87 *****}

{$IFDEF WIN64}
  class operator TExtendedX87.Implicit (D: double): TExtendedX87;
  //Win64: @SELF in RCX, D in XMM1
  asm
    movsd [rsp+08h], D                 ;// xmm1
    fld qword ptr [rsp+08h]            ;// D
    fstp tbyte ptr @Result.AsBytes     ;// [rcx]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.Implicit (E: TExtendedX87): Double;
  asm
    fld tbyte ptr E.AsBytes            ;// [rcx]
    fstp qword ptr [rsp+08h]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
    movsd xmm0, [rsp+08h]              ;// Result:= xmm0
  end;

  class operator TExtendedX87.Implicit (S: Single): TExtendedX87;
  asm
    movss [rsp+08h], S                 ;// xmm1
    fld dword ptr [rsp+08h]            ;// S
    fstp tbyte ptr @Result.AsBytes     ;// [rcx]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.Implicit (E: TExtendedX87): Single;
  asm
    fld tbyte ptr E.AsBytes            ;// [rcx]
    fstp dword ptr [rsp+08h]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
    movss xmm0, [rsp+08h]              ;// Result:= xmm0
  end;

  class operator TExtendedX87.Add (X, Y: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr X.AsBytes            ;// [rdx]
    fld tbyte ptr Y.AsBytes            ;// [r8]
    faddp st(1), st(0)
    fstp tbyte ptr @Result.AsBytes     ;// [rcx]
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.Subtract (X, Y: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr X.AsBytes            ;// [rdx]
    fld tbyte ptr Y.AsBytes            ;// [r8]
    fsubp st(1), st(0)
    fstp tbyte ptr @Result.AsBytes     ;// [rcx]
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.Multiply (X, Y: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr X.AsBytes            ;// [rdx]
    fld tbyte ptr Y.AsBytes            ;// [r8]
    fmulp st(1), st(0)
    fstp tbyte ptr @Result.AsBytes     ;// [rcx]
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.Divide (X, Y: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr X.AsBytes            ;// [rdx]
    fld tbyte ptr Y.AsBytes            ;// [r8]
    fdivp st(1), st(0)
    fstp tbyte ptr @Result.AsBytes     ;// [rcx]
    {$IFDEF EnableFWAIT_Division}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.Negative (E: TExtendedX87) : TExtendedX87;
  asm
    fld tbyte ptr E.AsBytes            ;// [rcx]
    fchs
    fstp tbyte ptr @Result.AsBytes     ;// [rcx]
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.Equal (X, Y: TExtendedX87): Boolean;
  asm
    fld tbyte ptr Y.AsBytes
    fld tbyte ptr X.AsBytes
    fcomip st(0), st(1)
    setz al
    fstp st(0)
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.NotEqual (X, Y: TExtendedX87): Boolean;
  asm
    fld tbyte ptr Y.AsBytes
    fld tbyte ptr X.AsBytes
    fcomip st(0), st(1)
    setnz al
    fstp st(0)
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.GreaterThan (X, Y: TExtendedX87): Boolean;
  asm
    fld tbyte ptr Y.AsBytes
    fld tbyte ptr X.AsBytes
    fcomip st(0), st(1)
    seta al
    fstp st(0)
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.GreaterThanOrEqual (X, Y: TExtendedX87): Boolean;
  asm
    fld tbyte ptr Y.AsBytes
    fld tbyte ptr X.AsBytes
    fcomip st(0), st(1)
    setnc al
    fstp st(0)
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.LessThan (X, Y: TExtendedX87): Boolean;
  asm
    fld tbyte ptr Y.AsBytes
    fld tbyte ptr X.AsBytes
    fcomip st(0), st(1)
    setc al
    fstp st(0)
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;

  class operator TExtendedX87.LessThanOrEqual (X, Y: TExtendedX87): Boolean;
  asm
    fld tbyte ptr Y.AsBytes
    fld tbyte ptr X.AsBytes
    fcomip st(0), st(1)
    setbe al
    fstp st(0)
    {$IFDEF EnableFWAIT_Arithmetic}
     fwait
    {$ENDIF}
  end;
{$ENDIF}



{ ***** Helper routines *****}

{$IFDEF EnableHelperRoutines}
  procedure ResetFPU;
  asm
    finit
  end;

  procedure ClearFPUExceptions;
  asm
    fnclex
  end;

  procedure CheckForFPUExceptions;
  asm
    fwait
  end;

  procedure ExitMMX;
  asm
    emms
  end;

  function IsST0Valid: boolean;
  asm
     fxam                           ;// Examine ST(0) state
     fstsw ax                       ;// Get FPU Status Word in AX
     and ah, 01000101b              ;// Clear all uninteresting bits
     cmp ah, 01000001b              ;// Compare with "Empty" pattern
     setne al                       ;// Set Result to True (1) if ST(0)<>empty
  end;

  procedure PopST0;
  asm
    fstp st(0)
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure ExtX87SetFromMemory (var SrcExt87; out DstExt87: TExtendedX87);
  asm
    fld tbyte ptr [SrcExt87]
    fstp tbyte ptr [DstExt87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure ExtX87FetchST0AndPop (out DstExt: TExtendedX87);
  asm
    fstp tbyte ptr [DstExt]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure ExtX87PushOntoST0 (var SrcExt87: TExtendedX87);
  asm
    fld tbyte ptr [SrcExt87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure GetExtX87Pi (out Ext87: TExtendedX87);
  asm
    fldpi
    fstp tbyte ptr [Ext87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure GetExtX87One (out Ext87: TExtendedX87);
  asm
    fld1
    fstp tbyte ptr [Ext87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure GetExtX87Zero (out Ext87: TExtendedX87);
  asm
    fldz
    fstp tbyte ptr [Ext87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure GetExtX87Ld10 (out Ext87: TExtendedX87);
  asm
    fldl2t
    fstp tbyte ptr [Ext87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure GetExtX87LdE (out Ext87: TExtendedX87);
  asm
    fldl2e
    fstp tbyte ptr [Ext87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure GetExtX87Lg2 (out Ext87: TExtendedX87);
  asm
    fldlg2
    fstp tbyte ptr [Ext87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  procedure GetExtX87Ln2 (out Ext87: TExtendedX87);
  asm
    fldln2
    fstp tbyte ptr [Ext87]
    {$IFDEF EnableFWAITsEverywhere}
     fwait
    {$ENDIF}
  end;

  function Ext2Dbl (Const Ext87: TExtendedX87): double;
  asm
    fld tbyte ptr [Ext87]
   {$IFDEF Win64}
    fstp qword ptr [rsp+08h]
    movsd xmm0, [rsp+08h]              ;// Result:= xmm0
   {$ELSE}
    //Ext87 is in ST(0) and does not need to be converted to double
   {$ENDIF}
   {$IFDEF EnableFWAITsEverywhere}
    fwait
   {$ENDIF}
  end;

  function Ext2Sgl (const Ext87: TExtendedX87): single;
  asm
    fld tbyte ptr [Ext87]
   {$IFDEF Win64}
    fstp dword ptr [rsp+08h]
    movss xmm0, [rsp+08h]              ;// Result:= xmm0
   {$ELSE}
    //Ext87 is in ST(0) and does not need to be converted to single
   {$ENDIF}
   {$IFDEF EnableFWAITsEverywhere}
    fwait
   {$ENDIF}
  end;

  function Sgl2Ext (const Sgl: single): TExtendedX87;
  asm
   {$IFDEF Win64}
    movss [rsp+08h], xmm1
    fld dword ptr [rsp+08h]
    fstp tbyte ptr @Result.AsBytes
   {$ELSE}
    fld dword ptr [Sgl]
    //Nothing else to do - Can be accessed from ST(0) as Extended
   {$ENDIF}
   {$IFDEF EnableFWAITsEverywhere}
    fwait
   {$ENDIF}
  end;

  function Dbl2Ext (const Dbl: double): TExtendedX87;
  asm
   {$IFDEF Win64}
    movsd [rsp+08h], xmm1
    fld qword ptr [rsp+08h]
    fstp tbyte ptr @Result.AsBytes
   {$ELSE}
    fld qword ptr [Dbl]
    //Nothing else to do - Can be accessed from ST(0) as Extended
   {$ENDIF}
   {$IFDEF EnableFWAITsEverywhere}
    fwait
   {$ENDIF}
  end;
{$ENDIF}



{ ***** Replacement rountines for standard System.pas rountines for Win32/Win64 *****}

{$IF defined(CPUX64) or defined(DontUseSystemUnitOn32bit)}
  function abs (const Val: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr [Val]
    fabs
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Arithmetic}
    fwait
   {$ENDIF}
  end;

  function round (const Val: TExtendedX87): Int64;
  var
    TmpVal: Int64;
  asm
   {$IFDEF Win64} .NOFRAME {$ENDIF}
    fld tbyte ptr [Val]
    fistp qword ptr [TmpVal]
   {$IFDEF Win64}
    mov rax, TmpVal
   {$ELSE}
    mov eax, dword ptr [TmpVal]
    mov edx, dword ptr [TmpVal + 4]
   {$ENDIF}
   {$IFDEF EnableFWAIT_Arithmetic}
    fwait
   {$ENDIF}
  end;

  function trunc (const Val: TExtendedX87): Int64;
  var
    TmpVal: Int64;
    SaveCW, ScratchCW: word;
  asm
   {$IFDEF Win64} .NOFRAME {$ENDIF}
     fld tbyte ptr [Val]
     fnstcw word ptr [SaveCW]
     fnstcw word ptr [ScratchCW]
     or word ptr [ScratchCW], 0F00h  ;// trunc toward zero, full precision
     fldcw word ptr [ScratchCW]
     fistp qword ptr [TmpVal]
     fldcw word ptr [SaveCW]
   {$IFDEF Win64}
     mov rax, TmpVal
   {$ELSE}
     mov eax, dword ptr [TmpVal]
     mov edx, dword ptr [TmpVal + 4]
   {$ENDIF}
   {$IFDEF EnableFWAIT_Arithmetic}
    fwait
   {$ENDIF}
  end;

  function int (const X: TExtendedX87): TExtendedX87;
  var
    SaveCW, ScratchCW: word;
  asm
   {$IFDEF Win64} .NOFRAME {$ENDIF}
    fld tbyte ptr [X]
    fnstcw word ptr [SaveCW]
    fnstcw word ptr [ScratchCW]
    or word ptr [ScratchCW], 0F00h  // trunc toward zero, full precision
    fldcw word ptr [ScratchCW]
    frndint
    fldcw word ptr [SaveCW]
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Arithmetic}
    fwait
   {$ENDIF}
  end;

  function frac(const X: TExtendedX87): TExtendedX87;
  var
    SaveCW, ScratchCW: word;
  asm
   {$IFDEF Win64} .NOFRAME {$ENDIF}
    fld tbyte ptr [X]
    fld st(0)
    fnstcw word ptr [SaveCW]
    fnstcw word ptr [ScratchCW]
    or word ptr [ScratchCW], 0F00h // trunc toward zero, full precision
    fldcw word ptr [ScratchCW]
    frndint
    fldcw word ptr [SaveCW]
    fsub
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Arithmetic}
    fwait
   {$ENDIF}
  end;

  function exp (const X: TExtendedX87): TExtendedX87;
  asm
    // This follows the Win32 implementation in the System unit, so as to achieve equivalence of output
    { e^x = 2^(x*log2(e)) }
    fld tbyte ptr [X]
    fldl2e
    fmul
    fld st(0)
    frndint
    fsub st(1), st(0)
    fxch st(1)
    f2xm1
    fld1
    fadd
    fscale
    fstp st(1)
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Transcendental}
    fwait
   {$ENDIF}
  end;

  function cos (const X: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr [X]
    fcos
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Transcendental}
    fwait
   {$ENDIF}
  end;

  function sin (const X: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr [X]
    fsin
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Transcendental}
    fwait
   {$ENDIF}
  end;

  function ln (const X: TExtendedX87): TExtendedX87;
  asm
    fldln2
    fld tbyte ptr [X]
    fyl2x
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Transcendental}
    fwait
   {$ENDIF}
  end;

  function arctan (const X: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr [x]
    fld1
    fpatan
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Transcendental}
    fwait
   {$ENDIF}
  end;

  function sqrt (const X: TExtendedX87): TExtendedX87;
  asm
    fld tbyte ptr [x]
    fsqrt
   {$IFDEF Win64}
    fstp tbyte ptr @Result.AsBytes
   {$ENDIF}
   {$IFDEF EnableFWAIT_Division}
    fwait
   {$ENDIF}
  end;
{$IFEND}

INITIALIZATION

  {$IFDEF WIN64}
    {$IFDEF SetWin32Default8087Cw}
      Set8087CW (Default8087CW_Win32);
    {$ENDIF}
  {$ENDIF}

FINALIZATION

end.
