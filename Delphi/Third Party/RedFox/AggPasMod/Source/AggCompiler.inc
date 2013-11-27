//
// AggPas 2.4 RM3 compiler options configuration file
//


{ CPU }

{$IFDEF CPUPOWERPC}
  {$DEFINE AGG_CPU_PPC}
{$ENDIF}

{$IFDEF CPUI386}
  {$DEFINE AGG_CPU_386}
{$ENDIF}

{$IFDEF CPU386}
  {$DEFINE AGG_CPU_386}
{$ENDIF}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

{$IFDEF CPUX64}
  {$DEFINE CPU64}
{$ENDIF}

{$IFDEF CPU64}
  {$DEFINE AGG_CPU_64}
{$ENDIF}


{ OS }

{$IFDEF WIN32}
  {$DEFINE AGG_WINDOWS}
{$ENDIF}

{$IFDEF WIN64}
  {$DEFINE AGG_WINDOWS}
{$ENDIF}

{$IFDEF WINDOWS}
  {$DEFINE AGG_WINDOWS}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE AGG_LINUX}
{$ENDIF}

{$IFDEF DARWIN}
  {$DEFINE AGG_MACOSX}
{$ENDIF}


{ Compiler Mode }

{-$DEFINE DEBUG}
{$IFDEF DEBUG}
  {$DEFINE AGG_FULL_DEBUG}
{$ENDIF}


{ Configuration Switches }

{$IFDEF FPC}
  {$ASMMODE Intel}
  {$DEFINE SUPPORTS_INLINE}
{$ELSE}
  {$A-} { Fields alignment OFF }
  {$F+} { Far code generation }
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion >= 17.0}
      {$DEFINE SUPPORTS_INLINE}
    {$IFEND}
    {$IF CompilerVersion < 22.0}
      {$DEFINE SUPPORTS_INLINE}
    {$IFEND}

  {-$DEFINE USE_FASTMM4}

  {$ELSE}
    Delphi Version not supported!!!
  {$ENDIF}
{$ENDIF}

{$HINTS OFF}
{$WARNINGS OFF}

{$B-} { Complete boolean evaluation }
{$V-} { String type checking }
{$X+} { Extended syntax }

{$IFDEF AGG_FULL_DEBUG}
  {$R+} { Range checking }
  {$I+} { IO checking }
  {$Q+} { Overflow checking }

  {$IFNDEF FPC}
    {$O-} { Code Optimization }
  {$ENDIF}

  {$D+} { Debug Info ON }
  {$IFNDEF FPC}
    {$Y+} { References Info ON }
  {$ENDIF}

  {$IFDEF USE_FASTMM4}
    {$DEFINE FullDebugMode}
  {$ENDIF}
{$ELSE}
  {$R-} { Range checking }
  {$I-} { IO checking }
  {$Q-} { Overflow checking }

  { Code Optimization }
  {$IFNDEF FPC}
    {$IFDEF AGG_OPTIMIZE}
      {$O+}
    {$ELSE}
      {$O-}
    {$ENDIF}
  {$ENDIF}

  {$D-} { Debug Info OFF }
  {$IFNDEF FPC}
    {$Y-} { References Info OFF }
  {$ENDIF}
{$ENDIF}
