{$DEFINE CONVERT_TO_DYNARRAY}
{$DEFINE UseNativeTypes}
{$DEFINE UseDelphi}
{-$DEFINE Debug}

{$DEFINE BORLAND}

{$IFDEF FPC}
 {$MODE DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
 {$UNDEF BORLAND}
{$ELSE}
 {$DEFINE x87}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

{$IFDEF BORLAND}
  {$DEFINE WIN32}
  {$IFDEF LINUX}
    {$DEFINE KYLIX}
  {$ENDIF LINUX}
{$ENDIF BORLAND}

{$IFDEF BORLAND}
    {$DEFINE UNKNOWN_COMPILER_VERSION}
    {$IFDEF VER80}
      {$DEFINE COMPILER1}
      {$DEFINE DELPHI1}
      {$DEFINE DELPHICOMPILER1}
      {$DEFINE RTL80_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER90}
      {$DEFINE COMPILER2}
      {$DEFINE DELPHI2}
      {$DEFINE DELPHICOMPILER2}
      {$DEFINE RTL90_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER93}
      {$DEFINE COMPILER2}
      {$DEFINE BCB1}
      {$DEFINE BCB}
      {$DEFINE RTL93_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER100}
      {$DEFINE COMPILER3}
      {$DEFINE DELPHI3}
      {$DEFINE DELPHICOMPILER3}
      {$DEFINE RTL100_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER110}
      {$DEFINE COMPILER35}
      {$DEFINE BCB3}
      {$DEFINE RTL110_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER120}
      {$DEFINE COMPILER4}
      {$DEFINE DELPHI4}
      {$DEFINE DELPHICOMPILER4}
      {$DEFINE RTL120_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER125}
      {$DEFINE COMPILER4}
      {$DEFINE BCB4}
      {$DEFINE BCB}
      {$DEFINE RTL125_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER130}
      {$DEFINE COMPILER5}
      {$IFDEF BCB}
        {$DEFINE BCB5}
      {$ELSE}
        {$DEFINE DELPHI5}
        {$DEFINE DELPHICOMPILER5}
      {$ENDIF}
      {$DEFINE RTL130_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER140}
      {$DEFINE COMPILER6}
      {$IFDEF BCB}
        {$DEFINE BCB6}
      {$ELSE}
        {$DEFINE DELPHI6}
        {$DEFINE DELPHICOMPILER6}
      {$ENDIF}
      {$DEFINE RTL140_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER150}
      {$DEFINE COMPILER7}
      {$DEFINE DELPHI7}
      {$DEFINE DELPHICOMPILER7}
      {$DEFINE RTL150_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER160}
      {$DEFINE BDS2}
      {$DEFINE BDS}
      {$DEFINE COMPILER8}
      {$DEFINE DELPHI8}
      {$DEFINE DELPHICOMPILER8}
      {$DEFINE RTL160_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER170}
      {$DEFINE BDS3}
      {$DEFINE BDS}
      {$DEFINE COMPILER9}
      {$DEFINE DELPHI9}
      {$DEFINE DELPHI2005} // synonym to DELPHI9
      {$DEFINE DELPHICOMPILER9}
      {$DEFINE RTL170_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF VER180}
      {$DEFINE BDS}
      {$IFDEF VER185}
        {$DEFINE BDS5}
        {$DEFINE COMPILER11}
        {$IFDEF BCB}
          {$DEFINE BCB11}
        {$ELSE}
          {$DEFINE DELPHI11}
          {$DEFINE DELPHICOMPILER11}
        {$ENDIF}
        {$DEFINE RTL185_UP}
      {$ELSE ~~VER185}
        {$DEFINE BDS4}
        {$DEFINE COMPILER10}
        {$IFDEF BCB}
          {$DEFINE BCB10}
        {$ELSE}
          {$DEFINE DELPHI10}
          {$DEFINE DELPHICOMPILER10}
        {$ENDIF}
        {$DEFINE RTL180_UP}
      {$ENDIF ~VER185}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}

    {$IFDEF UNKNOWN_COMPILER_VERSION} // adjust for newer version
      {$DEFINE BDS5}
      {$DEFINE BDS}
      {$DEFINE COMPILER11}
      {$DEFINE DELPHI11}
      {$DEFINE DELPHICOMPILER11}
      {$DEFINE RTL185_UP}
      {$UNDEF UNKNOWN_COMPILER_VERSION}
    {$ENDIF}
    {$DEFINE CPU386}
{$ENDIF BORLAND}

{------------------------------------------------------------------------------}
{ DELPHIX_UP from DELPHIX mappings                                             }
{------------------------------------------------------------------------------}

{$IFDEF DELPHI11} {$DEFINE DELPHI11_UP} {$ENDIF}
{$IFDEF DELPHI10} {$DEFINE DELPHI10_UP} {$ENDIF}
{$IFDEF DELPHI9}  {$DEFINE DELPHI9_UP}  {$ENDIF}
{$IFDEF DELPHI8}  {$DEFINE DELPHI8_UP}  {$ENDIF}
{$IFDEF DELPHI7}  {$DEFINE DELPHI7_UP}  {$ENDIF}
{$IFDEF DELPHI6}  {$DEFINE DELPHI6_UP}  {$ENDIF}
{$IFDEF DELPHI5}  {$DEFINE DELPHI5_UP}  {$ENDIF}
{$IFDEF DELPHI4}  {$DEFINE DELPHI4_UP}  {$ENDIF}
{$IFDEF DELPHI3}  {$DEFINE DELPHI3_UP}  {$ENDIF}
{$IFDEF DELPHI2}  {$DEFINE DELPHI2_UP}  {$ENDIF}
{$IFDEF DELPHI1}  {$DEFINE DELPHI1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ DELPHIX_UP from DELPHIX_UP mappings                                          }
{------------------------------------------------------------------------------}

{$IFDEF DELPHI11_UP} {$DEFINE DELPHI10_UP} {$ENDIF}
{$IFDEF DELPHI10_UP} {$DEFINE DELPHI9_UP}  {$ENDIF}

{$IFDEF DELPHI9_UP}
  {$DEFINE DELPHI2005_UP} // synonym to DELPHI9_UP
  {$DEFINE DELPHI8_UP}
{$ENDIF}

{$IFDEF DELPHI8_UP} {$DEFINE DELPHI7_UP} {$ENDIF}
{$IFDEF DELPHI7_UP} {$DEFINE DELPHI6_UP} {$ENDIF}
{$IFDEF DELPHI6_UP} {$DEFINE DELPHI5_UP} {$ENDIF}
{$IFDEF DELPHI5_UP} {$DEFINE DELPHI4_UP} {$ENDIF}
{$IFDEF DELPHI4_UP} {$DEFINE DELPHI3_UP} {$ENDIF}
{$IFDEF DELPHI3_UP} {$DEFINE DELPHI2_UP} {$ENDIF}
{$IFDEF DELPHI2_UP} {$DEFINE DELPHI1_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ BCBX_UP from BCBX mappings                                                   }
{------------------------------------------------------------------------------}

{$IFDEF BCB11} {$DEFINE BCB11_UP} {$ENDIF}
{$IFDEF BCB10} {$DEFINE BCB10_UP} {$ENDIF}
{$IFDEF BCB6}  {$DEFINE BCB6_UP}  {$ENDIF}
{$IFDEF BCB5}  {$DEFINE BCB5_UP}  {$ENDIF}
{$IFDEF BCB4}  {$DEFINE BCB4_UP}  {$ENDIF}
{$IFDEF BCB3}  {$DEFINE BCB3_UP}  {$ENDIF}
{$IFDEF BCB1}  {$DEFINE BCB1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ BCBX_UP from BCBX_UP mappings                                                }
{------------------------------------------------------------------------------}

{$IFDEF BCB11_UP} {$DEFINE BCB10_UP} {$ENDIF}
{$IFDEF BCB10_UP} {$DEFINE BCB6_UP}  {$ENDIF}
{$IFDEF BCB6_UP}  {$DEFINE BCB5_UP}  {$ENDIF}
{$IFDEF BCB5_UP}  {$DEFINE BCB4_UP}  {$ENDIF}
{$IFDEF BCB4_UP}  {$DEFINE BCB3_UP}  {$ENDIF}
{$IFDEF BCB3_UP}  {$DEFINE BCB1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ BDSX_UP from BDSX mappings                                                   }
{------------------------------------------------------------------------------}

{$IFDEF BDS5} {$DEFINE BDS5_UP} {$ENDIF}
{$IFDEF BDS4} {$DEFINE BDS4_UP} {$ENDIF}
{$IFDEF BDS3} {$DEFINE BDS3_UP} {$ENDIF}
{$IFDEF BDS2} {$DEFINE BDS2_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ BDSX_UP from BDSX_UP mappings                                                }
{------------------------------------------------------------------------------}

{$IFDEF BDS5_UP} {$DEFINE BDS4_UP} {$ENDIF}
{$IFDEF BDS4_UP} {$DEFINE BDS3_UP} {$ENDIF}
{$IFDEF BDS3_UP} {$DEFINE BDS2_UP} {$ENDIF}

{------------------------------------------------------------------------------}
{ DELPHICOMPILERX_UP from DELPHICOMPILERX mappings                             }
{------------------------------------------------------------------------------}

{$IFDEF DELPHICOMPILER11} {$DEFINE DELPHICOMPILER11_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER10} {$DEFINE DELPHICOMPILER10_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER9}  {$DEFINE DELPHICOMPILER9_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER8}  {$DEFINE DELPHICOMPILER8_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER7}  {$DEFINE DELPHICOMPILER7_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER6}  {$DEFINE DELPHICOMPILER6_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER5}  {$DEFINE DELPHICOMPILER5_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER4}  {$DEFINE DELPHICOMPILER4_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER3}  {$DEFINE DELPHICOMPILER3_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER2}  {$DEFINE DELPHICOMPILER2_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER1}  {$DEFINE DELPHICOMPILER1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ DELPHICOMPILERX_UP from DELPHICOMPILERX_UP mappings                          }
{------------------------------------------------------------------------------}

{$IFDEF DELPHICOMPILER11_UP} {$DEFINE DELPHICOMPILER10_UP} {$ENDIF}
{$IFDEF DELPHICOMPILER10_UP} {$DEFINE DELPHICOMPILER9_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER9_UP}  {$DEFINE DELPHICOMPILER8_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER8_UP}  {$DEFINE DELPHICOMPILER7_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER8_UP}  {$DEFINE DELPHICOMPILER7_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER7_UP}  {$DEFINE DELPHICOMPILER6_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER6_UP}  {$DEFINE DELPHICOMPILER5_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER5_UP}  {$DEFINE DELPHICOMPILER4_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER4_UP}  {$DEFINE DELPHICOMPILER3_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER3_UP}  {$DEFINE DELPHICOMPILER2_UP}  {$ENDIF}
{$IFDEF DELPHICOMPILER2_UP}  {$DEFINE DELPHICOMPILER1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ COMPILERX_UP from COMPILERX mappings                                         }
{------------------------------------------------------------------------------}

{$IFDEF COMPILER11} {$DEFINE COMPILER11_UP} {$ENDIF}
{$IFDEF COMPILER10} {$DEFINE COMPILER10_UP} {$ENDIF}
{$IFDEF COMPILER9}  {$DEFINE COMPILER9_UP}  {$ENDIF}
{$IFDEF COMPILER8}  {$DEFINE COMPILER8_UP}  {$ENDIF}
{$IFDEF COMPILER7}  {$DEFINE COMPILER7_UP}  {$ENDIF}
{$IFDEF COMPILER6}  {$DEFINE COMPILER6_UP}  {$ENDIF}
{$IFDEF COMPILER5}  {$DEFINE COMPILER5_UP}  {$ENDIF}
{$IFDEF COMPILER4}  {$DEFINE COMPILER4_UP}  {$ENDIF}
{$IFDEF COMPILER35} {$DEFINE COMPILER35_UP} {$ENDIF}
{$IFDEF COMPILER3}  {$DEFINE COMPILER3_UP}  {$ENDIF}
{$IFDEF COMPILER2}  {$DEFINE COMPILER2_UP}  {$ENDIF}
{$IFDEF COMPILER1}  {$DEFINE COMPILER1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ COMPILERX_UP from COMPILERX_UP mappings                                      }
{------------------------------------------------------------------------------}

{$IFDEF COMPILER11_UP} {$DEFINE COMPILER10_UP} {$ENDIF}
{$IFDEF COMPILER10_UP} {$DEFINE COMPILER9_UP}  {$ENDIF}
{$IFDEF COMPILER9_UP}  {$DEFINE COMPILER8_UP}  {$ENDIF}
{$IFDEF COMPILER8_UP}  {$DEFINE COMPILER7_UP}  {$ENDIF}
{$IFDEF COMPILER7_UP}  {$DEFINE COMPILER6_UP}  {$ENDIF}
{$IFDEF COMPILER6_UP}  {$DEFINE COMPILER5_UP}  {$ENDIF}
{$IFDEF COMPILER5_UP}  {$DEFINE COMPILER4_UP}  {$ENDIF}
{$IFDEF COMPILER4_UP}  {$DEFINE COMPILER35_UP} {$ENDIF}
{$IFDEF COMPILER35_UP} {$DEFINE COMPILER3_UP}  {$ENDIF}
{$IFDEF COMPILER3_UP}  {$DEFINE COMPILER2_UP}  {$ENDIF}
{$IFDEF COMPILER2_UP}  {$DEFINE COMPILER1_UP}  {$ENDIF}

{------------------------------------------------------------------------------}
{ RTLX_UP from RTLX_UP mappings                                                }
{------------------------------------------------------------------------------}

{$IFDEF RTL185_UP} {$DEFINE RTL180_UP} {$ENDIF}
{$IFDEF RTL180_UP} {$DEFINE RTL170_UP} {$ENDIF}
{$IFDEF RTL170_UP} {$DEFINE RTL160_UP} {$ENDIF}
{$IFDEF RTL160_UP} {$DEFINE RTL150_UP} {$ENDIF}
{$IFDEF RTL150_UP} {$DEFINE RTL145_UP} {$ENDIF}
{$IFDEF RTL145_UP} {$DEFINE RTL142_UP} {$ENDIF}
{$IFDEF RTL142_UP} {$DEFINE RTL140_UP} {$ENDIF}
{$IFDEF RTL140_UP} {$DEFINE RTL130_UP} {$ENDIF}
{$IFDEF RTL130_UP} {$DEFINE RTL125_UP} {$ENDIF}
{$IFDEF RTL125_UP} {$DEFINE RTL120_UP} {$ENDIF}
{$IFDEF RTL120_UP} {$DEFINE RTL110_UP} {$ENDIF}
{$IFDEF RTL110_UP} {$DEFINE RTL100_UP} {$ENDIF}
{$IFDEF RTL100_UP} {$DEFINE RTL93_UP}  {$ENDIF}
{$IFDEF RTL93_UP}  {$DEFINE RTL90_UP}  {$ENDIF}
{$IFDEF RTL90_UP}  {$DEFINE RTL80_UP}  {$ENDIF}

{------------------------------------------------------------------------------}

{$IFDEF DELPHICOMPILER}
  {$DEFINE DELPHILANGUAGE}
{$ENDIF}

{$IFDEF BCBCOMPILER}
  {$DEFINE DELPHILANGUAGE}
{$ENDIF}

{------------------------------------------------------------------------------}
{ Cross-platform related defines                                               }
{------------------------------------------------------------------------------}

{$IFNDEF CPU386}
  {$DEFINE PUREPASCAL}
{$ENDIF}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS} // predefined for D6+/BCB6+
  {$DEFINE Win32API}
{$ENDIF}

{$IFDEF DELPHILANGUAGE}
  {$IFDEF LINUX}
    {$DEFINE UNIX}
  {$ENDIF}

  {$IFNDEF CONSOLE}
    {$IFDEF LINUX}
      {$DEFINE VisualCLX}
    {$ENDIF}
    {$IFNDEF VisualCLX}
      {$DEFINE VCL}
    {$ENDIF}
  {$ENDIF ~CONSOLE}
{$ENDIF DELPHILANGUAGE}

{------------------------------------------------------------------------------}
{ Compiler settings                                                            }
{------------------------------------------------------------------------------}

{$IFOPT A+} {$DEFINE ALIGN_ON} {$ENDIF}
{$IFOPT B+} {$DEFINE BOOLEVAL_ON} {$ENDIF}
{$IFDEF COMPILER2_UP}
  {$IFOPT C+} {$DEFINE ASSERTIONS_ON} {$ENDIF}
{$ENDIF}
{$IFOPT D+} {$DEFINE DEBUGINFO_ON} {$ENDIF}
{$IFOPT G+} {$DEFINE IMPORTEDDATA_ON} {$ENDIF}
{$IFDEF COMPILER2_UP}
  {$IFOPT H+} {$DEFINE LONGSTRINGS_ON} {$ENDIF}
{$ENDIF}

// Hints
{$IFOPT I+} {$DEFINE IOCHECKS_ON} {$ENDIF}
{$IFDEF COMPILER2_UP}
  {$IFOPT J+} {$DEFINE WRITEABLECONST_ON} {$ENDIF}
{$ENDIF}
{$IFOPT L+} {$DEFINE LOCALSYMBOLS} {$ENDIF}
{$IFOPT M+} {$DEFINE TYPEINFO_ON} {$ENDIF}
{$IFOPT O+} {$DEFINE OPTIMIZATION_ON} {$ENDIF}
{$IFOPT P+} {$DEFINE OPENSTRINGS_ON} {$ENDIF}
{$IFOPT Q+} {$DEFINE OVERFLOWCHECKS_ON} {$ENDIF}
{$IFOPT R+} {$DEFINE RANGECHECKS_ON} {$ENDIF}

// Real compatibility
{$IFOPT T+} {$DEFINE TYPEDADDRESS_ON} {$ENDIF}
{$IFOPT U+} {$DEFINE SAFEDIVIDE_ON} {$ENDIF}
{$IFOPT V+} {$DEFINE VARSTRINGCHECKS_ON} {$ENDIF}
{$IFOPT W+} {$DEFINE STACKFRAMES_ON} {$ENDIF}

// Warnings
{$IFOPT X+} {$DEFINE EXTENDEDSYNTAX_ON} {$ENDIF}

{$IFDEF DELPHI9_UP} {$DEFINE useinlining} {$ENDIF}

{$IFDEF FPC} {$DEFINE Debug} {$ENDIF}
{$IFNDEF FPC}{$IFDEF DELPHI6_UP} {$DEFINE CPU_Detection} {$ENDIF} {$ENDIF}

{$IFDEF DELPHI7_UP} {$ELSE} {$UNDEF UseNativeTypes} {$ENDIF}
