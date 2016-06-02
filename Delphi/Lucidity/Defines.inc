{$IFNDEF Debug}
  {$DEFINE ReleaseBuild}
{$ELSE}
  {$UNDEF ReleaseBuild}
{$ENDIF}


{$DEFINE REQUIRE_TEMPLATE_VERSION_2}

{.$DEFINE Demo}
{$DEFINE Beta}

{.$DEFINE CpuMonitor}
{$DEFINE OverSampleEnabled}

{.$DEFINE HasAudioIns}
{$DEFINE HasAudioOuts}

{$DEFINE HasMidiIn}
{$DEFINE HasMidiOut}


// Use assembly functions?
{$DEFINE UseASM}



{$IFDEF ReleaseBuild}
  //== Release Defines ==
  {.$DEFINE madExcept} // Do not use MadExcept.
  {.$DEFINE Logging}
  {.$DEFINE LogToFile}
  {.$DEFINE LogToConsole}
  {.$DEFINE LogDispatcherOpcodes}
  {.$DEFINE LogUserInteractions}
  {.$DEFINE SimpleLog}
  {.$DEFINE AudioInline}
  {.$DEFINE StorePatchWhenRestoring}
{$ELSE}
  //== Debug Defines ==
  {.$DEFINE madExcept} // Do not use MadExcept.
  {$DEFINE Logging}
  {$DEFINE LogToFile}
  {$DEFINE LogToConsole}
  {$DEFINE LogDispatcherOpcodes}
  {$DEFINE LogUserInteractions}
  {.$DEFINE SimpleLog}
  {.$DEFINE AudioInline}
  {.$DEFINE StrictDebugChecks}
  {.$DEFINE StorePatchWhenRestoring}

  // Development only features include a few advanced features only intended for
  // use during development.
  {.$DEFINE IncludeDevelopmentOnlyFeatures}
{$ENDIF}


//==== Copy Protection ====
{.$DEFINE KeySalt1}
{.$DEFINE KeySalt2}
{.$DEFINE KeySalt3}
{.$DEFINE KeySalt4}
{.$DEFINE KeySalt5}
{.$DEFINE KeySalt6}
{.$DEFINE KeySalt7}
{.$DEFINE KeySalt8}
{.$DEFINE KeySalt9}
{.$DEFINE KeySalt10}
{.$DEFINE KeySalt11}
{.$DEFINE KeySalt12}
{.$DEFINE KeySalt13}
{.$DEFINE KeySalt14}
{.$DEFINE KeySalt15}
{.$DEFINE KeySalt16}
{.$DEFINE KeySalt17}
{.$DEFINE KeySalt18}
{.$DEFINE KeySalt19}
{.$DEFINE KeySalt20}
{.$DEFINE KeySalt21}
{.$DEFINE KeySalt22}
{.$DEFINE KeySalt23}
{.$DEFINE KeySalt24}
{.$DEFINE KeySalt25}


//===== General Purpose Defines ========
{$IF Defined(UseASM) and Defined(CPUX64)}
{$ELSEIF Defined(UseASM) and Defined(CPUX86)}
{$ELSE}
{$IFEND}


{$EXCESSPRECISION OFF}
{$WARN SYMBOL_PLATFORM OFF}
