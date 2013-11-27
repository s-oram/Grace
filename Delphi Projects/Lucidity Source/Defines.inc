{$DEFINE REQUIRE_TEMPLATE_VERSION_2}

{.$DEFINE Demo}
{$DEFINE Beta}

{$DEFINE CpuMonitor}
{$DEFINE OverSampleEnabled}

{.$DEFINE HasAudioIns}
{$DEFINE HasAudioOuts}

{$DEFINE HasMidiIn}
{$DEFINE HasMidiOut}


//== Release Defines ==
{$IFNDEF Debug}
  {$DEFINE madExcept}
  {.$DEFINE Logging}
  {.$DEFINE LogToFile}
  {.$DEFINE LogToConsole}
  {.$DEFINE LogDispatcherOpcodes}
  {.$DEFINE LogUserInteractions}
  {.$DEFINE SimpleLog}
  {$DEFINE AudioInline}
{$ENDIF}

//== Debug Defines ==
{$IFDEF Debug}
  {$DEFINE madExcept}
  {$DEFINE Logging}
  {$DEFINE LogToFile}
  {$DEFINE LogToConsole}
  {.$DEFINE LogDispatcherOpcodes}
  {$DEFINE LogUserInteractions}
  {.$DEFINE SimpleLog}
  {$DEFINE AudioInline}

  {.$DEFINE StrictDebugChecks}
{$ENDIF}



