// This file contains the default conditional defines for the EasyEffect VST template.
// All conditional defines should be documented in here.
// This shouldn't actually be used in a project. Instead copy this file to your
// Delphi VST project directory and rename it "Defines.inc".
// The EasyEffect Vst Template files will attempt to include it at the top of
// unit it is used in.

{$DEFINE REQUIRE_TEMPLATE_VERSION_2}

{$DEFINE Beta}

{$DEFINE OverSampleEnabled}

{.$DEFINE Logging}
{$IFDEF Logging}
  {.$DEFINE LogDispatcherOpcodes}
  {$DEFINE LogToConsole}
{$ENDIF}

{$DEFINE CpuMonitor}



