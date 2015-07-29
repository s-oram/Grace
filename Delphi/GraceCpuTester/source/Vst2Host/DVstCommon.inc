// Define SDK Version (you can generate different versions (from 2.0 to 2.4) of this SDK by setting the unwanted extensions to 0).
{$DEFINE VST_2_1_EXTENSIONS} // Version 2.1 extensions (08-06-2000)
{$DEFINE VST_2_2_EXTENSIONS} // Version 2.2 extensions (08-06-2001)
{$DEFINE VST_2_3_EXTENSIONS} // Version 2.3 extensions (20-05-2003)
{$DEFINE VST_2_4_EXTENSIONS} // Version 2.4 extensions (01-01-2006)

// Uncomment to use deprecated features
{$DEFINE VST_USE_DEPRECATED}

// Uncomment for 64 Bit Platform (should be automatic?)
{$IFNDEF FPC}
  {$IFDEF WIN64}
    {$DEFINE CPU64}
  {$ELSE}
    {$DEFINE CPU32}
  {$ENDIF}
{$ENDIF}

{$IFDEF CPUX64}
  {$DEFINE VST_64BIT_PLATFORM}
{$ENDIF}

{$IFDEF VST_64BIT_PLATFORM}
  {$ALIGN 8}
{$ELSE}
  {$ALIGN 1}
{$ENDIF}
