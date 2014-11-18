// -----------------------------------------------------------------------------
// Project:         New Drag and Drop Component Suite
// Module:          DragDrop
// Description:     Implements base classes and utility functions.
// Version:         5.6
// Date:            16-SEP-2014
// Target:          Win32, Delphi 6-XE7
// Authors:         Anders Melander, anders@melander.dk, http://melander.dk
// Latest Version   https://github.com/landrix/The-new-Drag-and-Drop-Component-Suite-for-Delphi
// Copyright        © 1997-1999 Angus Johnson & Anders Melander
//                  © 2000-2010 Anders Melander
//                  © 2011-2014 Sven Harazim
// -----------------------------------------------------------------------------

// -----------------------------------------------------------------------------
//              Required compiler directives
// -----------------------------------------------------------------------------

{$BOOLEVAL OFF}         // Short circuit boolean evaluation.
{$EXTENDEDSYNTAX ON}    // Enable Delphi Pascal extensions.
{$LONGSTRINGS ON}       // String = AnsiString.
{$ALIGN ON}             // Aligned data. Required by COM.
{$ifdef BCB}
  {$ObjExportAll ON}    // Required for C++ Builder
{$endif}

// The following are not realy nescessary, but they are good practice.
{$TYPEDADDRESS ON}      // @ operator returns typed pointer.
{$WRITEABLECONST OFF}   // Typed consts are R/O.

// Disable platform warnings. This library is only supported on Windows.
{$IF CompilerVersion >= 6.0}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$IFEND}
// Disable "unsafe" warnings. Let's assume I know what I'm doing.

