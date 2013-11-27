//******************************************************************************
//
//  DVstFxStore.pas
//  8 December 2006
//
//  Part of the VST 2.4.2 SDK for Delphi
//  by Frederic Vanmol
//     http://www.axiworld.be
//     frederic@axiworld.be
//
//------------------------------------------------------------------------------
//
// VST Plug-Ins SDK
// Version 2.4		$Date: 2006/01/12 09:04:56 $
//
// Category     : VST 2.x Interfaces
// Filename     : vstfxstore.h
// Created by   : Steinberg Media Technologies
// Description  : Definition of Program (fxp) and Bank (fxb) structures
//
// © 2006, Steinberg Media Technologies, All Rights Reserved
//
//******************************************************************************
unit
    DVstFxStore;

interface

uses
    DAEffect;

{$INCLUDE DVstCommon.inc}

//------------------------------------------------------------------------------
const
     cMagic           = 'CcnK';  // Root chunk identifier for Programs (fxp) and Banks (fxb) 
     fMagic           = 'FxCk';  // Regular Program (fxp) identifier
     bankMagic        = 'FxBk';  // Regular Bank (fxb) identifier
     //chunkGlobalMagic = 'FxCh';
     chunkPresetMagic = 'FPCh';  // Program (fxp) identifier for opaque chunk data
     chunkBankMagic   = 'FBCh';  // Bank (fxb) identifier for opaque chunk data

(*
	Note: The C data structures below are for illustration only. You can not read/write them directly.
	The byte order on disk of fxp and fxb files is Big Endian. You have to swap integer
	and floating-point values on Little Endian platforms (Windows, MacIntel)!
*)

//------------------------------------------------------------------------------
/// Program (fxp) structure
//------------------------------------------------------------------------------
type
    PFxProgram = ^fxProgram;
    fxProgram = record
      chunkMagic : VstInt32;               // 'CcnK'
      byteSize   : VstInt32;               // size of this chunk, excl. magic + byteSize

      fxMagic    : VstInt32;               // 'FxCk' (regular) or 'FPCh' (opaque chunk)
      version    : VstInt32;               // format version (currently 1)
      fxID       : VstInt32;               // fx unique ID
      fxVersion  : VstInt32;               // fx version

      numParams  : VstInt32;               // number of parameters
      prgName    : array[0..27] of Ansichar;   // program name (null-terminated ASCII string)

      case integer of
        0 :  (params: array[0..0] of single);  // variable sized array with parameter values
        1 :  (size  : VstInt32;		             // size of program data
			        chunk : array[0..0] of byte);    // variable sized array with opaque program data
    end;


//------------------------------------------------------------------------------
// Bank (fxb) structure
//------------------------------------------------------------------------------
type
    PFxBank = ^fxBank;
    fxBank = record
      chunkMagic     : VstInt32;                  // 'CcnK'
      byteSize       : VstInt32;                  // size of this chunk, excl. magic + byteSize

      fxMagic        : VstInt32;                  // 'FxBk' (regular) or 'FBCh' (opaque chunk)
      version        : VstInt32;                  // format version (1 or 2)
      fxID           : VstInt32;                  // fx unique ID
      fxVersion      : VstInt32;                  // fx version

      numPrograms    : VstInt32;                  // number of programs

      {$IFDEF VST_2_4_EXTENSIONS}
      currentProgram : VstInt32;                  // version 2: current program number
      future         : array[0..123] of byte;     // reserved, should be zero
      {$ELSE}
      future         : array[0..127] of byte;     // reserved, should be zero
      {$ENDIF}

      case integer of
        0 :  (programs: array[0..0] of fxProgram);  // variable number of programs
        1 :  (size  : VstInt32;                     // size of bank data
			        chunk : array[0..0] of byte);                 // variable sized array with opaque bank data
    end;
    

implementation

end.
