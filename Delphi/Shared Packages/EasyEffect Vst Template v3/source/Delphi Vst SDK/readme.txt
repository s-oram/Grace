Cubase VST SDK for Delphi v2.4.2.1
================================== 11 December 2006

Introduction
------------
This is a conversion of the Cubase VST SDK 2.4 from C/C++ to Delphi.

Included in this archive is the license of the original SDK, which also 
applies to this one. So read it first ! If you don't agree with the license, 
then don't develop plugins with this sdk.

For more information about VST in general and to see the online documentation, 
visit http://www.steinberg.de.



About me
--------
Frederic Vanmol
frederic@axiworld.be
http://www.axiworld.be


Units
-----
  + DAEffect       : The low level interface to Cubase VST.
  + DAEffectX      : types and constants for VST 2.x
  + DAudioEffect   : The high level interface to Cubase VST. Contains the
                     classes from which you can derive your own effects.
  + DAudioEffectX  : base class for VST 2.x plugins
  + DVstFxStore    : records to work with fxp and fxb files
  + DVstUtils      : Some utility functions (also see vstutils.txt).


Thanks to
---------
- Martin Fay for a bunch of bugfixes and improvements
- Frank Olbrich for a detailed explanation and solution of the vcl vs vst bug
- Luca Rocchi and Bo Johanssen for the first solution to the vcl vs vst bug


Changes
-------
  + version 2.4.2.1  (11-12-2006)
    - small bugfix

  + version 2.4.2 (08-12-2006)
    - adapted to VST sdk 2.4 revision 2:
       - changes to FXB/FXP records
       - audioMasterPinConnected now deprecated

  + version 2.4.0.3  (06-08-2006)
    - made all records in DAEffect and DAEffectX packed

  + version 2.4.0.2  (28-02-2006)
    - fixed the implementation of AudioEffectX.UpdateSampleRate (casting error)

  + version 2.4.0.1  (08-02-2006)
    - getChunk data value was wrong, fixed now

  + version 2.4.0  (29-01-2006)
    - completely translated the Steinberg VST 2.4 sdk
    - new file DVstCommon.inc. It includes compiler defines that are common to the other files

  + version 2.3.0  (22-05-2003)
    - added all changes from the Steinberg VST 2.3 sdk (see whatsnew.txt)
    - new unit DVstFxStore
    - small updates of documentation (what documentation ?)

  + version 2.2.2  (27-01-2003)
    - fixed the implementation of AudioEffectX.canHostDo to return the result
      of the call to audioMasterCanHostDo instead of either 0 or 1

  + version 2.2.1  (15-02-2002)
    - changed some char members of records to shortint

  + version 2.2.0  (08-08-2001)
    - added VST 2.1 and 2.2 extensions

  + version 2.0.13  (23-01-2001)
    - some minor bugfixes in the constructor of AudioEffect

  + version 2.0.12  (21-11-2000)
    - added the license agreement and manual of the original VST 2.0 sdk

  + version 2.0.11  (17-11-2000)
    - added some new types to DVSTUtils (PSingleArray and PByteArray)

  + version 2.0.10  (03-11-2000)
    - made the rect parameter of AEffEditor.getRect a var parameter instead of a pointer

  + version 2.0.9  (13-02-2000)
    - made VSTMidiEvent.noteOffVelocity a byte instead of a char (DAEffectX.pas)
    - added type TMainProc for VST hosts (DAEffect.pas)
    - changed some of the utility functions a bit to work more efficiently (DVSTUtils.pas)
    - removed the PSingle type from the DAEffect unit. It can be found in the
      Windows unit instead

  + version 2.0.8  (18-02-2000)
    - added function dB2stringRound to DVstUtils

  + version 2.0.7  (16-12-1999)
    - changed the declaration of AudioEffect.getChunk after a suggestion from 
      Martin Fay. The first parameter is now a var instead of just a pointer, 
      which makes mistakes in using it less possible. It does mean that any 
      code you wrote which uses this function has to be (thorougly) checked to
      see if it still does what it's supposed to. I'm sorry for the 
      inconvenience this causes, but in the long run it's better.

  + version 2.0.6  (05-12-1999)
    - fixed a bug with the number of parameters not being reported right
    - made it compile after yesterday's change

  + version 2.0.5  (04-12-1999)
    - changed the "resvd1" field in the AEffect structure to "reservedForHost",
      after a change discussed in the vst-plugins mailing list

  + version 2.0.4  (08-10-1999)
    - removed a bug from the ADelay example and made some changes in AudioEffect
      to make sure it wouldn't happen anymore

  + version 2.0.3  (22-09-1999)
    - moved the Set8087CW call to the initialization part of the DAudioEffect unit

  + version 2.0.2
    - fixed a bug in AudioEffect.SetBlockSize which caused an infinite loop

  + version 2.0.1 :
    - removed the SetFPU method from the AudioEffect class. In the System unit, 
      there is a Set8087CW function which does the same
    - changed the implementation of gapSmallValue to be faster
    - changed the implementation of LogZ to reflect the fact that it does the same
      as LogN in the Delphi Math unit

  + version 2.0 (translation of the VST 2.0 specification) :
    - added units DAEffectX and DAudioEffectX
    - renamed the VSTUtils unit to DVstUtils
    - renamed DAudioE unit to DAudioEffect
    - changed DAEffect and DAudioEffect to reflect changes in VST 2.0
    - moved Hz2string and ms2string functions from the AudioEffect class to the 
      DVstUtils unit

  + version 1.13 :
    - the VSTUtils unit has some more functions
    - the functions in the VSTUtils unit are now documented a bit better

  + version 1.12 :
    - the fix for the VCL compatibility bug is now incorporated into the
      AudioEffect class. So you don't need to call SetControlWordForVstPlugin in
      the constructor of your effect class anymore. This is a bit easier and
      makes it impossible to forget to do this
    - the Control87 unit is now obsolete. I don't include it in the SDK anymore.
      It's availeable for seperate download from my website if you want it
    - I made some cosmetic changes to the units and the documentation

  + version 1.11 :
    - the fields in the ERect record type are now of type Smallint (16bit
      integer). They used to be Shortint (8bit integers), because I had missed
      the fact that the SHORT type in C++ is 16bit