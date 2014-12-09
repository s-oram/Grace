// r8brain-free-src Copyright (c) 2013 Aleksey Vaneev
// See the "License.txt" file for license.
//
// Please read the "r8bsrc.h" file for function descriptions.

unit r8bsrc;

interface

{$include r8Brain.inc}

type
	CR8BResampler = Pointer;
	PR8BDouble = ^Double;

const
  R8BrainVersion = 1.4;

const
	r8brr16 = 0; // 16-bit precision resampler.
	r8brr16IR = 1; // 16-bit precision resampler for impulse responses.
	r8brr24 = 2; // 24-bit precision resampler (including 32-bit floating point).

///  function r8b_create();
///
///  ReqTransBand
///  Required transition band, in percent of the
///	 spectral space of the input signal (or the output signal if
///	 downsampling is performed) between filter's -3 dB point and the Nyquist
///	 frequency. The range is from CDSPFIRFilter::getLPMinTransBand() to
///	 CDSPFIRFilter::getLPMaxTransBand(), inclusive. When upsampling 88200 or
///	 96000 audio to a higher sample rates the ReqTransBand can be
///	 considerably increased, up to 30. The selection of ReqTransBand depends
///	 on the level of desire to preserve the high-frequency content. While
///	 values 0.5 to 2 are extremely "greedy" settings, not necessary in most
///	 cases, values 2 to 3 can be used in most cases. Values 3 to 4 are
///	 relaxed settings, but they still offer a flat frequency response up to
///	 21kHz with 44.1k source or destination sample rate.
///
///  Res
///  0 - 16-bit precision resampler.
///  1 - 16-bit precision resampler for impulse responses.
///  2 - 24-bit precision resampler (including 32-bit floating point)
///
function r8b_create( SrcSampleRate: Double; DstSampleRate: Double;
	MaxInLen: LongInt; ReqTransBand: Double; Res: LongInt ): CR8BResampler;
	cdecl; external 'r8bsrc.dll';

procedure r8b_delete( rs: CR8BResampler ); cdecl; external 'r8bsrc.dll';

function r8b_get_latency( rs: CR8BResampler ): LongInt; cdecl;
	external 'r8bsrc.dll';

procedure r8b_clear( rs: CR8BResampler ); cdecl; external 'r8bsrc.dll';

function r8b_process( rs: CR8BResampler; ip0: PR8BDouble; l: LongInt;
	var op0: PR8BDouble ): LongInt; cdecl; external 'r8bsrc.dll';

implementation

end.
