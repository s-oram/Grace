(*
  This file is a part of Audio Components Suite v 2.2 (Kylix Edition).
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit acs_reg;

interface

uses
  Classes, ACS_Audio, ACS_CDROM, ACS_AudioMix, ACS_Converters, ACS_Mixer,
  ACS_Misc, ACS_Vorbis, ACS_Wave, ACS_Filters, ACS_LAME, ACS_Streams, ACS_ALSA,
  ACS_Indicator, ACS_FLAC, ACS_MAD;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Audio I/O', [TAudioIn, TAudioOut, TALSAAudioIn, TALSAAudioOut, TAOLive,
  TCDIn, TCDPlayer,  TMixer,  TMemoryIn, TMPEGIn,
  TMP3Out, TVorbisIn, TVorbisOut, TFLACIn, TFLACOut,
  TWaveIn, TWaveOut, TStreamIn, TStreamOut, TInputList, TNULLOut, TMP3ToWav]);
  RegisterComponents('Audio Processing', [TAudioMixer, TSampleConverter, TRateConverter, TMSConverter, TBWFilter, TSincFilter, TConvolver, TStereoBalance, TAudioProcessor, TSoundIndicator]);
end;


end.
