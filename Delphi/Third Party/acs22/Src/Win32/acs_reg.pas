unit acs_reg;

interface

uses
  Classes, ACS_Audio, ACS_CDROM, ACS_AudioMix, ACS_Converters,
  ACS_Misc, ACS_Vorbis, ACS_Wave, ACS_Filters, ACS_LAME, ACS_MAC, ACS_Streams, ACS_Indicator, ACS_MAD, ACS_FLAC;

  procedure Register();

implementation

procedure Register();
begin
  RegisterComponents('Audio I/O', [TAudioIn, TAudioOut, TCDPlayer, TCDIn,
  TInputList, TMemoryIn, TVorbisIn, TVorbisOut,
  TWaveIn, TWaveOut, TMP3Out, TMACIn, TMACOut, TStreamIn, TStreamOut, TFLACIn, TFLACOut, TMP3ToWav, TNULLOut]);
  RegisterComponents('Audio Processing', [TAudioMixer, TSampleConverter,
  TRateConverter, TMSConverter, TAudioProcessor, TBWFilter, TSincFilter, TSoundIndicator, TStereoBalance, TConvolver]);

end;


end.
