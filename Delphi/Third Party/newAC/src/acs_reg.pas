unit acs_reg;

(* Title: ACS_Reg 
    Registers all ACS components. *)

interface

uses
  Classes, ACS_DXAudio, AuASIO,
  ACS_CDROM, ACS_AudioMix, ACS_Converters, NewAC_DSP,
  ACS_Misc, ACS_Vorbis, ACS_Wave, ACS_Filters, ACS_LAME, ACS_smpeg, ACS_MAC, ACS_Streams, ACS_FLAC, AuSampleRate, ACS_WavPack, ACS_WinMedia, ACS_TTA, AudioPass, ACS_OptimFROG, ACS_TAK, ACS_MPC, AudioDMO, NewAC_AVI, AuSynch, mr_cddb,
  NewACDTS, NewACAC3, NewACIndicators, NewACDSAudio, ACS_AAC;

  procedure Register();

implementation

procedure Register();
begin
  RegisterComponents('Audio I/O', [TDSAudioOut, TDXAudioIn, TDXAudioOut, TASIOAudioIn, TASIOAudioOut, TASIOAudioDuplex, TCDIn, TWMStreamedIn, TWMStreamedOut,
  TMemoryIn, TVorbisIn, TVorbisOut,
  TWaveIn, TWaveOut, TMP3In, TMP3Out, TMACIn, TMACOut, TStreamIn, TStreamOut, TFLACIn, TFLACOut, TWVIn, TWVOut, TWMIn,
  TWMAOut, TTTAIn, TTTAOut, TOFRIn, TWaveTap, TWMATap, TTAKIn, TMPCIn, TMPCOut, TWMADualPassOut, TAVIIn, TDTSIn, TAC3In, TMpgIn, TMP4In, TAACIn]);
  RegisterComponents('Audio Processing', [TAudioMixer, TFastResampler, TRealTimeMixer, TAudioConverter, TACMConverter,
  TAudioProcessor, TMSResampler, TBWFilter, TSincFilter, TChebyshevFilter,
  TStereoBalance, TResampler, TDitherer, TAudioPass, TNormalizer, TVoiceFilter, TFrequencyAnalysis, TConvolver, TDifferenceEquation, TDownMixer, TGainAnalysis, TGainProcessor]);
  RegisterComponents('Audio Utilities',  [TCDPlayer, TNULLOut, TAudioHiResTimer, TAudioSynchronizer, TCDDBInfo, TCueSplitter, TGainIndicator, TFastGainIndicator, TSpectrumIndicator, TTagEditor, TAudioCache, TAudioPlayList]);
end;


end.
