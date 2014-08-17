
(* $Id: GainAnalysis.pas 992 2009-09-27 12:19:26Z andrei.borovsky $ *)

unit GainAnalysis;

interface

uses
  Windows, SysUtils, ACS_Classes, ACS_Procs;

const
  LibGainPath = 'libgain.dll';

var
  LibGainLoaded : Boolean = False;

const
  GAIN_NOT_ENOUGH_SAMPLES = -24601;
  GAIN_ANALYSIS_ERROR  = 0;
  GAIN_ANALYSIS_OK = 1;
  INIT_GAIN_ANALYSIS_ERROR = 0;
  INIT_GAIN_ANALYSIS_OK = 1;

type

  InitGainAnalysis_t = function(SampleFreq : LongWord) : Integer; cdecl;
  AnalyzeSamples_t = function(left_samples, right_samples : PDouble; num_samples : LongWord; num_channels : Integer) : Integer; cdecl;
  ResetSampleFrequency_t = function(SampleFreq : LongWord) : Integer; cdecl;
  GetTitleGain_t = function : Double; cdecl;
  GetAlbumGain_t = function : Double; cdecl;

var

  InitGainAnalysis : InitGainAnalysis_t;
  AnalyzeSamples : AnalyzeSamples_t;
  ResetSampleFrequency : ResetSampleFrequency_t;
  GetTitleGain : GetTitleGain_t;
  GetAlbumGain : GetAlbumGain_t;

  procedure LoadLibGain;
  procedure UnloadLibGain;

implementation

var
  LibHandle : HMODULE = 0;

procedure LoadLibGain;
begin
  LoadLibCS.Enter;
  if LibGainLoaded then
  begin
    LoadLibCS.Leave;
    Exit;
  end;
  LibHandle := LoadLibrary(LibGainPath);
  LibGainLoaded := (LibHandle <> 0);
  if LibGainLoaded then
  begin
    InitGainAnalysis := GetProcAddress(LibHandle, 'InitGainAnalysis');
    AnalyzeSamples := GetProcAddress(LibHandle, 'AnalyzeSamples');
    ResetSampleFrequency := GetProcAddress(LibHandle, 'ResetSampleFrequency');
    GetTitleGain := GetProcAddress(LibHandle, 'GetTitleGain');
    GetAlbumGain := GetProcAddress(LibHandle, 'GetAlbumGain');
  end;
  LoadLibCS.Leave;
end;

procedure UnloadLibGain;
begin
  if LibHandle <> 0 then
    FreeLibrary(LibHandle);
  LibGainLoaded := False;
end;

end.
