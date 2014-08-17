(*
  This file is a part of New Audio Components package 1.7
  Copyright (c) 2002-2008 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* C-style DMO management API. Copyright (c) 2008 Andrei Borovsky *)

(* $Id: msdmo.pas 552 2008-03-20 10:49:45Z andrei.borovsky $ *)

unit msdmo;

interface

uses
  Windows, Classes, SysUtils, ACS_Classes, ACS_Procs, ActiveX, MMSystem, SyncObjs, ComObj, _DXTypes, _Direct3D9, _DirectShow9, wmfintf;

const

 CLSID_CWMAEncMediaObject : TGUID = '{70f598e9-f4ab-495a-99e2-a7c4d3d89abf}';
 CLSID_CWMADecMediaObject : TGUID = '{2eeb4adf-4578-4d10-bca7-bb955f56320a}';
 CLSID_CResamplerMediaObject : TGUID = '{f447b69e-1884-4a7e-8055-346f74d6edb3}';
 CLSID_CWMAudioAEC : TGUID = '{745057c7-f353-4f2d-a7ee-58434477730e}';
 IID_IPropertyStore : TGUID = '{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}';

 PID_FIRST_USABLE = $02;

type

  {$EXTERNALSYM _tagpropertykey}
  _tagpropertykey = packed record
    fmtid: TGUID;
    pid: DWORD;
  end;

  {$EXTERNALSYM PROPERTYKEY}
  PROPERTYKEY = _tagpropertykey;
  PPropertyKey = ^TPropertyKey;
  TPropertyKey = _tagpropertykey;


const
  MFPKEY_WMAAECMA_SYSTEM_MODE : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 0);
  MFPKEY_WMAAECMA_DMO_SOURCE_MODE : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 1);
  MFPKEY_WMAAECMA_DEVICE_INDEXES : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 2);
  MFPKEY_WMAAECMA_FEATURE_MODE   : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 3);
  MFPKEY_WMAAECMA_FEATR_FRAME_SIZE : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 4);
  MFPKEY_WMAAECMA_FEATR_ECHO_LENGTH : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 5);
  MFPKEY_WMAAECMA_FEATR_NS : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 6);
  MFPKEY_WMAAECMA_FEATR_AGC : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 7);
  MFPKEY_WMAAECMA_FEATR_AES : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 8);
  MFPKEY_WMAAECMA_FEATR_VAD : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 9);
  MFPKEY_WMAAECMA_FEATR_CENTER_CLIP  : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 10);
  MFPKEY_WMAAECMA_FEATR_NOISE_FILL : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 11);
  MFPKEY_WMAAECMA_RETRIEVE_TS_STATS  : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 12);
  MFPKEY_WMAAECMA_QUALITY_METRICS  : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 13);
  MFPKEY_WMAAECMA_MICARRAY_DESCPTR : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 14);
  MFPKEY_WMAAECMA_DEVICEPAIR_GUID : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 15);
  MFPKEY_WMAAECMA_FEATR_MICARR_MODE : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 16);
  MFPKEY_WMAAECMA_FEATR_MICARR_BEAM : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 17);
  MFPKEY_WMAAECMA_FEATR_MICARR_PREPROC : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 18);
  MFPKEY_WMAAECMA_MIC_GAIN_BOUNDER : TPropertyKey = ( fmtid: '{6f52c567-0360-4bd2-9617-ccbf1421c939}'; pid: PID_FIRST_USABLE + 19);

  SINGLE_CHANNEL_AEC = 0;
  ADAPTIVE_ARRAY_ONLY = SINGLE_CHANNEL_AEC + 1;
  OPTIBEAM_ARRAY_ONLY = ADAPTIVE_ARRAY_ONLY + 1;
  ADAPTIVE_ARRAY_AND_AEC = OPTIBEAM_ARRAY_ONLY + 1;
  OPTIBEAM_ARRAY_AND_AEC = ADAPTIVE_ARRAY_AND_AEC;
  SINGLE_CHANNEL_NSAGC	= OPTIBEAM_ARRAY_AND_AEC + 1;
  MODE_NOT_SET = SINGLE_CHANNEL_NSAGC + 1;

  AEC_VAD_DISABLED = 0;
  AEC_VAD_NORMAL = 1;
  AEC_VAD_FOR_AGC = 2;
  AEC_VAD_FOR_SILENCE_SUPPRESSION = 3;

type

 IPropertyStore = interface
 ['{886d8eeb-8cf2-4446-8d02-cdba1dbdcf99}']
   function GetCount(out cProps: DWORD): HResult; stdcall;
   function GetAt(iProp: DWORD; out pkey: TPropertyKey): HResult; stdcall;
   function GetValue(const key: TPropertyKey; out pv: TPropVariant): HResult; stdcall;
   function SetValue(const key: TPropertyKey; const propvar: TPropVariant): HResult; stdcall;
   function Commit: HResult; stdcall;
 end;

 dmo_resampler_spec = record
    sample_rate : LongWord;
    channels : Word;
    bps : Word;
  end;

  dmo_resampler = record
    resampler : IMediaObject;
    input_spec : dmo_resampler_spec;
    output_sr : LongWord;
    InputBuffer, OutputBuffer : IMediaBuffer;
  end;

  dmo_vcfilter = record
    filter : IMediaObject;
//    EnableAES : WordBool;
    EnableAGC : WordBool;
    EnableNoiseSuppression : WordBool;
    EnableVAD : WordBool;
    input_spec : dmo_resampler_spec;
    output_spec : dmo_resampler_spec;
    InputBuffer, OutputBuffer : IMediaBuffer;
    FrameSize : LongWord;
  end;

  procedure dmo_resampler_init(var resampler : dmo_resampler);
  procedure dmo_resampler_set_formats(var resampler : dmo_resampler);
  function dmo_resampler_accepts_data(var resampler : dmo_resampler) : Boolean;
  procedure dmo_resampler_prepare_input(var resampler : dmo_resampler; var Buffer : Pointer; var BufSize : LongWord);
  procedure dmo_resampler_reset_input(var resampler : dmo_resampler; var BufSize : LongWord);
  procedure dmo_resampler_write_input(var resampler : dmo_resampler);
  function dmo_resampler_get_output(var resampler : dmo_resampler; var Buffer : Pointer; var BufSize : LongWord) : Boolean;
  procedure dmo_resampler_free_output(var resampler : dmo_resampler);
  procedure dmo_resampler_free(var resampler : dmo_resampler);

  procedure dmo_vcfilter_init(var filter : dmo_vcfilter);
  procedure dmo_vcfilter_set_formats(var filter : dmo_vcfilter);
  function dmo_vcfilter_accepts_data(var filter : dmo_vcfilter) : Boolean;
  procedure dmo_vcfilter_prepare_input(var filter : dmo_vcfilter; var Buffer : Pointer; var BufSize : LongWord);
  procedure dmo_vcfilter_reset_input(var filter : dmo_vcfilter; var BufSize : LongWord);
  procedure dmo_vcfilter_write_input(var filter : dmo_vcfilter);
  function dmo_vcfilter_get_output(var filter : dmo_vcfilter; var Buffer : Pointer; var BufSize : LongWord) : Boolean;
  procedure dmo_vcfilter_free_output(var filter : dmo_vcfilter);
  procedure dmo_vcfilter_free(var filter : dmo_vcfilter);


implementation

type
 TMediaBuffer = class(TInterfacedObject, IMediaBuffer)
 private
   m_cbLength : DWORD;
   m_cbMaxLength : DWORD;
   m_pbData : PByte;
 public
   constructor Create(cbMaxLength : DWORD);
   destructor Destroy; override;
   function SetLength(cbLength: DWORD): HResult; stdcall;
   function GetMaxLength(out pcbMaxLength: DWORD): HResult; stdcall;
   function GetBufferAndLength(out ppBuffer: PByte; // not filled if NULL
                                out pcbLength: DWORD    // not filled if NULL
                                ): HResult; stdcall;
 end;


  procedure dmo_vcfilter_init(var filter : dmo_vcfilter);
  var
    res : HResult;
    ps : IPropertyStore;
    propvar : TPropVariant;
  begin
    //FillChar(filter, SizeOf(filter), 0);
    filter.filter := nil;
    CoInitialize(nil);
    res := CoCreateInstance(CLSID_CWMAudioAEC, nil, CLSCTX_INPROC_SERVER, IID_IMediaObject, filter.filter);
    if res <> S_OK then
      raise EAuException.Create('Failed to initialize DSP Class: ' + IntToHex(res, 8));
    ps := filter.filter as IPropertyStore;
    propvar.vt := VT_BOOL;
    propvar.boolVal := False;
    res := ps.SetValue(MFPKEY_WMAAECMA_DMO_SOURCE_MODE, propvar);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up DSP Class: ' + IntToHex(res, 8));
    propvar.vt := VT_I4;
    propvar.lVal := 5;
    res := ps.SetValue(MFPKEY_WMAAECMA_SYSTEM_MODE, propvar);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up DSP Class: ' + IntToHex(res, 8));
    dmo_vcfilter_set_formats(filter);
    propvar.vt := VT_BOOL;
    propvar.boolVal := True;
    res := ps.SetValue(MFPKEY_WMAAECMA_FEATURE_MODE, propvar);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up DSP Class: ' + IntToHex(res, 8));
 (*   propvar.vt := VT_I4;
    if filter.EnableAES then
      propvar.lVal := 1
    else
      propvar.lVal := 0;
    res := ps.SetValue(MFPKEY_WMAAECMA_FEATR_AES, propvar);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up DSP Class: ' + IntToHex(res, 8)); *)
    propvar.vt := VT_BOOL;
    propvar.boolVal := filter.EnableAGC;
    res := ps.SetValue(MFPKEY_WMAAECMA_FEATR_AGC, propvar);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up DSP Class: ' + IntToHex(res, 8));
    propvar.vt := VT_I4;
    if filter.EnableNoiseSuppression then
      propvar.lVal := 1
    else
      propvar.lVal := 0;
    res := ps.SetValue(MFPKEY_WMAAECMA_FEATR_NS, propvar);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up DSP Class: ' + IntToHex(res, 8));
    propvar.vt := VT_I4;
    if filter.EnableVAD then
    begin
//      if filter.EnableAGC or filter.EnableNoiseSuppression then
//         propvar.lVal := AEC_VAD_FOR_AGC
//      else
        propvar.lVal := AEC_VAD_FOR_SILENCE_SUPPRESSION;
    end else
      propvar.lVal := AEC_VAD_DISABLED;
    res := ps.SetValue(MFPKEY_WMAAECMA_FEATR_VAD, propvar);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up DSP Class: ' + IntToHex(res, 8));
    propvar.vt := VT_I4;
    propvar.ulVal := 160;
    res := ps.SetValue(MFPKEY_WMAAECMA_FEATR_FRAME_SIZE, propvar);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up DSP Class: ' + IntToHex(res, 8));
    filter.FrameSize := 160; //propvar.ulVal;
    res := filter.filter.AllocateStreamingResources;
    if res <> S_OK then
      raise EAuException.Create('Failed to allocate resources: ' + IntToHex(res, 8));

  end;

  procedure dmo_vcfilter_set_formats(var filter : dmo_vcfilter);
  var
    res : HResult;
    MT : TAMMediaType;
    pWave  : PWAVEFORMATEX;
    Wave : TWAVEFORMATEX;
    SC1, SC2 : LongWord;
  begin
    filter.filter.GetStreamCount(SC1, SC2);
    MT.majortype := MEDIATYPE_Audio;
    MT.subtype := MEDIASUBTYPE_PCM;
    MT.lSampleSize := 0;
    MT.bFixedSizeSamples := True;
    MT.bTemporalCompression := False;
    MT.formattype := FORMAT_WaveFormatEx;
    MoInitMediaType(@MT, SizeOf(TWAVEFORMATEX));
    MT.subtype := MEDIASUBTYPE_PCM;
    MT.cbFormat := SizeOf(Wave);
    MT.pbFormat := @Wave;
    Wave.wFormatTag := 1;
    Wave.nChannels := filter.input_spec.channels;
    Wave.nSamplesPerSec := filter.input_spec.sample_rate;
    Wave.wBitsPerSample := filter.input_spec.bps;
    Wave.nBlockAlign := Wave.wBitsPerSample*Wave.nChannels div 8;
    Wave.nAvgBytesPerSec := Wave.nSamplesPerSec * Wave.nBlockAlign;
    Wave.cbSize := 0;
    res := filter.filter.SetInputType(0, @MT, 0);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up input: ' + IntToHex(res, 8));
      MoFreeMediaType(@MT);
    res := filter.filter.GetInputCurrentType(0,MT);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up input: ' + IntToHex(res, 8));
    if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
    begin
      pWave := MT.pbFormat;
      filter.input_spec.sample_rate := pWave.nSamplesPerSec;
      filter.input_spec.channels := pWave.nChannels;
      filter.input_spec.bps := pWave.wBitsPerSample;
    end;
    MoFreeMediaType(@MT);

    MT.majortype := MEDIATYPE_Audio;
    MT.subtype := MEDIASUBTYPE_PCM;
    MT.lSampleSize := 0;
    MT.bFixedSizeSamples := True;
    MT.bTemporalCompression := False;
    MT.formattype := FORMAT_WaveFormatEx;
    MoInitMediaType(@MT, SizeOf(TWAVEFORMATEX));
    MT.cbFormat := SizeOf(Wave);
    MT.pbFormat := @Wave;
    Wave.wFormatTag := 1;
    Wave.nChannels := filter.output_spec.channels;
    Wave.nSamplesPerSec := filter.output_spec.sample_rate;
    Wave.wBitsPerSample := filter.output_spec.bps;
    Wave.nBlockAlign := Wave.wBitsPerSample*Wave.nChannels div 8;
    Wave.nAvgBytesPerSec := Wave.nSamplesPerSec * Wave.nBlockAlign;
    Wave.cbSize := 0;
    filter.filter.SetOutputType(0, @MT, 0);
    MoFreeMediaType(@MT);
    res := filter.filter.GetOutputCurrentType(0, MT);
    if res <> S_OK then
      raise EAuException.Create('Failed to set up output: ' + IntToHex(res, 8));
    if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
    begin
      pWave := MT.pbFormat;
      filter.output_spec.sample_rate := pWave.nSamplesPerSec;
      filter.output_spec.channels := pWave.nChannels;
      filter.output_spec.bps := pWave.wBitsPerSample;
    end;
    MoFreeMediaType(@MT);
  end;

  function dmo_vcfilter_accepts_data(var filter : dmo_vcfilter) : Boolean;
  var
    Flags : LongWord;
    res : HResult;
  begin
    res := filter.filter.GetInputStatus(0, Flags);
    if res <> S_OK then
      raise EAuException.Create('Getting status failed: ' + IntToHex(res, 8));
    Result := (Flags and DMO_INPUT_STATUSF_ACCEPT_DATA) <> 0;
  end;

  procedure dmo_vcfilter_prepare_input(var filter : dmo_vcfilter; var Buffer : Pointer; var BufSize : LongWord);
  var
    MB : TMediaBuffer;
  begin
    MB := TMediaBuffer.Create(BufSize);
    MB.SetLength(BufSize);
    MB.GetBufferAndLength(PByte(Buffer), BufSize);
    filter.InputBuffer := MB as IMediaBuffer;
  end;

  procedure dmo_vcfilter_reset_input(var filter : dmo_vcfilter; var BufSize : LongWord);
  begin
    filter.InputBuffer.SetLength(BufSize);
  end;

  procedure dmo_vcfilter_write_input(var filter : dmo_vcfilter);
  var
    res : HResult;
  begin
    res := filter.filter.ProcessInput(0, filter.InputBuffer, 0, 0, 0);
    filter.InputBuffer := nil;
    if res <> S_OK then
      raise EAuException.Create('Reading input failed: ' + IntToHex(res, 8));
  end;

  function dmo_vcfilter_get_output(var filter : dmo_vcfilter; var Buffer : Pointer; var BufSize : LongWord) : Boolean;
  var
    res : HResult;
    Outp : LongWord;
    OutputDataBuffer : TDMOOutputDataBuffer;
    A : TDMOOutputDataBufferArray;
  begin
    filter.OutputBuffer := TMediaBuffer.Create(1024*1024*3) as IMediaBuffer;
    OutputDataBuffer.pBuffer := filter.OutputBuffer;
    OutputDataBuffer.rtTimestamp := 0;
    OutputDataBuffer.rtTimelength := 0;
    OutputDataBuffer.dwStatus := 0;
    A[0] := OutputDataBuffer;
    res := filter.filter.ProcessOutput(0, 1, A, Outp);
    OutputDataBuffer.pBuffer := nil;
    if res <> S_OK then
      raise EAuException.Create('Data processing failed: ' + IntToHex(res, 8));
    filter.OutputBuffer.GetBufferAndLength(PByte(Buffer), BufSize);
    Result := (OutputDataBuffer.dwStatus and DMO_OUTPUT_DATA_BUFFERF_INCOMPLETE) = 0;
  end;

  procedure dmo_vcfilter_free_output(var filter : dmo_vcfilter);
  begin
    filter.OutputBuffer := nil;
  end;

  procedure dmo_vcfilter_free(var filter : dmo_vcfilter);
  begin
    filter.filter.FreeStreamingResources;
    filter.filter := nil;
  end;


  procedure dmo_resampler_init(var resampler : dmo_resampler);
  var
    res : HResult;
  begin
    FillChar(resampler, SizeOf(resampler), 0);
    CoInitialize(nil);
    res := CoCreateInstance(CLSID_CResamplerMediaObject, nil, CLSCTX_INPROC_SERVER, IID_IMediaObject, resampler.resampler);
    if res <> S_OK then
      raise EAuException.Create('Failed to initialize resampler: ' + IntToHex(res, 8));
    resampler.resampler.AllocateStreamingResources; //?
  end;

  procedure dmo_resampler_set_formats(var resampler : dmo_resampler);
  var
    res : HResult;
    i : LongWord;
    MT : TAMMediaType;
    pWave  : PWAVEFORMATEX;
    Wave : TWAVEFORMATEX;
  begin
    i := 0;

    res := resampler.resampler.GetInputType(0, i, MT);
    while res <> DMO_E_NO_MORE_ITEMS do
    begin
      if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
      //if GUIDSEqual(MT.formattype, WMFORMAT_WaveFormatEx) then
      begin
        MT.formattype := WMFORMAT_WaveFormatEx;
        MT.cbFormat := SizeOf(Wave);
        MT.pbFormat := @Wave;
        Wave.wFormatTag := 1;
        Wave.nChannels := resampler.input_spec.channels;
        Wave.nSamplesPerSec := resampler.input_spec.sample_rate;
        Wave.wBitsPerSample := resampler.input_spec.bps;
        Wave.nBlockAlign := Wave.wBitsPerSample*Wave.nChannels div 8;
        Wave.nAvgBytesPerSec := Wave.nSamplesPerSec * Wave.nBlockAlign;
        Wave.cbSize := 0;
        res := resampler.resampler.SetInputType(0, @MT, 0);
        MoFreeMediaType(@MT); //!!!!!
        if res <> S_OK then
          raise EAuException.Create('Failed to set up input: ' + IntToHex(res, 8));
       Break;
      end;
      MoFreeMediaType(@MT);
      Inc(i);
      res := resampler.resampler.GetInputType(0, i, MT);
    end;
    FillChar(MT, SizeOf(MT), 0);
    resampler.resampler.GetInputCurrentType(0, MT);
    if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
    if GUIDSEqual(MT.formattype, WMFORMAT_WaveFormatEx) then
    begin
      pWave := PWAVEFORMATEX(MT.pbFormat);
      resampler.input_spec.sample_rate := pWave.nSamplesPerSec;
      resampler.input_spec.channels := pWave.nChannels;
      resampler.input_spec.bps := pWave.wBitsPerSample;
    end;
    MoFreeMediaType(@MT);

    i := 0;
    res := resampler.resampler.GetOutputType(0, i, MT);
    while res <> DMO_E_NO_MORE_ITEMS do
    begin
      if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
      begin
        MT.formattype := WMFORMAT_WaveFormatEx;
        MT.cbFormat := SizeOf(Wave);
        MT.pbFormat := @Wave;
        Wave.wFormatTag := 1;
        Wave.nChannels := resampler.input_spec.channels;
        Wave.nSamplesPerSec := resampler.output_sr;
        Wave.wBitsPerSample := resampler.input_spec.bps;
        Wave.nBlockAlign := Wave.wBitsPerSample*Wave.nChannels div 8;;
        Wave.nAvgBytesPerSec := Wave.nSamplesPerSec * Wave.nBlockAlign;
        Wave.cbSize := 0;
        res := resampler.resampler.SetOutputType(0, @MT, 0);
        MoFreeMediaType(@MT); //!!!!
        if res <> S_OK then
          raise EAuException.Create('Failed to set up output: ' + IntToHex(res, 8));
       Break;
      end;
      MoFreeMediaType(@MT);
      Inc(i);
      res := resampler.resampler.GetOutputType(0, i, MT);
    end;

    FillChar(MT, SizeOf(MT), 0);
    resampler.resampler.GetOutputCurrentType(0, MT);
    if GUIDSEqual(MT.subtype, MEDIASUBTYPE_PCM) then
    if GUIDSEqual(MT.formattype, WMFORMAT_WaveFormatEx) then
    begin
       pWave := PWAVEFORMATEX(MT.pbFormat);
       resampler.output_sr := pWave.nSamplesPerSec;
    end;
    MoFreeMediaType(@MT);

  end;

  procedure dmo_resampler_prepare_input(var resampler : dmo_resampler; var Buffer : Pointer; var BufSize : LongWord);
  var
    MB : TMediaBuffer;
  begin
    MB := TMediaBuffer.Create(BufSize);
    MB.SetLength(BufSize);
    MB.GetBufferAndLength(PByte(Buffer), BufSize);
    resampler.InputBuffer := MB as IMediaBuffer;
  end;

  procedure dmo_resampler_reset_input(var resampler : dmo_resampler; var BufSize : LongWord);
  begin
    resampler.InputBuffer.SetLength(BufSize);
  end;

  function dmo_resampler_accepts_data(var resampler : dmo_resampler) : Boolean;
  var
    Flags : LongWord;
  begin
    resampler.resampler.GetInputStatus(0, Flags);
    Result := (Flags and DMO_INPUT_STATUSF_ACCEPT_DATA) <> 0;
  end;

  procedure dmo_resampler_write_input(var resampler : dmo_resampler);
  var
    res : HResult;
  begin
    res := resampler.resampler.ProcessInput(0, resampler.InputBuffer, 0, 0, 0);
    resampler.InputBuffer := nil;
    if res <> S_OK then
      raise EAuException.Create('Reading input failed: ' + IntToHex(res, 8));
  end;

  function dmo_resampler_get_output(var resampler : dmo_resampler; var Buffer : Pointer; var BufSize : LongWord) : Boolean;
  var
    res : HResult;
    Outp : LongWord;
    OutputDataBuffer : TDMOOutputDataBuffer;
    A : TDMOOutputDataBufferArray;
  begin
    resampler.OutputBuffer := TMediaBuffer.Create(1024*1024*3) as IMediaBuffer;
    OutputDataBuffer.pBuffer := resampler.OutputBuffer;
    OutputDataBuffer.rtTimestamp := 0;
    OutputDataBuffer.rtTimelength := 0;
    OutputDataBuffer.dwStatus := 0;
    A[0] := OutputDataBuffer;
    res := resampler.resampler.ProcessOutput(0, 1, A, Outp);
    OutputDataBuffer.pBuffer := nil;
    if res <> S_OK then
      raise EAuException.Create('Data processing failed: ' + IntToHex(res, 8));
    resampler.OutputBuffer.GetBufferAndLength(PByte(Buffer), BufSize);
    Result := (OutputDataBuffer.dwStatus and DMO_OUTPUT_DATA_BUFFERF_INCOMPLETE) = 0;
  end;

  procedure dmo_resampler_free_output(var resampler : dmo_resampler);
  begin
    resampler.OutputBuffer := nil;
  end;

  procedure dmo_resampler_free(var resampler : dmo_resampler);
  begin
    resampler.resampler.FreeStreamingResources;
    resampler.resampler := nil;
  end;

  constructor TMediaBuffer.Create(cbMaxLength: Cardinal);
  begin
    inherited Create;
    m_cbMaxLength := cbMaxLength;
    GetMem(m_pbData, cbMaxLength);
  end;

  destructor TMediaBuffer.Destroy;
  begin
    FreeMem(m_pbData);
    inherited Destroy;
  end;

  function TMediaBuffer.SetLength;
  begin
    if cbLength > m_cbMaxLength then
    begin
      Result := E_INVALIDARG;
      Exit;
    end;
    m_cbLength := cbLength;
    Result := S_OK;
  end;

  function TMediaBuffer.GetMaxLength;
  begin
    pcbMaxLength := m_cbMaxLength;
    Result := S_OK;
  end;

  function TMediaBuffer.GetBufferAndLength;
  begin
    if @ppBuffer <> nil then
     ppBuffer := m_pbData;
    pcbLength := m_cbLength;
    Result := S_OK;
  end;

end.
