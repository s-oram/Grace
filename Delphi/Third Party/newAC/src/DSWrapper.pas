(*
 * DSWrapper - Simpilified DirectSound interface for Delphi.
 * The original code for this unit comes from
 * PortAudio Portable Real-Time Audio Library, written in C++ (see the copyright note below).
 * Delphi Translation and some modifications (c) 2007 Andrei Borovsky
 * You can contact me at anb@symmetrica.net.
 *
 ****************************************************************************
 *
 * Simplified DirectSound interface.
 *
 * Author: Phil Burk & Robert Marsanyi
 *
 * For PortAudio Portable Real-Time Audio Library
 * For more information see: http://www.softsynth.com/portaudio/
 * DirectSound Implementation
 * Copyright (c) 1999-2000 Phil Burk & Robert Marsanyi
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *)

 (* $Revision: 1.5 $ $Date: 2007/08/15 10:00:23 $ *)

unit DSWrapper;

(*  DSWrapper
    Simpilified DirectSound interface for Delphi. The original code for this
    unit comes from PortAudio Portable Real-Time Audio Library, written in C++
    (see the copyright note in source). Delphi Translation and some 
    modifications (c) 2007 Andrei Borovsky (anb@symmetrica.net). *)

interface

{$define SUPPORT_AUDIO_CAPTURE}

{$WARNINGS OFF}
uses
  SysUtils, Windows, MMSystem, _DirectSound, ACS_Types;

type

  LPDIRECTSOUND = ^IDirectSound;
  LPDIRECTSOUNDBUFFER  = ^IDirectSoundBuffer;
  LPDIRECTSOUNDCAPTURE = ^IDIRECTSOUNDCAPTURE;
  LPDIRECTSOUNDCAPTUREBUFFER = ^IDIRECTSOUNDCAPTUREBUFFER;
  INT = Integer;
  long = Integer;

  DSoundWrapper = record
    dsw_pDirectSound : IDIRECTSOUND;
    dsw_OutputBuffer : IDIRECTSOUNDBUFFER;
    dsw_WriteOffset : DWORD;
    dsw_OutputSize : LongWord;
    dsw_BytesPerFrame : LongWord;
    ///* Try to detect play buffer underflows. */
    dsw_CounterTicksPerBuffer :LARGE_INTEGER; //* counter ticks it should take to play a full buffer */
    dsw_LastPlayTime : LARGE_INTEGER;
    dsw_LastPlayCursor : UINT;
    dsw_OutputUnderflows : UINT;
    dsw_OutputRunning : BOOL;
    ///* use double which lets us can play for several thousand years with enough precision */
    dsw_FramesWritten : double;
    dsw_FramesPlayed : double;
    {$IFDEF SUPPORT_AUDIO_CAPTURE}
    // Input */
    dsw_pDirectSoundCapture : IDIRECTSOUNDCAPTURE;
    dsw_InputBuffer : IDIRECTSOUNDCAPTUREBUFFER;
    dsw_ReadOffset : UINT;      ///* last read position */
    dsw_InputSize : UINT;
    {$ENDIF} // SUPPORT_AUDIO_CAPTURE */
    dsw_PrimaryBuffer : IDIRECTSOUNDBUFFER;
  end;

  PDSoundWrapper = ^DSoundWrapper;

  XDSoundWrapper = record
    dsw_pDirectSound : IDIRECTSOUND;
    dsw_PrimaryBuffer : IDIRECTSOUNDBUFFER;
    dsw_OutputBuffer : IDIRECTSOUNDBUFFER;
    dsw_WriteOffset : DWORD;
    dsw_OutputSize : LongWord;
    dsw_BytesPerFrame : LongWord;
    dsw_OutputUnderflows : UINT;
    dsw_OutputRunning : BOOL;
    events : array[0..3] of THandle;
    offsets : array[0..3] of LongWord;
  end;


  DSW_DeviceInfo = record
    guid : TGUID;
    {$IFDEF UNICODE}
    name : array [0..127] of WideChar;
    {$ENDIF}
    {$IFNDEF UNICODE}
    name : array [0..127] of Char;
    {$ENDIF}
  end;

  PDSW_DeviceInfo = ^DSW_DeviceInfo;

  DSW_Devices = record
    devcount : Integer;
    dinfo : array [0..15] of DSW_DeviceInfo;
  end;

  PDSW_Devices = ^DSW_Devices;


  function DSW_Init(var dsw : DSoundWrapper) : HRESULT;
  function DSW_InitOutputDevice(var dsw : DSoundWrapper; lpGUID : PGUID) : HRESULT;
  function DSW_InitOutputBuffer(var dsw  : DSoundWrapper; Wnd : HWND; bps : LongWord;
        nFrameRate : LongWord; nChannels, bytesPerBuffer : LongWord) : HRESULT;
  function DSW_InitOutputBufferEx(var dsw  : DSoundWrapper; Wnd : HWND; var WaveFormat : TWaveFormatExtensible; bytesPerBuffer : LongWord) : HRESULT;
  function DSW_StartOutput(var dsw : DSoundWrapper) : HRESULT;
  function DSW_StopOutput(var dsw : DSoundWrapper): HRESULT;
  function DSW_RestartOutput(var dsw : DSoundWrapper) : HRESULT;
  function DSW_QueryOutputSpace(var dsw : DSoundWrapper; var bytesEmpty : LongWord) : HRESULT;
  function DSW_FillEmptySpace(var dsw : DSoundWrapper; Fill : Byte) : HRESULT;
  function DSW_WriteBlock(var dsw : DSoundWrapper; buf : PByte; numBytes : long) : HRESULT;
  function DSW_GetOutputStatus(var dsw : DSoundWrapper) : DWORD;
  function DSW_EnumerateOutputDevices(devices : PDSW_Devices) : HRESULT;
  function DSW_EnumerateInputDevices(devices : PDSW_Devices) : HRESULT;
  function DSW_InitInputDevice(var dsw : DSoundWrapper; lpGUID : PGUID) : HRESULT;
  function DSW_InitInputBuffer(var dsw : DSoundWrapper; bps : Integer;
                nFrameRate : LongWord; nChannels, bufSize : Integer) : HRESULT;
  function DSW_StartInput(var dsw : DSoundWrapper) : HRESULT;
  function DSW_StopInput(var dsw : DSoundWrapper) : HRESULT;
  function DSW_QueryInputFilled(var dsw : DSoundWrapper; var bytesFilled : long) : HRESULT;
  function DSW_ReadBlock(var dsw : DSoundWrapper; buf : PByte; var numBytes : long) : HRESULT;
  procedure DSW_Term(var dsw : DSoundWrapper);

  function DSW_FlushOutputBuffer(var dsw : DSoundWrapper; BytesToLeave : Integer) : HRESULT;

  function DSW_GetVolume(var dsw : DSoundWrapper; out Volume: Longint) : HRESULT;
  function DSW_SetVolume(var dsw : DSoundWrapper; Volume: Longint) : HRESULT;

implementation

const DSW_NUM_POSITIONS = 4;
const DSW_NUM_EVENTS = 5;
const DSW_TERMINATION_EVENT = DSW_NUM_POSITIONS;

procedure DSW_Term(var dsw : DSoundWrapper);
begin
  // Cleanup the sound buffers

  if Assigned(dsw.dsw_OutputBuffer) then
  begin
    dsw.dsw_OutputBuffer.Stop;
//    dsw.dsw_OutputBuffer._Release;
    dsw.dsw_OutputBuffer := nil;
  end;
  dsw.dsw_PrimaryBuffer := nil;
 {$IFDEF SUPPORT_AUDIO_CAPTURE}
  if dsw.dsw_InputBuffer <> nil then
  begin
    dsw.dsw_InputBuffer.Stop;
//     dsw.dsw_InputBuffer._Release;
    dsw.dsw_InputBuffer := nil;
  end;
  if dsw.dsw_pDirectSoundCapture <> nil then
  begin
//    dsw.dsw_pDirectSoundCapture._Release;
    dsw.dsw_pDirectSoundCapture := nil;
  end;
  {$ENDIF} ///* SUPPORT_AUDIO_CAPTURE */
  if Assigned(dsw.dsw_pDirectSound) then
  begin
    dsw.dsw_pDirectSound := nil;
//    dsw.dsw_pDirectSound._Release;
  end;
end;

function DSW_Init(var dsw : DSoundWrapper) : HRESULT;
begin
  FillChar(dsw, sizeof(DSoundWrapper), 0);
  Result := 0;
end;

function DSW_InitOutputDevice(var dsw : DSoundWrapper; lpGUID : PGUID) : HRESULT;
begin
    // Create the DS object
  Result := DirectSoundCreate(lpGUID, dsw.dsw_pDirectSound, nil);
end;

function DSW_InitOutputBuffer(var dsw  : DSoundWrapper; Wnd : HWND; bps : LongWord;
        nFrameRate : LongWord; nChannels, bytesPerBuffer : LongWord) : HRESULT;
var
  dwDataLen : DWORD;
  playCursor : DWORD;
  _hWnd : HWND;
  hr : HRESULT;
  wfFormat : TWaveFormatEx;
  primaryDesc : TDSBufferDesc;
  secondaryDesc : TDSBufferDesc;
  pDSBuffData : PByte;
  counterFrequency : LARGE_INTEGER;
  framesInBuffer : Integer;
begin
  dsw.dsw_OutputSize := bytesPerBuffer;
  dsw.dsw_OutputRunning := FALSE;
  dsw.dsw_OutputUnderflows := 0;
  dsw.dsw_FramesWritten := 0;
  dsw.dsw_BytesPerFrame := nChannels * bps div 8;
  // We were using getForegroundWindow() but sometimes the ForegroundWindow may not be the
  // applications's window. Also if that window is closed before the Buffer is closed
  // then DirectSound can crash. (Thanks for Scott Patterson for reporting this.)
  // So we will use GetDesktopWindow() which was suggested by Miller Puckette.
  // hWnd = GetForegroundWindow();
  _hWnd := Wnd;
  if _hWnd = 0 then
    _hWnd := GetDesktopWindow();
  // Set cooperative level to DSSCL_EXCLUSIVE so that we can get 16 bit output, 44.1 KHz.
  // Exclusize also prevents unexpected sounds from other apps during a performance.
  hr := dsw.dsw_pDirectSound.SetCooperativeLevel(_hWnd, DSSCL_EXCLUSIVE);
  if hr <> DS_OK then
  begin
    Result := hr;
    Exit;
  end;
  // -----------------------------------------------------------------------
  // Create primary buffer and set format just so we can specify our custom format.
  // Otherwise we would be stuck with the default which might be 8 bit or 22050 Hz.
  // Setup the primary buffer description
  FillChar(primaryDesc, sizeof(TDSBUFFERDESC), 0);
  primaryDesc.dwSize := sizeof(DSBUFFERDESC);
  primaryDesc.dwFlags := DSBCAPS_PRIMARYBUFFER; // all panning, mixing, etc done by synth
  primaryDesc.dwBufferBytes := 0;
  primaryDesc.lpwfxFormat := nil;
  // Create the buffer
  Result := dsw.dsw_pDirectSound.CreateSoundBuffer(
        primaryDesc, dsw.dsw_PrimaryBuffer, nil);  // ATTENTION
  if (Result <> DS_OK) then Exit;
    // Define the buffer format
  wfFormat.wFormatTag := WAVE_FORMAT_PCM;
  wfFormat.nChannels := nChannels;
  wfFormat.nSamplesPerSec := nFrameRate;
  wfFormat.wBitsPerSample := bps;
  wfFormat.nBlockAlign := wfFormat.nChannels * wfFormat.wBitsPerSample div 8;
  wfFormat.nAvgBytesPerSec := wfFormat.nSamplesPerSec * wfFormat.nBlockAlign;
  wfFormat.cbSize := 0;  // No extended format info.
  // Set the primary buffer's format
  Result := dsw.dsw_PrimaryBuffer.SetFormat(@wfFormat);
  if Result <> DS_OK then Exit;
    // ----------------------------------------------------------------------
    // Setup the secondary buffer description
  ZeroMemory(@secondaryDesc, sizeof(TDSBUFFERDESC));
  secondaryDesc.dwSize := sizeof(TDSBUFFERDESC);
  secondaryDesc.dwFlags :=  DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_CTRLVOLUME;
  secondaryDesc.dwBufferBytes := bytesPerBuffer;
  secondaryDesc.lpwfxFormat := @wfFormat;
    // Create the secondary buffer
  Result := dsw.dsw_pDirectSound.CreateSoundBuffer(secondaryDesc, dsw.dsw_OutputBuffer, nil);
  if Result <> DS_OK then Exit;
    // Lock the DS buffer

  Result := dsw.dsw_OutputBuffer.Lock(0, dsw.dsw_OutputSize, @pDSBuffData,
            @dwDataLen, nil, nil, 0);
  if Result <> DS_OK then Exit;
    // Zero the DS buffer
  ZeroMemory(pDSBuffData, dwDataLen);
    // Unlock the DS buffer
  Result := dsw.dsw_OutputBuffer.Unlock(pDSBuffData, dwDataLen, nil, 0);
  if Result <> DS_OK then Exit;
  if QueryPerformanceFrequency(Int64(counterFrequency)) then
  begin
    framesInBuffer := bytesPerBuffer div (nChannels * bps div 8);
    dsw.dsw_CounterTicksPerBuffer.QuadPart := (counterFrequency.QuadPart * framesInBuffer) div nFrameRate;
//        AddTraceMessage("dsw_CounterTicksPerBuffer = %d\n", dsw->dsw_CounterTicksPerBuffer.LowPart );
  end
  else begin
    dsw.dsw_CounterTicksPerBuffer.QuadPart := 0;
  end;
    // Let DSound set the starting write position because if we set it to zero, it looks like the
    // buffer is full to begin with. This causes a long pause before sound starts when using large buffers.
  hr := dsw.dsw_OutputBuffer.GetCurrentPosition(@playCursor, @(dsw.dsw_WriteOffset));
  if( hr <> DS_OK ) then
  begin
    Result := hr;
    Exit;
  end;
  dsw.dsw_FramesWritten := dsw.dsw_WriteOffset/dsw.dsw_BytesPerFrame;
    ///* printf("DSW_InitOutputBuffer: playCursor = %d, writeCursor = %d\n", playCursor, dsw->dsw_WriteOffset ); */
  Result := DS_OK;
end;

function DSW_InitOutputBufferEx(var dsw  : DSoundWrapper; Wnd : HWND; var WaveFormat : TWaveFormatExtensible; bytesPerBuffer : LongWord) : HRESULT;
var
  dwDataLen : DWORD;
  playCursor : DWORD;
  _hWnd : HWND;
  hr : HRESULT;
  primaryDesc : TDSBufferDesc;
  secondaryDesc : TDSBufferDesc;
  pDSBuffData : PByte;
  counterFrequency : LARGE_INTEGER;
  framesInBuffer : Integer;
begin
  dsw.dsw_OutputSize := bytesPerBuffer;
  dsw.dsw_OutputRunning := FALSE;
  dsw.dsw_OutputUnderflows := 0;
  dsw.dsw_FramesWritten := 0;
  dsw.dsw_BytesPerFrame := WaveFormat.Format.nBlockAlign;
  // We were using getForegroundWindow() but sometimes the ForegroundWindow may not be the
  // applications's window. Also if that window is closed before the Buffer is closed
  // then DirectSound can crash. (Thanks for Scott Patterson for reporting this.)
  // So we will use GetDesktopWindow() which was suggested by Miller Puckette.
  // hWnd = GetForegroundWindow();
  _hWnd := Wnd;
  if _hWnd = 0 then
    _hWnd := GetDesktopWindow();
  // Set cooperative level to DSSCL_EXCLUSIVE so that we can get 16 bit output, 44.1 KHz.
  // Exclusize also prevents unexpected sounds from other apps during a performance.
  hr := dsw.dsw_pDirectSound.SetCooperativeLevel(_hWnd, DSSCL_EXCLUSIVE);
  if hr <> DS_OK then
  begin
    Result := hr;
    Exit;
  end;
  // -----------------------------------------------------------------------
  // Create primary buffer and set format just so we can specify our custom format.
  // Otherwise we would be stuck with the default which might be 8 bit or 22050 Hz.
  // Setup the primary buffer description
  FillChar(primaryDesc, sizeof(TDSBUFFERDESC), 0);
  primaryDesc.dwSize := sizeof(DSBUFFERDESC);
  primaryDesc.dwFlags := DSBCAPS_PRIMARYBUFFER; // all panning, mixing, etc done by synth
  primaryDesc.dwBufferBytes := 0;
  primaryDesc.lpwfxFormat := nil; //PWaveFormatEx(@WaveFormat);
  // Create the buffer
  Result := dsw.dsw_pDirectSound.CreateSoundBuffer(
        primaryDesc, dsw.dsw_PrimaryBuffer, nil);  // ATTENTION
  if (Result <> DS_OK) then Exit;
//  Set the primary buffer's format
Result := dsw.dsw_PrimaryBuffer.SetFormat(PWaveFormatEx(@WaveFormat));
  if (Result <> DS_OK) then Exit;
    // ----------------------------------------------------------------------
    // Setup the secondary buffer description
  ZeroMemory(@secondaryDesc, sizeof(TDSBUFFERDESC));
  secondaryDesc.dwSize := sizeof(TDSBUFFERDESC);
  secondaryDesc.dwFlags :=  DSBCAPS_CTRLPAN or DSBCAPS_CTRLFREQUENCY or DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_CTRLVOLUME;
  secondaryDesc.dwBufferBytes := bytesPerBuffer;
  secondaryDesc.lpwfxFormat := PWaveFormatEx(@WaveFormat);
    // Create the secondary buffer
  Result := dsw.dsw_pDirectSound.CreateSoundBuffer(secondaryDesc, dsw.dsw_OutputBuffer, nil);
  if Result <> DS_OK then Exit;
    // Lock the DS buffer

  Result := dsw.dsw_OutputBuffer.Lock(0, dsw.dsw_OutputSize, @pDSBuffData,
            @dwDataLen, nil, nil, 0);
  if Result <> DS_OK then Exit;
    // Zero the DS buffer
  ZeroMemory(pDSBuffData, dwDataLen);
    // Unlock the DS buffer
  Result := dsw.dsw_OutputBuffer.Unlock(pDSBuffData, dwDataLen, nil, 0);
  if Result <> DS_OK then Exit;
  if QueryPerformanceFrequency(Int64(counterFrequency)) then
  begin
    framesInBuffer := bytesPerBuffer div WaveFormat.Format.nBlockAlign;
    dsw.dsw_CounterTicksPerBuffer.QuadPart := (counterFrequency.QuadPart * framesInBuffer) div WaveFormat.Format.nSamplesPerSec;
//        AddTraceMessage("dsw_CounterTicksPerBuffer = %d\n", dsw->dsw_CounterTicksPerBuffer.LowPart );
  end
  else begin
    dsw.dsw_CounterTicksPerBuffer.QuadPart := 0;
  end;
    // Let DSound set the starting write position because if we set it to zero, it looks like the
    // buffer is full to begin with. This causes a long pause before sound starts when using large buffers.
  hr := dsw.dsw_OutputBuffer.GetCurrentPosition(@playCursor, @(dsw.dsw_WriteOffset));
  if( hr <> DS_OK ) then
  begin
    Result := hr;
    Exit;
  end;
  dsw.dsw_FramesWritten := dsw.dsw_WriteOffset/dsw.dsw_BytesPerFrame;
    ///* printf("DSW_InitOutputBuffer: playCursor = %d, writeCursor = %d\n", playCursor, dsw->dsw_WriteOffset ); */
  Result := DS_OK;
end;


(*
function XDSW_InitPrimaryBuffer(var dsw : XDSoundWrapper; Device : PGUID; Wnd : HWND; DSLevel : LongWord) : HRESULT;
var
  primaryDesc : TDSBufferDesc;
  _hWnd : HWND;
  hr : HResult;
begin
  Result := DirectSoundCreate(Device, dsw.dsw_pDirectSound, nil);
  if hr <> DS_OK then
  begin
    Result := hr;
    Exit;
  end;
  _hWnd := Wnd;
  if _hWnd = 0 then
    _hWnd := GetDesktopWindow();
  hr := dsw.dsw_pDirectSound.SetCooperativeLevel(_hWnd, DSLevel);
  if hr <> DS_OK then
  begin
    Result := hr;
    Exit;
  end;
  FillChar(primaryDesc, sizeof(TDSBUFFERDESC), 0);
  primaryDesc.dwSize := sizeof(DSBUFFERDESC);
  primaryDesc.dwFlags := DSBCAPS_PRIMARYBUFFER; // all panning, mixing, etc done by synth
  primaryDesc.dwBufferBytes := 0;
  primaryDesc.lpwfxFormat := nil; //PWaveFormatEx(@WaveFormat);
  // Create the buffer
   Result := dsw.dsw_pDirectSound.CreateSoundBuffer(
        primaryDesc, dsw.dsw_PrimaryBuffer, nil);  // ATTENTION
end;

function XDSW_SetPrimaryFormat(var dsw : XDSoundWrapper; var WaveFormat : TWaveFormatExtensible) : HRESULT;
begin
  Result := dsw.dsw_PrimaryBuffer.SetFormat(PWaveFormatEx(@WaveFormat));
end;

function XDSW_InitSecondaryBuffer(var dsw : XDSoundWrapper; bytesPerBuffer : LongWord; var WaveFormat : TWaveFormatExtensible) : HRESULT;
var
  dwDataLen : DWORD;
  playCursor : DWORD;
  hr : HRESULT;
  secondaryDesc : TDSBufferDesc;
  pDSBuffData : PByte;
begin
  dsw.dsw_OutputSize := bytesPerBuffer;
  dsw.dsw_OutputUnderflows := 0;
  dsw.dsw_BytesPerFrame := WaveFormat.Format.nBlockAlign;

  ZeroMemory(@secondaryDesc, sizeof(TDSBUFFERDESC));
  secondaryDesc.dwSize := sizeof(TDSBUFFERDESC);
  secondaryDesc.dwFlags :=  DSBCAPS_CTRLPAN or DSBCAPS_CTRLFREQUENCY or DSBCAPS_GLOBALFOCUS or DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_CTRLVOLUME;
  secondaryDesc.dwBufferBytes := bytesPerBuffer;
  secondaryDesc.lpwfxFormat := PWaveFormatEx(@WaveFormat);
    // Create the secondary buffer
  Result := dsw.dsw_pDirectSound.CreateSoundBuffer(secondaryDesc, dsw.dsw_OutputBuffer, nil);
  if Result <> DS_OK then Exit;
    // Lock the DS buffer

  Result := dsw.dsw_OutputBuffer.Lock(0, dsw.dsw_OutputSize, @pDSBuffData,
            @dwDataLen, nil, nil, 0);
  if Result <> DS_OK then Exit;
    // Zero the DS buffer
  ZeroMemory(pDSBuffData, dwDataLen);
    // Unlock the DS buffer
  Result := dsw.dsw_OutputBuffer.Unlock(pDSBuffData, dwDataLen, nil, 0);
  if Result <> DS_OK then Exit;
  Result := DS_OK;
end;
*)

function DSW_StartOutput(var dsw : DSoundWrapper) : HRESULT;
begin
  QueryPerformanceCounter(Int64(dsw.dsw_LastPlayTime));
  dsw.dsw_LastPlayCursor := 0;
  dsw.dsw_FramesPlayed := 0;
  Result := dsw.dsw_OutputBuffer.SetCurrentPosition(0);
  if Result <> DS_OK then Exit;
    // Start the buffer playback in a loop.
  if Assigned(dsw.dsw_OutputBuffer) then
  begin
    Result := dsw.dsw_OutputBuffer.Play(0, 0, DSBPLAY_LOOPING );
    if Result <> DS_OK then Exit;
    dsw.dsw_OutputRunning := TRUE;
  end;
  Result := 0;
end;

function DSW_StopOutput(var dsw : DSoundWrapper): HRESULT;
begin
    // Stop the buffer playback
  if dsw.dsw_OutputBuffer <> nil then
  begin
    dsw.dsw_OutputRunning := FALSE;
    Result := dsw.dsw_OutputBuffer.Stop;
  end
  else Result := 0;
end;

function DSW_RestartOutput(var dsw : DSoundWrapper) : HRESULT;
begin
  if dsw.dsw_OutputBuffer <> nil then
  begin
    Result := dsw.dsw_OutputBuffer.Play(0, 0, DSBPLAY_LOOPING);
    if Result = DS_OK then
    begin
      dsw.dsw_OutputRunning := TRUE;
      Result := 0;
      Exit;
    end;
    Exit;
  end;
  Result := -1;
end;

function DSW_QueryOutputSpace(var dsw : DSoundWrapper; var bytesEmpty : LongWord) : HRESULT;
var
  //hr : HRESULT;
  playCursor :  DWORD;
  writeCursor : DWORD;
  numBytesEmpty :  long;
  playWriteGap :  long;
  currentTime : LARGE_INTEGER;
  elapsedTime : LARGE_INTEGER;
  bytesPlayed : long;
  bytesExpected : long;
  buffersWrapped : long;

begin
    // Query to see how much room is in buffer.
    // Note: Even though writeCursor is not used, it must be passed to prevent DirectSound from dieing
    // under WinNT. The Microsoft documentation says we can pass NULL but apparently not.
    // Thanks to Max Rheiner for the fix.
  Result := dsw.dsw_OutputBuffer.GetCurrentPosition(@playCursor, @writeCursor);
  if Result <> DS_OK then Exit;
  //   AddTraceMessage("playCursor", playCursor);
  //    AddTraceMessage("dsw_WriteOffset", dsw->dsw_WriteOffset);
  // Determine size of gap between playIndex and WriteIndex that we cannot write into.
  playWriteGap := writeCursor - playCursor;
  if playWriteGap < 0 then
    playWriteGap := playWriteGap + dsw.dsw_OutputSize; // unwrap
    //* DirectSound doesn't have a large enough playCursor so we cannot detect wrap-around. */
    //* Attempt to detect playCursor wrap-around and correct it. */
  if dsw.dsw_OutputRunning and (dsw.dsw_CounterTicksPerBuffer.QuadPart <> 0) then
  begin
        //* How much time has elapsed since last check. */
    QueryPerformanceCounter(Int64(currentTime));
    elapsedTime.QuadPart := currentTime.QuadPart - dsw.dsw_LastPlayTime.QuadPart;
    dsw.dsw_LastPlayTime := currentTime;
        //* How many bytes does DirectSound say have been played. */
    bytesPlayed := playCursor - dsw.dsw_LastPlayCursor;
    if bytesPlayed < 0 then
        bytesPlayed := bytesPlayed + dsw.dsw_OutputSize; // unwrap
    dsw.dsw_LastPlayCursor := playCursor;
        //* Calculate how many bytes we would have expected to been played by now. */
    bytesExpected := (elapsedTime.QuadPart * dsw.dsw_OutputSize) div dsw.dsw_CounterTicksPerBuffer.QuadPart;
    buffersWrapped := (bytesExpected - bytesPlayed) div dsw.dsw_OutputSize;
    if buffersWrapped > 0  then
    begin
      playCursor := playCursor + (buffersWrapped * dsw.dsw_OutputSize);
      bytesPlayed := bytesPlayed + (buffersWrapped * dsw.dsw_OutputSize);
    end;
        //* Maintain frame output cursor. */
    dsw.dsw_FramesPlayed := dsw.dsw_FramesPlayed + (bytesPlayed div dsw.dsw_BytesPerFrame);
  end;
  numBytesEmpty := playCursor - dsw.dsw_WriteOffset;
  if numBytesEmpty < 0 then
    numBytesEmpty := numBytesEmpty + dsw.dsw_OutputSize; // unwrap offset
    ///* Have we underflowed? */
  if numBytesEmpty > (dsw.dsw_OutputSize - playWriteGap) then
  begin
    if dsw.dsw_OutputRunning then
    begin
      Inc(dsw.dsw_OutputUnderflows);
                //            AddTraceMessage("underflow detected! numBytesEmpty", numBytesEmpty );
    end;
    dsw.dsw_WriteOffset := writeCursor;
    numBytesEmpty := dsw.dsw_OutputSize - playWriteGap;
  end;
  bytesEmpty := numBytesEmpty;
end;

// Note by A.B.
// DSW_FillEmptySpace replaces the original DSW_ZeroEmptySpace
// because 'zero' for 8-bit sound is 128 actually.

function DSW_FillEmptySpace(var dsw : DSoundWrapper; Fill : Byte) : HRESULT;
var
//   hr : HRESULT;
  lpbuf1 : PBYTE;
  lpbuf2 : PBYTE;
  dwsize1 : DWORD;
  dwsize2 : DWORD;
  bytesEmpty : LongWord;
begin
  Result := DSW_QueryOutputSpace(dsw, bytesEmpty); // updates dsw_FramesPlayed
  if Result <> DS_OK then Exit;
  if bytesEmpty = 0 then
  begin
    Result := DS_OK;
    Exit;
  end;
    // Lock free space in the DS
  Result := dsw.dsw_OutputBuffer.Lock(dsw.dsw_WriteOffset, bytesEmpty, @lpbuf1,
            @dwsize1, @lpbuf2, @dwsize2, 0);
  if Result = DS_OK then
  begin
     // Copy the buffer into the DS
    FillMemory(lpbuf1, dwsize1, Fill);
    if lpbuf2 <> nil then
      FillMemory(lpbuf2, dwsize2, Fill);
      // Update our buffer offset and unlock sound buffer
    dsw.dsw_WriteOffset := (dsw.dsw_WriteOffset + dwsize1 + dwsize2) mod dsw.dsw_OutputSize;
    dsw.dsw_OutputBuffer.Unlock(lpbuf1, dwsize1, lpbuf2, dwsize2);
    dsw.dsw_FramesWritten := dsw.dsw_FramesWritten + (bytesEmpty div dsw.dsw_BytesPerFrame);
  end;
end;

function DSW_WriteBlock(var dsw : DSoundWrapper; buf : PByte; numBytes : long) : HRESULT;
var
  lpbuf1 : PBYTE;
  lpbuf2 : PBYTE;
  dwsize1 : DWORD;
  dwsize2 : DWORD;
  tmpbuf : PByte;
begin
  // Lock free space in the DS
  Result := dsw.dsw_OutputBuffer.Lock(dsw.dsw_WriteOffset, numBytes, @lpbuf1,
                @dwsize1, @lpbuf2, @dwsize2, 0);
  if Result = DS_OK then
  begin
    // Copy the buffer into the DS
    Move(buf^, lpbuf1^, dwsize1);
    if lpbuf2 <> nil then
    begin
      tmpbuf := buf;
      Inc(tmpbuf, dwsize1);
      Move(tmpbuf^, lpbuf2^, dwsize2);
    end;
        // Update our buffer offset and unlock sound buffer
    dsw.dsw_WriteOffset := (dsw.dsw_WriteOffset + dwsize1 + dwsize2) mod dsw.dsw_OutputSize;
    dsw.dsw_OutputBuffer.Unlock(lpbuf1, dwsize1, lpbuf2, dwsize2);
    dsw.dsw_FramesWritten := dsw.dsw_FramesWritten + (numBytes div dsw.dsw_BytesPerFrame);
  end;
end;

function DSW_GetOutputStatus(var dsw : DSoundWrapper) : DWORD;
begin
    if dsw.dsw_OutputBuffer.GetStatus(Result) <> DS_OK then
        Result := DWORD(DSERR_INVALIDPARAM);
end;

function DSEnumOutputCallback(lpGuid : PGUID;
                              {$IFDEF UNICODE}
                              lpcstrDescription : PWideChar;
                              {$ENDIF}
                              {$IFNDEF UNICODE}
                              lpcstrDescription : PChar;
                              {$ENDIF}
                              lpcstrModule : PChar; lpContext : Pointer) : BOOL; stdcall;
var
  l : Integer;
  devices : PDSW_Devices; // = (DSW_Devices *) lpContext;
begin
  devices := lpContext;
  if devices.devcount > 15 then
  begin
    Result := FALSE;
    Exit;
  end;
  if lpGuid <> nil then
    Move(lpGuid^, devices.dinfo[devices.devcount].guid, sizeof(TGUID))
  else
    FillChar(devices.dinfo[devices.devcount].guid, sizeof(TGUID), 0);
  {$IFDEF UNICODE}
  l := strlen(lpcstrDescription)*2;
  if l > 255 then l := 255;
  {$ENDIF}
  {$IFNDEF UNICODE}
  l := strlen(lpcstrDescription);
  if l > 127 then l := 127;
  {$ENDIF}
  Move(lpcstrDescription[0], devices.dinfo[devices.devcount].name, l);
  devices.dinfo[devices.devcount].name[l] := Char(0);
  Inc(devices.devcount);
  Result := TRUE;
end;

function DSW_EnumerateOutputDevices(devices : PDSW_Devices) : HRESULT;
begin
  devices.devcount := 0;
  Result := DirectSoundEnumerate(DSEnumOutputCallback, devices);
end;


function DSW_EnumerateInputDevices(devices : PDSW_Devices) : HRESULT;
begin
  devices.devcount := 0;
  Result := DirectSoundCaptureEnumerate(DSEnumOutputCallback, devices);
end;


function DSW_InitInputDevice(var dsw : DSoundWrapper; lpGUID : PGUID) : HRESULT;
begin
  Result := DirectSoundCaptureCreate(lpGUID, dsw.dsw_pDirectSoundCapture, nil);
end;

function DSW_InitInputBuffer(var dsw : DSoundWrapper; bps : Integer;
                nFrameRate : LongWord; nChannels, bufSize : Integer) : HRESULT;
var
  wfFormat : TWaveFormatEx;
  captureDesc : TDSCBufferDesc;
begin
  // Define the buffer format
  wfFormat.wFormatTag      := WAVE_FORMAT_PCM;
  wfFormat.nChannels       := nChannels;
  wfFormat.nSamplesPerSec  := nFrameRate;
  wfFormat.wBitsPerSample  := bps;
  wfFormat.nBlockAlign     := wfFormat.nChannels * (wfFormat.wBitsPerSample div 8);
  wfFormat.nAvgBytesPerSec := wfFormat.nSamplesPerSec * wfFormat.nBlockAlign;
  wfFormat.cbSize          := 0;   //* No extended format info. */
  dsw.dsw_InputSize := bufSize;
  // ----------------------------------------------------------------------
  // Setup the secondary buffer description
  ZeroMemory(@captureDesc, sizeof(TDSCBUFFERDESC));
  captureDesc.dwSize := sizeof(TDSCBUFFERDESC);
  captureDesc.dwFlags :=  0;
  captureDesc.dwBufferBytes := bufSize;
  captureDesc.lpwfxFormat := @wfFormat;
  // Create the capture buffer
  Result :=  dsw.dsw_pDirectSoundCapture.CreateCaptureBuffer(captureDesc, dsw.dsw_InputBuffer, nil);
  if Result <> DS_OK then Exit;
  dsw.dsw_ReadOffset := 0;  // reset last read position to start of buffer
  Result := DS_OK;
end;

function DSW_StartInput(var dsw : DSoundWrapper) : HRESULT;
begin
  // Start the buffer playback
  if dsw.dsw_InputBuffer <> nil then
    Result := dsw.dsw_InputBuffer.Start(DSCBSTART_LOOPING)
  else Result := 0;
end;

function DSW_StopInput(var dsw : DSoundWrapper) : HRESULT;
begin
  // Stop the buffer playback
  if dsw.dsw_InputBuffer <> nil  then
     Result := dsw.dsw_InputBuffer.Stop
  else Result := 0;
end;

function DSW_QueryInputFilled(var dsw : DSoundWrapper; var bytesFilled : long) : HRESULT;
var
  capturePos : DWORD;
  readPos : DWORD;
  filled : long;
begin
    // Query to see how much data is in buffer.
    // We don't need the capture position but sometimes DirectSound doesn't handle NULLS correctly
    // so let's pass a pointer just to be safe.
  Result := dsw.dsw_InputBuffer.GetCurrentPosition(@capturePos, @readPos);
  if Result <> DS_OK  then Exit;
  filled := readPos - dsw.dsw_ReadOffset;
  if filled < 0  then
     Inc(filled, dsw.dsw_InputSize); // unwrap offset
  bytesFilled := filled;
end;

function DSW_ReadBlock(var dsw : DSoundWrapper; buf : PByte; var numBytes : long) : HRESULT;
var
  lpbuf1 : PBYTE;
  lpbuf2 : PBYTE;
  dwsize1 : DWORD;
  dwsize2 : DWORD;
  tmpbuf : PByte;
begin
    // Lock free space in the DS
  Result := dsw.dsw_InputBuffer.Lock(dsw.dsw_ReadOffset, numBytes, @lpbuf1, @dwsize1,
              @lpbuf2, @dwsize2, 0);
  if Result = DS_OK then
  begin
    // Copy from DS to the buffer
    CopyMemory(buf, lpbuf1, dwsize1);
    if lpbuf2 <> nil then
    begin
      tmpbuf := buf;
      Inc(tmpbuf, dwsize1);
      CopyMemory(tmpbuf, lpbuf2, dwsize2);
    end;
    // Update our buffer offset and unlock sound buffer
    dsw.dsw_ReadOffset := (dsw.dsw_ReadOffset + dwsize1 + dwsize2) mod dsw.dsw_InputSize;
    dsw.dsw_InputBuffer.Unlock(lpbuf1, dwsize1, lpbuf2, dwsize2);
  end;
  numBytes := dwsize1 + dwsize2;
end;

function DSW_GetVolume(var dsw : DSoundWrapper; out Volume: Longint) : HRESULT;
begin
  if Assigned(dsw.dsw_OutputBuffer) then
    Result := dsw.dsw_OutputBuffer.GetVolume(Volume)
  else
  begin
    Volume := 0;
    Result := S_OK;
  end;
end;

function DSW_SetVolume(var dsw : DSoundWrapper; Volume: Longint) : HRESULT;
begin
  if Assigned(dsw.dsw_OutputBuffer) then
    Result := dsw.dsw_OutputBuffer.SetVolume(Volume)
  else
    Result := S_OK;
  if Result = DSERR_CONTROLUNAVAIL then
    raise Exception.Create('Control not available');
end;

  function DSW_FlushOutputBuffer(var dsw : DSoundWrapper; BytesToLeave : Integer) : HRESULT;
  var
    PlayCursor, WriteCursor : DWORD;
    delta : Integer;
  begin
    dsw.dsw_OutputBuffer.GetCurrentPosition(@PlayCursor, @WriteCursor);
    delta := WriteCursor - PlayCursor;
    if delta >= 0 then
    begin
      if delta > BytesToLeave then
      begin
        Inc(PlayCursor, delta - BytesToLeave);
        Result := dsw.dsw_OutputBuffer.SetCurrentPosition(PlayCursor);
      end else
      Result := S_OK;
    end else
    begin
      delta := dsw.dsw_OutputSize + delta;
      if delta > BytesToLeave then
      begin
        if (dsw.dsw_OutputSize - PlayCursor) > (delta - BytesToLeave) then
        begin
          Inc(PlayCursor, delta - BytesToLeave);
          Result := dsw.dsw_OutputBuffer.SetCurrentPosition(PlayCursor);
        end else
        begin
          PlayCursor := (delta - BytesToLeave) - (dsw.dsw_OutputSize - PlayCursor);
          Result := dsw.dsw_OutputBuffer.SetCurrentPosition(PlayCursor);
        end;
      end else Result := S_OK;
    end;
  end;

{$WARNINGS ON}
end.
