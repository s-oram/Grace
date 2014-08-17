(*
  This file is a part of New Audio Components package 2.0
  Copyright (c) 2002-2009, Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at anb@symmetrica.net
*)

(* $Id: AuSynch.pas 1117 2010-01-22 09:28:59Z andrei.borovsky $ *)

unit AuSynch;

(* Title: AuSynch
    Components for real time audio processing *)

interface

uses
   Windows, MMSystem, SysUtils, Classes, ACS_Classes, ACS_Procs;

type

 THiResTimerEvent = procedure(Sender : TComponent) of object;

  (* Class: TAudioHiResTimer
     This component encapsulate high-resolution multimedia timer.
   *)

 TAudioHiResTimer = class(TComponent)
 private
   FResolution : Word;
   FInterval : Word;
   FFireOnce : Boolean;
   FTimerEvent : THiResTimerEvent;
   FID : THandle;
   FTimeStart : LongWord;
   function GetTimeElapsed : LongWord;
 public
   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   (* Function: Start
      Starts the timer. *)
   procedure Start;
   (* Function: Kill
      Kills the timer. *)
   procedure Kill;
   (* Property: TimeElapsed
      The time elapsed since the timer started in milliseconds. *)
   property TimeElapsed : LongWord read GetTimeElapsed;
 published
   (* Property: Interval
      The interval between timer events in milliseconds.  *)
   property Interval : Word read FInterval write FInterval;
   (* Property: Resolution
      The timer resolution (accuracy) in milliseconds. Zero means the maximum accuracy (and the stress on the system).  *)
   property Resolution : Word read FResolution write FResolution;
   (* Property: FireOnce
      This property determines if the <OnTimer> event should occur once or regularly.  *)
   property FireOnce : Boolean read FFireOnce write FFireOnce;
   (* Property: OnTimer
      This event is fired when the <Interval> has passed. *)
   property OnTimer : THiResTimerEvent read FTimerEvent write FTimerEvent;
 end;

  (* Class: TAudioSynchronizer
     A descendent of <TAuConverter>.
     This component tries to keep in synch the audio playback phisical time and the number of samples played.
     This component is good at eliminating the kind of delays where data stops coming in for a while and then rushes in.
   *)

  TAudioSynchronizer = class(TAuConverter)
  private
    FTimeStart : LongWord;
    FThreshold :  Word;
    FLatency : Word;
    FFrameSize : Word;
    Delta : ShortInt;
    FFramesCount : Double;
    _Buffer : Pointer;
    FDeltaTime : Integer;
    FirstCall : Boolean;
  protected
    procedure GetDataInternal(var Buffer : Pointer; var Bytes : LongWord); override;
    procedure InitInternal; override;
    procedure FlushInternal; override;
  public
    procedure _Pause; override;
    procedure _Resume; override;
    constructor Create(AOwner: TComponent); override;
    (* Function: ResetSync
       Resets the synchronizer. *)
    procedure ResetSync;
    (* Property: DeltaTime
      The current difference between the physical time and the "playback time" - the number of samples passed in milliseconds. *)
    property DeltaTime : Integer read FDeltaTime;
  published
    (* Property: Latency
      While TAudioSynchronizer tries to eliminate the lag between audio data coming in and the time of playback,
      there is always some inevitable latency which the component should not try to eliminate.
      Use this property to set this latency value in milliseconds. You may need to increase this value during the audio operation in case  the data is lost for example
      (the other way to deal with data dropped is to <Reset> the synchronizer).
      The Latency value should always be less than that of <Threshold>.
      See i-Radio demo on how operate <Latency> *)
    property Latency : Word read FLatency write FLatency;
    (* Property: Threshold
      The maximum tolerable latency value in milliseconds. This value should always be greater than the value of <Latency>.
      See i-Radio demo on how operate <Latency> *)
    property Threshold :  Word read FThreshold write FThreshold;
  end;


implementation

const
  TIME_KILL_SYNCHRONOUS = $0100;

type
TTimeProc = procedure(uID, uMsg : UINT; dwUser, dw1, dw2 : DWORD); stdcall;

function timeSetEvent(uDelay, uResolution : UINT; LPTIMECALLBACK : TTimeProc; dwUser : Pointer; fuEvent : UINT) : MMResult; stdcall; external 'winmm.dll' name 'timeSetEvent';
function timeKillEvent(uTimerID : UINT) : MMResult; stdcall; external 'winmm.dll' name 'timeKillEvent';

  procedure CBTimerProc(uID, uMsg : UINT; dwUser, dw1, dw2 : DWORD); stdcall;
  var
    Timer : TAudioHiResTimer;
  begin
    Timer := TAudioHiResTimer(dwUser);
    if Assigned(Timer.FTimerEvent) then
      Timer.FTimerEvent(Timer);
  end;

 constructor TAudioHiResTimer.Create(AOwner: TComponent);
 begin
  inherited;
  FID := 0;
 end;

 destructor TAudioHiResTimer.Destroy;
 begin
   if FID <> 0 then
      timeKillEvent(FID);
   inherited;
 end;

 procedure TAudioHiResTimer.Start;
 var
   Flags : LongWord;
 begin
   if FID <> 0 then
      timeKillEvent(FID);
   FTimeStart := timeGetTime;
   if FFireOnce then Flags := TIME_ONESHOT
   else Flags := TIME_PERIODIC;
   Flags := Flags or TIME_KILL_SYNCHRONOUS;
   FID := timeSetEvent(FInterval, FResolution, CBTimerProc, Self, Flags);
 end;

 procedure TAudioHiResTimer.Kill;
 begin
   if FID <> 0 then
      timeKillEvent(FID);
   FID := 0;
 end;

 function TAudioHiResTimer.GetTimeElapsed : LongWord;
 begin
   Result := timeGetTime - FTimeStart;
 end;

 constructor TAudioSynchronizer.Create(AOwner: TComponent);
 begin
   inherited;
   FThreshold := 100;
   FLatency := 50;
 end;

 procedure TAudioSynchronizer.InitInternal;
 begin
   if not Assigned(FInput) then
     raise EAuException.Create('Input not assigned');
   FInput.Init;
   Busy := True;
   FSize := -1;
   FFrameSize := FInput.Channels*FInput.BitsPerSample div 8;
   GetMem(_Buffer, FFrameSize*32);
   FTimeStart := timeGetTime;
   FFramesCount := 0;
   Delta := 0;
   FirstCall := True;
 end;

 procedure TAudioSynchronizer.GetDataInternal(var Buffer: Pointer; var Bytes: Cardinal);
 var
   TimeFix, TimeReal, FramesReq, RealBytes : LongWord;
   EOF : Boolean;
 begin
   if FirstCall then
   begin
     FTimeStart := timeGetTime;
     FFramesCount := 0;
     Delta := 0;
     FirstCall := False;
   end;
   TimeFix := Round(FFramesCount/Finput.SampleRate*1000);
   TimeReal := timeGetTime - FTimeStart;
   FramesReq := Bytes div FFrameSize;
   FDeltaTime := Integer(TimeReal - TimeFix);
   if FDeltaTime < -FThreshold then
   Delta := -1;
   if FDeltaTime > FThreshold then
   Delta := 1;
   if Abs(FDeltaTime) <= FLatency then
     Delta := 0;
   if Delta < 0 then
   begin
      if FramesReq > 32 then
      begin
        RealBytes := FFrameSize * 30;
        RealBytes := Finput.FillBuffer(_Buffer, RealBytes, EOF);
        Bytes := RealBytes + FFrameSize*2;
        Buffer := _Buffer
      end else
      if FramesReq > 16 then
      begin
        RealBytes := FFrameSize * 15;
        RealBytes := Finput.FillBuffer(_Buffer, RealBytes, EOF);
        Bytes := RealBytes + FFrameSize;
        Buffer := _Buffer
      end else
      begin
        FInput.GetData(Buffer, Bytes);
        RealBytes := Bytes;
      end;
   end;
   if Delta > 0 then
   begin
      if FramesReq > 32 then
      begin
        RealBytes := FFrameSize * 2;
        FInput.GetData(Buffer, RealBytes);
        Bytes := FFrameSize * 30;
        FInput.GetData(Buffer, Bytes);
        RealBytes := RealBytes + Bytes;
      end else
      if FramesReq > 16 then
      begin
        RealBytes := FFrameSize;
        FInput.GetData(Buffer, RealBytes);
        Bytes := FFrameSize * 15;
        FInput.GetData(Buffer, Bytes);
        RealBytes := RealBytes + Bytes;
      end else
      begin
       FInput.GetData(Buffer, Bytes);
       RealBytes := Bytes;
      end;
   end;
   if Delta = 0 then
   begin
     FInput.GetData(Buffer, Bytes);
     RealBytes := Bytes;
   end;
   FFramesCount := FFramesCount + RealBytes/FFrameSize;
 end;

 procedure TAudioSynchronizer.FlushInternal;
 begin
   FInput.Flush;
   FreeMem(_Buffer);
 end;

 procedure TAudioSynchronizer._Pause;
 begin
   Finput._Pause;
 end;

 procedure TAudioSynchronizer._Resume;
 begin
   FirstCall := True;
   Finput._Resume;
 end;

 procedure TAudioSynchronizer.ResetSync;
 begin
   FirstCall := True;
 end;

end.
