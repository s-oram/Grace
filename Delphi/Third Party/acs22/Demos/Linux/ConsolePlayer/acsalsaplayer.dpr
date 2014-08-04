(*
 ACS Demo Console player/converter.
 (c) Andrei Borovsky.
 Calling the program with the file name as a sole parameter makes it to
 play the file via ALSA driver.
 -w switch tells the propgram to convert input audio file
 into wav file with the name specified.
 This demo shows how to use ACS components in console applications.
*)

program acsalsaplayer;

{$APPTYPE CONSOLE}

uses
  Libc, SysUtils,
  Classes,
  ACS_Classes,
  ACS_ALSA,
  ACS_Wave,
  ACS_Vorbis,
  ACS_FLAC;

var

  pid : pid_t;

type

  TDummie = class(TObject)
  public
     procedure OnProgress(Sender : TComponent);
     procedure OnDone(Sender : TComponent);
     procedure OnThreadException(Sender : TComponent; E: Exception);
  end;

  procedure TDummie.OnProgress;
  var
    vOutput : TACSOutput;
  begin
    vOutput := Sender as TACSOutput;
    if vOutput.Progress mod 10 = 0 then Write('*');
  end;

  procedure TDummie.OnDone;
  begin
    WriteLn;
    (*
     We use kill and not __raise because this procedure
     is to be called from the output thread context while
     the signal should be caught within the main thread context.
    *)
    kill(pid, SIGUSR1);
  end;

  procedure TDummie.OnThreadException;
  begin
    WriteLn(E.Message);
  end;

var

  AO : TALSAAudioOut;
  WI : TWaveIn;
  WO : TWaveOut;
  VI : TVorbisIn;
  FI : TFLACIn;
  aOutput : TACSOutput;
  aInput : TACSFileIn;
  Dummie : TDummie;
  S, FN : String;
  i : Integer;

  SigSet : __sigset_t;

  procedure SIGUSR1Proc(SigNum : Integer); cdecl;
  begin
    // Doing nothing
  end;

begin
  WriteLn('ACS Demo Audio Player/Converter');

  if ParamCount = 0 then
  begin
    S := ParamStr(0);
    S := Format('usage: %s [-w] <input_file> [output_wav_file]', [S]);
    WriteLn(S);
    Halt(0);
  end;
  Dummie := TDummie.Create;
  if ParamStr(1) = '-w' then
  begin
    if ParamCount <> 3 then
    begin
      WriteLn('No output file specified');
      Halt(1);
    end;
    FN := ParamStr(2);
    WO := TWaveOut.Create(nil);
    aOutput := WO;
    S := ParamStr(3);
    if ExtractFileExt(S) <> '.wav' then S := S+'.wav';
    WO.FileName := S;
  end else
  begin
    FN := ParamStr(1);
    AO := TALSAAudioOut.Create(nil);
    aOutput := AO;
  end;
  S := ExtractFileExt(FN);
  if S = '.wav' then
  begin
    WI := TWaveIn.Create(nil);
    WI.FileName := FN;
    aInput := WI;
  end;
  if S = '.ogg' then
  begin
    VI := TVorbisIn.Create(nil);
    VI.FileName := FN;
    aInput := VI;
  end;
  if S = '.flac' then
  begin
    FI := TFLACIn.Create(nil);
    FI.FileName := FN;
    aInput := FI;
  end;
  if not aInput.Valid then
  begin
    S := Format('%s is not a valid audio file', [FN]);
    WriteLn(S);
    Halt(1);
  end;
  pid := getpid; // see the note above
  aOutput.OnDone := Dummie.OnDone;
  aOutput.OnProgress := Dummie.OnProgress;
  aOutput.OnThreadException := Dummie.OnThreadException;
  aOutput.Input := aInput;
  aOutput.Run;
  signal(SIGUSR1, SIGUSR1Proc);
  sigfillset(SigSet);
  sigwait(SigSet, @i); // waiting for any signal to continue
  aOutput.Free;
  aInput.Free;
  Dummie.Free;
  WriteLn('Done');
end.
