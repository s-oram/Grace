(*
  This file is a part of Audio Components Suite v 2.2
  Copyright (c) 2002, 2003 Andrei Borovsky. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at aborovsky@mtu-net.ru
*)

unit ACS_AudioMix;

interface

uses
  Classes, SysUtils, ACS_Types, ACS_Classes;

const

  BUF_SIZE = $4000;

type

  TAudioMixerMode = (amMix, amConcatenate, amRTMix);

  TAudioMixer = class(TACSInput)
  private
    FInput1, FInput2 : TACSInput;
    BufStart, BufEnd : Integer;
    FVolume1, FVolume2 : Byte;
    EndOfInput1, EndOfInput2 : Boolean;
    InBuf1, InBuf2 : array[1..BUF_SIZE] of Byte;
    Buisy : Boolean;
    FMode : TAudioMixerMode;
    FLock : Boolean;
    FFgPlaying : Boolean;
    function GetBPS : Integer; override;
    function GetCh : Integer; override;
    function GetSR : Integer; override;
    procedure SetInput1(aInput : TACSInput);
    procedure SetInput2(aInput : TACSInput);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetData(Buffer : Pointer; BufferSize : Integer): Integer; override;
    procedure Init; override;
    procedure Flush; override;
    property FgPlaying : Boolean read FFgPlaying;
  published
    property Input1 : TACSInput read FInput1 write SetInput1;
    property Input2 : TACSInput read FInput2 write SetInput2;
    property Mode : TAudioMixerMode read FMode write FMode;
    property Volume1 : Byte read FVolume1 write FVolume1;
    property Volume2 : Byte read FVolume2 write FVolume2;
  end;

implementation

procedure MixChannels16(Buf1, Buf2 : PBuffer16; Vol1, Vol2, InSize : Integer);
var
  i : Integer;
begin
  for i := 0 to (Insize shr 1) - 1 do
  Buf2[i] := (Round(Buf1[i]*(Vol1/255)) + Round(Buf2[i]*(Vol2/255))) div 2;
end;

  constructor TAudioMixer.Create;
  begin
    inherited Create(AOwner);
    FVolume1 := 255;
    FVolume2 := 255;
  end;

  destructor TAudioMixer.Destroy;
  begin
    inherited Destroy;
  end;

  function TAudioMixer.GetBPS;
  begin
    if not Assigned(FInput1) then
    raise EACSException.Create('Input not assigned');
    Result := FInput1.BitsPerSample;
  end;

  function TAudioMixer.GetCh;
  begin
    if not Assigned(FInput1) then
    raise EACSException.Create('Input not assigned');
    Result:= FInput1.Channels;
  end;

  function TAudioMixer.GetSR;
  begin
    if not Assigned(FInput1) then
    raise EACSException.Create('Input not assigned');
    Result := FInput1.SampleRate;
  end;

  procedure TAudioMixer.Init;
  begin
    Buisy := True;
    FPosition := 0;
    BufStart := 1;
    BufEnd := 0;
    EndOfInput1 := False;
    EndOfInput2 := False;
    if not Assigned(FInput1) then
    raise EACSException.Create('Input1 not assigned');
    if FMode = amRTMix then
    begin
      FInput1.Init;
      FSize := FInput1.Size;
      if Assigned(FInput2) then
      begin
        FInput2.Init;
        FFgPlaying := True;
      end else EndOfInput2 := True;
      FLock := False;
    end else
    begin
      if not Assigned(FInput2) then
      raise EACSException.Create('Input2 not assigned');
      FInput1.Init;
      FInput2.Init;
      case FMode of
        amMix :
        if FInput1.Size > FInput2.Size then FSize := FInput1.Size
        else FSize := FInput2.Size;
        amConcatenate :
        FSize := FInput1.Size + FInput2.Size;
      end;
    end;
  end;

  procedure TAudioMixer.Flush;
  begin
    FInput1.Flush;
    if (FMode <> amRTMix) or Assigned(FInput2) then
    FInput2.Flush;
    Buisy := False;
  end;

  function TAudioMixer.GetData;
  var
    l1, l2 : Integer;
    InSize : Integer;
  begin
    if not Buisy then  raise EACSException.Create('The Stream is not opened');
    if BufStart > BufEnd then
    begin
      if EndOfInput1 and  EndOfInput2 then
      begin
        Result := 0;
        Exit;
      end;
      if (FMode = amRTMix) and  EndOfInput1 then
      begin
        Result := 0;
        Exit;
      end;
      BufStart := 1;
      case Mode of
        amMix :
        begin
          l1 := 0;
          l2 := 0;
          FillChar(InBuf1[1], BUF_SIZE, 0);
          FillChar(InBuf2[1], BUF_SIZE, 0);
          if not EndOfInput1 then
          begin
            l1 := FInput1.GetData(@InBuf1[1], BUF_SIZE);
            InSize := l1;
            while (InSize <> 0) and (l1 < BUF_SIZE) do
            begin
              InSize := FInput1.GetData(@InBuf1[l1+1], BUF_SIZE - l1);
              Inc(l1, InSize);
            end;
            if InSize = 0 then EndOfInput1 := True;
          end;
          if not EndOfInput2 then
          begin
            l2 := FInput2.GetData(@InBuf2[1], BUF_SIZE);
            InSize := l2;
            while (InSize <> 0) and (l2 < BUF_SIZE) do
            begin
              InSize := FInput2.GetData(@InBuf2[l2+1], BUF_SIZE - l2);
              Inc(l2, InSize);
            end;
            if InSize = 0 then EndOfInput2 := True;
          end;
          if (l1 = 0) and (l2 = 0) then
          begin
            Result := 0;
            Exit;
          end;
          if l1 > l2 then BufEnd := l1 else BufEnd := l2;
          MixChannels16(@InBuf1[1], @InBuf2[1], FVolume1, FVolume2, BufEnd);
        end;
        amConcatenate :
        begin
          if not EndOfInput1 then
          begin
            l1 := FInput1.GetData(@InBuf2[1], BUF_SIZE);
            if l1 = 0 then EndOfInput1 := True
            else BufEnd := l1;
          end;
          if EndOfInput1 then
          begin
            l2 := FInput2.GetData(@InBuf2[1], BUF_SIZE);
            if l2 = 0 then
            begin
              Result := 0;
              Exit;
            end
            else BufEnd := l2;
          end;
        end;
        amRTMix :
        begin
          l1 := 0;
          l2 := 0;
          FillChar(InBuf1[1], BUF_SIZE, 0);
          FillChar(InBuf2[1], BUF_SIZE, 0);
          if not EndOfInput1 then
          begin
            l1 := FInput1.GetData(@InBuf1[1], BUF_SIZE);
            InSize := l1;
            while (InSize <> 0) and (l1 < BUF_SIZE) do
            begin
              InSize := FInput1.GetData(@InBuf1[l1+1], BUF_SIZE - l1);
              Inc(l1, InSize);
            end;
            if InSize = 0 then EndOfInput1 := True;
          end;
          if not (FLock or EndOfInput2) then
          begin
            FLock := True;
            l2 := FInput2.GetData(@InBuf2[1], BUF_SIZE);
            InSize := l2;
            while (InSize <> 0) and (l2 < BUF_SIZE) do
            begin
              InSize := FInput2.GetData(@InBuf2[l2+1], BUF_SIZE - l2);
              Inc(l2, InSize);
            end;
            if InSize = 0 then
            begin
              EndOfInput2 := True;
              FFGPlaying := False;
              FInput2.Flush;
              FInput2 := nil;
            end;
            FLock := False;
          end;
          if (l1 = 0) and (l2 = 0) then
          begin
            Result := 0;
            Exit;
          end;
          if l1 > l2 then BufEnd := l1 else BufEnd := l2;
          MixChannels16(@InBuf1[1], @InBuf2[1], FVolume1, FVolume2, BufEnd);
        end;
      end;
    end;
    if BufferSize < (BufEnd - BufStart + 1)
    then Result := BufferSize
    else Result := BufEnd - BufStart + 1;
    Move(InBuf2[BufStart], Buffer^, Result);
    Inc(BufStart, Result);
    Inc(FPosition, Result);
  end;

  procedure TAudioMixer.SetInput1;
  begin
    if Buisy then
    raise EACSException.Create('The component is buisy.');
    FInput1 := aInput;
  end;

  procedure TAudioMixer.SetInput2;
  begin
    if not Buisy then  FInput2 := aInput
    else
    if FMode = amRTMix then
    begin
      if FFgPlaying then
      begin
        while Flock do;
        FLock := True;
        Input2.Flush;
      end;
      FInput2 := aInput;
      Finput2.Init;
      Flock := False;
      FFgPlaying := True;
      EndOfInput2 := False;
    end else
    raise EACSException.Create('The component is not in amFB mode.');
  end;


end.
