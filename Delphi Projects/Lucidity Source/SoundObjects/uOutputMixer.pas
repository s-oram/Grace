unit uOutputMixer;

interface

{$INCLUDE Defines.inc}

uses
  MoreTypes, eeFunctions;


type
  TOutputMixer = class
  private
    fVoiceMixMain: single;
    fVoiceMixAuxB: single;
    fVoiceMixAuxA: single;
    fModOutA: single;
    fModOutB: single;
    fVoicePan: single;

    procedure SetVoicePan(const Value: single);
  protected
    GainLeft  : single;
    GainRight : single;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModPointer(const Name:string):PSingle;

    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure AudioRateProcess(VoiceX1, VoiceX2:PSingle; const Outputs:TArrayOfPSingle; const SampleFrames:integer); {$IFDEF _AudioInline}inline;{$ENDIF}

    property VoiceMixMain : single read fVoiceMixMain write fVoiceMixMain;
    property VoiceMixAuxA : single read fVoiceMixAuxA write fVoiceMixAuxA;
    property VoiceMixAuxB : single read fVoiceMixAuxB write fVoiceMixAuxB;

    property VoicePan  : single read fVoicePan write SetVoicePan;

    property ModOutA : single read fModOutA write fModOutA;
    property ModOutB : single read fModOutB write fModOutB;
  end;

implementation

uses
  SysUtils;

{ TOuputMixer }

constructor TOutputMixer.Create;
begin
  fModOutA := 0;
  fModOutB := 0;
end;

destructor TOutputMixer.Destroy;
begin

  inherited;
end;

procedure TOutputMixer.FastControlProcess;
begin

end;

function TOutputMixer.GetModPointer(const Name: string): PSingle;
begin
  if Name = 'ModOutA' then Exit(@fModOutA);
  if Name = 'ModOutB' then Exit(@fModOutB);

  raise Exception.Create('ModPointer (' + Name + ') doesn''t exist.');
end;

procedure TOutputMixer.SetVoicePan(const Value: single);
begin
  fVoicePan := Value;
end;

procedure TOutputMixer.SlowControlProcess;
begin

end;

procedure TOutputMixer.AudioRateProcess(VoiceX1, VoiceX2: PSingle; const Outputs: TArrayOfPSingle; const SampleFrames: integer);
const
  kNumberOutputs = 10;
var
  Outs : array[0..kNumberOutputs-1] of PSingle;
  c1: Integer;
begin
  for c1 := 0 to kNumberOutputs-1 do
  begin
    Outs[c1] := Outputs[c1];
  end;

  for c1 := 0 to SampleFrames-1 do
  begin
    Outs[0]^ := Outs[0]^ + VoiceX1^ * fVoiceMixMain;
    Outs[1]^ := Outs[1]^ + VoiceX2^ * fVoiceMixMain;

    //Outs[2]^ := Outs[2]^ + VoiceX1^ * fVoiceMixAuxA;
    //Outs[3]^ := Outs[3]^ + VoiceX2^ * fVoiceMixAuxA;

    //Outs[4]^ := Outs[4]^ + VoiceX1^ * fVoiceMixAuxB;
    //Outs[5]^ := Outs[5]^ + VoiceX2^ * fVoiceMixAuxB;

    Outs[2]^ := Outs[2]^ + fModOutA;
    Outs[3]^ := Outs[3]^ + fModOutA;

    Outs[4]^ := Outs[4]^ + fModOutB;
    Outs[5]^ := Outs[5]^ + fModOutB;

    inc(Outs[0]);
    inc(Outs[1]);
    inc(Outs[2]);
    inc(Outs[3]);
    inc(Outs[4]);
    inc(Outs[5]);

    inc(VoiceX1);
    inc(VoiceX2);
  end;

end;



end.
