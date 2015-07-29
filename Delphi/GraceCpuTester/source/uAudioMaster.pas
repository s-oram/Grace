unit uAudioMaster;

interface

uses
  DAEffect, DAEffectX;

function AudioMasterCallBack(effect: PAEffect; opcode, index, value: longint; ptr: pointer; opt: Single): longint; cdecl;

function VstDispatch(Effect: PAEffect; Opcode : integer; index: VstInt32 = 0; value: VstIntPtr = 0; ptr: pointer = nil; opt: single = 0): VstIntPtr;


implementation

uses
  VamVst.Extra;

function AudioMasterCallBack(effect: PAEffect; opcode, index, value: Integer; ptr: pointer; opt: Single): longint; cdecl;
var
  s : string;
begin
  //default result. (Usually means the opcode is not supported.
  case Opcode of
    audioMasterVersion:   result := 2400;
    audioMasterWantMidi:  result := 0; // deprecated. Not sure what the response is supposed to be.
    audioMasterIOChanged: result := 1;
  else
    WriteLn('=================================================================');
    WriteLn('WARNING: Opcode not handled.');
    s := VstAudioMasterOpcodeToStr(Opcode);
    WriteLn('Plugin->Host ' + s);
    WriteLn('=================================================================');
    result := 0;
    exit; //=================== exit ========================>>
  end;

  // Opcode has been handled. Log it anyway.
  //s := VstAudioMasterOpcodeToStr(Opcode);
  //WriteLn('Plugin->Host ' + s);
end;


function VstDispatch(Effect: PAEffect; Opcode : integer; index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr;
begin
  result := Effect^.dispatcher(Effect, Opcode, Index, Value, ptr, opt);
end;


end.
