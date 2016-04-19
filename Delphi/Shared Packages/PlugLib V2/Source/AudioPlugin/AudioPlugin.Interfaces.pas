unit AudioPlugin.Interfaces;

interface

uses
  VamVst2.DAEffect,
  VamVst2.DAEffectX,
  AudioPlugin.Types;

type
  IPlugMain = interface
    ['{4AB3FA68-027F-400C-A9C7-AE5CBBA73E2B}']
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single); overload;
    procedure SetPrivateParameter(const ParIndex : integer; const ParName : string; const ValueA : integer; const ValueB : single; const Data : array of pointer); overload;
    function GetPrivateParameter(const ParIndex : integer; const ParName : string):TPrivateParResult;
  end;

  IVst2AudioEffectX = interface
    ['{9BDA491F-5BE6-4AB7-9151-64625165B358}']
    // NOTE TO SELF: This interface should only concern itself with methods in AudioEffectX.
    // Don't be tempted to add additional methods here for non-AudioEffectX additional methods.
    function GetParameter(Index: integer): single;
    procedure SetParameter(Index: VstInt32; Value: single);
    procedure SetParameterAutomated(Index: VstInt32; Value: single);
    function BeginEdit(Index: VstInt32): boolean;
    function EndEdit(Index: VstInt32): boolean;

    function SendVstEventsToHost(Events: PVstEvents): boolean;

    function GetNumInputs: VstInt32;
    function GetNumOutputs: VstInt32;
    function GetBlockSize : VstInt32;
    function GetSampleRate : single;

    function GetTimeInfo(Filter: VstInt32): PVstTimeInfo;
  end;

  IMidiOut = interface
    ['{FCBF440B-37B8-41A2-901B-6E282E6A9E63}']
    procedure AddEvents(const ev : PVstEvents);
    procedure AddMidiEvent(const Status, Channel, Data1, Data2: integer); overload;
    procedure AddMidiEvent(const Status, Channel, Data1, Data2, DeltaOffset: integer); overload;
  end;

  TDoNothingMidiOut = class(TInterfacedObject, IMidiOut)
  private
    procedure AddEvents(const ev : PVstEvents);
    procedure AddMidiEvent(const Status, Channel, Data1, Data2: integer); overload;
    procedure AddMidiEvent(const Status, Channel, Data1, Data2, DeltaOffset: integer); overload;
  end;

implementation

{ TDoNothingMidiOut }

procedure TDoNothingMidiOut.AddEvents(const ev: PVstEvents);
begin
  // do nothing!
end;

procedure TDoNothingMidiOut.AddMidiEvent(const Status, Channel, Data1, Data2: integer);
begin
  // do nothing!
end;

procedure TDoNothingMidiOut.AddMidiEvent(const Status, Channel, Data1, Data2, DeltaOffset: integer);
begin
  // do nothing!
end;

end.
