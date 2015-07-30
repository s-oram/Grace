unit TestHost.Vst2;

interface

uses
  TestHost.Vst2.DAEffect,
  TestHost.Vst2.DAEffectX;

type
  PVst2HostInfo = ^TVst2HostInfo;
  TVst2HostInfo = record
    SampleRate : integer;
    MaxBufferSize : integer;
  end;

  TVst2Plugin = class
  private
  protected
    fVstEffect : PAEffect;
    HostInfo : PVst2HostInfo;
    VstDllHandle : THandle;
    procedure UnloadPlugin;
  public
    constructor Create(const aHostInfo : PVst2HostInfo);
    destructor Destroy; override;

    function LoadPlugin(const FileName : string):boolean;

    function Dispatch(Opcode : integer; index: VstInt32 = 0; value: VstIntPtr = 0; ptr: pointer = nil; opt: single = 0): VstIntPtr;

    property VstEffect : PAEffect read fVstEffect;
  end;

function AudioMasterCallBack(effect: PAEffect; opcode, index, value: longint; ptr: pointer; opt: Single): longint; cdecl;

implementation

uses
  VamVst.Extra,
  WinApi.Windows;

function AudioMasterCallBack(effect: PAEffect; opcode, index, value: longint; ptr: pointer; opt: Single): longint;
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

{ TVst2Plugin }

constructor TVst2Plugin.Create(const aHostInfo : PVst2HostInfo);
begin
  fVstEffect := nil;
  HostInfo := aHostInfo;
end;

destructor TVst2Plugin.Destroy;
begin
  UnloadPlugin;
  inherited;
end;


function TVst2Plugin.Dispatch(Opcode: integer; index: VstInt32; value: VstIntPtr; ptr: pointer; opt: single): VstIntPtr;
begin
  assert(assigned(fVstEffect));
  result := fVstEffect^.dispatcher(fVstEffect, Opcode, Index, Value, ptr, opt);
end;

function TVst2Plugin.LoadPlugin(const FileName: string): boolean;
var
  Main:TMainProc;
begin
  WriteLn('Loading plugin...');

  VstDllHandle := LoadLibrary(@FileName[1]);
  if VstDllHandle <> 0 then
  begin
    @Main := GetProcAddress(VstDllHandle,'main');
    //TODO: Check main has been found.

    //==== Create the plugin ====
    fVstEffect := Pointer(Main(AudioMasterCallBack));
    if assigned(fVstEffect) then
    begin


      fVstEffect.reservedForHost := @self;

      //Init Vst plugin.
      Dispatch(effOpen);
      Dispatch(effSetSampleRate,0,0,nil, HostInfo.SampleRate);
      Dispatch(effSetBlockSize,0, HostInfo.MaxBufferSize,nil,0);

      // InputCount := Vst^.numInputs;
      // OutputCount := Vst^.numOutputs;

      //QueryPlugCanDo;

      //After all initialisation, calling TurnOn allows the plugin to ready itself for audio processing.
      Dispatch(effMainsChanged,0,0); //Call 'suspend'
      Dispatch(effMainsChanged,0,1); //Call 'resume'

      //==== perform audio processing here ====

      WriteLn('Load complete.');
      WriteLn('');
    end;
  end;

  //WriteLn('VST Plugin has been created.');
end;

procedure TVst2Plugin.UnloadPlugin;
begin
  WriteLn('Unloading plugin...');

  if assigned(fVstEffect) then
  begin
    // turn plugin off
    Dispatch(effMainsChanged);  //Call 'suspend'
    //=== close the plugin =====
    Dispatch(effClose);

    fVstEffect := nil;
  end;

  if VstDllHandle <> 0
    then FreeLibrary(VstDllHandle);

  VstDllHandle := 0;

  WriteLn('Unload complete.');
end;

end.
