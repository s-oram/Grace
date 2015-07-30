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

  PVst2Plugin = ^TVst2Plugin;
  TVst2Plugin = class
  private
    MagicTest : integer;
    fTimeInfoStruct: PVstTimeInfo;
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

    //=== VST Methods ====
    procedure Suspend;
    procedure Resume;
    procedure SetSampleRate(const aSampleRate : integer);
    procedure SetBlockSize(const aBlockSize : integer);

    procedure SetTimeInfoStruct(const Info : PVstTimeInfo);
  end;

function AudioMasterCallBack(effect: PAEffect; opcode, index, value: longint; ptr: pointer; opt: Single): longint; cdecl;

implementation

uses
  VamVst.Extra,
  WinApi.Windows;

const
  ProductName : ansistring = 'VstTester';

function AudioMasterCallBack(effect: PAEffect; opcode, index, value: longint; ptr: pointer; opt: Single): longint;
var
  s : string;
  PluginHost : PVst2Plugin;
  x : integer;
begin
  //default result. (Usually means the opcode is not supported.
  case Opcode of
    audioMasterVersion:   result := 2400;
    audioMasterWantMidi:  result := 0; // deprecated. Not sure what the response is supposed to be.
  else
    if (assigned(effect)) and (assigned(effect^.reservedForHost)) then
    begin
      PluginHost := PVst2Plugin(effect^.reservedForHost);
      x := PluginHost^.MagicTest;
      if x <> 2091 then
      begin
        PluginHost := nil;
      end;
    end else
    begin
      PluginHost := nil;
    end;

    case Opcode of
      audioMasterGetProductString: result := longint(@ProductName[1]);
      audioMasterIOChanged: result := 1;
      audioMasterGetTime:
        if assigned(PluginHost)
          then result := longint(PluginHost^.fTimeInfoStruct)
          else result := 0;

    else
      WriteLn('=================================================================');
      WriteLn('WARNING: Opcode not handled.');
      s := VstAudioMasterOpcodeToStr(Opcode);
      WriteLn('Plugin->Host ' + s);
      WriteLn('=================================================================');
      result := 0;
      exit; //=================== exit ========================>>
    end;
  end;

  // Opcode has been handled. Log it anyway.
  //s := VstAudioMasterOpcodeToStr(Opcode);
  //WriteLn('Plugin->Host ' + s);
end;

{ TVst2Plugin }

constructor TVst2Plugin.Create(const aHostInfo : PVst2HostInfo);
begin
  MagicTest := 2091;
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
  LoadResult : boolean;
  Main:TMainProc;
begin
  WriteLn('Loading plugin...');
  LoadResult := false;

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
      Self.Suspend;
      Self.Resume;

      // The plugin should be ready for audio processing here.

      LoadResult := true;
    end;
  end;
  result := LoadResult;
end;

procedure TVst2Plugin.UnloadPlugin;
begin
  WriteLn('Unloading plugin...');

  if assigned(fVstEffect) then
  begin
    // turn plugin off
    Suspend;
    //=== close the plugin =====
    Dispatch(effClose);

    fVstEffect := nil;
  end;

  if VstDllHandle <> 0
    then FreeLibrary(VstDllHandle);

  VstDllHandle := 0;
end;

procedure TVst2Plugin.SetBlockSize(const aBlockSize: integer);
begin
  Dispatch(effSetBlockSize, 0, aBlockSize, nil, 0);
end;

procedure TVst2Plugin.SetSampleRate(const aSampleRate: integer);
begin
  Dispatch(effSetSampleRate, 0, 0, nil, aSampleRate);
end;

procedure TVst2Plugin.SetTimeInfoStruct(const Info: PVstTimeInfo);
begin
  assert(Info^.tempo = 126);
  self.fTimeInfoStruct := Info;
end;

procedure TVst2Plugin.Suspend;
begin
  Dispatch(effMainsChanged,0,0); //Call 'suspend'
end;

procedure TVst2Plugin.Resume;
begin
  Dispatch(effMainsChanged,0,1); //Call 'resume'
end;

end.
