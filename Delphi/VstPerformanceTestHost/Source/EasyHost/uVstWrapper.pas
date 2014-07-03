unit uVstWrapper;

interface

uses
  //DAEffect, DAEffectX, DAudioEffectX, DAudioEffect,
  DAEffect, DAEffectX, uMachine,
  Types, Windows, Classes;


type
  TPointerEvent = procedure(aSender:TObject; aPointer:Pointer);
  TCanDoEvent = procedure(aSender:TObject; CanDo:string; var Result:integer);

  PHostCanDo = ^THostCanDo;
  THostCanDo = class
  public
    SendVstEvents:boolean;       //Host supports send of Vst events to plug-in.
    SendVstMidiEvents:boolean;   //Host supports send of MIDI events to plug-in.
    SendVstTimeInfo:boolean;     //Host supports send of VstTimeInfo to plug-in.
    ReceiveVstEvents:boolean;    //Host can receive Vst events from plug-in.
    ReceiveVstMidiEvents:boolean;    //Host can receive MIDI events from plug-in.
    ReportConnectionChanges:boolean; //Host will indicates the plug-in when something change in plug-in´s routing/connections with suspend/resume/setSpeakerArrangement.
    AcceptIOChanges:boolean;     //Host supports ioChanged ().
    SizeWindow:boolean;          //Host supports plugin changing GUI size.
    Offline:boolean;             //Host supports offline feature.
    OpenFileSelector:boolean;    //Host supports function openFileSelector ().
    CloseFileSelector:boolean;   //Host supports function closeFileSelector ().
    StartStopProcess:boolean;    //Host supports functions startProcess () and stopProcess ().
    ShellCategory:boolean;       //'shell' handling via uniqueID. If supported by the Host and the Plug-in has the category kPlugCategShell
    SendVstMidiEventFlagIsRealtime:boolean;  //"sendVstMidiEventFlagIsRealtime" Host supports flags for VstMidiEvent.
  end;

  PPlugCanDo = ^TPlugCanDo;
  TPlugCanDo = class
  public
    SendVstEvents:boolean;         //plug-in will send Vst events to Host
    SendVstMidiEvents:boolean;     //plug-in will send MIDI events to Host
    ReceiveVstEvents:boolean;      //plug-in can receive MIDI events from Host
    ReceiveVstMidiEvents:boolean;  //plug-in can receive MIDI events from Host
    ReceiveVstTimeInfo:boolean;    //plug-in can receive Time info from Host
    Offline:boolean;               //plug-in supports offline functions (offlineNotify, offlinePrepare, offlineRun)
    MidiProgramNames:boolean;      //plug-in supports function getMidiProgramName ()
    Bypass:boolean;                //plug-in supports function setBypass ()
  end;

  //TVSTRect is used to find the size of the plugin GUI.
  PPVstRect = ^PVstRect;
  PVstRect = ^TVstRect;
  TVstRect = record
    Top,
    Left,
    Bottom,
    Right   : Smallint;
  end;

  PVstWrapper = ^TVstWrapper;
  TVstWrapper = class(TMachine)
  private
    fSampleRate: single;
    fBlockSize: integer;
    fIsPluginLoaded: boolean;
    fInputCount: integer;
    fOutputCount: integer;
    fDisplayWidth: integer;
    fDisplayHeight: integer;
    fIsDisplayVisible: boolean;
    fOnDisplaySizeChanged: TNotifyEvent;
    fWantsMidi: boolean;
    fNeedsIdleCalls: boolean;
    fPluginVendor: string;
    fPluginProductName: string;
    fParCount: integer;
    fProgramCount: integer;
    fTimeInfo: PVstTimeInfo;
    fOnProcessEvents: TPointerEvent;
    fOnIOChanged: TNotifyEvent;
    fHostVendor: string;
    fHostName: string;
    fHostVersion: integer;
    fPlugCanDo: TPlugCanDo;
    fOnHostCanDo: TCanDoEvent;
    fIsPlugOn: boolean;
    fIsTimeInfoValid: boolean;
    procedure SetBlockSize(const Value: integer);
    procedure SetSampleRate(const Value: single);
    procedure SetNumberOfPar(const Value: integer);
    function GetPar(Index: integer): single;
    procedure SetPar(Index: integer; const Value: single);
  protected
    fPar:array of single;
    Vst:PAEffect;
    VstDllHandle:THandle;

    procedure GetDisplaySize(out aWidth,aHeight:integer);
    procedure QueryPlugCanDo;

  public
    constructor Create; override;
	  destructor Destroy; override;

    function GetPluginVendor:string;
    function GetPluginName:string;
    function GetPluginVersion:integer;

    //Turn On/Off can be used to notify plugins processing will be suspended/resumed.
    procedure TurnOn;
    procedure TurnOff;

    procedure LoadVst(DllFileName:string);  //Load a vst plugin.
    procedure Unload;

    procedure OpenDisplay(WindowHandle:hwnd);  //Open the plug GUI in the window indicated by 'WindowHandle'.
    procedure UpdateDisplay;
    procedure CloseDisplay;                    //Ask the plugin to close it's GUI. (Will still need to close the parent window)

    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: longint);
    procedure SendVstEventsToPlugin(const ev : PVstEvents);

    function VstDispatch(OpCode:integer; Index:integer = 0; Value:integer = 0; Ptr: pointer = nil; Opt: single = 0):integer;

    //-- Do not use these methods directly. Used by the AudioMasterCallback. ---
    procedure amDisplaySizeChanged(NewWidth,NewHeight:integer);
    procedure amIOChanged;
    procedure amProcessEvents(aEvents:pointer);
    function amGetTempoAt(SampleFrames: integer):integer;
    function amHostCanDo(CanDo:string):integer;
    //--------------------------------------------------------------------------


    //-- Set these properties to provide information to the vst plugin ---------
    property BlockSize:integer read fBlockSize write SetBlockSize;   //Set to the largest process block size (IE: max SampleFrames)
    property SampleRate:single read fSampleRate write SetSampleRate;  //Set to change the plugins operating samplerate.

    property TimeInfo:PVstTimeInfo read fTimeInfo write fTimeInfo;  //Pointer to VstTimeInfo record for current process block.
    property IsTimeInfoValid:boolean read fIsTimeInfoValid write fIsTimeInfoValid;

    property HostVendor:string read fHostVendor write fHostVendor;
    property HostName:string read fHostName write fHostName;
    property HostVersion:integer read fHostVersion write fHostVersion;

    //--------------------------------------------------------------------------



    //-- These properties expose information about the loaded plugin. ----------
    property DisplayWidth:integer read fDisplayWidth;     //Size of the plugin GUI
    property DisplayHeight:integer read fDisplayHeight;  //Size of the plugin GUI

    property IsPluginLoaded:boolean read fIsPluginLoaded;  //Is a vst plugin currently loaded?
    property IsDisplayVisible:boolean read fIsDisplayVisible;  //Is the plugin GUI visible?
    property IsPluginOn:boolean read fIsPlugOn;

    property InputCount:integer read fInputCount write fInputCount;  //Number of inputs the plugin has.
    property OutputCount:integer read fOutputCount write fOutputCount;  //Number of outputs the plugin has. (Stero outputs count has 2 outputs.)

    property WantsMidi:boolean read fWantsMidi write fWantsMidi;  //Does the plugin want midi data?
    property NeedsIdleCalls:boolean read fNeedsIdleCalls write fNeedsIdleCalls;  //Does need idle calls?

    property ProgramCount:integer read fProgramCount write fProgramCount;  //Number of programs the plugin has.
    property ParCount:integer read fParCount write SetNumberOfPar;  //Number of parameters per plugin.
    property Par[Index:integer]:single read GetPar write SetPar;    //access the exposed plugin parameters directly.

    property PlugCanDo:TPlugCanDo read fPlugCanDo write fPlugCanDo;
    //--------------------------------------------------------------------------

    property OnDisplaySizeChanged:TNotifyEvent read fOnDisplaySizeChanged write fOnDisplaySizeChanged;  //Handle this event to respond to changes in the plugin GUI size.
    property OnIOChanged:TNotifyEvent read fOnIOChanged write fOnIOChanged;
    property OnHostCanDo:TCanDoEvent read fOnHostCanDo write fOnHostCanDo;
    property OnProcessEvents:TPointerEvent read fOnProcessEvents write fOnProcessEvents;  //This event will be called when the plugin has events (usually midi data) to send to the host.

  end;

implementation

uses
  uAudioMaster, Dialogs;



{ TVstWrapper }

constructor TVstWrapper.Create;
begin
  inherited;

  OnHostCanDo := nil;
  
  PlugCanDo := TPlugCanDo.Create;

  PlugCanDo.SendVstEvents        := false;
  PlugCanDo.SendVstMidiEvents    := false;
  PlugCanDo.ReceiveVstEvents     := false;
  PlugCanDo.ReceiveVstMidiEvents := false;
  PlugCanDo.ReceiveVstTimeInfo   := false;
  PlugCanDo.Offline              := false;
  PlugCanDo.MidiProgramNames     := false;
  PlugCanDo.Bypass               := false;


  HostVendor := 'One Small Clue';
  HostName := 'One Small Clue VST Wrapper';
  HostVersion := 1;

  fIsDisplayVisible := false;
  fIsPluginLoaded := false;

  BlockSize := 256;
  SampleRate := 44100;

  VstDllHandle := 0;

  NeedsIdleCalls := false;
  WantsMidi := false;
  ParCount := 0;

  TimeInfo := nil;
  IsTimeInfoValid := false;


end;

destructor TVstWrapper.Destroy;
begin
  //before free'ing anything, close and unload the vst plugin.
  if IsDisplayVisible then CloseDisplay;

  if fIsPluginLoaded then Unload;
  //---------------------------------------------------------

  inherited;
end;

function TVstWrapper.VstDispatch(OpCode, Index, Value: integer; Ptr: pointer; Opt: single): integer;
begin
  assert(IsPluginLoaded);
  result := Vst^.dispatcher(Vst,OpCode,Index,Value,Ptr,Opt);
end;

procedure TVstWrapper.GetDisplaySize(out aWidth,aHeight:integer);
var
  Rect:TVstRect;
  ptr:PPVstRect;
  r : integer;
begin
  //Set to empty values.
  aWidth  := 0;
  aHeight := 0;

  GetMem(ptr,SizeOf(PPVstRect));
  try
    if IsPluginLoaded then
    begin
      r := VstDispatch(effEditGetRect,0,0,ptr,0);

      if (r <> 0) and (assigned(ptr)) and (assigned(ptr^)) then
      begin
        Rect := ptr^^;
        aHeight := Rect.Bottom;
        aWidth  := Rect.Right;
      end;
    end;




  finally
    FreeMem(ptr);
  end;
end;

function TVstWrapper.GetPar(Index: integer): single;
begin
  result := fPar[Index];
end;

function TVstWrapper.GetPluginVendor: string;
var
  s:string[64];
begin
  if fIsPluginLoaded = false then
  begin
    result := '';
  end else
  begin
    VstDispatch(effGetVendorString,0,0,@s[1],0);
    result := s;
  end;
end;

function TVstWrapper.GetPluginName: string;
var
  s:string[64];
begin
  if IsPluginLoaded = false then
  begin
    result := '';
  end else
  begin
    VstDispatch(effGetProductString,0,0,@s[1],0);
    result := s;
  end;
end;

function TVstWrapper.GetPluginVersion: integer;
begin
  if IsPluginLoaded
    then result := VstDispatch(effGetVendorVersion)
    else result := 0;
end;

procedure TVstWrapper.SetPar(Index: integer; const Value: single);
begin
  fPar[Index] := Value;
end;

procedure TVstWrapper.LoadVst(DllFileName: string);
var
  Main:TMainProc;
  AudioMaster:TAudioMasterCallbackFunc;
  h,w:integer;
begin
  if IsPluginLoaded then Unload;

  assert(VstDllHandle = 0);

  VstDllHandle := LoadLibrary(@DllFileName[1]);

  if VstDllHandle <> 0 then
  begin
    AudioMaster := AudioMasterCallBack;
    @Main := GetProcAddress(VstDllHandle,'main');
    //TODO: Check main has been found.

    CreatePlugLock.Acquire;

    CreatePlugInfo.Effect := nil;
    CreatePlugInfo.VstWrapper := @Self;

    CreatingPlug := true;

    try
      Vst := Pointer(Main(AudioMaster));
      //TODO: Check Vst pointer was returned correctly.

      //Add the plugin reference so that the audio master call back function
      //knows what plugin wrapper + host the vst effect record is associated with.
      //This is important for when events need to be triggered from the audiomaster
      //callback function.
      uAudioMaster.AddPlugReference(Vst,@Self);
    finally
      CreatingPlug := false;
      CreatePlugLock.Release;
    end;

    if not assigned(Vst.processReplacing) then
    begin
      ShowMessage('This plugin does not support ProcessReplacing. It can not be loaded.');
      Unload;
    end;

    fIsPluginLoaded := true;

    //Init Vst plugin.
    VstDispatch(effOpen);
    VstDispatch(effSetSampleRate,0,0,nil,SampleRate);
    VstDispatch(effSetBlockSize,0,BlockSize,nil,0);

    InputCount := Vst^.numInputs;
    OutputCount := Vst^.numOutputs;

    ProgramCount := Vst^.numPrograms;
    ParCount := Vst^.numParams;

    GetDisplaySize(w,h);
    fDisplayHeight := h;
    fDisplayWidth := w;


    QueryPlugCanDo;


    //After all initialisation, calling TurnOn allows the plugin to ready itself for audio processing.
    TurnOn;

  end;


end;

procedure TVstWrapper.OpenDisplay(WindowHandle: hwnd);
var
  w,h:integer;
begin
  if (IsPluginLoaded) and (IsDisplayVisible = false) then
  begin
    VstDispatch(effEditOpen,0,0,Pointer(WindowHandle),0);

    //send effEditIdle after opening the GUI. Some plugins will not display correctly with out it.
    //Including U-he Zebra.
    VstDispatch(effEditIdle);

    fIsDisplayVisible := true;

    //Update the display size now that the GUI window has been asked to open.
    //This improves compatability with some plugins.
    GetDisplaySize(w,h);
    fDisplayHeight := h;
    fDisplayWidth := w;
  end;
end;

procedure TVstWrapper.ProcessReplacing(Inputs, Outputs: PPSingle;
  SampleFrames: Integer);
begin
  assert(IsPluginLoaded);
  assert(assigned(Vst));
  assert(IsPluginOn);
  assert(SampleFrames <= Self.BlockSize);

  Vst^.processReplacing(Vst, Inputs,Outputs,SampleFrames);

end;

procedure TVstWrapper.QueryPlugCanDo;
var
  s:string;
begin
  s := 'SendVstEvents';
  PlugCanDo.SendVstEvents        := Boolean(VstDispatch(effCanDo,0,0,PChar(s),0));

  s := 'SendVstMidiEvents';
  PlugCanDo.SendVstMidiEvents    := Boolean(VstDispatch(effCanDo,0,0,PChar(s),0));

  s := 'ReceiveVstEvents';
  PlugCanDo.ReceiveVstEvents     := Boolean(VstDispatch(effCanDo,0,0,PChar(s),0));

  s := 'ReceiveVstMidiEvents';
  PlugCanDo.ReceiveVstMidiEvents := Boolean(VstDispatch(effCanDo,0,0,PChar(s),0));

  s := 'ReceiveVstTimeInfo';
  PlugCanDo.ReceiveVstTimeInfo   := Boolean(VstDispatch(effCanDo,0,0,PChar(s),0));

  s := 'Offline';
  PlugCanDo.Offline              := Boolean(VstDispatch(effCanDo,0,0,PChar(s),0));

  s := 'MidiProgramNames';
  PlugCanDo.MidiProgramNames     := Boolean(VstDispatch(effCanDo,0,0,PChar(s),0));

  s := 'Bypass';
  PlugCanDo.Bypass               := Boolean(VstDispatch(effCanDo,0,0,PChar(s),0));
end;

procedure TVstWrapper.CloseDisplay;
begin
  if (IsPluginLoaded)  and (IsDisplayVisible) then
  begin
    VstDispatch(effEditClose);
    fIsDisplayVisible := false;
  end;
end;

procedure TVstWrapper.amDisplaySizeChanged(NewWidth, NewHeight: integer);
begin
  //Something isn't right here.
  //Need to step though code and find exactly where this method is being called
  //from within Zebra and why this is causing problems with the effIdle call.
  fDisplayWidth := NewWidth;
  fDisplayHeight := NewHeight;

  if assigned(fOnDisplaySizeChanged) then OnDisplaySizeChanged(self);

end;

function TVstWrapper.amGetTempoAt(SampleFrames: integer):integer;
begin
  // returns tempo (in bpm * 10000) at sample frame location.
  if assigned(TimeInfo)
    then result := round(TimeInfo^.tempo * 10000)
    else result := 120 * 10000;

  //NOTE: GetTempoAt is a deprecated function of the VST interface, so don't support
  //it directly, just return the tempo for the current sample block.
end;


function TVstWrapper.amHostCanDo(CanDo: string):integer;
var
  CanDoResult:integer;
begin
  CanDoResult := 0;  //By default return 0.

  //Respond to all possible CanDo strings with default values.
  if CanDo = 'sendVstEvents'     then CanDoResult := 1;
  if CanDo = 'sendVstMidiEvents' then CanDoResult := 1;
  if CanDo = 'sendVstTimeInfo'   then CanDoResult := 1;
  if CanDo = 'receiveVstEvents'  then CanDoResult := 1;
  if CanDo = 'receiveVstMidiEvents'    then CanDoResult := 1;
  if CanDo = 'reportConnectionChanges' then CanDoResult := 0;
  if CanDo = 'acceptIOChanges'         then CanDoResult := 0;
  if CanDo = 'sizeWindow'        then CanDoResult := 0;
  if CanDo = 'offline'           then CanDoResult := 0;
  if CanDo = 'openFileSelector'  then CanDoResult := 0;
  if CanDo = 'closeFileSelector' then CanDoResult := 0;
  if CanDo = 'startStopProcess'  then CanDoResult := 0;
  if CanDo = 'shellCategory'     then CanDoResult := 0;
  if CanDo = 'sendVstMidiEventFlagIsRealtime' then CanDoResult := 0;


  //Allow host to change the CanDo result value.
  if assigned(fOnHostCanDo) then
  begin
    OnHostCanDo(self,CanDo,CanDoResult);
  end;

  //Finally, return the CanDo result.
  Result := CanDoResult;


end;

procedure TVstWrapper.amIOChanged;
begin
  if assigned(Vst) then
  begin
    InputCount := Vst^.numInputs;
    OutputCount := Vst^.numOutputs;
  end;

  if assigned(OnIOChanged) then OnIOChanged(Self);  
end;

procedure TVstWrapper.amProcessEvents(aEvents: pointer);
begin
  if assigned(OnProcessEvents) then
  begin
    OnProcessEvents(self,aEvents);
  end;

end;



procedure TVstWrapper.SetBlockSize(const Value: integer);
begin
  fBlockSize := Value;

  uAudioMaster.LocalBlockSize := Value;

  if IsPluginLoaded then
  begin
    TurnOff;
    VstDispatch(effSetBlockSize,0,fBlockSize,nil,0);
    TurnOn;
  end;

end;

procedure TVstWrapper.SetNumberOfPar(const Value: integer);
begin
  assert(Value >= 0);
  fParCount := Value;
  SetLength(fPar, Value);
end;

procedure TVstWrapper.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;

  uAudioMaster.LocalSampleRate := Value;

  if IsPluginLoaded then
  begin
    TurnOff;
    VstDispatch(effSetSampleRate,0,0,nil,fSampleRate);
    TurnOn;
  end;

end;

procedure TVstWrapper.Unload;
begin
  if IsDisplayVisible then CloseDisplay;

  if IsPluginLoaded then
  begin
    VstDispatch(effClose);
  end;

  RemovePlugReference(Vst);

  if VstDllHandle <> 0 then
  begin
    FreeLibrary(VstDllHandle);
    VstDllHandle := 0;
  end;

  fIsPluginLoaded := false;

  //Reset some values
  NeedsIdleCalls := false;
  WantsMidi := false;
  ParCount := 0;
  Vst := nil;

  PlugCanDo.SendVstEvents        := false;
  PlugCanDo.SendVstMidiEvents    := false;
  PlugCanDo.ReceiveVstEvents     := false;
  PlugCanDo.ReceiveVstMidiEvents := false;
  PlugCanDo.ReceiveVstTimeInfo   := false;
  PlugCanDo.Offline              := false;
  PlugCanDo.MidiProgramNames     := false;
  PlugCanDo.Bypass               := false;

end;


procedure TVstWrapper.UpdateDisplay;
begin
  assert(IsPluginLoaded);
  assert(IsDisplayVisible);

  VstDispatch(effEditIdle);
end;

procedure TVstWrapper.TurnOn;
begin
  assert(IsPluginLoaded);
  VstDispatch(effMainsChanged,0,1); //Call 'resume'
  fIsPlugOn := true;
end;


procedure TVstWrapper.TurnOff;
begin
  assert(IsPluginLoaded);

  fIsPlugOn := false;
  VstDispatch(effMainsChanged);  //Call 'suspend'

end;

procedure TVstWrapper.SendVstEventsToPlugin(const ev: PVstEvents);
begin
  VstDispatch(effProcessEvents, 0, 0, ev, 0);
end;



end.
