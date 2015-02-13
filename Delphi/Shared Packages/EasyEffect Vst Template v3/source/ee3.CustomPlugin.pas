unit ee3.CustomPlugin;

interface

uses
  Vcl.Forms,
  Classes,
  DAEffect,
  DAEffectX,
  DAudioEffect,
  DAudioEffectX,
  ee3.VstConstants,
  ee3.VstGlobals,
  ee3.ProcessController,
  ee3.CustomPlugin.PublishedVstParInfo;

type
  TCustomVstPlugin = class;
  TCustomVstEditor = class;
  TCustomVstGuiForm = class;

  TCustomVstPlugin = class(AudioEffectX)
  private
    FVstParInfo        : IPublishedVstParInfo;
    FVstGlobals        : TVstGlobals;
    ProcessController  : TCustomProcessController;
    ChunkData          : TMemoryStream;
  protected
    // InjectDependency() - TCustomVstPlugin will assume ownership of injected objects.
    procedure InjectDependency(aProcessController : TCustomProcessController); overload;
    procedure InjectDependency(const aVstParInfo : IPublishedVstParInfo); overload;
  public
    //=================== overriden AudioEffectX methods ======================
    constructor Create(anAudioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: VstInt32); reintroduce;
    destructor Destroy; override;

    function GetInputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean; override;             // Return the properties of output index
    function GetOutputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean; override;            // Return the properties of input index

    function GetChunk(var data: pointer; isPreset: Boolean): longint; override;
    function SetChunk(data: pointer; byteSize: Integer; isPreset: Boolean): longint; override;

    procedure SetUniqueID(ID: string); reintroduce;

    function ProcessEvents(Events: PVstEvents): VstInt32; override;

    procedure Suspend; override;   // Called when plug-in is switched to off
    procedure Resume; override;    // Called when plug-in is switched to on

    procedure SetNumInputs(Inputs: VstInt32); override;   // Set the number of inputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available inputs.
    procedure SetNumOutputs(Outputs: VstInt32); override; // Set the number of outputs the plug-in will handle. For a plug-in which could change its IO configuration, this number is the maximun available ouputs.

    procedure ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32); override;


    procedure SetParameter(Index: VstInt32; Value: single); override;            // Called when a parameter changed
    function GetParameter(Index: integer): single; override;                     // Return the value of the parameter with index

    procedure GetParameterName(Index: VstInt32; Text: PAnsiChar); override;    // Stuff text with the name ("Time", "Gain", "RoomType", etc...) of parameter index. Limited to kVstMaxParamStrLen.
    procedure GetParameterDisplay(Index: VstInt32; Text: PAnsiChar); override; // Stuff text with a string representation ("0.5", "-3", "PLATE", etc...) of the value of parameter index. Limited to kVstMaxParamStrLen.
    procedure GetParameterLabel(Index: VstInt32; aLabel: PAnsiChar); override;   // Stuff label with the units in which parameter index is displayed (i.e. "sec", "dB", "type", etc...). Limited to kVstMaxParamStrLen.

    //=============== additions to the VST template =========================
    // NOTE: Looking at how I'm adapting VST AudioEffect methods to fit my own idiom, it
    // might be better to use a custom "Master Effect" object that would be the root of
    // all my plugins. The AudioEffect descendent would then wrap around my custom plugin format
    // and serve it as a VST plugin. I've used this method in Lucidity and Poise.
    procedure SetPreset(PresetData : TMemoryStream); virtual;
    procedure GetPreset(PresetData : TMemoryStream); virtual;

    procedure GetInputInfo(const Index:integer; var IsStereo:boolean; var Name,ShortName:AnsiString); virtual;
    procedure GetOutputInfo(const Index:integer; var IsStereo:boolean; var Name,ShortName:AnsiString); virtual;

    property VstGlobals : TVstGlobals read FVstGlobals;
  end;

  TCustomVstEditor = class(AEffEditor)
  private
  protected
  public
  end;

  TCustomVstGuiForm = class(TForm)
  end;

procedure ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
procedure ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: VstInt32); cdecl;

implementation

uses
  SysUtils,
  DVstUtils;

procedure ProcessClassReplacing(e: PAEffect; Inputs, Outputs: PPSingle; SampleFrames: VstInt32); cdecl;
begin
  assert(assigned(TCustomVstPlugin(e^.vObject).ProcessController), 'ProcessController not assigned.');
  TCustomVstPlugin(e^.vObject).ProcessController.ProcessReplacing(Inputs, Outputs, SampleFrames);
end;

procedure ProcessClassDoubleReplacing(e: PAEffect; Inputs, Outputs: PPDouble; SampleFrames: VstInt32); cdecl;
begin
  assert(assigned(TCustomVstPlugin(e^.vObject).ProcessController), 'ProcessController not assigned.');
  TCustomVstPlugin(e^.vObject).ProcessController.ProcessDoubleReplacing(Inputs, Outputs, SampleFrames);
end;

{ TCustomPlugin }

constructor TCustomVstPlugin.Create(anAudioMaster: TAudioMasterCallbackFunc; aNumPrograms, aNumParams: VstInt32);
begin
  inherited Create(anAudioMaster, aNumPrograms, aNumParams);

  ProgramsAreChunks(true);
  CanProcessReplacing(TRUE);

  ChunkData := TMemoryStream.Create;

  FVstGlobals := TVstGlobals.Create(self);

  Effect.processReplacing := ProcessClassReplacing;
  Effect.processDoubleReplacing := ProcessClassDoubleReplacing;

  Randomize;
end;

destructor TCustomVstPlugin.Destroy;
begin
  FVstParInfo := nil;
  FVstGlobals.Free;
  if assigned(ProcessController) then ProcessController.Free;
  ChunkData.Free;
  inherited;
end;

function TCustomVstPlugin.GetInputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean;
var
  Name:AnsiString;
  ShortName:AnsiString;
  IsStereo:boolean;
begin
  try
    IsStereo  := true;
    Name      := 'Input ' + inttostr(index div 2 + 1);
    ShortName := 'In'     + inttostr(index div 2 + 1);

    self.GetInputInfo(Index,IsStereo,Name,ShortName);

    strPCopy(@Properties^.vLabel[0],Name);
    strPCopy(@Properties^.ShortLabel[0],ShortName);
    if IsStereo then Properties^.flags := kVstPinIsStereo;

    Properties^.flags := Properties^.flags + kVstPinIsActive;

    result := true;
  except
    on EAccessViolation do result := false;
    else raise;
  end;
end;

function TCustomVstPlugin.GetOutputProperties(Index: VstInt32; Properties: PVstPinProperties): boolean;
var
  Name:AnsiString;
  ShortName:AnsiString;
  IsStereo:boolean;
begin
  try
    IsStereo  := true;
    Name      := 'Output ' + inttostr(index div 2 + 1);
    ShortName := 'Out'     + inttostr(index div 2 + 1);

    self.GetOutputInfo(Index,IsStereo,Name,ShortName);

    strPCopy(@Properties^.vLabel[0],Name);
    strPCopy(@Properties^.ShortLabel[0],ShortName);
    if IsStereo then Properties^.flags := kVstPinIsStereo;

    Properties^.flags := Properties^.flags + kVstPinIsActive;

    result := true;
  except
    // Access violation error is raised here sometimes in Mix Craft 6, build 180.
    // reported by Dan dan@acoustica.com
    // I can't figure out why this should cause an exception so I catch the
    // exception and return false. .
    on EAccessViolation do result := false;
    else raise;
  end;
end;

procedure TCustomVstPlugin.GetInputInfo(const Index: integer; var IsStereo: boolean; var Name, ShortName: AnsiString);
begin

end;

procedure TCustomVstPlugin.GetOutputInfo(const Index: integer; var IsStereo: boolean; var Name, ShortName: AnsiString);
begin

end;

function TCustomVstPlugin.GetChunk(var data: pointer; isPreset: Boolean): longint;
begin
  {$IFDEF Logging}Log.Main.TrackMethod('TeeVstAdapter.GetChunk()');{$ENDIF}

  //Clear the data storage memory stream
  ChunkData.Clear;
  try
    GetPreset(ChunkData);
    data   := ChunkData.Memory;
    result := ChunkData.Size;
  except
    //TODO:MED should log exceptions here.
    data   := ChunkData.Memory;
    result := 0;
  end;
end;

function TCustomVstPlugin.SetChunk(data: pointer; byteSize: Integer; isPreset: Boolean): longint;
begin
  {$IFDEF Logging}Log.Main.TrackMethod('TeeVstAdapter.SetChunk()');{$ENDIF}

  result := byteSize;

  ChunkData.Clear;
  ChunkData.Write(data^,bytesize);
  ChunkData.Seek(0,soFromBeginning);

  try
    SetPreset(ChunkData);
  except
    //TODO:MED should log exceptions here.
  end;

  ChunkData.Clear;
end;

procedure TCustomVstPlugin.SetPreset(PresetData: TMemoryStream);
begin
  // do nothing here.
end;

procedure TCustomVstPlugin.GetPreset(PresetData: TMemoryStream);
begin
  // do nothing here.
end;

procedure TCustomVstPlugin.SetParameter(Index: VstInt32; Value: single);
begin
  FVstParInfo.VstPar_SetParameter(Index, Value);
end;

function TCustomVstPlugin.GetParameter(Index: integer): single;
begin
  result := FVstParInfo.VstPar_GetParameter(Index);
end;

procedure TCustomVstPlugin.GetParameterDisplay(Index: VstInt32; Text: PAnsiChar);
var
  s : string;
begin
  s := FVstParInfo.VstPar_GetParameterDisplay(Index);
  StrPCopy(Text, AnsiString(s));
end;

procedure TCustomVstPlugin.GetParameterLabel(Index: VstInt32; aLabel: PAnsiChar);
var
  s : string;
begin
  s := FVstParInfo.VstPar_GetParameterLabel(Index);
  StrPCopy(aLabel, AnsiString(s));
end;

procedure TCustomVstPlugin.GetParameterName(Index: VstInt32; Text: PAnsiChar);
var
  s : string;
begin
  s := FVstParInfo.VstPar_GetParameterName(Index);
  StrPCopy(Text, AnsiString(s));
end;

procedure TCustomVstPlugin.InjectDependency(const aVstParInfo: IPublishedVstParInfo);
begin
  FVstParInfo := aVstParInfo;
end;

procedure TCustomVstPlugin.InjectDependency(aProcessController: TCustomProcessController);
begin
  if assigned(ProcessController) then ProcessController.Free;
  ProcessController := aProcessController;
end;

procedure TCustomVstPlugin.Suspend;
begin
  inherited;
  ProcessController.Suspend;
end;

procedure TCustomVstPlugin.Resume;
begin
  inherited;
  ProcessController.Resume;
  ChunkData.Clear;
end;

function TCustomVstPlugin.ProcessEvents(Events: PVstEvents): VstInt32;
begin
  ProcessController.ProcessVstEvents(Events);
  result := 0;
end;

procedure TCustomVstPlugin.ProcessReplacing(Inputs, Outputs: PPSingle; SampleFrames: VstInt32);
begin
  inherited;
end;

procedure TCustomVstPlugin.SetNumInputs(Inputs: VstInt32);
begin
  inherited;
  ProcessController.SetNumInputs(Inputs);
end;

procedure TCustomVstPlugin.SetNumOutputs(Outputs: VstInt32);
begin
  inherited;
  ProcessController.SetNumOutputs(Outputs);
end;

procedure TCustomVstPlugin.SetUniqueID(ID: string);
var
  UniqueId:AnsiString;
begin
  assert(Length(ID) = 4);
  UniqueId := AnsiString(ID);
  inherited SetUniqueID(FourCharToLong(UniqueID[1],UniqueID[2],UniqueID[3],UniqueID[4]));
end;



end.
