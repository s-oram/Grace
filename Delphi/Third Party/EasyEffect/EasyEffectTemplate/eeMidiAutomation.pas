unit eeMidiAutomation;

interface

uses
  VamLib.Types,
  SyncObjs, Classes, eeIniFile, eeMidiMap, eeMidiInputSmoother;

type
  TParameterAutomation = procedure(Index:integer; Value:single) of object;
  TEnableMidiLearn   = procedure(ParameterID : integer; TargetType : TTargetType) of object;
  TRemoveMidiBinding = procedure(ParameterID : integer; TargetType : TTargetType) of object;
  TSetMidiBinding    = procedure(ParameterID : integer; TargetType : TTargetType; MidiCCIndex : integer) of object;

  TMidiAutomation = class
  private
    fIsMidiLearnActive: boolean;
    fOnVstParameterAutomation: TParameterAutomation;
    fOnPrivateParameterAutomation: TParameterAutomation;
    fMidiMap: TMidiMap;
    fInitialMidiMapFileName: string;
    fAutoSaveMidiMapOnExit: boolean;
    fSampleRate: single;
    procedure SetSampleRate(const Value: single);
  protected

    InputSmoother : TMidiInputSmoother;
    LastTouched:TMidiBinding;
    InstanceLock : TFixedCriticalSection;
    class var InstanceCount : integer;
    property InitialMidiMapFileName : string read fInitialMidiMapFileName write fInitialMidiMapFileName;

    procedure EventHandle_SampleRateChanged(Sender:TObject);
  public
    constructor Create(const aMidiMapFileName:string);
	  destructor Destroy; override;

    procedure FastControlProcess; inline;

    //NOTE: Using parameter name as part of the enabled midi learn thing seems redundant.
    procedure EnableMidiLearn(ParameterID: integer; ParameterName: string; TargetType: TTargetType); overload;
    procedure EnableMidiLearn(ParameterID: integer; TargetType: TTargetType); overload;
    procedure DisableMidiLearn;

    procedure RemoveBinding(ParameterID:integer; TargetType:TTargetType);
    function GetCurrentBinding(ParameterID:integer; TargetType:TTargetType):integer;
    procedure SetMidiBinding(ParameterID : integer; TargetType : TTargetType; MidiCCIndex : integer);

    procedure ProcessMidiCC(Data1,Data2:byte);

    property IsMidiLearnActive:boolean read fIsMidiLearnActive;

    property MidiMap:TMidiMap read fMidiMap; //Use midi map to access information on midi bindings.

    property AutoSaveMidiMapOnExit : boolean read fAutoSaveMidiMapOnExit write fAutoSaveMidiMapOnExit;

    property SampleRate : single read fSampleRate  write SetSampleRate;

    property OnVstParameterAutomation     :TParameterAutomation read fOnVstParameterAutomation     write fOnVstParameterAutomation;
    property OnPrivateParameterAutomation :TParameterAutomation read fOnPrivateParameterAutomation write fOnPrivateParameterAutomation;

  end;

implementation

uses
  SysUtils;

var
  GlobalMidiMap:TMidiMap;

{ TMidiAutomation }

constructor TMidiAutomation.Create(const aMidiMapFileName:string);
begin
  FSampleRate := 44100;
  AutoSaveMidiMapOnExit := true;

  InstanceLock := TFixedCriticalSection.Create;
  InstanceLock.Acquire;

  InitialMidiMapFileName := aMidiMapFileName;

  try
    if (InstanceCount = 0) then
    begin
      assert(not assigned(GlobalMidiMap));
      GlobalMidiMap := TMidiMap.Create;
      if FileExists(InitialMidiMapFileName) then
      begin
        GlobalMidiMap.LoadFromXmlFile(InitialMidiMapFileName);
      end;
    end;
    fMidiMap := GlobalMidiMap;
    inc(InstanceCount);
  finally
    InstanceLock.Release;
  end;

  fIsMidiLearnActive := false;

  InputSmoother := TMidiInputSmoother.Create;

  EventHandle_SampleRateChanged(self);
end;

destructor TMidiAutomation.Destroy;
begin
  fMidiMap := nil;

  InstanceLock.Acquire;
  try
    dec(InstanceCount);
    assert(InstanceCount >= 0);

    if (InstanceCount = 0) then
    begin
      if (AutoSaveMidiMapOnExit) and (InitialMidiMapFileName <> '') then
      begin
        GlobalMidiMap.SaveToXmlFile(InitialMidiMapFileName);
      end;
      FreeAndNil(GlobalMidiMap);
    end;
  finally
    InstanceLock.Release;
  end;


  InstanceLock.Free;
  InputSmoother.Free;

  inherited;
end;



procedure TMidiAutomation.EnableMidiLearn(ParameterID: integer; ParameterName: string; TargetType: TTargetType);
var
  Index:integer;
begin
  //Check to see if control is already bound to a MIDI CC number, if so remove.
  Index := MidiMap.FindByParameterID(ParameterID, TargetType);
  if Index <> -1 then MidiMap.DeleteBinding(Index);

  //Activate midi learn and record parameter info.
  fIsMidiLearnActive := true;

  LastTouched.ParameterId   := ParameterID;
  LastTouched.ParameterName := ParameterName;
  LastTouched.TargetType    := TargetType;

end;

procedure TMidiAutomation.EnableMidiLearn(ParameterID: integer; TargetType: TTargetType);
var
  ParName : string;
begin
  case TargetType of
    ttVstParameter:     ParName := 'VstPar'     + IntToStr(ParameterID);
    ttPrivateParameter: ParName := 'PrivatePar' + IntToStr(ParameterID);
  else
    ParName := '';
    raise Exception.Create('Unexpected TargetType value.');
  end;

  EnableMidiLearn(ParameterID, ParName, TargetType);
end;

procedure TMidiAutomation.EventHandle_SampleRateChanged(Sender: TObject);
begin
  InputSmoother.SmoothingBufferSize := round(SampleRate / 50) + 1;
end;

function TMidiAutomation.GetCurrentBinding(ParameterID: integer; TargetType: TTargetType): integer;
var
  Index  : integer;
  MidiCC : integer;
begin
  Index := MidiMap.FindByParameterID(ParameterID, ttVstParameter);

  if Index <> -1
    then MidiCC := MidiMap.Binding[Index]^.MidiCC
    else MidiCC := -1;

  result := MidiCC;
end;

procedure TMidiAutomation.DisableMidiLearn;
begin
  fIsMidiLearnActive := false;
end;

procedure TMidiAutomation.RemoveBinding(ParameterID: integer; TargetType: TTargetType);
var
  Index:integer;
begin
  Index := MidiMap.FindByParameterID(ParameterID, TargetType);
  if Index <> -1 then MidiMap.DeleteBinding(Index);
end;

procedure TMidiAutomation.SetMidiBinding(ParameterID: integer; TargetType: TTargetType; MidiCCIndex: integer);
var
  ParName : string;
  BidingIndex:integer;
begin
  case TargetType of
    ttVstParameter:     ParName := 'VstPar'     + IntToStr(ParameterID);
    ttPrivateParameter: ParName := 'PrivatePar' + IntToStr(ParameterID);
  else
    ParName := '';
    raise Exception.Create('Unexpected TargetType value.');
  end;

  BidingIndex := MidiMap.FindByMidiCC(MidiCCIndex);
  if BidingIndex <> -1 then MidiMap.DeleteBinding(BidingIndex);

  MidiMap.AddBinding(MidiCCIndex, ParameterId, ParName, TargetType);
end;

procedure TMidiAutomation.SetSampleRate(const Value: single);
begin
  if Value <> fSampleRate then
  begin
    fSampleRate := Value;
    EventHandle_SampleRateChanged(self);
  end;
end;

procedure TMidiAutomation.ProcessMidiCC(Data1, Data2: byte);
var
  Index:integer;
  amb:PMidiBinding;
begin
  if (IsMidiLearnActive)  then
  begin
    // NOTE: TODO: Ideally, this bit of code should call SetMidiBiding() instead of adding the code manually,
    // but SetMidiBinding() doesn't allow a ParameterName value to be set, so it is not a straight one-for-one
    // refactoring, some functionality would change. I don't think Parameter name is very important. I can't
    // recall any instances where it's used in a significant way. Perhaps the whole, custom parameter name value
    // should be removed.
    Index := MidiMap.FindByMidiCC(Data1);
    if Index <> -1 then MidiMap.DeleteBinding(Index);

    MidiMap.AddBinding(Data1, LastTouched.ParameterId, LastTouched.ParameterName, LastTouched.TargetType);

    fIsMidiLearnActive := false;
  end;



  // Do midi automation stage if any midi bindings found.
  Index := MidiMap.FindByMidiCC(Data1);
  if (index <> -1) then
  begin
    amb := MidiMap.Binding[Index];

    if (amb^.TargetType = ttVstParameter) and (assigned(OnVstParameterAutomation)) then
    begin
      InputSmoother.MidiInput(amb^.ParameterId, Data2 / 127);
      //OnVstParameterAutomation(amb^.ParameterId, Data2 / 127);
    end;
  end;

end;


procedure TMidiAutomation.FastControlProcess;
begin
  InputSmoother.Step(fOnVstParameterAutomation);
end;



initialization

finalization

end.
