unit ee3AddOn.MidiAutomation.VstPar;

interface

uses
  Contnrs,
  Classes,
  VamLib.Types,
  DAEffect,
  DAEffectX,
  DAudioEffectX,
  ee3AddOn.VstMidi;

type
  TMidiBinding = class
  public
    ParName     : string;
    VstParIndex : integer;
    MidiCC      : integer;
  end;

  TMidiAutomationController = class
  private
    function GetBindingCount: integer;
    function GetMidiBinding(Index: integer): TMidiBinding;
  protected
    Effect : AudioEffectX;

    // TODO:HIGH binding list isn't thread-safe. This Midi automation controller needs
    // to be implemented in a thread safe manner.
    // Either use locks to protect access to the binding list.
    // or use a thread-safe list. (Which i don't have yet.)
    BindingList : TObjectList;
    BindingListLock : TMultiReadSingleWrite;
    MidiLearn : record
      IsActive : boolean;
      ParName : string;
      VstParIndex : integer;
    end;

    property BindingCount : integer read GetBindingCount;
    property Binding[Index : integer]:TMidiBinding read GetMidiBinding;

  protected
    function Internal_FindBindingByMidiCC(const MidiCC : integer):TMidiBinding;  inline;
    procedure Internal_DeleteBindingByParName(const ParName : string);  inline;
    procedure Internal_DeleteBindingByVstPar(const VstParIndex : integer);  inline;
    procedure Internal_DeleteBindingByMidiCC(const MidiCC : integer);  inline;
    function Internal_AddBinding(const VstParIndex : integer; const ParName : string; const MidiCC : integer):integer; inline;
  public
    constructor Create(aEffect : AudioEffectX);
    destructor Destroy; override;

    function ProcessMidiCC(ev : PVstMidiEvent):boolean;

    procedure ActivateMidiLearnForVstParameter(const VstParIndex : integer; const ParName : string);
    procedure DeactiveMidiLearn;


    function AddBinding(const VstParIndex : integer; const ParName : string; const MidiCC : integer):integer;
    procedure DeleteBindingByParName(const ParName : string);
    procedure DeleteBindingByVstPar(const VstParIndex : integer);
    procedure DeleteBindingByMidiCC(const MidiCC : integer);
    procedure DeleteAllBindings;
  end;

implementation

{ TMidiAutomationController }

constructor TMidiAutomationController.Create(aEffect: AudioEffectX);
begin
  Effect := aEffect;

  BindingList := TObjectList.Create;
  BindingList.OwnsObjects := true;

  BindingListLock := TMultiReadSingleWrite.Create;
end;

destructor TMidiAutomationController.Destroy;
begin
  BindingList.Free;
  BindingListLock.Free;
  inherited;
end;

function TMidiAutomationController.GetBindingCount: integer;
begin
  result := BindingList.Count;
end;

function TMidiAutomationController.GetMidiBinding(Index: integer): TMidiBinding;
begin
  result := BindingList[Index] as TMidiBinding;
end;

function TMidiAutomationController.ProcessMidiCC(ev: PVstMidiEvent):boolean;
var
  Data1 : byte;
  Data2 : byte;
  b : TMidiBinding;
begin
  assert(VstMidiEventEx(ev).IsControlChange);

  //================ MIDI Learn - if active ==============================
  if MidiLearn.IsActive then
  begin
    BindingListLock.BeginWrite;
    try
      MidiLearn.IsActive := false;
      Data1 := VstMidiEventEx(ev).Data1;
      Internal_DeleteBindingByVstPar(MidiLearn.VstParIndex);
      Internal_DeleteBindingByMidiCC(Data1);
      Internal_AddBinding(MidiLearn.VstParIndex, MidiLearn.ParName, Data1);
    finally
      BindingListLock.EndWrite;
    end;
  end;

  //=================== regular event processing ================================
  BindingListLock.BeginRead;
  try
    Data1 := VstMidiEventEx(ev).Data1;
    b := self.Internal_FindBindingByMidiCC(Data1);
    if assigned(b) then
    begin
      Data2 := VstMidiEventEx(ev).Data2;
      Effect.SetParameterAutomated(b.VstParIndex, Data2 / 127);
      result := true;
    end else
    begin
      result := false;
    end;
  finally
    BindingListLock.EndRead;
  end;
end;

procedure TMidiAutomationController.ActivateMidiLearnForVstParameter(const VstParIndex: integer; const ParName : string);
begin
  MidiLearn.ParName     := ParName;
  MidiLearn.VstParIndex := VstParIndex;
  MidiLearn.IsActive    := true;
end;

procedure TMidiAutomationController.DeactiveMidiLearn;
begin
  MidiLearn.IsActive := false;
end;


procedure TMidiAutomationController.DeleteAllBindings;
begin
  BindingListLock.BeginWrite;
  try
    BindingList.Clear;
  finally
    BindingListLock.EndWrite;
  end;

end;

procedure TMidiAutomationController.DeleteBindingByMidiCC(const MidiCC: integer);
begin
  BindingListLock.BeginWrite;
  try
    Internal_DeleteBindingByMidiCC(MidiCC);
  finally
    BindingListLock.EndWrite;
  end;
end;

procedure TMidiAutomationController.DeleteBindingByParName(const ParName: string);
begin
  BindingListLock.BeginWrite;
  try
    Internal_DeleteBindingByParName(ParName);
  finally
    BindingListLock.EndWrite;
  end;
end;

procedure TMidiAutomationController.DeleteBindingByVstPar(const VstParIndex: integer);
begin
  BindingListLock.BeginWrite;
  try
    Internal_DeleteBindingByVstPar(VstParIndex);
  finally
    BindingListLock.EndWrite;
  end;
end;

function TMidiAutomationController.AddBinding(const VstParIndex: integer; const ParName: string; const MidiCC: integer): integer;
begin
  BindingListLock.BeginWrite;
  try
    result := Internal_AddBinding(VstParIndex, ParName, MidiCC);
  finally
    BindingListLock.EndWrite;
  end;
end;




procedure TMidiAutomationController.Internal_DeleteBindingByMidiCC(const MidiCC: integer);
var
  c1: Integer;
  b : TMidiBinding;
begin
  for c1 := BindingList.Count-1 downto 0 do
  begin
    b := BindingList[c1] as TMidiBinding;
    if b.MidiCC = MidiCC then
    begin
      BindingList.Delete(c1);
    end;
  end;
end;

procedure TMidiAutomationController.Internal_DeleteBindingByParName(const ParName: string);
var
  c1: Integer;
  b : TMidiBinding;
begin
  for c1 := BindingList.Count-1 downto 0 do
  begin
    b := BindingList[c1] as TMidiBinding;
    if b.ParName = ParName then
    begin
      BindingList.Delete(c1);
    end;
  end;
end;

procedure TMidiAutomationController.Internal_DeleteBindingByVstPar(const VstParIndex: integer);
var
  c1: Integer;
  b : TMidiBinding;
begin
  for c1 := BindingList.Count-1 downto 0 do
  begin
    b := BindingList[c1] as TMidiBinding;
    if b.VstParIndex = VstParIndex then
    begin
      BindingList.Delete(c1);
    end;
  end;
end;



function TMidiAutomationController.Internal_FindBindingByMidiCC(const MidiCC: integer): TMidiBinding;
var
  c1: Integer;
  b : TMidiBinding;
begin
  for c1 := BindingList.Count-1 downto 0 do
  begin
    b := BindingList[c1] as TMidiBinding;
    if b.MidiCC = MidiCC then
    begin
      result := b;
      exit; //============>> exit >>==========>>
    end;
  end;

  result := nil;
end;

function TMidiAutomationController.Internal_AddBinding(const VstParIndex: integer; const ParName: string; const MidiCC: integer):integer;
var
  b : TMidiBinding;
begin
  b := TMidiBinding.Create;
  b.ParName     := ParName;
  b.VstParIndex := VstParIndex;
  b.MidiCC      := MidiCC;
  result := BindingList.Add(b)
end;



end.
