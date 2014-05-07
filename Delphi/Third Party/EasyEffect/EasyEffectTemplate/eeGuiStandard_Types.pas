unit eeGuiStandard_Types;

interface

uses
  eeTypes, eeMidiAutomation,
  eeMidiMap;

type
  TGetParemeterDisplayFunction = function(ParameterID:integer):string of object;
  TGetParemeterLabelFunction = function(ParameterID:integer):string of object;
  TGetCurrentMidiBinding = function (ParameterID:integer; TargetType:TTargetType):integer of object;


  PVstMethodReferences = ^TVstMethodReferences;
  TVstMethodReferences = record
    SetParameterAutomated : TSetParameterProcedure;
    GetParameter          : TGetParameterFunction;
    GetParameterDisplay   : TGetParemeterDisplayFunction;
    GetParameterLabel     : TGetParemeterLabelFunction;
    BeginParameterEdit    : TBeginParameterEdit;
    EndParameterEdit      : TEndParameterEdit;
    EnableMidiLearn       : TEnableMidiLearn;   //TODO: This EnableMIDILearn isn't required anymore I believe.
    SetMidiBinding        : TSetMidiBinding;      //TODO: This midi binding event isn't required anymore I believe.
    RemoveMidiBinding     : TRemoveMidiBinding;     //TODO: This midi binding event isn't required anymore I believe.
    GetCurrentMidiBiding  : TGetCurrentMidiBinding; //TODO: This midi binding event isn't required anymore I believe.

    procedure AssignFrom(const Source:TVstMethodReferences);
  end;

implementation

{ TVstMethodReferences }

procedure TVstMethodReferences.AssignFrom(const Source: TVstMethodReferences);
begin
  Self.SetParameterAutomated   := Source.SetParameterAutomated;
  Self.GetParameter            := Source.GetParameter;
  Self.GetParameterDisplay     := Source.GetParameterDisplay;
  Self.GetParameterLabel       := Source.GetParameterLabel;
  Self.BeginParameterEdit      := Source.BeginParameterEdit;
  Self.EndParameterEdit        := Source.EndParameterEdit;
  Self.EnableMidiLearn         := Source.EnableMidiLearn;
  Self.SetMidiBinding          := Source.SetMidiBinding;
  Self.RemoveMidiBinding       := Source.RemoveMidiBinding;
  Self.GetCurrentMidiBiding    := Source.GetCurrentMidiBiding;
end;

end.
