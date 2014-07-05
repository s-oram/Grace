unit eeGuiStandard_Types;

interface

uses
  eeTypes,
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
end;

end.
