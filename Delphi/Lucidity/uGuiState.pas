unit uGuiState;

interface

{$INCLUDE Defines.inc}

uses
  uConstants, uLucidityEnums, Classes, Controls,
  eeTypes;

type
  TGuiState = class
  private
    fLowerTabState: TLowerTabOptions;
    fMouseOverRegionID: TGUID;
    fCurrentModDestTarget: TModDest;
    fIsModDestAutoSelectEnabled: boolean;
    fMainGuiLayout: TMainGuiLayout;
    fActiveVstPluginParameterID: TPluginParameterId;
    fSampleMapGroupVisibility: TGroupVisibility;
    fSampleDisplayOffset: single;
    fSampleDisplayZoom: single;
    fIsAutoSelectActive: boolean;
    fHotkeyContext: THotKeyContext;
    fIsMouseOverModSlot: boolean;
    fMouseOverModSlot: integer;
    fSelectedModSlot: integer;
    procedure SetActiveVstPluginParameterID(const Value: TPluginParameterId);
    procedure SetHotkeyContext(const Value: THotKeyContext);
  public
    constructor Create;
    destructor Destroy; override;






    property MouseOverRegionID : TGUID read fMouseOverRegionID write fMouseOverRegionID;

    property ModDestTarget : TModDest read fCurrentModDestTarget write fCurrentModDestTarget;
    property IsModDestAutoSelectEnabled : boolean read fIsModDestAutoSelectEnabled write fIsModDestAutoSelectEnabled;

    property MainGuiLayout : TMainGuiLayout read fMainGuiLayout   write fMainGuiLayout;
    property LowerTabState : TLowerTabOptions read fLowerTabState write fLowerTabState;

    property SampleDisplayZoom   : single read fSampleDisplayZoom   write fSampleDisplayZoom;   //range 0..1
    property SampleDisplayOffset : single read fSampleDisplayOffset write fSampleDisplayOffset; //range 0..1


    // NOTE:
    // Normally calling SetParameterAutomated() when GUI controls are automated is the prefered way to update parameters that
    // are visible to the host application (as a VST Plugin Parameter). The host will echo the parameter change back to the
    // plugin so it can than update it's internal state. This works well with most plugins. However in the case of
    // multi-timbral plugins where a GUI control is "focused" on a particalar layer, GUI control changes are normally intended
    // to changed the "focused" element, where as, VST Plugin Parameter changes normally change all elements. (In truth this
    // depends on how the plugin developer decides to respond to plugin parameter changes. Making Vst Plugin Parameter changes
    // "Global" seems to be appropiate in my experience.)
    // Because of the above we need some way to apply parameter changes whilst filtering out the echo parameter change that is
    // received back from the host. ActiveVstPluginParameterID stores the currently active parameter on the GUI.
    // The plugin can then use this to filter out the echoed parameter changes.
    property ActiveVstPluginParameterID : TPluginParameterId read fActiveVstPluginParameterID write SetActiveVstPluginParameterID;

    property SampleMapGroupVisibility : TGroupVisibility read fSampleMapGroupVisibility write fSampleMapGroupVisibility;

    property IsAutoSelectActive : boolean read fIsAutoSelectActive write fIsAutoSelectActive;

    property HotkeyContext : THotKeyContext read fHotkeyContext write SetHotkeyContext;

    property SelectedModSlot    : integer read fSelectedModSlot    write fSelectedModSlot;  //valid range is -1..7,
    property MouseOverModSlot   : integer read fMouseOverModSlot   write fMouseOverModSlot; //valid range is -1..7, same as SelectedModSlot.
    property IsMouseOverModSlot : boolean read fIsMouseOverModSlot write fIsMouseOverModSlot;
  end;

implementation

uses
  {$IFDEF Logging}
  SmartInspectLogging,
  VamLib.LoggingProxy,
  {$ENDIF}
  VamLib.Utils,
  SysUtils,
  TypInfo;


{ TGuiState }

constructor TGuiState.Create;
begin
  fLowerTabState := TLowerTabOptions.TabMain; // shows the main tab by default.

  MouseOverRegionID := GuidEx.EmptyGuid;

  fIsModDestAutoSelectEnabled := true;
  fMainGuiLayout := TMainGuiLayout.Default;

  IsAutoSelectActive := true;

  SelectedModSlot    := -1;
  MouseOverModSlot   := -1;
  IsMouseOverModSlot := false;

end;

destructor TGuiState.Destroy;
begin

  inherited;
end;

procedure TGuiState.SetActiveVstPluginParameterID(const Value: TPluginParameterId);
begin
  fActiveVstPluginParameterID := Value;
end;

procedure TGuiState.SetHotkeyContext(const Value: THotKeyContext);
begin
  if Value <> fHotkeyContext then
  begin
    {$IFDEF Logging}
    LogMain.LogMessage('Hot Key Context Changed : ' + GetEnumName(TypeInfo(THotKeyContext),Integer(Value)));
    {$ENDIF}
    fHotkeyContext := Value;
  end;
end;

end.
