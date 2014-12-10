unit Lucidity.GuiState;

interface

{$INCLUDE Defines.inc}

uses
  //LucidityGui.KnobHandler,
  //LucidityGui.MenuButtonHandler,
  uConstants, Lucidity.Enums, Classes, Controls,
  eeTypes;

type
  TGuiState = class
  private
    fLowerTabState: TLowerTabOptions;
    fMouseOverRegionID: TGUID;
    fCurrentModDestTarget: TModDest;
    fIsModDestAutoSelectEnabled: boolean;
    fMainGuiLayout: TMainGuiLayout;
    fActivePluginParameterID1: TPluginParameterId;
    fActivePluginParameterID2: TPluginParameterId;
    fSampleMapGroupVisibility: TGroupVisibility;
    fSampleDisplayOffset: single;
    fSampleDisplayZoom: single;
    fIsAutoSelectActive: boolean;
    fHotkeyContext: THotKeyContext;
    fIsMouseOverModSlot: boolean;
    fMouseOverModSlot: integer;
    fSelectedModSlot: integer;
    fIsGuiFirstOpen: boolean;
    procedure SetHotkeyContext(const Value: THotKeyContext);
  public
    constructor Create;
    destructor Destroy; override;

    property IsGuiFirstOpen : boolean read fIsGuiFirstOpen write fIsGuiFirstOpen;

    property MouseOverRegionID : TGUID read fMouseOverRegionID write fMouseOverRegionID;

    property ModDestTarget : TModDest read fCurrentModDestTarget write fCurrentModDestTarget;
    property IsModDestAutoSelectEnabled : boolean read fIsModDestAutoSelectEnabled write fIsModDestAutoSelectEnabled;

    property MainGuiLayout : TMainGuiLayout read fMainGuiLayout   write fMainGuiLayout;
    property LowerTabState : TLowerTabOptions read fLowerTabState write fLowerTabState;

    property SampleDisplayZoom   : single read fSampleDisplayZoom   write fSampleDisplayZoom;   //range 0..1
    property SampleDisplayOffset : single read fSampleDisplayOffset write fSampleDisplayOffset; //range 0..1


    // NOTE:
    // When published VST parameters are changed (via SetParameterAutomated()) the changes
    // are sent to the host and echo'd back to the plugin. Because of Lucidity's architecture
    // we need to filter these changes out.
    property ActivePluginParameterID1 : TPluginParameterId read fActivePluginParameterID1 write fActivePluginParameterID1;
    property ActivePluginParameterID2 : TPluginParameterId read fActivePluginParameterID2 write fActivePluginParameterID2;

    property SampleMapGroupVisibility : TGroupVisibility read fSampleMapGroupVisibility write fSampleMapGroupVisibility;

    property IsAutoSelectActive : boolean read fIsAutoSelectActive write fIsAutoSelectActive;

    property HotkeyContext : THotKeyContext read fHotkeyContext write SetHotkeyContext;

    property SelectedModSlot    : integer read fSelectedModSlot    write fSelectedModSlot;  //valid range is -1..7,
    property MouseOverModSlot   : integer read fMouseOverModSlot   write fMouseOverModSlot; //valid range is -1..7, same as SelectedModSlot.
    property IsMouseOverModSlot : boolean read fIsMouseOverModSlot write fIsMouseOverModSlot;
  end;

implementation

uses
  {$IFDEF Logging}VamLib.SmartInspect,{$ENDIF}
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

  fIsGuiFirstOpen := true;
end;

destructor TGuiState.Destroy;
begin

  inherited;
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
