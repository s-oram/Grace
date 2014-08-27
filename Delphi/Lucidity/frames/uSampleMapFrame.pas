unit uSampleMapFrame;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.UniqueID,
  VamLib.ZeroObject, VamShortMessageOverlay,
  Menu.GroupVisibility,
  Menu.SampleMapContextMenu, eeGuiStandardv2,
  eePlugin, Lucidity.Interfaces, uKeyStateTrackerOverlay,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  RedFoxWinControl, VamWinControl, VamPanel, RedFoxContainer, VamSampleMap,
  VamSamplerKeys, VamScrollBox, Vcl.Menus, RedFoxGraphicControl,
  VamGraphicControl, VamLabel, VamDiv, VamCompoundLabel, VamVisibleControl,
  VamNumericKnob,
  VamCompoundNumericKnob, VamTextBox;

type
  TSampleMapFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    ScrollBox: TVamScrollBox;
    SampleMapKeys: TVamSamplerKeys;
    SampleMap: TVamSampleMap;
    InsidePanel: TVamPanel;
    UpperPanelArea: TVamPanel;
    RegionInfoBox: TVamPanel;
    LowNoteKnob: TVamCompoundNumericKnob;
    HighNoteKnob: TVamCompoundNumericKnob;
    LowVelKnob: TVamCompoundNumericKnob;
    HighVelKnob: TVamCompoundNumericKnob;
    RootNoteKnob: TVamCompoundNumericKnob;
    CloseSampleMapButton: TVamTextBox;
    GroupVisibilityButton: TVamTextBox;
    procedure ScrollBoxScroll(Sender: TObject; Kind: TScrollEventKind; ScrollPos: Single);
    procedure SampleMapSelectRegion(const Sender: TObject; aRegion: TVamSampleRegion);
    procedure SampleMapFocusRegion(const Sender: TObject; aRegion: TVamSampleRegion);
    procedure SampleMapDeselectRegion(const Sender: TObject; aRegion: TVamSampleRegion);
    procedure SampleMapDeselectOtherRegions(const Sender: TObject; aRegion: TVamSampleRegion);
    procedure SampleMapDeselectAllRegions(Sender: TObject);
    procedure SampleMapShowRegionContextMenu(const Sender: TObject; aRegion: TVamSampleRegion);
    procedure SampleMapRegionMoved(const Sender: TObject; aRegion: TVamSampleRegion);
    procedure SampleMapRegionInfoChanged(Sender: TObject);
    procedure SampleMapMouseOverRegionChanged(const Sender: TObject; aRegion: TVamSampleRegion);
    procedure SampleMapDragSelectStart(Sender: TObject);
    procedure SampleMapDragSelectionChanged(Sender: TObject);
    procedure SampleMapDragSelectEnd(Sender: TObject);
    procedure SampleMapKeysRootKeyChanged(Sender: TObject; const RootKeyOffset: Integer);
    procedure SampleMapKeysRootKeyChanging(Sender: TObject; const NewRootKey: Integer);
    procedure SampleMapGetDragRegionCount(Sender: TObject; const Data: IVamDragData; var DragRegionCount: Integer);
    procedure SampleMapNewRegions(Sender: TObject; const aRegions: TVamSampleRegionList; const Data: IVamDragData);
    procedure SampleMapKeysMidiKeyDown(Sender: TObject;
      const KeyIndex: Integer);
    procedure SampleMapKeysMidiKeyUp(Sender: TObject; const KeyIndex: Integer);
    procedure SampleMapNewCopiedRegions(Sender: TObject;
      const aRegions: TVamSampleRegionList);
    procedure SampleMapReplaceRegion(Sender: TObject; const NewRegion,
      OldRegion: TVamSampleRegion; const Data: IVamDragData);
    procedure SampleMapOleDragEnter(Sender: TObject; ShiftState: TShiftState;
      APoint: TPoint; var Effect: Integer; Data: IVamDragData);
    procedure SampleMapShowReplaceRegionMessage(Sender: TObject;
      Value: Boolean);
    procedure CloseSampleMapButtonClick(Sender: TObject);
    procedure GroupVisibilityButtonMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GroupVisibilityButtonMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    fPlugin: TeePlugin;
    function GetScrollPosX: single;
    function GetScrollPosY: single;
    procedure SetScollPosX(const Value: single);
    procedure SetScollPosY(const Value: single);
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IZeroMessageData); 
    procedure UpdateGroupVisibility;
  protected
    MessageOverlay         : TVamShortMessageOverlay;
    MessageOverlayAnimateID : TUniqueID;

    KeyStateTrackerOverlay : TKeyStateTrackerOverlay;
    SampleMapMenu : TSampleMapContextMenu;
    GroupVisibilityMenu : TGroupVisibilityMenu; 

    ThrottleID : TUniqueID;
    procedure ScrollPosChanged;

    procedure UpdateRegionInfoControls(const SampleName, LowNote, HighNote, LowVel, HighVel, RootNote : string); overload;
    procedure UpdateRegionInfoControls(const SampleName : string; const LowNote, HighNote, LowVel, HighVel, RootNote : integer); overload;

    procedure Handle_KnobStepUp(Sender : TObject);
    procedure Handle_KnobStepDown(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);

    procedure UpdateSampleRegions;
    procedure UpdateRegionInfoDisplay;
    procedure UpdateRootNoteKeys;

    procedure MidiKeyChanged;
  published
    property ScrollPosX : single read GetScrollPosX write SetScollPosX;
    property ScrollPosY : single read GetScrollPosY write SetScollPosY;

    property Plugin:TeePlugin read fPlugin;
  end;

implementation

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  {$IFDEF Logging}VamLib.LoggingProxy,{$ENDIF}
  uGuiUtils,
  VamLib.Throttler,
  VamLib.Animation,
  VamLib.Threads,
  VamLib.Utils,
  SampleMapFrame.Extra,
  Lucidity.Types,
  uLucidityExtra,
  uLucidityEnums,
  eeVstXml,
  RedFoxColor, eePitch,
  VamLayoutWizard,
  VamKeyStateTracker, GuidEx,
  Lucidity.PluginParameters,
  uConstants, Lucidity.SampleMap, Lucidity.KeyGroup;

{$R *.dfm}

constructor TSampleMapFrame.Create(AOwner: TComponent);
begin
  inherited;

  ThrottleID.Init;

  MessageOverlayAnimateID.Init;

  SampleMapMenu := TSampleMapContextMenu.Create;

  ScrollPosChanged;

  KeyStateTrackerOverlay := TKeyStateTrackerOverlay.Create(AOwner);

  KeyStateTrackerOverlay.Parent  := SampleMap;
  KeyStateTrackerOverlay.Align := alClient;
  KeyStateTrackerOverlay.Visible := true;
  KeyStateTrackerOverlay.HitTest := false;

  MessageOverlay := TVamShortMessageOverlay.Create(aOwner);
  MessageOverlay.Visible := false;
  MessageOverlay.Align := alClient;
  MessageOverlay.Parent := SampleMap;
  MessageOverlay.HitTest := false;
  MessageOverlay.AutoSizeBackground := true;
  MessageOverlay.TextPadding.SetBounds(10,6,10,6);
  MessageOverlay.BorderWidth := 3;
  MessageOverlay.CornerRadius1 := 3;
  MessageOverlay.CornerRadius2 := 3;
  MessageOverlay.CornerRadius3 := 3;
  MessageOverlay.CornerRadius4 := 3;
  MessageOverlay.Color     := kColor_LcdDark6;
  MessageOverlay.ColorText := kColor_LcdDark1;
  MessageOverlay.ColorBorder := kColor_LcdDark1;
  MessageOverlay.ShowBorder := true;


end;

destructor TSampleMapFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  KeyStateTrackerOverlay.Free;
  SampleMapMenu.Free;

  if assigned(GroupVisibilityMenu) then GroupVisibilityMenu.Free;
  

  inherited;
end;

procedure TSampleMapFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
begin
  fPlugin := aPlugin;
  UpdateSampleRegions;

  SampleMapMenu.Initialize(aPlugin);

  UpperPanelArea.Height := 24;

  RegionInfoBox.AlignWithMargins := true;
  RegionInfoBox.Margins.SetBounds(0,0,0,0);
  RegionInfoBox.Align := alLeft;
  RegionInfoBox.Width := 412;
  RegionInfoBox.Color := kColor_LcdDark1;
  RegionInfoBox.CornerRadius1 := 3;
  RegionInfoBox.CornerRadius2 := 3;
  RegionInfoBox.CornerRadius3 := 3;
  RegionInfoBox.CornerRadius4 := 3;
  RegionInfoBox.Padding.SetBounds(10,2,10,2);

  CloseSampleMapButton.AlignWithMargins := true;
  CloseSampleMapButton.Margins.SetBounds(0,0,0,0);
  CloseSampleMapButton.Align := alRight;
  CloseSampleMapButton.Width := 80;
  GuiSetup.StyleButton_CommandButton_Bright(CloseSampleMapButton);

  GuiSetup.StyleButton_SelectorButton(GroupVisibilityButton);
  GroupVisibilityButton.Align := alClient;
  GroupVisibilityButton.Width := 80;
  GroupVisibilityButton.Margins.SetBounds(4,0,4,0);
  GroupVisibilityButton.AlignWithMargins := true;



  SampleMapRegionInfoChanged(self);

  LowVelKnob.Color_Label   := GetRedFoxColor(kColor_LcdDark4);
  LowVelKnob.Color_Numeric := GetRedFoxColor(kColor_LcdDark5);
  LowVelKnob.Color_Arrows1 := '$33FFFFFF';
  LowVelKnob.Color_Arrows2 := '$ccFFFFFF';
  LowVelKnob.KnobDecimalPlaces := 0;
  LowVelKnob.Margins.SetBounds(8,0,0,0);
  LowVelKnob.Width := 70;
  LowVelKnob.KnobNumericStyle := nsCustom;
  LowVelKnob.OnRotaryStepUp   := Handle_KnobStepUp;
  LowVelKnob.OnRotaryStepDown := Handle_KnobStepDown;

  HighVelKnob.Color_Label   := GetRedFoxColor(kColor_LcdDark4);
  HighVelKnob.Color_Numeric := GetRedFoxColor(kColor_LcdDark5);
  HighVelKnob.Color_Arrows1 := '$33FFFFFF';
  HighVelKnob.Color_Arrows2 := '$ccFFFFFF';
  HighVelKnob.KnobDecimalPlaces := 0;
  HighVelKnob.Margins.SetBounds(8,0,0,0);
  HighVelKnob.Width := 70;
  HighVelKnob.KnobNumericStyle := nsCustom;
  HighVelKnob.OnRotaryStepUp   := Handle_KnobStepUp;
  HighVelKnob.OnRotaryStepDown := Handle_KnobStepDown;

  LowNoteKnob.Color_Label   := GetRedFoxColor(kColor_LcdDark4);
  LowNoteKnob.Color_Numeric := GetRedFoxColor(kColor_LcdDark5);
  LowNoteKnob.Color_Arrows1 := '$33FFFFFF';
  LowNoteKnob.Color_Arrows2 := '$ccFFFFFF';
  LowNoteKnob.KnobDecimalPlaces := 0;
  LowNoteKnob.Margins.SetBounds(8,0,0,0);
  LowNoteKnob.Width := 80;
  LowNoteKnob.KnobNumericStyle := nsCustom;
  LowNoteKnob.OnRotaryStepUp   := Handle_KnobStepUp;
  LowNoteKnob.OnRotaryStepDown := Handle_KnobStepDown;

  HighNoteKnob.Color_Label   := GetRedFoxColor(kColor_LcdDark4);
  HighNoteKnob.Color_Numeric := GetRedFoxColor(kColor_LcdDark5);
  HighNoteKnob.Color_Arrows1 := '$33FFFFFF';
  HighNoteKnob.Color_Arrows2 := '$ccFFFFFF';
  HighNoteKnob.KnobDecimalPlaces := 0;
  HighNoteKnob.Margins.SetBounds(8,0,0,0);
  HighNoteKnob.Width := 80;
  HighNoteKnob.KnobNumericStyle := nsCustom;
  HighNoteKnob.OnRotaryStepUp   := Handle_KnobStepUp;
  HighNoteKnob.OnRotaryStepDown := Handle_KnobStepDown;

  RootNoteKnob.Color_Label   := GetRedFoxColor(kColor_LcdDark4);
  RootNoteKnob.Color_Numeric := GetRedFoxColor(kColor_LcdDark5);
  RootNoteKnob.Color_Arrows1 := '$33FFFFFF';
  RootNoteKnob.Color_Arrows2 := '$ccFFFFFF';
  RootNoteKnob.KnobDecimalPlaces := 0;
  RootNoteKnob.Margins.SetBounds(8,0,0,0);
  RootNoteKnob.Width := 60;
  RootNoteKnob.KnobNumericStyle := nsCustom;
  RootNoteKnob.OnRotaryStepUp   := Handle_KnobStepUp;
  RootNoteKnob.OnRotaryStepDown := Handle_KnobStepDown;


  LowVelKnob.Align   := TAlign.alNone;
  HighVelKnob.Align  := TAlign.alNone;
  LowNoteKnob.Align  := TAlign.alNone;
  HighNoteKnob.Align := TAlign.alNone;
  RootNoteKnob.Align := TAlign.alNone;

  LowVelKnob.Layout.SetPos(8,1);
  HighVelKnob.Layout.Anchor(LowVelKnob).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  LowNoteKnob.Layout.Anchor(HighVelKnob).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  HighNoteKnob.Layout.Anchor(LowNoteKnob).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  RootNoteKnob.Layout.Anchor(HighNoteKnob).SnapToEdge(TControlFeature.RightEdge).Move(8,0);

  //UpperPanelArea.Color := kColor_LcdDark1;
  UpperPanelArea.Color := kPanelLight;


  UpdateGroupVisibility;
end;


function TSampleMapFrame.GetScrollPosX: single;
begin
  result := ScrollBox.ScrollXPos;
end;

function TSampleMapFrame.GetScrollPosY: single;
begin
  result := ScrollBox.ScrollYPos;
end;

procedure TSampleMapFrame.GroupVisibilityButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    case Plugin.Globals.GuiState.SampleMapGroupVisibility of
      TGroupVisibility.AllGroups:     Plugin.Globals.GuiState.SampleMapGroupVisibility := TGroupVisibility.SelectedGroup;
      TGroupVisibility.SelectedGroup: Plugin.Globals.GuiState.SampleMapGroupVisibility := TGroupVisibility.AllGroups;
    else
      raise Exception.Create('type not handled.');
    end;

    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.GroupVisibilityChanged);
  end;
end;

procedure TSampleMapFrame.GroupVisibilityButtonMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = TMouseButton.mbRight then
  begin
    if not assigned(GroupVisibilityMenu) then
    begin
      GroupVisibilityMenu := TGroupVisibilityMenu.Create;
      GroupVisibilityMenu.Initialize(Plugin, nil);
    end;

    GroupVisibilityMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TSampleMapFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TSampleMapFrame.SetScollPosX(const Value: single);
begin
  ScrollBox.ScrollXPos := Value;
  ScrollPosChanged;
end;

procedure TSampleMapFrame.SetScollPosY(const Value: single);
begin
  ScrollBox.ScrollYPos := Value;
  ScrollPosChanged;
end;



procedure TSampleMapFrame.ScrollBoxScroll(Sender: TObject; Kind: TScrollEventKind; ScrollPos: Single);
begin
  ScrollPosChanged;
end;

procedure TSampleMapFrame.ScrollPosChanged;
var
  x : single;
begin
  x := 1 - ScrollBox.ScrollYPos;
  if x < 0.1 then x := 0.1;
  ScrollBox.ScrollXIndexSize := x;

  SampleMap.Zoom   := ScrollBox.ScrollYPos;
  SampleMap.Offset := ScrollBox.ScrollXPos;

  SampleMapKeys.Zoom   := ScrollBox.ScrollYPos;
  SampleMapKeys.Offset := ScrollBox.ScrollXPos;

  if assigned(KeyStateTrackerOverlay) then
  begin
    KeyStateTrackerOverlay.Zoom   := ScrollBox.ScrollYPos;
    KeyStateTrackerOverlay.Offset := ScrollBox.ScrollXPos;
  end;
end;



procedure TSampleMapFrame.UpdateSampleRegions;
var
  DisplayRegion : TVamSampleRegion;
  MapRegion     : IRegion;
  c1: Integer;
  id : TGUID;
  KG : IKeyGroup;
  NameA, NameB : string;

  Text : string;
begin
  if not assigned(Plugin) then exit;

  SampleMap.BeginUpdate;
  try
    KG := Plugin.FocusedKeyGroup;

    if not assigned(KG) then
    begin
      SampleMap.SampleRegions.Clear;
    end else
    begin
      //Delete any items that aren't in the plugins sample map.
      for c1 := SampleMap.SampleRegions.Count-1 downto 0 do
      begin
        id := SampleMap.SampleRegions[c1].UniqueID;
        if Plugin.SampleMap.FindRegionByUniqueID(id) = nil then
        begin
          SampleMap.SampleRegions.Delete(c1);
        end;
      end;

      //Update the sample map display to match the Lucidities sample map.
      for c1 := 0 to Plugin.SampleMap.RegionCount-1 do
      begin
        MapRegion := Plugin.SampleMap.Regions[c1];
        id := MapRegion.GetProperties^.UniqueID;
        DisplayRegion := SampleMap.FindRegionByUniqueID(id);

        if (DisplayRegion = nil) then
        begin
          DisplayRegion := TVamSampleRegion.Create;
          SampleMap.SampleRegions.Add(DisplayRegion);
          SampleMap.MoveRegionToFront(DisplayRegion);
        end;

        DisplayRegion.UniqueID      := MapRegion.GetProperties^.UniqueID;
        DisplayRegion.FileName      := MapRegion.GetProperties^.SampleFileName;
        DisplayRegion.IsSampleError := MapRegion.GetProperties^.IsSampleError;
        DisplayRegion.LowKey        := MapRegion.GetProperties^.LowNote;
        DisplayRegion.HighKey       := MapRegion.GetProperties^.HighNote;
        DisplayRegion.LowVelocity   := MapRegion.GetProperties^.LowVelocity;
        DisplayRegion.HighVelocity  := MapRegion.GetProperties^.HighVelocity;
        DisplayRegion.RootNote      := MapRegion.GetProperties^.RootNote;
        DisplayRegion.IsSelected    := MapRegion.GetProperties^.IsSelected;
        DisplayRegion.IsFocused     := MapRegion.GetProperties^.IsFocused;

        Text := IntToStr(MapRegion.GetProperties^.LowNote) + ' ' + IntToStr(MapRegion.GetProperties^.HighNote)  + ' ' + IntToStr(MapRegion.GetProperties^.LowVelocity)  + ' ' + IntToStr(MapRegion.GetProperties^.HighVelocity);
        Text := 'Region Bounds (' + IntToStr(c1 + 1) + ') ' + Text;


        NameA := MapRegion.GetKeyGroup.GetName;
        NameB := KG.GetName;

        if (NameA = NameB)
          then DisplayRegion.IsInOtherKeyGroup := false
          else DisplayRegion.IsInOtherKeyGroup := true;

        if Plugin.Globals.GuiState.SampleMapGroupVisibility = TGroupVisibility.AllGroups then
        begin
          DisplayRegion.IsVisible := true;
        end else
        begin
          if (NameA = NameB)
            then DisplayRegion.IsVisible := true
            else DisplayRegion.IsVisible := false;
        end;
      end;
    end;
  finally
    SampleMap.EndUpdate;
    SampleMap.Invalidate;
  end;
end;

procedure TSampleMapFrame.SampleMapSelectRegion(const Sender: TObject; aRegion: TVamSampleRegion);
begin
  Plugin.SampleMap.SelectRegion(aRegion.UniqueID);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
end;

procedure TSampleMapFrame.SampleMapFocusRegion(const Sender: TObject; aRegion: TVamSampleRegion);
begin
  if not assigned(Plugin) then exit;

  if assigned(aRegion) then
  begin
    //bring the region into focus
    Plugin.FocusRegion(aRegion.UniqueID)
  end else
  begin
    Plugin.ClearFocus;
  end;
end;



procedure TSampleMapFrame.SampleMapDeselectRegion(const Sender: TObject; aRegion: TVamSampleRegion);
begin
  if not assigned(Plugin) then exit;

  Plugin.SampleMap.DeselectRegion(aRegion.UniqueID);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
end;



procedure TSampleMapFrame.SampleMapDeselectOtherRegions(const Sender: TObject; aRegion: TVamSampleRegion);
var
  SG : IKeyGroup;
begin
  if not assigned(Plugin) then exit;

  SG := Plugin.FocusedKeyGroup;
  Plugin.SampleMap.DeselectOtherRegions(aRegion.UniqueID);
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
end;

procedure TSampleMapFrame.SampleMapDeselectAllRegions(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  Plugin.SampleMap.DeselectAllRegions;
  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
end;

procedure TSampleMapFrame.SampleMapShowRegionContextMenu(const Sender: TObject; aRegion: TVamSampleRegion);
var
  RegionContext : IRegion;
begin
  RegionContext := Plugin.SampleMap.FindRegionByUniqueID(aRegion.UniqueID);

  SampleMapMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y, RegionContext);
end;

procedure TSampleMapFrame.SampleMapShowReplaceRegionMessage(Sender: TObject; Value: Boolean);
begin
  if Value
    then Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_ShowReplaceRegionMessage)
    else Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.Command_HideReplaceRegionMessage);

end;

procedure TSampleMapFrame.SampleMapRegionMoved(const Sender: TObject; aRegion: TVamSampleRegion);
var
  MapRegion : IRegion;
begin
  MapRegion := Plugin.SampleMap.FindRegionByUniqueID(aRegion.UniqueID);

  if assigned(MapRegion) then
  begin
    MapRegion.GetProperties^.LowNote      := aRegion.MovedLowKey;
    MapRegion.GetProperties^.HighNote     := aRegion.MovedHighKey;
    MapRegion.GetProperties^.LowVelocity  := aRegion.MovedLowVelocity;
    MapRegion.GetProperties^.HighVelocity := aRegion.MovedHighVelocity;
    MapRegion.GetProperties^.RootNote     := aRegion.MovedRootNote;
  end;

  Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleRegionChanged);
end;

procedure TSampleMapFrame.MidiKeyChanged;
begin
  if not assigned(Plugin) then exit;

  // Update the Sample map components with current key state data. (ie. What midi keys have
  // been pressed and released.)
  Plugin.KeyStateTracker.GetData(KeyStateTrackerOverlay.GetKeyStateData^);
  Plugin.KeyStateTracker.GetData(SampleMapKeys.GetKeyStateData^);

  KeyStateTrackerOverlay.Invalidate;
  SampleMapKeys.Invalidate;
end;

procedure TSampleMapFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IZeroMessageData);
var
  aniObj : TByteAnimation;
begin
  if MsgID = TLucidMsgID.Msg_XRegionsDuplicated then
  begin
    MessageOverlay.Text := string(Data^);
    MessageOverlay.Opacity := 255;
    MessageOverlay.Visible := true;

    AniObj := TByteAnimation.Create;
    AniObj.StartValue := 255;
    AniObj.EndValue   := 0;
    AniObj.RunTime       := 1400;
    AniObj.ApplyMethod := procedure(CurrentValue:Byte)
    begin
      MessageOverlay.Opacity := CurrentValue;
    end;

    GlobalAnimator.Animate(MessageOverlayAnimateID, AniObj);
  end;

  if MsgID = TLucidMsgID.MidiKeyChanged then
  begin
    MidiKeyChanged;
  end;

  if MsgID = TLucidMsgID.SampleFocusChanged then
  begin
    UpdateRegionInfoDisplay;
    UpdateSampleRegions;
  end;

  if MsgID = TLucidMsgId.SampleRegionChanged then
  begin
    UpdateSampleRegions;
    UpdateRootNoteKeys;
    UpdateRegionInfoDisplay;
  end;

  if MsgID = TLucidMsgID.GroupVisibilityChanged then UpdateGroupVisibility;

end;

procedure TSampleMapFrame.SampleMapRegionInfoChanged(Sender: TObject);
begin
  //TODO:HIGH AVBug #2
  UpdateRegionInfoDisplay;
end;

procedure TSampleMapFrame.SampleMapMouseOverRegionChanged(const Sender: TObject; aRegion: TVamSampleRegion);
begin
  if not assigned(Plugin) then exit;

  if assigned(aRegion)
    then Plugin.Globals.GuiState.MouseOverRegionID := aRegion.UniqueID
    else Plugin.Globals.GuiState.MouseOverRegionID := TGuidEx.EmptyGuid;

  UpdateRegionInfoDisplay;
  Plugin.Globals.MotherShip.MsgVCL(TLucidMsgID.MouseOverSampleRegionChanged);
end;



procedure TSampleMapFrame.SampleMapDragSelectStart(Sender: TObject);
begin
  UpdateRegionInfoDisplay;
end;

procedure TSampleMapFrame.SampleMapDragSelectionChanged(Sender: TObject);
begin
  UpdateRegionInfoDisplay;
end;

procedure TSampleMapFrame.SampleMapDragSelectEnd(Sender: TObject);
begin
  UpdateRegionInfoDisplay;
end;

procedure TSampleMapFrame.UpdateRegionInfoDisplay;
  procedure SetRegionInfoControlVisibility(const IsVisible : boolean);
  begin
    LowVelKnob.Visible   := IsVisible;
    HighVelKnob.Visible  := IsVisible;
    LowNoteKnob.Visible  := IsVisible;
    HighNoteKnob.Visible := IsVisible;
    RootNoteKnob.Visible := IsVisible;
  end;
var
  DragSelectedCount : integer;
  SelectedCount     : integer;
  Info : TVamSampleMapDisplayInfo;
  MouseOverRegionInfo : TVamSampleMapDisplayInfo;
  SampleNameText : string;
begin
  DragSelectedCount   := SampleMap.GetDragSelectCount;
  SelectedCount       := SampleMap.GetSelectedCount;
  MouseOverRegionInfo := SampleMap.GetMouseOverRegionInfo;

  if (DragSelectedCount = -1) and (MouseOverRegionInfo.IsValid) then
  begin
    SetRegionInfoControlVisibility(true);

    Info := MouseOverRegionInfo;

    if SelectedCount <= 1
      then SampleNameText := ExtractFilename(Info.FileName)
      else SampleNameText := ExtractFilename(Info.FileName) + '  (' + IntToStr(SelectedCount) + ' regions selected)';

    UpdateRegionInfoControls(SampleNameText, Info.LowKey, Info.HighKey, Info.LowVelocity, Info.HighVelocity, Info.RootNote);
  end;


  if (DragSelectedCount = -1) and (SelectedCount = 0) and (MouseOverRegionInfo.IsValid = false) then
  begin
    SetRegionInfoControlVisibility(false);
    UpdateRegionInfoControls('', '', '', '', '', '');
  end;

  if (DragSelectedCount = -1) and (SelectedCount = 1) and (MouseOverRegionInfo.IsValid = false) then
  begin
    Info := SampleMap.GetDisplayInfo;

    if Info.IsValid then
    begin
      SetRegionInfoControlVisibility(true);

      SampleNameText := ExtractFilename(Info.FileName);
      UpdateRegionInfoControls(SampleNameText, Info.LowKey, Info.HighKey, Info.LowVelocity, Info.HighVelocity, Info.RootNote);
    end;
  end;

  if (DragSelectedCount = -1) and (SelectedCount > 1) and (MouseOverRegionInfo.IsValid = false) then
  begin
    SetRegionInfoControlVisibility(true);

    SampleNameText := '(' + IntToStr(SelectedCount) + ' regions selected)';
    UpdateRegionInfoControls(SampleNameText, '-', '-', '-', '-', '-');
  end;

  if (DragSelectedCount > 0) and (MouseOverRegionInfo.IsValid = false) then
  begin
    SetRegionInfoControlVisibility(true);

    if DragSelectedCount = 1
      then SampleNameText := '(' + IntToStr(DragSelectedCount) + ' region selected)'
      else SampleNameText := '(' + IntToStr(DragSelectedCount) + ' regions selected)';

    UpdateRegionInfoControls(SampleNameText, '-', '-', '-', '-', '-');
  end;

  UpdateRootNoteKeys; // Don't delete.
end;



procedure TSampleMapFrame.UpdateRootNoteKeys;
var
  c1 : integer;
  DisplayRegion : TVamSampleRegion;
  RootNote : integer;
  MouseOverRegionInfo : TVamSampleMapDisplayInfo;
  DragSelectedCount : integer;
begin
  //reset the sample map keys root notes.
  for c1 := 0 to 127 do
  begin
    SampleMapKeys.Key_IsRootNote[c1] := false;
  end;

  for c1 := 0 to SampleMap.SampleRegions.Count-1 do
  begin
    DisplayRegion := SampleMap.SampleRegions[c1];

    if (DisplayRegion.IsVisible) and ((DisplayRegion.IsSelected) xor (DisplayRegion.IsDragSelected)) then
    begin
      if DisplayRegion.IsMoving = false
        then RootNote := DisplayRegion.RootNote
        else RootNOte := DisplayRegion.MovedRootNote;

      if (RootNote >= 0) and (RootNote <= 127)
        then SampleMapKeys.Key_IsRootNote[RootNote] := true;
    end;
  end;


  DragSelectedCount   := SampleMap.GetDragSelectCount;
  MouseOverRegionInfo := SampleMap.GetMouseOverRegionInfo;
  if (DragSelectedCount = -1) and (MouseOverRegionInfo.IsValid) then
  begin
    RootNote := MouseOverRegionInfo.RootNote;
    if (RootNote >= 0) and (RootNote <= 127)
      then SampleMapKeys.Key_IsRootNote[RootNote] := true;
  end;

  SampleMapKeys.Invalidate;

end;

procedure TSampleMapFrame.SampleMapKeysRootKeyChanged(Sender: TObject; const RootKeyOffset: Integer);
begin
  Plugin.MoveRootKey(RootKeyOffset);
end;

procedure TSampleMapFrame.SampleMapKeysRootKeyChanging(Sender: TObject; const NewRootKey: Integer);
begin
  {
  if NewRootKey <> -1
    then RegionRootInfo.Text2 := MidiToName(NewRootKey)
    else RegionRootInfo.Text2 := '-';
  }
end;

procedure TSampleMapFrame.SampleMapGetDragRegionCount(Sender: TObject; const Data: IVamDragData; var DragRegionCount: Integer);
var
  XmlRegionCount : integer;
begin
  XmlRegionCount := GetVstXmlRegionCount(Data.GetText);

  if XmlRegionCount <> 0
    then DragRegionCount := XmlRegionCount
    else DragRegionCount := Data.GetFiles.Count;

end;

procedure TSampleMapFrame.SampleMapNewRegions(Sender: TObject; const aRegions: TVamSampleRegionList; const Data: IVamDragData);
type
  TDropType = (dtFiles, dtVstXml);
var
  c1: Integer;
  RegionCreateInfo : TRegionCreateInfo;
  DropType : TDropType;
  XmlRegionCount : integer;
  XmlRegionInfo : TVstXmlRegion;
  NewRegionCount : integer;
  NewRegions : array of IRegion;
begin
  if not assigned(Plugin) then exit;

  NewRegionCount := aRegions.Count;
  SetLength(NewRegions, aRegions.Count);
  try
    Plugin.StopPreview;

    //====== Load the new regions ==============================================
    XmlRegionCount := GetVstXmlRegionCount(Data.GetText);

    if XmlRegionCount > 0
      then DropType := TDropType.dtVstXml
      else DropType := TDropType.dtFiles;

    for c1 := 0 to aRegions.Count-1 do
    begin
      RegionCreateInfo.KeyGroup      := Plugin.FocusedKeyGroup;
      RegionCreateInfo.LowNote       := aRegions[c1].LowKey;
      RegionCreateInfo.HighNote      := aRegions[c1].HighKey;
      RegionCreateInfo.LowVelocity   := aRegions[c1].LowVelocity;
      RegionCreateInfo.HighVelocity  := aRegions[c1].HighVelocity;
      RegionCreateInfo.RootNote      := aRegions[c1].RootNote;

      case DropType of
        dtFiles:
        begin
          RegionCreateInfo.AudioFileName := Data.GetFiles[c1];
        end;

        dtVstXml:
        begin
          XmlRegionInfo := GetVstXmlRegionInfo(c1, Data.GetText);
          RegionCreateInfo.AudioFileName := XmlRegionInfo.FileName;
        end
      else
        raise Exception.Create('Drop type not handled.');
      end;

      NewRegions[c1] := Plugin.NewRegion(RegionCreateInfo);
    end;



    //====== update sample region selections ====================================
    Plugin.ClearSelected;

    if NewRegionCount = 1 then
    begin
      if assigned(NewRegions[0]) then
      begin
        Plugin.FocusRegion(NewRegions[0].GetProperties^.UniqueID);
      end;
    end;


    if NewRegionCount > 1 then
    begin
      for c1 := 0 to NewRegionCount-1 do
      begin
        if assigned(NewRegions[c1]) then
        begin
          Plugin.SelectRegion(NewRegions[c1].GetProperties^.UniqueID);
        end;
      end;
    end;

    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
  finally
    SetLength(NewRegions, 0);
  end;
end;

procedure TSampleMapFrame.SampleMapOleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data: IVamDragData);
begin
  //Plugin.ClearSelected;
end;

procedure TSampleMapFrame.SampleMapReplaceRegion(Sender: TObject; const NewRegion, OldRegion: TVamSampleRegion; const Data: IVamDragData);
type
  TDropType = (dtFiles, dtVstXml);
var
  RegionCreateInfo : TRegionCreateInfo;
  fn : string;
  DropType : TDropType;
  XmlRegionCount : integer;
  XmlRegionInfo : TVstXmlRegion;
  NewSR : IRegion;
  curRG : IRegion;
  newRG : IRegion;
begin
  if not assigned(Plugin) then exit;
  if not assigned(OldRegion) then exit;

  Plugin.StopPreview;

  //====== Load the new regions ==============================================
  XmlRegionCount := GetVstXmlRegionCount(Data.GetText);

  if XmlRegionCount > 0
    then DropType := TDropType.dtVstXml
    else DropType := TDropType.dtFiles;

  case DropType of
    dtFiles:
    begin
      fn := Data.GetFiles[0];
    end;
    dtVstXml:
    begin
      XmlRegionInfo := GetVstXmlRegionInfo(0, Data.GetText);
      fn := XmlRegionInfo.FileName;
    end
  else
    raise Exception.Create('Drop type not handled.');
  end;

  curRg := Plugin.SampleMap.FindRegionByUniqueID(OldRegion.UniqueID);
  if not assigned(CurRG) then exit;

  if IsSupportedAudioFormat(fn) then
  begin
    NewRG := Plugin.ReplaceSample(CurRG, fn);
    if assigned(NewRegion) then
    begin
      Plugin.FocusRegion(NewRG.GetProperties^.UniqueID);
      Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleFocusChanged);
    end else
    begin
      // TODO:HIGH display error message here.
    end;
  end;
end;



procedure TSampleMapFrame.SampleMapNewCopiedRegions(Sender: TObject; const aRegions: TVamSampleRegionList);
var
  c1: Integer;
  RegionCreateInfo : TRegionCreateInfo;
  NewRegionCount : integer;
  NewRegions : array of IRegion;
begin
  if not assigned(Plugin) then exit;

  NewRegionCount := aRegions.Count;
  SetLength(NewRegions, aRegions.Count);
  try
    Plugin.StopPreview;

    //====== Load the new regions ==============================================
    for c1 := 0 to aRegions.Count-1 do
    begin
      RegionCreateInfo.KeyGroup      := Plugin.FocusedKeyGroup;
      RegionCreateInfo.LowNote       := aRegions[c1].MovedLowKey;
      RegionCreateInfo.HighNote      := aRegions[c1].MovedHighKey;
      RegionCreateInfo.LowVelocity   := aRegions[c1].MovedLowVelocity;
      RegionCreateInfo.HighVelocity  := aRegions[c1].MovedHighVelocity;
      RegionCreateInfo.RootNote      := aRegions[c1].MovedRootNote;
      RegionCreateInfo.AudioFileName := aRegions[c1].FileName;

      NewRegions[c1] := Plugin.NewRegion(RegionCreateInfo);

      Text := IntToStr(RegionCreateInfo.LowNote) + ' ' + IntToStr(RegionCreateInfo.HighNote)  + ' ' + IntToStr(RegionCreateInfo.LowVelocity)  + ' ' + IntToStr(RegionCreateInfo.HighVelocity);
      Text := 'New Copy Region Bounds (' + IntToStr(c1 + 1) + ') ' + Text;
    end;



    //====== update sample region selections ====================================
    Plugin.ClearSelected;


    if NewRegionCount >= 1 then
    begin
      for c1 := 0 to NewRegionCount-1 do
      begin
        if assigned(NewRegions[c1]) then
        begin
          Plugin.SelectRegion(NewRegions[c1].GetProperties^.UniqueID);
        end;
      end;

      Plugin.FocusRegion(NewRegions[0].GetProperties^.UniqueID);
    end;

    Plugin.Globals.MotherShip.MsgVcl(TLucidMsgID.SampleRegionChanged);
  finally
    SetLength(NewRegions, 0);
  end;
end;




procedure TSampleMapFrame.SampleMapKeysMidiKeyDown(Sender: TObject; const KeyIndex: Integer);
begin
  Plugin.TriggerNoteOn(KeyIndex, 90);
end;

procedure TSampleMapFrame.SampleMapKeysMidiKeyUp(Sender: TObject; const KeyIndex: Integer);
begin
  Plugin.TriggerNoteOff(KeyIndex, 0);
end;

procedure TSampleMapFrame.UpdateRegionInfoControls(const SampleName, LowNote, HighNote, LowVel, HighVel, RootNote: string);
begin
  LowNoteKnob.KnobCustomText  := LowNote;
  HighNoteKnob.KnobCustomText := HighNote;
  LowVelKnob.KnobCustomText   := LowVel;
  HighVelKnob.KnobCustomText  := HighVel;
  RootNoteKnob.KnobCustomText := RootNote;
end;

procedure TSampleMapFrame.UpdateGroupVisibility;
begin
  case Plugin.Globals.GuiState.SampleMapGroupVisibility of
    TGroupVisibility.AllGroups:     GroupVisibilityButton.Text := 'All Groups';
    TGroupVisibility.SelectedGroup: GroupVisibilityButton.Text := 'Cur Group';
  else
    raise Exception.Create('type not handled.');
  end;

  UpdateSampleRegions;
end;

procedure TSampleMapFrame.UpdateRegionInfoControls(const SampleName: string; const LowNote, HighNote, LowVel, HighVel, RootNote: integer);
begin
  LowNoteKnob.KnobCustomText  := MidiNoteToName(LowNote);
  HighNoteKnob.KnobCustomText := MidiNoteToName(HighNote);
  LowVelKnob.KnobCustomText   := IntToStr(LowVel);
  HighVelKnob.KnobCustomText  := IntToStr(HighVel);
  RootNoteKnob.KnobCustomText := MidiNoteToName(RootNote);
end;

procedure TSampleMapFrame.Handle_KnobStepDown(Sender: TObject);
var
  RegionList : TGuidList;
  c1 : integer;
begin
  RegionList := TGuidList.Create;
  AutoFree(@RegionList);

  for c1 := 0 to SampleMap.SampleRegions.Count-1 do
  begin
    if SampleMap.SampleRegions[c1].IsSelected
      then RegionList.Add(SampleMap.SampleRegions[c1].UniqueID);
  end;

  if Sender = LowNoteKnob  then TAdjustRegions.DecLowNote(Plugin, RegionList);
  if Sender = HighNoteKnob then TAdjustRegions.DecHighNote(Plugin, RegionList);

  if Sender = LowVelKnob   then TAdjustRegions.DecLowVel(Plugin, RegionList);
  if Sender = HighVelKnob  then TAdjustRegions.DecHighVel(Plugin, RegionList);

  if Sender = RootNoteKnob then TAdjustRegions.DecRootNote(Plugin, RegionList);


  UpdateSampleRegions;
  UpdateRegionInfoDisplay;


  // TODO: There is a bug here. For some reason the sample map frame numeric knobs
  // are only updated to show the correct value after the SampleRegionChanged message
  // has been set. This results in a very slight delay for the final value to show.
  // It's not a showstopper bug but would be great to fix.
  Throttle(ThrottleID, 25, procedure
  begin
    Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleRegionChanged, nil);
  end);

end;

procedure TSampleMapFrame.Handle_KnobStepUp(Sender: TObject);
var
  RegionList : TGuidList;
  c1 : integer;
begin
  RegionList := TGuidList.Create;
  AutoFree(@RegionList);

  for c1 := 0 to SampleMap.SampleRegions.Count-1 do
  begin
    if SampleMap.SampleRegions[c1].IsSelected
      then RegionList.Add(SampleMap.SampleRegions[c1].UniqueID);
  end;

  if Sender = LowNoteKnob  then TAdjustRegions.IncLowNote(Plugin, RegionList);
  if Sender = HighNoteKnob then TAdjustRegions.IncHighNote(Plugin, RegionList);

  if Sender = LowVelKnob   then TAdjustRegions.IncLowVel(Plugin, RegionList);
  if Sender = HighVelKnob  then TAdjustRegions.IncHighVel(Plugin, RegionList);

  if Sender = RootNoteKnob then TAdjustRegions.IncRootNote(Plugin, RegionList);


  UpdateSampleRegions;
  UpdateRegionInfoDisplay;


  //TODO: It would be handy to have a thread safe throttle.
  Throttle(ThrottleID, 25, procedure
  begin
    Plugin.Globals.MotherShip.MsgVclTS(TLucidMsgID.SampleRegionChanged, nil);
  end);
end;


procedure TSampleMapFrame.CloseSampleMapButtonClick(Sender: TObject);
begin
  Command.ToggleSampleMapVisibility(Plugin);
end;





end.
