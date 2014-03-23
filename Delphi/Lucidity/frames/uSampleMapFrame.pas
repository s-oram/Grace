unit uSampleMapFrame;

interface

uses
  VamLib.UniqueID,
  VamLib.ZeroObject, VamShortMessageOverlay,
  Menu.SampleMapContextMenu, eeGuiStandard,
  eePlugin, uLucidityKeyGroupInterface, uKeyStateTrackerOverlay,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  RedFoxWinControl, VamWinControl, VamPanel, RedFoxContainer, VamSampleMap,
  VamSamplerKeys, VamScrollBox, Vcl.Menus, RedFoxGraphicControl,
  VamGraphicControl, VamLabel, VamDiv, VamCompoundLabel, VamVisibleControl,
  VamNumericKnob,
  VamCompoundNumericKnob;

type
  TSampleMapFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    ScrollBox: TVamScrollBox;
    SampleMapKeys: TVamSamplerKeys;
    SampleMap: TVamSampleMap;
    RegionInfoBox: TVamDiv;
    SampleNameLabel: TVamLabel;
    InsidePanel: TVamPanel;
    UpperPanelArea: TVamPanel;
    RootNoteKnob: TVamCompoundNumericKnob;
    LowVelKnob: TVamCompoundNumericKnob;
    HighVelKnob: TVamCompoundNumericKnob;
    LowNoteKnob: TVamCompoundNumericKnob;
    HighNoteKnob: TVamCompoundNumericKnob;
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
    procedure SampleMapDblClick(Sender: TObject);
  private
    fPlugin: TeePlugin;
    function GetScrollPosX: single;
    function GetScrollPosY: single;
    procedure SetScollPosX(const Value: single);
    procedure SetScollPosY(const Value: single);
  private
    FMotherShip : IMothership;
    function GetMotherShipReference:IMotherShip;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    MessageOverlay         : TVamShortMessageOverlay;
    MessageOverlayAnimateID : TUniqueID;

    KeyStateTrackerOverlay : TKeyStateTrackerOverlay;
    SampleMapMenu : TSampleMapContextMenu;
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
  VamLib.Animation,
  VamLib.Threads,
  VamLib.Utils,
  SampleMapFrame.Extra,
  Lucidity.Types,
  eeVstXml,
  RedFoxColor, eePitch,
  VamKeyStateTracker, GuidEx,
  uConstants, Lucidity.SampleMap, uLucidityKeyGroup;

{$R *.dfm}

constructor TSampleMapFrame.Create(AOwner: TComponent);
begin
  inherited;
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
  if (assigned(FMotherShip))
    then FMotherShip.DeregisterZeroObject(self);

  KeyStateTrackerOverlay.Free;
  SampleMapMenu.Free;

  inherited;
end;

procedure TSampleMapFrame.InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
begin
  fPlugin := aPlugin;
  UpdateSampleRegions;

  SampleMapMenu.Initialize(aPlugin);

  UpperPanelArea.Height := 28;

  RegionInfoBox.Align := alClient;

  SampleNameLabel.Align := alClient;

  SampleMapRegionInfoChanged(self);

  SampleNameLabel.Font.Color := GetRedFoxColor(kColor_LcdDark5);

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

  UpperPanelArea.Color := kColor_LcdDark1;

end;

function TSampleMapFrame.GetMotherShipReference: IMotherShip;
begin
  result := FMotherShip;
end;

function TSampleMapFrame.GetScrollPosX: single;
begin
  result := ScrollBox.ScrollXPos;
end;

function TSampleMapFrame.GetScrollPosY: single;
begin
  result := ScrollBox.ScrollYPos;
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
begin
  if not assigned(Plugin) then exit;

  KG := Plugin.FocusedKeyGroup;

  if not assigned(KG) then exit;

  SampleMap.BeginUpdate;

  try
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

      NameA := MapRegion.GetKeyGroup.GetName;
      NameB := KG.GetName;

      if (NameA = NameB)
        then DisplayRegion.IsInOtherKeyGroup := false
        else DisplayRegion.IsInOtherKeyGroup := true;

      if (NameA = NameB)
        then DisplayRegion.IsVisible := true
        else DisplayRegion.IsVisible := false;
    end;


  finally
    SampleMap.EndUpdate;
    SampleMap.Invalidate;
  end;

end;

procedure TSampleMapFrame.SampleMapSelectRegion(const Sender: TObject; aRegion: TVamSampleRegion);
begin
  Plugin.SampleMap.SelectRegion(aRegion.UniqueID);

  UpdateRegionInfoDisplay;
  UpdateSampleRegions;

  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleFocusChanged);
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

  UpdateRegionInfoDisplay;
  UpdateSampleRegions;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleFocusChanged);
end;



procedure TSampleMapFrame.SampleMapDeselectOtherRegions(const Sender: TObject; aRegion: TVamSampleRegion);
var
  SG : IKeyGroup;
begin
  if not assigned(Plugin) then exit;

  SG := Plugin.FocusedKeyGroup;
  Plugin.SampleMap.DeselectOtherRegions(aRegion.UniqueID);

  UpdateRegionInfoDisplay;
  UpdateSampleRegions;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleFocusChanged);
end;

procedure TSampleMapFrame.SampleMapDblClick(Sender: TObject);
begin
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_HideSampleMapEdit);
end;

procedure TSampleMapFrame.SampleMapDeselectAllRegions(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  Plugin.SampleMap.DeselectAllRegions;

  UpdateRegionInfoDisplay;
  UpdateSampleRegions;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleFocusChanged);
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
    then Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_ShowReplaceRegionMessage)
    else Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_HideReplaceRegionMessage);

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

  UpdateRegionInfoDisplay;
  UpdateSampleRegions;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleRegionChanged);
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

procedure TSampleMapFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
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


end;

procedure TSampleMapFrame.SampleMapRegionInfoChanged(Sender: TObject);
begin
  UpdateRegionInfoDisplay;
end;

procedure TSampleMapFrame.SampleMapMouseOverRegionChanged(const Sender: TObject; aRegion: TVamSampleRegion);
begin
  if not assigned(Plugin) then exit;

  if assigned(aRegion)
    then Plugin.GuiState.MouseOverRegionID := aRegion.UniqueID
    else Plugin.GuiState.MouseOverRegionID := TGuidEx.EmptyGuid;

  UpdateRegionInfoDisplay;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.MouseOverSampleRegionChanged);
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
    RegionInfoBox.Visible := true;

    Info := MouseOverRegionInfo;

    if SelectedCount <= 1
      then SampleNameText := ExtractFilename(Info.FileName)
      else SampleNameText := ExtractFilename(Info.FileName) + '  (' + IntToStr(SelectedCount) + ' regions selected)';

    UpdateRegionInfoControls(SampleNameText, Info.LowKey, Info.HighKey, Info.LowVelocity, Info.HighVelocity, Info.RootNote);
  end;


  if (DragSelectedCount = -1) and (SelectedCount = 0) and (MouseOverRegionInfo.IsValid = false) then
  begin
    RegionInfoBox.Visible := false;
    UpdateRegionInfoControls('', '', '', '', '', '');
  end;

  if (DragSelectedCount = -1) and (SelectedCount = 1) and (MouseOverRegionInfo.IsValid = false) then
  begin
    Info := SampleMap.GetDisplayInfo;

    if Info.IsValid then
    begin
      RegionInfoBox.Visible := true;

      SampleNameText := ExtractFilename(Info.FileName);
      UpdateRegionInfoControls(SampleNameText, Info.LowKey, Info.HighKey, Info.LowVelocity, Info.HighVelocity, Info.RootNote);
    end;
  end;

  if (DragSelectedCount = -1) and (SelectedCount > 1) and (MouseOverRegionInfo.IsValid = false) then
  begin
    RegionInfoBox.Visible := true;

    SampleNameText := '(' + IntToStr(SelectedCount) + ' regions selected)';
    UpdateRegionInfoControls(SampleNameText, '-', '-', '-', '-', '-');
  end;

  if (DragSelectedCount > 0) and (MouseOverRegionInfo.IsValid = false) then
  begin
    RegionInfoBox.Visible := true;

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

  finally
    SetLength(NewRegions, 0);
  end;
end;

procedure TSampleMapFrame.SampleMapOleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data: IVamDragData);
begin
  Plugin.ClearSelected;
end;

procedure TSampleMapFrame.SampleMapReplaceRegion(Sender: TObject; const NewRegion, OldRegion: TVamSampleRegion; const Data: IVamDragData);
type
  TDropType = (dtFiles, dtVstXml);
var
  RegionCreateInfo : TRegionCreateInfo;
  DropType : TDropType;
  XmlRegionCount : integer;
  XmlRegionInfo : TVstXmlRegion;
  NewSR : IRegion;
begin
  if not assigned(Plugin) then exit;

  Plugin.StopPreview;

  //====== Load the new regions ==============================================
  XmlRegionCount := GetVstXmlRegionCount(Data.GetText);

  if XmlRegionCount > 0
    then DropType := TDropType.dtVstXml
    else DropType := TDropType.dtFiles;

  RegionCreateInfo.KeyGroup      := Plugin.FocusedKeyGroup;
  RegionCreateInfo.LowNote       := NewRegion.LowKey;
  RegionCreateInfo.HighNote      := NewRegion.HighKey;
  RegionCreateInfo.LowVelocity   := NewRegion.LowVelocity;
  RegionCreateInfo.HighVelocity  := NewRegion.HighVelocity;
  RegionCreateInfo.RootNote      := NewRegion.RootNote;

  case DropType of
    dtFiles:
    begin
      RegionCreateInfo.AudioFileName := Data.GetFiles[0];
    end;

    dtVstXml:
    begin
      XmlRegionInfo := GetVstXmlRegionInfo(0, Data.GetText);
      RegionCreateInfo.AudioFileName := XmlRegionInfo.FileName;
    end
  else
    raise Exception.Create('Drop type not handled.');
  end;

  NewSR := Plugin.NewRegion(RegionCreateInfo);

  if (assigned(NewSR)) and (NewSR.GetProperties^.IsSampleError = false) then
  begin
    Plugin.ClearSelected;
    Plugin.FocusRegion(NewSR.GetProperties^.UniqueID);
    Plugin.SampleMap.DeleteRegion(OldRegion.UniqueID);
  end;

  UpdateRegionInfoDisplay;
  UpdateSampleRegions;
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
      RegionCreateInfo.LowNote       := aRegions[c1].LowKey;
      RegionCreateInfo.HighNote      := aRegions[c1].HighKey;
      RegionCreateInfo.LowVelocity   := aRegions[c1].LowVelocity;
      RegionCreateInfo.HighVelocity  := aRegions[c1].HighVelocity;
      RegionCreateInfo.RootNote      := aRegions[c1].RootNote;
      RegionCreateInfo.AudioFileName := aRegions[c1].FileName;

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
  SampleNameLabel.Text        := SampleName;
  LowNoteKnob.KnobCustomText  := LowNote;
  HighNoteKnob.KnobCustomText := HighNote;
  LowVelKnob.KnobCustomText   := LowVel;
  HighVelKnob.KnobCustomText  := HighVel;
  RootNoteKnob.KnobCustomText := RootNote;
end;

procedure TSampleMapFrame.UpdateRegionInfoControls(const SampleName: string; const LowNote, HighNote, LowVel, HighVel, RootNote: integer);
begin
  SampleNameLabel.Text        := SampleName;
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
      then RegionList.Append(SampleMap.SampleRegions[c1].UniqueID);
  end;

  if Sender = LowNoteKnob  then TAdjustRegions.DecLowNote(Plugin, RegionList);
  if Sender = HighNoteKnob then TAdjustRegions.DecHighNote(Plugin, RegionList);

  if Sender = LowVelKnob   then TAdjustRegions.DecLowVel(Plugin, RegionList);
  if Sender = HighVelKnob  then TAdjustRegions.DecHighVel(Plugin, RegionList);

  if Sender = RootNoteKnob then TAdjustRegions.DecRootNote(Plugin, RegionList);

  //TODO: throttle this call..
  UpdateRegionInfoDisplay;
  UpdateSampleRegions;

  //TODO: throttle this call..
  //Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleRegionChanged);

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
      then RegionList.Append(SampleMap.SampleRegions[c1].UniqueID);
  end;

  if Sender = LowNoteKnob  then TAdjustRegions.IncLowNote(Plugin, RegionList);
  if Sender = HighNoteKnob then TAdjustRegions.IncHighNote(Plugin, RegionList);

  if Sender = LowVelKnob   then TAdjustRegions.IncLowVel(Plugin, RegionList);
  if Sender = HighVelKnob  then TAdjustRegions.IncHighVel(Plugin, RegionList);

  if Sender = RootNoteKnob then TAdjustRegions.IncRootNote(Plugin, RegionList);

  //TODO: throttle this call..
  UpdateRegionInfoDisplay;
  UpdateSampleRegions;

  //TODO: throttle this call..
  //Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleRegionChanged);
end;






end.
