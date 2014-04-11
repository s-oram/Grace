unit uSampleDisplayFrame;

interface

uses
  Lucidity.Types,
  eeGuiStandard, VamVisibleControl,
  eePlugin, Lucidity.SampleMap, uGuiFeedbackData, uConstants, Lucidity.Interfaces,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamScrollBox, VamSampleDisplay,
  VamDiv, LuciditySampleOverlay, VamSampleZoomControl, Vcl.Menus,
  RedFoxGraphicControl, VamGraphicControl, VamLabel, VamButton;

type
  TSampleDisplayFrame = class(TFrame)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    SampleDisplay: TVamSampleDisplay;
    HeadControlsContainer: TVamDiv;
    ZoomSampleDisplay: TVamSampleDisplay;
    SampleZoomControl: TVamSampleZoomControl;
    SampleOverlayContextMenu: TPopupMenu;
    miSetSampleStart: TMenuItem;
    miSetSampleEnd: TMenuItem;
    miSetLoopStart: TMenuItem;
    miSetLoopEnd: TMenuItem;
    SampleControlsPanel: TVamPanel;
    VamPanel1: TVamPanel;
    SampleStartTextBox: TVamLabel;
    VamLabel2: TVamLabel;
    VamPanel2: TVamPanel;
    SampleEndTextBox: TVamLabel;
    VamLabel4: TVamLabel;
    VamPanel3: TVamPanel;
    SampleNameTextBox: TVamLabel;
    VamLabel6: TVamLabel;
    VamPanel4: TVamPanel;
    SampleStartModTextBox: TVamLabel;
    VamLabel8: TVamLabel;
    VamPanel5: TVamPanel;
    SampleEndModTextBox: TVamLabel;
    VamLabel10: TVamLabel;
    VamPanel6: TVamPanel;
    LoopModeTextBox: TVamLabel;
    VamLabel12: TVamLabel;
    VamPanel7: TVamPanel;
    LoopStartTextBox: TVamLabel;
    VamLabel14: TVamLabel;
    VamPanel8: TVamPanel;
    LoopEndTextBox: TVamLabel;
    VamLabel16: TVamLabel;
    VamPanel9: TVamPanel;
    LoopStartModTextBox: TVamLabel;
    VamLabel18: TVamLabel;
    VamPanel10: TVamPanel;
    LoopEndModTextBox: TVamLabel;
    VamLabel20: TVamLabel;
    VamButton1: TVamButton;
    VamButton2: TVamButton;
    LoopModeMenu: TPopupMenu;
    miLoopOff: TMenuItem;
    miLoopForward: TMenuItem;
    miLoopBackward: TMenuItem;
    miLoopPingPong: TMenuItem;
    procedure SampleZoomControlChanged(Sender: TObject);
    procedure ZoomSampleDisplayResize(Sender: TObject);
    procedure SampleOverlayContextMenuItemClick(Sender: TObject);
    procedure SampleOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SampleDisplayOleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
    procedure SampleDisplayOleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
  private
    fPlugin: TeePlugin;
    fSampleOverlay : TLuciditySampleOverlay;
    SampleInfo : TSampleDisplayInfo;
    SampleOverlayClickPos : TPoint;

    procedure SampleMarkerChanged(Sender:TObject; Marker:TSampleMarker; NewPosition : integer);
    procedure UpdateSampleDisplayInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);

    procedure UpdateSampleDisplay;

    procedure UpdateGui(Sender:TObject; FeedBack:PGuiFeedbackData);

    procedure UseCompactLayout;
    procedure UseFullLayout;

    property SampleOverlay : TLuciditySampleOverlay read fSampleOverlay;
    property Plugin:TeePlugin read fPlugin;
  end;

implementation

{$R *.dfm}

uses
  {$IFDEF Logging}SmartInspectLogging,{$ENDIF}
  uGuiUtils,
  Lucidity.KeyGroup;




{ TSampleDisplayFrame }

constructor TSampleDisplayFrame.Create(AOwner: TComponent);
begin
  inherited;
  fSampleOverlay := TLuciditySampleOverlay.Create(AOwner);
  fSampleOverlay.Parent  := SampleDisplay;
  fSampleOverlay.Align := alClient;
  fSampleOverlay.Visible := true;

  fSampleOverlay.OnMouseDown := SampleOverlayMouseDown;
  fSampleOverlay.OnSampleMarkerChanged := SampleMarkerChanged;

  fSampleOverlay.OnOleDragDrop := SampleDisplayOleDragDrop;
  fSampleOverlay.OnOleDragOver := SampleDisplayOleDragOver;

  SampleInfo.IsValid := false;
end;

destructor TSampleDisplayFrame.Destroy;
begin
  inherited;
end;

procedure TSampleDisplayFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  fPlugin := aPlugin;
  UpdateSampleDisplay;
end;

procedure TSampleDisplayFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin
  fSampleOverlay.LinkToGuiFeedbackData(Feedback);
  fSampleOverlay.Invalidate;
end;

procedure TSampleDisplayFrame.UpdateSampleDisplay;
var
  CurRegion : IRegion;
  //s : string;
begin
  if not assigned(Plugin) then exit;

  // NOTE: TODO:
  // The sample display maintains a reference to the current region so it won't
  // be deleted mid-drawing operation. However it might be possible for the
  // sample could be changed mid-drawing operating.
  // I think the sample display system could be improved still.
  // - send a interface reference to a thread.
  // - Sample display draws the sample in a background thread.
  // - synchronises with the GUI thread to blit the drawn sample to the screen
  // - returns to the method at the beginning to null the interface reference.
  //
  // The system would need to be stress tested in a testing app to ensure it doesn't blow up.

  CurRegion := Plugin.FocusedRegion;

  if (assigned(CurRegion)) and (CurRegion.GetSample^.Properties.IsValid) then
  begin
    SampleInfo.IsValid := true;
    SampleInfo.ChannelCount := CurRegion.GetSample^.Properties.ChannelCount;
    SampleInfo.SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;

    if CurRegion.GetSample^.Properties.ChannelCount = 1 then
    begin
      SampleInfo.Ch1 := CurRegion.GetSample^.Properties.Ch1;
    end;

    if CurRegion.GetSample^.Properties.ChannelCount = 2 then
    begin
      SampleInfo.Ch1 := CurRegion.GetSample^.Properties.Ch1;
      SampleInfo.Ch2 := CurRegion.GetSample^.Properties.Ch2;
    end;

    SampleDisplay.DrawSample(SampleInfo);
    SampleDisplay.Invalidate;

    //== Zoom Sample Display ==
    ZoomSampleDisplay.Zoom := 0;
    ZoomSampleDisplay.Offset := 0;
    ZoomSampleDisplay.DrawSample(SampleInfo);
    ZoomSampleDisplay.Invalidate;

    //Update sample info...
    UpdateSampleDisplayInfo;


  end else
  begin
    SampleInfo.IsValid := false;
    SampleDisplay.ClearSample(true);

    //== Zoom Sample Display ==
    ZoomSampleDisplay.ClearSample(true);

    //Update sample info...
    UpdateSampleDisplayInfo;
  end;
end;

procedure TSampleDisplayFrame.UpdateSampleDisplayInfo;
var
  CurRegion : IRegion;
  s : string;
begin
  if not assigned(Plugin) then exit;

  // NOTE: TODO:
  // The sample display maintains a reference to the current region so it won't
  // be deleted mid-drawing operation. However it might be possible for the
  // sample could be changed mid-drawing operating.
  // I think the sample display system could be improved still.
  // - send a interface reference to a thread.
  // - Sample display draws the sample in a background thread.
  // - synchronises with the GUI thread to blit the drawn sample to the screen
  // - returns to the method at the beginning to null the interface reference.
  //
  // The system would need to be stress tested in a testing app to ensure it doesn't blow up.

  CurRegion := Plugin.FocusedRegion;

  if (assigned(CurRegion)) and (CurRegion.GetSample^.Properties.IsValid) then
  begin
    SampleInfo.IsValid := true;
    SampleInfo.ChannelCount := CurRegion.GetSample^.Properties.ChannelCount;
    SampleInfo.SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;

    //== Sample Overlay ==
    fSampleOverlay.SetSampleInfo(true, SampleInfo.SampleFrames);
    fSampleOverlay.SampleStart := CurRegion.GetProperties^.SampleStart;
    fSampleOverlay.SampleEnd   := CurRegion.GetProperties^.SampleEnd;
    fSampleOverlay.LoopStart   := CurRegion.GetProperties^.LoopStart;
    fSampleOverlay.LoopEnd     := CurRegion.GetProperties^.LoopEnd;

    fSampleOverlay.ShowLoopPoints := true;

    //== Sample / Region Properties =============================
    s := ExtractFilename(CurRegion.GetProperties^.SampleFileName);
    if SampleNameTextBox.Text <> s then SampleNameTextBox.Text := s;

    s := IntToStr(CurRegion.GetProperties^.SampleStart);
    if SampleStartTextBox.Text <> s then SampleStartTextBox.Text := s;

    s := IntToStr(CurRegion.GetProperties^.SampleEnd);
    if SampleEndTextBox.Text <> s then SampleEndTextBox.Text := s;

    {
    case CurRegion.GetProperties^.LoopMode of
      LoopOff      : s := 'OFF';
      LoopForward  : s := 'FORWARD';
      LoopBackward : s := 'BACKWARD';
      LoopPingPong : s := 'PING-PONG';
    else
      s := 'ERROR';
    end;
    if LoopModeTextBox.Text <> s then LoopModeTextBox.Text := s;

    if CurRegion.GetProperties^.LoopMode <> LoopOff then
    begin
      s := IntToStr(CurRegion.GetProperties^.LoopStart);
      if LoopStartTextBox.Text <> s then LoopStartTextBox.Text := s;

      s := IntToStr(CurRegion.GetProperties^.LoopEnd);
      if LoopEndTextBox.Text <> s then LoopEndTextBox.Text := s;
    end else
    begin
      s := '--';
      if LoopStartTextBox.Text <> s then LoopStartTextBox.Text := s;
      if LoopEndTextBox.Text <> s then LoopEndTextBox.Text := s;
    end;
    }

    s := IntToStr(CurRegion.GetProperties^.LoopStart);
    if LoopStartTextBox.Text <> s then LoopStartTextBox.Text := s;

    s := IntToStr(CurRegion.GetProperties^.LoopEnd);
    if LoopEndTextBox.Text <> s then LoopEndTextBox.Text := s;





  end else
  begin
    //== Sample Display Overlay ==
    fSampleOverlay.SetSampleInfo(false, 0);
    fSampleOverlay.ShowLoopPoints := false;


    //== Sample / Region Properties =============================
    s := '';
    if SampleNameTextBox.Text <> s then SampleNameTextBox.Text := s;
    if SampleStartTextBox.Text <> s then SampleStartTextBox.Text := s;
    if SampleEndTextBox.Text <> s then SampleEndTextBox.Text := s;
    if LoopStartTextBox.Text <> s then LoopStartTextBox.Text := s;
    if LoopEndTextBox.Text <> s then LoopEndTextBox.Text := s;
    if LoopModeTextBox.Text <> s then LoopModeTextBox.Text := s;

  end;
end;


procedure TSampleDisplayFrame.ZoomSampleDisplayResize(Sender: TObject);
begin
  SampleZoomControl.Left   := 0;
  SampleZoomControl.Top    := 0;
  SampleZoomControl.Width  := SampleZoomControl.Parent.Width;
  SampleZoomControl.Height := SampleZoomControl.Parent.Height;
  SampleZoomControl.BringToFront;
end;

procedure TSampleDisplayFrame.SampleZoomControlChanged(Sender: TObject);
var
  tx : single;
  IndexA, IndexB : single;
  SampleFrames, DisplayPixelWidth : integer;
  Zoom, Offset : single;
begin
  if SampleInfo.IsValid then
  begin
    IndexA := (Sender as TVamSampleZoomControl).IndexA;
    IndexB := (Sender as TVamSampleZoomControl).IndexB;

    assert(IndexA >= 0);
    assert(IndexA <= 1);
    assert(IndexB >= 0);
    assert(IndexB <= 1);

    if IndexA > IndexB then
    begin
      tx := IndexA;
      IndexA := IndexB;
      IndexB := tx;
    end;

    SampleFrames := SampleInfo.SampleFrames;
    DisplayPixelWidth := SampleDisplay.Width;

    CalcZoomOffset(IndexA, IndexB, SampleFrames, DisplayPixelWidth, Zoom, Offset);

    SampleDisplay.Zoom := Zoom;
    SampleDisplay.Offset := Offset;


    fSampleOverlay.SetZoomOffset(Zoom, Offset);
  end;
end;

procedure TSampleDisplayFrame.SampleMarkerChanged(Sender: TObject; Marker: TSampleMarker; NewPosition: integer);
var
  CurRegion : IRegion;
begin
  if not assigned(Plugin) then exit;

  CurRegion := Plugin.FocusedRegion;

  case Marker of
    smSampleStartMarker: CurRegion.GetProperties^.SampleStart := NewPosition;
    smSampleEndMarker:   CurRegion.GetProperties^.SampleEnd   := NewPosition;
    smLoopStartMarker:   CurRegion.GetProperties^.LoopStart   := NewPosition;
    smLoopEndMarker:     CurRegion.GetProperties^.LoopEnd     := NewPosition;
  end;

  UpdateSampleDisplayInfo;
end;

procedure TSampleDisplayFrame.SampleOverlayMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  CurRegion : IRegion;
begin
  CurRegion := Plugin.FocusedRegion;
  if CurRegion = nil then exit;

  if (Button = mbRight) then
  begin
    SampleOverlayClickPos := Point(X, Y);
    SampleOverlayContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TSampleDisplayFrame.SampleOverlayContextMenuItemClick(Sender: TObject);
var
  Tag : integer;
  SamplePos : integer;
  SampleFrames : integer;
  CurRegion : IRegion;
begin
  if not assigned(Plugin) then exit;

  CurRegion := Plugin.FocusedRegion;
  if CurRegion = nil then exit;

  Tag := (Sender as TMenuItem).Tag;

  SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;

  SamplePos := round(fSampleOverlay.PixelPosToSamplePos(SampleOverlayClickPos.X, SampleFrames));

  if SamplePos < 0 then SamplePos := 0;
  if SamplePos >= SampleFrames-1 then SamplePos := SampleFrames-1;

  case Tag of
    1: CurRegion.GetProperties^.SampleStart := SamplePos;
    2: CurRegion.GetProperties^.SampleEnd   := SamplePos;
    3: CurRegion.GetProperties^.LoopStart   := SamplePos;
    4: CurRegion.GetProperties^.LoopEnd     := SamplePos;
  end;

  UpdateSampleDisplay;
end;

procedure TSampleDisplayFrame.UseCompactLayout;
begin
  HeadControlsContainer.Visible := false;
  ZoomSampleDisplay.Visible := false;
  Panel.Height := 200;
end;

procedure TSampleDisplayFrame.UseFullLayout;
begin
  HeadControlsContainer.Visible := true;
  ZoomSampleDisplay.Visible := true;
  Panel.Height := 367;
end;

procedure TSampleDisplayFrame.SampleDisplayOleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
var
  RegionCreateInfo : TRegionCreateInfo;
  aRegion : IRegion;
  CurRegion : IRegion;
  SG : IKeyGroup;
  OwningSampleGroup : IKeyGroup;
begin
  if not assigned(Plugin) then exit;

  SG := Plugin.FocusedKeyGroup;

  if Data.GetFiles.Count > 0 then
  begin
    CurRegion := Plugin.FocusedRegion;
    if assigned(CurRegion) then
    begin
      Plugin.SampleMap.DeleteRegion(CurRegion);
    end;

    if assigned(CurRegion)
      then OwningSampleGroup := CurRegion.GetKeyGroup
      else OwningSampleGroup := Plugin.FocusedKeyGroup;

    RegionCreateInfo.KeyGroup   := OwningSampleGroup;
    RegionCreateInfo.AudioFileName := Data.GetFiles[0];
    RegionCreateInfo.LowNote       := 0;
    RegionCreateInfo.HighNote      := 127;
    RegionCreateInfo.LowVelocity   := 0;
    RegionCreateInfo.HighVelocity  := 127;
    RegionCreateInfo.RootNote      := 60; //MIDI c4.

    aRegion := Plugin.NewRegion(RegionCreateInfo);

    if assigned(aRegion) then
    begin
      Plugin.FocusRegion(aRegion.GetProperties^.UniqueID);
    end;

    Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.SampleFocusChanged);
  end;
end;

procedure TSampleDisplayFrame.SampleDisplayOleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
begin
  //TODO: Nothing here yet.
end;

end.
