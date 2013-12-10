unit uLoopEditDialog;

interface

uses
  uConstants,
  uLucidityPopUpMenu,
  Lucidity.SampleImageRenderer,
  LuciditySampleOverlay,
  eePlugin, eeGuiStandard, uGuiFeedbackData,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Menus, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamButton, VamSampleDisplay, VamDiv,
  VamSampleZoomControl, VamLabel, RedFoxGraphicControl, VamGraphicControl,
  VamTextBox;

type
  {$SCOPEDENUMS ON}

  // TDialogSampleMarker is somewhat awkwardly named to avoid conflicts with LuciditySampleOverlay.TSampleMarker.
  TDialogSampleMarker = (SampleStart, SampleEnd, LoopStart, LoopEnd);

  TSampleMarkerEvent = procedure(Sender : TObject; Marker:TDialogSampleMarker) of object;

  TZoomMarkerMenu = class(TLucidityPopupMenu)
  private
    fOnZoomToSampleMarker: TSampleMarkerEvent;
  protected
    procedure ZoomToClick(Sender : TObject);
  public
    procedure Popup(const x, y : integer); override;
    property OnZoomToSampleMarker : TSampleMarkerEvent read fOnZoomToSampleMarker write fOnZoomToSampleMarker;
  end;

  TLoopEditForm = class(TForm)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    VamLabel1: TVamLabel;
    HeadControlsContainer: TVamDiv;
    ZoomOutButton: TVamTextBox;
    ZoomInButton: TVamTextBox;
    ZoomLoopEndButton: TVamTextBox;
    ZoomLoopStartButton: TVamTextBox;
    ZoomSampleEndButton: TVamTextBox;
    ZoomSampleStartButton: TVamTextBox;
    ZoomMarkerButton: TVamTextBox;
    SampleDisplayContainer: TVamPanel;
    SampleDisplay: TVamSampleDisplay;
    SampleZoomContainer: TVamPanel;
    ZoomSampleDisplay: TVamSampleDisplay;
    SampleZoomControl: TVamSampleZoomControl;
    VamLabel2: TVamLabel;
    Zoom100Button: TVamTextBox;
  private
    fGuiStandard: TGuiStandard;
    fPlugin: TeePlugin;
  protected
    SampleRenderer : TSampleImageRenderer;

    Zoom, Offset : single;

  public
    SampleInfo    : TSampleDisplayInfo;

    //The SampleOverlay GUI components show the sample start/end markers and other
    // information that is drawn over the sample display.
    SampleOverlay     : TLuciditySampleOverlay;
    ZoomSampleOverlay : TLuciditySampleOverlay;

    ZoomMarkerMenu : TZoomMarkerMenu;

    property Plugin : TeePlugin read fPlugin;
    property GuiStandard : TGuiStandard read fGuiStandard;

    procedure SampleOverlay_MarkerChanged(Sender:TObject; Marker:TSampleMarker; NewPosition : integer);

    procedure ZoomToSampleMarker(Sender : TObject; Marker:TDialogSampleMarker);

    procedure PanelResize(Sender: TObject);
    procedure SampleZoomControlChanged(Sender: TObject);
    procedure ZoomButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure SampleFocusChanged(var Msg: TMessage); //message UM_SAMPLE_FOCUS_CHANGED;
    procedure SampleRegionChanged(var Msg: TMessage); //message UM_SAMPLE_REGION_CHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GuiEvent_SampleMakersChanged;

    procedure UpdateSampleDisplay;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);
  end;


  ILoopEditDialog = interface
    ['{3B1C6F01-C788-4394-9AE3-C8FEBFA12711}']
    procedure Setup(ParentContainer : TWinControl; aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
  end;

  TLoopEditDialog = class(TInterfacedObject, ILoopEditDialog)
  private
    Form : TLoopEditForm;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Setup(ParentContainer : TWinControl; aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
  end;






implementation

uses
  VamLib.Graphics, eeDsp,
  GuidEx, uGuiUtils, RedFoxColor,
  Lucidity.SampleMap;

{$R *.dfm}

{ TZoomMarkerMenu }

procedure TZoomMarkerMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
begin
  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom To Sample Start';
  mi.OnClick := self.ZoomToClick;
  mi.Tag := 1;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom To Sample End';
  mi.OnClick := self.ZoomToClick;
  mi.Tag := 2;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom To Loop Start';
  mi.OnClick := self.ZoomToClick;
  mi.Tag := 3;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Zoom To Loop End';
  mi.OnClick := self.ZoomToClick;
  mi.Tag := 4;
  Menu.Items.Add(mi);


  Menu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TZoomMarkerMenu.ZoomToClick(Sender: TObject);
var
  Tag : integer;
begin
  Tag := (Sender as TMenuItem).Tag;

  case Tag of
    1: OnZoomToSampleMarker(self, TDialogSampleMarker.SampleStart);
    2: OnZoomToSampleMarker(self, TDialogSampleMarker.SampleEnd);
    3: OnZoomToSampleMarker(self, TDialogSampleMarker.LoopStart);
    4: OnZoomToSampleMarker(self, TDialogSampleMarker.LoopEnd);
  else
    raise Exception.Create('Tag not handled.');
  end;
end;

{ TLoopEditDialog }

constructor TLoopEditForm.Create(AOwner: TComponent);
begin
  inherited;

  Zoom   := 0;
  Offset := 0;

  SampleRenderer := TSampleImageRenderer.Create;

  Panel.OnResize                    := PanelResize;
  BackgroundPanel.OnResize          := PanelResize;

  Zoom100Button.OnMouseDown         := ZoomButtonMouseDown;
  ZoomSampleStartButton.OnMouseDown := ZoomButtonMouseDown;
  ZoomSampleEndButton.OnMouseDown   := ZoomButtonMouseDown;
  ZoomLoopStartButton.OnMouseDown   := ZoomButtonMouseDown;
  ZoomLoopEndButton.OnMouseDown     := ZoomButtonMouseDown;
  ZoomOutButton.OnMouseDown         := ZoomButtonMouseDown;
  ZoomInButton.OnMouseDown          := ZoomButtonMouseDown;

  SampleZoomControl.OnChanged := SampleZoomControlChanged;


  SampleInfo.IsValid := false;

  SampleOverlay := TLuciditySampleOverlay.Create(self.Owner);
  SampleOverlay.Parent := SampleDisplayContainer;
  SampleOverlay.Visible := true;
  SampleOverlay.Margins.Left   := 0;
  SampleOverlay.Margins.Right  := 0;
  SampleOverlay.Margins.Top    := 0;
  SampleOverlay.Margins.Bottom := 0;
  SampleOverlay.SetZoomOffset(0,0);
  SampleOverlay.OnSampleMarkerChanged := SampleOverlay_MarkerChanged;
  SampleOverlay.ShowMarkerTags := true;


  ZoomSampleOverlay := TLuciditySampleOverlay.Create(self.Owner);
  ZoomSampleOverlay.Parent := SampleZoomContainer;
  ZoomSampleOverlay.Visible := true;
  ZoomSampleOverlay.Margins.Left   := 0;
  ZoomSampleOverlay.Margins.Right  := 0;
  ZoomSampleOverlay.Margins.Top    := 0;
  ZoomSampleOverlay.Margins.Bottom := 0;
  ZoomSampleOverlay.SetZoomOffset(0,0);
  ZoomSampleOverlay.OnSampleMarkerChanged := SampleOverlay_MarkerChanged;
  ZoomSampleOverlay.ShowMarkerTags := true;
  ZoomSampleOverlay.HitTest := false;
  ZoomSampleOverlay.ShowMarkerTags := true;





  ZoomMarkerMenu := TZoomMarkerMenu.Create;
  ZoomMarkerMenu.OnZoomToSampleMarker := self.ZoomToSampleMarker;
end;

destructor TLoopEditForm.Destroy;
begin
  FreeAndNil(ZoomMarkerMenu);
  SampleRenderer.Free;
  inherited;
end;

procedure TLoopEditForm.SampleFocusChanged(var Msg: TMessage);
begin
  UpdateSampleDisplay;
end;

procedure TLoopEditForm.SampleRegionChanged(var Msg: TMessage);
begin
  UpdateSampleDisplay;
end;





procedure TLoopEditForm.PanelResize(Sender: TObject);
begin
  //=== Main Sample Display Setup ===

  SampleDisplay.AlignToParent(true);
  SampleOverlay.AlignToParent(true);
  //ensure the correct Z-index order.
  SampleDisplay.BringToFront;
  SampleOverlay.BringToFront;



  //=== Zoom Sample Display Setup ===

  ZoomSampleDisplay.AlignToParent(true);
  SampleZoomControl.AlignToParent(true);
  ZoomSampleOverlay.AlignToParent(true);

  //ensure the correct Z-index order.
  ZoomSampleDisplay.BringToFront;
  ZoomSampleOverlay.BringToFront;
  SampleZoomControl.BringToFront;

end;



procedure TLoopEditForm.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  fPlugin := aPlugin;
  fGuiStandard := aGuiStandard;

  SampleInfo.IsValid := false;

  ZoomMarkerMenu.Initialize(self.Plugin);

  ZoomMarkerButton.Visible := false;

  //==== Sample Display Container ====
  SampleDisplayContainer.Color := kColor_LcdDark1;
  SampleDisplayContainer.CornerRadius1 := 3;
  SampleDisplayContainer.CornerRadius2 := 3;
  SampleDisplayContainer.CornerRadius3 := 3;
  SampleDisplayContainer.CornerRadius4 := 3;

  SampleDisplayContainer.Padding.SetBounds(4,4,4,4);
  SampleDisplayContainer.AlignWithMargins := true;
  SampleDisplayContainer.Margins.SetBounds(0,0,0,4);

  //==== Sample Zoom Container ====
  SampleZoomContainer.Color := kColor_LcdDark1;
  SampleZoomContainer.CornerRadius1 := 3;
  SampleZoomContainer.CornerRadius2 := 3;
  SampleZoomContainer.CornerRadius3 := 3;
  SampleZoomContainer.CornerRadius4 := 3;
  SampleZoomContainer.Padding.SetBounds(4,4,4,4);
  SampleZoomContainer.AlignWithMargins := true;
  SampleZoomContainer.Margins.SetBounds(0,0,0,4);








  Zoom100Button.Color                  := kColor_LcdDark1;
  ZoomSampleStartButton.Color          := kColor_LcdDark1;
  ZoomSampleEndButton.Color            := kColor_LcdDark1;
  ZoomLoopStartButton.Color            := kColor_LcdDark1;
  ZoomLoopEndButton.Color              := kColor_LcdDark1;
  ZoomOutButton.Color                  := kColor_LcdDark1;
  ZoomInButton.Color                   := kColor_LcdDark1;

  Zoom100Button.ColorMouseOver         := kColor_ButtonMouseOver;
  ZoomSampleStartButton.ColorMouseOver := kColor_ButtonMouseOver;
  ZoomSampleEndButton.ColorMouseOver   := kColor_ButtonMouseOver;
  ZoomLoopStartButton.ColorMouseOver   := kColor_ButtonMouseOver;
  ZoomLoopEndButton.ColorMouseOver     := kColor_ButtonMouseOver;
  ZoomOutButton.ColorMouseOver         := kColor_ButtonMouseOver;
  ZoomInButton.ColorMouseOver          := kColor_ButtonMouseOver;

  Zoom100Button.Font.Color             := GetRedFoxColor(kColor_LcdDark5);
  ZoomSampleStartButton.Font.Color     := GetRedFoxColor(kColor_LcdDark5);
  ZoomSampleEndButton.Font.Color       := GetRedFoxColor(kColor_LcdDark5);
  ZoomLoopStartButton.Font.Color       := GetRedFoxColor(kColor_LcdDark5);
  ZoomLoopEndButton.Font.Color         := GetRedFoxColor(kColor_LcdDark5);
  ZoomOutButton.Font.Color             := GetRedFoxColor(kColor_LcdDark5);
  ZoomInButton.Font.Color              := GetRedFoxColor(kColor_LcdDark5);

  SampleDisplay.LineColor     := kColor_LcdDark4;
  ZoomSampleDisplay.LineColor := kColor_LcdDark4;


  //==========================
  Zoom100Button.Width := 40;



end;

procedure TLoopEditForm.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;

procedure TLoopEditForm.UpdateSampleDisplay;
var
  CurRegion : IRegion;
  xSampleImage : IInterfacedBitmap;
  Par:TSampleRenderParameters;
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

  if Plugin.GuiState.MouseOverRegionID <> TGuidEx.EmptyGuid then
  begin
    CurRegion := Plugin.SampleMap.FindRegionByUniqueID(Plugin.GuiState.MouseOverRegionID)
  end else
  begin
    CurRegion := Plugin.FocusedRegion;
  end;

  if (assigned(CurRegion)) and (CurRegion.GetSample^.Properties.IsValid) then
  begin
    SampleInfo.IsValid := true;
    SampleInfo.ChannelCount := CurRegion.GetSample^.Properties.ChannelCount;
    SampleInfo.SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;


    Par.BackgroundColor := kColor_LcdDark1;
    Par.LineColor       := kColor_SampleDisplayLine;
    Par.ImageWidth      := SampleDisplay.ClientRect.Width;
    Par.ImageHeight     := SampleDisplay.ClientRect.Height;
    Par.Zoom            := Zoom;
    Par.Offset          := Offset;
    Par.VertGain        := DecibelsToLinear(CurRegion.GetProperties^.SampleVolume);

    xSampleImage := SampleRenderer.RenderSample(Par, CurRegion, nil);
    SampleDisplay.DrawSample(xSampleImage);


    Par.BackgroundColor := kColor_LcdDark1;
    Par.LineColor       := kColor_SampleDisplayLine;
    Par.ImageWidth      := ZoomSampleDisplay.ClientRect.Width;
    Par.ImageHeight     := ZoomSampleDisplay.ClientRect.Height;
    Par.Zoom            := 0;
    Par.Offset          := 0;
    Par.VertGain        := DecibelsToLinear(CurRegion.GetProperties^.SampleVolume);

    xSampleImage := SampleRenderer.RenderSample(Par, CurRegion, nil);
    ZoomSampleDisplay.DrawSample(xSampleImage);





    {
    if CurRegion.GetSample^.Properties.ChannelCount = 1 then
    begin
      SampleInfo.Ch1 := CurRegion.GetSample^.Properties.Ch1;
    end;

    if CurRegion.GetSample^.Properties.ChannelCount = 2 then
    begin
      SampleInfo.Ch1 := CurRegion.GetSample^.Properties.Ch1;
      SampleInfo.Ch2 := CurRegion.GetSample^.Properties.Ch2;
    end;

    //== Main Sample Display ==
    SampleDisplay.DrawSample(SampleInfo);
    SampleDisplay.Invalidate;

    //== zoom sample display ==
    ZoomSampleDisplay.DrawSample(SampleInfo);
    ZoomSampleDisplay.Invalidate;
    }



    //== Sample Overlay ==
    SampleOverlay.BeginUpdate;
    try
      SampleOverlay.Visible     := true;
      SampleOverlay.SetSampleInfo(true, CurRegion.GetSample^.Properties.SampleFrames);
      SampleOverlay.SampleStart := CurRegion.GetProperties^.SampleStart;
      SampleOverlay.SampleEnd   := CurRegion.GetProperties^.SampleEnd;
      SampleOverlay.LoopStart   := CurRegion.GetProperties^.LoopStart;
      SampleOverlay.LoopEnd     := CurRegion.GetProperties^.LoopEnd;
      SampleOverlay.ShowLoopPoints := ShowLoopMarkers(CurRegion);
      SampleOverlay.SetZoomOffset(Zoom, Offset);
    finally
      SampleOverlay.EndUpdate;
    end;

    //== Zoom Sample Overlay ==
    ZoomSampleOverlay.BeginUpdate;
    try
      ZoomSampleOverlay.Visible     := true;
      ZoomSampleOverlay.SetSampleInfo(true, CurRegion.GetSample^.Properties.SampleFrames);
      ZoomSampleOverlay.SampleStart := CurRegion.GetProperties^.SampleStart;
      ZoomSampleOverlay.SampleEnd   := CurRegion.GetProperties^.SampleEnd;
      ZoomSampleOverlay.LoopStart   := CurRegion.GetProperties^.LoopStart;
      ZoomSampleOverlay.LoopEnd     := CurRegion.GetProperties^.LoopEnd;
      ZoomSampleOverlay.ShowLoopPoints := ShowLoopMarkers(CurRegion);
      ZoomSampleOverlay.SetZoomOffset(0, 0);
    finally
      ZoomSampleOverlay.EndUpdate;
    end;


    //=======
    if ShowLoopMarkers(CurRegion) then
    begin
      ZoomLoopStartButton.Visible := true;
      ZoomLoopEndButton.Visible   := true;
    end else
    begin
      ZoomLoopStartButton.Visible := false;
      ZoomLoopEndButton.Visible   := false;
    end;



  end else
  begin
    SampleInfo.IsValid := false;
    SampleDisplay.ClearSample(true);

    //== Sample Overlay ==
    SampleOverlay.Visible := false;
    ZoomSampleOverlay.Visible := false
  end;
end;

procedure TLoopEditForm.SampleZoomControlChanged(Sender: TObject);
var
  tx : single;
  IndexA, IndexB : single;
  SampleFrames, DisplayPixelWidth : integer;
  xZoom, xOffset : single;
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

    CalcZoomOffset(IndexA, IndexB, SampleFrames, DisplayPixelWidth, xZoom, xOffset);

    Zoom := xZoom;
    Offset := xOffset;

    UpdateSampleDisplay;
  end;
end;

procedure TLoopEditForm.SampleOverlay_MarkerChanged(Sender:TObject; Marker:TSampleMarker; NewPosition : integer);
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

  GuiEvent_SampleMakersChanged;
  Plugin.Globals.SendWindowsMessage(UM_SAMPLE_MARKERS_CHANGED);

end;

procedure TLoopEditForm.ZoomButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tag : integer;
  Zoom, Offset : double;
  SampleFrames, DisplayPixelWidth : integer;
  IndexA, IndexB : single;
begin
  Tag := (Sender as TVamTextBox).Tag;

  if Button = mbLeft then
  begin
    SampleFrames      := SampleInfo.SampleFrames;
    DisplayPixelWidth := SampleDisplay.Width;


    if Tag = 1 then
    begin
      //Zoom in.
      Zoom   := SampleDisplay.Zoom + ((1 - SampleDisplay.Zoom) * 0.25);
      Offset := SampleDisplay.Offset;
      SampleDisplay.Zoom := Zoom;
      SampleDisplay.Offset := Offset;
      SampleOverlay.SetZoomOffset(Zoom, Offset);
      ZoomSampleOverlay.SetZoomOffset(0,0);
      SampleDisplay.Invalidate;

      assert(Zoom >= 0);
      assert(Zoom <= 1);
      assert(Offset >= 0);
      assert(Offset <= 1);

      CalcZoomBounds(Zoom, Offset, SampleFrames, DisplayPixelWidth, IndexA, IndexB);

      SampleZoomControl.IndexA := IndexA;
      SampleZoomControl.IndexB := IndexB;
      SampleZoomControl.Invalidate;
    end;

    if Tag = 2 then
    begin
      //Zoom out.
      Zoom   := SampleDisplay.Zoom - ((1 - SampleDisplay.Zoom) * 0.5);
      if Zoom <= 0.15 then Zoom := 0;
      Offset := SampleDisplay.Offset;
      SampleDisplay.Zoom := Zoom;
      SampleDisplay.Offset := Offset;
      SampleOverlay.SetZoomOffset(Zoom, Offset);
      ZoomSampleOverlay.SetZoomOffset(0,0);
      SampleDisplay.Invalidate;

      assert(Zoom >= 0);
      assert(Zoom <= 1);
      assert(Offset >= 0);
      assert(Offset <= 1);

      CalcZoomBounds(Zoom, Offset, SampleFrames, DisplayPixelWidth, IndexA, IndexB);

      SampleZoomControl.IndexA := IndexA;
      SampleZoomControl.IndexB := IndexB;
      SampleZoomControl.Invalidate;
    end;


    if Tag = 3 then ZoomToSampleMarker(self, TDialogSampleMarker.SampleStart);
    if Tag = 4 then ZoomToSampleMarker(self, TDialogSampleMarker.SampleEnd);
    if Tag = 5 then ZoomToSampleMarker(self, TDialogSampleMarker.LoopStart);
    if Tag = 6 then ZoomToSampleMarker(self, TDialogSampleMarker.LoopEnd);
    if Tag = 7 then ZoomMarkerMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);

    if Tag = 8 then
    begin
      SampleDisplay.Zoom := 0;
      SampleDisplay.Offset := 0;
      SampleOverlay.SetZoomOffset(0, 0);
      ZoomSampleOverlay.SetZoomOffset(0,0);
      SampleDisplay.Invalidate;

      CalcZoomBounds(0, 0, SampleFrames, DisplayPixelWidth, IndexA, IndexB);

      SampleZoomControl.IndexA := IndexA;
      SampleZoomControl.IndexB := IndexB;
      SampleZoomControl.Invalidate;
    end;


  end;

end;

procedure TLoopEditForm.ZoomToSampleMarker(Sender: TObject; Marker: TDialogSampleMarker);
var
  CurRegion : IRegion;
  TargetSampleFrame : integer;
  SampleFrames : integer;
  DisplayPixelWidth : integer;

  ZoomPos : TZoomPos;
begin
  if not assigned(Plugin)         then exit;
  if (SampleInfo.IsValid = false) then exit;

  // TODO: NOTE: Getting the current sample region is a pretty hacky way to do things here.
  // This loop edit frame is kind of like a dialog.
  // It needs an equivlent Show() method that will initialize the dialog/frame starting
  // state and something to be called when the dialog/frame closes so it can clean up
  // after itself.
  if Plugin.GuiState.MouseOverRegionID <> TGuidEx.EmptyGuid then
  begin
    CurRegion := Plugin.SampleMap.FindRegionByUniqueID(Plugin.GuiState.MouseOverRegionID)
  end else
  begin
    CurRegion := Plugin.FocusedRegion;
  end;

  if not assigned(CurRegion) then exit;

  // Safty check: Check the current sample is loaded and is valid.
  assert(CurRegion.GetSample^.Properties.IsValid);


  case Marker of
    TDialogSampleMarker.SampleStart: TargetSampleFrame := CurRegion.GetProperties^.SampleStart;
    TDialogSampleMarker.SampleEnd:   TargetSampleFrame := CurRegion.GetProperties^.SampleEnd;
    TDialogSampleMarker.LoopStart:   TargetSampleFrame := CurRegion.GetProperties^.LoopStart;
    TDialogSampleMarker.LoopEnd:     TargetSampleFrame := CurRegion.GetProperties^.LoopEnd;
  else
    raise Exception.Create('Marker type not handled.');
  end;

  SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;
  DisplayPixelWidth := SampleDisplay.Width;

  ZoomPos := CalcZoomPos(SampleFrames, DisplayPixelWidth, TargetSampleFrame);


  //== Set a bunch of zoom/offset values ==
  SampleDisplay.Zoom   := ZoomPos.Zoom;
  SampleDisplay.Offset := ZoomPos.Offset;
  SampleOverlay.SetZoomOffset(ZoomPos.Zoom, ZoomPos.Offset);
  ZoomSampleOverlay.SetZoomOffset(0,0);
  SampleDisplay.Invalidate;

  SampleZoomControl.IndexA := ZoomPos.IndexA;
  SampleZoomControl.IndexB := ZoomPos.IndexB;
  SampleZoomControl.Invalidate;

end;

procedure TLoopEditForm.GuiEvent_SampleMakersChanged;
var
  CurRegion : IRegion;
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

  if Plugin.GuiState.MouseOverRegionID <> TGuidEx.EmptyGuid then
  begin
    CurRegion := Plugin.SampleMap.FindRegionByUniqueID(Plugin.GuiState.MouseOverRegionID)
  end else
  begin
    CurRegion := Plugin.FocusedRegion;
  end;

  if (assigned(CurRegion)) and (CurRegion.GetSample^.Properties.IsValid) then
  begin
    SampleInfo.IsValid := true;
    SampleInfo.ChannelCount := CurRegion.GetSample^.Properties.ChannelCount;
    SampleInfo.SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;

    //== Sample Overlay ==
    SampleOverlay.BeginUpdate;
    try
      SampleOverlay.Visible     := true;
      SampleOverlay.SetSampleInfo(true, SampleInfo.SampleFrames);
      SampleOverlay.SampleStart := CurRegion.GetProperties^.SampleStart;
      SampleOverlay.SampleEnd   := CurRegion.GetProperties^.SampleEnd;
      SampleOverlay.LoopStart   := CurRegion.GetProperties^.LoopStart;
      SampleOverlay.LoopEnd     := CurRegion.GetProperties^.LoopEnd;
      SampleOverlay.ShowLoopPoints := ShowLoopMarkers(CurRegion);
    finally
      SampleOverlay.EndUpdate;
    end;

    //== Sample Overlay ==
    ZoomSampleOverlay.BeginUpdate;
    try
      ZoomSampleOverlay.Visible     := true;
      ZoomSampleOverlay.SetSampleInfo(true, SampleInfo.SampleFrames);
      ZoomSampleOverlay.SampleStart := CurRegion.GetProperties^.SampleStart;
      ZoomSampleOverlay.SampleEnd   := CurRegion.GetProperties^.SampleEnd;
      ZoomSampleOverlay.LoopStart   := CurRegion.GetProperties^.LoopStart;
      ZoomSampleOverlay.LoopEnd     := CurRegion.GetProperties^.LoopEnd;
      ZoomSampleOverlay.ShowLoopPoints := ShowLoopMarkers(CurRegion);
    finally
      ZoomSampleOverlay.EndUpdate;
    end;
  end else
  begin
    //== Sample Overlay ==
    SampleOverlay.Visible := false;
    ZoomSampleOverlay.Visible := false
  end;
end;


{ TLoopEditDialog }

constructor TLoopEditDialog.Create;
begin

end;

destructor TLoopEditDialog.Destroy;
begin
  if assigned(Form)
    then Form.Free;

  inherited;
end;

procedure TLoopEditDialog.Setup(ParentContainer: TWinControl; aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
var
  aW, aH, aT, aL : integer;
begin
  Form := TLoopEditForm.CreateParented(ParentContainer.Handle);

  Form.BackgroundPanel.Align := alNone;
  Form.BackgroundPanel.Visible := false;
  Form.BackgroundPanel.Parent := ParentContainer;

  aW := round(ParentContainer.Width * (2/3));
  aH := round(ParentContainer.Height * (2/3));
  aL := round((ParentContainer.Width  - aW)  * (1/2));
  aT := round((ParentContainer.Height - aH) * (1/3));

  Form.BackgroundPanel.Width := aW;
  Form.BackgroundPanel.Height := aH;
  Form.BackgroundPanel.Left := aL;
  Form.BackgroundPanel.Top  := aT;


  Form.BackgroundPanel.Padding.SetBounds(16,4,16,16);
  Form.BackgroundPanel.CornerRadius1 := 3;
  Form.BackgroundPanel.CornerRadius2 := 3;
  Form.BackgroundPanel.CornerRadius3 := 3;
  Form.BackgroundPanel.CornerRadius4 := 3;

  Form.InitializeFrame(aPlugin, aGuiStandard);

  Form.UpdateSampleDisplay;

  Form.BackgroundPanel.Visible := true;
end;

end.
