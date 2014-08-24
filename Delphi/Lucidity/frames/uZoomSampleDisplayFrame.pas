unit uZoomSampleDisplayFrame;

interface

//TODO:HIGH I don't think this zoom sample frame unit is needed anymore. Delete!

uses
  Math,
  LuciditySampleOverlay,
  Lucidity.SampleImageRenderer,
  Lucidity.FlexSampleRenderer,
  eePlugin, eeGuiStandardv2,
  VamLib.ZeroObject,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamSampleDisplay;

type
  TZoomSampleDisplayFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    SampleDisplayContainer: TVamPanel;
    SampleDisplay: TVamSampleDisplay;
    procedure SampleDisplayContainerResize(Sender: TObject);
  private
    Plugin:TeePlugin;
    GuiStandard : TGuiStandard;
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IZeroMessageData); 
  protected
    SampleRenderer : TSampleImageRenderer;
    FlexSampleRender : TFlexSampleImageRenderer;
    SampleInfo    : TSampleDisplayInfo;
    Zoom, Offset : single;

    //The SampleOverlay GUI components show the sample start/end markers and other
    // information that is drawn over the sample display.
    SampleOverlay     : TLuciditySampleOverlay;

    procedure UpdateSampleDisplay;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);
  end;

implementation

uses
  uGuiUtils,
  eeDSP,
  VamLib.Graphics,
  Lucidity.Interfaces,
  GuiDEx,
  uConstants;

{$R *.dfm}

{ TZoomSampleDisplayFrame }

constructor TZoomSampleDisplayFrame.Create(AOwner: TComponent);
begin
  inherited;

  SampleRenderer := TSampleImageRenderer.Create;
  FlexSampleRender := TFlexSampleImageRenderer.Create;

  SampleInfo.IsValid := false;

  SampleOverlay := TLuciditySampleOverlay.Create(self.Owner);
  SampleOverlay.Parent := SampleDisplayContainer;
  SampleOverlay.Visible := true;
  SampleOverlay.Margins.Left   := 0;
  SampleOverlay.Margins.Right  := 0;
  SampleOverlay.Margins.Top    := 0;
  SampleOverlay.Margins.Bottom := 0;
  SampleOverlay.SetZoomOffset(0,0);
  //SampleOverlay.OnSampleMarkerChanged := SampleOverlay_MarkerChanged;
  SampleOverlay.ShowMarkerTags := true;

end;

destructor TZoomSampleDisplayFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  SampleRenderer.Free;
  FlexSampleRender.Free;

  inherited;
end;

procedure TZoomSampleDisplayFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: TGuiStandard);
begin
  Plugin := aPlugin;
  GuiStandard := aGuiStandard;

  SampleInfo.IsValid := false;

  //==== Sample Display Container ====
  SampleDisplayContainer.Color := kColor_LcdDark1;
  SampleDisplayContainer.CornerRadius1 := 3;
  SampleDisplayContainer.CornerRadius2 := 3;
  SampleDisplayContainer.CornerRadius3 := 3;
  SampleDisplayContainer.CornerRadius4 := 3;

  SampleDisplayContainer.Padding.SetBounds(4,4,4,4);
  SampleDisplayContainer.AlignWithMargins := true;
  SampleDisplayContainer.Margins.SetBounds(0,0,0,4);





  UpdateSampleDisplay;
end;

procedure TZoomSampleDisplayFrame.SampleDisplayContainerResize(Sender: TObject);
begin
  SampleDisplay.AlignToParent(true);
  SampleOverlay.AlignToParent(true);
  UpdateSampleDisplay;
end;

procedure TZoomSampleDisplayFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TZoomSampleDisplayFrame.UpdateSampleDisplay;
var
  CurRegion : IRegion;
  xSampleImage : IInterfacedBitmap;
  //Par:TSampleRenderParameters;
  FlexPar:TFlexRenderPar;

  RegionLoopStart, RegionLoopEnd : integer;
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

  if Plugin.Globals.GuiState.MouseOverRegionID <> TGuidEx.EmptyGuid then
  begin
    CurRegion := Plugin.SampleMap.FindRegionByUniqueID(Plugin.Globals.GuiState.MouseOverRegionID)
  end else
  begin
    CurRegion := Plugin.FocusedRegion;
  end;

  if (assigned(CurRegion)) and (CurRegion.GetSample^.Properties.IsValid) then
  begin
    SampleInfo.IsValid := true;
    SampleInfo.ChannelCount := CurRegion.GetSample^.Properties.ChannelCount;
    SampleInfo.SampleFrames := CurRegion.GetSample^.Properties.SampleFrames;


    FlexPar.BackgroundColor := kColor_LcdDark1;
    FlexPar.LineColor       := kColor_SampleDisplayLine;
    FlexPar.ImageWidth      := SampleDisplay.ClientRect.Width;
    FlexPar.ImageHeight     := SampleDisplay.ClientRect.Height;
    FlexPar.Zoom            := Zoom;
    FlexPar.Offset          := Offset;
    FlexPar.VertGain        := DecibelsToLinear(CurRegion.GetProperties^.SampleVolume);

    xSampleImage := FlexSampleRender.RenderSample(CurRegion, FlexPar);
    SampleDisplay.DrawSample(xSampleImage);


    //== Sample Overlay ==
    SampleOverlay.BeginUpdate;
    try
      CurRegion.GetProperties^.GetRegionLoopPoints(RegionLoopStart, RegionLoopEnd);
      if RegionLoopStart = -1 then RegionLoopStart := CurRegion.GetProperties^.SampleStart;
      if RegionLoopEnd = -1   then RegionLoopEnd   := CurRegion.GetProperties^.SampleEnd;

      SampleOverlay.Visible     := true;
      SampleOverlay.SetSampleInfo(true, CurRegion.GetSample^.Properties.SampleFrames);
      SampleOverlay.SampleStart := CurRegion.GetProperties^.SampleStart;
      SampleOverlay.SampleEnd   := CurRegion.GetProperties^.SampleEnd;
      SampleOverlay.LoopStart   := RegionLoopStart;
      SampleOverlay.LoopEnd     := RegionLoopEnd;

      //SampleOverlay.ShowLoopPoints := ShowLoopMarkers(CurRegion);
      SampleOverlay.ShowLoopPoints := true;

      SampleOverlay.SetZoomOffset(Zoom, Offset);
    finally
      SampleOverlay.EndUpdate;
    end;

    //=======
    {
    if ShowLoopMarkers(CurRegion) then
    begin
      ZoomLoopStartButton.Visible := true;
      ZoomLoopEndButton.Visible   := true;
    end else
    begin
      ZoomLoopStartButton.Visible := false;
      ZoomLoopEndButton.Visible   := false;
    end;
    }


  end else
  begin
    SampleInfo.IsValid := false;
    SampleDisplay.ClearSample(true);

    //== Sample Overlay ==
    SampleOverlay.Visible := false;
  end;
end;

procedure TZoomSampleDisplayFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IZeroMessageData);
begin

end;



end.
