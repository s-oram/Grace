unit uXYPadsFrame;

interface

{$INCLUDE Defines.inc}

uses
  uGuiFeedbackData,
  VamLib.ZeroObject, eePlugin, Lucidity.GuiStandard,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamKnob, VamLabel, VamDiv, VamXYPad;

type
  TXYPadsFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    XYPadsContainer: TVamDiv;
    XYPadsContainerLabel: TVamLabel;
    XYPad1: TVamXYPad;
    XYPad2: TVamXYPad;
    XYPad3: TVamXYPad;
    XYPad4: TVamXYPad;
    PadLabel1: TVamLabel;
    PadLabel2: TVamLabel;
    PadLabel3: TVamLabel;
    PadLabel4: TVamLabel;
    procedure BackgroundPanelResize(Sender: TObject);
    procedure XYPadChanged(Sender: TObject);
    procedure XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer; DataB:IInterface);
  protected
    Plugin : TeePlugin;
    GuiStandard : TGuiStandard;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:TGuiStandard);

    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);
  end;

implementation

uses
  eeTypes,
  Lucidity.PluginParameters,
  Lucidity.Types,
  RedFox,
  VamLayoutWizard,
  uConstants;

{$R *.dfm}

{ TVoiceSetupFrame }

constructor TXYPadsFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TXYPadsFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;
  inherited;
end;

procedure TXYPadsFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TXYPadsFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard:TGuiStandard);
const
  PadWidth = 120;
  PadHeight = 120;
  PadXOffset = 24;
begin
  Plugin := aPlugin;
  GuiStandard := aGuiStandard;

  XYPad1.Layout.SetSize(PadWidth, PadHeight).SetPos(0, TGuiConst.SectionLabelHeight + 8);
  XYPad2.Layout.SetSize(PadWidth, PadHeight).Anchor(XYPad1).SnapToEdge(TControlFeature.RightEdge).Move(PadXOffset,0);
  XYPad3.Layout.SetSize(PadWidth, PadHeight).Anchor(XYPad2).SnapToEdge(TControlFeature.RightEdge).Move(PadXOffset,0);
  XYPad4.Layout.SetSize(PadWidth, PadHeight).Anchor(XYPad3).SnapToEdge(TControlFeature.RightEdge).Move(PadXOffset,0);

  PadLabel1.Layout.Anchor(XYPad1).SnapToEdge(TControlFeature.BottomEdge).Move(0,4);
  PadLabel2.Layout.Anchor(XYPad2).SnapToEdge(TControlFeature.BottomEdge).Move(0,4);
  PadLabel3.Layout.Anchor(XYPad3).SnapToEdge(TControlFeature.BottomEdge).Move(0,4);
  PadLabel4.Layout.Anchor(XYPad4).SnapToEdge(TControlFeature.BottomEdge).Move(0,4);

  XYPadsContainer.Width  := WidthOfControls(XYPad1, XYPad4);
  XYPadsContainer.Height := HeightOfControls(XYPadsContainerLabel, PadLabel1);

  PadLabel1.Text := 'XY Pad 1';
  PadLabel2.Text := 'XY Pad 2';
  PadLabel3.Text := 'XY Pad 3';
  PadLabel4.Text := 'XY Pad 4';

  // == set pad parameter names ==
  XYPad1.ParameterNameX := PluginParToName(TPluginParameter.PadX1);
  XYPad1.ParameterNameY := PluginParToName(TPluginParameter.PadY1);
  XYPad2.ParameterNameX := PluginParToName(TPluginParameter.PadX2);
  XYPad2.ParameterNameY := PluginParToName(TPluginParameter.PadY2);
  XYPad3.ParameterNameX := PluginParToName(TPluginParameter.PadX3);
  XYPad3.ParameterNameY := PluginParToName(TPluginParameter.PadY3);
  XYPad4.ParameterNameX := PluginParToName(TPluginParameter.PadX4);
  XYPad4.ParameterNameY := PluginParToName(TPluginParameter.PadY4);

  // == register the pads to the handler ==
  GuiStandard.XyPadHandler.RegisterControl(XyPad1);
  GuiStandard.XyPadHandler.RegisterControl(XyPad2);
  GuiStandard.XyPadHandler.RegisterControl(XyPad3);
  GuiStandard.XyPadHandler.RegisterControl(XyPad4);
end;

procedure TXYPadsFrame.BackgroundPanelResize(Sender: TObject);
begin
  XYPadsContainer.Layout.AlignWithinParent(TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignNear).Move(0,8);
end;

procedure TXYPadsFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer; DataB:IInterface);
begin

end;

procedure TXYPadsFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin
  // TODO:MED delete
end;


procedure TXYPadsFrame.XYPadChanged(Sender: TObject);
begin
  // TODO:MED delete
end;

procedure TXYPadsFrame.XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // TODO:MED delete
end;

end.
