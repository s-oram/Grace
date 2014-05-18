unit uVoiceSetupFrame;

interface

uses
  uGuiFeedbackData,
  VamLib.ZeroObject, eePlugin, eeGuiStandardv2,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamKnob, VamLabel, VamDiv, VamXYPad;

type
  TVoiceSetupFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    MacroKnobDiv: TVamDiv;
    MacroDivLabel: TVamLabel;
    XYPad1: TVamXYPad;
    XYPad2: TVamXYPad;
    XYPad3: TVamXYPad;
    XYPad4: TVamXYPad;
    PadLabel1: TVamLabel;
    PadLabel2: TVamLabel;
    PadLabel3: TVamLabel;
    PadLabel4: TVamLabel;
    procedure BackgroundPanelResize(Sender: TObject);
  private
  private
    FMotherShip : IMothership;
    procedure SetMotherShipReference(aMotherShip : IMothership);
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    Plugin : TeePlugin;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:eeGuiStandardv2.TGuiStandard);

    procedure UpdateGui(Sender:TObject; FeedBack: PGuiFeedbackData);
  end;

implementation

uses
  RedFox,
  VamLayoutWizard,
  uConstants;

{$R *.dfm}

{ TVoiceSetupFrame }

constructor TVoiceSetupFrame.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TVoiceSetupFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  inherited;
end;

procedure TVoiceSetupFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TVoiceSetupFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: eeGuiStandardv2.TGuiStandard);
const
  PadWidth = 120;
  PadHeight = 120;
  PadXOffset = 24;
begin
  Plugin := aPlugin;

  XYPad1.Layout.SetSize(PadWidth, PadHeight).SetPos(0, TGuiConst.SectionLabelHeight + 8);
  XYPad2.Layout.SetSize(PadWidth, PadHeight).Anchor(XYPad1).SnapToEdge(TControlFeature.RightEdge).Move(PadXOffset,0);
  XYPad3.Layout.SetSize(PadWidth, PadHeight).Anchor(XYPad2).SnapToEdge(TControlFeature.RightEdge).Move(PadXOffset,0);
  XYPad4.Layout.SetSize(PadWidth, PadHeight).Anchor(XYPad3).SnapToEdge(TControlFeature.RightEdge).Move(PadXOffset,0);

  PadLabel1.Layout.Anchor(XYPad1).SnapToEdge(TControlFeature.BottomEdge).Move(0,4);
  PadLabel2.Layout.Anchor(XYPad2).SnapToEdge(TControlFeature.BottomEdge).Move(0,4);
  PadLabel3.Layout.Anchor(XYPad3).SnapToEdge(TControlFeature.BottomEdge).Move(0,4);
  PadLabel4.Layout.Anchor(XYPad4).SnapToEdge(TControlFeature.BottomEdge).Move(0,4);

  MacroKnobDiv.Width  := WidthOfControls(XYPad1, XYPad4);
  MacroKnobDiv.Height := HeightOfControls(MacroDivLabel, PadLabel1);

  PadLabel1.Text := 'XY Pad 1';
  PadLabel2.Text := 'XY Pad 2';
  PadLabel3.Text := 'XY Pad 3';
  PadLabel4.Text := 'XY Pad 4';
end;

procedure TVoiceSetupFrame.BackgroundPanelResize(Sender: TObject);
begin
  MacroKnobDiv.Layout.AlignWithinParent(TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignNear).Move(0,8);
end;


procedure TVoiceSetupFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin

end;

procedure TVoiceSetupFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin

end;




end.
