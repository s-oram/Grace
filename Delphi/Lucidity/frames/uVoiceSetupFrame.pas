unit uVoiceSetupFrame;

interface

uses
  VamLib.ZeroObject, eePlugin, eeGuiStandardv2,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, VamKnob, VamLabel, VamDiv;

type
  TVoiceSetupFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
    MacroKnobDiv: TVamDiv;
    MacroDivLabel: TVamLabel;
    MacroKnobLabel3: TVamLabel;
    MacroKnobLabel2: TVamLabel;
    MacroKnobLabel1: TVamLabel;
    MacroKnob1: TVamKnob;
    MacroKnob2: TVamKnob;
    MacroKnob3: TVamKnob;
    MacroKnob4: TVamKnob;
    MacroKnobLabel4: TVamLabel;
    MacroKnob5: TVamKnob;
    MacroKnobLabel5: TVamLabel;
    MacroKnobLabel6: TVamLabel;
    MacroKnob6: TVamKnob;
    MacroKnob7: TVamKnob;
    MacroKnobLabel7: TVamLabel;
    MacroKnobLabel8: TVamLabel;
    MacroKnob8: TVamKnob;
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
  end;

implementation

uses
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
begin
  Plugin := aPlugin;

  MacroKnob1.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight).SetPos(0, TGuiConst.SectionLabelHeight);
  MacroKnob2.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight).Anchor(MacroKnob1).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  MacroKnob3.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight).Anchor(MacroKnob2).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  MacroKnob4.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight).Anchor(MacroKnob3).SnapToEdge(TControlFeature.RightEdge).Move(8,0);

  MacroKnob5.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight).Anchor(MacroKnob1).SnapToEdge(TControlFeature.BottomEdge).Move(0,28);
  MacroKnob6.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight).Anchor(MacroKnob5).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  MacroKnob7.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight).Anchor(MacroKnob6).SnapToEdge(TControlFeature.RightEdge).Move(8,0);
  MacroKnob8.Layout.SetSize(TGuiConst.KnobWidth, TGuiConst.KnobHeight).Anchor(MacroKnob7).SnapToEdge(TControlFeature.RightEdge).Move(8,0);

  MacroKnobLabel1.Layout.Anchor(MacroKnob1).SnapToEdge(TControlFeature.BottomEdge);
  MacroKnobLabel2.Layout.Anchor(MacroKnob2).SnapToEdge(TControlFeature.BottomEdge);
  MacroKnobLabel3.Layout.Anchor(MacroKnob3).SnapToEdge(TControlFeature.BottomEdge);
  MacroKnobLabel4.Layout.Anchor(MacroKnob4).SnapToEdge(TControlFeature.BottomEdge);
  MacroKnobLabel5.Layout.Anchor(MacroKnob5).SnapToEdge(TControlFeature.BottomEdge);
  MacroKnobLabel6.Layout.Anchor(MacroKnob6).SnapToEdge(TControlFeature.BottomEdge);
  MacroKnobLabel7.Layout.Anchor(MacroKnob7).SnapToEdge(TControlFeature.BottomEdge);
  MacroKnobLabel8.Layout.Anchor(MacroKnob8).SnapToEdge(TControlFeature.BottomEdge);


  MacroKnobDiv.Width := WidthOfControls(MacroKnob1, MacroKnob4);
  MacroKnobDiv.Height := HeightOfControls(MacroDivLabel, MacroKnobLabel8);
  MacroKnobDiv.Top := 16;
  MacroKnobDiv.Left := 300;


end;




procedure TVoiceSetupFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin

end;


end.
