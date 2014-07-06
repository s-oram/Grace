unit uXYPadsFrame;

interface

{$INCLUDE Defines.inc}

uses
  uGuiFeedbackData, Menu.XYPadContextMenu,
  VamLib.ZeroObject, eePlugin, eeGuiStandardv2,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
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
    procedure ProcessZeroObjectMessage(MsgID:cardinal; Data:Pointer);
  protected
    Plugin : TeePlugin;
    PadContextMenu : TXYPadContextMenu;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InitializeFrame(aPlugin : TeePlugin; aGuiStandard:eeGuiStandardv2.TGuiStandard);

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
  PadContextMenu := TXYPadContextMenu.Create;
end;

destructor TXYPadsFrame.Destroy;
begin
  if (assigned(FMotherShip)) then
  begin
    FMotherShip.DeregisterZeroObject(self);
    FMotherShip := nil;
  end;

  PadContextMenu.Free;

  inherited;
end;

procedure TXYPadsFrame.SetMotherShipReference(aMotherShip: IMothership);
begin
  FMotherShip := aMotherShip;
end;

procedure TXYPadsFrame.InitializeFrame(aPlugin: TeePlugin; aGuiStandard: eeGuiStandardv2.TGuiStandard);
const
  PadWidth = 120;
  PadHeight = 120;
  PadXOffset = 24;
begin
  Plugin := aPlugin;

  PadContextMenu.Plugin := aPlugin;

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
end;

procedure TXYPadsFrame.BackgroundPanelResize(Sender: TObject);
begin
  XYPadsContainer.Layout.AlignWithinParent(TRedFoxAlign.AlignCenter, TRedFoxAlign.AlignNear).Move(0,8);
end;


procedure TXYPadsFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin

end;

procedure TXYPadsFrame.UpdateGui(Sender: TObject; FeedBack: PGuiFeedbackData);
begin
  assert(assigned(Plugin));

  if XYPad1.PosX <> Plugin.XYPads.PadX1 then XYPad1.PosX := Plugin.XYPads.PadX1;
  if XYPad2.PosX <> Plugin.XYPads.PadX2 then XYPad2.PosX := Plugin.XYPads.PadX2;
  if XYPad3.PosX <> Plugin.XYPads.PadX3 then XYPad3.PosX := Plugin.XYPads.PadX3;
  if XYPad4.PosX <> Plugin.XYPads.PadX4 then XYPad4.PosX := Plugin.XYPads.PadX4;

  if XYPad1.PosY <> Plugin.XYPads.PadY1 then XYPad1.PosY := Plugin.XYPads.PadY1;
  if XYPad2.PosY <> Plugin.XYPads.PadY2 then XYPad2.PosY := Plugin.XYPads.PadY2;
  if XYPad3.PosY <> Plugin.XYPads.PadY3 then XYPad3.PosY := Plugin.XYPads.PadY3;
  if XYPad4.PosY <> Plugin.XYPads.PadY4 then XYPad4.PosY := Plugin.XYPads.PadY4;
end;


procedure TXYPadsFrame.XYPadChanged(Sender: TObject);
var
  Tag : integer;
  PadX, PadY : single;
begin
  Tag := (Sender as TVamXYPad).Tag;

  PadX := (Sender as TVamXYPad).PosX;
  PadY := (Sender as TVamXYPad).PosY;

  case Tag of
    1:
    begin
      Plugin.XYPads.PadX1 := PadX;
      Plugin.XYPads.PadY1 := PadY;
    end;

    2:
    begin
      Plugin.XYPads.PadX2 := PadX;
      Plugin.XYPads.PadY2 := PadY;
    end;

    3:
    begin
      Plugin.XYPads.PadX3 := PadX;
      Plugin.XYPads.PadY3 := PadY;
    end;

    4:
    begin
      Plugin.XYPads.PadX4 := PadX;
      Plugin.XYPads.PadY4 := PadY;
    end;
  else
    raise Exception.Create('Index not handled.');
  end;
end;

procedure TXYPadsFrame.XYPadMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tag : integer;
  Name1, Name2 : string;
begin
  if (Button = TMouseButton.mbLeft) and (ssCtrl in Shift) then
  begin
    Tag := (Sender as TVamXYPad).Tag;

    case Tag of
      1:
      begin
        Name1 := PluginParToName(TPluginParameter.PadX1);
        Name2 := PluginParToName(TPluginParameter.PadY1);
      end;

      2:
      begin
        Name1 := PluginParToName(TPluginParameter.PadX2);
        Name2 := PluginParToName(TPluginParameter.PadY2);
      end;

      3:
      begin
        Name1 := PluginParToName(TPluginParameter.PadX3);
        Name2 := PluginParToName(TPluginParameter.PadY3);
      end;

      4:
      begin
        Name1 := PluginParToName(TPluginParameter.PadX4);
        Name2 := PluginParToName(TPluginParameter.PadY4);
      end;
    else
      Name1 := '';
      Name2 := '';
      raise Exception.Create('Tag not handled.');
    end;

    Plugin.ResetPluginParameter(TParChangeScope.psFocused, Name1);
    Plugin.ResetPluginParameter(TParChangeScope.psFocused, Name2);
  end;

  if Button = TMouseButton.mbRight then
  begin
    Tag := (Sender as TVamXYPad).Tag;
    PadContextMenu.TargetXYPadIndex := Tag - 1; //NOTE: The (Tag-1) is because the pads are numbered 1 to 4.
    PadContextMenu.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;

end;

end.
