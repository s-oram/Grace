unit uVoiceSetupFrame;

interface

uses
  VamLib.ZeroObject, eePlugin, eeGuiStandardv2,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer;

type
  TVoiceSetupFrame = class(TFrame, IZeroObject)
    Panel: TRedFoxContainer;
    BackgroundPanel: TVamPanel;
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
end;

procedure TVoiceSetupFrame.ProcessZeroObjectMessage(MsgID: cardinal; Data: Pointer);
begin

end;


end.
