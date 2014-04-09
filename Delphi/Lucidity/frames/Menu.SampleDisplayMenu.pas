unit Menu.SampleDisplayMenu;

interface

uses
  LucidityInterfaces,
  eePlugin, Vcl.Menus;


type
  TSampleDisplayMenu = class
  private
  protected
    Plugin : TeePlugin;
    Menu : TPopUpMenu;

    procedure EventHandle_NormaliseSample(Sender : TObject);
    procedure EventHandle_EditSamplePoints(Sender : TObject);
    procedure EventHandle_EditSampleMap(Sender : TObject);
    procedure EventHandle_ShowInWindowsExplorer(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin);

    procedure Popup(const x, y : integer);

  end;

implementation

uses
  uGuiUtils,
  SysUtils,
  eeWinEx,
  Lucidity.SampleMap,
  uConstants;

{ TSampleDisplayMenu }

constructor TSampleDisplayMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TSampleDisplayMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TSampleDisplayMenu.Initialize(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

procedure TSampleDisplayMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
begin
  Menu.Items.Clear;


  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Normalise Sample';
  mi.OnClick := EventHandle_NormaliseSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Edit Sample Points...';
  mi.OnClick := EventHandle_EditSamplePoints;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Edit Sample Map...';
  mi.OnClick := EventHandle_EditSampleMap;
  Menu.Items.Add(mi);


  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Show in Windows Exporer...';
  mi.OnClick := EventHandle_ShowInWindowsExplorer;
  Menu.Items.Add(mi);


  Menu.Popup(x, y);
end;

procedure TSampleDisplayMenu.EventHandle_EditSampleMap(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  if Plugin.GuiState.IsSampleMapVisible
    then Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_HideSampleMapEdit)
    else Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_ShowSampleMapEdit);

end;

procedure TSampleDisplayMenu.EventHandle_EditSamplePoints(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.Command_ShowLoopEditFrame);
end;

procedure TSampleDisplayMenu.EventHandle_NormaliseSample(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.NormaliseSamples(Plugin);
end;

procedure TSampleDisplayMenu.EventHandle_ShowInWindowsExplorer(Sender: TObject);
var
  Region : IRegion;
  fn : string;
begin
  if not assigned(Plugin) then exit;

  Region := Plugin.FocusedRegion;

  if assigned(Region) then
  begin
    fn := Region.GetProperties^.SampleFileName;
    if FileExists(Fn) then
    begin
      OpenFolderAndSelectFile(Fn);
    end;

  end;
end;

end.
