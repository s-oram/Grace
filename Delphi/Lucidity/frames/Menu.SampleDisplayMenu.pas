unit Menu.SampleDisplayMenu;

interface

uses
  Lucidity.Interfaces,
  eePlugin, Vcl.Menus;


type
  TSampleDisplayMenu = class
  private
    fLoopPointsVisible: boolean;
  protected
    Plugin : TeePlugin;
    Menu : TPopUpMenu;
    MouseDownSamplePos : integer;

    procedure EventHandle_NormaliseSample(Sender : TObject);
    procedure EventHandle_EditSamplePoints(Sender : TObject);
    procedure EventHandle_EditSampleMap(Sender : TObject);
    procedure EventHandle_ShowInWindowsExplorer(Sender : TObject);
    procedure EventHandle_ModulationCommand(Sender : TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize(aPlugin : TeePlugin);

    procedure Popup(const x, y : integer; const aMouseDownSamplePos : integer);

    property LoopPointsVisible : boolean read fLoopPointsVisible write fLoopPointsVisible;

  end;

implementation

uses
  Lucidity.PluginParameters,
  Lucidity.Types,
  uGuiUtils,
  SysUtils,
  eeWinEx,
  Lucidity.SampleMap,
  uConstants;

const
  SampleStartTag = 1;
  SampleEndTag   = 2;
  LoopStartTag   = 3;
  LoopEndTag     = 4;

  Caption_MoveHere               = 'Move Here';
  Caption_ClearAllModulation     = 'Clear All Modulation';
  Caption_ClearCurrentModulation = 'Clear Current Modulation';

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

procedure TSampleDisplayMenu.Popup(const x, y: integer; const aMouseDownSamplePos : integer);
var
  mi : TMenuItem;
  Childmi : TMenuItem;
  Tag : integer;
begin
  Menu.Items.Clear;


  MouseDownSamplePos := aMouseDownSamplePos;


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

  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);



  Tag := SampleStartTag;
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Sample Start';
  Menu.Items.Add(mi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_MoveHere;
    ChildMi.Hint    := Caption_MoveHere;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_ClearCurrentModulation;
    ChildMi.Hint    := Caption_ClearCurrentModulation;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_ClearAllModulation;
    ChildMi.Hint    := Caption_ClearAllModulation;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);


  Tag := SampleEndTag;
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Sample End';
  Menu.Items.Add(mi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_MoveHere;
    ChildMi.Hint    := Caption_MoveHere;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_ClearCurrentModulation;
    ChildMi.Hint    := Caption_ClearCurrentModulation;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);

    Childmi := TMenuItem.Create(Menu);
    ChildMi.Caption := Caption_ClearAllModulation;
    ChildMi.Hint    := Caption_ClearAllModulation;
    ChildMi.OnClick := EventHandle_ModulationCommand;
    ChildMi.Tag := Tag;
    mi.Add(ChildMi);


  if LoopPointsVisible then
  begin

    Tag := LoopStartTag;
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Loop Start';
    Menu.Items.Add(mi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_MoveHere;
      ChildMi.Hint    := Caption_MoveHere;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_ClearCurrentModulation;
      ChildMi.Hint    := Caption_ClearCurrentModulation;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_ClearAllModulation;
      ChildMi.Hint    := Caption_ClearAllModulation;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);


    Tag := LoopEndTag;
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'Loop End';
    Menu.Items.Add(mi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_MoveHere;
      ChildMi.Hint    := Caption_MoveHere;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_ClearCurrentModulation;
      ChildMi.Hint    := Caption_ClearCurrentModulation;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);

      Childmi := TMenuItem.Create(Menu);
      ChildMi.Caption := Caption_ClearAllModulation;
      ChildMi.Hint    := Caption_ClearAllModulation;
      ChildMi.OnClick := EventHandle_ModulationCommand;
      ChildMi.Tag := Tag;
      mi.Add(ChildMi);
  end;

  Menu.Popup(x, y);
end;

procedure TSampleDisplayMenu.EventHandle_EditSampleMap(Sender: TObject);
begin
  if not assigned(Plugin) then exit;

  if Plugin.Globals.GuiState.IsSampleMapVisible
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

procedure TSampleDisplayMenu.EventHandle_ModulationCommand(Sender: TObject);
var
  Tag : integer;
  ModParIndex : integer;
  Caption : string;
  SampleMarker : TSampleMarker;
begin
  Tag := (Sender as TMenuItem).Tag;

  case Tag of
    SampleStartTag : ModParIndex := GetModParIndex(TPluginParameter.SampleStart);
    SampleEndTag   : ModParIndex := GetModParIndex(TPluginParameter.SampleEnd);
    LoopStartTag   : ModParIndex := GetModParIndex(TPluginParameter.LoopStart);
    LoopEndTag     : ModParIndex := GetModParIndex(TPluginParameter.LoopEnd);
  else
    raise Exception.Create('Type not handled.');
  end;

  case Tag of
    SampleStartTag : SampleMarker := smSampleStartMarker;
    SampleEndTag   : SampleMarker := smSampleEndMarker;
    LoopStartTag   : SampleMarker := smLoopStartMarker;
    LoopEndTag     : SampleMarker := smLoopEndMarker;
  else
    raise Exception.Create('Type not handled.');
  end;

  Caption := (Sender as TMenuItem).Hint;

  if Caption = Caption_MoveHere then
  begin
    Command.MoveSampleMarker(Plugin, SampleMarker, MouseDownSamplePos);
  end;

  if Caption = Caption_ClearCurrentModulation then
  begin
    Command.ClearCurrentModulationForParameter(Plugin, ModParIndex);
  end;

  if Caption = Caption_ClearAllModulation then
  begin
    Command.ClearAllModulationForParameter(Plugin, ModParIndex);
  end;



end;



end.
