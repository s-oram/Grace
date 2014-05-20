unit Menu.SampleDisplayMenu;

interface

uses
  Lucidity.Interfaces,
  eePlugin, Vcl.Menus;


type
  TSampleContextMenu = class
  private
    fLoopPointsVisible: boolean;
  protected
    Plugin : TeePlugin;
    Menu : TPopUpMenu;
    MouseDownSamplePos : integer;

    procedure EventHandle_NormaliseSample(Sender : TObject);
    procedure EventHandle_ZoomSample(Sender : TObject);
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
  uLucidityEnums,
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

constructor TSampleContextMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TSampleContextMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TSampleContextMenu.Initialize(aPlugin: TeePlugin);
begin
  Plugin := aPlugin;
end;

procedure TSampleContextMenu.Popup(const x, y: integer; const aMouseDownSamplePos : integer);
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
  mi.Caption := 'Zoom';
  mi.OnClick := EventHandle_ZoomSample;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Show in Windows Exporer...';
  mi.OnClick := EventHandle_ShowInWindowsExplorer;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Edit Sample Map...';
  mi.OnClick := EventHandle_EditSampleMap;
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

procedure TSampleContextMenu.EventHandle_EditSampleMap(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.ToggleSampleMapVisibility(Plugin);
end;

procedure TSampleContextMenu.EventHandle_ZoomSample(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.ToggleSampleZoom(Plugin);
end;

procedure TSampleContextMenu.EventHandle_NormaliseSample(Sender: TObject);
begin
  if not assigned(Plugin) then exit;
  Command.NormaliseSamples(Plugin);
end;

procedure TSampleContextMenu.EventHandle_ShowInWindowsExplorer(Sender: TObject);
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

procedure TSampleContextMenu.EventHandle_ModulationCommand(Sender: TObject);
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
    Command.ClearCurrentModulationForParameter_OLD(Plugin, ModParIndex);
  end;

  if Caption = Caption_ClearAllModulation then
  begin
    Command.ClearAllModulationForParameter_OLD(Plugin, ModParIndex);
  end;



end;



end.
