unit Menu.XYPadContextMenu;

interface

uses
  Vcl.Menus,
  Menu.CustomPopupMenu,
  VamLib.UniqueID,
  Contnrs,
  Controls,
  Classes,
  eePlugin,
  VamLib.ZeroObject,
  eeMidiAutomationV2;

type
  TXYPadContextMenu = class(TCustomPopupMenu)
  private
    fTargetXYPadIndex: integer;
    fPlugin: TeePlugin;
  protected
    procedure Handle_MidiLearn(Sender:TObject);
    procedure Handle_MidiUnlearn(Sender:TObject);
    procedure Handle_SetMidiCC(Sender:TObject);

    function GetPluginParameterName(const PadIndex, Axis : integer):string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);

    property TargetXYPadIndex : integer read fTargetXYPadIndex write fTargetXYPadIndex; //range 0..3

    property Plugin : TeePlugin read fPlugin write fPlugin;
  end;

implementation

uses
  Lucidity.GuiUtils,
  SysUtils,
  VamLib.Utils,
  Lucidity.PluginParameters,
  Effect.MidiAutomation;

{ TXYPadContextMenu }

constructor TXYPadContextMenu.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TXYPadContextMenu.Destroy;
begin

  inherited;
end;

function TXYPadContextMenu.GetPluginParameterName(const PadIndex, Axis: integer): string;
begin
  assert(InRange(PadIndex, 0, 3));
  assert(InRange(Axis, 0, 1));

  if Axis = 0 then
  begin
    if PadIndex = 0 then exit(PluginParToName(TPluginParameter.PadX1));
    if PadIndex = 1 then exit(PluginParToName(TPluginParameter.PadX2));
    if PadIndex = 2 then exit(PluginParToName(TPluginParameter.PadX3));
    if PadIndex = 3 then exit(PluginParToName(TPluginParameter.PadX4));
  end else
  begin
    if PadIndex = 0 then exit(PluginParToName(TPluginParameter.PadY1));
    if PadIndex = 1 then exit(PluginParToName(TPluginParameter.PadY2));
    if PadIndex = 2 then exit(PluginParToName(TPluginParameter.PadY3));
    if PadIndex = 3 then exit(PluginParToName(TPluginParameter.PadY4));
  end;


  // NOTE: We shouldn't make it this far, return this parameter but also raise an exception.
  result := PluginParToName(TPluginParameter.PadX1);
  raise Exception.Create('Something went wrong here!');
end;

procedure TXYPadContextMenu.Handle_MidiLearn(Sender: TObject);
var
  TargetBinding : TMidiBinding;
  TargetParameterName : string;
  Tag : integer;
begin
  Tag := (Sender as TMenuItem).Tag;
  TargetParameterName := GetPluginParameterName(TargetXYPadIndex, Tag);

  TargetBinding := TMidiBinding.Create;
  TargetBinding.ParName := TargetParameterName;
  TargetBinding.ParId   := PluginParNameToID(TargetParameterName);
  Plugin.MidiAutomation.ActivateMidiLearn(TargetBinding);
end;

procedure TXYPadContextMenu.Handle_MidiUnlearn(Sender: TObject);
var
  Tag : integer;
  TargetParameterName : string;
begin
  Tag := (Sender as TMenuItem).Tag;
  TargetParameterName := GetPluginParameterName(TargetXYPadIndex, Tag);
  Plugin.MidiAutomation.ClearBindingByName(TargetParametername);
end;

procedure TXYPadContextMenu.Handle_SetMidiCC(Sender: TObject);
var
  Tag : integer;
  TargetParameterName : string;
begin
  Tag := (Sender as TMenuItem).Tag;
  TargetParameterName := GetPluginParameterName(TargetXYPadIndex, Tag);
  Command.SetMidiCCForParameter(Plugin, TargetParameterName);
end;

procedure TXYPadContextMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
  MidiBinding : IMidiBinding;
  MidiCC_XAxis : integer;
  MidiCC_YAxis : integer;
  Text : string;
  TargetParNameX, TargetParNameY : string;
begin
  case TargetXYPadIndex of
    0:
    begin
      TargetParNameX := PluginParToName(TPluginParameter.PadX1);
      TargetParNameY := PluginParToName(TPluginParameter.PadY1);
    end;

    1:
    begin
      TargetParNameX := PluginParToName(TPluginParameter.PadX2);
      TargetParNameY := PluginParToName(TPluginParameter.PadY2);
    end;

    2:
    begin
      TargetParNameX := PluginParToName(TPluginParameter.PadX3);
      TargetParNameY := PluginParToName(TPluginParameter.PadY3);
    end;

    3:
    begin
      TargetParNameX := PluginParToName(TPluginParameter.PadX4);
      TargetParNameY := PluginParToName(TPluginParameter.PadY4);
    end;
  else
    raise Exception.Create('Index not handled.');
  end;

  //==== MIDI Learn X ======
  MidiBinding := Plugin.MidiAutomation.FindBinding(TargetParNameX);

  if assigned(MidiBinding)
    then MidiCC_XAxis := MidiBinding.GetMidiCC
    else MidiCC_XAxis := -1;

  //==== MIDI Learn Y ======
  MidiBinding := Plugin.MidiAutomation.FindBinding(TargetParNameY);

  if assigned(MidiBinding)
    then MidiCC_YAxis := MidiBinding.GetMidiCC
    else MidiCC_YAxis := -1;
  //=====================================


  Menu.Items.Clear;


  // build the context menu before showing it.
  mi := TMenuItem.Create(Menu);
  if MidiCC_XAxis <> -1
    then Text := 'MIDI Learn X Axis [CC: ' + IntToStr(MidiCC_XAxis) + ']'
    else Text := 'MIDI Learn X Axis';
  mi.Caption := Text;
  mi.OnClick := Handle_MidiLearn;
  mi.Tag := 0;
  Menu.Items.Add(mi);

  if MidiCC_XAxis <> -1 then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'MIDI Unlearn X Axis';
    mi.OnClick := Handle_MidiUnlearn;
    mi.Tag := 0;
    Menu.Items.Add(mi);
  end;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Set MIDI CC X Axis...';
  mi.OnClick := Handle_SetMidiCC;
  mi.Tag := 0;
  Menu.Items.Add(mi);


  //=== Spacer =========
  mi := TMenuItem.Create(Menu);
  mi.Caption := '-';
  Menu.Items.Add(mi);
  //=====================


  mi := TMenuItem.Create(Menu);
  if MidiCC_YAxis <> -1
    then Text := 'MIDI Learn Y Axis [CC: ' + IntToStr(MidiCC_YAxis) + ']'
    else Text := 'MIDI Learn Y Axis';
  mi.Caption := Text;
  mi.OnClick := Handle_MidiLearn;
  mi.Tag := 1;
  Menu.Items.Add(mi);

  if MidiCC_YAxis <> -1 then
  begin
    mi := TMenuItem.Create(Menu);
    mi.Caption := 'MIDI Unlearn Y Axis';
    mi.OnClick := Handle_MidiUnlearn;
    mi.Tag := 1;
    Menu.Items.Add(mi);
  end;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Set MIDI CC Y Axis...';
  mi.OnClick := Handle_SetMidiCC;
  mi.Tag := 1;
  Menu.Items.Add(mi);



  Menu.Popup(X, Y);

end;

end.
