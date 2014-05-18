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
  eeMidiAutomationV2,
  eeGuiStandardv2;

type
  TXYPadContextMenu = class(TCustomPopupMenu)
  private
    Menu : TPopUpMenu;
    fTargetParameterName: string;
    fTargetXYPadIndex: integer;
  protected
    procedure Handle_MidiLearn(Sender:TObject);
    procedure Handle_MidiUnlearn(Sender:TObject);
    procedure Handle_SetMidiCC(Sender:TObject);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const x, y : integer);

    property TargetXYPadIndex : integer read fTargetXYPadIndex write fTargetXYPadIndex; //range 0..3
  end;

implementation

uses
  SysUtils,
  Effect.MidiAutomation;

{ TXYPadContextMenu }

constructor TXYPadContextMenu.Create;
begin
  Menu := TPopUpMenu.Create(nil);
end;

destructor TXYPadContextMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TXYPadContextMenu.Handle_MidiLearn(Sender: TObject);
begin

end;

procedure TXYPadContextMenu.Handle_MidiUnlearn(Sender: TObject);
begin

end;

procedure TXYPadContextMenu.Handle_SetMidiCC(Sender: TObject);
begin

end;

procedure TXYPadContextMenu.Popup(const x, y: integer);
var
  mi : TMenuItem;
  miMidiLearn : TMenuItem;
  MidiBinding : IMidiBinding;
  MidiCC : integer;
  Text : string;
begin
  Menu.Items.Clear;


  // Rebuild the context menu before showing it.
  mi := TMenuItem.Create(Menu);
  mi.Caption := 'MIDI Learn';
  mi.OnClick := Handle_MidiLearn;
  Menu.Items.Add(mi);
  miMidiLearn := mi;

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'MIDI Unlearn';
  mi.OnClick := Handle_MidiUnlearn;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Caption := 'Set MIDI CC...';
  mi.OnClick := Handle_SetMidiCC;
  Menu.Items.Add(mi);


  //TODO:HIGH
  {
  MidiBinding := Plugin.MidiAutomation.FindBinding(TargetParameterName);

  if assigned(MidiBinding)
    then MidiCC := MidiBinding.GetMidiCC
    else MidiCC := -1;

  if MidiCC <> -1
    then Text := 'MIDI Learn  [CC: ' + IntToStr(MidiCC) + ']'
    else Text := 'MIDI Learn  [CC: --]';

  miMidiLearn.Caption := Text;
  }
end;

end.
