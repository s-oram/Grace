unit ee3AddOn.ControlContextMenu.VstPar;

interface

uses
  Menus,
  ee3AddOn.MidiAutomation.VstPar;

type
  TVstParContextMenuHandler = class
  private
  protected
    MidiAutomation:TMidiAutomationController;
    VstParIndex : integer;
    VstParName  : string;
    procedure EventHandle_MidiLearn(Sender : TObject);
    procedure EventHandle_MidiUnLearn(Sender : TObject);
    procedure EventHandle_SetMidiCC(Sender : TObject);
  public
    constructor Create(aMidiAutomation:TMidiAutomationController);
    destructor Destroy; override;
    procedure AddMenuItems(const aMenu : TPopupMenu; const aVstParIndex : integer; const aVstParName : string);
  end;


implementation

{ TVstParContextMenuHandler }

constructor TVstParContextMenuHandler.Create(aMidiAutomation:TMidiAutomationController);
begin
  MidiAutomation := aMidiAutomation;
end;

destructor TVstParContextMenuHandler.Destroy;
begin

  inherited;
end;

procedure TVstParContextMenuHandler.AddMenuItems(const aMenu : TPopupMenu; const aVstParIndex : integer; const aVstParName : string);
var
  mi : TMenuItem;
begin
  VstParIndex := aVstParIndex;
  VstParName  := aVstParName;

  mi := TMenuItem.Create(aMenu);
  mi.Caption := 'MIDI Learn';
  mi.OnClick := EventHandle_MidiLearn;
  aMenu.Items.Add(mi);

  mi := TMenuItem.Create(aMenu);
  mi.Caption := 'MIDI Unlearn';
  mi.OnClick := EventHandle_MidiUnlearn;
  aMenu.Items.Add(mi);

  mi := TMenuItem.Create(aMenu);
  mi.Caption := 'Set MIDI CC';
  mi.OnClick := EventHandle_SetMidiCC;
  aMenu.Items.Add(mi);
end;

procedure TVstParContextMenuHandler.EventHandle_MidiLearn(Sender: TObject);
begin
  MidiAutomation.ActivateMidiLearnForVstParameter(VstParIndex, VstParName);
end;

procedure TVstParContextMenuHandler.EventHandle_MidiUnLearn(Sender: TObject);
begin
  MidiAutomation.DeleteBindingByVstPar(VstParIndex);
end;

procedure TVstParContextMenuHandler.EventHandle_SetMidiCC(Sender: TObject);
begin

end;



end.
