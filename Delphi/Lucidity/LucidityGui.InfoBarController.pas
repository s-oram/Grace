unit LucidityGui.InfoBarController;

interface

uses
  ExtCtrls, Classes, Controls, eePlugin;



type
  TTextEvent = procedure(Sender : TObject; Text:string) of object;

  TInfoBarController = class
  private
    fOnTextChanged: TTextEvent;
  protected
    UpdateInfoTimer : TTimer;
    Plugin : TeePlugin;
    LastControl : TControl;
    procedure SendText(const Text:string);
    procedure EventHandle_UpdateInfo(Sender : Tobject);
    procedure SendInfoForControl(c : TControl);

    function GetMenuButtonInfo(c : TControl):string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EventHandle_ControlMouseEnter(Sender : TObject);
    procedure EventHandle_ControlMouseLeave(Sender : TObject);

    procedure Initalize(aPlugin : TeePlugin; aGuiContainer:TControl);

    property OnTextChanged : TTextEvent read fOnTextChanged write fOnTextChanged;
  end;

implementation

uses
  uConstants,
  VamQuery, VamKnob, VamTextBox;

{ TLucidityInfoBarController }

constructor TInfoBarController.Create;
begin
  UpdateInfoTimer := TTimer.Create(nil);
  UpdateInfoTimer.Enabled := false;
  UpdateInfoTimer.Interval := 50;
  UpdateInfoTimer.OnTimer := self.EventHandle_UpdateInfo;
end;

destructor TInfoBarController.Destroy;
begin
  UpdateInfoTimer.Free;
  inherited;
end;


procedure TInfoBarController.Initalize(aPlugin: TeePlugin; aGuiContainer: TControl);
var
  VQ : IVamQuery;
  c : TControl;
begin
  Plugin := aPlugin;


  VQ := VamQueryRequest(aGuiContainer, TVamKnob);
  for c in VQ.List do
  begin
    (c as TVamKnob).OnMouseEnter := self.EventHandle_ControlMouseEnter;
    (c as TVamKnob).OnMouseLeave := self.EventHandle_ControlMouseLeave;
  end;

  //todo: property "DisplayClass" does not exist here.
  VQ := VamQueryRequest(aGuiContainer, 'MenuButton');
  for c in VQ.List do
  begin
    (c as TVamTextBox).OnMouseEnter := self.EventHandle_ControlMouseEnter;
    (c as TVamTextBox).OnMouseLeave := self.EventHandle_ControlMouseLeave;
  end;


  VQ := VamQueryRequest(aGuiContainer, dcGUIMenuButton);
  for c in VQ.List do
  begin
    (c as TVamTextBox).OnMouseEnter := self.EventHandle_ControlMouseEnter;
    (c as TVamTextBox).OnMouseLeave := self.EventHandle_ControlMouseLeave;
  end;






end;

procedure TInfoBarController.EventHandle_ControlMouseEnter(Sender: TObject);
var
  c : TControl;
  s : string;
  Tag : integer;
begin
  c := (Sender as TControl);
  LastControl := (Sender as TControl);
  SendInfoForControl(c);
  UpdateInfoTimer.Enabled := true;
end;

procedure TInfoBarController.EventHandle_ControlMouseLeave(Sender: TObject);
begin
  if (Sender = LastControl) then
  begin
    UpdateInfoTimer.Enabled := false;
    LastControl := nil;
    SendText('');
  end;
end;

procedure TInfoBarController.EventHandle_UpdateInfo(Sender: Tobject);
var
  c : TControl;
begin
  c := LastControl;
  if assigned(c) then
  begin
    SendInfoForControl(c);
  end else
  begin
    UpdateInfoTimer.Enabled := false;
  end;
end;




function TInfoBarController.GetMenuButtonInfo(c: TControl): string;
begin
  if c.Name = 'MainMenuButton'   then exit('Main Menu');
  if c.Name = 'SampleEditButton' then exit('Sample Editor...');
  if c.Name = 'MapEditButton'    then exit('Show/Hide Sample Map Editor');
  if c.Name = 'GroupMenuButton'  then exit('Key Group Menu');
  if c.Name = 'SampleMenuButton' then exit('Sample Menu');

  //== if we've made it this far there is no info for this button! ===
  result := '';

end;

procedure TInfoBarController.SendInfoForControl(c: TControl);
var
  Tag : integer;
  s   : string;
begin
  if (c is TVamKnob) then
  begin
    Tag := (c as TVamKnob).Tag;
    s := Plugin.Globals.VstParameters[Tag].ParInfo;
    SendText(s);
  end;

  if (c is TVamTextBox) and (HasDisplayClass(c, 'MenuButton')) then
  begin
    Tag := (c as TVamTextBox).Tag;
    s := Plugin.Globals.VstParameters[Tag].ParInfo;
    SendText(s);
  end;

  if (c is TVamTextBox) and (HasDisplayClass(c, dcGUIMenuButton)) then
  begin
    s := GetMenuButtonInfo(c);
    SendText(s);
  end;




end;

procedure TInfoBarController.SendText(const Text: string);
begin
  OnTextChanged(self, Text);
end;

end.
