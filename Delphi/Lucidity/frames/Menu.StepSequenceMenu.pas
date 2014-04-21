unit Menu.StepSequenceMenu;

interface

uses
  VamGuiControlInterfaces,
  Menu.CustomPopupMenu, eePlugin, Vcl.Menus;

type
  TStepSequenceMenu = class(TCustomPopupMenu)
  private
  protected
    Menu : TPopUpMenu;
    SequenceIndex : integer;

    procedure EventHandle_ResetSteps(Sender : TObject);
    procedure EventHandle_RandomizeSteps(Sender : TObject);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Popup(const x, y : integer; const aSequenceIndex : integer);
  end;

implementation

uses
  uConstants,
  Lucidity.Interfaces,
  Lucidity.KeyGroup,
  uAboutDialog,
  SysUtils,
  uAutoFree,
  uGuiUtils,
  Dialogs;



{ TStepSequenceMenu }

constructor TStepSequenceMenu.Create;
begin
  Menu := TPopupMenu.Create(nil);
end;

destructor TStepSequenceMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

procedure TStepSequenceMenu.Popup(const x, y: integer; const aSequenceIndex : integer);
var
  mi : TMenuItem;
begin
  assert(aSequenceIndex >= 0);
  assert(aSequenceIndex <= 1);

  SequenceIndex := aSequenceIndex;

  Menu.Items.Clear;

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 1;
  mi.Caption := 'Reset';
  mi.OnClick := EventHandle_ResetSteps;
  Menu.Items.Add(mi);

  mi := TMenuItem.Create(Menu);
  mi.Tag     := 1;
  mi.Caption := 'Randomize';
  mi.OnClick := EventHandle_RandomizeSteps;
  Menu.Items.Add(mi);


  Menu.Popup(x, y);
end;

procedure TStepSequenceMenu.EventHandle_RandomizeSteps(Sender: TObject);
var
  c1 : integer;
  SG : IKeyGroup;
  x1 : single;
  SeqData : IStepSequenceDataObject;
begin
  if not assigned(Plugin) then exit;

  SG := Plugin.FocusedKeyGroup;

  if not assigned(sg) then exit;

  //== Step Seq 1 ==
  if SequenceIndex = 0 then
  begin
    SeqData := SG.GetSequenceData(0);

    for c1 := 0 to kMaxStepSequencerLength-1 do
    begin
      x1 := Random * 2 - 1;
      SeqData.SetStepValue(c1, x1);
    end;
  end;


  //== Step Seq 2 ==
  if SequenceIndex = 1 then
  begin
    SeqData := SG.GetSequenceData(1);

    for c1 := 0 to kMaxStepSequencerLength-1 do
    begin
      x1 := Random * 2 - 1;
      SeqData.SetStepValue(c1, x1);
    end;
  end;

  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.RefreshRequest_StepSeqDisplay, nil, nil);
end;

procedure TStepSequenceMenu.EventHandle_ResetSteps(Sender: TObject);
var
  c1 : integer;
  SG : IKeyGroup;
  x1 : single;
  SeqData : IStepSequenceDataObject;
begin
  if not assigned(Plugin) then exit;

  SG := Plugin.FocusedKeyGroup;

  if not assigned(sg) then exit;

  //== Step Seq 1 ==
  if SequenceIndex = 0 then
  begin
    SeqData := SG.GetSequenceData(0);

    for c1 := 0 to kMaxStepSequencerLength-1 do
    begin
      x1 := 0;
      SeqData.SetStepValue(c1, x1);
    end;
  end;


  //== Step Seq 2 ==
  if SequenceIndex = 1 then
  begin
    SeqData := SG.GetSequenceData(1);

    for c1 := 0 to kMaxStepSequencerLength-1 do
    begin
      x1 := 0;
      SeqData.SetStepValue(c1, x1);
    end;
  end;

  Plugin.Globals.MotherShip.SendMessageUsingGuiThread(TLucidMsgID.RefreshRequest_StepSeqDisplay, nil, nil);
end;



end.

