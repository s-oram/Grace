unit eeGlobals.InfoBarReceiver;

interface

uses
  Classes,
  Controls;

type
  TInfoBarReceiver = class
  private
    fOnMessageChanged: TNotifyEvent;
    fMessageText: string;

    FocusedControl : TObject;

  protected
    procedure MessageChanged;
  public
    constructor Create;
    destructor Destroy; override;

    procedure EnterControl(const Control : TObject);
    procedure LeaveControl(const Control : TObject);
    procedure SendControlMessage(const Control : TObject; const msg : string);

    property MessageText : string read fMessageText;

    property OnMessageChanged : TNotifyEvent read fOnMessageChanged write fOnMessageChanged;
  end;

implementation

{ TInfoBarReceiver }

constructor TInfoBarReceiver.Create;
begin
  fMessageText := '';
  FocusedControl := nil;
end;

destructor TInfoBarReceiver.Destroy;
begin

  inherited;
end;

procedure TInfoBarReceiver.MessageChanged;
begin
  if assigned(OnMessageChanged) then OnMessageChanged(Self);
end;

procedure TInfoBarReceiver.EnterControl(const Control: TObject);
begin
  FocusedControl := Control;
end;

procedure TInfoBarReceiver.LeaveControl(const Control: TObject);
begin
  if Control = FocusedControl then
  begin
    FocusedControl := nil;
    fMessageText := '';
    MessageChanged;
  end;
end;

procedure TInfoBarReceiver.SendControlMessage(const Control: TObject; const msg: string);
begin
  assert(Control <> nil);

  if (Control = FocusedControl) and (msg <> fMessageText) then
  begin
    fMessageText := msg;
    MessageChanged;
  end;
end;

end.
