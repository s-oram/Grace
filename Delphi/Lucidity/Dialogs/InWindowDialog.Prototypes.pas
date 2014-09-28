unit InWindowDialog.Prototypes;

interface

uses
  Classes,
  Vcl.Forms,
  InWindowDialog.ModalShadow.Form;

type
  IPluginDialog = interface(IInterface)
  end;

  TluginDialog = class;
  TPluginDialogForm = class;

  TluginDialog = class(TInterfacedObject, IPluginDialog)
  private
    fDialogTop: integer;
    fDialogHeight: integer;
    fDialogLeft: integer;
    fDialogWidth: integer;
    fUseCustomSize: boolean;
    fUseCustomPosition: boolean;
  protected
    function CreateDialogForm(AOwner: TComponent) : TPluginDialogForm; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure ShowInWindow_WithAutoFree(const TopLevelForm : TForm; const ShowModalShadow : boolean; const AllowModalCancel:boolean);

    property UseCustomSize     : boolean read fUseCustomSize     write fUseCustomSize;
    property UseCustomPosition : boolean read fUseCustomPosition write fUseCustomPosition;

    property DialogLeft   : integer read fDialogLeft   write fDialogLeft;
    property DialogTop    : integer read fDialogTop    write fDialogTop;
    property DialogWidth  : integer read fDialogWidth  write fDialogWidth;
    property DialogHeight : integer read fDialogHeight write fDialogHeight;
  end;

  TPluginDialogForm = class(TForm)
  private
    DialogDataReference : IPluginDialog;
    ModalShadow : TModalShadow;
  protected
    procedure DoClose(var Action: TCloseAction); override; final;

    procedure EventHandle_ModalShadowClicked(Sender : TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Closes the main dialog form and the modal shadow.
    procedure CloseDialog;
  end;

implementation

uses
  VamLib.LoggingProxy,
  WinApi.Windows,
  InWindowDialog.ModalShadow;

{ TCustomPluginDialogData }

constructor TluginDialog.Create;
begin
  fUseCustomSize     := false;
  fUseCustomPosition := false;
  fDialogTop     := 0;
  fDialogHeight  := 0;
  fDialogLeft    := 0;
  fDialogWidth   := 0;
end;

destructor TluginDialog.Destroy;
begin
  Log.LogMessage('TCustomPluginDialogData.Destroy');

  inherited;
end;

procedure TluginDialog.ShowInWindow_WithAutoFree(const TopLevelForm: TForm; const ShowModalShadow : boolean; const AllowModalCancel:boolean);
var
  Region : HRGN;
  ModalShadow : TModalShadow;
  DialogForm : TPluginDialogForm;
  xLeft, xTop, xWidth, xHeight : integer; //TODO:MED rename these to dxLeft etc.
begin
  assert(assigned(TopLevelForm));

  //== Create the main dialog form ==
  DialogForm := CreateDialogForm(TopLevelForm);
  DialogForm.DialogDataReference := self;

  if UseCustomSize then
  begin
    // use custom dialog size.
    xWidth  := DialogWidth;
    xHeight := DialogHeight;
  end else
  begin
    // Use dialog size as defined.
    xWidth  := DialogForm.Width;
    xHeight := DialogForm.Height;
  end;

  if UseCustomPosition then
  begin
    // use custom dialog positon.
    xLeft := DialogLeft;
    xTop  := DialogTop;
  end else
  begin
    // Default to roughly middle of the top level form.
    xLeft := (TopLevelForm.Width  - xWidth)  div 2;
    xTop  := (TopLevelForm.Height - DialogForm.Height) div 5 * 2;
  end;

  //== create and setup the modal shadow ==
  if ShowModalShadow then
  begin
    ModalShadow := TModalShadow.CreateShadow(TopLevelForm);

    DialogForm.ModalShadow := ModalShadow;

    ModalShadow.Top  := 0;
    ModalShadow.Left := 0;
    ModalShadow.Width  := TopLevelForm.ClientWidth;
    ModalShadow.Height := TopLevelForm.ClientHeight;

    if AllowModalCancel
      then ModalShadow.OnShadowClicked := DialogForm.EventHandle_ModalShadowClicked;

    ModalShadow.ShowShadow;
  end;

  //== setup the main dialog form ==
  DialogForm.BorderStyle := TFormBorderStyle.bsNone;
  DialogForm.Position := TPosition.poDesigned;
  DialogForm.Parent := TopLevelForm;
  DialogForm.Left   := xLeft;
  DialogForm.Top    := xTop;
  DialogForm.Width  := xWidth;
  DialogForm.Height := xHeight;
  DialogForm.FormStyle := TFormStyle.fsStayOnTop;
  Region := CreateRoundRectRgn(0, 0, xWidth, xHeight, 6, 6);
  SetWindowRgn(DialogForm.Handle, Region, true);
  DialogForm.Visible := true;
  DialogForm.BringToFront;

end;

{ TCustomPluginDialogForm }

constructor TPluginDialogForm.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TPluginDialogForm.Destroy;
begin
  DialogDataReference := nil;

  inherited;
end;

procedure TPluginDialogForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action := TCloseAction.caFree;
end;

procedure TPluginDialogForm.EventHandle_ModalShadowClicked(Sender: TObject);
begin
  CloseDialog;
end;

procedure TPluginDialogForm.CloseDialog;
begin
  self.Close;
  if assigned(ModalShadow) then ModalShadow.Close;
end;





end.
