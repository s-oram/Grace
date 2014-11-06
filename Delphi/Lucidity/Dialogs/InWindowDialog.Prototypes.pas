unit InWindowDialog.Prototypes;

interface

uses
  Classes, Messages, Controls,
  Vcl.Forms, Contnrs,
  InWindowDialog.ModalShadow.Form;

type
  IPluginDialog = interface(IInterface)
  end;

  TPluginDialog = class;
  TPluginDialogForm = class;

  TPluginDialog = class(TInterfacedObject, IPluginDialog)
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

    // This method sets up the auto free mechansim for the TPluginDialog object.
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
    // Delphi's existing TabOrder mechanism doesn't work with in-window dialogs.
    // The focus is eventually passed to a control on the parent form/frame/panel.
    // TPluginDialogForm implements a custom tab order mechanism. Add controls
    // in the desired tab order to the TabOrderControlList to use.
    TabOrderControlList : TObjectList;

    procedure DoClose(var Action: TCloseAction); override; final;

    procedure EventHandle_ModalShadowClicked(Sender : TObject);

    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;

    procedure Resizing(State: TWindowState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AfterConstruction; override;

    procedure FocusFirstControl;
    procedure FocusPreviousControl;
    procedure FocusNextControl; // Typically called when tabbing.
    function FindFocusedControl:TObject;

    // Closes the main dialog form and the modal shadow.
    procedure CloseDialog;
  end;

implementation

uses
  Dialogs,
  SysUtils,
  VamLib.LoggingProxy,
  WinApi.Windows;

{ TCustomPluginDialogData }

constructor TPluginDialog.Create;
begin
  fUseCustomSize     := false;
  fUseCustomPosition := false;
  fDialogTop     := 0;
  fDialogHeight  := 0;
  fDialogLeft    := 0;
  fDialogWidth   := 0;
end;

destructor TPluginDialog.Destroy;
begin
  Log.LogMessage('TPluginDialog.Destroy');

  inherited;
end;

procedure TPluginDialog.ShowInWindow_WithAutoFree(const TopLevelForm: TForm; const ShowModalShadow : boolean; const AllowModalCancel:boolean);
// NOTE: This method sets up the mechansism to automatically free the TPluginDialog object when it is no longer
// required. This absolves the calling code of it's normal responsibility of freeing the TPluginDialog object.
// This mechanism works by using Interface reference counting.
// NOTE: I think this technique could be better signafied in the code if this method
// was a standalone method. However I like it being a member of the TPluginDialog object
// because it also shows the tight coupling between the ShowInWindow() method and the TPluginDialog object. hmmm.....
var
  Region : HRGN;
  ModalShadow : TModalShadow;
  DialogForm : TPluginDialogForm;
  xLeft, xTop, xWidth, xHeight : integer;
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

  //== setup the main dialog form ==
  DialogForm.BorderStyle := TFormBorderStyle.bsNone;
  DialogForm.Position := TPosition.poDesigned;
  DialogForm.Parent := TopLevelForm;
  DialogForm.Left   := xLeft;
  DialogForm.Top    := xTop;
  DialogForm.Width  := xWidth;
  DialogForm.Height := xHeight;
  DialogForm.FormStyle := TFormStyle.fsStayOnTop;
  Region := CreateRoundRectRgn(0, 0, xWidth+1, xHeight+1, 4, 4);
  SetWindowRgn(DialogForm.Handle, Region, true);

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

  //== show the dialog form ==
  DialogForm.Visible := true;
  DialogForm.BringToFront;
  DialogForm.FocusFirstControl;

end;

{ TCustomPluginDialogForm }

constructor TPluginDialogForm.Create(AOwner: TComponent);
begin
  inherited;

  KeyPreview := true;
  DoubleBuffered := true;

  TabOrderControlList := TObjectList.Create;
  TabOrderControlList.OwnsObjects := false;
end;

destructor TPluginDialogForm.Destroy;
begin
  DialogDataReference := nil;
  TabOrderControlList.Free;
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

function TPluginDialogForm.FindFocusedControl: TObject;
var
  c1: Integer;
  c : TWinControl;
begin
  for c1 := 0 to TabOrderControlList.Count-1 do
  begin
    c := TabOrderControlList[c1] as TWinControl;
    if c.Focused then
    begin
      exit(c); //=======>> exit >>======>>
    end;
  end;

  // if we make it this far no control has focus.
  result := nil;
end;

procedure TPluginDialogForm.FocusFirstControl;
var
  c : TWinControl;
begin
  if TabOrderControlList.Count > 0 then
  begin
    c := TabOrderControlList[0] as TWinControl;
    c.SetFocus;
  end;
end;

procedure TPluginDialogForm.FocusPreviousControl;
var
  c : TWinControl;
  c1 : integer;
  FocusIndex : integer;
begin
  if TabOrderControlList.Count = 0 then exit;

  FocusIndex := -1;

  for c1 := 0 to TabOrderControlList.Count-1 do
  begin
    c := TabOrderControlList[c1] as TWinControl;
    if c.Focused then
    begin
      FocusIndex := c1;
      break; //===BREAK====>>
    end;
  end;

  if FocusIndex = -1 then
  begin
    c := TabOrderControlList[0] as TWinControl;
    c.SetFocus;
  end else
  if FocusIndex >= 0 then
  begin
    dec(FocusIndex);
    if FocusIndex < 0 then FocusIndex := TabOrderControlList.Count-1;
    c := TabOrderControlList[FocusIndex] as TWinControl;
    c.SetFocus;
  end;
end;

procedure TPluginDialogForm.Resizing(State: TWindowState);
begin
  inherited;
end;

procedure TPluginDialogForm.FocusNextControl;
var
  c : TWinControl;
  c1 : integer;
  FocusIndex : integer;
begin
  if TabOrderControlList.Count = 0 then exit;

  FocusIndex := -1;

  for c1 := 0 to TabOrderControlList.Count-1 do
  begin
    c := TabOrderControlList[c1] as TWinControl;
    if c.Focused then
    begin
      FocusIndex := c1;
      break; //===BREAK====>>
    end;
  end;

  if FocusIndex = -1 then
  begin
    c := TabOrderControlList[0] as TWinControl;
    c.SetFocus;
  end else
  if FocusIndex >= 0 then
  begin
    inc(FocusIndex);
    if FocusIndex >= TabOrderControlList.Count then FocusIndex := 0;
    c := TabOrderControlList[FocusIndex] as TWinControl;
    c.SetFocus;
  end;
end;

procedure TPluginDialogForm.CMChildKey(var Message: TCMChildKey);
begin
  // Interesting artical for key processing.
  // http://edn.embarcadero.com/article/38447
  if Message.CharCode = VK_TAB then
  begin
    FocusNextControl;
    Message.Result := 1;
  end else
  begin
    inherited;
  end;
end;


procedure TPluginDialogForm.AfterConstruction;
begin
  inherited;

  if (self.Scaled) then
  begin
    raise Exception.Create('Dialog form is scaled. (' + self.ClassName + ')' );
  end;

end;

procedure TPluginDialogForm.CloseDialog;
begin
  self.Close;
  if assigned(ModalShadow) then ModalShadow.Close;
end;








end.
