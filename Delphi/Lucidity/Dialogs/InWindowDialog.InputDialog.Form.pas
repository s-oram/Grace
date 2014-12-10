unit InWindowDialog.InputDialog.Form;

interface

uses
  InWindowDialog.Prototypes, UITypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, VamLabel, VamDiv,
  RedFoxWinControl, VamWinControl, VamPanel, RedFoxContainer;

type
  TStringEvent = procedure(Sender : TObject; Text : string) of object;

  TInputDialogForm = class(TPluginDialogForm)
    RedFoxContainer1: TRedFoxContainer;
    BackPanel1: TVamPanel;
    BackPanel2: TVamPanel;
    ButtonDiv: TVamDiv;
    OkButton: TButton;
    MainDialogArea: TVamDiv;
    DialogTextControl: TLabel;
    InputAreaDiv: TVamDiv;
    InputEditControl: TEdit;
    CancelButton: TButton;
    InputLabelControl: TLabel;
    procedure ButtonDivResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);

  private
    fInputLabel: string;
    fDefaultValue: string;
    fInputText: string;
    fOnDialogResult: TStringEvent;
    procedure SetDefaultValue(const Value: string);
    procedure SetInputLabel(const Value: string);
    procedure SetInputText(const Value: string);
  protected
    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property InputText    : string read fInputText    write SetInputText;
    property InputLabel   : string read fInputLabel   write SetInputLabel;
    property DefaultValue : string read fDefaultValue write SetDefaultValue;

    property OnDialogResult : TStringEvent read fOnDialogResult write fOnDialogResult;
  end;



implementation

uses
  {$IFDEF DEBUG}Dialogs,{$ENDIF DEBUG}
  RedFoxColor,
  VamLib.Graphics,
  VamLib.Utils;

{$R *.dfm}

{ TInputDialogForm }

constructor TInputDialogForm.Create(AOwner: TComponent);
begin
  inherited;

  CancelButton.Width := OkButton.Width;

  OkButton.Height     := ButtonDiv.Height;
  CancelButton.Height := ButtonDiv.Height;

  OkButton.Top := 0;
  CancelButton.Top := 0;

  InputAreaDiv.Height := 20;
  InputEditControl.Align := alClient;



  BackPanel1.Color := GetRedfoxColor(clBlack);
  //BackPanel1.Color := GetRedfoxColor(clRed);
  BackPanel2.Color := GetRedfoxColor(cl3DLight);
  InputLabelControl.Color := cl3DLight;
  DialogTextControl.Color := cl3DLight;


  InputEditControl.TabOrder := 0;
  OkButton.TabOrder         := 1;
  CancelButton.TabOrder     := 2;


  TabOrderControlList.Add(InputEditControl);
  TabOrderControlList.Add(OkButton);
  TabOrderControlList.Add(CancelButton);



  OnKeyDown := FormKeyDown;
end;

destructor TInputDialogForm.Destroy;
begin

  inherited;
end;



procedure TInputDialogForm.FormDeactivate(Sender: TObject);
begin
  //
end;



procedure TInputDialogForm.ButtonDivResize(Sender: TObject);
var
  totalWidth : integer;
begin
  TotalWidth := OkButton.Width + CancelButton.Width + 8;
  OkButton.Left     := ButtonDiv.Width - TotalWidth;
  CancelButton.Left := ButtonDiv.Width - CancelButton.Width;
end;

procedure TInputDialogForm.SetDefaultValue(const Value: string);
begin
  fDefaultValue := Value;
  InputEditControl.Text := Value;
end;

procedure TInputDialogForm.SetInputLabel(const Value: string);
begin
  fInputLabel := Value;
  InputLabelControl.Caption := Value;
end;

procedure TInputDialogForm.SetInputText(const Value: string);
var
  AutoFormHeight : integer;
  AutoFormWidth  : integer;
  TextWidth, TextHeight : integer;
  TextRect : TRect;
begin
  fInputText := Value;

  DialogTextControl.Caption := Value;

  TextRect := CalcRequiredTextRect(DialogTextControl.Canvas.Handle, fInputText, 700);
  TextWidth  := TextRect.Width;
  TextHeight := TextRect.Height;

  DialogTextControl.WordWrap := true;
  DialogTextControl.Caption := fInputText;
  DialogTextControl.Align := alClient;

  AutoFormWidth  := TextWidth  + 22;
  AutoFormHeight := TextHeight + 22 + 24 + InputAreaDiv.Height + ButtonDiv.Height; // + 200;

  AutoFormWidth  := Clamp(AutoFormWidth, 300, 700);
  AutoFormHeight := Clamp(AutoFormHeight, 10, 500);

  self.Width := AutoFormWidth;
  self.Height := AutoFormHeight;
end;

procedure TInputDialogForm.OkButtonClick(Sender: TObject);
begin
  if assigned(OnDialogResult) then OnDialogResult(self, InputEditControl.Text);
  CloseDialog;
end;

procedure TInputDialogForm.CancelButtonClick(Sender: TObject);
begin
  CloseDialog;
end;


procedure TInputDialogForm.CMChildKey(var Message: TCMChildKey);
begin
  // NOTE: CMChildKey isn't called when dialog is used in a plugin.
  // It does work in standalone mode.
  if Message.CharCode = VK_RETURN then
  begin
    OkButtonclick(self);
    Message.Result := 1;
  end else
  if Message.CharCode = VK_ESCAPE then
  begin
    CancelButtonClick(self);
    Message.Result := 1;
  end else
  begin
    inherited;
  end;
end;

procedure TInputDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  // FormKeyDown is called when the dialog is used within a VST plugin.
  if Key = VK_RETURN then
  begin
    OkButtonclick(self);
    Key := 0;
  end else
  if Key = VK_ESCAPE then
  begin
    CancelButtonClick(self);
    Key := 0;
  end else
  if Key = VK_TAB then
  begin
    FocusNextControl;
    Key := 0;
  end;
end;

end.
