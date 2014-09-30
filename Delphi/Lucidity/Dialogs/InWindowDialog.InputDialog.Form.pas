unit InWindowDialog.InputDialog.Form;

interface

uses
  InWindowDialog.Prototypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VamLabel, VamDiv,
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
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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


end;

destructor TInputDialogForm.Destroy;
begin

  inherited;
end;



procedure TInputDialogForm.FormDeactivate(Sender: TObject);
begin
  //
end;

procedure TInputDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  //
end;

procedure TInputDialogForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  //
end;

procedure TInputDialogForm.ButtonDivResize(Sender: TObject);
var
  totalWidth : integer;
  LeftRef    : integer;
begin
  TotalWidth := OkButton.Width + CancelButton.Width + 8;
  LeftRef := (ButtonDiv.Width - TotalWidth) div 2;

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
  inherited;
end;



end.
