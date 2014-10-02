unit InWindowDialog.MessageDialog.Form;

interface

uses
  InWindowDialog.Prototypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, Vcl.StdCtrls, VamLabel, VamDiv;

type
  TMessageDialogForm = class(TPluginDialogForm)
    RedFoxContainer1: TRedFoxContainer;
    BackPanel1: TVamPanel;
    BackPanel2: TVamPanel;
    ButtonDiv: TVamDiv;
    MainDialogArea: TVamDiv;
    OkButton: TButton;
    DialogTextControl: TLabel;
    procedure OkButtonClick(Sender: TObject);
    procedure ButtonDivResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    fDialogText: string;
    fOnOkayButton: TNotifyEvent;
    fColorBorder: TColor;
    fColorText: TColor;
    fColorBackground: TColor;
    procedure SetDialogText(const Value: string);
    procedure SetColorBackground(const Value: TColor);
    procedure SetColorBorder(const Value: TColor);
    procedure SetColorText(const Value: TColor);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property DialogText : string read fDialogText write SetDialogText;

    property ColorBackground : TColor read fColorBackground write SetColorBackground;
    property ColorBorder     : TColor read fColorBorder     write SetColorBorder;
    property ColorText       : TColor read fColorText       write SetColorText;

    property OnOkButton : TNotifyEvent read fOnOkayButton write fOnOkayButton;
  end;



implementation

uses
  RedFoxColor,
  VamLib.Graphics,
  VamLib.Utils;



{$R *.dfm}

{ TForm3 }

constructor TMessageDialogForm.Create(AOwner: TComponent);
begin
  inherited;

  BackPanel1.Color := GetRedfoxColor(clWindowText);
  //BackPanel1.Color := GetRedfoxColor(clRed);
  BackPanel2.Color := GetRedfoxColor(cl3DLight);
  DialogTextControl.Color := GetRedfoxColor(cl3DLight);

  // Important: add controls to the alternative tab order list.
  TabOrderControlList.Add(OkButton);
end;

destructor TMessageDialogForm.Destroy;
begin

  inherited;
end;



procedure TMessageDialogForm.ButtonDivResize(Sender: TObject);
begin
  OkButton.Left := (ButtonDiv.Width - OkButton.Width) div 2;
  OkButton.Top := 0;
  OkButton.Height := ButtonDiv.Height;
end;



procedure TMessageDialogForm.SetColorBackground(const Value: TColor);
begin
  fColorBackground := Value;
end;

procedure TMessageDialogForm.SetColorBorder(const Value: TColor);
begin
  fColorBorder := Value;
end;

procedure TMessageDialogForm.SetColorText(const Value: TColor);
begin
  fColorText := Value;
end;

procedure TMessageDialogForm.SetDialogText(const Value: string);
var
  AutoFormHeight : integer;
  AutoFormWidth  : integer;
  TextWidth, TextHeight : integer;
  TextRect : TRect;
begin
  fDialogText := Value;

  TextRect := CalcRequiredTextRect(DialogTextControl.Canvas.Handle, fDialogText, 700);
  TextWidth  := TextRect.Width;
  TextHeight := TextRect.Height;

  DialogTextControl.WordWrap := true;
  DialogTextControl.Caption := fDialogText;
  DialogTextControl.Align := alClient;

  AutoFormWidth  := TextWidth + 22;
  AutoFormHeight := TextHeight + 22 + 16 + ButtonDiv.Height; // + 200;

  AutoFormWidth  := Clamp(AutoFormWidth, 300, 700);
  AutoFormHeight := Clamp(AutoFormHeight, 10, 500);

  self.Width := AutoFormWidth;
  self.Height := AutoFormHeight;
end;

procedure TMessageDialogForm.OkButtonClick(Sender: TObject);
begin
  if assigned(OnOkButton) then OnOkButton(self);
  CloseDialog;
end;

procedure TMessageDialogForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // FormKeyDown is called when the dialog is used within a VST plugin.
  if (Key = VK_RETURN) or (Key = VK_ESCAPE) then
  begin
    OkButtonclick(self);
    Key := 0;
  end else
  if Key = VK_TAB then
  begin
    FocusNextControl;
    Key := 0;
  end;
end;



end.
