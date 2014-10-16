unit InWindowDialog.CustomDialog.Form;

interface

uses
  InWindowDialog.Prototypes,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, RedFoxWinControl,
  VamWinControl, VamPanel, RedFoxContainer, Vcl.StdCtrls, VamLabel, VamDiv;

type
  TStringEvent = procedure(Sender : TObject; Text : string) of object;

  TCustomDialogForm = class(TPluginDialogForm)
    RedFoxContainer1: TRedFoxContainer;
    BackPanel1: TVamPanel;
    BackPanel2: TVamPanel;
    ButtonDiv: TVamDiv;
    MainDialogArea: TVamDiv;
    DialogTextControl: TLabel;
    procedure ButtonDivResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fButtons : array of TButton;
    fDialogText: string;
    fColorBorder: TColor;
    fColorText: TColor;
    fColorBackground: TColor;
    fOnDialogResult: TStringEvent;
    procedure SetDialogText(const Value: string);
    procedure SetColorBackground(const Value: TColor);
    procedure SetColorBorder(const Value: TColor);
    procedure SetColorText(const Value: TColor);
    procedure EventHandle_ButtonClick(Sender: TObject);
  protected
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CMChildKey(var Message: TCMChildKey); message CM_CHILDKEY;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AddButtons(const Buttons : array of string);
    property DialogText : string read fDialogText write SetDialogText;

    property ColorBackground : TColor read fColorBackground write SetColorBackground;
    property ColorBorder     : TColor read fColorBorder     write SetColorBorder;
    property ColorText       : TColor read fColorText       write SetColorText;

    property OnDialogResult : TStringEvent read fOnDialogResult write fOnDialogResult;
  end;



implementation

uses
  RedFoxColor,
  VamLib.Graphics,
  VamLib.Utils;



{$R *.dfm}

{ TForm3 }

constructor TCustomDialogForm.Create(AOwner: TComponent);
begin
  inherited;

  SetLength(fButtons, 0);

  BackPanel1.Color := GetRedfoxColor(clWindowText);
  BackPanel2.Color := GetRedfoxColor(cl3DLight);
  DialogTextControl.Color := GetRedfoxColor(cl3DLight);

  // Important: add controls to the alternative tab order list.
  //TabOrderControlList.Add(OkButton);

  self.OnKeyDown := FormKeyDown;
end;

destructor TCustomDialogForm.Destroy;
var
  c1 : integer;
begin
  if Length(fButtons) > 0 then
  begin
    for c1 := 0 to Length(fButtons)-1 do
    begin
      TabOrderControlList.Remove(fButtons[c1]);
      fButtons[c1].Free;
      fButtons[c1] := nil;
    end;
  end;

  SetLength(fButtons, 0);

  inherited;
end;



procedure TCustomDialogForm.AddButtons(const Buttons: array of string);
var
  c1: Integer;
begin
  //==== Free the old buttons ====
  if Length(fButtons) > 0 then
  begin
    for c1 := 0 to Length(fButtons)-1 do
    begin
      TabOrderControlList.Remove(fButtons[c1]);
      fButtons[c1].Free;
      fButtons[c1] := nil;
    end;
  end;


  //==== Add the new buttons =====
  SetLength(fButtons, Length(Buttons));
  for c1 := 0 to Length(fButtons)-1 do
  begin
    fButtons[c1] := TButton.Create(self);
    fButtons[c1].Caption := Buttons[c1];
    fButtons[c1].Parent := ButtonDiv;
    fButtons[c1].Visible := true;
    fButtons[c1].OnClick := EventHandle_ButtonClick;

    TabOrderControlList.Add(fButtons[c1]);
  end;
end;

procedure TCustomDialogForm.ButtonDivResize(Sender: TObject);
const
  kButtonWidth = 97;
  kButtonSpace = 16;
var
  c1 : integer;
  TotalButtonWidth : integer;
  ButtonCount : integer;
  LeftOffset  : integer;
begin
  ButtonCount := Length(fButtons);

  if ButtonCount > 0 then
  begin
    TotalButtonWidth := (ButtonCount * kButtonWidth) + ((ButtonCount-1) * kButtonSpace);
    LeftOffset := (ButtonDiv.Width - TotalButtonWidth) div 2;

    for c1 := 0 to ButtonCount-1 do
    begin
       fButtons[c1].Width  := kButtonWidth;
       fButtons[c1].Height := ButtonDiv.Height;
       fButtons[c1].Top := 0;
       fButtons[c1].Left := LeftOffset + c1 * (kButtonWidth + kButtonSpace);
    end;
  end;
end;

procedure TCustomDialogForm.SetColorBackground(const Value: TColor);
begin
  fColorBackground := Value;
end;

procedure TCustomDialogForm.SetColorBorder(const Value: TColor);
begin
  fColorBorder := Value;
end;

procedure TCustomDialogForm.SetColorText(const Value: TColor);
begin
  fColorText := Value;
end;

procedure TCustomDialogForm.SetDialogText(const Value: string);
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

procedure TCustomDialogForm.EventHandle_ButtonClick(Sender: TObject);
var
  Text : string;
begin
  Text := (Sender as TButton).Caption;
  if assigned(OnDialogResult) then OnDialogResult(self, Text);
  CloseDialog;
end;

procedure TCustomDialogForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  c : TWinControl;
begin
  c := self.FindFocusedControl as TWinControl;

  // FormKeyDown is called when the dialog is used within a VST plugin.
  if (Key = VK_RETURN) then
  begin
    Key := 0;
    if (assigned(c)) and (c is TButton) then
    begin
      EventHandle_ButtonClick(c);
    end;
  end else
  if (Key = VK_ESCAPE) then
  begin
    Key := 0;
    CloseDialog;
  end else
  if Key = VK_TAB then
  begin
    Key := 0;
    FocusNextControl;
  end else
  if (Key = VK_RIGHT) and (assigned(c)) and (c is TButton) then
  begin
    Key := 0;
    FocusNextControl;
  end else
  if (Key = VK_LEFT) and (assigned(c)) and (c is TButton) then
  begin
    Key := 0;
    FocusPreviousControl;
  end;

end;

procedure TCustomDialogForm.FormShow(Sender: TObject);
begin
  self.SetFocus;
end;

procedure TCustomDialogForm.CMChildKey(var Message: TCMChildKey);
begin
  inherited;
end;





end.
