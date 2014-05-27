unit uMainForm;

interface

uses
  Lucidity.CopyProtection, Lucidity.KeyGenMaster,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TMainForm = class(TForm)
    EmailInput: TEdit;
    NameInput: TEdit;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    GoButton: TButton;
    FileSaveDialog1: TFileSaveDialog;
    FileOpenDialog1: TFileOpenDialog;
    KeyInfoText: TLabel;
    OpenKeyButton: TButton;
    ClearButton: TButton;
    ExportKeyButton: TButton;
    procedure CreateKeyButtonClick(Sender: TObject);
    procedure OpenKeyButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure ExportKeyButtonClick(Sender: TObject);
  private
    Key : TLucidityKey;

    procedure ClearKeyInfo;
    procedure UpdateMemo;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  uAutoFree,
  eeHashes,
  EncdDecd,
  Punycode;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ClearKeyInfo;
end;

procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
  ClearKeyInfo;
end;

procedure TMainForm.ClearKeyInfo;
begin
  NameInput.Text := '';
  EmailInput.Text := '';
  KeyInfoText.Caption := '';
  Memo1.Clear;

  Key.Clear;
end;

procedure TMainForm.UpdateMemo;
var
  c1: Integer;
begin
  Memo1.Clear;

  Memo1.Lines.Add('BEGINKEY>>>>');
  Memo1.Lines.Add(Key.UserName);
  Memo1.Lines.Add(Key.UserEmail);
  for c1 := 0 to KeySaltCount-1 do
  begin
    Memo1.Lines.Add(Key.Sections[c1]);
  end;
  Memo1.Lines.Add(Key.DataCheck);
  Memo1.Lines.Add('<<<<ENDKEY');
end;






procedure TMainForm.CreateKeyButtonClick(Sender: TObject);
var
  Name, Email : String;
  Sections : array[0..7] of string;
  c1: Integer;
  DataCheck : string;
  KeyData : TStringList;
begin
  Key.Clear;

  Key := CreateLucidityKey(NameInput.Text, EmailInput.Text);

  UpdateMemo;

  if IsKeyValid_FullCheck(Key)
      then KeyInfoText.Caption := 'Key Is Valid (Full Check)'
      else KeyInfoText.Caption := 'Key Is Invalid';
end;

procedure TMainForm.ExportKeyButtonClick(Sender: TObject);
begin
  CreateKeyButtonClick(self);

  with FileSaveDialog1.FileTypes.Add do
  begin
    DisplayName := 'Key Data File';
    FileMask    := '*.dat';
  end;
  FileSaveDialog1.DefaultExtension := 'dat';
  FileSaveDialog1.FileName := 'LucidityKey.dat';

  if FileSaveDialog1.Execute then
  begin
    SaveKeyToFile(FileSaveDialog1.FileName, Key);
  end;
end;





procedure TMainForm.OpenKeyButtonClick(Sender: TObject);
var
  FileData : TStringList;
  KeyData  : TStringList;
begin
  if FileOpenDialog1.Execute then
  begin
    ClearKeyInfo;

    Key.LoadFromFile(FileOpenDialog1.FileName);

    NameInput.Text  := Key.UserName;
    EmailInput.Text := Key.UserEmail;

    UpdateMemo;

    if IsKeyValid_FullCheck(Key)
      then KeyInfoText.Caption := 'Key Is Valid (Full Check)'
      else KeyInfoText.Caption := 'Key Is Invalid';
  end;


end;






end.

