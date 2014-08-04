unit PlayListUnit;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, QButtons, QExtCtrls;

type
  TPLForm = class(TForm)
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    OpenDialog1: TOpenDialog;
    ListBox1: TListBox;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    SaveDialog1: TSaveDialog;
    OpenDialog2: TOpenDialog;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  PLForm: TPLForm;

implementation

{$R *.xfm}

procedure TPLForm.BitBtn1Click(Sender: TObject);
var
  i : Integer;
begin
  if OpenDialog1.Execute then
  begin
    for i := 0 to ListBox1.Items.Count-1 do
    begin
      if ListBox1.Selected[i] then
      begin
        ListBox1.Items.Insert(i, OpenDialog1.FileName);
        Exit;
      end;
    end;
    ListBox1.Items.Add(OpenDialog1.FileName);
  end;
end;

procedure TPLForm.BitBtn2Click(Sender: TObject);
var
  i : Integer;
begin
  for i := 0 to ListBox1.Items.Count-1 do
  begin
    if ListBox1.Selected[i] then
    begin
      ListBox1.Items.Delete(i);
      Exit;
    end;
  end;
end;

procedure TPLForm.BitBtn3Click(Sender: TObject);
begin
  Self.Hide;
end;

procedure TPLForm.BitBtn4Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  ListBox1.Items.SaveToFile(SaveDialog1.FileName);
end;

procedure TPLForm.BitBtn5Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
  ListBox1.Items.LoadFromFile(OpenDialog2.FileName);
end;

end.
