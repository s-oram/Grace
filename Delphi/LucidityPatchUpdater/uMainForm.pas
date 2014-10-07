unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm3 = class(TForm)
    SourceEdit: TEdit;
    DestEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SourceSelectButton: TButton;
    DestSelectButton: TButton;
    ConvertPatchButton: TButton;
    OpenSourceButton: TButton;
    OpenDestButton: TButton;
    FileOpenDialog1: TFileOpenDialog;
    FileSaveDialog1: TFileSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SourceSelectButtonClick(Sender: TObject);
    procedure DestSelectButtonClick(Sender: TObject);
    procedure ConvertPatchButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses
  Lucidity.StateManager;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  SourceEdit.Text := '';
  DestEdit.Text   := '';
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TForm3.SourceSelectButtonClick(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
  begin
    SourceEdit.Text := FileOpenDialog1.FileName;
  end;
end;

procedure TForm3.DestSelectButtonClick(Sender: TObject);
begin
  if FileSaveDialog1.Execute then
  begin
    DestEdit.Text := FileSaveDialog1.FileName;
  end;

end;

procedure TForm3.ConvertPatchButtonClick(Sender: TObject);
var
  SourceFN, DestFN : string;
begin
  SourceFN := SourceEdit.Text;
  DestFN   := DestEdit.Text;



end;





end.
