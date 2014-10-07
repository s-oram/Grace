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
  NativeXML,
  Lucidity.StateManager.PatchVersionUpdater;

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  SourceEdit.Text := '';
  DestEdit.Text   := '';

  // TODO:LOW The patch updater should write the last used source/dest edit values
  // to a config file.
  // The config file should be a simple wrapper object around an xml file or something.
  // something to generate key value pairs.
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
  XML : TNativeXML;
begin
  SourceFN := SourceEdit.Text;
  DestFN   := DestEdit.Text;

  XML := TNativeXML.Create(nil);
  try
    XML.LoadFromFile(SourceFN);
    CheckPatchVersionAndUpdateIfRequred(XML);
    XML.XmlFormat := xfReadable;
    XML.SaveToFile(DestFN);
  finally
    XML.Free;
  end;
end;


end.
