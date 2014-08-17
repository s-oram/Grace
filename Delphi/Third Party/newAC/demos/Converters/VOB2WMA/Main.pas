unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACS_Classes, ACS_WinMedia, ACS_Misc, NewACDTS, ComCtrls,
  NewACAC3;

type
  TForm6 = class(TForm)
    OpenDialog1: TOpenDialog;
    WMAOut1: TWMAOut;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button3: TButton;
    StatusBar1: TStatusBar;
    DTSIn1: TDTSIn;
    AC3In1: TAC3In;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure WMAOut1Done(Sender: TComponent);
    procedure WMAOut1ThreadException(Sender: TComponent);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    DTSIn1.FileName := OpenDialog1.FileName;
    AC3In1.FileName := OpenDialog1.FileName;
  end;
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    WMAOut1.Input := DTSIn1;
    if not DTSIn1.Valid then
    begin
      WMAOut1.Input := AC3In1;
      if not AC3In1.Valid then
        raise Exception.Create('No DTS or AC-3 audio stream in the input file');
    end;
    WMAOut1.FileName := SaveDialog1.FileName;
    WMAOut1.Id3v2Tags.Artist := Edit1.Text;
    WMAOut1.Id3v2Tags.Album := Edit2.Text;
    Button1.Enabled := False;
    Button2.Enabled := False;
    Button3.Enabled := False;
    WMAOut1.Run;
  end;
end;

procedure TForm6.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WMAOut1.Stop(False);
end;

procedure TForm6.WMAOut1Done(Sender: TComponent);
begin
  Button1.Enabled := True;
  Button2.Enabled := True;
  Button3.Enabled := True;
  if WMAOut1.ExceptionMessage = '' then
    StatusBar1.Panels[0].Text := 'Done.';
end;

procedure TForm6.WMAOut1ThreadException(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := WMAOut1.ExceptionMessage;
end;

end.
