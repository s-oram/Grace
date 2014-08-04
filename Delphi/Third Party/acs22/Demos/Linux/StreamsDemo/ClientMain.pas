unit ClientMain;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, ACS_Classes, ACS_Audio, ACS_Vorbis, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, IdHTTP, IdAntiFreezeBase,
  IdAntiFreeze;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    IdHTTP1: TIdHTTP;
    VorbisIn1: TVorbisIn;
    AudioOut1: TAudioOut;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure AudioOut1Done(Sender: TComponent);
  private
    { Private declarations }
    MS : TMemoryStream;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  MS.Clear;
  IdHTTP1.Get('http://127.0.0.1/'+Edit1.Text, MS);
  MS.Position := 0;
  if MS.Size <> 0 then
  begin
    VorbisIn1.Stream := MS;
    AudioOut1.Run;
    Button1.Enabled := False;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MS := TMemoryStream.Create;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MS.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  AudioOut1.Stop;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
  MS.Clear;
  Button1.Enabled := True;
end;

end.
