unit ClientMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACS_Classes, ACS_Vorbis, ACS_Audio, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient, ACS_Wave, IdHTTP, IdUDPBase,
  IdUDPClient, IdTrivialFTP;

type
  TForm1 = class(TForm)
    AudioIn1: TAudioIn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    WaveOut1: TWaveOut;
    WaveIn1: TWaveIn;
    AudioOut1: TAudioOut;
    IdTrivialFTP1: TIdTrivialFTP;
    Button4: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure WaveOut1Done(Sender: TComponent);
    procedure IdTrivialFTP1WorkEnd(Sender: TObject; AWorkMode: TWorkMode);
    procedure Button4Click(Sender: TObject);
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

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Button1.Enabled := False;
  Button3.Enabled := False;
  Button4.Enabled := False;
  Button2.Enabled := True;
  MS.Clear;
  MS.Position := 0;
  WaveOut1.Run;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MS := TMemoryStream.Create;
  WaveOut1.Stream := MS;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MS.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  WaveOut1.Stop;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  IdTrivialFTP1.Host := Edit1.Text;
  IdTrivialFTP1.Put(MS, '');
end;

procedure TForm1.WaveOut1Done(Sender: TComponent);
begin
  MS.Position := 0;
  Button2.Enabled := False;
  Button1.Enabled := True;
  Button3.Enabled := True;
  Button4.Enabled := True;
end;

procedure TForm1.IdTrivialFTP1WorkEnd(Sender: TObject;
  AWorkMode: TWorkMode);
begin
  Button4.Enabled := False;
  Button3.Enabled := False;
  MS.Clear;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  WaveIn1.Stream := MS;
  AudioOut1.Run;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
  MS.Position := 0;
end;

end.
