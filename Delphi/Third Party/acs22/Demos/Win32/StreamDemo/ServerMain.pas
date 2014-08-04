unit ServerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IdBaseComponent, IdComponent, IdTCPServer, ACS_Classes,
  ACS_Audio, ACS_Vorbis, StdCtrls, ACS_Wave, IdHTTPServer, IdUDPBase,
  IdUDPServer, IdTrivialFTPServer;

type
  TForm1 = class(TForm)
    AudioOut1: TAudioOut;
    Button1: TButton;
    Button2: TButton;
    WaveIn1: TWaveIn;
    IdTrivialFTPServer1: TIdTrivialFTPServer;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdTrivialFTPServer1WriteFile(Sender: TObject;
      var FileName: String; const PeerInfo: TPeerInfo;
      var GrantAccess: Boolean; var AStream: TStream;
      var FreeStreamOnComplete: Boolean);
    procedure IdTrivialFTPServer1TransferComplete(Sender: TObject;
      const Success: Boolean; const PeerInfo: TPeerInfo; AStream: TStream;
      const WriteOperation: Boolean);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  MS := TMemoryStream.Create;
  WaveIn1.Stream := MS;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MS.Free;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  IdTrivialFTPServer1.Binding.IP := Edit1.Text;
  IdTrivialFTPServer1.Active := True;
  Label1.Caption := Format('Listening at %s:%d',[IdTrivialFTPServer1.Binding.IP,
       IdTrivialFTPServer1.DefaultPort]);
  Button1.Enabled := False;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  IdTrivialFTPServer1.Active := False;
  Label1.Caption := '';
  Button1.Enabled := True;
end;

procedure TForm1.IdTrivialFTPServer1WriteFile(Sender: TObject;
  var FileName: String; const PeerInfo: TPeerInfo;
  var GrantAccess: Boolean; var AStream: TStream;
  var FreeStreamOnComplete: Boolean);
begin
  if AudioOut1.Status <> tosIdle then GrantAccess := False
  else
  begin
    GrantAccess := True;
    MS.Clear;
    AStream := MS;
    FreeStreamOnComplete := False;
  end;  
end;

procedure TForm1.IdTrivialFTPServer1TransferComplete(Sender: TObject;
  const Success: Boolean; const PeerInfo: TPeerInfo; AStream: TStream;
  const WriteOperation: Boolean);
begin
  MS.Position :=0;
  AudioOut1.Run;
//  MS.Clear;
end;

end.
