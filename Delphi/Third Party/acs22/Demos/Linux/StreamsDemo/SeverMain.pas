unit SeverMain;

interface

uses
  SysUtils, Types, Classes, Variants, QTypes, QGraphics, QControls, QForms, 
  QDialogs, QStdCtrls, IdBaseComponent, IdComponent, IdTCPServer,
  IdCustomHTTPServer, IdHTTPServer;

const
  FILE_ROOT = '/home/k3/Network';

type
  TForm1 = class(TForm)
    IdHTTPServer1: TIdHTTPServer;
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    procedure IdHTTPServer1CommandGet(AThread: TIdPeerThread;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.IdHTTPServer1CommandGet(AThread: TIdPeerThread;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  FS : TFileStream;
begin
  Memo1.Lines.Add(Format('%s %s%s', ['File requested:', PChar(FILE_ROOT), PChar(ARequestInfo.Document)]));
  if FileExists(FILE_ROOT+ARequestInfo.Document) then
  begin
    IdHTTPServer1.ServeFile(AThread, AResponseInfo,FILE_ROOT+ARequestInfo.Document);
{    FS := TFileStream.Create(FILE_ROOT+ARequestInfo.Document, fmOpenRead or fmShareDenyWrite);
    AResponseInfo.ContentStream := FS;
    AResponseInfo.WriteContent;
    FS.Free;
    AResponseInfo.CloseSession;}
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  IdHTTPServer1.Active := True;
  Button1.Enabled := False;
  Button2.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  IdHTTPServer1.Active := False;
  Button2.Enabled := False;
  Button1.Enabled := True;
end;

end.
