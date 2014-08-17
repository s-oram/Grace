(*
 NewAC Windows Media player demo.
 (c) Andrei Borovsky, anb@symmetrica.net
*)

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, ACS_Classes,
  Buttons, ACS_DXAudio, NewACDTS, ACS_Misc, AuASIO;

type
  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    StatusBar1: TStatusBar;
    DTSIn1: TDTSIn;
    AudioPlayList1: TAudioPlayList;
    Label1: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Button1: TButton;
    ListBox1: TListBox;
    DXAudioOut1: TDXAudioOut;
    procedure BitBtn1Click(Sender: TObject);
    procedure AudioOut1Done(Sender: TComponent);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AudioOut1ThreadException(Sender: TComponent;
      const Msg: String);
    procedure Button1Click(Sender: TObject);
    procedure AudioPlayList1PlayItemChanged(Sender: TComponent);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
    FS : TFileStream;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  DXAudioOut1.Run;
  BitBtn1.Enabled := False;
end;

procedure TForm1.AudioOut1Done(Sender: TComponent);
begin
   BitBtn1.Enabled := True;
   StatusBar1.Panels[0].Text := DXAudioOut1.ExceptionMessage;
end;

procedure TForm1.BitBtn2Click(Sender: TObject);
begin
  if DXAudioOut1.Status = tosPlaying then
    DXAudioOut1.Pause
  else
    DXAudioOut1.Resume;
end;

procedure TForm1.BitBtn3Click(Sender: TObject);
begin
  DXAudioOut1.Stop;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ListBox1.Items.Add(OpenDialog1.FileName);
    AudioPlayList1.Files.Assign(ListBox1.Items);
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  DXAudioOut1.Stop(False);
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var
  i : Integer;
begin
  if DXAudioOut1.Status <> tosPaused then
    for i := 0 to ListBox1.Items.Count - 1 do
      if ListBox1.Selected[i] then
       AudioPlayList1.CurrentItem := i;
end;

procedure TForm1.AudioOut1ThreadException(Sender: TComponent;
  const Msg: String);
begin
  StatusBar1.Panels[0].Text := Msg;
end;



procedure TForm1.AudioPlayList1PlayItemChanged(Sender: TComponent);
begin
  StatusBar1.Panels[0].Text := DTSIn1.FileName;
  Label1.Caption := IntToStr(DTSIn1.BitRate div 1000) + ' kbps';
  Label4.Caption := IntToStr(DTSIn1.SampleRate) + ' Hz';
  if DTSIn1.Channels = 1 then
    Label8.Caption := 'Mono'
  else
  if DTSIn1.Channels = 2 then
    Label8.Caption := 'Stereo'
  else
  if DTSIn1.Channels = 5 then
    Label8.Caption := '4.1 channels'
  else
  if DTSIn1.Channels = 6 then
    Label8.Caption := '5.1 channels'
  else
  if DTSIn1.Channels = 8 then
    Label8.Caption := '7.1 channels';
end;

end.
