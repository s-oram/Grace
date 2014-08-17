unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ACS_Classes, ACS_Misc, ACS_FLAC, ACS_WinMedia, ACS_smpeg,
  ACS_Wave, ACS_Vorbis;

type
  TForm10 = class(TForm)
    GainAnalysis1: TGainAnalysis;
    NULLOut1: TNULLOut;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    ListBox1: TListBox;
    MP3In1: TMP3In;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure NULLOut1Done(Sender: TComponent);
  private
    { Private declarations }
  public
    { Public declarations }
    index : Integer;
  end;

var
  Form10: TForm10;

implementation

{$R *.dfm}

procedure TForm10.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ListBox1.Items.Assign(OpenDialog1.Files);
    index := 0;
    GainAnalysis1.NewAlbum;
    MP3In1.FileName := ListBox1.Items[0];
    Button1.Enabled := False;
    NULLOut1.Run;
  end;
end;

procedure TForm10.NULLOut1Done(Sender: TComponent);
begin
  ListBox1.Items[Index] := ListBox1.Items[Index] + Format(' [gain = %f dB, peak = %f %%]', [GainAnalysis1.TitleGain,GainAnalysis1.TitlePeak]);
  Inc(Index);
  if Index >= ListBox1.Items.Count then
  begin
    ListBox1.Items.Add(Format('Album gain %f dB peak = %f %%', [GainAnalysis1.AlbumGain, GainAnalysis1.AlbumPeak]));
    Button1.Enabled := True;
  end
  else
  begin
    MP3In1.FileName := ListBox1.Items[Index];
    NULLOut1.Run;
  end;
end;

end.
