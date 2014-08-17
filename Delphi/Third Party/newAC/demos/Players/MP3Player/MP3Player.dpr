program MP3Player;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  ACS_Wave;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
