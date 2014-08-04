program AudioPlayer;

uses
  QForms,
  Main in 'Main.pas' {Form1},
  PlayListUnit in 'PlayListUnit.pas' {PLForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TPLForm, PLForm);
  Application.Run;
end.
