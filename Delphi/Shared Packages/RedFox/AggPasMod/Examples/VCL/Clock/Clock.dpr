program Clock;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FmClock};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmClock, FmClock);
  Application.Run;
end.

