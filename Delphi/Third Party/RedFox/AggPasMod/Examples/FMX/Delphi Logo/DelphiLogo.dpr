program DelphiLogo;

uses
  FMX.Forms,
  MainUnit in 'MainUnit.pas' {FmDelphiLogo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmDelphiLogo, FmDelphiLogo);
  Application.Run;
end.

