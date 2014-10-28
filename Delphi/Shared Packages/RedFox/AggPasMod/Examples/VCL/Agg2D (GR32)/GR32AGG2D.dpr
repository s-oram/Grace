program GR32AGG2D;

uses
  Vcl.Forms,
  MainUnit in 'MainUnit.pas' {FmGr32Agg2D},
  GR32_Agg2D in 'GR32_Agg2D.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmGr32Agg2D, FmGr32Agg2D);
  Application.Run;
end.

