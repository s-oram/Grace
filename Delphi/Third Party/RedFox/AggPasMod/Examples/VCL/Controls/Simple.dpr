program Simple;

{$I AggCompiler.inc}

uses
{$IFNDEF FPC}
  FastMM4,
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {FmAggPasControlsdemo};

begin
  Application.Initialize;
  Application.CreateForm(TFmAggPasControlsdemo, FmAggPasControlsdemo);
  Application.Run;
end.

