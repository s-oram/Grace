program DemoAgg2D;

{$I AggCompiler.inc}

uses
{$IFNDEF FPC}
  FastMM4,
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {FmAgg2D};

begin
  Application.Initialize;
  Application.CreateForm(TFmAgg2D, FmAgg2D);
  Application.Run;
end.

