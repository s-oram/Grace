unit test_0_Beep;

interface

uses
  System.SysUtils, FMX.Types, System.Classes, FMX.Controls, FMX.Forms,
  {$IFDEF MSWindows}Winapi.Windows,{$ENDIF}
  OtlTask,
  OtlTaskControl;

type
  TfrmTestSimple = class(TForm)
    btnBeep: TButton;
    procedure btnBeepClick(Sender: TObject);
  private
    procedure Beep(const task: IOmniTask);
  end;

var
  frmTestSimple: TfrmTestSimple;

implementation

uses
  DSiWin32;

{$R *.FMX}

{ TfrmTestOTL }

procedure TfrmTestSimple.btnBeepClick(Sender: TObject);
begin
  CreateTask(Beep, 'Beep').Run;
end;

procedure TfrmTestSimple.Beep(const task: IOmniTask);
begin
  //Executed in a background thread
  {$IFDEF MSWindows}
  MessageBeep($FFFFFFFF);
  {$ELSE}
  System.SysUtils.Beep;
  {$ENDIF}
end;

initialization
  Randomize;
end.
