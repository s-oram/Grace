unit eeProfilerV2.Form;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TStringListEvent = procedure(Sender : TObject; var Data:TStringList) of object;

  TProfillerForm = class(TForm)
    UpdateTimer: TTimer;
    ReportDisplayBox: TMemo;
    ResetButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure UpdateTimerStep(Sender: TObject);
    procedure ResetButtonClick(Sender: TObject);
  private
    fOnGetReportData: TStringListEvent;

    Data : TStringList;
    fOnReset: TNotifyEvent;
  public


    property OnGetReportData : TStringListEvent read fOnGetReportData write fOnGetReportData;

    property OnReset : TNotifyEvent read fOnReset write fOnReset;
  end;


implementation

{$R *.dfm}

procedure TProfillerForm.FormCreate(Sender: TObject);
begin
  ReportDisplayBox.Clear;

  Data := TStringList.Create;
end;

procedure TProfillerForm.FormDestroy(Sender: TObject);
begin
  Data.Free;
end;

procedure TProfillerForm.ResetButtonClick(Sender: TObject);
begin
  if assigned(OnReset) then OnReset(self);
end;

procedure TProfillerForm.UpdateTimerStep(Sender: TObject);
begin
  Data.Clear;

  if assigned(OnGetReportData) then
  begin
    OnGetReportData(self, Data);
  end;

  ReportDisplayBox.Lines.Assign(Data);
  ReportDisplayBox.Invalidate;
end;

end.
