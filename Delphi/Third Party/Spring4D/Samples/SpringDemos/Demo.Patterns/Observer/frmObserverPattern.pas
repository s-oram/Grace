unit frmObserverPattern;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, uObserverDemo;

type
  TForm28 = class(TForm)
    Timer1: TTimer;
    Edit1: TEdit;
    Edit2: TEdit;
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FEditorMonitor: TEditorMonitor;
  end;

var
  Form28: TForm28;

implementation

{$R *.dfm}

procedure TForm28.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm28.FormCreate(Sender: TObject);
begin
  FEditorMonitor := TEditorMonitor.Create;

  FEditorMonitor.AddListener(TCurrentTimeEditUpdater.Create(Edit1));
  FEditorMonitor.AddListener(TTickTimeEditUpdater.Create(Edit2));
end;

procedure TForm28.FormDestroy(Sender: TObject);
begin
  FEditorMonitor.Free;
end;

procedure TForm28.Timer1Timer(Sender: TObject);
var
  UpdateProc: TProc<TEditUpdater>;
begin
  UpdateProc := procedure(aEditUpdater: TEditUpdater)
                begin
                  aEditUpdater.Update;
                end;

  FEditorMonitor.NotifyListeners(UpdateProc);
end;

end.
