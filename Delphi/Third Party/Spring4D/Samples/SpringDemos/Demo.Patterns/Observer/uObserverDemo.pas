unit uObserverDemo;

interface

uses
      Spring.DesignPatterns
    , StdCtrls
    ;

type

  TEditUpdater =  class abstract
  private
    FEdit: TEdit;
  public
    constructor Create(aEdit: TEdit);
    procedure Update; virtual; abstract;
    property Edit: TEdit read FEdit;
  end;

  TCurrentTimeEditUpdater = class(TEditUpdater)
    procedure Update; override;
  end;

  TTickTimeEditUpdater = class(TEditUpdater)
    procedure Update; override;
  end;

  TEditorMonitor = class(TObservable<TEditUpdater>)
    destructor Destroy; override;
  end;

implementation

{ TEditUpdater }

uses
       SysUtils
     , Windows
     ;

constructor TEditUpdater.Create(aEdit: TEdit);
begin
  inherited Create;
  FEdit := aEdit;
end;


{ TCurrentTimeEditUpdater }

procedure TCurrentTimeEditUpdater.Update;
begin
  inherited;
  FEdit.Text := DateTimeToStr(Now);
end;

{ TTickTimeEditUpdater }

procedure TTickTimeEditUpdater.Update;
begin
  inherited;
  FEdit.Text := IntToStr(GetTickCount);
end;

{ TEditorMonitor }

destructor TEditorMonitor.Destroy;
var
  aEditUpdater: TEditUpdater;
begin
  for aEditUpdater in Listeners do
  begin
    aEditUpdater.Free;
  end;

  inherited;
end;

end.
