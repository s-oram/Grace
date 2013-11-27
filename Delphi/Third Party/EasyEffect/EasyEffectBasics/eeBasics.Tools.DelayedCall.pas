unit eeBasics.Tools.DelayedCall;

interface

uses
  SysUtils;


{
  DelayedCall() Example:

    var
      Delay : integer; //In milliseconds.
      proc:TProc;
    begin
      // Wrap your delayed code in an annoymous method.
      proc := procedure()
      begin
        //Do stuff here.... for example:
        showMessage('Hello World');
      end;

      // Call your procedure with a delay
      Delay := 2000;
      DelayedCall(proc, Delay);
    end;
}

procedure DelayedCall(aProcedure : TProc; Delay_ms : integer);

implementation

uses
  Vcl.ExtCtrls;

  //NOTE: Before using in production code, the TTimer class should probably
  // be replaced with a thread. That will remove the dependence of the VCL.

type
  TDelayedCallClass = class
  private
    Timer : TTimer;
    ProcToCall : TProc;
    procedure EventHandler(Sender:TObject);
  public
    constructor Create(aProcedure : TProc; Delay_ms : integer);
    destructor Destroy; override;
  end;


{ TDelayedCallClass }

constructor TDelayedCallClass.Create(aProcedure: TProc; Delay_ms: integer);
begin
  Timer := TTimer.Create(nil);
  Timer.Enabled := false;
  Timer.Interval := Delay_ms;
  Timer.OnTimer  := EventHandler;
  ProcToCall := aProcedure;
  Timer.Enabled := true;
end;

destructor TDelayedCallClass.Destroy;
begin
  Timer.Free;
  inherited;
end;

procedure TDelayedCallClass.EventHandler(Sender: TObject);
begin
  (Sender as TTimer).Enabled := false;
  ProcToCall;
  self.Free;
end;

procedure DelayedCall(aProcedure : TProc; Delay_ms : integer);
begin
  TDelayedCallClass.Create(aProcedure, Delay_ms);
end;


end.
