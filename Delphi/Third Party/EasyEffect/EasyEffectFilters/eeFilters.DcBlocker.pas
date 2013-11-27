unit eeFilters.DcBlocker;

interface

uses
  MoreTypes;

type

  // This DC Blocking filter is described in
  // "Streamlining Digital Signal Processing" page 297.
  TDcBlocker = class
  private
    OldL : double;
    OldR : double;
    a1   : double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Process(var In1:single);      overload; inline;
    procedure Process(var In1:double);      overload; inline;
    procedure Process(var In1, In2:single); overload; inline;

    property Coefficient1 : double read a1 write a1; //range 0..1.  Default 0.8;
  end;

implementation

{ TDcBlocker }

constructor TDcBlocker.Create;
begin
  OldL := 0;
  OldR := 0;
  a1 := 0.999;
end;

destructor TDcBlocker.Destroy;
begin

  inherited;
end;

procedure TDcBlocker.Process(var In1: single);
var
  xL : double;
begin
  xL   := In1 + (OldL * a1);
  In1  := xL  - OldL;
  OldL := xL;
end;

procedure TDcBlocker.Process(var In1: double);
var
  xL : double;
begin
  xL   := In1 + (OldL * a1);
  In1  := xL  - OldL;
  OldL := xL;
end;



procedure TDcBlocker.Process(var In1, In2: single);
var
  xL : double;
  xR : double;
begin
  xL   := In1 + (OldL * a1);
  In1  := xL  - OldL;
  OldL := xL;

  xR   := In2 + (OldR * a1);
  In2  := xR  - OldR;
  OldR := xR;
end;


end.
