unit soCustomSynthFilter;

interface

uses
  eeGlobals;

type
  TCustomSynthFilter = class
  private
    fGlobals: TGlobals;
    procedure SetPar1(const Value: single);
    procedure SetPar2(const Value: single);
    procedure SetPar3(const Value: single);
    procedure SetPar4(const Value: single);
  protected
    fPar1: single;
    fPar2: single;
    fPar3: single;
    fPar4: single;
    

    property Globals : TGlobals read fGlobals write fGlobals;
  public
    constructor Create(aGlobals : TGlobals);
    destructor Destroy; override;

    property Par1 : single read fPar1 write SetPar1; //range 0..1
    property Par2 : single read fPar2 write SetPar2; //range 0..1
    property Par3 : single read fPar3 write SetPar3; //range 0..1
    property Par4 : single read fPar4 write SetPar4; //range 0..1
  end;

implementation

{ TCustomSynthFilter }

constructor TCustomSynthFilter.Create(aGlobals: TGlobals);
begin
  Globals := aGlobals;
end;

destructor TCustomSynthFilter.Destroy;
begin

  inherited;
end;

procedure TCustomSynthFilter.SetPar1(const Value: single);
begin
  fPar1 := Value;
end;

procedure TCustomSynthFilter.SetPar2(const Value: single);
begin
  fPar2 := Value;
end;

procedure TCustomSynthFilter.SetPar3(const Value: single);
begin
  fPar3 := Value;
end;

procedure TCustomSynthFilter.SetPar4(const Value: single);
begin
  fPar4 := Value;
end;

end.
