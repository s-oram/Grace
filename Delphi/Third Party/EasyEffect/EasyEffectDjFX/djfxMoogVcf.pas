unit djfxMoogVcf;

interface

uses
  eeGlobals, VamLib.MoreTypes, djfxBase, eeFilters;

type
  TDJFXMoogLP = class(TDJFXCustomFX)
  private
  protected
    procedure SetParValue(Index: integer; const Value: single); override;
  public
    constructor Create(aGlobals:TGlobals); override;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); override;
  end;


implementation

{ TDJFXOnePoleLP }

constructor TDJFXMoogLP.Create(aGlobals: TGlobals);
begin
  inherited;

end;

destructor TDJFXMoogLP.Destroy;
begin

  inherited;
end;

procedure TDJFXMoogLP.SetParValue(Index: integer; const Value: single);
begin
  inherited;

end;

procedure TDJFXMoogLP.Process(In1, In2: PSingle; SampleFrames: integer);
begin
  inherited;

end;



end.
