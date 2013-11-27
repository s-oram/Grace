{
  An empty place holder effect.
}


unit djfxNone;

interface

uses
  djfxBase, eeGlobals, MoreTypes;

type
  TDJFXNone = class(TDJFXCustomFX)
  public
    constructor Create(aGlobals:TGlobals); override;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); override;
  end;

implementation

{ TDJFXNone }

constructor TDJFXNone.Create(aGlobals: TGlobals);
begin
  inherited;    
  Name          := '--';
  fFxType       := dxNone;
  ParName[0]    := '--';
  ParName[1]    := '--';
  ParDisplay[0] := '--';
  ParDisplay[1] := '--';
  ParValue[0]   := 0.5;
  ParValue[1]   := 0.5;
end;

destructor TDJFXNone.Destroy;
begin

  inherited;
end;

procedure TDJFXNone.Process(In1, In2: PSingle; SampleFrames: integer);
begin


end;

end.
