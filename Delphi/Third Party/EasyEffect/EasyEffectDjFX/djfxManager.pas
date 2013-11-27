{
  TODO: May need to look at locks to avoid errors in multi-threaded apps.
}

unit djfxManager;

interface

uses
  Contnrs, eeGlobals,
  djfxBase;

type
  TDJFXManager = class
  private
    function GetFx(Index: integer): TDJFXCustomFX;
    procedure SetFx(Index: integer; const Value: TDJFXCustomFX);
  protected
    Globals:TGlobals;
    FxList:TObjectList;
  public
    constructor Create(aGlobals:TGlobals);
	  destructor Destroy; override;

    function AddNew(FxType:TDJFXType):integer;
    procedure Insert(Index:integer; FxType:TDJFXType);
    procedure Replace(Index:integer; NewFx:TDJFXType);
    procedure Delete(Index:integer);

    property FX[Index:integer]:TDJFXCustomFX read GetFx write SetFx; default;
  end;

implementation

uses
  SysUtils,
  djfxNone,
  djfxLofiA,
  djfxAutoPan,
  djfxAmpMod,
  djfxPitchMod,
  djfxOnePoleLP,
  djfxTwoPoleLP,
  djfxMoogLP;

{ TDJFXManager }

constructor TDJFXManager.Create(aGlobals: TGlobals);
begin
  Globals := aGlobals;

  FxList := TObjectList.Create;
  FxList.OwnsObjects := true;
end;

destructor TDJFXManager.Destroy;
begin
  FxList.Free;
  inherited;
end;

function TDJFXManager.GetFx(Index: integer): TDJFXCustomFX;
begin
  result := FxList[Index] as TDJFXCustomFX;
end;

procedure TDJFXManager.SetFx(Index: integer; const Value: TDJFXCustomFX);
begin
  FxList[Index] := Value;
end;

procedure TDJFXManager.Insert(Index:integer; FxType: TDJFXType);
var
  efx:TDJFXCustomFX;
begin
  case FxType of
    dxNone:      efx := TDJFXNone.Create(Globals);
    dxLofiA:     efx := TDJFXLofiA.Create(Globals);
    dxAutoPan:   efx := TDJFXAutoPan.Create(Globals);
    dxAmpMod:    efx := TDJFXAmpMod.Create(Globals);
    dxPitchMod:  efx := TDJFXPitchMod.Create(Globals);
    dxOnePoleLP: efx := TDJFXOnePoleLP.Create(Globals);
    dxTwoPoleLP: efx := TDJFXTwoPoleLP.Create(Globals);
    dxMoogLP:    efx := TDJFXMoogLP.Create(Globals);
  else
    efx := TDJFXNone.Create(Globals);
    FxList.Insert(Index, efx);
    raise Exception.Create('Effect type not handled.');
  end;

  FxList.Insert(Index, efx);
end;


function TDJFXManager.AddNew(FxType: TDJFXType): integer;
begin
  Insert(FxList.Count, FxType);
  result := FxList.Count-1;
end;




procedure TDJFXManager.Replace(Index: integer; NewFx: TDJFXType);
begin
  FxList.Delete(Index);
  Insert(Index, NewFx);
end;

procedure TDJFXManager.Delete(Index: integer);
begin
  FxList.Delete(Index);
end;







end.
