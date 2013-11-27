unit djfxBase;

interface

uses
  eeGlobals, MoreTypes;

const
  DJFXParCount = 2;

type
  TDJFXType = (dxNone, dxLofiA, dxAutoPan, dxAmpMod, dxPitchMod, dxOnePoleLP, dxTwoPoleLP, dxMoogLP);

const
  TagDXNone      = 1;
  TagDXLofiA     = 2;
  TagDXAutoPan   = 3;
  TagDXAmpMod    = 4;
  TagDXPitchMod  = 5;
  TagDXOnePoleLP = 6;
  TagDXTwoPoleLP = 7;
  TagDXMoogLP    = 8;

type
  // TDJFXCustomFX is the base effect class that all "DJ" effects are derived from.
  TDJFXCustomFX = class
  private
    fName: string;
    fParName    :array[0..DJFXParCount-1] of string;
    fParDisplay :array[0..DJFXParCount-1] of string;
    fParValue   :array[0..DJFXParCount-1] of single;
    function GetParName(Index: integer): string;
    function GetParDisplay(Index: integer): string;
    function GetParValue(Index: integer): single;

    procedure SetParName(Index: integer; const Value: string);
    procedure SetParDisplay(Index: integer; const Value: string);
  protected
    Globals:TGlobals;
    fFxType: TDJFXType;
    procedure SetParValue(Index: integer; const Value: single); virtual; //Override to respond to parameter changes.
  public
    constructor Create(aGlobals:TGlobals); virtual;
	  destructor Destroy; override;

    procedure Process(In1, In2:PSingle; SampleFrames:integer); virtual; abstract;

    property Name    :string    read fName     write fName;
    property FxType  :TDJFXType read fFxType;

    property ParName    [Index:integer]:string  read GetParName     write SetParName;
    property ParDisplay [Index:integer]:string  read GetParDisplay  write SetParDisplay;
    property ParValue   [Index:integer]:single  read GetParValue    write SetParValue; //range 0..1
  end;

implementation

{ TDJFXBase }

constructor TDJFXCustomFX.Create(aGlobals: TGlobals);
begin
  Globals := aGlobals;
end;

destructor TDJFXCustomFX.Destroy;
begin

  inherited;
end;

function TDJFXCustomFX.GetParDisplay(Index: integer): string;
begin
  result := fParDisplay[Index];
end;

function TDJFXCustomFX.GetParName(Index: integer): string;
begin
  result := fParName[Index];
end;

function TDJFXCustomFX.GetParValue(Index: integer): single;
begin
  assert(fParValue[Index] >= 0);
  assert(fParValue[Index] <= 1);

  result := fParValue[Index];
end;

procedure TDJFXCustomFX.SetParDisplay(Index: integer; const Value: string);
begin
  fParDisplay[Index] := Value;
end;

procedure TDJFXCustomFX.SetParName(Index: integer; const Value: string);
begin
  fParName[Index] := Value;
end;

procedure TDJFXCustomFX.SetParValue(Index: integer; const Value: single);
begin
  assert(Value >= 0);
  assert(Value <= 1);

  fParValue[Index] := Value;
end;

end.
