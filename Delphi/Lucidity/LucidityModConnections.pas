unit LucidityModConnections;

interface

uses
  Classes,
  VamLib.Collections.Lists,
  VamLib.MoreTypes,
  Lucidity.Enums,
  uConstants;

{$SCOPEDENUMS ON}

type
  PModConnections = ^TModConnections;
  TModConnections = class
  private
    fModSourcePolarity : array[0..kModSlotCount-1] of TModSourcePolarity;
    fModSource : array[0..kModSlotCount-1] of TModSource;
    fModVia    : array[0..kModSlotCount-1] of TModSource;
    fModMute   : array[0..kModSlotCount-1] of boolean;
    fOnChanged: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

    function GetModSourcePolarity(const ModSlotIndex : integer):TModSourcePolarity;
    procedure SetModSourcePolarity(const ModSlotIndex : integer; const aPolarity : TModSourcePolarity);

    function GetModMute(const ModSlotIndex : integer) : boolean;
    procedure SetModMute(const ModSlotIndex : integer; IsMuted:boolean);

    function GetModSource(const ModSlotIndex : integer) : TModSource;
    procedure SetModSource(const ModSlotIndex : integer; aSource:TModSource);

    function GetModVia(const ModSlotIndex : integer) : TModSource;
    procedure SetModVia(const ModSlotIndex : integer; aVia:TModSource);

    property OnChanged : TNotifyEvent read fOnChanged write fOnChanged;
  end;



implementation

uses
  SysUtils;

{ TModConnections }

constructor TModConnections.Create;
begin
  self.fModSource[0] := TModSource.AmpEnv_Unipolar;
  self.fModSource[1] := TModSource.ModEnv_Unipolar;
  self.fModSource[2] := TModSource.Lfo1_UniPolar;
  self.fModSource[3] := TModSource.Lfo2_UniPolar;
  self.fModSource[4] := TModSource.StepSeq1_Unipolar;
  self.fModSource[5] := TModSource.StepSeq2_Unipolar;
  self.fModSource[6] := TModSource.Midi_ModWheel_Unipolar;
  //self.ModSource[7] := TModSource.Midi_Velocity; //TODO Add velocity as a mod source.
end;

destructor TModConnections.Destroy;
begin
  inherited;
end;


function TModConnections.GetModMute(const ModSlotIndex: integer): boolean;
begin
  result := fModMute[ModSlotIndex];
end;

function TModConnections.GetModSource(const ModSlotIndex: integer): TModSource;
begin
  result := fModSource[ModSlotIndex];
end;

function TModConnections.GetModSourcePolarity(const ModSlotIndex : integer): TModSourcePolarity;
begin
  result := fModSourcePolarity[ModSlotIndex];
end;

function TModConnections.GetModVia(const ModSlotIndex: integer): TModSource;
begin
  result := fModVia[ModSlotIndex]
end;

procedure TModConnections.SetModMute(const ModSlotIndex: integer; IsMuted: boolean);
begin
  fModMute[ModSlotIndex] := IsMuted;
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TModConnections.SetModSource(const ModSlotIndex: integer; aSource: TModSource);
begin
  fModSource[ModSlotIndex] := aSource;
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TModConnections.SetModSourcePolarity(const ModSlotIndex : integer; const aPolarity: TModSourcePolarity);
begin
  fModSourcePolarity[ModSlotIndex] := aPolarity;
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TModConnections.SetModVia(const ModSlotIndex: integer; aVia: TModSource);
begin
  fModVia[ModSlotIndex] := aVia;
  if assigned(OnChanged) then OnChanged(self);
end;


end.
