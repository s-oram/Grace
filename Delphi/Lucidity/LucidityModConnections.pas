unit LucidityModConnections;

interface

uses
  Classes,
  VamLib.Collections.Lists,
  VamLib.MoreTypes,
  uConstants, uLucidityEnums;

type
  PModConnections = ^TModConnections;
  TModConnections = class
  private
    fModSource : array[0..kModSlotCount-1] of TModSource;
    fModVia    : array[0..kModSlotCount-1] of TModSource;
    fModMute   : array[0..kModSlotCount-1] of boolean;
    fOnChanged: TNotifyEvent;
  public
    constructor Create;
    destructor Destroy; override;

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
  self.fModSource[0] := TModSource.AmpEnv;
  self.fModSource[1] := TModSource.FilterEnv;
  self.fModSource[2] := TModSource.Lfo1;
  self.fModSource[3] := TModSource.Lfo2;
  self.fModSource[4] := TModSource.StepSeq1;
  self.fModSource[5] := TModSource.StepSeq2;
  self.fModSource[6] := TModSource.Midi_ModWheel;
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

function TModConnections.GetModVia(const ModSlotIndex: integer): TModSource;
begin
  result := fModVia[ModSlotIndex]
end;

procedure TModConnections.SetModMute(const ModSlotIndex: integer; IsMuted: boolean);
begin
  fModMute[ModSlotIndex] := IsMuted;
end;

procedure TModConnections.SetModSource(const ModSlotIndex: integer; aSource: TModSource);
begin
  fModSource[ModSlotIndex] := aSource;
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TModConnections.SetModVia(const ModSlotIndex: integer; aVia: TModSource);
begin
  fModVia[ModSlotIndex] := aVia;
  if assigned(OnChanged) then OnChanged(self);
end;


end.
