unit LucidityModConnections;

interface

uses
  Classes,
  VamLib.Collections.Lists,
  VamLib.MoreTypes,
  uConstants, uLucidityEnums;

type
  // NOTE: TModConnections stores all mod connections for the sampler.
  TModLink = record
    ModAmount : array[0..kModSlotCount-1] of single;
  end;

  TModLinkArray = TArray<TModLink>;


  PModConnections = ^TModConnections;
  TModConnections = class
  private
    fModSource : array[0..kModSlotCount-1] of TModSource;
    fModVia    : array[0..kModSlotCount-1] of TModSource;
    fModMute   : array[0..kModSlotCount-1] of boolean;

    fOnChanged: TNotifyEvent;

    //TODO: Delete ModLinks. - it's not being used.
    fModLinks: TModLinkArray;
    property ModLinks  : TModLinkArray   read fModLinks  write fModLinks; //One for each modulated parameter
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



  // NOTE: TModConnections stores all mod connections for the sampler.
  PModLink_OLD = ^TModLink_OLD;
  TModLink_OLD = record
    UniqueID : string; //NOTE: I'm not entirely sure this UniqueID is a good idea.
    Source   : TModSource;
    Dest     : TModDest;
    Via      : TModSource;
    Amount   : single; //range -1..1
    Offset   : single; //range -0.5..0.5
    procedure AssignFrom(const aSource:TModLink_OLD);
  end;



implementation

uses
  SysUtils;

{ TModConnections }

constructor TModConnections.Create;
begin
  SetLength(fModLinks, kModulatedParameterCount);

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
  SetLength(fModLinks, 0);
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

{ TModLink_OLD }

procedure TModLink_OLD.AssignFrom(const aSource: TModLink_OLD);
begin
  self.Source   := aSource.Source;
  self.Dest     := aSource.Dest;
  self.Via      := aSource.Via;
  self.Amount   := aSource.Amount;
  self.Offset   := aSource.Offset;
  self.UniqueID := aSource.UniqueID;
end;


end.
