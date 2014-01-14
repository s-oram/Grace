unit soModMatrix;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.MoreTypes, LucidityModConnections,
  uConstants, uLucidityEnums, eeFunctions,
  SoundObjectTypes;

  {$SCOPEDENUMS ON}

type
  TModSourceValues = array of PSingle;
  TModDestValues   = array of PSingle;

  PPrivateModLink = ^TPrivateModLink;
  TPrivateModLink = record
    UniqueID : string;
    Source   : TModSource;
    Dest     : TModDest;
    Via      : TModSource;
    Amount   : single; //range -1..1
    Offset   : single; //range -0.5..0.5
    PSource  : PSingle;
    PVia     : PSingle;
    PDest    : PSingle;
    procedure AssignFrom(const aSource:TModLink_OLD);
  end;



  TModMatrix = class
  private
    fStepSeq1ClockSource: TClockSource;
    fModCheck: single;
    fStepSeq2ClockSource: TClockSource;
  protected
    type
      TModLinkState = (Inactive, FastMod, SlowMod);
    var
    FakeDest, FakeSource : single;

    fModSourceValues : TModSourceValues;
    fModDestValues   : TModDestValues;

    ModSourceCount : integer;
    ModDestCount   : integer;

    FastModulations : array of TPrivateModLink;
    SlowModulations : array of TPrivateModLink;

    FastModulationCount : integer;
    SlowModulationCount : integer;

    function GetModLinkState(aModLink : PModLink_OLD):TModLinkState;
    procedure CalcModOutput(const aModLink : PPrivateModLink); {$IFDEF AudioInline}inline;{$ENDIF}
    function CalcModOffset(aModLink : PPrivateModLink): single;
    function AreModSourceValuesInRange:boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure UpdateAllModLinks(const aModConnections : TModConnections_OLD);
    procedure UpdateModLink(const ModLinkData : PModLink_OLD);

    //==========================================================================
    // NOTE: These methods are here for debugging purposes.
    function GetModSourceValue( const aModSource : TModSource):single;
    property ModSourceValues : TModSourceValues read fModSourceValues;
    property ModDestValues   : TModDestValues   read fModDestValues;
    //==========================================================================

    procedure ZeroAllValues;

    procedure SetModSourcePointer(const aModSource : TModSource; const Source:PSingle);
    procedure SetModDestPointer(const aModDest : TModDest; const Dest:PSingle);

    procedure FastControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}
    procedure SlowControlProcess; {$IFDEF AudioInline}inline;{$ENDIF}

    //===
    property ModCheck : single read fModCheck write fModCheck; //WTF does this do?
    //===

    property StepSeq1ClockSource : TClockSource read fStepSeq1ClockSource write fStepSeq1ClockSource;
    property StepSeq2ClockSource : TClockSource read fStepSeq2ClockSource write fStepSeq2ClockSource;

  end;

implementation

uses
  SysUtils;

type
  EModMatrixException = Exception;

{ TPrivateModLink }

procedure TPrivateModLink.AssignFrom(const aSource: TModLink_OLD);
begin
  self.UniqueID := aSource.UniqueID;
  self.Source   := aSource.Source;
  self.Dest     := aSource.Dest;
  self.Via      := aSource.Via;
  self.Amount   := aSource.Amount;
  self.Offset   := aSource.Offset;
end;


{ TModMatrix }

constructor TModMatrix.Create;
var
  c1: Integer;
begin
  //==== Modulation Sources and Destinations =====
  ModSourceCount := TModSourceHelper.GetEnumTypeCount;
  ModDestCount   := TModDestHelper.GetEnumTypeCount;

  SetLength(fModSourceValues, TModSourceHelper.GetEnumTypeCount);
  SetLength(fModDestValues, TModDestHelper.GetEnumTypeCount);

  // NOTE:
  // Point all the mod source/dest pointers to a fake location. This is a convient
  // safe guard during initial development to avoid potential AV errors. It won't
  // be needed closer to the finished release.
  FakeDest := 0;
  FakeSource := 0;

  for c1 := 0 to ModSourceCount-1 do
  begin
    fModSourceValues[c1] := @FakeSource;
  end;

  for c1 := 0 to ModDestCount-1 do
  begin
    fModDestValues[c1] := @FakeDest;
  end;
end;

destructor TModMatrix.Destroy;
begin
  SetLength(fModSourceValues, 0);
  SetLength(fModDestValues, 0);

  SetLength(FastModulations, 0);
  SetLength(SlowModulations, 0);

  inherited;
end;

function TModMatrix.GetModSourceValue(const aModSource: TModSource): single;
begin
  result := fModSourceValues[Integer(aModSource)]^;
end;

procedure TModMatrix.SetModDestPointer(const aModDest: TModDest; const Dest: PSingle);
begin
  fModDestValues[Integer(aModDest)] := Dest;
end;

procedure TModMatrix.SetModSourcePointer(const aModSource: TModSource; const Source: PSingle);
begin
  fModSourceValues[Integer(aModSource)] := Source;
end;

procedure TModMatrix.ZeroAllValues;
var
  c1 : integer;
begin
  for c1 := 0 to ModSourceCount-1 do
  begin
    fModSourceValues[c1]^ := 0;
  end;

  for c1 := 0 to ModDestCount-1 do
  begin
    fModDestValues[c1]^ := 0;
  end;
end;


procedure TModMatrix.CalcModOutput(const aModLink: PPrivateModLink);
var
  sx : PSingle;
  vx : PSingle;
  dx : PSingle;
begin

  //if (aModLink.Source = TModSource.None) or (aModLink.Dest = TModDest.None) or (aModLink.Amount = 0) then exit;

  if aModLink^.Via = TModSource.None then
  begin
    sx := aModLink^.PSource;
    dx := aModLink^.PDest;
    dx^ := dx^ + (sx^ + aModLink^.Offset) * aModLink^.Amount;
  end else
  begin
    sx := aModLink^.PSource;
    dx := aModLink^.PDest;
    vx := aModLink^.PVia;
    dx^ := dx^ + (sx^ + aModLink^.Offset) * (aModLink^.Amount * vx^);
  end;
end;



procedure TModMatrix.FastControlProcess;
var
  c1 : integer;
begin
  {$IFDEF StrictDebugChecks}
  AreModSourceValuesInRange; //Expensive!!
  {$ENDIF}

  for c1 := 0 to FastModulationCount-1 do
  begin
    FastModulations[c1].PDest^ := 0;
  end;

  for c1 := 0 to FastModulationCount-1 do
  begin
    CalcModOutput(@FastModulations[c1]);
  end;

  //NOTE: important: FastControlProcess should always be called before slow control rate process.
end;


procedure TModMatrix.SlowControlProcess;
var
  c1 : integer;
begin
  for c1 := 0 to SlowModulationCount-1 do
  begin
    SlowModulations[c1].PDest^ := 0;
  end;

  for c1 := 0 to SlowModulationCount-1 do
  begin
    CalcModOutput(@SlowModulations[c1]);
  end;
end;

procedure TModMatrix.UpdateAllModLinks(const aModConnections: TModConnections_OLD);
var
  c1: Integer;
  Link : PModLink_OLD;
  PrivateLink : PPrivateModLink;
begin
  //TODO: this is an excessive use of memory. It's unlikely that
  // FastModulations will ever need to be close to the same size a aModConnetions.ModLink
  SetLength(FastModulations, aModConnections.ModLinkCount);
  SetLength(SlowModulations, aModConnections.ModLinkCount);

  //TODO: Currently UpadeModConnections operates on live data. (It is being used
  // by an active voice. It might be necessary to buffer the data and swap buffers
  // when the new buffer is ready.

  //Zero all mod destinations before updating internal links.
  for c1 := 0 to ModDestCount-1 do
  begin
    fModDestValues[c1]^ := 0;
  end;

  FastModulationCount := 0;
  SlowModulationCount := 0;

  // sort the variable modulations...
  for c1 := 0 to aModConnections.ModLinkCount-1 do
  begin
    //Link := @ModLinkArray[c1];
    Link := aModConnections.ModLinks[c1];
    case GetModLinkState(Link) of
      TModLinkState.Inactive:
      begin
        //do nothing
      end;

      TModLinkState.FastMod:
      begin
        FastModulations[FastModulationCount].AssignFrom(Link^);
        inc(FastModulationCount);
      end;

      TModLinkState.SlowMod:
      begin
        SlowModulations[SlowModulationCount].AssignFrom(Link^);
        inc(SlowModulationCount);
      end;
    else
      raise Exception.Create('Mod link state not handled.');
    end;
  end;


  //=== Now pre-compute the some modulation variables ====
  for c1 := 0 to FastModulationCount-1 do
  begin
    PrivateLink := @FastModulations[c1];
    FastModulations[c1].PSource := fModSourceValues[Integer(PrivateLink^.Source)];
    FastModulations[c1].PDest   := fModDestValues[Integer(PrivateLink^.Dest)];
    FastModulations[c1].PVia    := fModSourceValues[Integer(PrivateLink^.Via)];
    FastModulations[c1].Offset  := CalcModOffset(PrivateLink);
  end;

  for c1 := 0 to SlowModulationCount-1 do
  begin
    PrivateLink := @SlowModulations[c1];
    SlowModulations[c1].PSource := fModSourceValues[Integer(PrivateLink^.Source)];
    SlowModulations[c1].PDest   := fModDestValues[Integer(PrivateLink^.Dest)];
    SlowModulations[c1].PVia    := fModSourceValues[Integer(PrivateLink^.Via)];
    SlowModulations[c1].Offset  := CalcModOffset(PrivateLink);
  end;

end;

procedure TModMatrix.UpdateModLink(const ModLinkData: PModLink_OLD);
var
  c1 : integer;
begin
  // NOTE: Update modlink only updates the MOD depth.
  // if the mod source, dest or via has changed, then a full update
  // is required.
  for c1 := 0 to FastModulationCount-1 do
  begin
    if (FastModulations[c1].UniqueID = ModLinkData^.UniqueID) then
    begin
      FastModulations[c1].Amount := ModLinkData^.Amount;
      assert(FastModulations[c1].Dest   = ModLinkData^.Dest);
      assert(FastModulations[c1].Via    = ModLinkData^.Via);
      assert(FastModulations[c1].Source = ModLinkData^.Source);
    end;
  end;

  for c1 := 0 to SlowModulationCount-1 do
  begin
    if (SlowModulations[c1].UniqueID = ModLinkData^.UniqueID) then
    begin
      SlowModulations[c1].Amount := ModLinkData^.Amount;
      assert(SlowModulations[c1].Dest   = ModLinkData^.Dest);
      assert(SlowModulations[c1].Via    = ModLinkData^.Via);
      assert(SlowModulations[c1].Source = ModLinkData^.Source);
    end;
  end;
end;

function TModMatrix.GetModLinkState(aModLink: PModLink_OLD): TModLinkState;
begin
  if (aModLink^.Source = TModSource.None) or (aModLink^.Dest = TModDest.None) then
  begin
    exit(TModLinkState.Inactive);
  end;

  case aModLink^.Dest of
    TModDest.VoiceAmplitude,
    TModDest.VoicePan,
    TModDest.Filter1_Par1,
    TModDest.Filter1_Par2,
    TModDest.Filter1_Par3,
    TModDest.Filter1_Par4,
    TModDest.Filter2_Par1,
    TModDest.Filter2_Par2,
    TModDest.Filter2_Par3,
    TModDest.Filter2_Par4:
    begin
      exit(TModLinkState.FastMod);
    end;

    TModDest.ModOutA,
    TModDest.ModOutB,
    TModDest.SampleStart,
    TModDest.SampleEnd,
    TModDest.LoopStart,
    TModDest.LoopEnd,
    TModDest.Lfo1_Rate,
    TModDest.Lfo1_ParB,
    TModDest.Lfo2_Rate,
    TModDest.Lfo2_ParB:
    begin
      exit(TModLinkState.SlowMod);
    end;

  else
    raise Exception.Create('Mod link dest not handled. (error: 129)');
  end;



end;

function TModMatrix.AreModSourceValuesInRange: boolean;
var
  c1: Integer;
  x : single;
  s : string;
begin

  for c1 := 0 to ModSourceCount-1 do
  begin
    //TODO: currently TModSource.Midi_Note is out of range. this check
    // will need to be updated once the Midi_note range is updated.
    if TModSourceHelper.ToEnum(c1) <> TModsource.Midi_Note then
    begin
      x := fModSourceValues[c1]^;
      if InRange(x, 0, 1) = false then
      begin
        s := TModSourceHelper.ToString(c1);
        raise Exception.Create(s + ' is out of range.');
      end;
    end;
  end;

  result := true;
end;

function TModMatrix.CalcModOffset(aModLink: PPrivateModLink): single;
const
  //We use a slightly different center point when using MIDI note as a bi-polar signal source.
  kMidiNoteOffset = 0.5 + (36 / 12);
var
  ModSource : TModSource;
  ModDest   : TModDest;
  Offset : single;
begin
  ModSource := aModLink^.Source;

  case ModSource of
    // special sources.
    TModSource.None:      Offset := 0;
    //TModSource.Midi_Note: Offset := -kMidiNoteOffset;
    TModSource.Midi_Note: Offset := 0;
    // envelopes.
    TModSource.AmpEnv:    Offset := 0;
    TModSource.FilterEnv: Offset := 0;
    TModSource.ModEnv1:   Offset := 0;
    TModSource.ModEnv2:   Offset := 0;
    // bi-polar mod sources.
    TModSource.Midi_PitchBend:  Offset := -0.5;
    TModSource.Midi_ModWheel:   Offset := -0.5;
    TModSource.Lfo1:            Offset := -0.5;
    TModSource.Lfo2:            Offset := -0.5;
    TModSource.StepSeq1:        Offset := -0.5;
    TModSource.StepSeq2:        Offset := -0.5;
    TModSource.PadX1:           Offset := -0.5;
    TModSource.PadY1:           Offset := -0.5;
    TModSource.PadX2:           Offset := -0.5;
    TModSource.PadY2:           Offset := -0.5;
    TModSource.PadX3:           Offset := -0.5;
    TModSource.PadY3:           Offset := -0.5;
    TModSource.PadX4:           Offset := -0.5;
    TModSource.PadY4:           Offset := -0.5;
  else
    raise Exception.Create('type not handled.');
  end;


  // TODO: I think exceptions will need to be made here
  // for sample point modulation.



  // Override the source offset when the destination is a sample point marker.
  // Using bi-polar modulation for sample markers makes the interface confusing.
  // I think's it's better the the sample start/end markers, and loop start/end
  // markers always represent an 'extreme value'. Modulation moves the
  // marker in one direction.
  ModDest := aModLink^.Dest;
  case ModDest of
    TModDest.SampleStart: Offset := 0;
    TModDest.SampleEnd:   Offset := 0;
    TModDest.LoopStart:   Offset := 0;
    TModDest.LoopEnd:     Offset := 0;
  end;

  result := Offset;


end;






end.




