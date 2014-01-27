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

    SourceNoneValue : single;
    ViaNoneValue    : single;
  protected
    type
      TModLinkState = (Inactive, FastMod, SlowMod);
    var
    FakeDest, FakeSource : single;


    fModSourceValues : TModSourceValues;
    fModDestValues_OLD   : TModDestValues;

    ModSourceCount : integer;
    ModDestCount   : integer;

    FastModulations : array of TPrivateModLink;
    SlowModulations : array of TPrivateModLink;

    FastModulationCount : integer;
    SlowModulationCount : integer;

    ParValueData: PModulatedPars;
    ParModData: PParModulationData;
    ModConnections: PModConnections;

    ModSlotValues : array[0..kModSlotCount-1] of single;
    ModSlotSourcePointers : array[0..kModSlotCount-1] of psingle;
    ModSlotViaPointers    : array[0..kModSlotCount-1] of psingle;

    function GetModLinkState(aModLink : PModLink_OLD):TModLinkState;
    procedure CalcModOutput(const aModLink : PPrivateModLink); {$IFDEF AudioInline}inline;{$ENDIF}
    function CalcModOffset(aModLink : PPrivateModLink): single;
    function AreModSourceValuesInRange:boolean;

  protected
    procedure FastControlProcess_PartA;
    procedure FastControlProcess_PartB;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(const aParValueData : PModulatedPars; const aParModData : PParModulationData; const aModConnections : PModConnections);

    procedure UpdateModConnections;

    //==========================================================================
    // NOTE: These methods are here for debugging purposes.
    property ModSourceValues : TModSourceValues read fModSourceValues;
    property ModDestValues_OLD   : TModDestValues   read fModDestValues_OLD;   //TODO: Delete
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

function CalcSummedModulationValue(ModSlotValues, ModAmountValues : PSingle):single;
var
  x : single;
  c2: Integer;
begin
  //TODO: this probably can be optimised. It would be a good candiate for a SSE vector operation.
  x := 0;
  for c2 := 0 to kModSlotCount-1 do
  begin
    x := x + ModSlotValues^ * ModAmountValues^;
    inc(ModSlotValues);
    inc(ModAmountValues);
  end;
  result := x;
end;

//TODO: need to write x64 bit asm version of this function.
function CalcSummedModulationValue_asm2(ModSlotValues, ModAmountValues : PSingle):single;
asm
  movups XMM0,[eax]     //Move 8 ModSlotValues to first 2 sse registers.
  movups XMM1,[eax+$10]

  movups XMM2,[edx]     //Move 8 ModAmountValues to the next 2 sse registers.
  movups XMM3,[edx+$10]

  mulps XMM0, XMM2      // multiply the 8 ModSlotValues with their respective ModAmountValues
  mulps XMM1, XMM3

  addps XMM0, XMM1     // sum the results so we have 4 single values.

  //TODO: there may be an oppertunity for
  // further optisation here by using an horizontal add function.

  sub esp, 16
  movdqu dqword [esp], xmm0

  //sum the 4 single values so we can finish with 1 number for the result.

  fld dword [[esp]]
  fadd dword [[esp+$04]]
  fadd dword [[esp+$08]]
  fadd dword [[esp+$0C]]

  add esp, 16
end;

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
  SourceNoneValue := 0;
  ViaNoneValue := 1;

  //==== Modulation Sources and Destinations =====
  ModSourceCount := TModSourceHelper.GetEnumTypeCount;
  ModDestCount   := TModDestHelper.GetEnumTypeCount;

  SetLength(fModSourceValues, TModSourceHelper.GetEnumTypeCount);
  SetLength(fModDestValues_OLD, TModDestHelper.GetEnumTypeCount);

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
    fModDestValues_OLD[c1] := @FakeDest;
  end;
end;

destructor TModMatrix.Destroy;
begin
  SetLength(fModSourceValues, 0);
  SetLength(fModDestValues_OLD, 0);

  SetLength(FastModulations, 0);
  SetLength(SlowModulations, 0);

  inherited;
end;

procedure TModMatrix.Init(const aParValueData: PModulatedPars; const aParModData: PParModulationData; const aModConnections: PModConnections);
var
  c1 : integer;
begin
  ParValueData   := aParValueData;
  ParModData     := aParModData;
  ModConnections := aModConnections;

  // Zero all modulation points.
  for c1 := 0 to kModulatedParameterCount-1 do
  begin
    ParModData^[c1] := 0;
  end;

end;

procedure TModMatrix.SetModDestPointer(const aModDest: TModDest; const Dest: PSingle);
begin
  fModDestValues_OLD[Integer(aModDest)] := Dest;
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
    fModDestValues_OLD[c1]^ := 0;
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
  y : single;
begin
  {$IFDEF StrictDebugChecks}
  AreModSourceValuesInRange; //Expensive!!
  {$ENDIF}


  //TODO: At the moment all modulations are updated at the FastControlRate.
  // see if some modulations can be updated at slow control rate.
  // TODO: use some assembly.
  FastControlProcess_PartA;
  FastControlProcess_PartB;








end;


procedure TModMatrix.FastControlProcess_PartA;
var
  c1 : integer;
  y : single;
begin
  // Update the Mod Slot Values
  for c1 := 0 to kModSlotCount - 1 do
  begin
    ModSlotValues[c1] := ModSlotSourcePointers[c1]^ * ModSlotViaPointers[c1]^;
  end;
end;

procedure TModMatrix.FastControlProcess_PartB;
var
  c1 : integer;
  y : single;
begin
  //TODO: plenty of options for optimisation here.
  // - is the parameter being modulated?
  // - is the parameter a fast or slow target?
  // +++ make sure to bench mark +++
  for c1 := 0 to kModulatedParameterCount-1 do
  begin
    //ParModData^[c1] := CalcSummedModulationValue(@ModSlotValues[0], @ParValueData^[c1].ModAmount[0]);
    ParModData^[c1] := CalcSummedModulationValue_asm2(@ModSlotValues[0], @ParValueData^[c1].ModAmount[0]);
  end;
end;




procedure TModMatrix.SlowControlProcess;
var
  c1 : integer;
begin
  {
  for c1 := 0 to SlowModulationCount-1 do
  begin
    SlowModulations[c1].PDest^ := 0;
  end;

  for c1 := 0 to SlowModulationCount-1 do
  begin
    CalcModOutput(@SlowModulations[c1]);
  end;
  }
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
    // bi-polar mod sources.
    TModSource.Midi_PitchBend:  Offset := -0.5;
    TModSource.Midi_ModWheel:   Offset := -0.5;
    TModSource.Lfo1:            Offset := -0.5;
    TModSource.Lfo2:            Offset := -0.5;
    TModSource.StepSeq1:        Offset := -0.5;
    TModSource.StepSeq2:        Offset := -0.5;
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



procedure TModMatrix.UpdateModConnections;
var
  c1: Integer;
  aModSource : TModSource;
begin
  for c1 := 0 to kModSlotCount-1 do
  begin
    aModSource := self.ModConnections^.GetModSource(c1);
    if aModSource <> TModSource.None
      then ModSlotSourcePointers[c1] := fModSourceValues[Integer(aModSource)]
      else ModSlotSourcePointers[c1] := @SourceNoneValue;

    aModSource := self.ModConnections^.GetModSource(c1);
    if aModSource <> TModSource.None
      then ModSlotViaPointers[c1] := fModSourceValues[Integer(aModSource)]
      else ModSlotViaPointers[c1] := @ViaNoneValue;
  end;

end;




end.




