unit soModMatrix;

interface

{$INCLUDE Defines.inc}

uses
  VamLib.Utils,
  VamLib.MoreTypes, LucidityModConnections,
  Lucidity.Types,
  uConstants, uLucidityEnums,
  SoundObjectTypes;

  {$SCOPEDENUMS ON}

type
  TModSourceValues = array of PSingle;
  TModDestValues   = array of PSingle;


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

    FastModulationCount : integer;
    SlowModulationCount : integer;
    NoModulationCount   : integer;

    FastModulationIndexes : array[0..kModulatedParameterCount-1] of integer;
    SlowModulationIndexes : array[0..kModulatedParameterCount-1] of integer;
    NoModulationIndexes   : array[0..kModulatedParameterCount-1] of integer;

    ParValueData  : PModulatedPars;
    ParModData    : PParModulationData;
    ModConnections: PModConnections;

    ModSlotValues : array[0..kModSlotCount-1] of single;
    ModSlotSourcePointers : array[0..kModSlotCount-1] of psingle;
    ModSlotViaPointers    : array[0..kModSlotCount-1] of psingle;

    function GetModLinkState(aModLink : PModLink_OLD):TModLinkState;
    function AreModSourceValuesInRange:boolean;

    function IsParameterModulated(ModParIndex : integer):boolean;
    function DoesParameterRequireFastModulation(ModParIndex : integer):boolean;
  protected

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



//TODO: need to write x64 bit asm version of this function.
function CalcSummedModulationValue(ModSlotValues, ModAmountValues : PSingle):single;
{$IF Defined(UseASM) and Defined(CPUX64)}
asm
  movups XMM0,[rcx]
  movups XMM1,[rcx+$10]

  movups XMM2,[rdx]
  movups XMM3,[rdx+$10]


  mulps XMM0, XMM2
  mulps XMM1, XMM3

  addps XMM0, XMM1

  sub esp, 16
  movdqu dqword [esp], xmm0

  movss xmm0, [esp]
  addss xmm0, [[esp+$04]]
  addss xmm0, [[esp+$08]]
  addss xmm0, [[esp+$0c]]

  add esp, 16
end;
{$ELSEIF Defined(UseASM) and Defined(CPUX86)}
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
{$ELSE}
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
{$IFEND}


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


procedure TModMatrix.FastControlProcess;
var
  c1 : integer;
  Index : integer;
  pv, combinedModValues : single;
begin
  {$IFDEF StrictDebugChecks}
  AreModSourceValuesInRange; //Expensive!!
  {$ENDIF}

  // Update the Mod Slot Values
  for c1 := 0 to kModSlotCount - 1 do
  begin
    ModSlotValues[c1] := ModSlotSourcePointers[c1]^ * ModSlotViaPointers[c1]^;
  end;

  // calc the modulation for each modulated parameter.
  for c1 := 0 to FastModulationCount-1 do
  begin
    Index := FastModulationIndexes[c1];
    ParModData^[Index] := CalcSummedModulationValue(@ModSlotValues[0], @ParValueData^[Index].ModAmount[0]);

    pv := ParValueData^[Index].ParValue;
    combinedModValues := ParModData^[Index];
    ParValueData^[Index].ModulatedParValue := Clamp(pv + CombinedModValues, 0, 1);
  end;
end;

procedure TModMatrix.SlowControlProcess;
var
  c1 : integer;
  Index : integer;
  pv, combinedModValues : single;
begin
  // calc the modulation for each modulated parameter.
  for c1 := 0 to SlowModulationCount-1 do
  begin
    Index := SlowModulationIndexes[c1];
    ParModData^[Index] := CalcSummedModulationValue(@ModSlotValues[0], @ParValueData^[Index].ModAmount[0]);

    pv := ParValueData^[Index].ParValue;
    combinedModValues := ParModData^[Index];
    ParValueData^[Index].ModulatedParValue := Clamp(pv + CombinedModValues, 0, 1);
  end;

  for c1 := 0 to NoModulationCount-1 do
  begin
    Index := NoModulationIndexes[c1];
    ParValueData^[Index].ModulatedParValue := ParValueData^[Index].ParValue;
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
        s := TModSourceHelper.ToUnicodeString(c1);
        raise Exception.Create(s + ' is out of range.');
      end;
    end;
  end;

  result := true;
end;


procedure TModMatrix.UpdateModConnections;
var
  c1: Integer;
  aModSource : TModSource;
  IsMute : boolean;
  Index : integer;
begin

  // Update mod source pointers...
  for c1 := 0 to kModSlotCount-1 do
  begin
    IsMute := self.ModConnections^.GetModMute(c1);

    aModSource := self.ModConnections^.GetModSource(c1);
    if (aModSource <> TModSource.None) and (IsMute = false)
      then ModSlotSourcePointers[c1] := fModSourceValues[Integer(aModSource)]
      else ModSlotSourcePointers[c1] := @SourceNoneValue;

    aModSource := self.ModConnections^.GetModVia(c1);
    if (aModSource <> TModSource.None) and (IsMute = false)
      then ModSlotViaPointers[c1] := fModSourceValues[Integer(aModSource)]
      else ModSlotViaPointers[c1] := @ViaNoneValue;
  end;



  // update modulated parameters
  FastModulationCount := 0;
  SlowModulationCount := 0;
  NoModulationCount   := 0;

  for c1 := 0 to kModulatedParameterCount-1 do
  begin
    if IsParameterModulated(c1) then
    begin
      if DoesParameterRequireFastModulation(c1) then
      begin
        Index := FastModulationCount;
        FastModulationIndexes[Index] := c1;
        inc(FastModulationCount);
      end else
      begin
        Index := SlowModulationCount;
        SlowModulationIndexes[Index] := c1;
        inc(SlowModulationCount);
      end;
    end else
    begin
      Index := NoModulationCount;
      NoModulationIndexes[Index] := c1;
      inc(NoModulationCount);
    end;
  end;
end;

function TModMatrix.IsParameterModulated(ModParIndex: integer): boolean;
var
  TotalModDepth : single;
  c1: Integer;
begin
  TotalModDepth := 0;
  for c1 := 0 to kModSlotCount-1 do
  begin
    TotalModDepth := TotalModDepth + abs(ParValueData^[ModParIndex].ModAmount[c1])
  end;

  if TotalModDepth = 0
    then result := false
    else result := true;

end;

function TModMatrix.DoesParameterRequireFastModulation(ModParIndex: integer): boolean;
begin
  case ModParIndex of
    TModParIndex.OutputGain,
    TModParIndex.OutputPan,
    TModParIndex.VoicePitchOne,
    TModParIndex.VoicePitchTwo,
    TModParIndex.Filter1Par1,
    TModParIndex.Filter1Par2,
    TModParIndex.Filter1Par3,
    TModParIndex.Filter1Par4,
    TModParIndex.Filter2Par1,
    TModParIndex.Filter2Par2,
    TModParIndex.Filter2Par3,
    TModParIndex.Filter2Par4: result := true
  else
    result := false;
  end;
end;




end.




