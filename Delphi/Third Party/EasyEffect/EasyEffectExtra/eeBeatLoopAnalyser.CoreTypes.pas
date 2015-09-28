unit eeBeatLoopAnalyser.CoreTypes;

interface

uses
  Generics.Collections, MtxVec;



type
  PTransient = ^TTransient;
  TTransient = record
    Position : integer;  //sample position in the source.
    Strength : single;   // range 0..1.
  end;

  TTransientList = class(TList<TTransient>)
  protected
    function NextTransient(CurIndex: integer; const Env1, Env2 : TVec; const SampleFrames : integer):integer;
    function AdvanceIndex(CurIndex: integer; const Env1, Env2 : TVec; const SampleFrames : integer):integer;
    function CalculateTransientStrength(TransientPosition : integer; const Env1, Env2 : TVec; const SampleFrames : integer):single;
  public
    procedure FindTransients(Env1, Env2 : TVec);

    procedure SortByTransientPosition;
  end;

  TSliceInfo = record
    Position      : integer; //The start position of the slice.
    SampleFrames  : integer; //The total slice length
  end;

  TSliceList = class(TList<TSliceInfo>)
  public
    procedure ImportFrom(T:TTransientList; SourceSampleFrames:integer);
    procedure UpdateSliceLengths(SourceSampleFrames : integer);
  end;





implementation

uses
  Math, VamLib.Utils, eeFilters.EnvFollowerA, eeDsp,
  MtxVecHelper, SignalUtils;



{ TTransientList }

function TTransientList.NextTransient(CurIndex: integer; const Env1, Env2 : TVec; const SampleFrames : integer):integer;
var
  ex1, ex2 : single;
  IsTransientFound : boolean;
begin
  // Look for transient....
  IsTransientFound := false;

  while (IsTransientFound = false) and (CurIndex < SampleFrames) do
  begin
    ex1 := Env1[CurIndex];
    ex2 := Env2[CurIndex];

    if ex1 > ex2
      then IsTransientFound := true
      else inc(CurIndex);
  end;

  // Return results...
  if IsTransientFound
    then result := CurIndex
    else result := -1;
end;

function TTransientList.AdvanceIndex(CurIndex: integer; const Env1, Env2: TVec; const SampleFrames: integer): integer;
var
  ex1, ex2 : single;
  IsUnder : boolean;
begin
  IsUnder := false;

  while (IsUnder = false) and (CurIndex < SampleFrames) do
  begin
    ex1 := Env1[CurIndex];
    ex2 := Env2[CurIndex];

    if ex1 < ex2
      then IsUnder := true
      else inc(CurIndex);
  end;

  result := CurIndex;
end;

function TTransientList.CalculateTransientStrength(TransientPosition: integer; const Env1, Env2: TVec; const SampleFrames: integer): single;
var
  ex1, ex2 : single;
  IsUnder : boolean;
  LowPoint, HighPoint : single;
  EndTransientPosition : integer;
  c1: Integer;
begin
  LowPoint  := Env2[TransientPosition];
  HighPoint := Env1[TransientPosition];

  EndTransientPosition := AdvanceIndex(TransientPosition, Env1, Env2, SampleFrames);

  for c1 := TransientPosition to EndTransientPosition-1 do
  begin
    if Env1[c1] > HighPoint then HighPoint := Env1[c1];
  end;

  result := HighPoint - LowPoint;
end;


procedure TTransientList.FindTransients(Env1, Env2: TVec);
var
  CurIndex : integer;
  SampleFrames : integer;
  aTransient   : TTransient;
  TrPos        : Integer;
  IsSearching  : boolean;
begin
  assert(Env1.Length = Env2.Length);
  assert(Env1.IsComplex = false);
  assert(Env2.IsComplex = false);

  CurIndex := 0;
  SampleFrames := Env1.Length;
  IsSearching  := true;

  TrPos := NextTransient(CurIndex, Env1, Env2, SampleFrames);
  while TrPos <> -1 do
  begin
    //Transient found...
    aTransient.Position := TrPos;
    aTransient.Strength := CalculateTransientStrength(TrPos, Env1, Env2, SampleFrames);
    self.Add(aTransient);

    //Look for next transient...
    CurIndex := TrPos;
    CurIndex := AdvanceIndex(CurIndex, Env1, Env2, SampleFrames);
    TrPos    := NextTransient(CurIndex, Env1, Env2, SampleFrames);
  end;


end;

procedure TTransientList.SortByTransientPosition;
var
  Index : integer;
begin
  // Sort by transient position using a very basic sorting algo.
  Index := 0;
  while Index < Count - 1 do
  begin
    if self[Index].Position < self[Index+1].Position then
    begin
      inc(Index);
    end else
    begin
      self.Exchange(Index, Index+1);
      if Index > 0 then dec(Index);
    end;
  end;
end;




{ TSliceList }

procedure TSliceList.ImportFrom(T: TTransientList; SourceSampleFrames:integer);
var
  c1: Integer;
  aSlice : TSliceInfo;
  sf     : integer;
begin
  self.Clear;

  T.SortByTransientPosition;

  sf := SourceSampleFrames-1;

  for c1 := T.Count-1 downto 0 do
  begin
    aSlice.Position := T[c1].Position;
    aSlice.SampleFrames := sf - aSlice.Position;
    sf := sf - aSlice.SampleFrames;

    self.Add(aSlice);
  end;
end;

procedure TSliceList.UpdateSliceLengths(SourceSampleFrames : integer);
var
  sf : integer;
  c1: Integer;
  aSlice:TSliceInfo;
  Index : integer;
begin
  sf := SourceSampleFrames-1;

  //Update last slice.
  if Count > 0 then
  begin
    Index := Count-1;
    aSlice := Self[Index];
    aSlice.SampleFrames := SourceSampleFrames - aSlice.Position;
    Self[Index] := aSlice;
  end;

  for c1 := Count-2 downto 0 do
  begin
    aSlice := Self[c1];
    aSlice.SampleFrames := Self[c1+1].Position - aSlice.Position;
    Self[c1] := aSlice;
  end;
end;

end.
