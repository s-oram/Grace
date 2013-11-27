unit eeMidiInputSmoother;

interface

uses
  Contnrs, Generics.Collections, B2.MovingAverageFilter;

type
  TValueChangeMethod = procedure(CCIndex : integer; CCValue:single) of object;

  TSmoothy = class
  private
    fParIndex: integer;
    fTarget: single;
    fIsActive: boolean;
    procedure SetTarget(const Value: single);
  protected
    //TODO: Consider using a critically damped filter instead of the moving average filter.
    // - advantages
    //   - maybe similar CPU usage? maybe less?
    //   - could maybe use a dynamic smoothing time for smoothing between
    //     similar values, and use a faster smoothing time for large value jumps.
    Filter : TMovingAverageFilter;
    fSmoothingBufferSize : integer;
    StepCount : integer;
  public
    constructor Create(const aSmoothingBufferSize : integer);
    destructor Destroy; override;

    function Step:single;

    property IsActive : boolean read fIsActive;

    procedure Reset(Value : single);
    property ParIndex : integer read fParIndex write fParIndex;
    property Target   : single  read fTarget   write SetTarget;
  end;




  // TODO: Replace the TObjectList based list class
  // with a custom fast array list class. As of yet
  // the "fast array list" is un-written but I think it would
  // be a useful addition to the EasyEffect template classes.
  TSmoothyList = class(TObjectList<TSmoothy>)
  private
  public
    function FindByParIndex(aParIndex : integer):TSmoothy;
  end;


  TMidiInputSmoother = class
  private
    List : TSmoothyList;
    CurrentMIDIValue : array[0..127] of single;
    fSmoothingBuffersize: integer;
    procedure SetSmoothingBufferSize(const Value: integer); //midi CC Values are stored as 0..1 ranged floats.
  public
    constructor Create;
    destructor Destroy; override;

    procedure MidiInput(const CCIndex : integer; const CCValue:single);

    procedure Step(const StepCallBack : TValueChangeMethod);

    property SmoothingBufferSize : integer read fSmoothingBufferSize write SetSmoothingBufferSize;

  end;

implementation

{ TInputSmoother }

constructor TMidiInputSmoother.Create;
var
  c1: Integer;
begin
  List := TSmoothyList.Create(true);

  for c1 := 0 to 127 do
  begin
    CurrentMidiValue[c1] := 0.5;
  end;

  SmoothingBufferSize := 128;
end;

destructor TMidiInputSmoother.Destroy;
begin
  List.Free;
  inherited;
end;

procedure TMidiInputSmoother.MidiInput(const CCIndex: integer; const CCValue: single);
var
  aSmoothy : TSmoothy;
begin
  aSmoothy := List.FindByParIndex(CCIndex);

  if not assigned(aSmoothy) then
  begin
    // TODO: instead of constanting creating and free'ing smoothies, consider
    // using an object cache so smoothy instances can be recycled.
    aSmoothy := TSmoothy.Create(SmoothingBufferSize);
    aSmoothy.ParIndex := CCIndex;
    aSmoothy.Reset(CurrentMidiValue[CCIndex]);
    List.Add(aSmoothy);
  end;

  aSmoothy.Target := CCValue;
end;

procedure TMidiInputSmoother.SetSmoothingBufferSize(const Value: integer);
begin
  if Value <> fSmoothingBufferSize then
  begin
    fSmoothingBufferSize := Value;
    List.Clear;
  end;
end;

procedure TMidiInputSmoother.Step(const StepCallBack : TValueChangeMethod);
var
  c1: Integer;
  CCIndex : integer;
  CCValue : single;
begin
  //TODO: reverse list and remove inactive smoothers.
  for c1 := 0 to List.Count-1 do
  begin
    if List[c1].IsActive then
    begin
      CCIndex := List[c1].ParIndex;
      CCValue := List[c1].Step;
      CurrentMidiValue[CCIndex] := CCValue;
      StepCallBack(CCIndex, CCValue);

      // TODO: a possible optimisation here might be to remove inactive 'smoothies' from
      // the list (and re-create them when the smoothy is required again).
    end;
  end;
end;

{ TSmoothy }

constructor TSmoothy.Create(const aSmoothingBufferSize : integer);
begin
  fSmoothingBufferSize := aSmoothingBufferSize;

  Filter := TMovingAverageFilter.Create;
  Filter.BufferSize := aSmoothingBufferSize;
end;

destructor TSmoothy.Destroy;
begin
  Filter.Free;
  inherited;
end;

procedure TSmoothy.Reset(Value: single);
begin
  Filter.Reset(Value);
end;

procedure TSmoothy.SetTarget(const Value: single);
begin
  fIsActive := true;
  StepCount := 0;
  fTarget := Value;
end;

function TSmoothy.Step: single;
begin
  inc(StepCount);
  if StepCount > fSmoothingBufferSize then
  begin
    fIsActive := false;
  end;

  result := Filter.Step(fTarget);
end;

{ TSmoothyList }

function TSmoothyList.FindByParIndex(aParIndex: integer): TSmoothy;
var
  c1: Integer;
begin
  for c1 := 0 to self.Count-1 do
  begin
    if Items[c1].ParIndex = aParIndex then
    begin
      result := Items[c1];
      exit;
    end;
  end;

  // no matching item found.
  result := nil;
end;

end.
