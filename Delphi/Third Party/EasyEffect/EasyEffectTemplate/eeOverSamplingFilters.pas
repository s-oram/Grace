unit eeOverSamplingFilters;

interface

uses
  MoreTypes, SignalTools, SignalProcessing;

type
  TInputFilter = class
  private
    procedure SetBufferSize(const Value: integer);
    function GetInterpolationFactor: integer;
    procedure SetInterpolationFactor(const Value: integer);
  protected
    Signal:TSignal;
    SignalInterpolator : TSignalInterpolator;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDataPointer:PSingle;
    procedure Process(InBuffer:PSingle; InputSampleFrames:integer);
    property InterpolationFactor : integer read GetInterpolationFactor write SetInterpolationFactor;
    property BufferSize : integer write SetBufferSize;
  end;

  TOutputFilter = class
  private
    procedure SetBufferSize(const Value: integer);
    function GetDecimationFactor: integer;
    procedure SetDecimationFactor(const Value: integer);
  protected
    Signal:TSignal;
    SignalDecimator : TSignalDecimator;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDataPointer:PSingle;
    procedure ZeroBuffer;
    procedure Process(OutBuffer:PSingle; InputSampleFrames:integer);
    property DecimationFactor : integer read GetDecimationFactor write SetDecimationFactor;
    property BufferSize : integer write SetBufferSize;
  end;


  TArrayOfInputFilter  = array of TInputFilter;
  TArrayOfOutputFilter = array of TOutputFilter;

  TInputFilters = class
  private
    fFilters: TArrayOfInputFilter;
    fFilterCount: integer;
    procedure SetFilterCount(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    property Filters : TArrayOfInputFilter read fFilters write fFilters;
    property FilterCount : integer read fFilterCount write SetFilterCount;
  end;

  TOutputFilters = class
  private
    fFilters: TArrayOfOutputFilter;
    fFilterCount: integer;
    procedure SetFilterCount(const Value: integer);
  public
    constructor Create;
    destructor Destroy; override;

    property Filters : TArrayOfOutputFilter read fFilters write fFilters;
    property FilterCount : integer read fFilterCount write SetFilterCount;
  end;

implementation

//=========================================================================================================================
//=========================================================================================================================
//=========================================================================================================================



{ TInputFilter }

constructor TInputFilter.Create;
begin
  Signal := TSignal.Create(nil);
  Signal.Complex := false;
  Signal.ChannelCount := 1;

  SignalInterpolator := TSignalInterpolator.Create(nil);
  SignalInterpolator.Input := Signal;
  SignalInterpolator.Factor := 2;
  SignalInterpolator.Ripple := 0.00001;
end;

destructor TInputFilter.Destroy;
begin
  Signal.Free;
  SignalInterpolator.Free;
  inherited;
end;

function TInputFilter.GetDataPointer: PSingle;
begin
  result := @SignalInterpolator.Data.SData[0];
end;

function TInputFilter.GetInterpolationFactor: integer;
begin
  result := SignalInterpolator.Factor;
end;

procedure TInputFilter.Process(InBuffer: PSingle; InputSampleFrames: integer);
var
  c1: Integer;
  ps:PSingle;
begin
  assert(InputSampleFrames = Signal.Data.Length);

  ps := @Signal.Data.SData[0];
  for c1 := 0 to InputSampleFrames-1 do
  begin
    ps^ := InBuffer^;
    inc(InBuffer);
    inc(ps);
  end;

  SignalInterpolator.Update;
end;

procedure TInputFilter.SetBufferSize(const Value: integer);
begin
  Signal.Length := Value;
end;

procedure TInputFilter.SetInterpolationFactor(const Value: integer);
begin
  SignalInterpolator.Factor := Value;
end;

//=========================================================================================================================
//=========================================================================================================================
//=========================================================================================================================





{ TOutputFilter }

constructor TOutputFilter.Create;
begin
  Signal := TSignal.Create(nil);
  Signal.Complex := false;
  Signal.ChannelCount := 1;

  SignalDecimator := TSignalDecimator.Create(nil);
  SignalDecimator.Input := Signal;
  SignalDecimator.Factor := 2;
  SignalDecimator.Ripple := 0.00001;
end;

destructor TOutputFilter.Destroy;
begin
  Signal.Free;
  SignalDecimator.Free;
  inherited;
end;

function TOutputFilter.GetDataPointer: PSingle;
begin
  result := @Signal.Data.SData[0];
end;

function TOutputFilter.GetDecimationFactor: integer;
begin
  result := SignalDecimator.Factor;
end;

procedure TOutputFilter.Process(OutBuffer: PSingle; InputSampleFrames: integer);
var
  c1: Integer;
  ps : PSingle;
begin
  assert(InputSampleFrames = Signal.Data.Length);

  SignalDecimator.Update;

  ps := @SignalDecimator.Data.SData[0];

  for c1 := 0 to InputSampleFrames div DecimationFactor - 1 do
  begin
    OutBuffer^ := ps^;
    inc(OutBuffer);
    inc(ps);
  end;

end;


procedure TOutputFilter.SetBufferSize(const Value: integer);
begin
  Signal.Length := Value;
end;

procedure TOutputFilter.SetDecimationFactor(const Value: integer);
begin
  SignalDecimator.Factor := Value;
end;

procedure TOutputFilter.ZeroBuffer;
var
  c1 : Integer;
  ps : PSingle;
begin
  ps := @Signal.Data.SData[0];

  for c1 := 0 to Signal.Data.Length-1 do
  begin
    ps^ := 0;
    inc(ps);
  end;
end;

{ TInputFilters }

constructor TInputFilters.Create;
begin
  fFilterCount := 0;
end;

destructor TInputFilters.Destroy;
begin
  FilterCount := 0;
  inherited;
end;

procedure TInputFilters.SetFilterCount(const Value: integer);
var
  c1 : integer;
begin
  for c1 := 0 to Length(fFilters)-1 do fFilters[c1].Free;
  fFilterCount := Value;
  SetLength(fFilters, Value);
  for c1 := 0 to Length(fFilters)-1 do fFilters[c1] := TInputFilter.Create;
end;



{ TOutputFilters }

constructor TOutputFilters.Create;
begin
  fFilterCount := 0;
end;

destructor TOutputFilters.Destroy;
begin
  FilterCount := 0;
  inherited;
end;

procedure TOutputFilters.SetFilterCount(const Value: integer);
var
  c1 : integer;
begin
  for c1 := 0 to Length(fFilters)-1 do fFilters[c1].Free;
  fFilterCount := Value;
  SetLength(fFilters, Value);
  for c1 := 0 to Length(fFilters)-1 do fFilters[c1] := TOutputFilter.Create;
end;

end.
