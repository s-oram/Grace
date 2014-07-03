unit uBuffers;

interface

uses
  MoreTypes;

type
  TBuffers = class
  private
    fCount: integer;
    fBufferLength: integer;
    procedure SetCount(const Value: integer);
    procedure SetBufferLength(const Value: integer);
  protected
    procedure MakeBuffers;
    procedure FreeBuffers;
  public
    VstBufferPointer:pointer;
    BufferPointers:array of PSingle;
    Buffers:array of array of single;
    constructor Create;
	  destructor Destroy; override;

    procedure ZeroBuffers; inline;

    property Count:integer read fCount write SetCount;
    property BufferLength:integer read fBufferLength write SetBufferLength;

  end;

implementation

{ TBuffers }

constructor TBuffers.Create;
begin
  fBufferLength := 0;
  fCount := 0;
end;

destructor TBuffers.Destroy;
begin
  FreeBuffers;
  inherited;
end;

procedure TBuffers.FreeBuffers;
var
  c1:integer;
begin
  for c1 := 0 to Count - 1 do
  begin
    SetLength(Buffers[c1],0);
  end;

  SetLength(Buffers, 0);
  SetLength(BufferPointers, 0);

  fBufferLength := 0;
  fCount := 0;

  VstBufferPointer := nil;
end;

procedure TBuffers.MakeBuffers;
var
  c1:integer;
begin
  SetLength(Buffers, Count);
  SetLength(BufferPointers, Count);

  for c1 := 0 to Count - 1 do
  begin
    SetLength(Buffers[c1],BufferLength);
    BufferPointers[c1] := @Buffers[c1,0];
  end;

  if Count > 0
    then VstBufferPointer := @BufferPointers[0]
    else VstBufferPointer := nil;


end;

procedure TBuffers.SetBufferLength(const Value: integer);
begin
  fBufferLength := Value;

  MakeBuffers;
end;

procedure TBuffers.SetCount(const Value: integer);
begin
  fCount := Value;

  MakeBuffers;
end;

procedure TBuffers.ZeroBuffers;
var
  c1: Integer;
  c2: Integer;
begin
  for c1 := 0 to fCount-1 do
  begin
    for c2 := 0 to fBufferLength-1 do
    begin
      Buffers[c1, c2] := 0;
    end;
  end;
end;

end.
