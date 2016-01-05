unit FarScape.InvalidateBuffer;

interface

uses
  Types,
  ExtCtrls;

type
  TInvalidateRegionEvent = procedure(Region : TRect) of object;

  TInvalidateBuffer = class
  private
    Buffer : array of TRect;
    BufferSize : integer;
    FOnInvalidateRegion: TInvalidateRegionEvent;
    InvalidateTimer : TTimer;

    ReadIndex : integer;
    WriteIndex : integer;

    procedure HandleTimerStep(Sender : TObject);
    procedure ProcessQueue;

  public
    constructor Create(const aBufferSize : integer);
    destructor Destroy; override;

    procedure QueueInvalidate(Region : TRect);

    property OnInvalidateRegion : TInvalidateRegionEvent read FOnInvalidateRegion write FOnInvalidateRegion;
  end;

implementation


function IsContainedBy(const RectA, RectB : TRect):boolean;
begin
  if RectB.Left   < RectA.Left then exit(false);
  if RectB.Top    < RectA.Top  then exit(false);
  if RectB.Bottom > RectA.Bottom then exit(false);
  if RectB.Right  > RectA.Right  then exit(false);

  result := true;
end;

{ TInvalidateBuffer }

constructor TInvalidateBuffer.Create(const aBufferSize : integer);
begin
  SetLength(Buffer, aBufferSize);
  BufferSize := aBufferSize;

  ReadIndex := 0;
  WriteIndex := 0;

  InvalidateTimer := TTimer.Create(nil);
  InvalidateTimer.Enabled := true;
  InvalidateTimer.Interval := 1000 div 48;
  InvalidateTimer.OnTimer := self.HandleTimerStep;
end;

destructor TInvalidateBuffer.Destroy;
begin
  BufferSize := 0;
  InvalidateTimer.Enabled := false;
  InvalidateTimer.Free;
  SetLength(Buffer, 0);
  inherited;
end;

procedure TInvalidateBuffer.HandleTimerStep(Sender: TObject);
begin
  if WriteIndex > 0
    then ProcessQueue;
end;

procedure TInvalidateBuffer.ProcessQueue;
var
  c1: Integer;
  CombinedRect : TRect;
begin
  if (assigned(OnInvalidateRegion)) and (WriteIndex > 0) then
  begin
    CombinedRect := Buffer[0];

    for c1 := 1 to WriteIndex-1 do
    begin
      CombinedRect.Union(Buffer[c1]);
    end;

    OnInvalidateRegion(CombinedRect);
    {
    for c1 := 0 to WriteIndex-1 do
    begin
      OnInvalidateRegion(Buffer[c1]);
    end;
    }
  end;
  WriteIndex := 0;
end;

procedure TInvalidateBuffer.QueueInvalidate(Region: TRect);
var
  c1: Integer;
begin
  if BufferSize = 0 then exit;

  for c1 := 0 to WriteIndex-1 do
  begin
    if IsContainedBy(Buffer[c1], Region)
      then exit; //=====>> exit >>===========>>
  end;

  Buffer[WriteIndex] := Region;
  inc(WriteIndex);

  if WriteIndex >= BufferSize-1
    then ProcessQueue;
end;

end.
