{
  A event buffer to hold events to be processed between sample blocks.
}


unit eeBufferedEventList;

interface

uses
  SyncObjs,
  VamLib.Types;

type
  PBufferedEvent = ^TBufferedEvent; 
  TBufferedEvent = record
    Tag:integer;
    ValueA:integer;
    ValueB:integer;
    Text:string;
    Data:pointer;
  end;

  TBufferedEventList = class
  private
    fEventCount: integer;
    fMaxEventCount: integer;
    procedure SetMaxEventCount(const Value: integer);
    function GetEvent(Index: integer): PBufferedEvent;
  protected
    fEvents:array of TBufferedEvent;
    cs:TFixedCriticalSection;
    property MaxEventCount:integer read fMaxEventCount write SetMaxEventCount;
  public
    constructor Create;
	  destructor Destroy; override;

    function TryLockList:boolean;
    procedure LockList;
    procedure UnlockList;
    procedure ResetEventCount;

    procedure AddEvent(ev:PBufferedEvent);

    property EventCount:integer read fEventCount;

    property Events[Index:integer]:PBufferedEvent read GetEvent;
  end;

implementation

uses
  Windows;

{ TBufferedEventList }

procedure TBufferedEventList.AddEvent(ev: PBufferedEvent);
begin
  if EventCount = MaxEventCount then MaxEventCount := MaxEventCount + 16;

  Events[EventCount]^.Tag    := ev^.Tag;
  Events[EventCount]^.ValueA := ev^.ValueA;
  Events[EventCount]^.ValueB := ev^.ValueB;
  Events[EventCount]^.Text   := ev^.Text;
  Events[EventCount]^.Data   := ev^.Data;

  inc(fEventCount);

  
end;

constructor TBufferedEventList.Create;
begin
  MaxEventCount := 16;
  cs := TFixedCriticalSection.Create;
end;

destructor TBufferedEventList.Destroy;
begin
  SetLength(fEvents, 0);
  cs.Free;
  inherited;
end;

function TBufferedEventList.GetEvent(Index: integer): PBufferedEvent;
begin
  result := @fEvents[Index];
end;

procedure TBufferedEventList.SetMaxEventCount(const Value: integer);
begin
  fMaxEventCount := Value;
  SetLength(fEvents, fMaxEventCount);
end;

function TBufferedEventList.TryLockList: boolean;
begin
  result := cs.TryEnter;
end;

procedure TBufferedEventList.LockList;
begin
  cs.Acquire;
end;

procedure TBufferedEventList.UnlockList;
begin
  cs.Release;
end;

procedure TBufferedEventList.ResetEventCount;
begin
  fEventCount := 0;
end;



end.
