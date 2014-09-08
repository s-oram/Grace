unit VamKeyStateTracker;

interface

uses
  Generics.Collections, VamLib.Types;

type
  TKeyState = record
    Note     : byte;
    Velocity : byte;
  end;

  TKeyStateList = TList<TKeyState>;

  PKeyStateData = ^TKeyStateData;
  TKeyStateData = array of TKeyState;

  TKeyStateTracker = class
  private
  protected
    List : TKeyStateList;
    // TODO: see if ListLock below can be removed. It's used in
    // the audio thread.
    ListLock : TFixedCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure NoteOn(const Data1, Data2 : byte);
    procedure NoteOff(const Data1, Data2 : byte);

    procedure GetData(out Data : TKeyStateData);
  end;

implementation

{ TKeyStateTracker }

constructor TKeyStateTracker.Create;
begin
  List := TKeyStateList.Create;
  ListLock := TFixedCriticalSection.Create;

  List.ToArray
end;

destructor TKeyStateTracker.Destroy;
begin
  List.Free;
  ListLock.Free;
  inherited;
end;

procedure TKeyStateTracker.NoteOn(const Data1, Data2: byte);
var
  ks : TKeyState;
begin
  ListLock.Enter;
  try
    ks.Note := Data1;
    ks.Velocity := Data2;
    List.Add(ks);
  finally
    ListLock.Leave;
  end;
end;

procedure TKeyStateTracker.NoteOff(const Data1, Data2: byte);
var
  c1: Integer;
begin
  ListLock.Enter;
  try
    for c1 := List.Count-1 downto 0 do
    begin
      if List.Items[c1].Note = Data1 then List.Delete(c1);
    end;
  finally
    ListLock.Leave;
  end;
end;

procedure TKeyStateTracker.GetData(out Data: TKeyStateData);
var
  c1: Integer;
begin
  ListLock.Enter;
  try
    SetLength(Data, List.Count);

    for c1 := 0 to List.Count-1 do
    begin
      Data[c1].Note     := List[c1].Note;
      Data[c1].Velocity := List[c1].Velocity;
    end;

  finally
    ListLock.Leave;
  end;
end;





end.
