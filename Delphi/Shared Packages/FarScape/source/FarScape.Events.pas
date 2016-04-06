unit FarScape.Events;

interface

uses
  FarScape.Event,
  Classes,
  UITypes;

type
  //========================================
  //       Forward declarations
  //========================================
  TMouseEnterEvent = class;
  TMouseLeaveEvent = class;
  //TMouseDownEvent = class;
  //TMouseUpEvent = class;

  //========================================
  //    Finish declarations
  //========================================
  TMouseEvent = class(TFarScapeEvent)
  protected
    // These methods need tests written to see if they actually work.
    function GetDataAsTMouseButton(const Index: Integer): TMouseButton;
    procedure SetDataAsTMouseButton(const Index : integer; const Value : TMouseButton);

    function GetDataAsTShiftState(const Index: Integer): TShiftState;
    procedure SetDataAsTShiftState(const Index : integer; const Value : TShiftState);
  end;


  TMouseEnterEvent = class(TMouseEvent)
  public
    constructor Create(const Target : TObject); override;
  end;

  TMouseLeaveEvent = class(TMouseEvent)
  public
    constructor Create(const Target : TObject); override;
  end;


{
type

  //========================================
  //       Forward declarations
  //========================================
  TChildRemovingEvent = class;
  TChildRemovedEvent = class;
  TChildAddedEvent = class;

  TControlBoundsChangedEvent = class;

  TMouseEnterEvent = class;
  TMouseLeaveEvent = class;
  TMouseDownEvent = class;
  TMouseUpEvent = class;
  TContextClickEvent = class;

  TControlNameChangedEvent = class;

  TBeginEditEvent = class; // Controls should trigger a BeginEdit event before sending ValueChanged events.
  TEndEditEvent = class;   // Controls should trigger a EndEdit event after sending ValueChanged events.
  TValueChangedEvent = class;




  //========================================
  //    Finish declarations
  //========================================

  // The target is about to be removed.
  TChildRemovingEvent = class(TCustomEvent)
  public
    constructor Create(const Target : TObject);
  end;

  // The target control has been removed from the object hierarchy.
  TChildRemovedEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

  // The target is now a child control (or a child-of-child at any depth).
  TChildAddedEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

  TControlBoundsChangedEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

  TMouseEnterEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

  TMouseLeaveEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

  TMouseEvent = class(TCustomEvent)
  protected
    // These methods need tests written to see if they actually work.
    function GetDataAsTMouseButton(const Index: Integer): TMouseButton;
    procedure SetDataAsTMouseButton(const Index : integer; const Value : TMouseButton);

    function GetDataAsTShiftState(const Index: Integer): TShiftState;
    procedure SetDataAsTShiftState(const Index : integer; const Value : TShiftState);
  end;

  TMouseDownEvent = class(TMouseEvent)
  public
    class function Create(const Target : TObject; const Button : TMouseButton; const Shift  : TShiftState; const X, Y : integer):TEventData;

    property Button : TMouseButton index 0 read GetDataAsTMouseButton;
    property Shift  : TShiftState  index 1 read GetDataAsTShiftState;
    property X      : integer      index 2 read GetDataAsInteger;
    property Y      : integer      index 3 read GetDataAsInteger;
  end;

  TMouseUpEvent = class(TMouseEvent)
  public
    class function Create(const Target : TObject; const Button : TMouseButton; const Shift  : TShiftState; const X, Y : integer):TEventData;

    property Button : TMouseButton index 0 read GetDataAsTMouseButton;
    property Shift  : TShiftState  index 1 read GetDataAsTShiftState;
    property X      : integer      index 2 read GetDataAsInteger;
    property Y      : integer      index 3 read GetDataAsInteger;
  end;

  TContextClickEvent = class(TMouseEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

  TControlNameChangedEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject; const OldName, NewName : PString):TEventData;
    property OldName : PString index 0 read GetDataAsPString;
    property NewName : PString index 1 read GetDataAsPString;
  end;

  TValueChangedEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

  TBeginEditEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

  TEndEditEvent = class(TCustomEvent)
  public
    class function Create(const Target : TObject):TEventData;
  end;

}
implementation


{ TMouseEvent }

function TMouseEvent.GetDataAsTMouseButton(const Index: Integer): TMouseButton;
begin
  result := TMouseButton(FData[Index].VInteger);
end;

function TMouseEvent.GetDataAsTShiftState(const Index: Integer): TShiftState;
var
  wx : word;
begin
  // casting from an integer to a set.
  // http://stackoverflow.com/a/9554208/395461
  assert(SizeOf(wx) = SizeOf(result));
  wx := self.GetDataAsInteger(Index);
  word(result) := wx;
end;

procedure TMouseEvent.SetDataAsTMouseButton(const Index: integer; const Value: TMouseButton);
begin
  FData[Index].VInteger := Ord(Value);
end;

procedure TMouseEvent.SetDataAsTShiftState(const Index: integer; const Value: TShiftState);
var
  wx : word;
begin
  // casting from an integer to a set.
  // http://stackoverflow.com/a/9554208/395461
  assert(SizeOf(wx) = SizeOf(Value));
  wx := Word(Value);
  FData[Index].VInteger := wx;
end;

{ TMouseLeaveEvent }

constructor TMouseLeaveEvent.Create(const Target: TObject);
begin
  inherited;
  self.FTarget := Target;
  self.FEventClass := TMouseLeaveEvent;
end;

{ TMouseEnterEvent }

constructor TMouseEnterEvent.Create(const Target: TObject);
begin
  inherited;
  self.FTarget := self;
  self.FEventClass := TMouseEnterEvent;
end;

end.
