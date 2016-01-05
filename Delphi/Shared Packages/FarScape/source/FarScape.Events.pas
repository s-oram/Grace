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
    class function Create(const Target : TObject):TEventData;
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


implementation

uses
  Rtti,
  FarScape.SupportFunctions;

type
  TProtectedEventHack = class(TCustomEvent);

type
  TShiftStateValues = (ssvShift, ssvAlt, ssvCtrl, ssvLeft, ssvRight, ssvMiddle, ssvDouble, ssvTouch, ssvPen, ssvCommand);

{ TControlNameChangedEvent }

class function TControlNameChangedEvent.Create(const Target: TObject; const OldName, NewName: PString): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TControlNameChangedEvent);
  TProtectedEventHack(ev).FTarget := Target;
  TProtectedEventHack(ev).SetDataAsPString(0,OldName);
  TProtectedEventHack(ev).SetDataAsPString(1,NewName);
  result := ev;
end;

{ TMouseEvent }

function TMouseEvent.GetDataAsTShiftState(const Index: Integer): TShiftState;
var
  wx : word;
begin
  // casting from an integer to a set.
  //http://stackoverflow.com/a/9554208/395461
  wx := self.GetDataAsInteger(Index);
  word(result) := wx;
end;

procedure TMouseEvent.SetDataAsTShiftState(const Index: integer; const Value: TShiftState);
var
  wx : word;
begin
  wx := word(Value);
  self.SetDataAsInteger(Index, wx);
end;


procedure TMouseEvent.SetDataAsTMouseButton(const Index: integer; const Value: TMouseButton);
begin
  FData[Index].VInteger := Integer(Value);
end;

function TMouseEvent.GetDataAsTMouseButton(const Index: Integer): TMouseButton;
begin
  result := TMouseButton(FData[Index].VInteger);
end;


{ TMouseDownEvent }

class function TMouseDownEvent.Create(const Target: TObject; const Button: TMouseButton; const Shift: TShiftState; const X, Y: integer): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TMouseDownEvent);
  TMouseEvent(ev).FTarget := Target;
  TMouseEvent(ev).SetDataAsTMouseButton(0, Button);
  TMouseEvent(ev).SetDataAsTShiftState(1, Shift);
  TMouseEvent(ev).SetDataAsInteger(2, X);
  TMouseEvent(ev).SetDataAsInteger(3, Y);
  result := ev;
end;

{ TMouseUpEvent }

class function TMouseUpEvent.Create(const Target: TObject; const Button: TMouseButton; const Shift: TShiftState; const X, Y: integer): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TMouseUpEvent);
  TMouseEvent(ev).FTarget := Target;
  TMouseEvent(ev).SetDataAsTMouseButton(0, Button);
  TMouseEvent(ev).SetDataAsTShiftState(1, Shift);
  TMouseEvent(ev).SetDataAsInteger(2, X);
  TMouseEvent(ev).SetDataAsInteger(3, Y);
  result := ev;
end;

{ TRemovingChildEvent }

class function TChildRemovingEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TChildRemovingEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;

{ TChildRemovedEvent }

class function TChildRemovedEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TChildRemovedEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;


{ TNewChildEvent }

class function TChildAddedEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TChildAddedEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;

{ TMouseEnterEvent }

class function TMouseEnterEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TMouseEnterEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;

{ TMouseLeaveEvent }

class function TMouseLeaveEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TMouseLeaveEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;

{ TValueChangedEvent }

class function TValueChangedEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TValueChangedEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;


{ TControlBoundsChangedEvent }

class function TControlBoundsChangedEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TControlBoundsChangedEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;

{ TBeginEditEvent }

class function TBeginEditEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TBeginEditEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;

{ TEndEditEvent }

class function TEndEditEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TEndEditEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;

{ TContextClickEvent }

class function TContextClickEvent.Create(const Target: TObject): TEventData;
var
  ev : TEventData;
begin
  ev := TEventData.Create(TContextClickEvent);
  TProtectedEventHack(ev).FTarget := Target;
  result := ev;
end;

end.
