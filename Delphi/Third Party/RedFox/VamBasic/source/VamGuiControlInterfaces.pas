unit VamGuiControlInterfaces;

interface

uses
  Classes,
  Vcl.Controls;

// This is a collection of interfaces GUI controls can implement so that the
// EasyEffect VST template can interact with them programmatically.

type
  // The knob control is for the standard VST synth type knob.
  IKnobControl = interface
    ['{9134916E-5ACF-4E64-AA71-61B9B65D5844}']

    // typically used to store the VST parameter index.
    procedure SetParameterIndex(Index : integer);
    function GetParameterIndex:integer;

    // Get/Set the value/position of the knob. Valid range is 0..1
    function GetKnobValue : single;
    procedure SetKnobValue(Value : single);

    function GetModAmountValue : single;
    procedure SetModAmountValue(Value : single);

    procedure SetOnMouseEnter(Handler:TNotifyEvent);
    procedure SetOnMouseLeave(Handler:TNotifyEvent);
    procedure SetOnMouseDown(Handler:TMouseEvent);
    procedure SetOnMouseUp(Handler:TMouseEvent);

    procedure SetOnKnobPosChanged(Handler:TNotifyEvent);
    procedure SetOnModAmountChanged(Handler:TNotifyEvent);
  end;


  // The IMenuControl is for mode button type menus. The button will
  // show one value as text. (Ie. SQUARE or SAW.)
  // Left clicking on the button will show a menu with all options.
  // Right clicking will cause the control to automatically advance to the next value.
  IMenuControl = interface
    ['{24A7258B-A37B-4A88-8B81-2F5836CDBF8D}']

    procedure SetMenuText(Value : string);

    procedure SetOnMouseEnter(Handler:TNotifyEvent);
    procedure SetOnMouseLeave(Handler:TNotifyEvent);
    procedure SetOnMouseDown(Handler:TMouseEvent);
    procedure SetOnMouseUp(Handler:TMouseEvent);
  end;

implementation

end.
