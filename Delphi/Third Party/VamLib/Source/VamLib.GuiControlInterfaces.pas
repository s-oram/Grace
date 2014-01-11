unit VamLib.GuiControlInterfaces;

interface

uses
  Classes,
  Vcl.Controls;

// This is a collection of interfaces GUI controls can implement so that the
// EasyEffect VST template can interact with them programmatically.

type
  IKnobControl = interface
    ['{9134916E-5ACF-4E64-AA71-61B9B65D5844}']

    // Get/Set the value/position of the knob. Valid range is 0..1
    function GetKnobValue : single;
    procedure SetKnobValue(Value : single);

    procedure SetOnMouseDown(Handler:TMouseEvent);
    procedure SetOnMouseUp(Handler:TMouseEvent);
    procedure SetOnChanged(Handler:TNotifyEvent);
  end;

implementation

end.
