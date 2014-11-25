unit VamGuiControlInterfaces;

interface

uses
  Classes,
  Vcl.Controls;

// This is a collection of interfaces GUI controls can implement so that the
// EasyEffect VST template can interact with them programmatically.

type
  TKnobMode = (PositionEdit, ModEdit);

  // The knob control is for the standard VST synth type knob.
  IKnobControl = interface
    ['{9134916E-5ACF-4E64-AA71-61B9B65D5844}']

    // typically used to store the VST parameter index.
    procedure SetParameterIndex(Index : integer);
    function GetParameterIndex:integer;

    // typically used to store the linked parameter name.
    procedure SetParameterName(aName:string);
    function GetParameterName:string;

    // Get/Set the value/position of the knob. Valid range is 0..1
    function GetKnobValue : single;
    procedure SetKnobValue(Value : single);

    function GetModAmountValue : single;
    procedure SetModAmountValue(Value : single);

    function GetKnobMode:TKnobMode;
    procedure SetKnobMode(const Value: TKnobMode);


    procedure SetOnMouseEnter(Handler:TNotifyEvent);
    procedure SetOnMouseLeave(Handler:TNotifyEvent);
    procedure SetOnMouseDown(Handler:TMouseEvent);
    procedure SetOnMouseUp(Handler:TMouseEvent);

    procedure SetOnKnobPosChanged(Handler:TNotifyEvent);
    procedure SetOnModAmountChanged(Handler:TNotifyEvent);
  end;

  IXYPadControl = interface(IInterface)
    ['{2230D1CD-7C5E-48E4-8BE1-5CD37F5A8431}']
    //TODO:LOW
  end;


  // The IMenuControl is for mode button type menus. The button will
  // show one value as text. (Ie. SQUARE or SAW.)
  // Left clicking on the button will show a menu with all options.
  // Right clicking will cause the control to automatically advance to the next value.
  IMenuControl = interface
    ['{24A7258B-A37B-4A88-8B81-2F5836CDBF8D}']

    procedure SetParameterName(const Value: string);
    function GetParameterName: string;

    procedure SetMenuText(Value : string);

    function GetMenuItemSelectedCallback: TNotifyEvent;

    procedure SetOnMouseEnter(Handler:TNotifyEvent);
    procedure SetOnMouseLeave(Handler:TNotifyEvent);
    procedure SetOnMouseDown(Handler:TMouseEvent);
    procedure SetOnMouseUp(Handler:TMouseEvent);
  end;





  //==== Data source interfaces ====
  ILevelMonitor = interface
    ['{AE6977D8-D39D-4093-B9FA-7875F02A838C}']
    procedure GetDbLevel(out Ch1, Ch2 : single);
  end;

  IStepSequenceDataObject = interface
    ['{2CB04233-08C0-425A-B122-C8D476A5D50F}']
    function GetStepValue(Index : integer):single;
    procedure SetStepValue(Index : integer; const Value:single);
  end;

implementation

end.

