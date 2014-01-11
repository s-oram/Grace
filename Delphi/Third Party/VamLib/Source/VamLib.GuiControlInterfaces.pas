unit VamLib.GuiControlInterfaces;

interface

type
  IKnobControl = interface
    ['{9134916E-5ACF-4E64-AA71-61B9B65D5844}']

    // Get/Set the value/position of the knob. Valid range is 0..1
    function GetKnobValue : single;
    procedure SetKnobValue(Value : single);
  end;

implementation

end.
