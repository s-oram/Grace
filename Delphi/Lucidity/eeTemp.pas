unit eeTemp;

interface


type
  IStandardControlHandler = interface
    ['{4EE51F43-F6EC-458F-8043-457823A5D45E}']


    // SetupControl() is called by TGuiStandard. The handler
    // should set event handling etc so that the event handler
    // is called when the user interacts with the control.
    procedure RegisterControl(const c : TObject);
    procedure DeregisterControl(const c : TObject);

    // UpdateControl() is called by TGuiStandard. When called
    // the handler should check the plugin for parameter state
    // changes and update the control if required.
    procedure UpdateControl(const c : TObject);
    procedure UpdateAllControls;
  end;

implementation

end.
