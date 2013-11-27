{
  eeMidiPorts relies on uMidi for it's functionality, but uMidi uses global midi port variables, so
  only one midi port can be accessed at once for all instances of the application. In the case of a plugin
  this probably isn't preferable, so uMidi port will probably need to be replaced with a new implementation
  at some point. To make the potential transition easier, TMidiInput and TMidiOutput classes will wrap most
  of the functionality provided by the uMidi classes.

}


unit eeMidiPorts;

interface

{$INCLUDE Defines.inc}

uses
  uMidi, Classes;

type
  TMidiPort = class
  private
    fDevices: TStringList;
    fCurrentDeviceIndex: integer;
  protected
    fIsPortOpen: boolean;
    fCurrentPort: string;

    property CurrentDeviceIndex:integer read fCurrentDeviceIndex write fCurrentDeviceIndex;
  public
    constructor Create; virtual;
	  destructor Destroy; override;

    property Devices:TStringList read fDevices write fDevices;
    property IsPortOpen:boolean read fIsPortOpen;
    property CurrentPort:string read fCurrentPort;
  end;



  TMidiInput = class(TMidiPort)
  private
  public
    constructor Create; override;
	  destructor Destroy; override;

    procedure OpenPort(PortName:string);
    procedure ClosePort;

    property Devices;
    property IsPortOpen;
    property CurrentPort;
  end;



  TMidiOutput = class(TMidiPort)
  private
  public
    constructor Create; override;
	  destructor Destroy; override;

    procedure OpenPort(PortName:string);
    procedure ClosePort;

    procedure Send(const aStatus, aData1, aData2: byte);

    property Devices;
    property IsPortOpen;
    property CurrentPort;
  end;



implementation

uses
  SysUtils;


{ TMidiPort }

constructor TMidiPort.Create;
begin
  Devices := TStringList.Create;
  fIsPortOpen := false;
  fCurrentPort := '';
  fCurrentDeviceIndex := -1;
end;

destructor TMidiPort.Destroy;
begin
  Devices.Free;
  inherited;
end;


{ TMidiInput }

constructor TMidiInput.Create;
begin
  inherited;

  Devices.AddStrings(MidiInput.Devices);

end;

destructor TMidiInput.Destroy;
begin

  inherited;
end;


procedure TMidiInput.OpenPort(PortName: string);
begin

end;

procedure TMidiInput.ClosePort;
begin

end;



{ TMidiOutput }

constructor TMidiOutput.Create;
begin
  inherited;

  Devices.AddStrings(MidiOutput.Devices);
end;

destructor TMidiOutput.Destroy;
begin
  //Close port if required.
  if CurrentDeviceIndex <> -1 then ClosePort;
  
  inherited;
end;


procedure TMidiOutput.OpenPort(PortName: string);
var
  Index:integer;
begin
  //Close port if required.
  if CurrentDeviceIndex <> -1 then ClosePort;

  //Look for port index.
  Index := Devices.IndexOf(PortName);;
  if Index = -1 then raise Exception.Create('MIDI port does not exist.');

  //Update port information and open the port.
  CurrentDeviceIndex := Index;
  fCurrentPort       := PortName;
  fIsPortOpen        := true;

  MidiOutput.Open(CurrentDeviceIndex);
end;

procedure TMidiOutput.Send(const aStatus, aData1, aData2: byte);
begin
  if CurrentDeviceIndex = -1 then raise Exception.Create('MIDI port not open.');

  MidiOutput.Send(CurrentDeviceIndex, aStatus, aData1, aData2);  
end;

procedure TMidiOutput.ClosePort;
begin
  if CurrentDeviceIndex <> -1 then
  begin
    MidiOutput.Close(fCurrentDeviceIndex);
    fCurrentDeviceIndex := -1;
    fCurrentPort        := '';
    fIsPortOpen         := false;
  end;

end;



end.
