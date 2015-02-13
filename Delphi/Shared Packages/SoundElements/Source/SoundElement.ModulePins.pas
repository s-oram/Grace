unit SoundElement.ModulePins;

interface

{$SCOPEDENUMS ON}

uses
  SysUtils,
  Types,
  Classes,
  Spring,
  SoundElement.Types;


{
  Module Pins are divided into two types.
  - Event Pins.
  - Stream Pins.

  Event Pins are intended for module parameters that change
  infrequently. Value changes propgate from module to module
  via an event system. Individual events have a significant
  overhead when triggered.

  Stream Pins are intended for continuous 'streaming' data
  like audio signals. Steam Pins are optimised to have a
  low overhead for continuously routing blocks of data
  between modules. The downside is that the overhead
  is always paid even if the data value doesn't change.
  Therefore Stream Pins are a poor choice for constant
  values.

  == Assumptions for Event Pins ==
  - Event Pins can have multiple inputs and outputs connected.
  -- An input pin will be in an undefined state after an output
     is disconnected. Input pins are only update when connected
     output pins change. The application user can trigger
     an update when connecting pins.


  == Assumptions for Stream Pins ==
  - Stream Inputs can only be connected to one inputs.
  - Stream Outputs can be connected to multiple inputs.
  - It is assumed Stream Inputs will be updated every processing
    block.
}



type
  PFloat64Stream = ^TFloat64Stream;
  TFloat64Stream = record
  strict private
    function GetSize: integer;
    procedure SetSize(const Value: integer);
  public
    Data : array of double;
    SampleFrames : integer;
    property Size : integer read GetSize write SetSize;
  end;

  //======== Forward Declarations ========================
  TModulePin = class;

  TInputPin = class;
  TOutputPin = class;

  TEventInputPin = class;
  TEventOutputPin = class;

  TStreamInputPin = class;
  TStreamOutputPin = class;

  TFloatInput = class;
  TFloatOutput = class;

  TFloat64StreamInput  = class;
  TFloat64StreamOutput = class;
  //======================================================
  //======================================================

  TPinType = (FloatEvent, IntegerEvent, StringEvent, Float64Stream);
  TPinPolarity = (Input, Output);

  TModulePin = class
  private
    FName: string;
    FOwner: TObject;
    function GetConnection(Index: integer): TModulePin;
    function GetConnectionCount: integer;
  protected
    // NOTE: Don't expose FConnections to code outside this unit. I might
    // change the FConnections type to a simpler list type.
    FConnections: TSoundElementObjectArray;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetPinValue(const Value : integer); overload; virtual;
    procedure SetPinValue(const Value : double); overload; virtual;
    procedure SetPinValue(const Value : string); overload; virtual;

    property Name  : string read FName write FName;
    // Owner should be a reference to the module that owns the pin. But Owner will be NIL in some cases.
    property Owner : TObject read FOwner write FOwner;

    // The maximum expected required buffer size for this pin.
    function SetMaxBufferSize(const MaxSize : integer):TModulePin; virtual;

    property ConnectionCount : integer read GetConnectionCount;
    property Connections[Index : integer] : TModulePin read GetConnection;
  end;

  TInputPin = class(TModulePin)
  private
    fOnChanged: TNotifyAnonEvent;
  protected
    // Check if a connection from this pin class type is valid.
    function IsValidConnectionFrom(const Sender: TOutputPin): boolean; virtual; //TODO: should be a class type, not an instance.

    // Can the pin currently accept a new connection from this output.
    // - Connection will be refused if it's the wrong type.
    // - Connection might be refused for other reasons.
    function AcceptNewConnectionFrom(const OutputPin : TOutputPin) : boolean; virtual;

    // The sender pin has changed somehow.
    procedure Handle_ConnectedOutputChanged(const OutputPin : TOutputPin); virtual; abstract;

    procedure ConnectTo(OutputPin : TOutputPin); virtual;
    procedure DisconnectFrom(OutputPin : TOutputPin); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    property OnChanged : TNotifyAnonEvent read fOnChanged write fOnChanged;
  end;

  TOutputPin = class(TModulePin)
  private
  protected
    procedure ConnectTo(InputPin : TInputPin); virtual;
    procedure DisconnectFrom(InputPin : TInputPin); virtual;

    // Call OutputChanged() to notify connected input pins that something has changed.
    procedure OutputChanged;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TEventInputPin = class(TInputPin);
  TEventOutputPin = class(TOutputPin);

  TStreamInputPin = class(TInputPin);
  TStreamOutputPin = class(TOutputPin);

  TFloatInput = class(TEventInputPin)
  private
    FPinValue: Double;
  protected
    procedure SetPinValue(const Value : integer); override;
    procedure SetPinValue(const Value : double); override;

    function IsValidConnectionFrom(const Sender: TOutputPin): boolean; override;
    procedure Handle_ConnectedOutputChanged(const Sender : TOutputPin); override;
  public
    constructor Create; override;
    property PinValue : Double read FPinValue write SetPinValue;
  end;

  TFloatOutput = class(TEventOutputPin)
  private
    FPinValue: Double;
  protected
    procedure SetPinValue(const Value: integer); override;
    procedure SetPinValue(const Value: Double); override;
  public
    constructor Create; override;
    property PinValue : Double read FPinValue write SetPinValue;
  end;

  TIntegerInput = class(TEventInputPin)
  private
    FPinValue: Integer;
  protected
    procedure SetPinValue(const Value : integer); override;
    function IsValidConnectionFrom(const Sender: TOutputPin): boolean; override;
    procedure Handle_ConnectedOutputChanged(const Sender : TOutputPin); override;
  public
    constructor Create; override;
    property PinValue : Integer read FPinValue write SetPinValue;
  end;

  TIntegerOutput = class(TEventOutputPin)
  private
    FPinValue: Integer;
  protected
    procedure SetPinValue(const Value: Integer); override;
  public
    constructor Create; override;
    property PinValue : Integer read FPinValue write SetPinValue;
  end;

  TStringInput = class(TEventInputPin)
  private
    FPinValue: String;
  protected
    procedure SetPinValue(const Value : string); override;
    function IsValidConnectionFrom(const Sender: TOutputPin): boolean; override;
    procedure Handle_ConnectedOutputChanged(const Sender : TOutputPin); override;
  public
    constructor Create; override;
    property PinValue : String read FPinValue write SetPinValue;
  end;

  TStringOutput = class(TEventOutputPin)
  private
    FPinValue: String;
  protected
    procedure SetPinValue(const Value: String); override;
  public
    constructor Create; override;
    property PinValue : String read FPinValue write SetPinValue;
  end;

  TFloat64StreamInput = class(TStreamInputPin)
  private
    fPinDataPointer: PFloat64Stream;
  strict protected
    LocalPinData : TFloat64Stream;
  protected
    function IsValidConnectionFrom(const Sender: TOutputPin): boolean; override;
    function AcceptNewConnectionFrom(const OutputPin : TOutputPin) : boolean; override;
    procedure Handle_ConnectedOutputChanged(const OutputPin : TOutputPin); override;

    procedure ConnectTo(OutputPin : TOutputPin); override;
    procedure DisconnectFrom(OutputPin : TOutputPin); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetMaxBufferSize(const MaxSize : integer):TModulePin; override;

    property PinData : PFloat64Stream read FPinDataPointer;
  end;

  TFloat64StreamOutput = class(TStreamOutputPin)
  private
    fPinDataPointer: PFloat64Stream;
  protected
    FPinData : TFloat64Stream;
  public
    constructor Create; override;
    destructor Destroy; override;

    function SetMaxBufferSize(const MaxSize : integer):TModulePin; override;

    property PinData : PFloat64Stream read fPinDataPointer;
  end;


  // NOTE: IMPORTANT: Client code should use the PinCommand methods to interact
  // with module pins. This decouple the low-level pin implementation from
  // the client code.
  // Additionally, pin states need to be kept in sync when being connected and
  // disconnected from each other. A independant facade allows the connection/disconnection
  // code to be put in one place.
  PinCommand = record
  public
    class function CanMakeConnection(const Output : TOutputPin; Input : TInputPin):boolean; static;
    class procedure MakeConnection(const Output : TOutputPin; Input : TInputPin); static;
    class procedure BreakConnection(const Output : TOutputPin; Input : TInputPin); static;

    class procedure BreakAllConnections(const Pin : TModulePin); static;
    class function GetConnectionCount(const Pin : TModulePin):integer; static;
    class function CreatePin(const PinType : TPinType; const PinPolarity : TPinPolarity):TModulePin; static;

  end;

implementation


//==========================================================================================================================================
//        Streaming Module data types
//==========================================================================================================================================

{ TFloat64Stream }

function TFloat64Stream.GetSize: integer;
begin
  result := Length(Data);
end;

procedure TFloat64Stream.SetSize(const Value: integer);
begin
  SetLength(Data, Value);
end;

//==========================================================================================================================================


{ TModulePin }

constructor TModulePin.Create;
begin
  FConnections := TSoundElementObjectArray.Create;
end;

destructor TModulePin.Destroy;
begin
  // TODO:MED - might need to send a warning that the pin is still connected.
  PinCommand.BreakAllConnections(self);
  if FConnections.Count > 0 then raise SoundElementModuleException.Create('Pin still has connections.');
  FConnections.Free;
  inherited;
end;


function TModulePin.GetConnection(Index: integer): TModulePin;
begin
  result := FConnections[Index] as TModulePin;
end;

function TModulePin.GetConnectionCount: integer;
begin
  result := FConnections.Count;
end;

function TModulePin.SetMaxBufferSize(const MaxSize: integer): TModulePin;
begin
  result := self;
end;

procedure TModulePin.SetPinValue(const Value: integer);
begin
  raise SoundElementModuleException.Create('SetPinValue(Integer) is unsupported for this pin type.');
end;

procedure TModulePin.SetPinValue(const Value: double);
begin
  raise SoundElementModuleException.Create('SetPinValue(double) is unsupported for this pin type.');
end;

procedure TModulePin.SetPinValue(const Value: string);
begin
  raise SoundElementModuleException.Create('SetPinValue(string) is unsupported for this pin type.');
end;


{ TInputPin }

constructor TInputPin.Create;
begin
  inherited;

end;

destructor TInputPin.Destroy;
begin


  inherited;
end;

procedure TInputPin.ConnectTo(OutputPin: TOutputPin);
begin
  if FConnections.Contains(OutputPin) then raise SoundElementModuleException.Create('Input already connected to Output.');
  FConnections.AddObject(OutputPin);
end;

procedure TInputPin.DisconnectFrom(OutputPin: TOutputPin);
begin
  FConnections.Remove(OutputPin);
end;

function TInputPin.IsValidConnectionFrom(const Sender: TOutputPin): boolean;
begin
  result := false;
end;

function TInputPin.AcceptNewConnectionFrom(const OutputPin: TOutputPin): boolean;
begin
  result := IsValidConnectionFrom(OutputPin);
end;



{ TOutputPin }

constructor TOutputPin.Create;
begin
  inherited;
end;

destructor TOutputPin.Destroy;
begin
  inherited;
end;

procedure TOutputPin.ConnectTo(InputPin: TInputPin);
begin
  if FConnections.Contains(InputPin) then raise SoundElementModuleException.Create('Output already connected to Input.');
  FConnections.AddObject(InputPin);
end;

procedure TOutputPin.DisconnectFrom(InputPin: TInputPin);
begin
  FConnections.Remove(InputPin);
end;

procedure TOutputPin.OutputChanged;
var
  c1: Integer;
begin
  for c1 := 0 to FConnections.Count-1 do
  begin
    (FConnections[c1] as TInputPin).Handle_ConnectedOutputChanged(self);
  end;
end;



{ TFloatOutput }

constructor TFloatOutput.Create;
begin
  inherited;
  FPinValue := 0;
end;

procedure TFloatOutput.SetPinValue(const Value: Double);
begin
  FPinValue := Value;
  OutputChanged;
end;

procedure TFloatOutput.SetPinValue(const Value: integer);
begin
  FPinValue := Value;
  OutputChanged;
end;

{ TFloatInput }

constructor TFloatInput.Create;
begin
  inherited;
  FPinValue := 0;
end;

function TFloatInput.IsValidConnectionFrom(const Sender: TOutputPin):boolean;
begin
  if (Sender is TFloatOutput)   then exit(true);
  if (Sender is TIntegerOutput) then exit(true);
  result := false;
end;

procedure TFloatInput.SetPinValue(const Value: double);
begin
  FPinValue := Value;
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TFloatInput.SetPinValue(const Value: integer);
begin
  FPinValue := Value;
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TFloatInput.Handle_ConnectedOutputChanged(const Sender: TOutputPin);
begin
  assert(IsValidConnectionFrom(Sender));
  if (Sender is TFloatOutput) then
  begin
    self.FPinValue := (Sender as TFloatOutput).PinValue;
  end else
  if (Sender is TIntegerOutput) then
  begin
    self.FPinValue := (Sender as TIntegerOutput).PinValue;
  end else
  begin
    raise SoundElementModuleException.Create('Unexpected output type.');
  end;
  if assigned(OnChanged) then OnChanged(self);
end;

{ TIntegerInput }

constructor TIntegerInput.Create;
begin
  inherited;
  FPinValue := 0;
end;

function TIntegerInput.IsValidConnectionFrom(const Sender: TOutputPin): boolean;
begin
  if (Sender is TIntegerOutput) then exit(true);
  result := false;
end;

procedure TIntegerInput.SetPinValue(const Value: integer);
begin
  FPinValue := Value;
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TIntegerInput.Handle_ConnectedOutputChanged(const Sender: TOutputPin);
begin
  assert(IsValidConnectionFrom(Sender));
  if (Sender is TIntegerOutput) then
  begin
    self.FPinValue := (Sender as TIntegerOutput).PinValue;
  end else
  begin
    raise SoundElementModuleException.Create('Unexpected output type.');
  end;
  if assigned(OnChanged) then OnChanged(self);
end;

{ TIntegerOutput }

constructor TIntegerOutput.Create;
begin
  inherited;
  FPinValue := 0;
end;

procedure TIntegerOutput.SetPinValue(const Value: Integer);
begin
  FPinValue := Value;
  OutputChanged;
end;

{ TStringOutput }

constructor TStringOutput.Create;
begin
  inherited;
  FPinValue := '';
end;

procedure TStringOutput.SetPinValue(const Value: String);
begin
  FPinValue := Value;
  OutputChanged;
end;

{ TStringInput }

constructor TStringInput.Create;
begin
  inherited;
  FPinValue := '';
end;

function TStringInput.IsValidConnectionFrom(const Sender: TOutputPin): boolean;
begin
  if (Sender is TStringOutput) then exit(true);
  result := false;
end;

procedure TStringInput.SetPinValue(const Value: string);
begin
  FPinValue := Value;
  if assigned(OnChanged) then OnChanged(self);
end;

procedure TStringInput.Handle_ConnectedOutputChanged(const Sender: TOutputPin);
begin
  assert(IsValidConnectionFrom(Sender));
  if (Sender is TStringOutput) then
  begin
    self.FPinValue := (Sender as TStringOutput).PinValue;
  end else
  begin
    raise SoundElementModuleException.Create('Unexpected output type.');
  end;
  if assigned(OnChanged) then OnChanged(self);
end;

{ TFloat64StreamInput }

constructor TFloat64StreamInput.Create;
begin
  inherited;

end;

destructor TFloat64StreamInput.Destroy;
begin
  LocalPinData.Size := 0;
  inherited;
end;

function TFloat64StreamInput.IsValidConnectionFrom(const Sender: TOutputPin): boolean;
begin
  if (Sender is TFloat64StreamOutput) then exit(true);
  result := false;
end;

function TFloat64StreamInput.SetMaxBufferSize(const MaxSize: integer):TModulePin;
var
  c1: Integer;
begin
  inherited;

  LocalPinData.Size := MaxSize;
  for c1 := 0 to MaxSize-1 do
  begin
    LocalPinData.Data[c1] := 0; // initialise all data points to 0.
  end;
end;

function TFloat64StreamInput.AcceptNewConnectionFrom(const OutputPin: TOutputPin): boolean;
begin
  if (IsValidConnectionFrom(OutputPin)) and (FConnections.Count = 0)
    then result := true
    else result := false;
end;

procedure TFloat64StreamInput.ConnectTo(OutputPin: TOutputPin);
begin
  inherited;
  Handle_ConnectedOutputChanged(OutputPin);
end;

procedure TFloat64StreamInput.DisconnectFrom(OutputPin: TOutputPin);
begin
  inherited;
  FPinDataPointer := @LocalPinData;
end;

procedure TFloat64StreamInput.Handle_ConnectedOutputChanged(const OutputPin: TOutputPin);
begin
  FPinDataPointer := (OutputPin as TFloat64StreamOutput).PinData;
end;


{ TFloat64StreamOutput }

constructor TFloat64StreamOutput.Create;
begin
  inherited;
  FPinDataPointer := @PinData;
end;

destructor TFloat64StreamOutput.Destroy;
begin
  FPinDataPointer := nil;
  inherited;
end;

function TFloat64StreamOutput.SetMaxBufferSize(const MaxSize: integer):TModulePin;
begin
  inherited;

  FPinData.Size := MaxSize;
  fPinDataPointer := @FPinData;
  OutputChanged;
end;


{ Pins }

class procedure PinCommand.BreakAllConnections(const Pin: TModulePin);
var
  c1 : integer;
  xInputPin : TInputPin;
  xOutputPin : TOutputPin;
begin
  assert((Pin is TInputPin) or (Pin is TOutputPin), 'Unexpected pin type.');

  if Pin.FConnections.Count = 0 then exit;

  if (Pin is TInputPin) then
  begin
    xInputPin := (Pin as TInputPin);
    for c1 := Pin.FConnections.Count-1 downto 0 do
    begin
      xOutputPin := (Pin.FConnections[c1] as TOutputPin);
      BreakConnection(xOutputPin, xInputPin);
    end;
  end;

  if (Pin is TOutputPin) then
  begin
    xOutputPin := (Pin as TOutputPin);
    for c1 := Pin.FConnections.Count-1 downto 0 do
    begin
      xInputPin := (Pin.FConnections[c1] as TInputPin);
      BreakConnection(xOutputPin, xInputPin);
    end;
  end;
end;

class procedure PinCommand.BreakConnection(const Output: TOutputPin; Input: TInputPin);
begin
  Output.DisconnectFrom(Input);
  Input.DisconnectFrom(Output);
end;

class function PinCommand.CanMakeConnection(const Output: TOutputPin; Input: TInputPin): boolean;
begin
  result := Input.AcceptNewConnectionFrom(Output);
end;

class function PinCommand.CreatePin(const PinType: TPinType; const PinPolarity: TPinPolarity): TModulePin;
begin
  result := nil;

  case PinPolarity of
    TPinPolarity.Input:
    begin
      case PinType of
        TPinType.FloatEvent:    result := TFloatInput.Create;
        TPinType.IntegerEvent:  result := TIntegerInput.Create;
        TPinType.StringEvent:   result := TStringInput.Create;
        TPinType.Float64Stream: result := TFloat64StreamInput.Create;
      else
        raise SoundElementModuleException.Create('Unexpected type.');
      end;
    end;

    TPinPolarity.Output:
    begin
      case PinType of
        TPinType.FloatEvent:    result := TFloatOutput.Create;
        TPinType.IntegerEvent:  result := TIntegerOutput.Create;
        TPinType.StringEvent:   result := TStringOutput.Create;
        TPinType.Float64Stream: result := TFloat64StreamOutput.Create;
      else
        raise SoundElementModuleException.Create('Unexpected type.');
      end;
    end;
  else
    raise SoundElementModuleException.Create('Unexpected type.');
  end;
end;

class function PinCommand.GetConnectionCount(const Pin: TModulePin): integer;
begin
  if (Pin is TModulePin) then
  begin
    result := (Pin as TModulePin).FConnections.Count;
  end else
  begin
    assert(false, 'TODO');
    result := 0;
  end;
end;

class procedure PinCommand.MakeConnection(const Output: TOutputPin; Input: TInputPin);
begin
  if not CanMakeConnection(Output, Input) then raise SoundElementModuleException.Create('Invalid connection request.');
  Output.ConnectTo(Input);
  Input.ConnectTo(Output);
end;

function CanMakeConnection(const Output : TOutputPin; Input : TInputPin):boolean;
begin
  result := Input.AcceptNewConnectionFrom(Output);
end;

procedure MakeConnection(const Output : TOutputPin; Input : TInputPin);
begin
  if not CanMakeConnection(Output, Input) then raise SoundElementModuleException.Create('Invalid connection request.');
  Output.ConnectTo(Input);
  Input.ConnectTo(Output);
end;

procedure BreakConnection(const Output : TOutputPin; Input : TInputPin);
begin
  Output.DisconnectFrom(Input);
  Input.DisconnectFrom(Output);
end;

procedure BreakAllConnections(const Pin : TModulePin);
var
  c1 : integer;
  xInputPin : TInputPin;
  xOutputPin : TOutputPin;
begin
  assert((Pin is TInputPin) or (Pin is TOutputPin), 'Unexpected pin type.');

  if Pin.FConnections.Count = 0 then exit;

  if (Pin is TInputPin) then
  begin
    xInputPin := (Pin as TInputPin);
    for c1 := Pin.FConnections.Count-1 downto 0 do
    begin
      xOutputPin := (Pin.FConnections[c1] as TOutputPin);
      BreakConnection(xOutputPin, xInputPin);
    end;
  end;

  if (Pin is TOutputPin) then
  begin
    xOutputPin := (Pin as TOutputPin);
    for c1 := Pin.FConnections.Count-1 downto 0 do
    begin
      xInputPin := (Pin.FConnections[c1] as TInputPin);
      BreakConnection(xOutputPin, xInputPin);
    end;
  end;
end;

function GetConnectionCount(const Pin : TModulePin):integer;
begin
  if (Pin is TModulePin) then
  begin
    result := (Pin as TModulePin).FConnections.Count;
  end else
  begin
    assert(false, 'TODO');
    result := 0;
  end;
end;



end.
