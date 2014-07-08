unit eeMidiAutomationV2;

interface

uses
  Contnrs,
  VamLib.Types, VamLib.ZeroObject,
  Classes, eeMidiInputSmoother;

type
  ICustomMidiBinding = interface(IInterface)
    ['{CFBF226A-5545-469C-9DE2-C9458256D358}']
    function GetMidiCC : integer;
    procedure SetMidiCC(const Value : integer);
  end;

  TCustomMidiBinding = class(TInterfacedObject, ICustomMidiBinding)
  strict private
    fMidiCC: integer;
    function GetMidiCC : integer;
    procedure SetMidiCC(const Value : integer);
  public
    property MidiCC  : integer read fMidiCC write fMidiCC;
  end;

  TMidiAutomationEvent = procedure(Sender : TObject; const MidiData1, MidiData2 : integer; const Binding : ICustomMidiBinding) of object;

  TCustomMidiAutomation = class(TZeroObject)
  private
    fOnMidiAutomation: TMidiAutomationEvent;
    fSampleRate: single;
    fOnNewBinding: TMidiAutomationEvent;
    procedure SetSampleRate(const Value: single);
    function GetIsMidiLearnActive: boolean;
  protected
    MidiLearnTarget : ICustomMidiBinding;
    BindingList : TInterfaceList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // NOTE: ActivateMidiLearn() takes ownership of the TargetBinding object.
    procedure ActivateMidiLearn(const TargetBinding : ICustomMidiBinding);
    procedure CancelMidiLearn;

    procedure AddBinding(const aBinding : ICustomMidiBinding);
    procedure Clear;
    function BindingCount : integer;

    procedure ProcessMidiCC(Data1,Data2:byte);
    procedure FastControlProcess; inline;

    property IsMidiLearnActive : boolean read GetIsMidiLearnActive;
    property SampleRate : single read fSampleRate write SetSampleRate;

    property OnMidiMessage : TMidiAutomationEvent read fOnMidiAutomation write fOnMidiAutomation;
    property OnNewBinding  : TMidiAutomationEvent read fOnNewBinding     write fOnNewBinding;
  end;




implementation

uses
  SysUtils;

{ TCustomMidiBinding }

function TCustomMidiBinding.GetMidiCC: integer;
begin
  result := fMidiCC;
end;

procedure TCustomMidiBinding.SetMidiCC(const Value: integer);
begin
  fMidiCC := Value;
end;




{ TCustomMidiAutomation }

constructor TCustomMidiAutomation.Create;
begin
  BindingList := TInterfaceList.Create;
end;

destructor TCustomMidiAutomation.Destroy;
begin
  BindingList.Free;

  inherited;
end;

function TCustomMidiAutomation.GetIsMidiLearnActive: boolean;
begin
  if assigned(MidiLearnTarget)
    then result := true
    else result := false;
end;

procedure TCustomMidiAutomation.SetSampleRate(const Value: single);
begin
  fSampleRate := Value;
end;

procedure TCustomMidiAutomation.Clear;
begin
  BindingList.Clear;
end;

procedure TCustomMidiAutomation.AddBinding(const aBinding: ICustomMidiBinding);
begin
  BindingList.Add(aBinding);
end;

function TCustomMidiAutomation.BindingCount: integer;
begin
  result := BindingList.Count;
end;

procedure TCustomMidiAutomation.ActivateMidiLearn(const TargetBinding: ICustomMidiBinding);
begin
  MidiLearnTarget := TargetBinding;
end;

procedure TCustomMidiAutomation.CancelMidiLearn;
begin
  MidiLearnTarget := nil;
end;

procedure TCustomMidiAutomation.ProcessMidiCC(Data1, Data2: byte);
var
  c1: Integer;
  mb : ICustomMidiBinding;
begin
  // Just check that OnMidiAutomation has been assigned. It should
  // be otherwise there is no use using the MIDI automation class.
  assert(assigned(OnMidiMessage));
  assert(assigned(OnNewBinding));


  mb := MidiLearnTarget;
  if assigned(mb) then
  begin
    mb.SetMidiCC(Data1);

    // Call the OnNewBinding() event to give the plugin
    // a chance to update the binding with relevant information
    // before it is 'learnt'.
    OnNewBinding(self, Data1, Data2, mb);

    BindingList.Add(mb);
    MidiLearnTarget := nil;
    mb := nil;
  end;


  for c1 := BindingList.Count-1 downto 0 do
  begin
    if Data1 = (BindingList[c1] as ICustomMidiBinding).GetMidiCC then
    begin
      OnMidiMessage(self, Data1, Data2, (BindingList[c1] as ICustomMidiBinding));
    end;
  end;

end;

procedure TCustomMidiAutomation.FastControlProcess;
begin

end;




end.
