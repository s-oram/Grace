unit eeMidiAutomationV2;

interface

uses
  Contnrs,
  VamLib.Types, VamLib.ZeroObject,
  Classes, eeMidiInputSmoother;

type
  TCustomMidiBinding = class
  strict private
    fMidiCC: integer;
  public
    property MidiCC  : integer read fMidiCC write fMidiCC;
  end;

  TMidiAutomationEvent = procedure(Sender : TObject; const MidiData1, MidiData2 : integer; const Binding : TCustomMidiBinding);

  TCustomMidiAutomation = class(TZeroObject)
  private
    fIsMidiLearnActive: boolean;
    fOnMidiAutomation: TMidiAutomationEvent;
  protected
    MidiLearnLock : TFixedCriticalSection;
    MidiLearnTarget : TCustomMidiBinding;

    BindingList : TObjectList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // NOTE: ActivateMidiLearn() takes ownership of the TargetBinding object.
    procedure ActivateMidiLearn(const TargetBinding : TCustomMidiBinding);
    procedure CancelMidiLearn;

    procedure AddBinding(const aBinding : TCustomMidiBinding);

    function BindingCount : integer;

    procedure Clear;

    procedure ProcessMidiCC(Data1,Data2:byte);

    property IsMidiLearnActive : boolean read fIsMidiLearnActive;

    property OnMidiAutomation : TMidiAutomationEvent read fOnMidiAutomation write fOnMidiAutomation;
  end;



  TMidiBinding = class(TCustomMidiBinding)
  private
    fParName: string;
  public
    property ParName : string read fParName write fParName;
  end;

  TMidiAutomation = class(TCustomMidiAutomation)
  private
  protected
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TCustomMidiAutomation }

constructor TCustomMidiAutomation.Create;
begin
  BindingList := TObjectList.Create;
  BindingList.OwnsObjects := true;

  MidiLearnLock := TFixedCriticalSection.Create;
end;

destructor TCustomMidiAutomation.Destroy;
begin
  BindingList.Free;
  MidiLearnLock.Free;
  inherited;
end;

procedure TCustomMidiAutomation.Clear;
begin
  BindingList.Clear;
end;

procedure TCustomMidiAutomation.AddBinding(const aBinding: TCustomMidiBinding);
begin

end;

function TCustomMidiAutomation.BindingCount: integer;
begin
  result := BindingList.Count;
end;

procedure TCustomMidiAutomation.ActivateMidiLearn(const TargetBinding: TCustomMidiBinding);
begin
  assert(assigned(TargetBinding));

  MidiLearnLock.Acquire;
  try
    if assigned(MidiLearnTarget)
      then FreeAndNil(MidiLearnTarget);

    MidiLearnTarget := TargetBinding;

    fIsMidiLearnActive := true;
  finally
    MidiLearnLock.Release;
  end;
end;

procedure TCustomMidiAutomation.CancelMidiLearn;
begin
  MidiLearnLock.Acquire;
  try
    fIsMidiLearnActive := false;

    if assigned(MidiLearnTarget)
      then FreeAndNil(MidiLearnTarget);
  finally
    MidiLearnLock.Release;
  end;
end;

procedure TCustomMidiAutomation.ProcessMidiCC(Data1, Data2: byte);
var
  c1: Integer;
begin
  // Just check that OnMidiAutomation has been assigned. It should
  // be otherwise there is no use using the MIDI automation class.
  assert(assigned(OnMidiAutomation));

  if (fIsMidiLearnActive) then
  begin
    MidiLearnLock.Acquire;
    try
      fIsMidiLearnActive := false;

      if assigned(MidiLearnTarget) then
      begin
        MidiLearnTarget.MidiCC := Data1;
        BindingList.Add(MidiLearnTarget);
        MidiLearnTarget := nil;
      end;
    finally
      MidiLearnLock.Release;
    end;
  end;


  for c1 := BindingList.Count-1 downto 0 do
  begin
    if Data1 = (BindingList[c1] as TCustomMidiBinding).MidiCC then
    begin
      OnMidiAutomation(self, Data1, Data2, (BindingList[c1] as TCustomMidiBinding));
    end;
  end;

end;

{ TMidiAutomation }

constructor TMidiAutomation.Create;
begin
  inherited;

end;

destructor TMidiAutomation.Destroy;
begin

  inherited;
end;

end.
