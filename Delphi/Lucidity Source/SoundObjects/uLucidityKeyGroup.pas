unit uLucidityKeyGroup;

interface

uses
  uLucidityKeyGroupInterface, LucidityModConnections,
  MoreTypes, eeGlobals,
  eeVoiceLogic, eeVstParameter,
  eeVstParameterList, eePatchObject,
  uConstants,
  uSampleMap,
  soLucidityVoice, soLucidityVoiceParameterWrapper,
  uModularConnectionManager,
  uLucidityStepSequencer,
  uGuiFeedbackData;

type
  // redeclare ISampleGroup for convenience.
  IKeyGroup = uLucidityKeyGroupInterface.IKeyGroup;



type
  TKeyGroup = class;

  TKeyGroup = class(TInterfacedObject, IKeyGroup)
  private
    fTriggeredNoteCount : cardinal;
    fName     : string;
    fSampleMap: TSampleMap;
    fVoiceParameters: TLucidityVoiceParameterWrapper;
    fModConnections : TModConnections;
    function GetVoiceParameters : TLucidityVoiceParameterWrapper;
    function GetObject : TObject;
    function GetTriggeredNoteCount:cardinal;
    procedure IncTriggeredNoteCount;
    function GetModConnections:TModConnections;
  protected
    Globals : TGlobals;
    GlobalModPoints : PGlobalModulationPoints;

    Voices : PArrayOfLucidityVoice;

    function GetName:string;
    procedure SetName(Value : string);

    //TODO: Delete this.
    property SampleMap : TSampleMap read fSampleMap write fSampleMap;
  public
    constructor Create(const aVoices:PArrayOfLucidityVoice; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
    destructor Destroy; override;

    procedure AssignFrom(const Source : TKeyGroup);

    procedure SetPatch(var Data:TPatchNode);
    procedure GetPatch(var Data:TPatchNode);

    procedure GetGuiFeedBack(const FeedbackData:TGuiFeedBackData);
    procedure GetFilterInfo(const Info : PFilterParameterInfo);

    property VoiceParameters : TLucidityVoiceParameterWrapper read fVoiceParameters;
    property ModConnections  : TModConnections                read fModConnections;

    property Name : string read fName;




  end;

implementation

uses
  SysUtils,
  uLucidityEnums;

{ TLucidityEngine }

constructor TKeyGroup.Create(const aVoices:PArrayOfLucidityVoice; const aGlobalModPoints : PGlobalModulationPoints; const aGlobals: TGlobals);
var
  c1: Integer;
begin
  fModConnections := TModConnections.Create;

  Globals := aGlobals;
  GlobalModPoints := aGlobalModPoints;

  Voices := aVoices;

  fVoiceParameters := TLucidityVoiceParameterWrapper.Create(aVoices, self);

  //== Init some values ==
  for c1 := 0 to kMaxStepSequencerLength-1 do
  begin
    VoiceParameters.Seq1StepValue[c1] := 0;
  end;

  fTriggeredNoteCount := 0;
end;

destructor TKeyGroup.Destroy;
begin
  fVoiceParameters.Free;
  fModConnections.Free;
  inherited;
end;

procedure TKeyGroup.AssignFrom(const Source: TKeyGroup);
begin
  self.VoiceParameters.AssignFrom(Source.VoiceParameters);
end;





procedure TKeyGroup.GetPatch(var Data: TPatchNode);
//var
//  ChildModule : TPatchNode;
begin
  //ChildModule := Data.NewChildNode('SampleMap');
  //SampleMap.GetPatchData(ChildModule);
end;


function TKeyGroup.GetTriggeredNoteCount: cardinal;
begin
  result := fTriggeredNoteCount;
end;

function TKeyGroup.GetVoiceParameters: TLucidityVoiceParameterWrapper;
begin
  result := fVoiceParameters;
end;

procedure TKeyGroup.IncTriggeredNoteCount;
begin
  inc(fTriggeredNoteCount);
end;

procedure TKeyGroup.SetPatch(var Data: TPatchNode);
//var
//  ChildModule : TPatchNode;
begin
  //ChildModule := Data.FindChildNode('SampleMap');
  //if assigned(ChildModule) then SampleMap.SetPatchData(ChildModule);
end;

procedure TKeyGroup.GetFilterInfo(const Info: PFilterParameterInfo);
  procedure GetFullName(const FilterType : TFilterType; ps1, ps2, ps3 : PString);
  begin
    case FilterType of
      ftNone:
      begin
        ps1^ := '-';
        ps2^ := '-';
        ps3^ := '-';
      end;

      ftLowPassA, ftBandPassA, ftHighPassA:
      begin
        ps1^ := 'HZ';
        ps2^ := 'RES';
        ps3^ := '';
      end;

      ftLofiA:
      begin
        ps1^ := 'Sample Rate';
        ps2^ := 'Bit Depth';
        ps3^ := '';
      end;

      ftRingModA:
      begin
        ps1^ := 'Mod Freq';
        ps2^ := 'Mod Depth';
        ps3^ := '';
      end;

      {
      ftDistA:
      begin
        ps1^ := '';
        ps2^ := 'Input Gain';
        ps3^ := 'Output Gain';
      end;
      }
      ftCombA:
      begin
        ps1^ := 'Frequency';
        ps2^ := 'Feedback';
        ps3^ := '';
      end;

    else
      raise Exception.Create('Filter type not handled.');
    end;
  end;

  procedure GetShortName(const FilterType : TFilterType; ps1, ps2, ps3 : PString);
  begin
    case FilterType of
      ftNone:
      begin
        ps1^ := '';
        ps2^ := '';
        ps3^ := '';
      end;

      ftLowPassA, ftBandPassA, ftHighPassA:
      begin
        ps1^ := 'HZ';
        ps2^ := 'RES';
        ps3^ := '';
      end;

      ftLofiA:
      begin
        ps1^ := 'SR';
        ps2^ := 'BITS';
        ps3^ := '';
      end;

      ftRingModA:
      begin
        ps1^ := 'HZ';
        ps2^ := 'AMT';
        ps3^ := '';
      end;

      {
      ftDistA:
      begin
        ps1^ := '';
        ps2^ := 'G1';
        ps3^ := 'G2';
      end;
      }
      ftCombA:
      begin
        ps1^ := 'HZ';
        ps2^ := 'FB';
        ps3^ := '';
      end;

    else
      raise Exception.Create('Filter type not handled.');
    end;

  end;
var
  ps1, ps2, ps3 : PString;
begin
  ps1 := @Info.Filter1Par1FullName;
  ps2 := @Info.Filter1Par2FullName;
  ps3 := @Info.Filter1Par3FullName;

  GetFullName(VoiceParameters.Filter1Type, ps1, ps2, ps3);

  ps1 := @Info.Filter1Par1ShortName;
  ps2 := @Info.Filter1Par2ShortName;
  ps3 := @Info.Filter1Par3ShortName;

  GetShortName(VoiceParameters.Filter1Type, ps1, ps2, ps3);

  ps1 := @Info.Filter2Par1FullName;
  ps2 := @Info.Filter2Par2FullName;
  ps3 := @Info.Filter2Par3FullName;

  GetFullName(VoiceParameters.Filter2Type, ps1, ps2, ps3);

  ps1 := @Info.Filter2Par1ShortName;
  ps2 := @Info.Filter2Par2ShortName;
  ps3 := @Info.Filter2Par3ShortName;

  GetShortName(VoiceParameters.Filter2Type, ps1, ps2, ps3);
end;

procedure TKeyGroup.GetGuiFeedBack(const FeedbackData: TGuiFeedBackData);
begin
end;

function TKeyGroup.GetModConnections: TModConnections;
begin
  result := fModConnections;
end;

procedure TKeyGroup.SetName(Value: string);
begin
  fName := Value;
end;

function TKeyGroup.GetName: string;
begin
  result := fName;
end;

function TKeyGroup.GetObject: TObject;
begin
  result := self;
end;

end.
