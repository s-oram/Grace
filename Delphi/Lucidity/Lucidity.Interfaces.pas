unit Lucidity.Interfaces;

interface

uses
  eeSampleFloat,
  uConstants,
  VamSampleDisplayBackBuffer,
  VamSamplePeakBuffer,
  Lucidity.Types,
  VamGuiControlInterfaces,
  LucidityModConnections,
  uSampleZeroCrossings;

type
  IVoiceController = interface
    ['{33599814-7B6A-42F6-83AB-99BB3833321C}']
  end;

  IKeyGroup = interface(IInterface)
    ['{5E91B9E8-D1BC-41E7-9686-0F24B44744C3}']

    function GetName:string;
    procedure SetName(Value : string);
    function GetObject : TObject;

    function GetID:TKeyGroupID;
    procedure SetID(ID:TKeyGroupID);

    function GetTriggeredNoteCount:cardinal;
    procedure IncTriggeredNoteCount;

    function GetModConnections:TModConnections;
    function GetModConnectionsPointer : PModConnections;
    function GetModulatedParameters : PModulatedPars;

    function GetModParValue(const ModParIndex : integer):single;
    procedure SetModParValue(const ModParIndex : integer; const Value:single);

    //TODO: this is a awkward procedure name.
    procedure SetModParModAmount(const ModParIndex, ModSlot : integer; const Value:single);
    function GetModParModAmount(const ModParIndex, ModSlot : integer):single;
    procedure GetModParModMinMax(const ModParIndex : integer; out ModMin, MoxMax:single);

    function GetSequenceData(SeqIndex : integer):IStepSequenceDataObject;
  end;


  IRegion = interface
    ['{2A1E25FA-DF90-46CE-BA90-EB69EDAE57F4}']
    function GetObject:TObject;
    function GetProperties    : PRegionProperties;
    function GetSample        : PSampleFloat;
    function GetKeyGroup      : IKeyGroup;
    function GetZeroCrossings : TSampleZeroCrossings;
    function GetSampleImage   : ISampleImageBuffer;
    function GetPeakBuffer    : IPeakBuffer;

    function GetDbLevelAt(SamplePoint:integer):single;

    procedure UpdateSampleImage;
  end;







implementation

end.
