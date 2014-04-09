unit LucidityInterfaces;

interface

uses
  eeSampleFloat,
  uConstants,
  VamSampleDisplayBackBuffer,
  VamSamplePeakBuffer,
  Lucidity.Types,
  Lucidity.Interfaces,
  LucidityModConnections,
  uSampleZeroCrossings;

type
  IKeyGroup = interface(IInterface)
    ['{5E91B9E8-D1BC-41E7-9686-0F24B44744C3}']

    function GetName:string;
    procedure SetName(Value : string);
    function GetObject : TObject;

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

    function GetSequenceData(SeqIndex : integer):IVectorSequenceDataObject;
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
