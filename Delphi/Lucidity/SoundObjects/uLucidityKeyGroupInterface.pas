unit uLucidityKeyGroupInterface;

interface

uses
  uConstants,
  LucidityModConnections;

type
  IKeyGroup = interface(IInterface)
    ['{5E91B9E8-D1BC-41E7-9686-0F24B44744C3}']

    function GetName:string;
    procedure SetName(Value : string);
    function GetObject : TObject;

    function GetTriggeredNoteCount:cardinal;
    procedure IncTriggeredNoteCount;

    function GetModConnections_OLD:TModConnections_OLD;
    function GetModConnections:TModConnections;
    function GetModConnectionsPointer : PModConnections;
    function GetModulatedParameters : PModulatedPars;

    function GetModParValue(const ModParIndex : integer):single;
    procedure SetModParValue(const ModParIndex : integer; const Value:single);
    //TODO: this is a awkward procedure name.
    procedure SetModParModAmount(const ModParIndex, ModSlot : integer; const Value:single);
  end;

implementation

end.
