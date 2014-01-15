unit uLucidityKeyGroupInterface;

interface

uses
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
  end;

implementation

end.
