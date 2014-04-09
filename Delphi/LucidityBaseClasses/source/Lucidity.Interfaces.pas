unit Lucidity.Interfaces;

interface

type
  IVectorSequenceDataObject = interface
    ['{2CB04233-08C0-425A-B122-C8D476A5D50F}']
    function GetStepValue(Index : integer):single;
    procedure SetStepValue(Index : integer; const Value:single);
  end;




implementation

end.
