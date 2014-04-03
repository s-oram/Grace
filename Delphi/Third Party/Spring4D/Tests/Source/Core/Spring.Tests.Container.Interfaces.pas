unit Spring.Tests.Container.Interfaces;

interface

type
  INameService = interface
    ['{1CB49D6A-660F-4D33-899C-60F1162D4D37}']
    function GetName: string;
    property Name: string read GetName;
  end;

implementation

end.
