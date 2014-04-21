unit eeSyncObjects;

interface

uses
  SyncObjs;

type
  // NOTE: On TFixedCriticalSection
  // http://delphitools.info/2011/11/30/fixing-tcriticalsection/
  TFixedCriticalSection = class(TCriticalSection)
  private
    {$Hints Off}
    FDummy : array [0..95] of Byte;
    {$Hints On}
  end;

implementation

end.
