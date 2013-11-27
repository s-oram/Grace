unit VamSyncObjects;

interface

uses
  SyncObjs;

type

  // NOTE: On TFixedCriticalSection
  // http://delphitools.info/2011/11/30/fixing-tcriticalsection/
  TFixedCriticalSection = class(TCriticalSection)
  private
    FDummy : array [0..95] of Byte;
  end;

implementation

end.
