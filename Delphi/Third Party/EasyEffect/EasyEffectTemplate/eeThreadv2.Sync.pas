{
  NOTE: OtlSync contains Compare-and-swap functions which may be useful.
}

unit eeThreadv2.Sync;

interface

uses
  Contnrs, WinApi.Windows;

type
  // TeeCritcalSection needs to be replaced by TFixedCriticalSection.
  // It is found in VamLib.Types.pas

  TeeCriticalSection = class
  private
    cs : TRTLCriticalSection;
    // NOTE: There is an isuue with critical sections that require them to have
    // some padding. AFAIK the padding increases the class size so that
    // multiple classes can't fit in the same CPU-core cache.
    // http://delphitools.info/2011/11/30/fixing-tcriticalsection/
    FDummy : array [0..95] of Byte;
  public
    constructor Create;
    destructor Destroy; override;

    function TryEnter: Boolean;
    procedure Enter;
    procedure Leave;
  end;

implementation

{ TCriticalSection }

constructor TeeCriticalSection.Create;
begin
  InitializeCriticalSection(cs);
end;

destructor TeeCriticalSection.Destroy;
begin
  DeleteCriticalSection(cs);
  inherited;
end;

procedure TeeCriticalSection.Enter;
begin
   EnterCriticalSection(cs);
end;

procedure TeeCriticalSection.Leave;
begin
  LeaveCriticalSection(cs);
end;

function TeeCriticalSection.TryEnter: Boolean;
begin
  Result := TryEnterCriticalSection(cs);
end;



end.
