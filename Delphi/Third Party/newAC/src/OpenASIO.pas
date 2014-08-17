// OpenASIO: import unit for OpenAsio.dll
//------------------------------------------------------------------------------
// Martin Fay (martin@martinfay.com), 2001
//
// Derived from iasiodrv.h (c) 1997 - 1999, Steinberg Soft- und Hardware GmbH
//
// Requires Asio.pas from the Delphi ASIO SDK translation, but you'll need that
//  anyway ;)
//
// Purpose:
// - The IASIO COM interface used by ASIO drivers has no explicitly declared
//    calling convention for the functions. This has resulted in the interface
//    being unusable from some compilers (i.e. Delphi).
// - OpenAsio provides a simple wrapper dll which exposes an interface where
//    this problem has been corrected.
//
// Usage:
// - Place OpenAsio.dll in the same directory as your application
// - Call OpenAsioLoaded to determine if the dll has loaded correctly
// - COM object creation:
//    "if OpenAsioCreate(MyAsioCLSID, MyAsioDriver) then"
// - Refer to ASIO2 documentation for usage of the interface functions
//-----------------------------------------------------------------------------------------------------------

unit OpenASIO;

interface

uses Windows, ActiveX, Asio;

type
  IOpenASIO = interface(IUnknown)
    function Init(sysHandle: HWnd): TASIOBool; stdcall;
    procedure GetDriverName(name: PChar); stdcall;
    function GetDriverVersion: longint; stdcall;
    procedure GetErrorMessage(errorString: PChar); stdcall;	
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    function GetChannels(out numInputChannels, numOutputChannels: LongWord): TASIOError; stdcall;
    function GetLatencies(out inputLatency, outputLatency: LongWord): TASIOError; stdcall;
    function GetBufferSize(out minSize, maxSize, preferredSize, granularity: LongWord): TASIOError; stdcall;
    function CanSampleRate(sampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetSampleRate(out sampleRate: TASIOSampleRate): TASIOError; stdcall;
    function SetSampleRate(sampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetClockSources(clocks: PASIOClockSource; out numSources: longint): TASIOError; stdcall;
    function SetClockSource(reference: longint): HResult; stdcall;
    function GetSamplePosition(out sPos: TASIOSamples; out tStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetChannelInfo(out info: TASIOChannelInfo): TASIOError; stdcall;
    function CreateBuffers(bufferInfos: PASIOBufferInfo; numChannels, bufferSize: longint; const callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function Future(selector: longint; opt: pointer): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
  end;

function OpenAsioLoaded: boolean;
function OpenAsioCreate(const AsioCLSID: TClsId; var OpenASIODriver: IOpenASIO): boolean;


implementation

var
  OpenAsioDll: HModule;

var
  CreateOpenAsio: function(const AsioCLSID: TClsId;
    var OpenASIODriver: IOpenASIO): HResult; stdcall;

function OpenAsioLoaded: boolean;
begin
  Result := (OpenAsioDll <> 0);
end;

function OpenAsioCreate(const AsioCLSID: TClsId; var OpenASIODriver: IOpenASIO): boolean;
begin
  if OpenAsioLoaded then
    Result := Succeeded(CreateOpenAsio(AsioCLSID, OpenASIODriver))
  else Result := false;
end;

initialization
begin
  OpenAsioDll := LoadLibrary('OpenAsio.dll');
  if OpenAsioLoaded then
    CreateOpenAsio := GetProcAddress(OpenAsioDll, 'CreateOpenAsio');
end;

finalization
begin
  FreeLibrary(OpenAsioDll);
end;

end.
