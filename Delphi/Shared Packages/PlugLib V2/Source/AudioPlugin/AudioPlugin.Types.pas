unit AudioPlugin.Types;

interface


uses
  SysUtils;

type
  EAudioPluginException = class(Exception);

  TPrivateParResult = record
  public
    ValueA : integer;
    ValueB : single;
    Data   : array of pointer;
  end;


  // TExceptionHandlerFunc() - return true if the execption has been handled. return false and the exception will
  // normally be re-raised from the call site.
  TExceptionHandlerFunc = reference to function:boolean;


implementation

end.
