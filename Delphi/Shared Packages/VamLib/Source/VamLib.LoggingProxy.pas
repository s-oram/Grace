unit VamLib.LoggingProxy;

interface

type
  ILoggingProxy = interface
    ['{1751C9A9-DC7E-4BF5-ABDD-66F7876C5163}']
    procedure LogMessage(const aTitle : string);
    procedure LogError(const aTitle : string);
    procedure TrackMethod(const aMethodName : string);
  end;



  Log = class
  private
  public
    class procedure SetProxy(aProxy : ILoggingProxy);
    class procedure ClearProxy;

    class procedure LogMessage(const aTitle : string);
    class procedure LogError(const aTitle : string);
    class procedure TrackMethod(const aMethodName : string);
  end;


implementation


//==============================================================================
//==============================================================================

type
  TDoNothingProxy = class(TInterfacedObject, ILoggingProxy)
  private
    procedure LogMessage(const aTitle : string);
    procedure LogError(const aTitle : string);
    procedure TrackMethod(const aMethodName : string);
  end;


{ TDoNothingProxy }

procedure TDoNothingProxy.LogError(const aTitle: string);
begin
end;

procedure TDoNothingProxy.LogMessage(const aTitle: string);
begin
end;



procedure TDoNothingProxy.TrackMethod(const aMethodName: string);
begin

end;

//==============================================================================
//==============================================================================

var
  GlobalProxy : ILoggingProxy;

{ Log }

class procedure Log.SetProxy(aProxy: ILoggingProxy);
begin
  GlobalProxy := aProxy;
end;

class procedure Log.ClearProxy;
begin
  GlobalProxy := TDoNothingProxy.Create;
end;

class procedure Log.TrackMethod(const aMethodName: string);
begin
  GlobalProxy.TrackMethod(aMethodName);
end;

class procedure Log.LogError(const aTitle: string);
begin
  GlobalProxy.LogError(aTitle);
end;

class procedure Log.LogMessage(const aTitle: string);
begin
  GlobalProxy.LogMessage(aTitle);
end;




initialization
  Log.ClearProxy;
finalization
  GlobalProxy := nil;

end.
