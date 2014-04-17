unit VamLib.LoggingProxy;

interface

type
  ILoggingProxy = interface
    ['{1751C9A9-DC7E-4BF5-ABDD-66F7876C5163}']
    procedure LogMessage(const aTitle : string);
  end;



  Log = class
  private
  public
    class procedure SetProxy(aProxy : ILoggingProxy);
    class procedure ClearProxy;

    class procedure LogMessage(const aTitle : string);
  end;






implementation


//==============================================================================
//==============================================================================

type
  TDoNothingProxy = class(TInterfacedObject, ILoggingProxy)
  private
    procedure LogMessage(const aTitle : string);
  end;


{ TDoNothingProxy }

procedure TDoNothingProxy.LogMessage(const aTitle: string);
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


class procedure Log.LogMessage(const aTitle: string);
begin
  GlobalProxy.LogMessage(aTitle);
end;




initialization
  Log.ClearProxy;
finalization
  GlobalProxy := nil;

end.
