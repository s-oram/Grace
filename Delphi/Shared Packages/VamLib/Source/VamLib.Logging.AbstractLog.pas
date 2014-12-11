unit VamLib.Logging.AbstractLog;

interface

type
  ITrackMethod = interface
    ['{F84BCE54-9594-4605-9468-F9AB792DC707}']
  end;

  ITrackMethodTime = interface
    ['{BA9C746A-9ED1-4C8B-9A35-0D99113A3E86}']
  end;

  IAbstractLog = interface
    ['{D85AE125-B21C-42EB-9737-2CB3A236B8E8}']
    procedure LogTime(const aTitle : string);
    procedure LogMessage(const aTitle : string);
    procedure LogError(const aTitle : string);
    procedure LogText(const aTitle, aText: string);

    procedure EnterMethod(const aMethodName : string);
    procedure LeaveMethod(const aMethodName : string);

    function TrackMethod(const aMethodName : string):ITrackMethod;
    function TrackMethodTime(const aMethodName : string):ITrackMethodTime;
  end;

  TDoNothingLog = class(TInterfacedObject, IAbstractLog)
  private
  public
    procedure LogTime(const aTitle : string);
    procedure LogMessage(const aTitle : string);
    procedure LogError(const aTitle : string);
    procedure LogText(const aTitle, aText: string);

    procedure EnterMethod(const aMethodName : string);
    procedure LeaveMethod(const aMethodName : string);

    function TrackMethod(const aMethodName : string):ITrackMethod;
    function TrackMethodTime(const aMethodName : string):ITrackMethodTime;
  end;

implementation

{ TDoNothingLog }

procedure TDoNothingLog.EnterMethod(const aMethodName: string);
begin

end;

procedure TDoNothingLog.LeaveMethod(const aMethodName: string);
begin

end;

procedure TDoNothingLog.LogError(const aTitle: string);
begin

end;

procedure TDoNothingLog.LogMessage(const aTitle: string);
begin

end;

procedure TDoNothingLog.LogText(const aTitle, aText: string);
begin

end;

procedure TDoNothingLog.LogTime(const aTitle: string);
begin

end;

function TDoNothingLog.TrackMethod(const aMethodName: string): ITrackMethod;
begin
  result := nil;
end;

function TDoNothingLog.TrackMethodTime(const aMethodName: string): ITrackMethodTime;
begin
  result := nil;
end;

end.
