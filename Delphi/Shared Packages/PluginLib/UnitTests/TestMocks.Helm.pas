unit TestMocks.Helm;

interface

uses
  Helm.Message;

type
  TMsgA = class(TAppMessage)
  public
    class function CreateMessage:THelmMessage;
  end;

  TMsgB = class(TAppMessage)
  public
    class function CreateMessage:THelmMessage;
  end;

  TMsgC = class(TAppMessage)
  public
    class function CreateMessage:THelmMessage;
  end;



implementation

{ TMsgA }

class function TMsgA.CreateMessage: THelmMessage;
begin
  result := THelmMessage.Create(TMsgA);
end;

{ TMsgB }

class function TMsgB.CreateMessage: THelmMessage;
begin
  result := THelmMessage.Create(TMsgB);
end;

{ TMsgC }

class function TMsgC.CreateMessage: THelmMessage;
begin
  result := THelmMessage.Create(TMsgC);
end;

end.
