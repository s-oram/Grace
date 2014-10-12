unit VamLib.DuckType;


{
  =========== ABOUT ========================

  The functionality in this unit is based on ideas put forward by:
    DuckDuckDelphi
    https://code.google.com/p/duckduckdelphi/

    Delphi Cookbook by  Daniele Teti
    http://www.amazon.com/Delphi-Cookbook-Daniele-Teti/dp/1783559586
    NOTE: I haven't read this. Only looked at the preview in Amazon.
    There is a section on duck typing.

  NOTE: Lots of code in this unit has been copied from DuckDuckDelphi.
  DuckDuckDelphi wouldn't compile with my version of XE2. The errors
  were over my head. Fortunatly I only need a small subset of the functionality
  in DuckDuckDelphi, so I've copied the revelant sections and modifed
  things slightly to fit my way of working.

  ==== Example ====
  Instead of
    Panel2.Color := clGreen
  do
    Panel2.Duck.SetProperty('Color', clGreen);



  ========= USFUL LINKS AND OTHER NOTES =====================

  Get method address of private method
  http://stackoverflow.com/q/10156430/395461


  Detour Externally Declared Function
  http://stackoverflow.com/questions/6905287/how-to-change-the-implementation-detour-of-an-externally-declared-function
  http://stackoverflow.com/a/6905461/395461

}

interface

uses
  TypInfo,
  System.Rtti;

type
  // IDuck is a fluent interface and method calls can be chained.

  IDuck = interface
    // calling RequireTarget before a Set() call will ensure the
    // target property exists. An exception will be raised if not.
    //   example: MyObject.Duck.RequireTarget.SetEvent('Color', clBlue);
    function RequireTarget:IDuck;

    function HasProperty(const Propertyname : string):boolean;
    function HasEvent(const EventName : string):boolean;
    function HasMethod(const MethodName : string):boolean;

    function SetProperty(const PropertyName : string; Value : TValue):IDuck;
    function SetEvent(const EventName : string; Handler : TMethod):IDuck; overload;
    function SetEvent(const EventName : string; Obj : TObject; MethodAddress : Pointer):IDuck; overload;
    function SetEvent(const EventName : string; Obj : TObject; MethodName : string):IDuck; overload;
    function ClearEvent(const EventName : string):IDuck; //removes an event handler.
  end;

  TDuckHelper = class helper for TObject
  public
    function Duck : IDuck;
  end;



implementation

uses
  SysUtils;

type
  TDuck = class(TInterfacedObject, IDuck)
  private
    FOwner : TObject;
    fRequireTarget : boolean;
  protected
    procedure EnsurePropertyExists(const Propertyname : string);

    //=== methods exposed by IDuck =====
    function RequireTarget:IDuck;

    function HasProperty(const Propertyname : string):boolean;
    function HasEvent(const EventName : string):boolean;
    function HasMethod(const MethodName : string):boolean;

    function SetProperty(const PropertyName : string; Value : TValue):IDuck;
    function SetEvent(const EventName : string; Handler : TMethod):IDuck; overload;
    function SetEvent(const EventName : string; Obj : TObject; MethodAddress : Pointer):IDuck; overload;
    function SetEvent(const EventName : string; Obj : TObject; MethodName : string):IDuck; overload;
    function ClearEvent(const EventName : string):IDuck;
  public
     constructor Create(AOwner: TObject); virtual;
  end;

  TRttiWrapper = class
    class procedure SetValue(obj: TObject; const PropertyName: string; Value : TValue); static;
    class function IsProperty(obj: TObject; const PropertyName: string) : boolean; static;
    class function IsMethod(obj: TObject; const MethodName: string) : boolean; static;
  end;




{ TDuck }

constructor TDuck.Create(AOwner: TObject);
begin
  fRequireTarget := false;
  FOwner := AOwner;
end;

procedure TDuck.EnsurePropertyExists(const Propertyname: string);
begin
  if TRttiWrapper.IsProperty(FOwner, Propertyname) = false
    then raise Exception.Create('Property (' + PropertyName + ') doesn''t exist and is required.');
end;

function TDuck.RequireTarget: IDuck;
begin
  fRequireTarget := true;
  result := self;
end;

function TDuck.HasEvent(const EventName: string): boolean;
begin
  result := TRttiWrapper.IsProperty(FOwner, EventName);
end;

function TDuck.HasMethod(const MethodName: string): boolean;
begin
  //NOTE: This function is untested.
  result := TRttiWrapper.IsMethod(FOwner, MethodName);
end;

function TDuck.HasProperty(const Propertyname: string): boolean;
begin
  result := TRttiWrapper.IsProperty(FOwner, PropertyName);
end;

function TDuck.SetEvent(const EventName: string; Handler: TMethod): IDuck;
begin
  if fRequireTarget then EnsurePropertyExists(EventName);
  if Handler.Code = nil then raise Exception.Create('TDuck.SetEvent() Handler.Code is not assigned.');
  if Handler.Data = nil then raise Exception.Create('TDuck.SetEvent() Handler.Data is not assigned.');
  SetMethodProp(FOwner, EventName, Handler);
  result := self;
end;

function TDuck.SetEvent(const EventName: string; Obj : TObject; MethodAddress: Pointer): IDuck;
var
  m : TMethod;
begin
  if fRequireTarget then EnsurePropertyExists(EventName);
  if not assigned(Obj) then raise Exception.Create('TDuck.SetEvent() Obj is not assigned.');
  if MethodAddress = nil then raise Exception.Create('TDuck.SetEvent() MethodAddress is not assigned.');
  m.Code := MethodAddress;
  m.Data := @Obj;
  SetMethodProp(FOwner, EventName, m);
  result := self;
end;

function TDuck.SetEvent(const EventName: string; Obj: TObject; MethodName: string): IDuck;
// IMPORTANT: The MethodName target must be published with runtime type information enabled. {+M}
var
  m : TMethod;
begin
  if fRequireTarget then EnsurePropertyExists(EventName);
  if not assigned(Obj) then raise Exception.Create('TDuck.SetEvent() Obj is not assigned.');
  m.Data := @Obj;
  m.Code := Obj.MethodAddress(MethodName);
  if m.Code = nil then raise Exception.Create('TDuck.SetEvent() Method address not found.');
  SetMethodProp(FOwner, EventName, m);
  result := self;
end;

function TDuck.SetProperty(const PropertyName: string; Value: TValue): IDuck;
begin
  if fRequireTarget then EnsurePropertyExists(PropertyName);
  TRttiWrapper.SetValue(FOwner, PropertyName, Value);
  result := self;
end;

function TDuck.ClearEvent(const EventName: string): IDuck;
var
  m : TMethod;
begin
  if fRequireTarget then EnsurePropertyExists(EventName);
  m.Code := nil;
  m.Data := nil;
  SetMethodProp(FOwner, EventName, m);
  result := self;
end;

{ TRttiWrapper }

class function TRttiWrapper.IsMethod(obj: TObject; const MethodName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := cxt.GetType(obj.ClassInfo).GetMethod(MethodName) <> nil;
end;

class function TRttiWrapper.IsProperty(obj: TObject;  const PropertyName: string): boolean;
var
  cxt : TRTTIContext;
begin
  Result := (cxt.GetType(obj.ClassInfo).GetProperty(PropertyName) <> nil) or
            (cxt.GetType(obj.ClassInfo).GetIndexedProperty(PropertyName) <> nil);
end;

class procedure TRttiWrapper.SetValue(obj: TObject; const PropertyName: string; Value: TValue);
var
  cxt : TRTTIContext;
  prop : TRttiProperty;
begin
  prop := cxt.GetType(obj.ClassInfo).GetProperty(PropertyName);
  if prop <> nil then
  begin
    prop.SetValue(obj, Value);
  end;
end;


{ TDuckHelper }

function TDuckHelper.Duck: IDuck;
begin
  result := TDuck.Create(self);
end;


//========== helper functions ============

{
type
  TNotifyEventReference = reference to procedure(Sender : TObject);

function EventToMethod(Event : TNotifyEventReference):TMethod;
var
  ptr : Pointer;
begin
  ptr := TMethod(Event).Code;
  //result.Data := TMethod(Event).Data;
end;
}






end.
