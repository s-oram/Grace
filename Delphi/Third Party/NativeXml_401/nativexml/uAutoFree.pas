{
  The AutoFree() procedure allows objects to be free'ed somewhat automatically.

  It is useful to help manage objects created for temporary use in procedures.
  Traditionally a try...finally block would be used, but code quickly looses
  readability with nested try...finally blocks.

  For more information on this class and method see this blog post.
  Te Waka o Delphi "Free Yourself" by Joylon Smith.
  http://www.deltics.co.nz/blog/?p=391


  ---------------------------------------------------
  Using the AutoFree construct...
  ---------------------------------------------------

   procedure DoSomething;
   var
     list: TStringList;
   begin
     list := TStringList.Create;
     AutoFree(@list);
     // Do work with list
   end;

  ---------------------------------------------------
}


unit uAutoFree;

interface

type
  PObject = ^TObject;

  function AutoFree(const aObject: PObject): IUnknown;

implementation

uses
  SysUtils;

type
  TAutoFree = class(TInterfacedObject, IUnknown)
  private
    fObject: PObject;
  public
    constructor Create(const aObject: PObject);
    destructor Destroy; override;
  end;

constructor TAutoFree.Create(const aObject: PObject);
begin
  inherited Create;
  fObject := aObject;
end;

destructor TAutoFree.Destroy;
begin
  FreeAndNIL(fObject^);
  inherited;
end;

{ - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - }
function AutoFree(const aObject: PObject): IUnknown;
begin
  result := TAutoFree.Create(aObject);
end;

end.

