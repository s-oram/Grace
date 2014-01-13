unit eeVstParameterEx;

interface

uses
  eeVstParameter;

type
  TVstParameterEx = class(TVstParameter)
  private
    fHasModLink: boolean;
    fModLinkIndex: integer;
  public
    constructor Create(const aName : string); override;
    destructor Destroy; override;

    procedure SetHasModLink(const aHasModLink : boolean; aModLinkIndex : integer = -1);
    property HasModLink    : boolean read fHasModLink;
    property ModLinkIndex  : integer read fModLinkIndex;

  end;

implementation

{ TVstParameterEx }

constructor TVstParameterEx.Create(const aName: string);
begin
  inherited;

  fHasModLink   := false;
  fModLinkIndex := -1;
end;

destructor TVstParameterEx.Destroy;
begin

  inherited;
end;

procedure TVstParameterEx.SetHasModLink(const aHasModLink: boolean; aModLinkIndex: integer);
begin
  fHasModLink := aHasModLink;
  fModLinkIndex := aModLinkIndex;
end;

end.
