unit VamLib.Graphics;

interface

uses
  Vcl.Graphics, VamLib.Utils;


type
  IInterfacedBitmap = interface
    ['{366D818B-9F33-4584-9C2A-93922B2F486E}']
    function Bitmap : TBitmap;
  end;

  TInterfacedBitmap = class(TVamInterfacedObject, IInterfacedBitmap)
  private
    fBitmap: TBitmap;

    function IInterfacedBitmap.Bitmap = GetBitmap;
    function GetBitmap : TBitmap;
  public
    constructor Create;
    destructor Destroy; override;

    property Bitmap : TBitmap read fBitmap;
  end;

implementation

{ TInterfacedBitmap }

constructor TInterfacedBitmap.Create;
begin
  fBitmap := TBitmap.Create;
end;

destructor TInterfacedBitmap.Destroy;
begin
  fBitmap.Free;
  inherited;
end;

function TInterfacedBitmap.GetBitmap: TBitmap;
begin
  result := fBitmap;
end;

end.
