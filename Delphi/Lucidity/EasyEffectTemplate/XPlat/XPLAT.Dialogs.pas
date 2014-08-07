unit XPLAT.Dialogs;

interface

uses
  Vcl.Dialogs,
  System.Classes;

{$SCOPEDENUMS ON}

type
  TxpMode = (WinXP, WinVista);

  TxpFileOpenDialog = class
  protected
    WinXP    : TOpenDialog;
    WinVista : TFileOpenDialog;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  end;

implementation

{ TxpFileOpenDialog }

constructor TxpFileOpenDialog.Create(AOwner: TComponent);
begin

end;

destructor TxpFileOpenDialog.Destroy;
begin

  inherited;
end;

end.
