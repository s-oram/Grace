unit AudioPlugin.PlugEdit;

interface

uses
  Windows,
  Vcl.Forms,
  Types,
  AudioPlugin.Globals;

type
  TAudioPlugEditor = class;
  TAudioPlugEditorClass = class of TAudioPlugEditor;

  TAudioPlugEditor = class
  private
    FGlobals: TGlobals;
    FIsOpen: boolean;
  protected
    property Globals : TGlobals read FGlobals;
    property IsOpen : boolean read FIsOpen;
  public
    class function GetInitialGuiSize:TRect; virtual; abstract;
    class function GetMinGuiSize : TSize; virtual; abstract;
  public
    constructor Create(const aGlobals : TGlobals); virtual;
    destructor Destroy; override;

    procedure Open(const WindowHandle : HWND; const InitialWidth, InitialHeight : integer); virtual;
    procedure Close; virtual;
  end;


implementation

{ TAudioPluginEditor }

constructor TAudioPlugEditor.Create(const aGlobals : TGlobals);
begin
  FGlobals := aGlobals;
  FIsOpen := false;
end;

destructor TAudioPlugEditor.Destroy;
begin

  inherited;
end;

procedure TAudioPlugEditor.Open(const WindowHandle: HWND; const InitialWidth, InitialHeight: integer);
begin
  // Open the GUI here.
  FIsOpen := true;
end;

procedure TAudioPlugEditor.Close;
begin
  // Close the GUI here.
  FIsOpen := false;
end;



end.
