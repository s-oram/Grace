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
    FIsOpen: boolean;
  protected
    property IsOpen : boolean read FIsOpen;
  public
    class function GetInitialGuiSize:TRect; virtual; abstract;
    class function GetMinGuiSize : TSize; virtual; abstract;
  public
    constructor Create(const GlobalsPtr : Pointer); virtual;
    destructor Destroy; override;

    procedure Open(const WindowHandle : HWND; const InitialWidth, InitialHeight : integer); virtual;
    procedure Close; virtual;
  end;


implementation

{ TAudioPluginEditor }

constructor TAudioPlugEditor.Create(const GlobalsPtr : Pointer);
begin
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
