unit AudioPlugin.PlugEdit;

interface

uses
  Windows,
  Vcl.Forms,
  Types,
  PlugLib.Types,
  AudioPlugin.Globals;

type
  TAbstractPlugEdit = class(TPureInterfacedObject)
  public
    constructor Create(const GlobalsPtr : Pointer); virtual;

    class function GetInitialGuiSize:TRect; virtual; abstract;
    class function GetMinGuiSize : TSize; virtual; abstract;

    procedure Open(const WindowHandle : HWND; const InitialWidth, InitialHeight : integer); virtual; abstract;
    procedure Close; virtual; abstract;
  end;

  TAudioPlugEditor = class(TAbstractPlugEdit);
  TAudioPlugEditorClass = class of TAudioPlugEditor;


implementation



{ TAbstractPlugEdit }


constructor TAbstractPlugEdit.Create(const GlobalsPtr : Pointer);
begin

end;



end.
