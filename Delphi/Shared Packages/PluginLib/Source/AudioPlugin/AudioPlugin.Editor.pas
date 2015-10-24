unit AudioPlugin.Editor;

interface

uses
  Vcl.Forms,
  Types,
  AudioPlugin.Globals;

type
  TAudioPluginEditor = class;
  TAudioPluginEditorClass = class of TAudioPluginEditor;

  TAudioPluginEditor = class
  private
    FGlobals: TGlobals;
  protected
    property Globals : TGlobals read FGlobals;
  public
    class function GetInitialGuiSize:TRect; virtual; abstract;
    class function GetMinGuiSize : TSize; virtual; abstract;
  public
    constructor Create(const aForm : TForm; const aGlobals : TGlobals); virtual;
  end;


implementation

{ TAudioPluginEditor }

constructor TAudioPluginEditor.Create(const aForm: TForm; const aGlobals : TGlobals);
begin
  FGlobals := aGlobals;
end;

end.
