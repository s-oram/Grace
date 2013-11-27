unit eeOleDropHandler;

interface

uses
  Windows,
  Classes,
  DragDropFile,
  DragDropText,
  DropTarget;



type
  // TMutantDropTarget is a love child of TDropTextTarget and TDropFileTarget.
  // It creates a drop target that accepts files and text.
  // Text is useful as the VST-XML drag and drop format is text.
  TMutantDropTarget = class(TCustomDropMultiTarget)
  private
    FFileFormat: TFileDataFormat;
    FFileMapFormat: TFileMapDataFormat;
    FTextFormat: TTextDataFormat;
  protected
    function GetFiles: TUnicodeStrings;
    function GetMappedNames: TUnicodeStrings;
    function GetPreferredDropEffect: LongInt; override;

    function GetText: string;
    function GetAnsiText: AnsiString;
    function GetLocale: DWORD;
    function GetUnicodeText: UnicodeString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Files: TUnicodeStrings read GetFiles;
    property MappedNames: TUnicodeStrings read GetMappedNames;
    property OptimizedMove default True;

    property Text: string read GetText;
    property AnsiText: AnsiString read GetAnsiText;
    property UnicodeText: UnicodeString read GetUnicodeText;
    property Locale: DWORD read GetLocale;
  end;




implementation

uses
  WinApi.ActiveX;

constructor TMutantDropTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OptimizedMove := True;

  FFileFormat := TFileDataFormat.Create(Self);
  FFileMapFormat := TFileMapDataFormat.Create(Self);

  FTextFormat := TTextDataFormat.Create(Self);
end;

destructor TMutantDropTarget.Destroy;
begin
  FFileFormat.Free;
  FFileMapFormat.Free;
  FTextFormat.Free;
  inherited Destroy;
end;

function TMutantDropTarget.GetFiles: TUnicodeStrings;
begin
  Result := FFileFormat.Files;
end;

function TMutantDropTarget.GetMappedNames: TUnicodeStrings;
begin
  Result := FFileMapFormat.FileMaps;
end;

function TMutantDropTarget.GetPreferredDropEffect: LongInt;
begin
  // TODO : Needs explanation of why this is nescessary.
  Result := inherited GetPreferredDropEffect;
  if (Result = DROPEFFECT_NONE) then
    Result := DROPEFFECT_COPY;
end;

function TMutantDropTarget.GetAnsiText: AnsiString;
begin
  Result := FTextFormat.AnsiText;
end;

function TMutantDropTarget.GetLocale: DWORD;
begin
  Result := FTextFormat.Locale;
end;

function TMutantDropTarget.GetText: string;
begin
  Result := FTextFormat.Text;
end;

function TMutantDropTarget.GetUnicodeText: UnicodeString;
begin
  Result := FTextFormat.UnicodeText;
end;




end.
