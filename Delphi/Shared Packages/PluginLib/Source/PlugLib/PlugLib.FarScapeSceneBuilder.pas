unit PlugLib.FarScapeSceneBuilder;

interface

uses
  FarScape.CustomControl,
  Ron.Parser;

type
  TFarScapeControlFactory = reference to function(const ClassName : string):TFarScapeControl;

  TFarScapeSceneBuilder = class
  private
    FarScapeRoot: TFarScapeAbstractRoot;
    ControlFactory : TFarScapeControlFactory;
    procedure ProcessParserObjectOpen(Sender : TObject; const ParserValue : TParserValue);
    procedure ProcessParserValue(Sender : TObject; const ParserValue : TParserValue);
  public
    // GuiLayoutScript is a Ron format text string.
    procedure BuildScene(const aFarScapeRoot : TFarScapeAbstractRoot; const aGuiLayoutScript : string; const aControlFactory : TFarScapeControlFactory);
  end;

implementation

uses
  SysUtils,
  FarScape.Assistant.ControlNamePath,
  VamLib.Utils;

{ TFarScapeSceneBuilder }

procedure TFarScapeSceneBuilder.BuildScene(const aFarScapeRoot: TFarScapeAbstractRoot; const aGuiLayoutScript: string; const aControlFactory : TFarScapeControlFactory);
var
  Parser : TParser;
begin
  FarScapeRoot := aFarScapeRoot;
  ControlFactory := aControlFactory;

  Parser := TParser.Create;
  AutoFree(@Parser);

  Parser.OnObjectOpen := ProcessParserObjectOpen;
  Parser.OnValue      := ProcessParserValue;

  if Parser.ParseText(aGuiLayoutScript) = false then
  begin
    exit;
  end;
end;

procedure TFarScapeSceneBuilder.ProcessParserObjectOpen(Sender: TObject; const ParserValue: TParserValue);
var
  fsc : TFarScapeControl;
  ParentControlNamePath : string;
  ParentControl : TFarScapeControl;
  NewControlName : string;
begin
  if not ControlNamePath.IsValidNamePath(ParserValue.ObjectPath) then raise Exception.Create('Control name path is invalid.');

  fsc := FarScapeRoot.FindControlByNamePath(ParserValue.ObjectPath);

  // If the control isn't found, it doesn't exists, therefore we need to create it.
  if not assigned(fsc) then
  begin
    ParentControlNamePath := ControlNamePath.AscendNamePathByOne(ParserValue.ObjectPath);

    if ParentControlNamePath <> ''
      then ParentControl := FarScapeRoot.FindControlByNamePath( ParentControlNamePath )
      else ParentControl := FarScapeRoot;

    if not assigned(ParentControl) then raise Exception.Create('Parent control not found.');

    fsc := ControlFactory(ParserValue.ObjectType);
    if not assigned(fsc) then raise Exception.Create('Unable to create new object.');

    NewControlName := ControlNamePath.LastName(ParserValue.ObjectPath);
    assert(NewControlName <> '');

    fsc.Name := NewControlName;
    fsc.Parent := ParentControl;
  end;
end;

procedure TFarScapeSceneBuilder.ProcessParserValue(Sender: TObject; const ParserValue: TParserValue);
var
  fsc : TFarScapeControl;
  StrArr   : array of string;
  FloatArr : array of single;
  IntArr   : array of integer;
  BoolArr  : array of boolean;
  c1: Integer;
begin
  fsc := FarScapeRoot.FindControlByNamePath(ParserValue.ObjectPath);
  if assigned(fsc) then
  begin
    assert(ParserValue.KeyName <> '');
    case ParserValue.ValueType of
      // Property value order is String, Int, Float, Boolean.
      pvInteger:
      begin
        SetLength(IntArr, 1);
        IntArr[0] := ParserValue.ValueAsInteger;
      end;

      pvFloat:
      begin
        SetLength(FloatArr, 1);
        FloatArr[0] := ParserValue.ValueAsFloat;
      end;

      pvBoolean:
      begin
        SetLength(BoolArr, 1);
        BoolArr[0] := ParserValue.ValueAsBoolean;
      end;

      pvUntyped:
      begin
        SetLength(StrArr, 1);
        StrArr[0] := ParserValue.ValueAsUntyped;
      end;

      pvString:
      begin
        SetLength(StrArr, 1);
        StrArr[0] := ParserValue.ValueAsString;
      end;

      pvIntegerArray:
      begin
        SetLength(IntArr, ParserValue.ArrayLength);
        for c1 := 0 to ParserValue.ArrayLength-1 do IntArr[c1] := ParserValue.ValueAsInteger(c1);
      end;

      pvFloatArray:
      begin
        SetLength(FloatArr, ParserValue.ArrayLength);
        for c1 := 0 to ParserValue.ArrayLength-1 do FloatArr[c1] := ParserValue.ValueAsFloat(c1);
      end;

      pvBooleanArray:
      begin
        SetLength(BoolArr, ParserValue.ArrayLength);
        for c1 := 0 to ParserValue.ArrayLength-1 do BoolArr[c1] := ParserValue.ValueAsBoolean(c1);
      end;

      pvUntypedArray:
      begin
        SetLength(StrArr, ParserValue.ArrayLength);
        for c1 := 0 to ParserValue.ArrayLength-1 do StrArr[c1] := ParserValue.ValueAsUntyped(c1);
      end;

      pvStringArray:
      begin
        SetLength(StrArr, ParserValue.ArrayLength);
        for c1 := 0 to ParserValue.ArrayLength-1 do StrArr[c1] := ParserValue.ValueAsString(c1);
      end;

      pvNoValue: ;
      pvEmptyArray: ;
    else
      raise Exception.Create('Unexpected value type.');
    end;

    fsc.SetPropertyValue(ParserValue.KeyName, StrArr, IntArr, FloatArr, BoolArr);
  end;

end;

end.
