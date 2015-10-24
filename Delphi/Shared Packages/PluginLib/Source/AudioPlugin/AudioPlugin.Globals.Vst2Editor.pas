unit AudioPlugin.Globals.Vst2Editor;

interface

const
  kVst2EditorGlobalsTag = 'Vst2EditorGlobals';

type
  TResizeGuiMethod = reference to procedure(const XOffset, YOffset: integer);

  TVst2EditorGlobals = class
  private
  public
    ResizeGui : TResizeGuiMethod;
  end;

implementation

end.
