unit NativeXmlHelper;

interface

uses
  NativeXml;

type
  TNativeXmlHelper = class(TNativeXML)
  public
    function LoadFromFile_Safe(FileName:String):boolean;
  end;

implementation

{ TNativeXmlHelper }

function TNativeXmlHelper.LoadFromFile_Safe(FileName: String): boolean;
begin

end;

end.
