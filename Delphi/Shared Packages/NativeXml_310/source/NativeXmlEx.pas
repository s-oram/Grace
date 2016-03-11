unit NativeXmlEx;

interface

uses
  NativeXml;

type
  TNativeXmlEx = class(TNativeXML)
  public
    //Load
    function LoadFromFile_Safe(FileName:String):boolean;
  end;

implementation

uses
  Classes;

{ TNativeXmlHelper }

function TNativeXmlEx.LoadFromFile_Safe(FileName: String): boolean;
begin
  try
    self.ParserWarnings := false;
    self.LoadFromFile(FileName);
  except
    // EFilerError is raised when the document is not well formed. It's would be better for it to fail silently, but
    // I don't think that is possible. Next best thing is to handle the exception.
    // Perhaps a different XML parser could be used to check if the document is well formed.
    on EFilerError do
    begin
      result := false;
      exit; //==============================>>
    end;
  end;

  result := true;
end;

end.
