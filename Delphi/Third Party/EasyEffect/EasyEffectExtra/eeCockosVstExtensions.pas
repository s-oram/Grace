unit eeCockosVstExtensions;

interface

uses
  DAEffect;

function CockosVstEx_GetProjectPath(AudioMaster : TAudioMasterCallbackFunc; out FunctionIsImplemented:boolean):string;

implementation

function CockosVstEx_GetProjectPath(AudioMaster : TAudioMasterCallbackFunc; out FunctionIsImplemented:boolean):string;
type
  TGetProjectPathFunction = procedure(Path : PAnsiChar; BufferSize : integer); cdecl;
var
  CallBackResult : VstIntPtr;
  Path           : PAnsiChar;
  BufferSize     : integer;
begin
  BufferSize := 1024;
  GetMem(Path, BufferSize);
  try
    //=====================================================================
    // STEP 1: Get the address of the GetProjectPath() function.
    //=====================================================================

    // Check if range checking is active. If so, termporarrily toggle it off.
    {$ifopt R+}
       {$R-}
       {$define TOGGLE_RANGE_CHECK}
    {$endif}

    // NOTE: Range checking needs to be off becuase the $deadbeef & $deadf00d constants
    // violate the range of the integer type. (Humorous but inconvient constant choice.)
    CallBackResult := AudioMaster(nil, $deadbeef, $deadf00d, 0, PAnsiChar('GetProjectPath'), 0);

    {$ifdef TOGGLE_RANGE_CHECK}
      {$R+}
      {$undef TOGGLE_RANGE_CHECK}
    {$endif}



    //=====================================================================
    // STEP 2: Translate the CallBackResult value to a callable function.
    //=====================================================================
    if CallBackResult <> 0 then
    begin
      TGetProjectPathFunction(CallBackResult)(Path, BufferSize);
      FunctionIsImplemented := true;
      result := Path;
    end else
    begin
      FunctionIsImplemented := false;
      result := '';
    end;

  finally
    FreeMem(Path, BufferSize);
  end;
end;

end.
