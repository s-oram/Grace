unit ee3.CustomPlugin.PublishedVstParInfo;

interface

type
  IPublishedVstParInfo = interface
    ['{5A484DB8-3294-44E7-A807-4AE4EFD15760}']
    procedure VstPar_SetParameter(const Index: Integer; Value: single);  // Called when a parameter changed
    function VstPar_GetParameter(const Index: Integer): single;          // Return the value of the parameter with index
    function VstPar_GetParameterName(const Index: Integer):string;    // Stuff text with the name ("Time", "Gain", "RoomType", etc...) of parameter index. Limited to kVstMaxParamStrLen.
    function VstPar_GetParameterDisplay(const Index: Integer):string; // Stuff text with a string representation ("0.5", "-3", "PLATE", etc...) of the value of parameter index. Limited to kVstMaxParamStrLen.
    function VstPar_GetParameterLabel(const Index: Integer):string;   // Stuff label with the units in which parameter index is displayed (i.e. "sec", "dB", "type", etc...). Limited to kVstMaxParamStrLen.
  end;

implementation

end.
