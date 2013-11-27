unit eeDetectHost;

interface

uses
  Windows, eeTypes;


function DetectVstHostFromWindow(EditorHandle:hwnd):TVstHost;

function VstHostToStr(host:TVstHost):string;

implementation

uses
  uGetWindowsInfo;


// NOTE: Finding out what host is being used from the plugin side is a problematic. There is a call back
// to ask the host to supply the info but this isn't always correct or even supplied. Checking the
// containing window type isn't failsafe but does provide additional clues about the current host.

function DetectVstHostFromWindow(EditorHandle:hwnd):TVstHost;
var
  ParentHandle:hwnd;
  WindowClass, WindowText:string;
begin
  ParentHandle := GetParent(EditorHandle);
  WindowClass  := eeGetClassName(ParentHandle);
  WindowText   := eeGetWindowText(ParentHandle);

  //The default response.
  result := vhUnknown;

  if WindowClass = 'eXTSAwin'
    then result := vhEnergyXT;

  if WindowClass = 'eXTVSTiwin'
    then result := vhEnergyXT;

  if WindowClass = 'eXTVSTwin'
    then result := vhEnergyXT;

  if WindowClass = 'XT2APP'
    then result := vhEnergyXT2;

  if WindowClass = 'XT3APP'
    then result := vhEnergyXT2;  //Added for EnergyXT 2.5

  if WindowClass = 'VSTPluginEditor'
    then result := vhCubaseSX2;

  if WindowClass = 'SteinbergPluginWrapper'
    then result := vhCubase5;    

  //NOTE: Both Reaper 2 and 3 use the same WindowClass identifier.
  if WindowClass = '#32770'
    then result := vhReaper;  //Unknown version.

  if WindowClass = 'REAPERb32host'
    then result := vhReaperBridge32;  //Unknown version.

  if (WindowClass = '#32770') and (WindowText = 'jBridge')
    then result := vhJBridge;

  if WindowClass = 'JUCE_253899123'
    then result := vhTracktion3;

  if WindowClass = 'TWPControl'
    then result := vhFLStudio;

  if WindowClass = 'TVSTPanel'
    then result := vhFLStudio;  //Added for FLStudio 9.

  if WindowClass = 'Plugin'
    then result := vhPodium;

  if WindowClass = 'AbletonVstPlugClass'
    then result := vhAbletonLive;

  //NOTE: WindowClass may be different for VSTi and VST Fx plugins.
  if WindowClass = 'SF.VSTiUIPageSite'
    then result := vhAcidPro6;

  if WindowClass = 'CakewalkVSTWndClass'
    then result := vhSonar;

  if WindowClass = 'TMyPanel'
    then result := vhMultiTrackStudio;

  if WindowClass  = 'AfxFrameOrView42s'
    then result := vhSamplitude11;

  if WindowClass = 'CCLWindowClass'
    then result := vhStudioOne;

end;

function VstHostToStr(host:TVstHost):string;
begin
  case host of
    vhUnKnown:          result := 'Unknown';
    vhEnergyXT:         result := 'EnergyXT 1';
    vhEnergyXT2:        result := 'EnergyXT 2';
    vhCubaseSX2:        result := 'Cubase SX 2';
    vhCubase5:          result := 'Cubase 5';
    vhReaper:           result := 'Reaper';
    vhReaperBridge32:   result := 'Reaper 32bit Bridge';
    vhTracktion3:       result := 'Tracktion 3';
    vhFLStudio:         result := 'FL Studio';
    vhPodium:           result := 'Podium';
    vhAbletonLive:      result := 'Ableton Live';
    vhAcidPro6:         result := 'Acid Pro 6';
    vhSonar:            result := 'Sonar';
    vhMultiTrackStudio: result := 'MultiTrackStudio';
    vhSamplitude11:     result := 'Samplitude 11';
    vhStudioOne:        result := 'Presonus Studio One';
    vhJBridge:          result := 'JBridge';
  else
    result := 'not defined.'
  end;
end;



end.
