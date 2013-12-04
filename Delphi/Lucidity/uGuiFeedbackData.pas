unit uGuiFeedbackData;

interface

uses
  uConstants,
  uLucidityEnums,
  Lucidity.SampleMap;

type
  TGuiFeeback_SampleBounds = record
    ShowRealTimeMarkers : boolean;

    ShowPlaybackBounds : boolean;
    PlaybackStart : integer;  // Usually sample start or loop start
    PlaybackEnd   : integer;  // Usually sample end or loop end

    ShowLoopBounds : boolean;
    LoopStart : integer;      // the current looping bounds.
    LoopEnd   : integer;      // the current looping bounds.

    // The current oscillator can set the highlight bounds.
    // Normally the highlight bounds is set to show the looping bounds.
    ShowHighlightBounds : boolean;
    HighlightStart : integer;
    HighlightEnd   : integer;

    //Modulated Positions
    ModLoopStart : integer;
    ModLoopEnd   : integer;

    // Playback Position
    PlaybackPos : integer;
  end;


  // TGuiFeedbackData.
  // The GUI passes an instance of TGuiFeedbackData to the synth engine. The
  // synth engine updates the object for display on the GUI.
  // I'm not sure if TGuiFeedbackData is the best name.
  PGuiFeedbackData = ^TGuiFeedbackData;
  TGuiFeedbackData = class
  private
    fFocusedRegion: IRegion;
    fSamplePlaybackType: TSamplePlaybackType;
  public
    SampleBounds    : TGuiFeeback_SampleBounds;

    // When IsVoiceActive is false, all of the other GuiFeedback data is invalid.
    // Feedback data is only returned when there is a active voice.
    IsVoiceActive    : boolean;
    StepSeq1CurStep  : integer;
    StepSeq2CurStep  : integer;
    ActiveVoiceCount : integer;

    constructor Create;
    destructor Destroy; override;

    //property FocusedEngine : TLucidityEngine read GetFocusedEngine;
    property FocusedRegion : IRegion read fFocusedRegion write fFocusedRegion;

    property SamplePlaybackType : TSamplePlaybackType read fSamplePlaybackType write fSamplePlaybackType;
  end;



  PFilterParameterInfo = ^TFilterParameterInfo;
  TFilterParameterInfo = record
    // Filter 1
    Filter1Par1FullName  : string;
    Filter1Par1ShortName : string;
    Filter1Par2FullName  : string;
    Filter1Par2ShortName : string;
    Filter1Par3FullName  : string;
    Filter1Par3ShortName : string;
    // Filter 2
    Filter2Par1FullName  : string;
    Filter2Par1ShortName : string;
    Filter2Par2FullName  : string;
    Filter2Par2ShortName : string;
    Filter2Par3FullName  : string;
    Filter2Par3ShortName : string;
  end;

implementation

{ TGuiFeedbackData }

constructor TGuiFeedbackData.Create;
begin

end;

destructor TGuiFeedbackData.Destroy;
begin

  inherited;
end;

end.
