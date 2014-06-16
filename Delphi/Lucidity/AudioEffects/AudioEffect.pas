unit AudioEffect;


{
  The goal of this "Audio Effect" code is to provide selfcontained implementations
  of audio effects such as distortion, waveshaping, delay, filters etc. The
  Audio Effects will be used in other applications.

  - Each Audio Effect should be as self-contained as possible.
  - Audio Effects should contain no application specific code. The application
    developer will normally need to use this code with a wrapper to fit the
    classes to main application design paradigm.
}

interface


const
  //A small value to prevent denormals.
  kDenormal    = 1.0e-24;
  GlobalTuneA4 = 440;

implementation

end.
