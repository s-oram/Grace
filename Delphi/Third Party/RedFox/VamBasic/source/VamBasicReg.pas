unit VamBasicReg;

interface

uses
  VamCompoundLabel,
  VamCompoundModMatrixSection,
  VamCompoundNumericKnob,
  VamArrows,
  VamButton,
  VamDiv,
  VamImage,
  VamKnob,
  VamLabel,
  VamMemo,
  VamMiniLevelMeter,
  VamModularJack,
  VamMultiLineTextBox,
  VamNumericKnob,
  VamPanel,
  VamSampleDisplay,
  VamSamplerKeys,
  VamSampleMap,
  VamSampleZoomControl,
  VamScrollBar,
  VamScrollBox,
  VamShortMessageOverlay,
  VamSlider,
  VamSliderSwitch,
  VamStatusLed,
  VamTabs,
  VamTabPanel,
  VamTextBox,
  VamTreeView,
  VamModSelector,
  VamXYPad;

procedure Register;

implementation

uses
  Classes, DesignIntf;

procedure Register;
begin
  RegisterComponents('Vam Basic', [TVamButton]);
  RegisterComponents('Vam Basic', [TVamDiv]);
  RegisterComponents('Vam Basic', [TVamImage]);
  RegisterComponents('Vam Basic', [TVamLabel]);
  RegisterComponents('Vam Basic', [TVamMemo]);
  RegisterComponents('Vam Basic', [TVamMultiLineTextBox]);
  RegisterComponents('Vam Basic', [TVamPanel]);
  RegisterComponents('Vam Basic', [TVamScrollBar]);
  RegisterComponents('Vam Basic', [TVamScrollBox]);
  RegisterComponents('Vam Basic', [TVamShortMessageOverlay]);
  RegisterComponents('Vam Basic', [TVamTabs]);
  RegisterComponents('Vam Basic', [TVamTabPanel]);
  RegisterComponents('Vam Basic', [TVamTextBox]);
  RegisterComponents('Vam Basic', [TVamTreeView]);

  RegisterComponents('Vam VST', [TVamArrows]);
  RegisterComponents('Vam VST', [TVamKnob]);
  RegisterComponents('Vam VST', [TVamMiniLevelMeter]);
  RegisterComponents('Vam VST', [TVamModSelector]);
  RegisterComponents('Vam VST', [TVamModularCableOverlay]);
  RegisterComponents('Vam VST', [TVamModularJack]);
  RegisterComponents('Vam VST', [TVamNumericKnob]);
  RegisterComponents('Vam VST', [TVamSampleDisplay]);
  RegisterComponents('Vam VST', [TVamSampleMap]);
  RegisterComponents('Vam VST', [TVamSamplerKeys]);
  RegisterComponents('Vam VST', [TVamSampleZoomControl]);
  RegisterComponents('Vam VST', [TVamStatusLed]);
  RegisterComponents('Vam VST', [TVamSlider]);
  RegisterComponents('Vam VST', [TVamSliderSwitch]);
  RegisterComponents('Vam VST', [TVamXYPad]);

  RegisterComponents('Vam Compound', [TVamCompoundLabel]);
  RegisterComponents('Vam Compound', [TVamCompoundModMatrixSection]);
  RegisterComponents('Vam Compound', [TVamCompoundNumericKnob]);
end;

end.
