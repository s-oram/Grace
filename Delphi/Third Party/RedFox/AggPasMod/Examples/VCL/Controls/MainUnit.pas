unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, AggControlVCL, AggColor;

type
  TFmAggPasControlsdemo = class(TForm)
    AggLabel: TAggLabel;
    AggSlider: TAggSlider;
    AggCheckBox: TAggCheckBox;
    AggRadioBox: TAggRadioBox;
  end;

var
  FmAggPasControlsdemo: TFmAggPasControlsdemo;

implementation

{$R *.dfm}

end.

