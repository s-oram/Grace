unit VamDiv;

interface

uses
  WinApi.Windows,
  Types, Graphics,
  WinApi.Messages, Classes, Vcl.Controls, RedFoxImageBuffer, VamWinControl;

type
  TVamDiv = class(TVamWinControl)
  private
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {$INCLUDE TControlProperties.inc}
    {$INCLUDE TWinControlProperties.inc}
    property AutoSize;
  end;

implementation

uses

  Agg2d, AggWin32Bmp,
  SysUtils, RedFox, RedFoxContainer;

{ TRedFoxDiv }

constructor TVamDiv.Create(AOwner: TComponent);
begin
  inherited;
  // Div's are always transparent.
  Transparent := true;
end;

destructor TVamDiv.Destroy;
begin
  inherited;
end;

procedure TVamDiv.Paint;
begin
  inherited;
  BackBuffer.BufferInterface.ClearAll(255,255,255,0);
end;

end.
