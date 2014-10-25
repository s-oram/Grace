unit VamLib.Win.DragDrop;

interface

uses
  Forms, Types, Classes;

type
  // was going to write a class based around this but got bored and decided not to.
  TFilesDroppedEvent = procedure (Sender : TObject; Files : TStringList) of object;

  TFormDropFileHandler = class
  private
    fOnFilesDropped: TFilesDroppedEvent;
  public
    constructor Create(aForm : TForm);
    destructor Destroy; override;

    property OnFilesDropped : TFilesDroppedEvent read fOnFilesDropped write fOnFilesDropped;
  end;

implementation

{ TFormDropFileHandler }

constructor TFormDropFileHandler.Create(aForm: TForm);
begin

end;

destructor TFormDropFileHandler.Destroy;
begin

  inherited;
end;

end.
