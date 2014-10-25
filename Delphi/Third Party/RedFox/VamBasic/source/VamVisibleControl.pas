unit VamVisibleControl;

interface

uses
  Classes, Types;

type
  IVamDragData = interface
    ['{2FDF5A04-3469-471F-95AB-410B7481B904}']
    function GetFiles : TStringList;
    function GetText  : string;
  end;


  IVamVisibleControl = interface
    ['{404B76E9-D95A-4D53-80EB-070339228B04}']
    function GetObject : TObject;

    procedure OleDragOver(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
    procedure OleDragDrop(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
    procedure OleDragEnter(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData);
    procedure OleDragLeave(Sender: TObject);
  end;

  TOleDragEvent      = procedure(Sender: TObject; ShiftState: TShiftState; APoint: TPoint; var Effect: Integer; Data:IVamDragData) of object;
  TOleDragLeaveEvent = procedure(Sender: TObject) of object;

  TShowContextMenuEvent = procedure(Sender : TObject; X, Y:integer) of object;

implementation

end.
