unit frmFactoryDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Spring.DesignPatterns, StdCtrls;

type
  TForm29 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    EditFactory: TFactory<string, TEdit>;
  public
    { Public declarations }
  end;

var
  Form29: TForm29;

implementation

{$R *.dfm}

const
    EditLeft = 250;
    RedEditLeft = 400;
var
    EditTop: integer;
    RedEditTop: integer;
    EditTotal: integer;
    RedEditTotal: integer;

procedure TForm29.Button1Click(Sender: TObject);
begin
  Inc(EditTotal);
  EditFactory.GetInstance('edit');
end;

procedure TForm29.Button2Click(Sender: TObject);
begin
  Inc(RedEditTotal);
  EditFactory.GetInstance('rededit');
end;

procedure TForm29.FormCreate(Sender: TObject);
var
  EditCreationCode: TFactoryMethod<TEdit>;
  RedTextEditCreationCode: TFactoryMethod<TEdit>;
begin
  EditTop := 25;
  EditTotal := 0;
  RedEditTop := 25;
  RedEditTotal := 0;

  EditFactory := TFactory<string, TEdit>.Create;

  EditCreationCode := function: TEdit
                      begin
                        Result := TEdit.Create(Self);
                        Result.Parent := Self;
                        Result.Left := EditLeft;
                        Result.Top := EditTop;
                        EditTop := EditTop + 30;
                        Result.Text := Format('This is Edit #%d', [EditTotal]);
                      end;

  RedTextEditCreationCode := function: TEdit
                             begin
                               Result := TEdit.Create(Self);
                               Result.Parent := Self;
                               Result.Left := RedEditLeft;
                               Result.Top := RedEditTop;
                               Result.Font.Color := clRed;
                               RedEditTop := RedEditTop + 30;
                               Result.Text := Format('This is Red Edit #%d', [RedEditTotal]);
                             end;


  EditFactory.RegisterFactoryMethod('edit', EditCreationCode);
  EditFactory.RegisterFactoryMethod('rededit', RedTextEditCreationCode);
end;

procedure TForm29.FormDestroy(Sender: TObject);
begin

  EditFactory.Free;
end;

end.
