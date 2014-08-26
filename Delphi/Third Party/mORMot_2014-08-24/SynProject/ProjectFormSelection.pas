unit ProjectFormSelection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TSelectionForm = class(TForm)
    List: TListBox;
    pnlTop: TPanel;
    edtFind: TEdit;
    btnNext: TButton;
    procedure ListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure edtFindChange(Sender: TObject);
    procedure ListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  public
    Lines: TStringList;
    OnlyNumberedTitles: boolean;
    Selected: integer;
    function TitleToNumber(index: integer): integer;
  end;

var
  SelectionForm: TSelectionForm;


implementation

{$R *.dfm}

function TSelectionForm.TitleToNumber(index: integer): integer;
var j: integer;
    s: string;
begin
  result := 0;
  if cardinal(index)>=cardinal(Lines.Count) then
    exit;
  s := Lines[index];
  if (s='') or (s[1]='[') then
    exit;
  j := 0;
  while s[j+1] in ['0'..'9'] do inc(j);
  if j>0 then
    result := StrToInt(copy(s,1,j));
end;

procedure TSelectionForm.ListDblClick(Sender: TObject);
begin
  Selected := List.ItemIndex;
  Close;
end;

procedure TSelectionForm.FormCreate(Sender: TObject);
begin
  Lines := TStringList.Create;
end;

procedure TSelectionForm.FormDestroy(Sender: TObject);
begin
  Lines.Free;
end;

procedure TSelectionForm.FormShow(Sender: TObject);
var i,n,max: integer;
begin
  max := 0;
  for i := Lines.Count-1 downto 0 do begin
    n := TitleToNumber(i);
    if n>0 then begin
      if n>max then
        max := n;
      continue;
    end;
    if not OnlyNumberedTitles then
      continue;
    Lines.Delete(i);
    if Selected>=i then
      if Selected=i then
        Selected := -1 else
        dec(Selected);
  end;
  if max>0 then
    Caption := format('%s - Maximum title # is %d',[Caption,max]);
  List.Count := Lines.Count;
  if cardinal(Selected)<cardinal(Lines.Count) then begin
    List.TopIndex := Selected-10;
    List.ItemIndex := Selected;
  end;
  edtFind.SetFocus;
  Selected := -1;
end;

procedure TSelectionForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_RETURN:
    ListDblClick(nil);
  VK_ESCAPE:
    Close;
  VK_F3:
    edtFindChange(nil);
  end;
end;

procedure TSelectionForm.edtFindChange(Sender: TObject);
var Search: string;
    i,found: integer;
begin
  edtFind.SetFocus;
  Search := UpperCase(edtFind.Text);
  if Search='' then
    exit;
  found := -1;
  for i := List.ItemIndex+1 to Lines.Count-1 do
    if Pos(Search,UpperCase(Lines[i]))>0 then begin
      found := i;
      break;
    end;
  if found<0 then
    for i := 0 to List.ItemIndex-1 do
      if Pos(Search,UpperCase(Lines[i]))>0 then begin
        found := i;
        break;
      end;
  if found>=0 then begin
    List.TopIndex := found-10;
    List.ItemIndex := found;
  end;
end;

procedure TSelectionForm.ListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var List: TCustomListBox absolute Control;
    Data,Number: string;
    i,sp: integer;
begin
  with List.Canvas do begin
    FillRect(Rect);
    if cardinal(Index)<cardinal(Lines.Count) then begin
      Data := Lines[index];
      if Data<>'' then begin
        if Data[1]<>'[' then begin
          sp := 0;
          while Data[sp+1] in ['0'..'9'] do inc(sp);
          Number := copy(Data,1,sp);
          delete(Data,1,sp);
          sp := 3;
          for i := 1 to length(Data) do
            if Data[i]=' ' then
              inc(sp) else
              break;
        end else
          sp := 1;
        TextOut(Rect.Left+sp*8,Rect.Top,Data);
        TextOut(Rect.Right-TextWidth(Number)-12,Rect.Top,Number);
      end;
    end;
  end;
end;

end.

