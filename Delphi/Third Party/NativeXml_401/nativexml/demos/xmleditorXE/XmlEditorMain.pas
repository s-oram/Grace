{ Unit XmlEditorMain

  This unit is the main unit for XmlEditor.exe.

  XmlEditor uses the VirtualTreeView component written by Mike Lischke

  Author: Nils Haeck
  email:  n.haeck@simdesign.nl
  Date:   21-07-2001

  Changes:
  20 Feb 2002: Adapted for changed versions of TVirtualStringTree and XML
  29 Jul 2003: Adapted for use with NativeXml.pas
  14 Nov 2003: Cleaned up
  16 Sep 2005: Added editing capabilities
  28 May 2007: Made compatible with NativeXmlEx
  29 Jun 2010: Working on writer (NativeXmlEx)
  23 Jul 2011: Implemented binary xml

  Important:
  in D7, simply reply to 'no' if the compiler complains that the xyz method has
  an incompatible parameter list (this is due to UnicodeString vs WideString)


    Properties of TXmlNode
    - AttributeByName -> AttributeValueByName
    - ValueAsWideString -> ValueWide

    Properties/methods of new TNativeXml compared to old TNativeXml
    - StyleSheetNode -> StyleSheet
    - Utf8Encoded no longer exists (is always the case)
    - OnUnicodeLoss no longer exists (but loss from unicode to
      any other encoding with codepages will be logged in a debug log)
    - TNativeXmlEx is now component, so Create has argument AOwner
    - RootNodeList -> RootNodes
    - AttributeExchange removed, use NodeExchange

    Renames
    - TXmlElementType -> TsdElementType
    -   xeNormal -> xeElement
    -   xeElement -> xeDtdElement
    -   xeAttList -> xeDtdAttList
    -   xeEntity -> xeDtdEntity
    -   xeNotation -> xeDtdNotation

  The reference compiler: Delphi 7  (ver150)
  Newest compiler:        Delphi XE (ver220)

  For Delphi7 users:
  Delphi7 "compains" that some virtualtreeview-related methods have wrong
  parameters. You can just click "no" at compilation time. These references
  should NOT be removed!

  copyright (c) 2001 - 2011 Nils Haeck  www.simdesign.nl
}
unit XmlEditorMain;

interface

{$i simdesign.inc}

uses
  // delphi
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActnList, ImgList, ComCtrls, ToolWin, ExtCtrls, VirtualTrees, Menus,
  StdCtrls,
  // nativexml
  NativeXml, sdDebug,
  // synedit
  SynEdit, SynMemo, SynEditHighlighter, SynHighlighterXML,
  // xmleditor app
  sdXmlOutputOptionsDlg;

type
  TfrmMain = class(TForm)
    ControlBar1: TControlBar;
    tbMain: TToolBar;
    tbNew: TToolButton;
    ilMenu: TImageList;
    alMain: TActionList;
    acFileOpen: TAction;
    acFileNew: TAction;
    tbOpen: TToolButton;
    sbMain: TStatusBar;
    nbMain: TNotebook;
    Splitter1: TSplitter;
    nbData: TNotebook;
    pcData: TPageControl;
    tsTags: TTabSheet;
    odFileOpen: TOpenDialog;
    nbTree: TNotebook;
    pcTree: TPageControl;
    tsXmlTree: TTabSheet;
    stXmlTree: TVirtualStringTree;
    mnuMain: TMainMenu;
    mnuFile: TMenuItem;
    mnuNew: TMenuItem;
    mnuOpen: TMenuItem;
    acSingleNodeAsAttrib: TAction;
    mnuOptions: TMenuItem;
    ilData: TImageList;
    tsXmlSource: TTabSheet;
    acFileSaveAs: TAction;
    sdFileSave: TSaveDialog;
    tbSave: TToolButton;
    mnuSave: TMenuItem;
    acHideSingleNodes: TAction;
    Singlenodesastags1: TMenuItem;
    Hidesinglenodes1: TMenuItem;
    Edit1: TMenuItem;
    acAddComment: TAction;
    acAddStyleSheet: TAction;
    AddComment1: TMenuItem;
    acOutputReadable: TAction;
    Outputinreadableformat1: TMenuItem;
    acElementDelete: TAction;
    acFileExit: TAction;
    N1: TMenuItem;
    acFileExit1: TMenuItem;
    pmTree: TPopupMenu;
    DeleteElement1: TMenuItem;
    InsertElement1: TMenuItem;
    acElementInsertBefore: TAction;
    acElementInsertAfter: TAction;
    acElementInsertSub: TAction;
    BeforeNode1: TMenuItem;
    AfterNode1: TMenuItem;
    Exit1: TMenuItem;
    InsertComment1: TMenuItem;
    acCommentInsert: TAction;
    acElementUp: TAction;
    acElementDown: TAction;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    N2: TMenuItem;
    stAttributes: TVirtualStringTree;
    acAttributeAdd: TAction;
    acAttributeDelete: TAction;
    acAttributeUp: TAction;
    acAttributeDown: TAction;
    pmAttributes: TPopupMenu;
    AddAttribute1: TMenuItem;
    DeleteAttribute1: TMenuItem;
    MoveUp2: TMenuItem;
    MoveDown2: TMenuItem;
    acLoadFromURL: TAction;
    acLoadFromURL1: TMenuItem;
    pcDebug: TPageControl;
    tsDebug: TTabSheet;
    reDebug: TRichEdit;
    Splitter2: TSplitter;
    smXmlSource: TSynMemo;
    hlXML: TSynXMLSyn;
    SaveWithEncodingAs1: TMenuItem;
    acSaveAsWithOptions: TAction;
    pmDebug: TPopupMenu;
    mnuSaveDebugInfo: TMenuItem;
    acSaveDebugInfo: TAction;
    sdDebugSave: TSaveDialog;
    acPreserveWS: TAction;
    Preservewhitespacewhenloading1: TMenuItem;
    ToolButton1: TToolButton;
    pbMain: TProgressBar;
    mnuFixstructuralerrors: TMenuItem;
    acCanonicalXML: TAction;
    mnuCanonicalXML: TMenuItem;
    mnuSaveToBinary: TMenuItem;
    procedure acFileOpenExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure stXmlTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure stXmlTreeExpanding(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var Allowed: Boolean);
    procedure stXmlTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure acSingleNodeAsAttribExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acHideSingleNodesExecute(Sender: TObject);
    procedure pcTreeChange(Sender: TObject);
    procedure acAddCommentExecute(Sender: TObject);
    procedure acOutputReadableExecute(Sender: TObject);
    procedure stXmlTreeEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure stXmlTreeGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure stXmlTreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure acElementDeleteExecute(Sender: TObject);
    procedure acFileExitExecute(Sender: TObject);
    procedure stXmlTreeNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
    procedure acElementInsertBeforeExecute(Sender: TObject);
    procedure acElementInsertAfterExecute(Sender: TObject);
    procedure acElementInsertSubExecute(Sender: TObject);
    procedure acCommentInsertExecute(Sender: TObject);
    procedure acElementUpExecute(Sender: TObject);
    procedure acElementDownExecute(Sender: TObject);
    procedure stAttributesGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    procedure stAttributesGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure stAttributesEditing(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure stAttributesNewText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; NewText: String);
    procedure acAttributeAddExecute(Sender: TObject);
    procedure acAttributeDeleteExecute(Sender: TObject);
    procedure acAttributeUpExecute(Sender: TObject);
    procedure acAttributeDownExecute(Sender: TObject);
    procedure stAttributesChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure stAttributesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure acLoadFromURLExecute(Sender: TObject);
    procedure acSaveAsWithOptionsExecute(Sender: TObject);
    procedure acSaveDebugInfoExecute(Sender: TObject);
    procedure acPreserveWSExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuFixstructuralerrorsClick(Sender: TObject);
    procedure acCanonicalXMLExecute(Sender: TObject);
    procedure stXmlTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure mnuSaveToBinaryClick(Sender: TObject);
  private
    FXml: TNativeXml;       // Xml document currently displayed
    FFileName: UTF8String;  // Last opened filename
    FFileSize: int64;
    FFocusedNode: TXmlNode; // Focused TXmlNode
    FFocusedAttributeIndex: integer;
    FUpdateCount: integer;   // If 0 we can update otherwise we're in begin/end update block
    FPreserveWhiteSpace: boolean;
    FCanonicalXml: boolean;
    procedure GetPropertyInfo(Node: PVirtualNode;
      var IsAttribute: boolean; var Index: integer);
    procedure Regenerate;
    procedure RegenerateFromNode(ANode: TXmlNode);
    procedure RegenerateProperties;
    function ElementTypeToImageIndex(AElementType: TsdElementType): integer;
    function IsSingleNode(ANode: TXmlNode): boolean;
    function MultiAttrCount(ANode: TXmlNode): integer;
    function MultiNodeCount(ANode: TXmlNode): integer;
    function MultiNodeByIndex(ANode: TXmlNode; AIndex: integer): TXmlNode;
    procedure UpdateMenu;
  public
    procedure BeginUpdate;
    function IsUpdating: boolean;
    procedure EndUpdate;
    procedure ShowDebugMsg(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
    // show the xml source in readable format
    procedure ShowXmlSource;
    // show progress
    procedure XmlProgress(Sender: TObject; Position: int64);
  end;

var
  frmMain: TfrmMain;

const

  cFormHeader        = 'Xml Editor with NativeXml (c) 2001-2011 SimDesign B.V.';

resourcestring
  sCannotInsertRootElement = 'You cannot insert another root element!';

implementation

{$R *.DFM}

type
  // This is the node record that is appended to each node in the virtual treeview
  PNodeRec = ^TNodeRec;
  TNodeRec = record
     FNode: TXmlNode;
  end;

{ TFrmMain }

procedure TfrmMain.acAddCommentExecute(Sender: TObject);
begin
  FXml.CommentString := InputBox('Add a comment', 'Comment:', FXml.CommentString);
end;

procedure TfrmMain.acHideSingleNodesExecute(Sender: TObject);
begin
  acHideSingleNodes.Checked := not acHideSingleNodes.Checked;
  Regenerate;
end;

procedure TfrmMain.acFileNewExecute(Sender: TObject);
// Create a blank Xml document with a blank root
begin
  FXml.New;
  FXml.Root.Name := 'root';
  Regenerate;
end;

procedure TfrmMain.acFileOpenExecute(Sender: TObject);
var
  F: TFileStream;
  // local
  function IsBinary(S: TStream): boolean;
  var
    Cookie: array[0..3] of AnsiChar;
  begin
    S.Position := 0;
    S.Read(Cookie, 4);
    Result := (Cookie = '$BXM');
    S.Position := 0;
  end;
// main
begin
  // Open a file
  if odFileOpen.Execute then
  begin
    FFileName := odFileOpen.FileName;
    try
      reDebug.Clear;

      // preserve whitespace?
      FXml.PreserveWhiteSpace := FPreserveWhiteSpace;

      // allow progress bar to update
      FXml.OnProgress := XmlProgress;

      // load the file
      F := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
      try
        FFileSize := F.Size;
        if IsBinary(F) then
          FXml.LoadFromBinaryStream(F)
        else
          FXml.LoadFromStream(F);
      finally
        F.Free;
      end;

{      // create canonical xml
      if FCanonicalXml then
      begin
        // class method Canonicalize
        FXml.Canonicalize;
      end;}

      // Display properties on statusbar
      if Length(FXml.Charset) > 0 then
        sbMain.SimpleText := Format(' Encoding="%s"', [FXml.Charset]);
      sbMain.SimpleText := sbMain.SimpleText +
        Format('unique strings: %d', [FXml.SymbolTable.SymbolCount]);
    except
      // Show exception on status bar
      on E: Exception do
        sbMain.SimpleText := 'Exception in editor:' + E.Message;
    end;
    Regenerate;
  end;
end;

procedure TfrmMain.acLoadFromURLExecute(Sender: TObject);
var
  URL: string;
  Res: boolean;
begin
  // Ask user for URL
  URL := 'http://www.simdesign.nl/xml.html';
  Res := InputQuery('Which URL to download?', 'URL:', URL);
  if not Res then
    exit;

  FXml.XmlFormat := xfPreserve;
  FXml.FixStructuralErrors := True;
  FXml.LoadFromURL(URL);

  Regenerate;
end;

procedure TfrmMain.acFileSaveAsExecute(Sender: TObject);
begin
  // Save a file
  sdFileSave.DefaultExt := '.xml';
  if sdFileSave.Execute then
  begin
    FFileName := sdFileSave.FileName;
    FXml.SaveToFile(FFilename);
    Regenerate;
  end;
end;

procedure TfrmMain.mnuSaveToBinaryClick(Sender: TObject);
begin
  // Save a binary file
  sdFileSave.DefaultExt := '.bxm';
  if sdFileSave.Execute then
  begin
    FFileName := sdFileSave.FileName;
    FXml.SaveToBinaryFile(FFilename);
    Regenerate;
  end;
end;

procedure TfrmMain.acSaveDebugInfoExecute(Sender: TObject);
begin
  // Save debug info
  if sdDebugSave.Execute then
  begin
    reDebug.Lines.SaveToFile(sdDebugSave.FileName);
  end;
end;

procedure TfrmMain.acSaveAsWithOptionsExecute(Sender: TObject);
var
  Dlg: TXmlOutputOptionsDlg;
  FExternalEncoding: TsdStringEncoding;
  FExternalCodepage: integer;
  FUseStoredEncoding: boolean;
begin
  FUseStoredEncoding := True;
  FExternalEncoding := seUTF8;
  FExternalCodepage := CP_UTF8;

  // dialog box
  Dlg := TXmlOutputOptionsDlg.Create(Self);
  try
    if Dlg.lbDefaultEncodings.ItemIndex < 0 then
      Dlg.lbDefaultEncodings.ItemIndex := 0;

    if Dlg.ShowModal = mrOK then
    begin

      if Dlg.rbDefaultEncodings.Checked then
      begin
        FUseStoredEncoding := False;
        case Dlg.lbDefaultEncodings.ItemIndex of
        0:
          begin
            FExternalEncoding := seUTF8;
            FExternalCodepage := CP_UTF8;
          end;
        1:
          begin
            FExternalEncoding := seUTF16LE;
            FExternalCodepage := CP_UTF16;
          end;
        2:
          begin
            FExternalEncoding := seAnsi;
            FExternalCodepage := CP_1252;
          end;
        end;
      end;

      if Dlg.rbCodepage.Checked then
      begin
        FUseStoredEncoding := False;
        FExternalEncoding := seAnsi;
        FExternalCodepage := sdCharsetToCodepage(Dlg.edCodepage.Text);
      end;

      // Save a file
      if sdFileSave.Execute then
      begin
        // specified encoding used?
        if not FUseStoredEncoding then
        begin
          FXml.ExternalEncoding := FExternalEncoding;
          FXml.ExternalCodepage := FExternalCodepage;
        end;

        // Other options
        FXml.XmlFormat :=  TXmlFormatType(Dlg.rgXmlFormat.ItemIndex);

        FFileName := sdFileSave.FileName;
        FXml.SaveToFile(FFilename);
        Regenerate;

      end;
    end;
  finally
    Dlg.Free;
  end;

end;

procedure TfrmMain.acOutputReadableExecute(Sender: TObject);
begin
  if IsUpdating then
    exit;
  // switch them around
  case FXml.XmlFormat of
  xfReadable: FXml.XmlFormat := xfCompact;
  else
    FXml.XmlFormat := xfReadable;
  end;
  UpdateMenu;
end;

procedure TfrmMain.acSingleNodeAsAttribExecute(Sender: TObject);
begin
  acSingleNodeAsAttrib.Checked := not acSingleNodeAsAttrib.Checked;
  if acSingleNodeAsAttrib.Checked then
    tsTags.Caption := 'Attributes and child elements'
  else
    tsTags.Caption := 'Attributes';
  RegenerateProperties;
end;

procedure TfrmMain.BeginUpdate;
begin
  inc(FUpdateCount);
end;

function TfrmMain.ElementTypeToImageIndex(AElementType: TsdElementType): integer;
begin
  case AElementType of
  xeElement:     Result := 1;
  xeComment:     Result := 2;
  xeCData:       Result := 3;
  xeCondSection: Result := 12;
  xeCharData:    Result := 4;
  xeWhiteSpace:  Result := 4;
  xeQuotedText:  Result := 4;
  xeDeclaration: Result := 5;
  xeStylesheet:  Result := 6;
  xeDoctype:     Result := 7;
  xeDtdElement:  Result := 8;
  xeDtdAttList:  Result := 9;
  xeDtdEntity:   Result := 10;
  xeDtdNotation: Result := 11;
  xeInstruction: Result := 13;
  xeAttribute:   Result := 0;
  else
    Result := 13;
  end;//case
end;

procedure TfrmMain.EndUpdate;
begin
  dec(FUpdateCount);
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FXml := TNativeXml.Create(Self);
  FXml.XmlFormat  := xfCompact;
  FXml.EolStyle   := esCRLF;
  FXml.OnDebugOut := ShowDebugMsg;

  // Open cmdline parameter 1 file (when associated with this tool)
  if length(ParamStr(1)) > 0 then
  begin
    FXml.LoadFromFile(ParamStr(1));
    Regenerate;
  end else
    acFileNew.Execute;

  FFocusedAttributeIndex := -1;

  FPreserveWhiteSpace := acPreserveWS.Checked;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FXml.Free;
end;

function TfrmMain.IsSingleNode(ANode: TXmlNode): boolean;
begin
  Result := True;
  if assigned(ANode) then
    if (ANode.NodeCount > 0) or (ANode.AttributeCount > 0) then
      Result := False;
end;

function TfrmMain.IsUpdating: boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TfrmMain.mnuFixstructuralerrorsClick(Sender: TObject);
begin
  FXml.FixStructuralErrors := mnuFixStructuralErrors.Checked;
end;

procedure TfrmMain.GetPropertyInfo(Node: PVirtualNode;
  var IsAttribute: boolean; var Index: integer);
var
  AIndex, ANodeIndex: integer;
begin
  IsAttribute := True;
  Index := -1;
  // Get the data of the tag's properties
  if assigned(FFocusedNode) then
    with FFocusedNode do
    begin
      AIndex := Node.Index;
      // Attributes
      if (AIndex >= 0) and (AIndex < AttributeCount) then
      begin
        Index := AIndex;
        exit;
      end;
      // Special feature: show single nodes as attribute
      AIndex := AIndex - AttributeCount;
      if acSingleNodeAsAttrib.Checked then
      begin
        // Find the single node at AIndex
        ANodeIndex := -1;
        while AIndex >= 0 do
        begin
          inc(ANodeIndex);
          if not assigned(Nodes[ANodeIndex]) then
            exit;
          if IsSingleNode(Nodes[ANodeIndex]) then
            dec(AIndex);
        end;
        if assigned(Nodes[ANodeIndex]) then
        begin
          IsAttribute := False;
          Index := ANodeIndex;
        end;
      end;
    end;
end;

procedure TfrmMain.stAttributesGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  IsAttribute: boolean;
  Index: integer;
  ANode: TXmlNode;
begin
  GetPropertyInfo(Node, IsAttribute, Index);
  if Index < 0 then exit;
  if IsAttribute then
  begin
    case Column of
    0: CellText := FFocusedNode.AttributeName[Index];
    1: CellText := TXmlNode.UTF8ToWide(FFocusedNode.AttributeValue[Index]);
    end;//case
  end else
  begin
    ANode := FFocusedNode.Nodes[Index];
    case Column of
    0: CellText := ANode.Name;
    1: CellText := ANode.ValueUnicode;
    end;//case
  end;
end;

procedure TfrmMain.stAttributesGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  IsAttribute: boolean;
  Index: integer;
begin
  ImageIndex := -1;
  if Kind = ikOverlay then
    exit;
  if Column > 0 then
    exit;
  GetPropertyInfo(Node, IsAttribute, Index);
  if Index < 0 then
    exit;
  if IsAttribute then
    ImageIndex := 0
  else
    ImageIndex := ElementTypeToImageIndex(FFocusedNode.Nodes[Index].ElementType);
end;

procedure TfrmMain.stAttributesEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := True;
end;

procedure TfrmMain.stAttributesNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: String);
var
  IsAttribute: boolean;
  Index: integer;
  ANode: TXmlNode;
begin
  GetPropertyInfo(Node, IsAttribute, Index);
  if Index < 0 then
    exit;
  if IsAttribute then
  begin
    case Column of
    0: FFocusedNode.AttributeName[Index] := NewText;
    1: FFocusedNode.AttributeValue[Index] := TXmlNode.WideToUTF8(NewText);
    end;//case
  end else
  begin
    ANode := FFocusedNode.Nodes[Index];
    case Column of
    0: ANode.Name := NewText;
    1: ANode.ValueUnicode := NewText;
    end;//case
  end;
end;

function TfrmMain.MultiAttrCount(ANode: TXmlNode): integer;
// Count the attributes, but if acSingleNodeAsAttrib is checked we also add
// the single nodes
var
  i: integer;
begin
  Result := 0;
  if not assigned(ANode) then
    exit;
  Result := ANode.AttributeCount;
  if acSingleNodeAsAttrib.Checked then
    for i := 0 to ANode.NodeCount - 1 do
      if IsSingleNode(ANode[i]) then
        inc(Result);
end;

function TfrmMain.MultiNodeByIndex(ANode: TXmlNode;
  AIndex: integer): TXmlNode;
// Return the child node of ANode at AIndex, taking into account the setting for
// acHideSingleNodes
var
  i: integer;
begin
  Result := nil;
  if assigned(ANode) then
    with ANode do
    begin
      if acHideSingleNodes.Checked then
      begin
        for i := 0 to NodeCount - 1 do
          if not IsSingleNode(Nodes[i]) then
          begin
            dec(AIndex);
            if AIndex < 0 then
            begin
              Result := Nodes[i];
              exit;
            end;
          end;
      end else
        Result := Nodes[AIndex];
    end;
end;

function TfrmMain.MultiNodeCount(ANode: TXmlNode): integer;
// Count the number of nodes, taking into account the setting for acHideSingleNodes
var
  i: integer;
begin
  Result := 0;
  if assigned(ANode) then
    with ANode do
    begin
      if acHideSingleNodes.Checked then
      begin
        for i := 0 to NodeCount - 1 do
          if not IsSingleNode(Nodes[i]) then
            inc(Result)
      end else
        Result := NodeCount;
    end;
end;

procedure TfrmMain.pcTreeChange(Sender: TObject);
begin
  if (pcTree.ActivePage = tsXmlSource) then
  begin
    ShowXmlSource;
  end;
end;

procedure TfrmMain.stXmlTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
// Use this event to display related info in other panes
var
  FData: PNodeRec;
begin
  // Signal the setup that the current tag changed
  FData := Sender.GetNodeData(Node);
  if assigned(FData) then
    FFocusedNode := FData^.FNode
  else
    FFocusedNode := nil;
  RegenerateProperties;
end;

procedure TfrmMain.stXmlTreeEditing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := true;
end;

procedure TfrmMain.stXmlTreeNewText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; NewText: WideString);
var
  FData: PNodeRec;
begin
  FData := Sender.GetNodeData(Node);
  case Column of
  0: FData.FNode.Name := NewText;
  1: FData.FNode.ValueUnicode := NewText;
  end;//case
end;

procedure TfrmMain.stXmlTreeExpanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
var
  FData: PNodeRec;
begin
  with stXMLTree do
  begin
    ChildCount[Node] := 0;
    FData := Sender.GetNodeData(Node);
    if assigned(FData^.FNode) then
      ChildCount[Node] := MultiNodeCount(FData^.FNode);
    InvalidateToBottom(Node);
  end;
end;

procedure TfrmMain.stXmlTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
// Get the data for this tree node
var
  FData: PNodeRec;
begin
  FData := Sender.GetNodeData(Node);
  if assigned(FData^.FNode) then
  begin
    case Column of
    0: CellText := FData^.FNode.Name;
    // As can be seen here, both XmlDocuments as TVirtualTreeview support
    // widestring, so you can view your differently-encoded XML documents correctly
    1: CellText := FData^.FNode.Value;
    end;
  end;
end;

procedure TfrmMain.stXmlTreeInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
// Initialize the tree node; assign its corresponding Xml node and tell it if it
// has any children yes or no
var
  FData, FParentData: PNodeRec;
  FParentNode: TXmlNode;
begin
  if ParentNode = nil then
  begin
    // Root node
    if assigned(FXml) then
    begin
      FData := Sender.GetNodeData(Node);
      FData^.FNode := FXml.RootNodes[Node.Index];
      InitialStates := [];
      if assigned(FData^.FNode) then
      begin
        FData^.FNode.Tag := integer(Node);
        // initial states
        if MultiNodeCount(FData^.FNode) > 0 then
          InitialStates := [ivsHasChildren];
      end;
    end;

  end else
  begin

    // We need to use the parent node
    FParentData := Sender.GetNodeData(ParentNode);
    FParentNode := FParentData.FNode;

    // Find the new node
    FData := Sender.GetNodeData(Node);
    if integer(Node.Index) < FParentNode.NodeCount then
    begin
      FData^.FNode := MultiNodeByIndex(FParentNode, Node.Index);
      InitialStates := [];
      if assigned(FData^.FNode) then
      begin
        FData^.FNode.Tag := integer(Node);
        // initial states
        if MultiNodeCount(FData^.FNode) > 0 then
          InitialStates := [ivsHasChildren];
      end;
    end;

  end;
end;

procedure TfrmMain.stXmlTreeGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  FData: PNodeRec;
  ANode: TXmlNode;
begin
  // The image belonging to the treenode
  ImageIndex := -1;
  if Kind in [ikNormal, ikSelected] then
    if Column = 0 then
    begin
      FData := Sender.GetNodeData(Node);
      if assigned(FData) then
      begin
        ANode := FData.FNode;
        if assigned(ANode) then
          ImageIndex := ElementTypeToImageIndex(ANode.ElementType);
      end;
    end;
end;

procedure TfrmMain.Regenerate;
// Regenerate all screen elements, rebuilds the treeview
begin
  // Redraw XML tree
  stXmlTree.Clear;
  stXmlTree.RootNodeCount := FXml.RootNodes.Count;

  // Properties pane
  RegenerateProperties;

  // Form caption
  if Length(FFileName) > 0 then
    Caption := Format('%s %s [%s]', [cFormHeader, cNativeXmlVersion, FFilename])
  else
    Caption := Format('%s %s - No file selected', [cFormHeader, cNativeXmlVersion]);

  // Update menu enabled/checked states
  UpdateMenu;
end;

procedure TfrmMain.RegenerateFromNode(ANode: TXmlNode);
var
  TreeNode: PVirtualNode;
begin
  if not assigned(ANode) then
  begin
    Regenerate;
    exit;
  end;
  TreeNode := pointer(ANode.Tag);
  if not assigned(TreeNode) then
  begin
    Regenerate;
    exit;
  end;
  stXmlTree.ResetNode(TreeNode);
  stXmlTree.Expanded[TreeNode] := True;
end;

procedure TfrmMain.RegenerateProperties;
// Invalidate the properties listview
begin
  stAttributes.Clear;
  stAttributes.RootNodeCount := MultiAttrCount(FFocusedNode);
end;

procedure TfrmMain.UpdateMenu;
begin
  BeginUpdate;
  try
    acOutputReadable.Checked := FXml.XmlFormat = xfReadable;
    acPreserveWS.Checked := FXml.PreserveWhiteSpace;
    acCanonicalXml.Checked := FCanonicalXml;
  finally
     EndUpdate;
  end;
end;

procedure TfrmMain.stXmlTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_DELETE:
    // User pressed DEL
    acElementDeleteExecute(nil);
  end;//case
end;

procedure TfrmMain.acFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.acElementDeleteExecute(Sender: TObject);
// Delete the focused element
var
  AParent: TXmlNode;
begin
  if assigned(FFocusedNode) and (FFocusedNode <> FXml.Root) then
  begin
    AParent := FFocusedNode.Parent;
    AParent.NodeRemove(FFocusedNode);
    FFocusedNode := nil;
    RegenerateFromNode(AParent);
  end;
end;

procedure TfrmMain.acElementInsertBeforeExecute(Sender: TObject);
var
  AParent, ANode: TXmlNode;
begin
  if not assigned(FFocusedNode) then
    exit;
  AParent := FFocusedNode.Parent;
  if not assigned(AParent) then
  begin
    ShowMessage(sCannotInsertRootElement);
    exit;
  end;
  ANode := TsdElement.CreateParentNear(FXml, AParent, FFocusedNode, True);
  ANode.Name := 'element';
  RegenerateFromNode(AParent);
end;

procedure TfrmMain.acElementInsertAfterExecute(Sender: TObject);
var
  AParent, ANode: TXmlNode;
begin
  if not assigned(FFocusedNode) then
    exit;
  AParent := FFocusedNode.Parent;
  if AParent = nil then
  begin
    ShowMessage(sCannotInsertRootElement);
    exit;
  end;
  if assigned(AParent) then
  begin
    ANode := TsdElement.CreateParentNear(FXml, AParent, FFocusedNode, False);
    ANode.Name := 'element';
    RegenerateFromNode(AParent);
  end;
end;

procedure TfrmMain.acElementInsertSubExecute(Sender: TObject);
var
  ANode: TXmlNode;
begin
  if not assigned(FFocusedNode) then
    exit;
  ANode := TsdElement.CreateParent(FXml, FFocusedNode);
  ANode.Name := 'element';
  RegenerateFromNode(FFocusedNode);
end;

procedure TfrmMain.acCommentInsertExecute(Sender: TObject);
var
  AParent: TXmlNode;
begin
  if not assigned(FFocusedNode) then
    exit;
  AParent := FFocusedNode.Parent;
  if assigned(AParent) then
  begin
    TsdComment.CreateParentNear(FXml, AParent, FFocusedNode, True);
    RegenerateFromNode(AParent);
  end;
end;

procedure TfrmMain.acElementUpExecute(Sender: TObject);
var
  Idx: integer;
  AParent: TXmlNode;
begin
  if not assigned(FFocusedNode) then
    exit;
  AParent := FFocusedNode.Parent;
  if assigned(AParent) then
  begin
    Idx := AParent.NodeIndexOf(FFocusedNode);
    if Idx > AParent.AttributeCount then
      AParent.NodeExchange(Idx - 1, Idx);
    RegenerateFromNode(AParent);
  end;
end;

procedure TfrmMain.acElementDownExecute(Sender: TObject);
var
  Idx: integer;
  AParent: TXmlNode;
begin
  if not assigned(FFocusedNode) then
    exit;
  AParent := FFocusedNode.Parent;
  if assigned(AParent) then
  begin
    Idx := AParent.NodeIndexOf(FFocusedNode);
    if Idx < AParent.NodeCount - 1 then
      AParent.NodeExchange(Idx, Idx + 1);
    RegenerateFromNode(AParent);
  end;
end;

procedure TfrmMain.stAttributesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  IsAttribute: boolean;
  Index: integer;
begin
  if not assigned(Node) then
  begin
    FFocusedAttributeIndex := -1;
    exit;
  end;
  GetPropertyInfo(Node, IsAttribute, Index);
  if IsAttribute then
    FFocusedAttributeIndex := Index
  else
    FFocusedAttributeIndex := -1;
end;

procedure TfrmMain.stAttributesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
  VK_DELETE:
    // User pressed DEL
    acAttributeDeleteExecute(nil);
  end;//case
end;

procedure TfrmMain.acAttributeAddExecute(Sender: TObject);
begin
  if assigned(FFocusedNode) then
    FFocusedNode.AttributeAdd('name', 'value');
  RegenerateProperties;
  RegenerateFromNode(FFocusedNode);
end;

procedure TfrmMain.acAttributeDeleteExecute(Sender: TObject);
var
  A: TsdAttribute;
begin
  if assigned(FFocusedNode) and (FFocusedAttributeIndex >= 0) then
  begin
    A := FFocusedNode.Attributes[FFocusedAttributeIndex];
    FFocusedNode.NodeRemove(A);
  end;
  RegenerateProperties;
  RegenerateFromNode(FFocusedNode);
end;

procedure TfrmMain.acAttributeUpExecute(Sender: TObject);
begin
  if assigned(FFocusedNode) and (FFocusedAttributeIndex > 0) then
    FFocusedNode.NodeExchange(FFocusedAttributeIndex - 1, FFocusedAttributeIndex);
  RegenerateProperties;
end;

procedure TfrmMain.acAttributeDownExecute(Sender: TObject);
begin
  if assigned(FFocusedNode) and (FFocusedAttributeIndex < FFocusedNode.AttributeCount - 1) then
    FFocusedNode.NodeExchange(FFocusedAttributeIndex, FFocusedAttributeIndex + 1);
  RegenerateProperties;
end;

procedure TfrmMain.ShowDebugMsg(Sender: TObject; WarnStyle: TsdWarnStyle; const AMessage: Utf8String);
begin
  // Debug panel
  reDebug.Lines.Add(Format('[%s] %s: %s', [cWarnStyleNames[WarnStyle], Sender.ClassName, AMessage]));
end;

procedure TfrmMain.ShowXmlSource;
begin
  // Show our Xml Document
  FXml.IndentString := '  ';
  if FXml.IsEmpty then
    smXmlSource.Text := ''
  else
    smXMLSource.Text := FXml.WriteToLocalUnicodeString;
end;

procedure TfrmMain.acPreserveWSExecute(Sender: TObject);
begin
  FPreserveWhiteSpace := acPreserveWS.Checked;
end;

procedure TfrmMain.acCanonicalXMLExecute(Sender: TObject);
begin
  FCanonicalXml := acCanonicalXML.Checked;
end;

procedure TfrmMain.XmlProgress(Sender: TObject; Position: int64);
begin
  if FFileSize > 0 then
    pbMain.Position := round((Position / FFileSize) * 100);
end;

end.
