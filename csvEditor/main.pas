unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  Buttons, ComCtrls, ActnList, Menus, StdCtrls,
  {$if defined(Linux) or defined(Darwin)}
  cthreads,
  {$endif}
  uCsv, Types, uConfig;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    ActionPaste: TAction;
    ActionCopy: TAction;
    ActionDelCol: TAction;
    ActionInsertCol: TAction;
    ActionInsertRow: TAction;
    ActionAddCol: TAction;
    ActionAddRow: TAction;
    ActionDelRow: TAction;
    ActionSave: TAction;
    ActionOpen: TAction;
    ActionList1: TActionList;
    cbFormulas: TCheckBox;
    edPosition: TEdit;
    edValue: TEdit;
    ImageList1: TImageList;
    lblCached: TLabel;
    lblModifs: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    N2: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    N1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    pnlEdit: TPanel;
    Panel3: TPanel;
    PopupMenuRecent: TPopupMenu;
    PopupMenuCols: TPopupMenu;
    PopupMenuRows: TPopupMenu;
    PopupMenuCells: TPopupMenu;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure ActionAddColExecute(Sender: TObject);
    procedure ActionAddRowExecute(Sender: TObject);
    procedure ActionCopyExecute(Sender: TObject);
    procedure ActionDelRowExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure cbFormulasChange(Sender: TObject);
    procedure edValueExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1GetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FColCLicked,
    FRowCLicked : longint;
    function GetConfigFilename: String;
    procedure Open(const aFilename : String);
    procedure DoSave;
    procedure MenuRecentClick(Sender : TObject);
    procedure LoadRecentMenu;
  private
    FStream : TCsvStream;
    FConfig : TConfig;
    procedure Notifyer(Sender: TObject; const Msg: string; State: TCsvState; nbRows : Integer);
    procedure EnableActions;
    procedure SizeGrid;
    property ConfigFilename : String read GetConfigFilename;
  public

  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses
  Clipbrd;

const
{$if defined(Darwin) or defined(Linux)}
  CS_CONFIG_PATH = '.config/csvEditor';
  CS_CONFIG_JSON = '/config.json';
{$else}
  CS_CONFIG_JSON = 'config.json';
{$endif}

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
var
  s : string;
  x, y : integer;
begin
  FConfig := TConfig(TConfig.Load(ConfigFilename, TConfig.Create));
  if FileExists(ConfigFilename) then
    BoundsRect := FConfig.GetBoundsRect;

  LoadRecentMenu;

  FStream := nil;
  StatusBar1.Panels[0].Width := StatusBar1.Width - 240;
  StringGrid1.ColWidths[0] := 50;

  FStream := TCsvStream.Create('', @Notifyer);
  cbFormulas.Checked := FConfig.UseFormulas;
  for y := 0 to 5 do
  begin
    s := '';
    for x := 0 To 5 do
      s := s + '"";';
    SetLength(s, Length(s)-1);
    FStream.AddRow(s);
  end;
  SizeGrid;
  EnableActions;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FStream) then
    FStream.Free;
  FConfig.SetBoundsRect(BoundsRect);
  FConfig.Save(ConfigFilename);
  FConfig.Free;
end;

procedure TFrmMain.FormResize(Sender: TObject);
begin
  StatusBar1.Panels[0].Width := ClientWidth div 2;
  pnlEdit.Width := ClientWidth div 2;
end;

procedure TFrmMain.MenuItem12Click(Sender: TObject);
begin
  Close;
end;

procedure TFrmMain.PopupMenuPopup(Sender: TObject);
begin
  EnableActions;
end;

procedure TFrmMain.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);

  function GetRowHeader(V : integer):String;
  var
    adiv, amod : integer;
    colLetter : string;
  begin
    adiv := V+1;
    colLetter := '';
    amod := 0;
    while adiv > 0 do
    begin
      amod := (adiv - 1) mod 26;
      colLetter := Chr(65 + amod) + colLetter;
      adiv := ((adiv - amod) div 26);
    end;
    result := colLetter;
  end;

var
  x, y : integer;
  s : string;
begin
  with StringGrid1 do
  begin
    with Canvas do
    begin
      if (aCol = 0) or (aRow = 0) then
        Brush.Color:=clLtGray
      else
      if gdSelected in aState then
        Brush.Color:=clLime
      else
        Brush.Color:=clwhite;

      FillRect(aRect);

      if gdFocused in aState then
        DrawFocusRect(aRect);

      if (aCol = 0) and (aRow = 0) then
        exit
      else
      if (aCol = 0) or (aRow = 0) then
      begin
        if aCol = 0 then
          s := IntToStr(aRow)
        else
        if aRow = 0 then
          s := GetRowHeader(aCol-1);

        y := ((aRect.Bottom - aRect.Top) - TextHeight(s)) div 2;
        x := ((aRect.Right - aRect.Left) - TextWidth(s)) div 2;
        TextOut(aRect.Left + x, aRect.Top + y, s);
      end
      else
      if Assigned (FStream) then
      begin
        s := FStream.CellAsString[aRow-1, aCol-1];
        y := ((aRect.Bottom - aRect.Top) - TextHeight(s)) div 2;
        TextOut(aRect.Left + 1, aRect.Top + y, s);
      end;
    end;
  end;
end;


procedure TFrmMain.StringGrid1GetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
begin
  if Assigned(FStream) and (aCol > 0) and (aRow > 0) then
    HintText := FStream.CellAsString[arow - 1, acol - 1];
end;

procedure TFrmMain.StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  if Assigned(FStream) and (aCol > 0) and (aRow > 0) then
    Value := FStream.CellAsStringNoEval[arow - 1, acol - 1];
end;

procedure TFrmMain.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
  begin
    StringGrid1.MouseToCell(X, Y, FColCLicked, FRowCLicked);

    if FColCLicked = 0 then
      StringGrid1.PopupMenu := PopupMenuRows
    else
    if FRowCLicked = 0 then
      StringGrid1.PopupMenu := PopupMenuCols
    else
      StringGrid1.PopupMenu := PopupMenuCells;
  end;
end;

procedure TFrmMain.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := (aCol > 0) and (aRow > 0);
  if Assigned(FStream) and CanSelect then
  begin
    edPosition.Text := Char(65 + (aCol - 1)) + IntToStr(aRow);
    edValue.Text := FStream.CellAsStringNoEval[arow - 1, acol - 1];
  end;
end;

procedure TFrmMain.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  with FStream do
    if CellAsString[arow - 1, acol - 1] <> Value then
      CellAsString[arow - 1, acol - 1] := Value;
  EnableActions;
end;

function TFrmMain.GetConfigFilename: String;
begin
  // make sure config folder exists
{$if defined(Darwin) or defined(Linux)}
  result := expandfilename('~/') + CS_CONFIG_PATH;
  ForceDirectories(result);
  result := result + CS_CONFIG_JSON;
{$else}
  result := IncludeTrailingPathDelimiter(GetAppConfigDir(False)) + CS_CONFIG_JSON;
{$endif}
end;

procedure TFrmMain.DoSave;
begin
  if not FStream.Modified then
    Exit;

  if FStream.Filename = '' then
  begin
    with TSaveDialog.Create(nil) do
    try
      DefaultExt := 'csv';
      FileName := 'Unitiled.csv';

      if Execute then
        FStream.Save(Filename);
    finally
      Free;
    end;
  end
  else
    FStream.Save;
end;

procedure TFrmMain.Open(const aFilename : String);
begin
  if Assigned(FStream) then
    if FStream.ReadState = csAnalyzing then
      FStream.Cancel;


  if Assigned(FStream) and FStream.Modified then
    case MessageDlg('Save changes ?', mtInformation, mbYesNoCancel, 0) of
      mrYes:    DoSave;
      MrCancel:  exit;
    end;

  Screen.Cursor := crHourGlass;
  FreeAndNil(FStream);
  FConfig.AddRecent(aFilename);
  LoadRecentMenu;
  FStream := TCsvStream.Create(aFilename, @Notifyer);
  FStream.SolveFormulas := cbFormulas.Checked;
  EnableActions;
end;

procedure TFrmMain.MenuRecentClick(Sender: TObject);
begin
  Open(TmenuItem(Sender).Caption);
end;

procedure TFrmMain.LoadRecentMenu;
var
  i : integer;

  procedure CreateMenuItem(const aCaption : String);
  var
    mn : TMenuItem;
  begin
    mn := TMenuItem.Create(PopupMenuRecent);
    mn.Caption := aCaption;
    mn.OnClick := @MenuRecentClick;
    PopupMenuRecent.Items.Add(mn);
  end;
begin
  PopupMenuRecent.Items.Clear;
  for i := 0 to FConfig.Recent.Count - 1 do
    CreateMenuItem(FConfig.Recent[i]);
end;

procedure TFrmMain.ActionOpenExecute(Sender: TObject);
begin
  With OpenDialog1 do
    if Execute then
      Open(Filename);
end;

procedure TFrmMain.ActionAddRowExecute(Sender: TObject);
var
  s : String;
  j : integer;
begin
  s := '';
  with FStream do
    if RowCount > 1 then
    begin
      for j := 0 to ColCounts[RowCount - 1] -1 do
        s := s + '"";';
      AddRow(s);
    end;
  EnableActions;
  SizeGrid;
end;

procedure TFrmMain.ActionCopyExecute(Sender: TObject);
var
  r : TRect;
  x, y : integer;
  s : string;
begin
  if StringGrid1.SelectedRangeCount > 0 then
  begin
    r := StringGrid1.SelectedRange[0];
    s := '';
    for y := r.Top to r.Bottom do
    begin
      for x := r.Left to r.Right do
        s := s + '"' + FStream.CellAsString[y - 1, x - 1] + '";';
      SetLength(s, Length(s)-1);
      s := s +#13+#10;
    end;
    Clipboard.AsText := s;
  end;
end;

procedure TFrmMain.ActionDelRowExecute(Sender: TObject);
begin
  FStream.DeleteRow(FRowCLicked - 1);
  SizeGrid;
end;

procedure TFrmMain.ActionPasteExecute(Sender: TObject);
begin
  edValue.Clear;
  edValue.PasteFromClipboard;
  edValue.OnExit(Self);
end;

procedure TFrmMain.ActionAddColExecute(Sender: TObject);
begin
  with FStream do
    AddCol(StringGrid1.Row, '');

  SizeGrid;
  EnableActions;
end;

procedure TFrmMain.ActionSaveExecute(Sender: TObject);
begin
  if FStream.Modified then
  try
    Screen.Cursor := crHourglass;
    DoSave;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFrmMain.cbFormulasChange(Sender: TObject);
begin
  FConfig.UseFormulas := cbFormulas.Checked;
  if Assigned(FStream) then
  begin
    FStream.SolveFormulas := cbFormulas.Checked;
    StringGrid1.Invalidate;
  end;
end;

procedure TFrmMain.edValueExit(Sender: TObject);
begin
  with StringGrid1, FStream do
    if CellAsString[Row - 1, Col - 1] <> edValue.Text then
      CellAsString[Row - 1 , Col - 1] := edValue.Text;
  EnableActions;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if Assigned(FStream) then
    if FStream.ReadState = csAnalyzing then
      FStream.Cancel;

  CanClose :=  True;

  if Assigned(FStream) and FStream.Modified then
    case MessageDlg('Save changes ?', mtInformation, mbYesNoCancel, 0) of
      mrYes:    DoSave;
      MrCancel:  CanClose := False;
    end;
end;

procedure TFrmMain.Notifyer(Sender: TObject; const Msg: string; State: TCsvState; nbRows : Integer);
begin
  with StatusBar1 do
  begin
    if (state <> csModifs) and (state <> csCached) then
      Panels[0].Text := Msg;

    Panels[1].Text := 'RowCount: ' + IntToStr(nbRows);
    Panels[2].Text := 'ColCount: ' + IntToStr(FStream.MaxColCount);

    case State of
      csReady :
        begin
          Panels[0].Text := 'Ready.';
          StringGrid1.Options := StringGrid1.Options + [goEditing];
          SizeGrid;
          Screen.Cursor := crDefault;
        end;

      csSaving:
        StringGrid1.Options := StringGrid1.Options - [goEditing];

      csAnalyzing :
        begin
          SizeGrid;
          if nbRows > 0 then
            Screen.Cursor := crDefault;
        end;

      csModifs: lblModifs.Caption := 'Modifed lines:' + IntToStr(nbRows);
      csCached: lblCached.Caption := 'Cached lines :' + IntToStr(nbRows) + '  ';
    end;

    Refresh;
  end;
end;

procedure TFrmMain.EnableActions;
begin
  ActionOpen.Enabled := True;
  ActionSave.Enabled := Assigned(FStream) and FStream.Modified;

  ActionPaste.Enabled := Assigned(FStream) and Clipboard.HasFormat(CF_TEXT);
  ActionCopy.Enabled := Assigned(FStream);
  ActionDelCol.Enabled := Assigned(FStream);
  ActionInsertCol.Enabled := Assigned(FStream);
  ActionInsertRow.Enabled := Assigned(FStream);
  ActionAddCol.Enabled := Assigned(FStream);
  ActionAddRow.Enabled := Assigned(FStream);
  ActionDelRow.Enabled := Assigned(FStream);
end;

procedure TFrmMain.SizeGrid;
begin
  with StringGrid1 do
  begin
    ColCount := FStream.MaxColCount + 1;
    RowCount := FStream.RowCount + 1;
  end;
end;

end.

