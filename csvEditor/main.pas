unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  Buttons, ComCtrls, ActnList, Menus, StdCtrls, uCsv, Types;

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
    edPosition: TEdit;
    edValue: TEdit;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel2: TPanel;
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
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionPasteExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure edValueExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PopupMenuPopup(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1EditingDone(Sender: TObject);
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
    FStream : TCsvStream;
    procedure Notifyer(Sender: TObject; const Msg: string; State: TCsvState; nbRows : Integer);
    procedure EnableActions;
    procedure SizeGrid;
  public

  end;

var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses
  Clipbrd;

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FStream := nil;
  EnableActions;
  StatusBar1.Panels[0].Width := StatusBar1.Width - 240;
  StringGrid1.ColWidths[0] := 50;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FStream) then
    FStream.Free;
end;

procedure TFrmMain.PopupMenuPopup(Sender: TObject);
begin
  EnableActions;
end;

procedure TFrmMain.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);

  function GetRowHeader(V : integer):String;
  var
    st : integer;
    vv : integer;
  begin
    result := '';
    vv := -1;
    while v >= 26 do
    begin
      inc(vv);
      dec(v, 26);
    end;
    while vv >= 26 do
    begin
      result := result + 'A';
      dec(vv, 26);
    end;
    if vv >= 0 then
      result := result + Chr(65 + vv);

    result := result + Chr(65 + v);
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
        TextOut(aRect.Left, aRect.Top + y, s);
      end;
    end;
  end;
end;

procedure TFrmMain.StringGrid1EditingDone(Sender: TObject);
begin
  //FStream.CellAsString[arow - 1, acol - 1];
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
    Value := FStream.CellAsString[arow - 1, acol - 1];
end;

procedure TFrmMain.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  aCol, aRow : longint;
begin
  if Button = mbRight then
  begin
    StringGrid1.MouseToCell(X, Y, aCol, aRow);
    if aCol = 0 then
      StringGrid1.PopupMenu := PopupMenuCols
//      PopupMenuCols.PopUp(X, y)
    else
    if aRow = 0 then
      StringGrid1.PopupMenu := PopupMenuRows
      //PopupMenuRows.PopUp(X, Y)
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
    edValue.Text := FStream.CellAsString[arow - 1, acol - 1];
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

procedure TFrmMain.ActionOpenExecute(Sender: TObject);
begin
  With OpenDialog1 do
    if Execute then
    begin
      FreeAndNil(FStream);
      FStream := TCsvStream.Create(Filename, @Notifyer);
      EnableActions;
    end;
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
        s := s + ';';
      AddRow(s);
    end;
  EnableActions;
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

procedure TFrmMain.ActionPasteExecute(Sender: TObject);
begin
  edValue.Clear;
  edValue.PasteFromClipboard;
  edValue.OnExit(Self);
end;

procedure TFrmMain.ActionAddColExecute(Sender: TObject);
begin
  with FStream do
    CellAsString[StringGrid1.Row, ColCounts[StringGrid1.Row]] := '';
  EnableActions;
end;

procedure TFrmMain.ActionSaveExecute(Sender: TObject);
begin
  if FStream.Modified then
  try
    Screen.Cursor := crHourglass;
    FStream.Save;
  finally
    Screen.Cursor := crDefault;
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
      mrYes:    FStream.Save;
      MrCancel:  CanClose := False;
    end;
end;

procedure TFrmMain.Notifyer(Sender: TObject; const Msg: string; State: TCsvState; nbRows : Integer);
begin
  with StatusBar1 do
  begin
    Panels[0].Text := Msg;
    Panels[1].Text := 'RowCount: ' + IntToStr(nbRows);

    case State of
      csReady :
        begin
          Panels[0].Text := 'Ready.';
          StringGrid1.Options := StringGrid1.Options + [goEditing];
          Panels[2].Text := 'ColCount: ' + IntToStr(FStream.ColCounts[0]);
          SizeGrid;
        end;

      csSaving:
        StringGrid1.Options := StringGrid1.Options - [goEditing];

      csAnalyzing :
        SizeGrid;
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
    ColCount:=FStream.MaxColCount + 1;
    RowCount := FStream.RowCount + 1;
  end;
end;

end.

