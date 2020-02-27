unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  Buttons, ComCtrls, ActnList, Menus, StdCtrls, uCsv, Types;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    ActionAutoSave: TAction;
    ActionAddCol: TAction;
    ActionAddRow: TAction;
    ActionSave: TAction;
    ActionOpen: TAction;
    ActionList1: TActionList;
    cbAutoSave: TCheckBox;
    edValue: TEdit;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tbAutoSave: TTrackBar;
    procedure ActionAddColExecute(Sender: TObject);
    procedure ActionAddRowExecute(Sender: TObject);
    procedure ActionAutoSaveExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure edValueExit(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1GetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
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

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  FStream := nil;
  EnableActions;
  StatusBar1.Panels[0].Width := StatusBar1.Width - 240;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FStream) then
    FStream.Free;
end;

procedure TFrmMain.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  x, y : integer;
  s : string;
begin
  with StringGrid1 do
  begin
    //if ColCount < aCol + 2 then
    //  ColCount := aCol + 2;

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
          s := Chr(65 + (aCol - 1));

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

procedure TFrmMain.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  CanSelect := (aCol > 0) and (aRow > 0);
  if Assigned(FStream) and CanSelect then
    edValue.Text := FStream.CellAsString[arow - 1, acol - 1];
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
      FStream.AutoSaveInc := tbAutoSave.Position;
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

procedure TFrmMain.ActionAutoSaveExecute(Sender: TObject);
begin
  FStream.AutoSaveInc := tbAutoSave.Position;
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
    if CellAsString[Row, Col] <> edValue.Text then
      CellAsString[Row, Col] := edValue.Text;
  EnableActions;
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;
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

    case State of
      csReady :
        begin
          Panels[0].Text := 'Ready.';
          StringGrid1.Options := StringGrid1.Options + [goEditing];
          Panels[2].Text := 'ColCount: ' + IntToStr(FStream.ColCounts[0]);
          SizeGrid;
          StringGrid1.AutoSizeColumn(0);
        end;

      csSaving,
      csAnalyzing :
        begin
          StringGrid1.Options := StringGrid1.Options - [goEditing];
        end;
    end;

    Panels[1].Text := 'RowCount: ' + IntToStr(nbRows);
    Refresh;
    if State = csAnalyzing then
      SizeGrid;
  end;
end;

procedure TFrmMain.EnableActions;
begin
  ActionAddCol.Enabled := Assigned(FStream);
  ActionAddRow.Enabled := Assigned(FStream);
  ActionOpen.Enabled := True;
  ActionSave.Enabled := Assigned(FStream) and FStream.Modified;
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

