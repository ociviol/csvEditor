unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  Buttons, ComCtrls, ActnList, Menus, uCsv, Types;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    ActionAddCol: TAction;
    ActionAddRow: TAction;
    ActionSave: TAction;
    ActionOpen: TAction;
    ActionList1: TActionList;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    StringGrid1: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure ActionAddColExecute(Sender: TObject);
    procedure ActionAddRowExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FStream : TCsvStream;
    procedure Notifyer(Sender: TObject; const Msg: string; State: TCsvState);
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
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FStream) then
    FStream.Free;
end;

procedure TFrmMain.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  i : integer;
begin
  with StringGrid1 do
  begin
    if ColCount < aCol + 1 then
      ColCount := aCol + 1;
    with Canvas do
    begin
      if gdSelected in aState then
        Brush.Color:=clLime
      else
        Brush.Color:=clwhite;

      FillRect(aRect);

      if gdFocused in aState then
        DrawFocusRect(aRect);

      if Assigned (FStream) then
      begin
        i := ((aRect.Bottom - aRect.Top) - TextHeight(FStream.CellAsString[aRow, aCol])) div 2;
        TextOut(aRect.Left, aRect.Top + i, FStream.CellAsString[aRow, aCol]);
      end;
    end;
  end;
end;

procedure TFrmMain.StringGrid1EditingDone(Sender: TObject);
begin
  //FStream.CellAsString[aRow, aCol];
end;

procedure TFrmMain.StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  Value := FStream.CellAsString[aRow, aCol];
end;

procedure TFrmMain.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  with FStream do
    if CellAsString[aRow, aCol] <> Value then
      CellAsString[aRow, aCol] := Value;
end;

procedure TFrmMain.ActionOpenExecute(Sender: TObject);
begin
  With OpenDialog1 do
    if Execute then
    begin
      FreeAndNil(FStream);
      FStream := TCsvStream.Create(Filename, True, @Notifyer);
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
end;

procedure TFrmMain.ActionAddColExecute(Sender: TObject);
begin
  with FStream do
    CellAsString[StringGrid1.Row, ColCounts[StringGrid1.Row]] := '';
end;

procedure TFrmMain.ActionSaveExecute(Sender: TObject);
begin

end;

procedure TFrmMain.Notifyer(Sender: TObject; const Msg: string; State: TCsvState);
begin
  case State of
    csReady :
      begin
        StatusBar1.Panels[0].Text := 'Ready.';
        SizeGrid;
      end;
    csAnalyzing :
      begin
        StatusBar1.Panels[0].Text := 'Analyzing File ...';
      end;
  end;
  with StatusBar1, FStream do
  begin
    Panels[1].Text := 'RowCount: ' + IntToStr(RowCount);
    Panels[2].Text := 'ColCount: ' + IntToStr(ColCounts[0]);
    Refresh;
  end;
end;

procedure TFrmMain.EnableActions;
begin
  ActionAddCol.Enabled := Assigned(FStream);
  ActionAddRow.Enabled := Assigned(FStream);
  ActionOpen.Enabled := True;
  ActionSave.Enabled := Assigned(FStream);
end;

procedure TFrmMain.SizeGrid;
begin
  with StringGrid1 do
  begin
    ColCount:=FStream.MaxColCount;
    RowCount := FStream.RowCount;
    //Invalidate;
  end;
end;

end.

