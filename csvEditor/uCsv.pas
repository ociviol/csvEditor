unit uCsv;

{$mode objfpc}{$H+}
//{$mode delphi}

interface

uses
  Classes,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  SysUtils;

type
  TCsvState = (csReady, csAnalyzing, csError, csSaving);
  TCsvNotyfier = procedure(Sender : TObject; const Msg : string; State : TCsvState; nbRows : Integer) of object;
  TCsvThreadRead = class;
  TRow = Array of string;
  TModif = class;
  TCacheObjList = Class;
  TCacheObj = Class;

  { TCsvStream }

  TCsvStream = class(TObject)
  private
    FStream : TStream;
    FColCounts : Array of Integer;
    FPositions : array of Int64;
    FRowCount,
    FCurRow,
    FPos : Int64;
    FCsvThreadRead : TCsvThreadRead;
    FNotifyer : TCsvNotyfier;
    FSeparator : String;
    FCachedRows : TCacheObjList; //TCacheObj;
    FModifs : TModif;
    InFlush : Boolean;
    FFilename : string;
    FCurLine: Integer;

    function GetCacheSz: Integer;
    function GetCellAsStringNoEval(aRow: Integer; aCol: Integer): String;
    function GetCellString(aRow: Integer; aCol: Integer): String;
    function GetCellVariant(aRow: Integer; aCol: Integer): Variant;
    function GetModified: Boolean;
    function GetModifsSz: Integer;
    function IsInCache(aRow : Integer):TCacheObj;
    function GetCachedRow(aRow : Integer):TRow;
    function GetColCount(aRow: Integer): Integer;
    function GetRowCount: Integer;
    function GetRow(aRow: Integer): TRow;
    procedure Close(bFreePos : Boolean = True);
    procedure Open(const aFilename : string);
    function GetState: TCsvState;
    procedure SetCellAsString(aRow: Integer; aCol: Integer; AValue: String);
    function ExecFormula(Formula : String):Variant;
  protected
    FMaxColCount: Integer;
    procedure ThreadTerminate(Sender : TObject);
    procedure SetRowCount(Value : Int64);
    procedure SetColCount(aRow, Value : Int64);
    procedure SetPosition(aRow, Value : Int64);
    function ReadLine(var Line: string): boolean;
    function CountCols(aLine : String):TRow;
    procedure Flush(Sender : TObject);
    property Stream:TStream read FStream;
  public
    constructor Create(const Filename : String; aNotifyer : TCsvNotyfier = nil);
    destructor Destroy; override;

    procedure DeleteRow(aRow : Integer);
    procedure AddRow(const aRow : Array of String); overload;
    procedure AddRow(const aLine : String); overload;
    procedure Save;
    procedure Cancel;

    property ColCounts[Row:Integer]:Integer read GetColCount;
    property MaxColCount:Integer read FMaxColCount;
    property RowCount:Integer read GetRowCount;
    property CellAsVariant[aRow:Integer; aCol:Integer]:Variant read GetCellVariant;
    property CellAsString[aRow:Integer; aCol:Integer]:String read GetCellString write SetCellAsString;
    property CellAsStringNoEval[aRow:Integer; aCol:Integer]:String read GetCellAsStringNoEval;
    property ReadState : TCsvState read GetState;
    property Modified:Boolean read GetModified;
    property CacheSz:Integer read GetCacheSz;
    property ModifsSz :Integer read GetModifsSz;
  end;

  { TCacheObjList }

  TCacheObjList = Class(TThreadList)
  private
    function GetCount: Integer;
    function GetObj(aRow: int64):TCacheObj;
    function GetRow(aRow: int64): TRow;
    procedure SetRow(aRow: int64; const Value: TRow);
  public
    destructor Destroy; override;
    procedure Clear;
    procedure Add(Index : Integer; const aRow : TRow);
    procedure Delete(aRow : Integer);
    property Row[aRow:int64]:TRow read GetRow write SetRow;
    property Count : Integer read GetCount;
  end;

  TCacheObj = Class
  private
  public
    Index : Integer;
    RowData : TRow;

    constructor Create(aIndex : Integer; const aDAta : TRow);
  end;

  TCsvThreadRead = class(TThread)
  private
    FFile : TCsvStream;
    FState : TCsvState;
    FNotifyer : TCsvNotyfier;
    procedure DoNotyfier;
  public
    constructor Create(aFile : TCsvStream; aNotifyer : TCsvNotyfier = nil);
    procedure Execute; override;
    property State : TCsvState read FState; 
  end;

  TModifRec = class
  private
    FRow : TRow;
    FIndex : Int64;
  public
    constructor Create(const aRow : TRow; aIndex : Integer);
    property Row : TRow read FRow write FRow;
    property Index : Int64 read FIndex;
  end;

  TModif = class
  private
    FList : TThreadList;
    function GetRow(aRow: int64): TRow;
    function Exists(aRow : Integer):Integer;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(aRow: TRow; aIndex: Int64);
    procedure Clear;
    procedure Lock;
    procedure Unlock;

    property Row[aRow:int64]:TRow read GetRow;
    property Count:Integer read GetCount;
  end;

implementation

uses
  DateUtils, StrUtils;


{ TCsvStream }

function TCsvStream.CountCols(aLine: String): TRow;
  function CountChar(const S: string; const C: char): integer;
  var
    i: Integer;
  begin
    result := 0;
    for i := 1 to Length(S) do
      if S[i] = C then
        inc(result);
  end;

  procedure DetermineSeparator;
  const
    ch : Array[0..2] of Char = (',',';',#9);
  var
    nb : Array of Integer;
    i, v : integer;
  begin
    SetLength(nb, Length(ch));
    for i := Low(nb) to High(nb) do
      nb[i] := CountChar(aLine, ch[i]);

    v := 0;
    for i := Low(nb) to High(nb) do
      if nb[i] > nb[v] then
        v := i;

    FSeparator := ch[v];
  end;

  function ProcessQuote:string;
  var
    inQuote : Boolean;
    ch : char;
  begin
    result := '';
    inQuote := False;
    repeat
      ch := aLine[1];
      if ch = '"' then
        inQuote := not inQuote;
      if (ch <> FSeparator) or (InQuote) then
        Result := Result + ch;
      aLine := Copy(aLine, 2, Length(aLine));
    until (aLine = '') or ((not InQuote) and (ch = FSeparator));

    result := Copy(Result, 2, Length(Result)-2);
  end;
begin
  SetLength(Result, 0);

  if FSeparator = '' then
    DetermineSeparator;

  while Pos(FSeparator, aLine) > 0 do
  begin
    SetLength(Result, Length(Result)+1);
    if aLine[1] = '"' then
      Result[Length(Result)-1] := ProcessQuote
    else
    begin
      Result[Length(Result)-1] := Copy(aLine, 1, Pos(FSeparator, aLine)-1);
      aLine := Copy(aLine, Pos(FSeparator, aLine) + 1, Length(aLine));
    end;
  end;

  if Length(aLine) > 0 then
  begin
    SetLength(Result, Length(Result)+1);
    if aLine[1] = '"' then
      Result[Length(Result)-1] := ProcessQuote
    else
      Result[Length(Result)-1] := aLine;
  end;
end;

constructor TCsvStream.Create(const Filename : String; aNotifyer : TCsvNotyfier = nil);
begin
  inherited Create;

  FFilename := Filename;
  FMaxColCount := 0;
  FNotifyer := aNotifyer;
  FRowCount := 0;
  FCurRow := -1;
  FPos := -1;
  InFlush := False;
  FSeparator := ''; //aSeparator;
  FCachedRows := TCacheObjList.Create;
  FModifs := TModif.Create;
  FCsvThreadRead := nil;
  FStream := nil;
  Open(Filename);
end;

procedure TCsvStream.Open(const aFilename : string);
begin
  if FileExists(afilename) then
  begin
    FStream := TFileStream.Create(aFilename, fmOpenRead);
    FCsvThreadRead := TCsvThreadRead.Create(Self, FNotifyer);
  end
  else
  begin
    FStream := TFileStream.Create(aFilename, fmCreate);
    if Assigned(FNotifyer) then
      FNotifyer(Self, '', csReady, 0);
  end;
end;

procedure TCsvStream.Close(bFreePos : Boolean = True);
begin
  if bFreePos then
    SetLength(FPositions, 0);

  FCachedRows.Clear;
  FreeAndNil(FStream);
end;

destructor TCsvStream.Destroy;
begin
  if Assigned(FCsvThreadRead) then
  try
    FCsvThreadRead.Terminate;
  except
  end;

  Close;
  FCachedRows.Clear;
  FCachedRows.Free;
  FModifs.Free;

  inherited;
end;

procedure TCsvStream.SetCellAsString(aRow: Integer; aCol: Integer;
  AValue: String);
var
  r : TRow;
begin
  FModifs.Lock;
  try
    r := FModifs.Row[aRow];
    if length(r) = 0 then
      r := GetRow(aRow);

    // remove from cache
    if Assigned(IsInCache(aRow)) then
      FCachedRows.Delete(aRow);

    r[aCol] := aValue;
    FModifs.Add(r, aRow);
  finally
    FModifs.UnLock;
  end;
end;

function TCsvStream.ExecFormula(Formula: String): Variant;
const
  CS_LETTERS = ['A'..'Z'];
  CS_NUMBERS = ['0'..'9'];

  function ColumnLetterToColumnIndex(const cell : String):Integer;
  var
    i : integer;
  begin
    Result := 0;

    for i := 1 to Length(cell) do
    begin
      Result := Result * 26;
      Inc(Result, ord(cell[i]) - ord('A'));
    end;
end;

  function GetChar(var Line : String):String; inline;
  begin
    result := Line[1];
    Line := Copy(Line, 2, Length(Line));
  end;

  function GetCellCol(Cell : String):Integer;
  var
    s : string;
  begin
    result := 0;
    s := '';
    while length(Cell) > 0 do
    begin
      if (Cell[1] in CS_LETTERS) then
        s := s + GetChar(Cell)
      else
        break;
    end;
    result := ColumnLetterToColumnIndex(s);
  end;

  function GetCellRow(Cell : String):Integer;
  var
    s : String;
  begin
    result := 0;
    s := '';
    while length(Cell) > 0 do
    begin
      if (Cell[1] in CS_NUMBERS) then
        s := s + GetChar(Cell)
      else
        Cell := Copy(Cell, 2, Length(Cell));
    end;
    result := StrToInt(s)-1;
  end;

  function GetCell:String;
  begin
    result := '';
    while length(Formula) > 0 do
    begin
      if (Formula[1] in CS_LETTERS) or (Formula[1] in CS_NUMBERS) then
        result := result + GetChar(Formula)
      else
        break;
    end;
  end;

  function Eval(aCell : String):Variant;
  var
    row, col : string;

    function ResolveCol:Integer;
    var
      i : integer;
    begin
      result := 0;
      while length(col) > 0 do
      begin
        i := ord(col[length(col)]) - 26;
        result := result + (ord(col[length(col)]) - 26);
      end;
    end;

  begin
    try
      {
    // get col
    col := '';
    while (length(Formula) > 0) and (Formula[1] in CS_LETTERS) do
      col := col + GetChar;
    // get row
    row := '';
    while (length(Formula) > 0) and (Formula[1] in CS_NUMBERS) do
      row := row + GetChar;
      }

    except
      result := '';
    end;
  end;

  function MakePoint:TPoint;
  var
    c : String;
  begin
    c := GetCell;
    Result.x := GetCellCol(c);
    Result.y := GetCellRow(c);
  end;

  function doSum(pDeb, pEnd : TPoint):Variant;
  var
    x,y : integer;
  begin
    result := 0;
    for y := pDeb.y to pEnd.y do
      for x := pDeb.x to pEnd.x do
        result := result + CellAsVariant[y, x];
  end;

  function GetStr(var Val : String; len : integer):String;
  begin
    result :=  Copy(Val, 1, len);
    Val := Copy(Val, len, Length(Val));
  end;

var
  c1, c2, s : String;
  p1, p2 : TPoint;
begin
  s := '';
  while length(Formula) > 0 do
  try
    if Copy(Formula, 1, 4) = 'SUM(' then
    begin
      s := 'SUM';
      Formula := Copy(Formula, 4, Length(Formula));
    end;

    case Formula[1] of
      '(':
        begin
          GetChar(Formula);
          if s = 'SUM' then
          begin
            // Get range
            p1 := MakePoint;
            if Formula[1] <> ':' then
              raise Exception.Create('Error in range, received: ''' + Formula[1] + ''' expected '':''');
            GetChar(Formula);
            p2 := MakePoint;
            result := doSum(p1, p2);
          end;
        end;

      'A'..'Z':
        begin
          c1 :=  GetCell;
          p1.x := GetCellCol(c1);
          p1.y := GetCellRow(c1);
        end;

      ':' :
        begin
          GetChar(Formula);
          c2 :=  GetCell;
          p2.x := GetCellCol(c1);
          p2.y := GetCellRow(c1);
        end;

    else
      s := s + GetChar(Formula);
    end;

  except
    result := '#ERR';
  end;

end;

function TCsvStream.GetCellString(aRow: Integer; aCol: Integer): String;
begin
  result := GetCellAsStringNoEval(aRow, aCol);
  //if result[1] = '=' then
  //  result := ExecFormula(copy(result, 2, length(result)))
end;

function TCsvStream.GetCacheSz: Integer;
begin
  result := FCachedRows.Count;
end;

function TCsvStream.GetCellAsStringNoEval(aRow: Integer; aCol: Integer): String;
var
  r : TRow;
begin
  r := GetRow(aRow);
  if aCol < length(r) then
    result := r[aCol]
  else
    result := '';
  SetLength(r, 0);
end;

function TCsvStream.GetModifsSz: Integer;
begin
  result := FModifs.Count;
end;

function TCsvStream.GetCellVariant(aRow: Integer; aCol: Integer): Variant;
var
  s : string;
  r : Extended;
  t : TDateTime;
  i : Integer;
begin
  if aCol >= Colcounts[aRow] then
    exit;

  s := GetCellString(aRow, aCol);
  if TryStrToInt(s, i) then
    result := i
  else
  if TryStrToFloat(s, r) then
    result := r
  else
  if TryStrToDate(s, t) then
    result := t
  else
    result := s;
end;

function TCsvStream.GetModified: Boolean;
begin
  result := FModifs.Count > 0;
end;

function TCsvStream.GetColCount(aRow: Integer): Integer;
begin
  FModifs.Lock;
  try
    if Length(FColcounts) >= aRow + 1 then
      Result := FColcounts[aRow]
    else
      Result := -1;
  finally
    FModifs.UnLock;
  end;
end;

function TCsvStream.GetRow(aRow: Integer): TRow;
var
  s : string;
  p : int64;
begin
  FModifs.Lock;
  try
    SetLength(Result, 0);

    if FModifs.Exists(aRow) >= 0 then
      Result := FModifs.Row[aRow]
    else
    if Assigned(IsInCache(aRow)) then
      Result := GetCachedRow(aRow)
    else
    if Length(FPositions) >= aRow + 1 then
    begin
      p := Stream.Position;
      Stream.Position := FPositions[aRow];
      ReadLine(s);
      Stream.Position := p;
      result := CountCols(s);
      FCachedRows.Add(aRow, result);
    end;
  finally
    FModifs.UnLock;
  end;
end;

function TCsvStream.GetRowCount: Integer;
begin
  FModifs.Lock;
  try
    Result := FRowCount;
  finally
    FModifs.UnLock;
  end;
end;

function TCsvStream.ReadLine(var Line: string): boolean;
var
  RawLine: UTF8String;
  ch: AnsiChar;
begin
  FModifs.Lock;
  try
    result := False;
    RawLine := '';
    ch := #0;
    while (Stream.Read( ch, 1) = 1) and (ch <> #13) do
    begin
      result := True;
      RawLine := RawLine + ch
    end;

    Line := RawLine;
    if ch = #13 then
    begin
      result := True;
      if (Stream.Read( ch, 1) = 1) and (ch <> #10) then
        Stream.Seek(-1, soCurrent) // unread it if not LF character.
    end
  finally
    FModifs.UnLock;
  end;
end;

procedure TCsvStream.SetColCount(aRow, Value: Int64);
begin
  FModifs.Lock;
  try
    if length(FColCounts) < aRow + 1 then
      SetLength(FColCounts, aRow + 1);
    FColCounts[aRow] := Value;
  finally
    FModifs.UnLock;
  end;
end;

procedure TCsvStream.SetPosition(aRow, Value: Int64);
begin
  FModifs.Lock;
  try
    if Length(FPositions) < aRow + 1 then
      SetLength(FPositions, aRow+1);
    FPositions[aRow] := Value;
  finally
    FModifs.UnLock;
  end;
end;

procedure TCsvStream.SetRowCount(Value: Int64);
begin
  FModifs.Lock;
  try
    FRowCount := Value;
  finally
    FModifs.UnLock;
  end;
end;

procedure TCsvStream.ThreadTerminate(Sender: TObject);
begin
  FCsvThreadRead := nil;
end;

procedure TCsvStream.Flush(Sender : TObject);
var
  Writer: TFileStream;
  Tm : TMemoryStream;
  i, j : Integer;
  s : string;
  r : TRow;
begin
  if InFlush then
    Exit;

  InFlush := True;

  try
    FModifs.Lock;
    try
      FCurLine := 0;
      FNotifyer(Self, 'Writing changes ...', csSaving, FCurLine);

      Writer := TFileStream.Create(FFilename + '.tmp', fmCreate);
      try
        Tm := TMemoryStream.Create;
        try
          for i := 0 to RowCount - 1 do
          begin
            r := GetRow(i);
            SetPosition(i, Writer.Position + tm.Position);
            s := '';
            for j := Low(r) to High(r) do
              s := s + '"' + r[j] + '"' + FSeparator;
            SetLength(s, Length(s)-1);
            s := s +#13+#10;
            Tm.WriteBuffer(s[1],length(s));
            FCurLine := i;
            if (i mod 50) = 0 then
              if Assigned(FNotifyer) then
                FNotifyer(Self, 'Writing changes ...', csSaving, FCurLine);
            // flush
            if Tm.Size > 1024 * 1024 then
            begin
              Tm.Position := 0;
              Writer.CopyFrom(Tm, Tm.Size);
              Tm.Clear;
            end;
          end;

          if Tm.Size > 0 then
          begin
            Tm.Position := 0;
            Writer.CopyFrom(Tm, Tm.Size);
            Tm.Clear;
          end;
        finally
          tm.Free;
        end;
      finally
        Writer.Free;
      end;

      FModifs.Clear;
      FCachedRows.Clear;
      FreeAndNil(FStream);
      DeleteFile(FFilename);
      RenameFile(FFilename + '.tmp', FFilename);
      Open(FFilename);
    finally
      FModifs.Unlock;
    end;

    if Assigned(FNotifyer) then
      FNotifyer(Self, 'Ready.', csReady, FCurLine);
  finally
    InFlush := False;
  end;
end;


procedure TCsvStream.AddRow(const aRow: array of String);
var
  r : TRow;
  i : integer;
begin
  FModifs.Lock;
  try
    SetLength(r, Length(aRow));
    for i := Low(aRow) to High(aRow) do
      r[i] := aRow[i];

    FModifs.Add(r, RowCount);
    Inc(FRowCount);
    SetLength(FColCounts, Length(FColCounts)+1);
    FColCounts[Length(FColCounts)-1] := Length(aRow);
    if (FModifs.Count > 500) then
      Flush(nil);
  finally
    FModifs.UnLock;
  end;
end;

procedure TCsvStream.AddRow(const aLine: String);
begin
  AddRow(CountCols(aLine));
end;

procedure TCsvStream.Save;
begin
  if Modified then
    Flush(nil);
end;

procedure TCsvStream.Cancel;
begin
  if Assigned(FCsvThreadRead) then
  begin
    FCsvThreadRead.Terminate;
    Sleep(1000);
  end;
end;

function TCsvStream.IsInCache(aRow: Integer): TCacheObj;
var
  i : Integer;
begin
  with FCachedRows.LockList do
  try
    for i:=0 to Count - 1 do
      if TCacheObj(Items[i]).Index = aRow then
      begin
        result := TCacheObj(Items[i]);
        exit;
      end;
  finally
    FCachedRows.UnlockList;
  end;

  result := nil;
end;

function TCsvStream.GetCachedRow(aRow: Integer): TRow;
begin
  Result := FCachedRows.Row[aRow];
end;

procedure TCsvStream.DeleteRow(aRow: Integer);
var
  i : integer;
begin
  if FModifs.Count > 0 then
    Flush(nil);

  if FRowCount = 0 then
    Exit;
    
  for i := 0 to FRowCount - 2 do
    FPositions[aRow + i] := FPositions[aRow + i + 1];
  FCachedRows.Clear;
  Dec(FRowCount);
  Flush(nil);
end;

function TCsvStream.GetState: TCsvState;
begin
  if Assigned(FCsvThreadRead) then
    result := FCsvThreadRead.State
  else
    result := csReady;
end;

{ TCsvThread }

constructor TCsvThreadRead.Create(aFile : TCsvStream; aNotifyer : TCsvNotyfier = nil);
begin
  FFile := aFile;
  FNotifyer := aNotifyer;
  FreeOnTerminate := True;
  OnTerminate := @FFile.ThreadTerminate;
  FState := csAnalyzing;
  inherited Create(False);
end;

procedure TCsvThreadRead.DoNotyfier;
begin
  FNotifyer(Self, ifthen(FState = csAnalyzing, 'Analyzing file ...', 'Ready.'), FState, FFile.RowCount);
end;

procedure TCsvThreadRead.Execute;
var
  nb : integer;
  s : string;
begin
  while not Terminated do
  try
    nb := 0;
    Synchronize(@DoNotyfier);
    FFile.SetPosition(nb, FFile.Stream.Position);
    while FFile.ReadLine(s) do
    begin
      if Terminated then
        Exit;

      if Length(s) > 0 then
      begin
        FFile.SetColCount(nb, Length(FFile.CountCols(s)));
        if FFile.FMaxColCount < FFile.FColCounts[nb] then
          FFile.FMaxColCount := FFile.FColCounts[nb];
        Inc(nb);
        FFile.SetPosition(nb, FFile.Stream.Position);
        FFile.SetRowCount(nb);
        FFile.SetPosition(nb, FFile.Stream.Position);
      end;
      if (nb mod 50) = 0 then
      begin
        Synchronize(@DoNotyfier);
        Sleep(50);
      end;
      Sleep(5);
    end;
    Terminate;
    FState := csReady;
    Synchronize(@DoNotyfier);
  except
    FState := csError;
  end;
end;


{ TCacheObjList }

function TCacheObjList.GetCount: Integer;
begin
  with LockList do
  try
    result := Count;
  finally
    UnlockList;
  end;
end;

function TCacheObjList.GetObj(aRow: int64): TCacheObj;
var
  i : integer;
  o : TCacheObj;
begin
  with LockList do
  try
    for i := 0 to Count - 1 do
    begin
      o := TCacheObj(Items[i]);
      if o.Index = aRow then
      begin
        result := o;
        exit;
      end;
    end;
  finally
    UnlockList;
  end;

  result := nil;
end;

function TCacheObjList.GetRow(aRow: int64): TRow;
var
  o : TCacheObj;
begin
  SetLength(Result, 0);
  o := GetObj(aRow);
  if Assigned(o) then
    result := o.RowData;
end;

procedure TCacheObjList.SetRow(aRow: int64; const Value: TRow);
var
  o : TCacheObj;
begin
  o := GetObj(aRow);
  if Assigned(o) then
    o.RowData := Value;
end;

destructor TCacheObjList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCacheObjList.Clear;
var
  i : integer;
begin
  with LockList do
  try
    for i := 0 to Count - 1 do
      TCacheObj(Items[i]).Free;
    Clear;
  finally
    UnlockList;
  end;
end;

procedure TCacheObjList.Add(Index: Integer; const aRow: TRow);
begin
  with LockList do
  try
     if Count >= 500 then
     begin
       TCacheObj(Items[0]).Free;
       Delete(0);
     end;
     inherited Add(TCacheObj.Create(Index, aRow));
   finally
    UnlockList;
  end;
end;

procedure TCacheObjList.Delete(aRow: Integer);
var
  i : integer;
  o : TCacheObj;
begin
  with LockList do
  try
    for i := 0 to Count - 1 do
    begin
      o := TCacheObj(Items[i]);
      if o.Index = aRow then
      begin
        o.Free;
        Delete(i);
        break;
      end;
    end;
  finally
    UnlockList;
  end;
end;


{ TModif }

constructor TModif.Create;
begin
  FList := TThreadList.Create;
  inherited Create;
end;

destructor TModif.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TModif.Add(aRow: TRow; aIndex: Int64);
var
  i : Integer;
begin
  with FList.LockList do
  try
    i := Exists(aIndex);
    if i >= 0 then
      TModifRec(Items[i]).Row := aRow
    else
      Add(TModifRec.Create(aRow, aIndex));
  finally
    FList.UnLockList;
  end;
end;

procedure TModif.Clear;
var
  i : integer;
begin
  with FList.LockList do
  try
    for i := 0 to Count - 1 do
      TModifRec(Items[i]).Free;
    Clear;
  finally
    FList.UnLockList;
  end;  
end;

function TModif.Exists(aRow: Integer): Integer;
var
  i : integer;
begin
  result := - 1;
  with FList.LockList do
  try
    for i:=0 to Count - 1 do
      if TModifRec(Items[i]).Index = aRow then
        Result := i;
  finally
    FList.UnLockList;
  end;
end;

function TModif.GetCount: Integer;
begin
  with FList.LockList do
  try
    result := Count;
  finally
    FList.UnLockList;
  end;
end;

function TModif.GetRow(aRow: int64): TRow;
var
  i : integer;
begin
  SetLength(Result, 0);
  with FList.LockList do
  try
    i := Exists(aRow);
    if i>= 0 then
      Result := TModifRec(Items[i]).Row;
  finally
    FList.UnLockList;
  end;
end;

procedure TModif.Lock;
begin
  FList.LockList
end;

procedure TModif.Unlock;
begin
  FList.UnLockList;
end;

{ TModifRec }

constructor TModifRec.Create(const aRow: TRow; aIndex: Integer);
begin
  FIndex := aIndex;
  FRow := aRow;
end;

{ TCacheObj }

constructor TCacheObj.Create(aIndex : Integer; const aDAta : TRow);
begin
  inherited Create;
  Index := aIndex;
  RowData := aData;
end;

end.
