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
  TCsvThreadWrite = class;
  TRow = Array of string;
  TModif = class;
  TCacheObj = Class;

  { TCsvStream }

  TCsvStream = class(TObject)
  private
    FStream : TFileStream;
    FColCounts : Array of Integer;
    FPositions : array of Int64;
    FRowCount,
    FCurRow,
    FPos : Int64;
    FCsvThreadRead : TCsvThreadRead;
    FCsvThreadWrite : TCsvThreadWrite;
    Flock : TThreadList;
    FNotifyer : TCsvNotyfier;
    FSeparator : String;
    FCachedRows : TCacheObj;
    FModifs : TModif;
    FSaveOnFree,
    InFlush : Boolean;
    FFilename : string;
    FCRLF : String;

    function GetCellString(aRow: Integer; aCol: Integer): String;
    function GetCellVariant(aRow: Integer; aCol: Integer): Variant;
    function GetModified: Boolean;
    function IsInCache(aRow : Integer):TCacheObj;
    procedure AddCachedRow(aRow : TRow; Index : Integer);
    function GetCachedRow(aRow : Integer):TRow;
    function GetColCount(aRow: Integer): Integer;
    function GetRowCount: Integer;
    function GetRow(aRow: Integer): TRow;
    //function GetCell(aRow, aCol: Integer): String;
    //function GetCellString(aRow, aCol: Integer): String;
    //function GetCellVariant(aRow, aCol: Integer): Variant;
    //procedure SetCellAsString(aRow, aCol: Integer; const Value: String);
    procedure Close(bFreePos : Boolean = True);
    procedure Open(const aFilename : string);
    function GetState: TCsvState;
    procedure SetCellAsString(aRow: Integer; aCol: Integer; AValue: String);
  protected
    FMaxColCount: Integer;
    procedure ThreadTerminate(Sender : TObject);
    procedure SetRowCount(Value : Int64);
    procedure SetColCount(aRow, Value : Int64);
    procedure SetPosition(aRow, Value : Int64);
    function ReadLine(var Line: string): boolean;
    function CountCols(aLine : String):TRow;
    procedure Flush(Sender : TObject);
    property Stream:TFileStream read FStream;
  public
    constructor Create(const Filename : String; WindowsfileFormat : Boolean = True; SaveOnFree : Boolean = False; aNotifyer : TCsvNotyfier = nil);
    destructor Destroy; override;

    procedure DeleteRow(aRow : Integer);
    procedure AddRow(const aRow : Array of String); overload;
    procedure AddRow(const aLine : String); overload;
    procedure Save;

    property ColCounts[Row:Integer]:Integer read GetColCount;
    property MaxColCount:Integer read FMaxColCount;
    property RowCount:Integer read GetRowCount;
    property CellAsVariant[aRow:Integer; aCol:Integer]:Variant read GetCellVariant;
    property CellAsString[aRow:Integer; aCol:Integer]:String read GetCellString write SetCellAsString;
    property ReadState : TCsvState read GetState;
    property Modified:Boolean read GetModified;
  end;

  TCacheObj = Class
  private
    function GetRow(aRow: int64): TRow;
    procedure SetRow(aRow: int64; const Value: TRow);
    function GetObj(aIndex: Integer):TCacheObj;
  public
    Index : Integer;
    RowData : TRow;
    Prev : TCacheObj;
    Next : TCacheObj;

    constructor Create;

    function Last:TCacheObj;
    function First:TCacheObj;
    procedure Clear;
    procedure Add(aIndex : Integer; aRow : TRow);
    function DeleteRow(aIndex : Integer):TCacheObj;
    property Row[aRow:int64]:TRow read GetRow write SetRow;
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

  TCsvThreadWrite = class(TThread)
  private
    FFile : TCsvStream;
    FNotifyer : TCsvNotyfier;
  public
    constructor Create(aFile : TCsvStream; aNotifyer : TCsvNotyfier = nil);
    procedure Execute; override;
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

//uses
//  uEncrypt;

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

constructor TCsvStream.Create(const Filename : String; WindowsfileFormat : Boolean = True; SaveOnFree : Boolean = False; aNotifyer : TCsvNotyfier = nil);
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
  FCachedRows := TCacheObj.Create;
  FSaveOnFree := SaveOnFree;
  Flock := TThreadList.Create;
  FModifs := TModif.Create;
  FCsvThreadWrite := nil;
  FCsvThreadRead := nil;
  FStream := nil;
  if WindowsfileFormat then
    FCRLF := #13#10
  else
    FCRLF := #10;
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

  if FSaveOnFree then
    FCsvThreadWrite := TCsvThreadWrite.Create(Self);
end;

procedure TCsvStream.Close(bFreePos : Boolean = True);
begin
  if (FModifs.Count > 0) and FSaveOnFree then
    if Assigned(FCsvThreadWrite) then
    begin
      FCsvThreadWrite.Resume;
      while FModifs.Count > 0 do
      Sleep(100);
    end;

  if Assigned(FCsvThreadWrite) then
  begin
    if (FModifs.Count > 0) and FSaveOnFree then
    begin
      FCsvThreadWrite.Resume;
      while FModifs.Count > 0 do
        Sleep(100);
    end;

    with FCsvThreadWrite do
    begin
      if Suspended then
        Resume;
      Terminate;
    end;
  end;

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

  FLock.Free;
  FModifs.Free;

  inherited;
end;

procedure TCsvStream.SetCellAsString(aRow: Integer; aCol: Integer;
  AValue: String);
var
  r : TRow;
begin
  r := FModifs.Row[aRow];
  if length(r) = 0 then
    r := GetRow(aRow);

  // remove from cache
  if Assigned(IsInCache(aRow)) then
    FCachedRows.DeleteRow(aRow).Free;

  r[aCol] := aValue;
  FModifs.Add(r, aRow);
end;

function TCsvStream.GetCellString(aRow: Integer; aCol: Integer): String;
var
  r : TRow;
begin
  r := GetRow(aRow);
  if aCol < length(r) then
    result := r[aCol]
  else
    result := '';
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
  FLock.LockList;
  try
    if Length(FColcounts) >= aRow + 1 then
      Result := FColcounts[aRow]
    else
      Result := -1;
  finally
    FLock.UnLockList;
  end;
end;

function TCsvStream.GetRow(aRow: Integer): TRow;
var
  s : string;
begin
  FLock.LockList;
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
      Stream.Position := FPositions[aRow];
      ReadLine(s);
      AddCachedRow(CountCols(s), aRow);
      result := GetCachedRow(aRow);
    end;
  finally
    FLock.UnLockList;
  end;
end;

function TCsvStream.GetRowCount: Integer;
begin
  FLock.LockList;
  try
    Result := FRowCount;
  finally
    FLock.UnLockList;
  end;
end;

function TCsvStream.ReadLine(var Line: string): boolean;
var
  RawLine: UTF8String;
  ch: AnsiChar;
begin
  FLock.LockList;
  try
    result := False;
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
    FLock.UnLockList;
  end;
end;

procedure TCsvStream.SetColCount(aRow, Value: Int64);
begin
  FLock.LockList;
  try
    if length(FColCounts) < aRow + 1 then
      SetLength(FColCounts, aRow + 1);
    FColCounts[aRow] := Value;
  finally
    FLock.UnLockList;
  end;
end;

procedure TCsvStream.SetPosition(aRow, Value: Int64);
begin
  FLock.LockList;
  try
    if Length(FPositions) < aRow + 1 then
      SetLength(FPositions, aRow+1);
    FPositions[aRow] := Value;
  finally
    FLock.UnLockList;
  end;
end;

procedure TCsvStream.SetRowCount(Value: Int64);
begin
  FLock.LockList;
  try
    FRowCount := Value;
  finally
    FLock.UnLockList;
  end;
end;

procedure TCsvStream.ThreadTerminate(Sender: TObject);
begin
  FCsvThreadRead := nil;
end;

procedure TCsvStream.Flush(Sender : TObject);
var
  Writer: TFileStream;
  i, j : Integer;
  s : string;
  r : TRow;
begin
  InFlush := True;
  try
    FModifs.Lock;
    try
      Writer := TFileStream.Create(FFilename + '.tmp', fmCreate);
      try
        for i := 0 to RowCount - 1 do
        begin
          r := GetRow(i);
          SetPosition(i, Writer.Position);
          s := '';
          for j := Low(r) to High(r) do
            s := s + '"' + r[j] + '"' + FSeparator;
          SetLength(s, Length(s)-1);
          s := s +#13+#10;
          Writer.WriteBuffer(s[1],length(s));
          if (i mod 50) = 0 then
          begin
            if not (Sender is TCsvThreadWrite) then
              FNotifyer(Self, 'Writing changes ...', csSaving, i);
          end;
        end;
      finally
        Writer.Free;
      end;

      if  not (Sender is TCsvThreadWrite) then
        FNotifyer(Self, 'Ready.', csReady, i);
      FModifs.Clear;
      FCachedRows.Clear;
      FStream.Free;
      DeleteFile(FFilename);
      RenameFile(FFilename + '.tmp', FFilename);
      FStream := TFileStream.Create(FFilename, fmOpenRead);
    finally
      FModifs.Unlock;
    end;
  finally
    InFlush := False;
  end;
end;


procedure TCsvStream.AddRow(const aRow: array of String);
var
  r : TRow;
  i : integer;
begin
  while InFlush do
    Sleep(250);

  SetLength(r, Length(aRow));
  for i := Low(aRow) to High(aRow) do
    r[i] := aRow[i];

  FModifs.Add(r, RowCount);
  Inc(FRowCount);
  SetLength(FColCounts, Length(FColCounts)+1);
  FColCounts[Length(FColCounts)-1] := Length(aRow);
  if FModifs.Count > 500 then
    Flush(nil);
    //if Assigned(FCsvThreadWrite) then
    //  FCsvThreadWrite.Resume;
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

function TCsvStream.IsInCache(aRow: Integer): TCacheObj;
var
  v : TCacheObj;
begin
  v := FCachedRows;
  if v.Index = aRow then
  begin
    result := v;
    exit;
  end;

  while Assigned(v.Next) do
  begin
    if v.Next.Index = aRow then
    begin
      Result := v.Next;
      exit;
    end;
    v := v.Next;
  end;  
  result := nil;
end;

procedure TCsvStream.AddCachedRow(aRow: TRow; Index: Integer);
begin
  FCachedRows.Add(Index, aRow);
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
  FNotifyer(Self, 'Analysing file ...', FState, FFile.RowCount);
end;

procedure TCsvThreadRead.Execute;
var
  nb : integer;
  s : string;
begin
  while not Terminated do
  try
    nb := 0;
    FFile.SetPosition(nb, FFile.Stream.Position);
    while FFile.ReadLine(s) do
    begin
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
        Sleep(5);
      end;
    end;
    Terminate;
    FState := csReady;
    Synchronize(@DoNotyfier);
  except
    FState := csError;
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

procedure TCacheObj.Add(aIndex: Integer; aRow: TRow);
var
  p : TCacheObj;
begin
  if (Index < 0 ) then
  begin
    Index := aIndex;
    RowData := aRow;
  end
  else
  begin
    p := Last;
    p.Next := TCacheObj.Create;
    p.Next.Prev := p;
    p := p.Next;
    p.Index := aIndex;
    p.RowData := aRow;
    p.Next := nil;
  end;
end;

constructor TCacheObj.Create;
begin
  Index := -1;
  Prev := nil;
  Next := nil;
end;

function TCacheObj.GetObj(aIndex: Integer):TCacheObj;
var
  v : TCacheObj;
begin
  v := First;
  while Assigned(v) do
  begin
    if v.Index = aIndex then
    begin
      result := v;
      exit;
    end;
    v := v.Next;
  end;
  result := nil;
end;

function TCacheObj.DeleteRow(aIndex: Integer):TCacheObj;
begin
  Result := GetObj(aIndex);
  if Assigned(Result.Prev) then
    Result.Prev.Next := Result.Next;
  if Assigned(Result.Next) then
    Result.Next.Prev := Result.Prev;
end;

function TCacheObj.First: TCacheObj;
begin
  Result := Self;
  while Assigned(Result.Prev) do
    Result := Result.Prev;
end;

function TCacheObj.GetRow(aRow: int64): TRow;
var
  v : TCacheObj;
begin
  v := GetObj(aRow);
  if Assigned(v) then
  begin
    result := v.RowData;
    exit;
  end;
end;

function TCacheObj.Last: TCacheObj;
begin
  Result := Self;
  while Assigned(Result.Next) do
    Result := Result.Next;
end;

procedure TCacheObj.SetRow(aRow: int64; const Value: TRow);
var
  v : TCacheObj;
begin
  v := GetObj(aRow);
  if Assigned(v) then
    v.RowData := Value;
end;

procedure TCacheObj.Clear;
var
  v : TCacheObj;
begin
  v := Last;
  while Assigned(v.Prev) do
  begin
    v := v.Prev;
    FreeAndNil(v.Next);
  end;
  v.Index := -1;
  SetLength(RowData, 0);
end;

{ TCsvThreadWrite }

constructor TCsvThreadWrite.Create(aFile: TCsvStream; aNotifyer : TCsvNotyfier = nil);
begin
  FFile := aFile;
  FreeOnTerminate := True;
  FNotifyer := aNotifyer;
  inherited Create(True);
end;

procedure TCsvThreadWrite.Execute;
begin
  while not Terminated do
  begin
    FFile.Flush(Self);
    Suspend;
  end;
end;

end.
