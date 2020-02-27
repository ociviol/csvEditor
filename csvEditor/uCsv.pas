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
    FCsvThreadWrite : TCsvThreadWrite;
    FNotifyer : TCsvNotyfier;
    FSeparator : String;
    FCachedRows : TCacheObjList; //TCacheObj;
    FModifs : TModif;
    FSaveOnFree,
    FInMemory,
    InFlush : Boolean;
    FFilename : string;
    FAutoSaveInc,
    FCurLine: Integer;

    function GetAutoSaveInc: Integer;
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
    procedure DoNotifyerSave;
    procedure DoNotifyerDoneSave;
    procedure SetAutoSaveInc(AValue: Integer);
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
    property Stream:TStream read FStream;
  public
    constructor Create(const Filename : String; aNotifyer : TCsvNotyfier = nil; SaveOnFree : Boolean = False);
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
    property ReadState : TCsvState read GetState;
    property Modified:Boolean read GetModified;
    property AutoSaveInc:Integer read GetAutoSaveInc write SetAutoSaveInc;
  end;

  { TCacheObjList }

  TCacheObjList = Class(TList)
  private
    function GetObj(aRow: int64):TCacheObj;
    function GetRow(aRow: int64): TRow;
    procedure SetRow(aRow: int64; const Value: TRow);
  public
    procedure Clear; override;
    procedure Add(Index : Integer; aRow : TRow);
    procedure Delete(aRow : Integer);
    property Row[aRow:int64]:TRow read GetRow write SetRow;
  end;

  TCacheObj = Class
  private
  public
    Index : Integer;
    RowData : TRow;

    constructor Create(aIndex : Integer; aDAta : TRow);
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
    FAutoSaveInc : Integer;
  public
    constructor Create(aFile : TCsvStream; AutoSaveInc : Integer = 0; aNotifyer : TCsvNotyfier = nil);
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

uses
  DateUtils, StrUtils;

{ TCacheObjList }

function TCacheObjList.GetObj(aRow: int64): TCacheObj;
var
  i : integer;
  o : TCacheObj;
begin
  for i := 0 to Count - 1 do
  begin
    o := TCacheObj(Items[i]);
    if o.Index = aRow then
    begin
      result := o;
      exit;
    end;
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

procedure TCacheObjList.Clear;
var
  i : integer;
begin
  for i := 0 to Count - 1 do
    TCacheObj(Items[i]).Free;
  inherited Clear;
end;

procedure TCacheObjList.Add(Index: Integer; aRow: TRow);
begin
  inherited Add(TCacheObj.Create(Index, aRow));
end;

procedure TCacheObjList.Delete(aRow: Integer);
var
  i : integer;
  o : TCacheObj;
begin
  for i := 0 to Count - 1 do
  begin
    o := TCacheObj(Items[i]);
    if o.Index = aRow then
    begin
       o.Free;
      inherited Delete(i);
      break;
    end;
  end;
end;


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

constructor TCsvStream.Create(const Filename : String; aNotifyer : TCsvNotyfier = nil;
                              SaveOnFree : Boolean = False);
begin
  inherited Create;

  FFilename := Filename;
  FMaxColCount := 0;
  FNotifyer := aNotifyer;
  FAutoSaveInc := 0;
  FRowCount := 0;
  FCurRow := -1;
  FPos := -1;
  InFlush := False;
  FSeparator := ''; //aSeparator;
  FCachedRows := TCacheObjList.Create;
  FSaveOnFree := SaveOnFree;
  FModifs := TModif.Create;
  FCsvThreadWrite := nil;
  FCsvThreadRead := nil;
  FStream := nil;
  Open(Filename);
end;

procedure TCsvStream.Open(const aFilename : string);
var
  st : THeapStatus;
  v : Cardinal;

  function GetFileSize : Int64;
  var
  sr : TSearchRec;
  begin
    if FindFirst(afileName, faAnyFile, sr ) = 0 then
      result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
    else
      result := -1;
    FindClose(sr);
  end;
begin
  FInMemory := False;
  st := GetHeapStatus;
  v := st.TotalAddrSpace - st.TotalAllocated;
  if FileExists(afilename) then
  begin
    if GetFileSize < (v div 10) then
    begin
      FInMemory := True;
      FStream := TMemoryStream.Create;
      TMemoryStream(FStream).LoadFromFile(aFilename);
      FStream.Position := 0;
    end
    else
      FStream := TFileStream.Create(aFilename, fmOpenRead);
    FCsvThreadRead := TCsvThreadRead.Create(Self, FNotifyer);
  end
  else
  begin
    FStream := TFileStream.Create(aFilename, fmCreate);
    if Assigned(FNotifyer) then
      FNotifyer(Self, '', csReady, 0);
  end;

  if not Assigned(FCsvThreadWrite) and (FSaveOnFree or (FAutoSaveInc > 0)) then
    FCsvThreadWrite := TCsvThreadWrite.Create(Self, FAutoSaveInc, FNotifyer);
end;

procedure TCsvStream.Close(bFreePos : Boolean = True);
begin
  if (FModifs.Count > 0) and FSaveOnFree then
    if Assigned(FCsvThreadWrite) then
    begin
      //FCsvThreadWrite.Resume;
      while FModifs.Count > 0 do
      Sleep(100);
    end;

  if Assigned(FCsvThreadWrite) then
  begin
    if (FModifs.Count > 0) and FSaveOnFree then
    begin
      //FCsvThreadWrite.Resume;
      while FModifs.Count > 0 do
        Sleep(100);
    end;

    with FCsvThreadWrite do
    begin
      //if Suspended then
      //  Resume;
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

procedure TCsvStream.SetAutoSaveInc(AValue: Integer);
begin
  FAutoSaveInc := AValue;
  if aValue > 0 then
  begin
    if not Assigned(FCsvThreadWrite) and (FSaveOnFree or (FAutoSaveInc > 0)) then
      FCsvThreadWrite := TCsvThreadWrite.Create(Self, FAutoSaveInc, FNotifyer);
  end
  else
  if Assigned(FCsvThreadWrite) then
  begin
    FCsvThreadWrite.Terminate;
    FCsvThreadWrite := nil;
  end;
end;

function TCsvStream.GetAutoSaveInc: Integer;
begin
  result := FAutoSaveInc;
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
      Stream.Position := FPositions[aRow];
      ReadLine(s);
      result := CountCols(s);
      AddCachedRow(result, aRow);
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

procedure TCsvStream.DoNotifyerDoneSave;
begin
  FNotifyer(Self, 'Ready.', csReady, FCurLine);
end;

procedure TCsvStream.DoNotifyerSave;
begin
  FNotifyer(Self, 'Writing changes ...', csSaving, FCurLine);
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
  if (Sender is TCsvThreadWrite) then
    TCsvThreadWrite(Sender).Synchronize(@DoNotifyerSave);

  try
    FModifs.Lock;
    try
      FCurLine := 0;

      if not (Sender is TCsvThreadWrite) then
        FNotifyer(Self, 'Writing changes ...', csSaving, 0);

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
            if not (Sender is TCsvThreadWrite) then
              if (i mod 50) = 0 then
                DoNotifyerSave;
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

    if (Sender is TCsvThreadWrite) then
      TCsvThreadWrite(Sender).Synchronize(@DoNotifyerDoneSave)
    else
      DoNotifyerDoneSave;
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
  for i:=0 to FCachedRows.Count - 1 do
    if TCacheObj(FCachedRows[i]).Index = aRow then
    begin
      result := TCacheObj(FCachedRows[i]);
      exit;
    end;

  result := nil;
end;

procedure TCsvStream.AddCachedRow(aRow: TRow; Index: Integer);
begin
  if FInMemory then
    Exit;

  FModifs.Lock;
  try
    if FCachedRows.Count >= 100 then
      FCachedRows.Delete(0);

    FCachedRows.Add(Index, aRow);
  finally
    FModifs.UnLock;
  end;
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

constructor TCacheObj.Create(aIndex : Integer; aDAta : TRow);
begin
  inherited Create;
  Index := aIndex;
  RowData := aData;
end;

{ TCsvThreadWrite }

constructor TCsvThreadWrite.Create(aFile: TCsvStream; AutoSaveInc : Integer = 0; aNotifyer : TCsvNotyfier = nil);
begin
  FFile := aFile;
  FreeOnTerminate := True;
  FAutoSaveInc := AutoSaveInc;
  FNotifyer := aNotifyer;
  inherited Create(False);
end;

procedure TCsvThreadWrite.Execute;
var
  LastCheck : TDateTime;
begin
  LastCheck := now;
  while not Terminated do
  begin
    if FFile.Modified and (FAutoSaveInc > 0) then
    begin
      if (MinutesBetween(now, LastCheck) >= FAutoSaveInc) then
      begin
        LastCheck := now;
        FFile.Flush(Self);
      end;
    end
    else
      Sleep(500);
    //Suspend;
  end;
end;

end.
