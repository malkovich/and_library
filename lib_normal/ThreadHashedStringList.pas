unit ThreadHashedStringList;

interface
uses windows,classes,sysUtils, IniFiles;

Type
  TThreadHashedSL = class (TObject)
  private
    FList: THashedStringList;
    FLock: TRTLCriticalSection;
    procedure Put(Index: Integer; const S: string);
    function Get(Index: Integer): string;
    function GetObject(Index: Integer): Pointer;
    procedure PutObject(Index: Integer; const aItem: Pointer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Item: String);
    procedure AddObject(Key: String; aItem: Pointer);
    procedure Clear;
    function Count: Integer;
    function  LockList: TStringList;
    function Find(Item: String): Integer;
    procedure Delete(Index: Integer);
    procedure UnlockList;
    property Strings[Index: Integer]: string read Get write Put;
    property Objects[Index: Integer]: Pointer read GetObject write PutObject;
  end;

implementation

constructor TThreadHashedSL.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := THashedStringList.Create;
end;

destructor TThreadHashedSL.Destroy;
begin
  LockList;    // Make sure nobody else is inside the list.
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DeleteCriticalSection(FLock);
  end;
end;

procedure TThreadHashedSL.Add(Item: String);
begin
  LockList;
  try
      FList.Add(Item);
  finally
    UnlockList;
  end;
end;

function TThreadHashedSL.Count: Integer;
begin
  LockList;
  try
    Result :=  FList.Count;
  finally
    UnlockList;
  end;
end;

procedure TThreadHashedSL.AddObject(Key: String; aItem: Pointer);
begin
  LockList;
  try
      FList.AddObject(Key, aItem);
  finally
    UnlockList;
  end;
end;

procedure TThreadHashedSL.Put(Index: Integer; const S: string);
begin
  LockList;
  try
    FList[Index] := S;
  finally
    UnlockList;
  end;
end;

function TThreadHashedSL.Get(Index: Integer): string;
begin
  LockList;
  try
    Result := FList[Index];
  finally
    UnlockList;
  end;
end;

procedure TThreadHashedSL.PutObject(Index: Integer; const aItem: Pointer);
begin
  LockList;
  try
    FList.Objects[Index] := aItem;
  finally
    UnlockList;
  end;
end;

function TThreadHashedSL.GetObject(Index: Integer): Pointer;
begin
  LockList;
  try
    Result := FList.Objects[Index];
  finally
    UnlockList;
  end;
end;

procedure TThreadHashedSL.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function  TThreadHashedSL.LockList: TStringList;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

function TThreadHashedSL.Find(Item: String): Integer;
begin
  LockList;
  try
    if not FList.Find(Item, Result) then
      Result := -1;
  finally
    UnlockList;
  end;
end;

procedure TThreadHashedSL.Delete(Index: Integer);
begin
  LockList;
  try
    FList.Delete(Index);
  finally
    UnlockList;
  end;
end;

procedure TThreadHashedSL.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

end.
