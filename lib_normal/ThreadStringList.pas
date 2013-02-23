unit ThreadStringList;

interface
uses windows,classes,sysUtils;

Type
  TThreadStringList = class (TObject)
  private
    FList: TStringList;
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
    function Text: String;
    function  LockList: TStringList;
    function Find(Item: String): Integer;
    procedure Delete(Index: Integer);
    procedure UnlockList;
    property Strings[Index: Integer]: string read Get write Put;
    property Objects[Index: Integer]: Pointer read GetObject write PutObject;
  end;

implementation

constructor TThreadStringList.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FList := TStringList.Create;
end;

destructor TThreadStringList.Destroy;
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

procedure TThreadStringList.Add(Item: String);
begin
  LockList;
  try
      FList.Add(Item);
  finally
    UnlockList;
  end;
end;

function TThreadStringList.Count: Integer;
begin
  LockList;
  try
    Result :=  FList.Count;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.Text: String;
begin
  LockList;
  try
    Result :=  FList.Text;
  finally
    UnlockList;
  end;
end;


procedure TThreadStringList.AddObject(Key: String; aItem: Pointer);
begin
  LockList;
  try
      FList.AddObject(Key, aItem);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Put(Index: Integer; const S: string);
begin
  LockList;
  try
    FList[Index] := S;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.Get(Index: Integer): string;
begin
  LockList;
  try
    Result := FList[Index];
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.PutObject(Index: Integer; const aItem: Pointer);
begin
  LockList;
  try
    FList.Objects[Index] := aItem;
  finally
    UnlockList;
  end;
end;

function TThreadStringList.GetObject(Index: Integer): Pointer;
begin
  LockList;
  try
    Result := FList.Objects[Index];
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function  TThreadStringList.LockList: TStringList;
begin
  EnterCriticalSection(FLock);
  Result := FList;
end;

function TThreadStringList.Find(Item: String): Integer;
begin
  LockList;
  try
    if not FList.Find(Item, Result) then
      Result := -1;
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.Delete(Index: Integer);
begin
  LockList;
  try
    FList.Delete(Index);
  finally
    UnlockList;
  end;
end;

procedure TThreadStringList.UnlockList;
begin
  LeaveCriticalSection(FLock);
end;

end.
