unit AndEchoMessages;

interface
uses windows, Classes, AndQueMessages, AndIpcProcedure;

Const
  MIN_CHANNEL_SIZE = $1000 DIV 2;
  MIN_CHANNEL_COUNT = 512;
  MIN_CHANNEL_MAX_THREAD = 16;

  HUGE_CHANNEL_SIZE = $1000 * 4;
  HUGE_CHANNEL_COUNT = 32;
  HUGE_CHANNEL_MAX_THREAD = 2;

Type
  TEchoCallBack = TIpcCallBack;

function CreateDualEchoMessage (IpcName: String; EchoCallBack: TEchoCallBack;
      MinChannelSize: Integer = MIN_CHANNEL_SIZE; MinChannelCount: Integer = MIN_CHANNEL_COUNT;
      MinThreadCount: Integer = MIN_CHANNEL_MAX_THREAD;
      HugeChannelSize: Integer = HUGE_CHANNEL_SIZE; HugeChannelCount: Integer = HUGE_CHANNEL_COUNT;
      HugeThreadCount: Integer = HUGE_CHANNEL_MAX_THREAD): BOOL; Stdcall;
function DestroyDualEchoMessage (IpcName: String): BOOL; Stdcall;

function SendEchoMessage (IpcName: String; TranSL: TStringList): BOOL; Stdcall;

Procedure GetSendQualityInfo (Out Average, Max: DWORD; ErrSL: TStringList);
function SendEchoMessageLOG (IpcName: String; TranSL: TStringList): BOOL; Stdcall;


implementation

uses
  SysUtils, ComObj, Math, ThreadStringList;

Type
  LPTMapHeadStruct = ^TMapHeadStruct;
  TMapHeadStruct = packed record
    OnService: BOOL;
    ShareMemName: String[NAME_STRING_LENGTH];
    MinChannelName: String[NAME_STRING_LENGTH];
    HugeChannelName: String[NAME_STRING_LENGTH];
    EchoCallBack: TEchoCallBack;
    MinChannelSize: Integer;
    MinChannelCount: Integer;
    MinThreadCount: Integer;
    HugeChannelSize: Integer;
    HugeChannelCount: Integer;
    HugeThreadCount: Integer;
    MapHandle: THandle;
    MapBase: Pointer;
  end;
                      
function CreateDualEchoMessage (IpcName: String; EchoCallBack: TEchoCallBack;
      MinChannelSize: Integer = MIN_CHANNEL_SIZE; MinChannelCount: Integer = MIN_CHANNEL_COUNT;
      MinThreadCount: Integer = MIN_CHANNEL_MAX_THREAD;
      HugeChannelSize: Integer = HUGE_CHANNEL_SIZE; HugeChannelCount: Integer = HUGE_CHANNEL_COUNT;
      HugeThreadCount: Integer = HUGE_CHANNEL_MAX_THREAD): BOOL; Stdcall;
var
  MapHead: LPTMapHeadStruct;
  FMapHandle: THandle;
  FMappingSize: Integer;
begin
  //初始结果
  Result := False;
  //边界断言
  Assert (Length(IpcName) < NAME_STRING_LENGTH, 'IpcName字符串过长');
  Assert (assigned (EchoCallBack), 'EchoCallBack = NULL');
  Assert (HugeChannelSize > MinChannelSize, 'HugeChannelSize太小');

  FMappingSize := SizeOf (TMapHeadStruct);
  FMapHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, FMappingSize, PChar(IpcName));
  if FMapHandle = 0 then
  begin
    SetLastError ($FFFF0003);
    Exit;
  end;          

  MapHead := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, FMappingSize);
  if MapHead = nil then
  begin
    SetLastError ($FFFF0004);
    CloseHandle (FMapHandle);
    Exit;
  end;

  MapHead.ShareMemName := IpcName;
  MapHead.MinChannelName := CreateClassID + '_Min';
  MapHead.MinChannelSize := MinChannelSize;
  MapHead.MinChannelCount := MinChannelCount;
  MapHead.HugeChannelName := CreateClassID + '_Huge';
  MapHead.HugeChannelSize := HugeChannelSize;
  MapHead.HugeChannelCount := HugeChannelCount;
  MapHead.EchoCallBack := EchoCallBack;
  MapHead.MinThreadCount := MinThreadCount;
  MapHead.HugeThreadCount := HugeThreadCount;
  MapHead.MapHandle := FMapHandle;
  MapHead.MapBase := Pointer(MapHead);

  //创建常规通道
  if not CreateIpcProcedure (MapHead.MinChannelName, EchoCallBack, MinChannelCount, MinChannelSize, MinThreadCount) then
  begin
    SetLastError ($FFFF0005);
    UnMapViewOfFile(MapHead);
    CloseHandle (FMapHandle);
    Exit;
  end;

  //创建特大通道
  if not CreateIpcProcedure (MapHead.HugeChannelName, EchoCallBack, HugeChannelCount, HugeChannelSize, HugeThreadCount) then
  begin
    SetLastError ($FFFF0006);
    UnMapViewOfFile(MapHead);
    CloseHandle (FMapHandle);
    DestroyIpcProcedure (MapHead.MinChannelName);
    Exit;
  end;

  MapHead.OnService := True;
  Result := True;
end;


function DestroyDualEchoMessage (IpcName: String): BOOL; Stdcall;
var
  MapHead: LPTMapHeadStruct;
  FMapHandle: THandle;
  MapHeadBk: TMapHeadStruct;
begin
  Result := False;

  FMapHandle := OpenFileMapping (FILE_MAP_ALL_ACCESS, False, PChar(IpcName));
  if FMapHandle = 0 then
  begin
    SetLastError ($FFFF0003);
    Exit;
  end;

  MapHead := MapViewOfFile (FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, SizeOf(TMapHeadStruct));
  if MapHead = nil then
  begin
    SetLastError ($FFFF0004);
    CloseHandle (FMapHandle);
    Exit;
  end;

  MapHead.OnService := False;  
  MapHeadBk := MapHead^;

  //释放本次访问的内存映射文件
  UnMapViewOfFile(MapHead);
  CloseHandle (FMapHandle);

  //释放原始内存映射文件
  UnMapViewOfFile (MapHeadBk.MapBase);
  CloseHandle (MapHeadBk.MapHandle);

  //解除双通道
  DestroyIpcProcedure (MapHeadBk.MinChannelName);
  DestroyIpcProcedure (MapHeadBk.HugeChannelName);

  Result := True;
end;          


function GetHeadMapInfo (IpcName: String; var RunResult: TMapHeadStruct): BOOL;
var
  MapHead: LPTMapHeadStruct;
  FMapHandle: THandle;
begin
  Result := False;

  FMapHandle := OpenFileMapping (FILE_MAP_READ, False, PChar(IpcName));
  if FMapHandle = 0 then
  begin
    SetLastError ($FFFF0101);
    Exit;
  end;

  MapHead := MapViewOfFile (FMapHandle, FILE_MAP_READ, 0, 0, SizeOf(TMapHeadStruct));
  if MapHead = nil then
  begin
    SetLastError ($FFFF0102);
    CloseHandle (FMapHandle);
    Exit;
  end;

  RunResult := MapHead^;

  //释放本次访问的内存映射文件
  UnMapViewOfFile(MapHead);
  CloseHandle (FMapHandle);

  Result := True;
end;


function SendEchoMessage (IpcName: String; TranSL: TStringList): BOOL; Stdcall;
var
  MapHead: TMapHeadStruct;
  SendSize:Integer;
begin
  Result := False;

  if not GetHeadMapInfo (IpcName, MapHead) then Exit;

  if not MapHead.OnService then
  begin
    Sleep(100);
    if not MapHead.OnService then
    begin
      SetLastError ($FFFF0201);
      Exit;
    end;
  end;

  SendSize := Length(TranSL.Text) + 1;
  if SendSize > MapHead.HugeChannelSize then
  begin
    SetLastError ($FFFF0202);
    Exit;
  end;

  if SendSize >= MapHead.MinChannelSize then
  begin
    Result := IpcProcedure (MapHead.HugeChannelName, TranSL);
  end else
  begin
    Result := IpcProcedure (MapHead.MinChannelName, TranSL);
  end;
end;

var
  TotalTickTime: DWORD;
  TotalTimes: DWORD;
  MaxTickTime: DWORD;
  ErrNoSL: TThreadStringList;     

Procedure RecordErrSend (ErrNo: DWORD);
var
  ErrNoStr, ErrCount: String;
  nErrCount: Integer;
  LockSL: TStringList;
begin
  LockSL := ErrNoSL.LockList;
  Try
    ErrNoStr := '$' + IntToHex(ErrNo,8);
    ErrCount := LockSL.Values[ErrNoStr];
    if ErrCount = '' then
    begin
      LockSL.Values[ErrNoStr] := '1';
    end else
    begin
      nErrCount := StrToInt (ErrCount);
      Inc(nErrCount);
      LockSL.Values[ErrNoStr] := IntToStr(nErrCount);
    end;
  finally
    ErrNoSL.UnlockList;
  end;     
end;

Procedure GetSendQualityInfo (Out Average, Max: DWORD; ErrSL: TStringList);
var
  LockSL: TStringList;
begin
  Max := MaxTickTime;
  Average := TotalTickTime div TotalTimes;

  LockSL := ErrNoSL.LockList;
  ErrSL.AddStrings(LockSL);
  ErrNoSL.UnlockList;
end;


function SendEchoMessageLOG (IpcName: String; TranSL: TStringList): BOOL; Stdcall;
var
  TimeTick: DWORD;
begin
  if not assigned (ErrNoSL) then
    ErrNoSL := TThreadStringList.Create;

  TimeTick := GetTickCount;
  Result := SendEchoMessage (IpcName, TranSL);
  if not Result then
    RecordErrSend (GetLastError);

  TimeTick := GetTickCount - TimeTick;
  Inc (TotalTickTime, TimeTick);
  Inc (TotalTimes);
  MaxTickTime := Max (MaxTickTime, TimeTick);
end;

end.
