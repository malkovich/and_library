unit IpcStack;

interface

uses
  windows, SysUtils, Dialogs, SyncObjs, Classes;

type
  PMappingInfo = ^TMappingInfo;
  TMappingInfo = packed   record
    Time: TDateTime;//保存创建的时间
    ProCount: LongInt;
    //当前创建了几个实例,对象在使用完后一定要Free   否则这里有值可能不准
  end;

  TFileMappingObj = class
  private
    FMutexName: String;           //互斥体名称
    FMutexHandle: THandle;        //互斥体
    FMapHandle: THandle;          //映射名柄
    FMapBuf: Pointer;             //映射文件数据区
    FMappingSize: LongInt;        //映射文件大小
    FMappingInfo: PMappingInfo;   //头信息
    FMaptingName: String;         //映射文件名
  protected
    property MutexHandle: THandle read FMutexHandle;
    procedure OnCreateMapping; virtual;//每一次创建时调用
    procedure OnOpenMapping;           //映射已创建,打开时调用
  public
    constructor Create(AMappingName: String; ASize: LongInt);//override;
    destructor Destroy; override;
    function GetMapBuf: Pointer;
    function GetMappingName: String;
    function GetMappingSize: LongInt;
    function GetProCount: LongInt;
    function GetTime: TDateTime;
  published
  end;


  PMapStackInfo = ^TMapStackInfo;
  TMapStackInfo = packed   record
    ItemSize: Integer;   //列表中元素的大小
    MaxCount: Integer;   //元素的最大个数
    Count: Integer;      //当前元素的总个数
    TopPoint: Integer;   //栈头
    EndPoint: Integer;   //栈底
  end;

  //弹出时清空原来的数据
  TStackNotifyEvent = procedure(Value: Pointer) of object;

  TMapCustomStack = class(TFileMappingObj)
  private
    FMapStackInfo: PMapStackInfo;
    FDateBuf: Pointer;
    FOnPopEndEvent: TStackNotifyEvent;    //出栈成功后清空数据时调用
    FOnPushEndEvent: TStackNotifyEvent;   //压栈成功后调用
  protected
    procedure PopClear(Value: Pointer); virtual;
  public
    constructor Create(AMappingName: String; AItemSize, AMaxCount: Integer);

    destructor Destroy; override;
    //压栈
    function Push(const Item): Integer; virtual; abstract;
    //出栈
    function Pop(var Item): Integer; virtual; abstract;
    function GetCount: Integer;
    function GetMaxCount: Integer;
    function GetItemSize: Integer;
    //给定物理序号返回指针,可能给定的位置没有值
    function GetItem(Index: Integer): Pointer;
    procedure Clear;
  published
    property OnPopEnd: TStackNotifyEvent read FOnPopEndEvent write FOnPopEndEvent;
    property OnPushEnd: TStackNotifyEvent read FOnPushEndEvent write FOnPushEndEvent;
  end;

  //用内存映射实现的栈,后进先出
  TMapStack = class(TMapCustomStack)
  private
  protected
  public
    //压栈   ,返回数据所在的位置   小于0没有成功   等于最大值栈满
    function Push(const Item): Integer; override;
    //出栈   小于0出栈操作失败
    function Pop(var Item): Integer; override;
  published

  end;

  //用内存映射实现的栈,先进先出
  TMapQueue = class(TMapCustomStack)
  private
  protected
  public
    //压栈
    function Push(const Item): Integer; override;
    //出栈
    function Pop(var Item): Integer; override;
  published

  end;

  //++++++++++++++++++内存映射实现的列表,保存同一种结构体支持多线程同步,
  //一次分配最大空间

  //用后进先出的堆栈实现,在栈中保存空闲空间的位置,
  //在数组中保存逻辑序号和物理序号的对应关系
  //创建两个映射文件,一个用来保存实际的数据,
  //另一个用户保存基本的分配信息

  //创建一个列表,可以多线程操作
  //用两个映射文件实现,一个用户保存基本的头信息,另一个保存数据
  //便于扩展成稀疏提交的文件方式

  PMappingListIndex = ^TMappingListIndex;
  TMappingListIndex = array of Integer;

  TMappingList = class(TFileMappingObj)
  private
    FItemSize: PInteger;
    FListIndexArr: PMappingListIndex;
    FListHeadStack: TMapStack;
    FListDataBuf: Pointer;//列表数据的位置
  protected
    procedure OnCreateMapping; override;
    procedure OnStackPopEnd(Value: Pointer);
  public
    constructor Create(AMappingName: String; AItemSize, AMaxCount: Integer);

    destructor Destroy; override;
    function Add(const Data): Integer;
    procedure Delete(Index: Integer);
    procedure ShowStackInfo(var AStrList: TStringList);
    procedure ShowDataInfo(var AStrList: TStringList);
    procedure Clear;
    function GetCount: Integer;
    function GetItem(Index: Integer): Pointer;
  published

  end;

implementation

  {   TFileMappingObj   }

function TFileMappingObj.GetProCount: LongInt;
begin
  Result := FMappingInfo.ProCount;
end;

constructor TFileMappingObj.Create(AMappingName: String; ASize: LongInt);
begin
  inherited   Create;
  FMaptingName := AMappingName;
  FMutexName := FMaptingName + '_Mutex';
  FMappingSize := ASize + Sizeof(TMappingInfo);
  //     +SEC_RESERVE

  FMutexHandle := CreateMutex(nil, False, PChar(FMutexName));
  case GetLastError of
    0:   //没有程序运行   第一次创建
      begin
        //映射到自己的地址空间
        case WaitForSingleObject(FMutexHandle, INFINITE) of
          WAIT_OBJECT_0:
            begin
              FMapHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, FMappingSize, PChar(FMaptingName));
              if FMapHandle <> 0 then
              begin
                FMappingInfo := MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0, 0, FMappingSize);
                if FMappingInfo <> nil then
                begin
                  FMapBuf := Pointer(Integer(FMappingInfo) + Sizeof(TMappingInfo));
                  FMappingInfo.ProCount := 1;
                  ZeroMemory(FMapBuf, FMappingSize);
                  //初始化为0
                  FMappingInfo.Time := Now;
                  OnCreateMapping;
                end;           
              end;
              ReleaseMutex(FMutexHandle);
            end;
          WAIT_TIMEOUT:;
          WAIT_FAILED:;
        end;
      end;
    ERROR_ALREADY_EXISTS://程序以经运行
      begin
        CloseHandle(FMutexHandle);
        FMutexHandle := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(FMutexName));
        case WaitForSingleObject(FMutexHandle, INFINITE) of
          WAIT_ABANDONED:
            ShowMessage('OK');
          WAIT_OBJECT_0:
            begin
              FMapHandle :=
                OpenFileMapping(FILE_MAP_WRITE, False, PChar(FMaptingName));
              if FMapHandle <> 0 then
              begin
                FMappingInfo :=
                  MapViewOfFile(FMapHandle, FILE_MAP_ALL_ACCESS, 0,
                  0, FMappingSize);
                if FMappingInfo <> nil then
                begin
                  FMapBuf :=
                    Pointer(Integer(FMappingInfo) + Sizeof(TMappingInfo));
                  InterlockedIncrement(FMappingInfo.ProCount);
                  //程序个数加1
                  OnOpenMapping;
                end;
              end;
              ReleaseMutex(FMutexHandle);
            end;
          WAIT_TIMEOUT:;
          WAIT_FAILED:;
        end;
      end;
    ERROR_INVALID_HANDLE:;//和其它内核对象同名
  end;
end;

destructor TFileMappingObj.Destroy;
begin
  FMappingInfo.ProCount := FMappingInfo.ProCount - 1;
  CloseHandle(FMapHandle);
  //释放互斥对象
  CloseHandle(FMutexHandle);
  inherited;
end;

function TFileMappingObj.GetMapBuf: Pointer;
begin
  Result := FMapBuf;
end;

function TFileMappingObj.GetMappingName: String;
begin
  Result := FMaptingName;
end;

function TFileMappingObj.GetMappingSize: LongInt;
begin
  Result := FMappingSize;
end;

function TFileMappingObj.GetTime: TDateTime;
begin
  Result := FMappingInfo.Time;
end;

procedure TFileMappingObj.OnCreateMapping;
begin
end;

procedure TFileMappingObj.OnOpenMapping;
begin
end;

  {   TMappingList   }

constructor TMappingList.Create(AMappingName: String;
  AItemSize, AMaxCount: Integer);
var
  i, L: Integer;
begin
  //列表的总的占用空间的大小   前部保存基本信息,后部保存数据
  FListHeadStack := TMapStack.Create(GetMappingName + '_HeadStack',
    Sizeof(Integer), AMaxCount);
  FListHeadStack.OnPopEnd := OnStackPopEnd;
  inherited   Create(AMappingName,
    Sizeof(Integer) +//保存用户数据大小
    Sizeof(Integer) * AMaxCount +//保存对应的序号列表
    AItemSize * AMaxCount);
  FItemSize := GetMapBuf;
  CopyMemory(FItemSize, @AItemSize, Sizeof(Integer));
  FListIndexArr := Pointer(Integer(FItemSize) + Sizeof(Integer));
  FListDataBuf := Pointer(Integer(FListIndexArr) + Sizeof(Integer) * AMaxCount);


  //第一次创建时将所有的空位置压栈
  L := FListHeadStack.GetMaxCount - 1;
  for   i := L downto 0 do
    TMappingListIndex(FListIndexArr)[i] := -1;
end;

function TMappingList.Add(const Data): Integer;
var
  APopIndex: Integer;
  ANewPoint: Pointer;
begin
  Result := -1;
  //添加一个记录,查找出一个空的位置保存,添加的位置并不一定是最后一个.
  if FListHeadStack.Pop(APopIndex) >= 0 then
  begin
    case WaitForSingleObject(MutexHandle, INFINITE) of
      WAIT_OBJECT_0:
        begin
          ANewPoint := Pointer(Integer(FListDataBuf) + APopIndex * FItemSize^);
          //将用户数据保存到映射文件
          CopyMemory(ANewPoint, @Data, FItemSize^);
          //将数据所在的物理位置保存
          Result := FListHeadStack.GetMaxCount - FListHeadStack.GetCount - 1;
          TMappingListIndex(FListIndexArr)[Result] := APopIndex;
          ReleaseMutex(MutexHandle);
        end;
      WAIT_TIMEOUT:;
      WAIT_FAILED:;
    end;
  end;
end;

procedure TMappingList.Delete(Index: Integer);
var
  ADataIndex: Integer;
  AItemPointer: Pointer;
  i, L: Integer;
begin
  //删除给定位置的值       给定的是用户添加的序号
  //保存时并不是保存在对应的物理位置
  //在栈中的对应的Index   中保存的是对应的物理位置
  //先找到给定的序号保存在物理空间的序号
  if (Index >= 0) and (Index < FListHeadStack.GetMaxCount) then
  begin
    //检查是否已经删除过了.
    ADataIndex := TMappingListIndex(FListIndexArr)[Index];
    //FListHeadStack.GetItem(Index);
    AItemPointer := Pointer(Integer(FListDataBuf) + ADataIndex * FItemSize^);

    //将当前删除的位置信息保存
    FListHeadStack.Push(ADataIndex);
    //栈的从上到下的序号对应用户数据的序号
    //用户数据的序号向上移动一位
    L := GetCount;
    for   i := Index to L do
      TMappingListIndex(FListIndexArr)[i] :=
        TMappingListIndex(FListIndexArr)[i + 1];
    TMappingListIndex(FListIndexArr)[L] := -1;
    //删除列表中的数据
    ZeroMemory(AItemPointer, FItemSize^);
  end;
end;


procedure TMappingList.OnCreateMapping;
var
  i, L: Integer;
begin
  inherited;
  //第一次创建时将所有的空位置压栈
  L := FListHeadStack.GetMaxCount - 1;
  for   i := L downto 0 do
  begin
    FListHeadStack.Push(i);
  end;
end;

procedure TMappingList.ShowStackInfo(var AStrList: TStringList);
var
  i, L: Integer;
  m: PInteger;
begin
  L := FListHeadStack.GetMaxCount - 1;
  for   i := 0 to L do
  begin
    m := FListHeadStack.GetItem(i);

    AStrList.Add(Format('%p   %d=%d',
      [m, m^, TMappingListIndex(FListIndexArr)[i]]));
  end;
end;

procedure TMappingList.Clear;
var
  i, L: Integer;
begin
  //清空所有的数据
  FListHeadStack.Clear;
  ZeroMemory(FListDataBuf, FItemSize^ * FListHeadStack.GetMaxCount);
  //第一次创建时将所有的空位置压栈
  L := FListHeadStack.GetMaxCount - 1;
  for   i := L downto 0 do
  begin
    FListHeadStack.Push(i);
  end;
  L := FListHeadStack.GetMaxCount - 1;
  for   i := L downto 0 do
    TMappingListIndex(FListIndexArr)[i] := -1;
end;

destructor TMappingList.Destroy;
begin
  FListHeadStack.Free;
  inherited;
end;

function TMappingList.GetCount: Integer;
begin
  Result := FListHeadStack.GetMaxCount - FListHeadStack.GetCount;
end;

procedure TMappingList.ShowDataInfo(var AStrList: TStringList);
var
  i, L: Integer;
  m: PInteger;
begin
  L := FListHeadStack.GetMaxCount - 1;
  for   i := 0 to L do
  begin
    m := Self.GetItem(i);
    AStrList.Add(Chr(m^) + '=' + IntToStr(i));
  end;
end;

function TMappingList.GetItem(Index: Integer): Pointer;
begin
  if (Index >= 0) and (Index < FListHeadStack.GetMaxCount) then
    Result := Pointer(Integer(FListDataBuf) + Index * FItemSize^)
  else
    Result := nil;
end;

procedure TMappingList.OnStackPopEnd(Value: Pointer);
var
  ANullIndex: Integer;
begin
  ANullIndex := -1;
  CopyMemory(Value, @ANullIndex, Sizeof(Integer));
end;

  {   TMapStack   }

function TMapCustomStack.GetCount: Integer;
begin
  Result := FMapStackInfo.Count;
end;

function TMapCustomStack.GetMaxCount: Integer;
begin
  Result := FMapStackInfo.MaxCount;
end;

function TMapStack.Pop(var Item): Integer;
var
  AFirstPointer: Pointer;
begin
  //出栈,后进先出
  case WaitForSingleObject(MutexHandle, INFINITE) of
    WAIT_OBJECT_0:
      begin
        if FMapStackInfo.Count = 0 then
          Result := -1
        else
        begin
          AFirstPointer :=
            Pointer(Integer(FDateBuf) + (FMapStackInfo.Count - 1) * GetItemSize);
          CopyMemory(@Item, AFirstPointer, GetItemSize);
          Result := FMapStackInfo.MaxCount - FMapStackInfo.Count;
          //返回弹出的位置
          FMapStackInfo.Count := FMapStackInfo.Count - 1;
          FMapStackInfo.TopPoint := FMapStackInfo.TopPoint - 1;
          PopClear(AFirstPointer);//弹出了,清空数据
          if Assigned(OnPopEnd) then
            OnPopEnd(AFirstPointer);
        end;
        ReleaseMutex(MutexHandle);
      end;
    WAIT_TIMEOUT: Result := -2;
    WAIT_FAILED:  Result := -3;
    ELSE          Result := -4;
  end;
end;

function TMapStack.Push(const Item): Integer;
var
  ANewPointer: Pointer;
begin
  //   压栈,添加一个新的数据,如果空间不足,则重新进行分配     后进先出
  case WaitForSingleObject(MutexHandle, INFINITE) of
    WAIT_OBJECT_0:
      begin
        if FMapStackInfo.Count = FMapStackInfo.MaxCount then
          Result := FMapStackInfo.MaxCount
            //如果栈满了则返回最大的值
        else
        begin
          ANewPointer := Pointer(Integer(FDateBuf) +
            (FMapStackInfo.TopPoint) * GetItemSize);
          CopyMemory(ANewPointer, @Item, GetItemSize);
          Result := FMapStackInfo.Count;   //返回当前所在的位置
          FMapStackInfo.Count := FMapStackInfo.Count + 1;
          FMapStackInfo.TopPoint := FMapStackInfo.TopPoint + 1;
          if Assigned(OnPushEnd) then
            OnPushEnd(ANewPointer);
        end;
        ReleaseMutex(MutexHandle);
      end;
    WAIT_TIMEOUT: Result := -2;
    WAIT_FAILED:  Result := -3;
    ELSE          Result := -4;
  end;
end;

  {   TMapCustomStack   }

constructor TMapCustomStack.Create(AMappingName: String;
  AItemSize, AMaxCount: Integer);
begin
  inherited   Create(AMappingName, AItemSize * AMaxCount + Sizeof(TMapStackInfo));

  FMapStackInfo := GetMapBuf;
  FDateBuf := Pointer(Integer(FMapStackInfo) + Sizeof(TMapStackInfo));
  FMapStackInfo.ItemSize := AItemSize;
  FMapStackInfo.MaxCount := AMaxCount;
  FMapStackInfo.TopPoint := 0;//当前的栈顶
  FMapStackInfo.EndPoint := 0;
end;

destructor TMapCustomStack.Destroy;
begin
  inherited;
end;

function TMapCustomStack.GetItem(Index: Integer): Pointer;
begin
  //压栈时第一个压入的是栈底,最后一个是栈顶
  //在分配空间中保存时先保存到了0
  //给定的   Index   是指栈的位置,即使数组0是栈底   MaxCount
  //给定一个物理的位置,返回指针
  if (Index >= 0) and (Index < FMapStackInfo.MaxCount) then
    Result := Pointer(Integer(FDateBuf) + (GetMaxCount - Index -1)
      * GetItemSize)
  else
    Result := nil;
end;

procedure TMapCustomStack.PopClear(Value: Pointer);
begin
  ZeroMemory(Value, GetItemSize);
end;

procedure TMapCustomStack.Clear;
begin
  FMapStackInfo.Count := 0;
  FMapStackInfo.TopPoint := 0;//当前的栈顶
  FMapStackInfo.EndPoint := 0;
  ZeroMemory(FDateBuf, GetItemSize * GetMaxCount);
end;

function TMapCustomStack.GetItemSize: Integer;
begin
  Result := FMapStackInfo.ItemSize;
end;

  {   TMapStack_2   }

function TMapQueue.Pop(var Item): Integer;
var
  AFirstPointer: Pointer;
begin
  //出栈   先进先出
  Result := -1;
  case WaitForSingleObject(MutexHandle, INFINITE) of
    WAIT_OBJECT_0:
      begin
        if FMapStackInfo.Count = 0 then     //没有数据

        else
        begin
          //最先弹出的是栈底
          AFirstPointer :=
            Pointer(Integer(FDateBuf) + (FMapStackInfo.EndPoint) * GetItemSize);
          CopyMemory(@Item, AFirstPointer, GetItemSize);
          Result := FMapStackInfo.EndPoint;   //返回弹出的位置
          FMapStackInfo.Count := FMapStackInfo.Count - 1;
          if FMapStackInfo.EndPoint = FMapStackInfo.MaxCount - 1 then   
            FMapStackInfo.EndPoint := 0
          else     //这里没有检查和顶的位置,用Count来控制
            FMapStackInfo.EndPoint := FMapStackInfo.EndPoint + 1;

          //   FMapStackInfo.TopPoint   :=FMapStackInfo.TopPoint-1;
          PopClear(AFirstPointer);
          if Assigned(OnPopEnd) then
            OnPopEnd(AFirstPointer);
        end;
        ReleaseMutex(MutexHandle);
      end;
    WAIT_TIMEOUT:;
    WAIT_FAILED:;
  end;
end;

function TMapQueue.Push(const Item): Integer;
var
  ANewPointer: Pointer;
begin       //压栈   先进先出
            //   压栈,添加一个新的数据,如果空间不足,则重新进行分配
  Result := -1;
  case WaitForSingleObject(MutexHandle, INFINITE) of
    WAIT_OBJECT_0:
      begin
        //if   FMapStackInfo.Count=FMapStackInfo.MaxCount   then     //栈满了
        if (FMapStackInfo.TopPoint = FMapStackInfo.EndPoint) and
          (FMapStackInfo.Count > 0) then
          Result := FMapStackInfo.MaxCount
            //如果栈满了则返回最大的值
        else
        begin
          //新的位置,就是栈头的位置
          ANewPointer := Pointer(Integer(FDateBuf) +
            (FMapStackInfo.TopPoint) * GetItemSize);
          CopyMemory(ANewPointer, @Item, GetItemSize);
          Result := FMapStackInfo.Count;   //返回当前所在的位置
          FMapStackInfo.Count := FMapStackInfo.Count + 1;
          //栈顶到了列表的最后一个则重新回到0
          if FMapStackInfo.TopPoint = FMapStackInfo.MaxCount - 1 then    
            FMapStackInfo.TopPoint := 0
          else
            FMapStackInfo.TopPoint := FMapStackInfo.TopPoint + 1;

          if Assigned(OnPushEnd) then
            OnPushEnd(ANewPointer);
        end;
        ReleaseMutex(MutexHandle);
      end;
    WAIT_TIMEOUT:;
    WAIT_FAILED:;
  end;
end;


  end.

