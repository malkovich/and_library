unit PacketViewSystem;
      //°ó¶¨ÁËlistview, memo
interface
uses
   TDStkQue,TDBasics, FormatFunction,
   Windows, SysUtils,StdCtrls, Variants, ComCtrls,Classes;

{$I PacketStruct.inc}

const
   BUF_SIZE=51200;
   ACAPACITY=100;

type
  PPackageUnit=^TPackageUnit;
  TPackageUnit= packed record
      len:word;
      buf:array[0..BUF_SIZE] of char;
  end;

  TPacketCallback = procedure (Buffer:pointer; Length:Word) of object;
  TModifyBufferInfo = procedure (var Buffer:pointer; var Length:Word) of object;
  TOnViewFormat = procedure (Buffer:pointer; Length:Word; var sMark,sTime,sIndex,sType,sLength,sContent,sDescribe:string)of object;
  TViewFilter = function (Buffer:Pointer; Length:Word):Boolean of object;
                                                                             
type
   TPktToListView = class(TThread)
   private
     FOnClick:TPacketCallback;
     FOnView : TOnViewFormat;
     FOnFilter :TViewFilter;
     FBuffer1:TtdArrayQueue;
     FBuffer2:Tlist;
     FaDispose1  : TtdDisposeProc;
     FStopFlag: bool;
     FMaxCapacity:word;
     FIsShutDown:bool;
     FAutoScroll :BOOL;
     CMD_ClearNow :BOOL;
     hEvent :THandle;
   protected
     FListView: TListView;
     procedure ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
     procedure FormatListViewStyle;
     procedure Clear_Viewlist;
     Procedure Clear_Queue;
     procedure AddToListView(Package:PPackageUnit);
     procedure Execute;override;
   public
     constructor Create(listview: TListview; MaxCapacity:word );
     destructor Destroy; override;
     procedure ShutDown;
     function ImportPacket(Buffer:Pointer; Length :WORD):bool;
     property OnClick:TPacketCallback read FOnClick write FOnClick;
     property OnView:TOnViewFormat read FOnView write FOnView;
     property OnCheckFilter:TViewFilter read FOnFilter write FOnFilter;
     property MaxCapacity:WORD read FMaxCapacity write FMaxCapacity;
     property AutoScroll:BOOL read FAutoScroll write FAutoScroll;
     property CleanNow:BOOL write CMD_ClearNow;
   end;


   TPktToMemo = class
   protected
      Fmemo:Tmemo;
      FIsContinueView:bool;
      FPacketSample : PPackageUnit;
   public
      constructor Create(memo:Tmemo);
      function ViewPacket(Buffer:pointer; Length:Word):LongBool;
      function StoreSample(Buffer:Pointer=NIL; Length:WORD=0):Pointer;
      function GetSample(var Buffer:Pointer; var Length:WORD):LongBool;
      property ContinueView:Bool read FIsContinueView write FIsContinueView;
   end;


type
  TPacketViewSystem= class(TObject)
   private     
     Fmemo:Tmemo;
     Flistview:TListview;
     FMaxCapacity:word;
     FOnOutputToMemo:TModifyBufferInfo;
   protected
     procedure OnListViewClick(Buffer:pointer; Length:Word);
     procedure SetMaxCapacity(MaxCapacity:word);
     function  GetMaxCapacity:WORD;
   public
     PacketViewer:TPktToListView;
     FPktToMemo:TPktToMemo;
     constructor Create(memo:Tmemo; listview:TListview; OnOutputToMemo:TModifyBufferInfo; OnHowToView:TOnViewFormat; MaxCapacity:word=1000);
     destructor Destroy; override;
     
     function ImportPacket(Buffer:Pointer; Length :WORD):bool;
     property MaxCapacity:word read  GetMaxCapacity write SetMaxCapacity;
   end;


implementation

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
///////////  TPktToListView


procedure TPktToListView.ShutDown;
begin
    FIsShutDown:=true;
    Terminate;
    SetEvent(hEvent);
    sleep(200);
    Clear_Queue;
    Clear_Viewlist;
end;  

procedure TPktToListView.Execute;
var
   Package:PPackageUnit;
   I, Count :Integer;
begin
  FreeOnTerminate:=False;
  TRY
    while NOT Terminated do
    begin
        if WAIT_FAILED = WaitForSingleObject(hEvent, 1000) then BREAK;
        if FIsShutDown then BREAK;
        
        if FStopFlag then continue;
        if FBuffer1.IsEmpty then continue;          

        if (FBuffer2.Count >= FMaxCapacity) then Clear_Viewlist;
        if CMD_ClearNow then
        begin
            Clear_Viewlist;
            CMD_ClearNow := False;
        end;

        Count := FBuffer1.Count;
        for I := 0 to Count - 1 do
        Begin
          Package:= FBuffer1.Dequeue;

          if CMD_ClearNow then
          begin
            Clear_Viewlist;
            CMD_ClearNow := False;
          end;

          if FOnFilter(@Package.buf[0], Package.len) then
          begin
            FBuffer2.Add(Package);
            AddToListView(Package);
          end;
        end;
    end;
  EXCEPT
  end;
end;

constructor TPktToListView.Create(listview: TListview; MaxCapacity:word);
begin
    if not assigned(listview) then exit;

    FListView:=listview;
    FMaxCapacity:=MaxCapacity;
    FStopFlag:=false;
    FIsShutDown:=false;
    ListView.OnSelectItem := ListView1SelectItem;
    FormatListViewStyle;

    hEvent := CreateEvent(Nil, True, False, nil);

    FBuffer1:=TtdArrayQueue.Create(FaDispose1,ACAPACITY);
    FBuffer2:=TList.Create;

    inherited Create(false);
end;

destructor TPktToListView.Destroy;
begin
    ShutDown;
    FBuffer1.Free;
    FBuffer2.Free;
    CloseHandle(hEvent);
    inherited Destroy;
end;

function TPktToListView.ImportPacket(Buffer:Pointer; Length :WORD):bool;
var
    Package:PPackageUnit;
begin
    if  FIsShutDown then
    begin
        result:=false;
        exit;
    end;

    Package:=pointer(GlobalAlloc(GPTR, Length + SizeOf(WORD)));

    Package.Len := Length;
    copymemory(@Package.buf[0], Buffer, Length);

    FBuffer1.Enqueue(Package);

    SetEvent(hEvent);
 
    result:=true;
end;


procedure TPktToListView.ListView1SelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  i:integer;
  PacketStruct :PPackageUnit;
begin
    FStopFlag:=true;
    i:=FListView.ItemIndex;
    if i=-1 then exit;
    PacketStruct:=FBuffer2.Items[i] ;

    if assigned(FOnClick) then
        FOnClick(@PacketStruct.Buf[0],PacketStruct.len);

    FStopFlag:=False;
end;

procedure TPktToListView.FormatListViewStyle;
begin
    FListView.ViewStyle:=vsReport;
    FListView.RowSelect:=true;
    FListView.ReadOnly:=true;
    FListView.MultiSelect:=false;
    FListView.Columns.Clear;

    FListView.Columns.Add.Caption:='Mark';
    FListView.Columns.Add.Caption:='Time';
    FListView.Columns.Add.Caption:='Index';
    FListView.Columns.Add.Caption:='Type';
    FListView.Columns.Add.Caption:='Size';
    FListView.Columns.Add.Caption:='Content';
    FListView.Columns.Add.Caption:='Describe';

    FListView.Columns.Items[0].Width:=125;
    FListView.Columns.Items[1].Width:=85;
    FListView.Columns.Items[2].Width:=50;
    FListView.Columns.Items[3].Width:=40;
    FListView.Columns.Items[4].Width:=40;
    FListView.Columns.Items[5].Width:=400;
    FListView.Columns.Items[6].Width:=200;
end;

procedure TPktToListView.Clear_Viewlist;
var
    i:integer;
begin
    for i:=0 to FBuffer2.Count-1 do
        GlobalFree(THandle(FBuffer2.Items[i]));
    FListView.Clear;
    FBuffer2.Clear;
end;

procedure TPktToListView.Clear_Queue;
begin
    while not FBuffer1.IsEmpty do
        GlobalFree(THandle(FBuffer1.Dequeue));
end;

procedure TPktToListView.AddToListView(Package:PPackageUnit);
var
  sMark,sTime,sIndex,sType,sLength,sContent,sDescribe:string;
begin
  if assigned(FOnView) then
  begin
    FOnView(@Package.buf[0], Package.len, sMark,sTime,sIndex,sType,sLength,sContent,sDescribe);

    with FListView.Items.Add do
    begin
        Caption:=sMark;
        SubItems.Add(sTime);
        SubItems.Add(sIndex);
        SubItems.Add(sType);
        SubItems.Add(sLength);
        SubItems.Add(sContent);
        SubItems.Add(sDescribe);

        if FAutoScroll then
          FListView.scroll(0,Position.Y);
    end;
  end;
end;


/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
///////////  TPacketViewSystem

constructor TPacketViewSystem.Create(memo:Tmemo; listview:TListview; OnOutputToMemo:TModifyBufferInfo; OnHowToView:TOnViewFormat; MaxCapacity:word=1000);
begin
    if not assigned(listview) then exit;
    if not assigned(memo) then exit;
    if not assigned(OnOutputToMemo) then exit;
    if not assigned(OnHowToView) then exit;

    inherited Create;

    Flistview:=listview;
    Fmemo:= memo;
    FMaxCapacity:=MaxCapacity;
    FOnOutputToMemo:=OnOutputToMemo;

    PacketViewer:=TPktToListView.Create(Flistview,FMaxCapacity);
    FPktToMemo:= TPktToMemo.Create(Fmemo);

    PacketViewer.OnClick := OnListViewClick;
    PacketViewer.OnView := OnHowToView;
end;

destructor TPacketViewSystem.Destroy;
begin
    PacketViewer.ShutDown;
    FPktToMemo.Free;
    inherited Destroy;
end;

function TPacketViewSystem.ImportPacket(Buffer:Pointer; Length :WORD):bool;
begin
    result:=PacketViewer.ImportPacket(Buffer,Length);
end;

procedure TPacketViewSystem.SetMaxCapacity(MaxCapacity:word);
begin
    PacketViewer.MaxCapacity := MaxCapacity;
end;

function TPacketViewSystem.GetMaxCapacity:WORD;
begin
    result := PacketViewer.MaxCapacity;
end;

procedure TPacketViewSystem.OnListViewClick(Buffer:pointer; Length:Word);
var
  buf:pointer;
  len:WORD;
begin
  buf := buffer;
  len := length;
  FOnOutputToMemo(buf, len);
  FPktToMemo.ViewPacket(buf,len);
  FPktToMemo.StoreSample(Buffer, Length);
end;

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
///////////  TPktToMemo

constructor TPktToMemo.Create(memo:Tmemo);
begin
    inherited Create;
    Fmemo:=memo;
    FIsContinueView:=false;
    Fmemo.Clear;
end;
function TPktToMemo.ViewPacket(Buffer:pointer; Length:Word):LongBool;
var
    ss:Tstringlist;
begin
    ss:=Tstringlist.Create;
    PcharToFormatedView(Buffer, Length, 0 , ss);
    result := ss.Count > 0;
    if FIsContinueView then
      FMemo.Lines.Add(ss.Text)
    else
      Fmemo.Text:=ss.Text;
    ss.Free;
end;

function TPktToMemo.StoreSample(Buffer:Pointer=NIL; Length:WORD=0):Pointer;
begin
  if Buffer = NIL then
  BEGIN
    FreeMem(FPacketSample);
    FPacketSample := NIL;
  END  ELSE
  BEGIN
    if FPacketSample <> NIL then
      FreeMem(FPacketSample);
    GetMem(FPacketSample, Length + SizeOf(WORD));
    FPacketSample.len := Length;
    CopyMemory(@FPacketSample.buf[0], Buffer, Length);
  END;
  RESULT := FPacketSample;
end;

function TPktToMemo.GetSample(var Buffer:Pointer; var Length:WORD):LongBool;
begin
  result := False;
  if not Assigned(FPacketSample) then exit;

  Buffer := @FPacketSample.buf[0];
  Length := FPacketSample.len;
  result := Assigned(Buffer);
end;


end.
