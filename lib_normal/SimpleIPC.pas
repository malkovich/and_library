unit SimpleIPC;

interface
uses windows, TCacheQueueThread;

Type
  THearCallBack = function (FromWho: PChar; Buffer: PChar; Size: Integer): LongBool; Stdcall;

function StartHear (WhoAmI: PChar; HearRecv: THearCallBack): LongBool; Stdcall;
function StopHear (WhoAmI: PChar): LongBool; Stdcall;
function Say (ToWho: PChar; Buffer: PChar; Size: Integer): LongBool; Stdcall;

var
  IsHearMultiThread: BOOL = True;

implementation

uses
  madCHook, classes, SyncObjs, SysUtils;


Type
  LPTRegistIPC = ^TRegistIPC;
  TRegistIPC = record
    IpcName: string;
    Callback: THearCallBack;
  end;

var
  RegistList: TThreadList;
  DelayFree: TList;


procedure GetMsgFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
var
  I: Integer;
  ipc: LPTRegistIPC;
  List: TList;
begin
  if Assigned (answerBuf) then
    PBOOL(answerBuf)^ := False;

  List := RegistList.LockList;
  RegistList.UnlockList;

  for I := 0 to List.Count -1 do
  begin
      ipc := List[I];
      if  StrPas (name) = ipc.IpcName then
      begin
        if Assigned (answerBuf) then
          PBOOL(answerBuf)^ := ipc.Callback (name, messageBuf, messageLen)
        else
          ipc.Callback (name, messageBuf, messageLen);
        Break;
      end;
  end;

end;

function StartHear (WhoAmI: PChar; HearRecv: THearCallBack): LongBool; Stdcall;
var
  I: Integer;
  ipc: LPTRegistIPC;
  List: TList;
begin
  Result := False;
   
  if not assigned (RegistList) then
  begin
    RegistList := TThreadList.Create;
    DelayFree:= TList.Create;
  end;

  List := RegistList.LockList;
  try
    for I := 0 to List.Count -1 do
    begin
      ipc := List[I];
      if  StrPas (WhoAmI) = ipc.IpcName then
        exit;
    end;
  finally
    RegistList.UnlockList;
  end;

  ipc := AllocMem (SizeOf (TRegistIPC));
  ipc.IpcName := StrPas (WhoAmI);
  ipc.Callback := HearRecv;

  RegistList.Add(ipc);

  if IsHearMultiThread then
    Result := CreateIpcQueueEx (WhoAmI, GetMsgFromDLL)
  else
    Result := CreateIpcQueueEx (WhoAmI, GetMsgFromDLL, 1);
end;

function StopHear (WhoAmI: PChar): LongBool; Stdcall;
var
  I: Integer;
  ipc: LPTRegistIPC;
  List: TList;
begin
  Result := False;
  if not assigned (RegistList) then exit;

  List := RegistList.LockList;
  try
    for I := List.Count -1 downto 0 do
    begin
      ipc := List[I];
      if  StrPas (WhoAmI) = ipc.IpcName then
      begin
        DestroyIpcQueue (WhoAmI);
        DelayFree.Add(ipc);
        List.Delete(I);
        Break;
      end;
    end;
  finally
    RegistList.UnlockList;
  end;
end;

//procedure SayCacheQueue (Sender:Pointer; Buffer:Pointer; Size:dword); Stdcall;
//begin
//  SendIpcMessage(Sender, Buffer, Size);
//end;
//
//var
//  SayCache: THandle = 0;
//
//function Say (ToWho: PChar; Buffer: PChar; Size: Integer): LongBool; Stdcall;
//begin
//  if SayCache = 0 then
//    SayCache := MakeCacheQueue (SayCacheQueue);
//
//  PushCacheQueue (SayCache, ToWho, Buffer, Size);
//  result := True;
//end;

function Say (ToWho: PChar; Buffer: PChar; Size: Integer): LongBool; Stdcall;
begin
  result := false;
  SendIpcMessage(ToWho, Buffer, Size, @result, sizeOf(result));
end;

end.
