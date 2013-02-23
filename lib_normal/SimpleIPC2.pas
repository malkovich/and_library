unit SimpleIPC2;

interface
uses windows, TCacheQueueThread;

Type
  THearCallBack = function (FromWho: PChar; Buffer: PChar; Size: Integer): LongBool; Stdcall;

function StartHear (WhoAmI: PChar; HearRecv: THearCallBack): LongBool; Stdcall;
function StopHear (WhoAmI: PChar): LongBool; Stdcall;
function Say (ToWho: PChar; Buffer: PChar; Size: Integer): LongBool; Stdcall;

implementation

uses
  madCHook, classes, SyncObjs, SysUtils;


Type
  LPTRegistIPC = ^TRegistIPC;
  TRegistIPC = record
    IpcName: String [48];
    QueHandle: THandle;
    Callback: THearCallBack;
  end;

var
  RegistList: TStringList;

procedure GlobalDataHandlePRO (Sender:Pointer; pBuf:Pointer; dwLen:dword; var Rollback: BOOL); Stdcall;
var
  ipc: LPTRegistIPC absolute Sender;
begin
  ipc.Callback (@ipc.IpcName[1], pBuf, dwLen);
end;

procedure PrintQueueCount (ipc: LPTRegistIPC);
var
  PacketCount: DWORD;
  PrintStr: String;
begin
  PacketCount := GetCacheCount (ipc.QueHandle);
  if PacketCount > 0 then
  if (PacketCount div 10) = 0 then
  begin
    PrintStr := ipc.IpcName + ':'+ IntToStr(PacketCount);
    OutputDebugString (PChar(PrintStr));
  end;
end;

procedure GetMsgFromDLL(name       : pchar;
                        messageBuf : pointer; messageLen : dword;
                        answerBuf  : pointer; answerLen  : dword); stdcall;
var
  Index: Integer;
  ipc: LPTRegistIPC;
  HearName:String;
begin
  if Assigned (answerBuf) then
    PBOOL(answerBuf)^ := False;

  HearName := StrPas (name);
  Index := RegistList.IndexOf(HearName);
  if Index = -1 then exit;
  ipc := Pointer (RegistList.Objects[Index]);    

  PrintQueueCount (ipc);
  PushCacheQueue (ipc.QueHandle, ipc, messageBuf, messageLen);

  if Assigned (answerBuf) then
    PBOOL(answerBuf)^ := True;
end;

function ReportQueueStatus (Param: Pointer): Integer; stdcall;
var
  ipc: LPTRegistIPC;
  Index: Integer;
  PacketCount: DWORD;
  PrintStr: String;
begin
  repeat
    sleep (2000);

    PrintStr := '';
    for Index := 0 to RegistList.Count - 1 do
    begin
      ipc := Pointer (RegistList.Objects[Index]);
      PacketCount := GetCacheCount (ipc.QueHandle);
      if PacketCount > 0 then
        PrintStr := PrintStr + ' ' + ipc.IpcName + ':'+ IntToStr(PacketCount);
    end;
    PrintStr := Trim(PrintStr);
    if PrintStr <> '' then
      OutputDebugString (PChar(PrintStr));
  until False;
  result:= 0;
end;

function StartHear (WhoAmI: PChar; HearRecv: THearCallBack): LongBool; Stdcall;
var
  Index: Integer;
  ipc: LPTRegistIPC;
  HearName:String;
begin
  Result := False;
   
  if not assigned (RegistList) then
  begin
    RegistList := TStringList.Create;
//    CreateThread (nil, 0, @ReportQueueStatus, nil, 0, DWORD(Index));
  end;

  HearName := StrPas (WhoAmi);
  Index := RegistList.IndexOf(HearName);
  if Index <> -1 then exit;

  ipc := AllocMem (SizeOf (TRegistIPC));
  ipc.Callback := HearRecv;
  ipc.IpcName := HearName;
  ipc.QueHandle := MakeCacheQueue (GlobalDataHandlePRO);
                             
  RegistList.AddObject(HearName, Pointer(ipc));
  Result := CreateIpcQueueEx (WhoAmI, GetMsgFromDLL, 1);
end;

function StopHear (WhoAmI: PChar): LongBool; Stdcall;
var
  ipc: LPTRegistIPC;
  Index: Integer;
  HearName:String;
begin
  Result := False;
  if not assigned (RegistList) then exit;

  HearName := StrPas (WhoAmi);
  Index := RegistList.IndexOf(HearName);
  ipc := Pointer (RegistList.Objects[Index]);
  FreeCacheQueue (ipc.QueHandle);
  FreeMem (ipc);
end;

function Say (ToWho: PChar; Buffer: PChar; Size: Integer): LongBool; Stdcall;
begin
  result := false;
  SendIpcMessage(ToWho, Buffer, Size, @result, sizeOf(result));
end;

end.
