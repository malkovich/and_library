library madRmtDll;

uses
  SysUtils, madRemote, madTypes, madDisasm,
  windows;


procedure InitToolhelp;
begin
  madRemote.InitToolhelp;
end;

function EnumProcesses: TDAProcess;
begin
  result := madRemote.EnumProcesses;
end;

function GetKernel32ProcessHandle: DWORD;
begin
  result := madRemote.GetKernel32ProcessHandle;
end;

function GetSmssProcessHandle: DWORD;
begin
  result := madRemote.GetSmssProcessHandle;
end;

function HandleLiveForever(handle: DWORD): DWORD;
begin
  result := madRemote.HandleLiveForever(handle);
end;

procedure InitSharedMem9x(alloc: TPPointer; free: TPPointer);
begin
  madRemote.InitSharedMem9x(alloc, free);
end;

function AllocMemEx(size: DWORD; processHandle: DWORD = 0): Pointer; stdcall;
begin
  result := madRemote.AllocMemEx(size, processHandle);
end;

function FreeMemEx(mem: Pointer; processHandle: DWORD = 0): BOOL; stdcall;
begin
  result := madRemote.FreeMemEx(mem, processHandle);
end;

procedure InitUnprotectMemory;
begin
  madRemote.InitUnprotectMemory;
end;

function IsMemoryProtected(addr: Pointer): Boolean;
begin
  result := madRemote.IsMemoryProtected(addr);
end;

procedure UnprotectMemoryAsm;
begin
  madRemote.InitUnprotectMemory;
end;

function UnprotectMemory(addr: Pointer; size: DWORD): Boolean;
begin
  result := madRemote.UnprotectMemory(addr, size);
end;

function ProtectMemory(addr: Pointer; size: DWORD): Boolean;
begin
  result := madRemote.ProtectMemory(addr, size);
end;

function CopyFunction(func: Pointer; processHandle: DWORD = 0; acceptUnknownTargets: Boolean = false; buffer: TPPointer = nil; fi: TPFunctionInfo = nil): Pointer;
begin
  result := madRemote.CopyFunction(func, processHandle, acceptUnknownTargets, buffer, fi);
end;

function CreateRemoteThreadEx(processHandle: DWORD; threadAttr: PSecurityAttributes; stackSize: Integer; startAddr: Pointer; params: Pointer; creationFlags: DWORD; var threadId: DWORD): DWORD; stdcall;
begin
  result := madRemote.CreateRemoteThreadEx(processHandle, threadAttr, stackSize, startAddr, params, creationFlags, threadId);
end;

function RemoteExecute(processHandle: DWORD; func: TRemoteExecuteFunction; var funcResult: DWORD; params: Pointer = nil; size: DWORD = 0): BOOL; stdcall;
begin
  result := madRemote.RemoteExecute(processHandle, func, funcResult, params, size);
end;

function ProcessHandleToId(processHandle: DWORD): DWORD; stdcall;
begin
  result := madRemote.ProcessHandleToId(processHandle);
end;

exports
  InitToolhelp,
  EnumProcesses,
  GetKernel32ProcessHandle,
  GetSmssProcessHandle,
  HandleLiveForever,
  InitSharedMem9x,
  AllocMemEx,
  FreeMemEx,
  InitUnprotectMemory,
  IsMemoryProtected,
  UnprotectMemoryAsm,
  UnprotectMemory,
  ProtectMemory,
  CopyFunction,
  CreateRemoteThreadEx,
  RemoteExecute,
  ProcessHandleToId;

begin
end.
