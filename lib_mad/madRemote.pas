unit madRemote;

interface

uses
  madTypes,
  madDisAsm,
  Windows;

type
  TRemoteExecuteFunction = function(params: Pointer): DWORD; stdcall;

  TDAProcess = array of record
    id: DWORD;
    exeFile: String;
    session: DWORD;
    sid: String;
  end;

  TProcessEntry32 = record
    size: Cardinal;
    usage: Cardinal;
    process: Cardinal;
    defaultHeap: Cardinal;
    module: Cardinal;
    threadCount: Cardinal;
    parentProcess: Cardinal;
    basePriority: Integer;
    flags: Cardinal;
    exeFile: packed array [0..259] of Char;
  end;

  TModuleEntry32 = record
    size: Cardinal;
    module: Cardinal;
    ownerProcess: Cardinal;
    GlobalUsage: Cardinal;
    ProcessUsage: Cardinal;
    baseAddress: Pointer;
    baseSize: Cardinal;
    handle: Cardinal;
    fileName: packed array [Byte] of Char;
    exePath: packed array [0..259] of Char;
  end;


var
  InitToolhelp: procedure;
  EnumProcesses: function: TDAProcess;
  GetKernel32ProcessHandle: function: DWORD;
  GetSmssProcessHandle: function:  DWORD;
  HandleLiveForever: function(handle: DWORD): DWORD;
  InitSharedMem9x: procedure(alloc: TPPointer; free: TPPointer);
  AllocMemEx: function(size: DWORD; processHandle: DWORD = 0): Pointer; stdcall;
  FreeMemEx: function(mem: Pointer; processHandle: DWORD = 0): BOOL; stdcall;
  InitUnprotectMemory: procedure;
  IsMemoryProtected: function(addr: Pointer): Boolean;
  UnprotectMemoryAsm: procedure;
  UnprotectMemory: function(addr: Pointer; size: DWORD): Boolean;
  ProtectMemory: function(addr: Pointer; size: DWORD): Boolean;
  CopyFunction: function(func: Pointer; processHandle: DWORD = 0; acceptUnknownTargets: Boolean = false; buffer: TPPointer = nil; fi: TPFunctionInfo = nil): Pointer;
  CreateRemoteThreadEx: function(processHandle: DWORD; threadAttr: PSecurityAttributes; stackSize: Integer; startAddr: Pointer; params: Pointer; creationFlags: DWORD; var threadId: DWORD): DWORD; stdcall;
  RemoteExecute: function(processHandle: DWORD; func: TRemoteExecuteFunction; var funcResult: DWORD; params: Pointer = nil; size: DWORD = 0): BOOL; stdcall;
  ProcessHandleToId: function(processHandle: DWORD): DWORD; stdcall;

implementation

uses madRmtDll;    

initialization

  InitToolhelp                  := madRmtDllDLL.FindExport('InitToolhelp');
  EnumProcesses                 := madRmtDllDLL.FindExport('EnumProcesses');
  GetKernel32ProcessHandle      := madRmtDllDLL.FindExport('GetKernel32ProcessHandle');
  GetSmssProcessHandle          := madRmtDllDLL.FindExport('GetSmssProcessHandle');
  HandleLiveForever             := madRmtDllDLL.FindExport('HandleLiveForever');
  InitSharedMem9x               := madRmtDllDLL.FindExport('InitSharedMem9x');
  AllocMemEx                    := madRmtDllDLL.FindExport('AllocMemEx');
  FreeMemEx                     := madRmtDllDLL.FindExport('FreeMemEx');
  InitUnprotectMemory           := madRmtDllDLL.FindExport('InitUnprotectMemory');
  IsMemoryProtected             := madRmtDllDLL.FindExport('IsMemoryProtected');
  UnprotectMemoryAsm            := madRmtDllDLL.FindExport('UnprotectMemoryAsm');
  UnprotectMemory               := madRmtDllDLL.FindExport('UnprotectMemory');
  ProtectMemory                 := madRmtDllDLL.FindExport('ProtectMemory');
  CopyFunction                  := madRmtDllDLL.FindExport('CopyFunction');
  CreateRemoteThreadEx          := madRmtDllDLL.FindExport('CreateRemoteThreadEx');
  RemoteExecute                 := madRmtDllDLL.FindExport('RemoteExecute');
  ProcessHandleToId             := madRmtDllDLL.FindExport('ProcessHandleToId');


end.