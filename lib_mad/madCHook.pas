// ***************************************************************
//  madCHook.pas              version:  2.1d  ? date: 2005-11-28
//  -------------------------------------------------------------
//  API hooking, code hooking
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-11-28 2.1d automatic unhooking bug fixed
// 2005-06-13 2.1c (1) automatic unhooking added when used in a single process
//                 new functions (Un)InstallMadCHook added
// 2005-04-05 2.1b (1) flag "DONT_COUNT" renamed to "NO_SAFE_UNHOOKING"
//                 (2) "NO_IMPROVED_SAFE_UNHOOKING" flag added
//                 (3) "SAFE_HOOKING" -> thread safe hook installation (winNT)
// 2004-04-27 2.1a IsHookInUse added
// 2004-03-07 2.1  (1) NO_MIXTURE_MODE flag added
//                 (2) CreateIpcQueueEx added with additional parameters
// 2003-10-05 2.0a changed initialization/finalization logic to fix memory leak
// 2003-08-10 2.0  (1) HookCode parameters changed -> only one flags parameter
//                 (2) (Un)InjectLibrary: user/session/system wide injection!
//                 (3) InjectLibrary2 replaced by InjectLibrary (auto detect)
//                 (4) static lib for Microsoft C++ added
//                 (5) CreateIpcQueue + SendIpcMessage + DestroyIpcQueue added
//                 (6) AmSystemProcess + AmUsingInputDesktop added
//                 (7) GetCurrentSessionId + GetInputSessionId added
//                 (8) GetCallingModule function added
//                 (9) ProcessIdToFileName added
//                 (a) Create/OpenGlobalMutex + Event + FileMapping added
//                 (b) WideToAnsi + AnsiToWide functions added
//                 (c) RenewHook function added
//                 (d) madCodeHook.dll -> madCHook.dll (8.3 dos name logic)
//                 (e) UnhookAPI added (= UnhookCode, added just for the look)
//                 (f) AddAccessForEveryone added
// 2002-10-17 1.3f InjectLibrary2(W) was not stdcall (dumb me)
// 2002-10-03 1.3e (1) InjectLibraryW added
//                 (2) InjectLibrary2(W) added for use in CreateProcess(W) hooks
// 2002-09-22 1.3d CreateProcessExW added
// 2002-03-24 1.3c CollectHooks/FlushHooks speed up mixture initialization
// 2002-02-24 1.3b LPSTARTUPINFO -> LPSTARTUPINFOA
// 2002-01-21 1.3a ProcessHandleToId exported
// 2001-07-08 1.3  new functions (1) AllocMemEx & FreeMemEx
//                               (2) CopyFunction
//                               (3) CreateRemoteThread and
//                               (4) InjectLibrary added
// 2001-04-20 1.2a you can now force HookCode/API to use the mixture mode
// 2001-04-16 1.2  new function CreateProcessEx -> dll injecting

unit madCHook;

interface

uses Windows, _madCHook;

// ***************************************************************

type TPCardinal = ^cardinal;
     TPPointer  = ^pointer;

// ***************************************************************

// you don't need to install madCodeHook, but you can
// what purpose does this have?
// (1) in the NT family madCodeHook uses a little kernel mode injection driver
//     to inject your hook dlls into all newly created processes
//     if you install madCodeHook, this driver is officially added to the OS
//     list of installed drivers with the description "madCodeHook DLL
//     injection driver"
//     if you don't install madCodeHook, the driver is temporarily extracted
//     to harddisk, which might look "suspicious"
// (2) some protection software (e.g. DeepFreeze) doesn't allow dynamic loading
//     of drivers, so we have to officially and permanently install our driver
//     to make madCodeHook work together with such protection software
// each (successful) call to "InstallMadCHook" needs its own call to
// "UninstallMadCHook", since the installation number is reference counted
function   InstallMadCHook : bool; stdcall; external 'madCHook.dll';
function UninstallMadCHook : bool; stdcall; external 'madCHook.dll';

// ***************************************************************

const
  // by default madCodeHook counts how many times any thread is currently
  // running inside of your callback function
  // this way unhooking can be safely synchronized to that counter
  // sometimes you don't need/want this counting to happen, e.g.
  // (1) if you don't plan to ever unhook, anyway
  // (2) if the counting performance drop is too high for your taste
  // (3) if you want to unhook from inside the hook callback function
  // in those cases you can set the flag "NO_SAFE_UNHOOKING"
  DONT_COUNT = $00000001;         // old name - kept for compatability
  NO_SAFE_UNHOOKING = $00000001;  // new name

  // with 2.1f the "safe unhooking" functionality (see above) was improved
  // most probably there's no problem with the improvement
  // but to be sure you can disable the improvement
  // the improved safe unhooking is currently only available in the NT family
  NO_IMPROVED_SAFE_UNHOOKING = $00000040;

  // optionally madCodeHook can use a special technique to make sure that
  // hooking in multi threaded situations won't result in crashing threads
  // this technique is not tested too well right now, so it's optional for now
  // you can turn this feature on by setting the flag "SAFE_HOOKING"
  // without this technique crashes can happen, if a thread is calling the API
  // which we want to hook in exactly the moment when the hook is installed
  // safe hooking is currently only available in the NT family
  SAFE_HOOKING = $00000020;

  // madCodeHook implements two different API hooking methods
  // the mixture mode is the second best method, it's only used if the main
  // hooking method doesn't work for whatever reason (e.g. API code structure)
  // normally madCodeHook chooses automatically which mode to use
  // you can force madCodeHook to use the mixture mode by specifying the flag:
  MIXTURE_MODE = $00000002;

  // if you don't want madCodeHook to use the mixture mode, you can say so
  // however, if the main hooking mode can't be used, hooking then simply fails
  NO_MIXTURE_MODE = $00000010;

  // under win9x you can hook code system wide, if it begins > $80000000
  // or if the code section of the to-be-hooked dll is shared
  // the callback function is in this case automatically copied to shared memory
  // use only kernel32 APIs in such a system wide hook callback function (!!)
  // if you want an easier way and/or a NT family compatible way to hook code
  // system wide, please use InjectLibrary(ALL_SESSIONS) instead of these flags:
  SYSTEM_WIDE_9X            = $00000004;
  ACCEPT_UNKNOWN_TARGETS_9X = $00000008;

// hook any code or a specific API
function HookCode (code         : pointer;
                   callbackFunc : pointer;
                   out nextHook : pointer;
                   flags        : dword = 0) : bool; stdcall;
         external 'madCHook.dll';
function HookAPI (module, api  : pchar;
                  callbackFunc : pointer;
                  out nextHook : pointer;
                  flags        : dword = 0) : bool; stdcall;
         external 'madCHook.dll';

// some firewall/antivirus programs kill our hooks, so we need to renew them
function RenewHook (var nextHook: pointer) : bool; stdcall;
         external 'madCHook.dll';

// is the hook callback function of the specified hook currently in use?
// 0: the hook callback function is not in use
// x: the hook callback function is in use x times
function IsHookInUse (var nextHook: pointer) : integer; stdcall;
         external 'madCHook.dll';

// unhook again
function UnhookCode (var nextHook: pointer) : bool; stdcall;
         external 'madCHook.dll';
function UnhookAPI (var nextHook: pointer) : bool; stdcall;
         external 'madCHook.dll';

// putting all your "HookCode/API" calls into a "CollectHooks".."FlushHooks"
// frame can eventually speed up the installation of the hooks
procedure CollectHooks; external 'madCHook.dll';
procedure   FlushHooks; external 'madCHook.dll';

// ***************************************************************
// same as CreateProcess
// additionally the dll "loadLibrary" is injected into the newly created process
// the dll is loaded right before the entry point of the exe module is called

function CreateProcessEx  (applicationName, commandLine : pchar;
                           processAttr, threadAttr      : PSecurityAttributes;
                           inheritHandles               : bool;
                           creationFlags                : dword;
                           environment                  : pointer;
                           currentDirectory             : pchar;
                           const startupInfo            : TStartupInfo;
                           var processInfo              : TProcessInformation;
                           loadLibrary                  : pchar              ) : bool; stdcall;
         external 'madCHook.dll' name 'CreateProcessExA';
function CreateProcessExW (applicationName, commandLine : pwidechar;
                           processAttr, threadAttr      : PSecurityAttributes;
                           inheritHandles               : bool;
                           creationFlags                : dword;
                           environment                  : pointer;
                           currentDirectory             : pwidechar;
                           const startupInfo            : TStartupInfo;
                           var processInfo              : TProcessInformation;
                           loadLibrary                  : pwidechar          ) : bool; stdcall;
         external 'madCHook.dll';

// ***************************************************************
// memory allocation in the specified processes (shared memory in win9x)
// if the processHandle is 0, the memory is allocated or freed in the shared
// area (in win9x) or in our own process (in winNT)

var
  AllocMemEx: function  (size : dword;   processHandle : dword = 0) : pointer; stdcall;
  FreeMemEx: function (mem  : pointer; processHandle : dword = 0) : bool;    stdcall;

// ***************************************************************
// copy (and relocate) any function to a new location in any process
// if the processHandle is 0, the function is copied to shared area (in win9x)
// or to another memory location in our own process (in winNT)
// don't forget to free the function with FreeMemEx, if you don't it anymore

function CopyFunction (func                 : pointer;
                       processHandle        : dword     = 0;
                       acceptUnknownTargets : boolean   = false;
                       buffer               : TPPointer = nil  ) : pointer; stdcall;
         external 'madCHook.dll';

// ***************************************************************
// like CreateRemoteThread, but 3 changes:
// (1) this one also works perfectly in win9x!!
// (2) this one also works on other sessions in winNt
// (3) the DACL of the current thread is copied in winNt (if threadAttr = nil)
var
  CreateRemoteThreadEx: function (processHandle : dword;
                               threadAttr    : PSecurityAttributes;
                               stackSize     : integer;
                               startAddr     : pointer;
                               params        : pointer;
                               creationFlags : dword;
                               var threadId  : dword              ) : dword; stdcall;

// ***************************************************************

// this is how your remote function must look like
type TRemoteExecuteFunction = function (params: pointer) : dword; stdcall;

// executes the specified function in the context of another process
// this works only if the function follows some specific rules
// e.g. it must not use global variables, nor Delphi private functions
// only win32 APIs are allowed
// don't use Delphi strings, since they end up in local Delphi function calls
// if "size" > 0, the "params" block will be copied to the other process
// after the remote function is finished, the "params" block is copied back
// so you can use the "params" block for both "in" and "out" parameters
// if "size" = 0, the "params" value is just given into the remote function
var
  RemoteExecute:  function (processHandle  : dword;
                        func           : TRemoteExecuteFunction;
                        var funcResult : dword;
                        params         : pointer = nil;
                        size           : dword = 0 ) : bool; stdcall;


// ***************************************************************

const
  // (un)inject the specified dll into (from) all current and future processes
  // these flags can be used for both UninjectLibrary + InjectLibrary
  ALL_SESSIONS     = $FFFFFFED;  // apps of all sessions
  CURRENT_SESSION  = $FFFFFFEC;  // apps of current session
  CURRENT_USER     = $FFFFFFEB;  // apps of current user

  // the following flags may only be used in combination with the first 3 flags
  SYSTEM_PROCESSES = $10;  // include this flag to include system processes + services
  CURRENT_PROCESS  = $08;  // exclude this flag to exclude injection into yourself

// same as LoadLibrary, but this one is able to load the library into any process
var
  InjectLibrary: function (processHandle: dword; libFileName: pchar;     timeOut: dword = 7000) : bool; stdcall;
  InjectLibraryW: function (processHandle: dword; libFileName: pwidechar; timeOut: dword = 7000) : bool; stdcall;

// same as InjectLibrary(CURRENT_SESSION, ...), but you can choose the session
  InjectLibrarySession: function  (session: dword; systemProcesses: bool; libFileName: pchar;     timeOut: dword = 7000) : bool; stdcall;
  InjectLibrarySessionW: function (session: dword; systemProcesses: bool; libFileName: pwidechar; timeOut: dword = 7000) : bool; stdcall;  

const
  // stop the "virus" injection effect (can be used for UninjectLibrary only)
  STOP_VIRUS = $FFFFFFFA;

// same as FreeLibrary, but is able to free the library from any process
function UninjectLibrary  (processHandle: dword; libFileName: pchar;     timeOut: dword = 7000) : bool; stdcall; external 'madCHook.dll' name 'UninjectLibraryA';
function UninjectLibraryW (processHandle: dword; libFileName: pwidechar; timeOut: dword = 7000) : bool; stdcall; external 'madCHook.dll';

// same as UninjectLibrary(CURRENT_SESSION, ...), but you can choose the session
function UninjectLibrarySession  (session: dword; systemProcesses: bool; libFileName: pchar;     timeOut: dword = 7000) : bool; stdcall; external 'madCHook.dll' name 'UninjectLibrarySessionA';
function UninjectLibrarySessionW (session: dword; systemProcesses: bool; libFileName: pwidechar; timeOut: dword = 7000) : bool; stdcall; external 'madCHook.dll';

// ***************************************************************

// which processId belongs to the specified process handle?
// undocumented function, works in all windows 32 bit systems
function ProcessHandleToId (processHandle : dword) : dword; stdcall;
         external 'madCHook.dll';

// find out what file the specified process was executed from
// the file name buffer must have a size of MAX_PATH characters (or more)
var
  ProcessIdToFileName: function  (processId : dword;  fileName  : pchar) : bool; stdcall;

// ***************************************************************

// is the current process a service/system process?  (win9x -> always false)
function AmSystemProcess : bool; stdcall; external 'madCHook.dll';

// is the current thread's desktop the input desktop?  (win9x -> always true)
// only in that case you should show messages boxes or other GUI stuff
// but please note that in XP fast user switching AmUsingInputDesktop may
// return true, although the current session is currently not visible
// XP fast user switching is implemented by using terminal server logic
// so each fast user session has its own window station and input desktop
var
  AmUsingInputDesktop : function : bool; stdcall;

// the following two functions can be used to get the session id of the
// current session and of the input session
// each terminal server (or XP fast user switching) session has its own id
// the "input session" is the one currently shown on the physical screen
function GetCurrentSessionId : dword; stdcall; external 'madCHook.dll';
function GetInputSessionId   : dword; stdcall; external 'madCHook.dll';

// ***************************************************************

// which module called me? works only if your function has a stack frame
var
  GetCallingModule : function :dword; stdcall;


// ***************************************************************
// global  =  normal  +  "access for everyone"  +  "non session specific"

function CreateGlobalMutex (name: pchar) : dword; stdcall; external 'madCHook.dll';
function   OpenGlobalMutex (name: pchar) : dword; stdcall; external 'madCHook.dll';

function CreateGlobalEvent (name: pchar; manual, initialState: bool) : dword; stdcall; external 'madCHook.dll';
function   OpenGlobalEvent (name: pchar                            ) : dword; stdcall; external 'madCHook.dll';

function CreateGlobalFileMapping (name: pchar; size: dword) : dword; stdcall; external 'madCHook.dll';
function   OpenGlobalFileMapping (name: pchar; write: bool) : dword; stdcall; external 'madCHook.dll';

// ***************************************************************

// convert strings ansi <-> wide
// the result buffer must have a size of MAX_PATH characters (or more)
// please use these functions in nt wide API hook callback functions
// because the OS' own functions seem to confuse nt in hook callback functions
procedure AnsiToWide (ansi: pchar; wide: pwidechar); stdcall; external 'madCHook.dll';
procedure WideToAnsi (wide: pwidechar; ansi: pchar); stdcall; external 'madCHook.dll';

// ***************************************************************
// ipc (inter process communication) message services
// in contrast to SendMessage the following functions don't crash NT services

type
  // this is how you get notified about incoming ipc messages
  // you have to write a function which fits to this type definition
  // and then you give it into "CreateIpcQueue"
  // your callback function will then be called for each incoming message
  // CAUTION: each ipc message is handled by a seperate thread, as a result
  //          your callback will be called by a different thread each time
  TIpcCallback = procedure (name       : pchar;
                            messageBuf : pointer;
                            messageLen : dword;
                            answerBuf  : pointer;
                            answerLen  : dword); stdcall;

// create an ipc queue
// please choose a unique ipc name to avoid conflicts with other programs
// only one ipc queue with the same name can be open at the same time
// so if 2 programs try to create the same ipc queue, the second call will fail
// you can specify how many threads may be created to handle incoming messages
// if the order of the messages is crucial for you, set "maxThreadCount" to "1"
// in its current implementation "maxThreadCount" only supports "1" or unlimited
// the parameter "maxQueueLen" is not yet implemented at all

var
  CreateIpcQueue: function (ipc: pchar; callback: TIpcCallback ): bool; stdcall;
  CreateIpcQueueEx: function (ipc            : pchar;
                           callback       : TIpcCallback;
                           maxThreadCount : dword = 16;
                           maxQueueLen    : dword = $1000) : bool; stdcall;

// send an ipc message to whomever has created the ipc queue (doesn't matter)
// if you only fill the first 3 parameters, SendIpcMessage returns at once
// if you fill the next two parameters, too, SendIpcMessage will
// wait for an answer of the ipc queue owner
// you can further specify how long you're willing to wait for the answer
// and whether you want SendIpcMessage to handle messages while waiting
var
  SendIpcMessage :function (ipc            : pchar;
                        messageBuf     : pointer;
                        messageLen     : dword;
                        answerBuf      : pointer = nil;
                        answerLen      : dword   = 0;
                        answerTimeOut  : dword   = INFINITE;
                        handleMessages : bool    = true    ) : bool; stdcall;

// destroy the ipc queue again
// when the queue owning process quits, the ipc queue is automatically deleted
// only the queue owning process can destroy the queue
var
  DestroyIpcQueue: function (ipc: pchar) : bool; stdcall;

// ***************************************************************

// this function adds some access rights to the specified target
// the target can either be a process handle or a service handle
function AddAccessForEveryone (processOrService, access: dword) : bool; stdcall;
         external 'madCHook.dll';

// ***************************************************************


implementation

uses madTools;

procedure Initial;
begin
  CreateIpcQueue  := madCHookDLL.FindExport('CreateIpcQueue');
  CreateIpcQueueEx := madCHookDLL.FindExport('CreateIpcQueueEx');
  DestroyIpcQueue := madCHookDLL.FindExport('DestroyIpcQueue');
  SendIpcMessage  := madCHookDLL.FindExport('SendIpcMessage');
  RemoteExecute   := madCHookDLL.FindExport('RemoteExecute');
  InjectLibrary   := madCHookDLL.FindExport('InjectLibraryA');
  InjectLibraryW  := madCHookDLL.FindExport('InjectLibraryW');
  InjectLibrarySession:= madCHookDLL.FindExport('InjectLibrarySessionA');
  InjectLibrarySessionW:= madCHookDLL.FindExport('InjectLibrarySessionW');
  CreateRemoteThreadEx:= madCHookDLL.FindExport('CreateRemoteThreadEx');
  AllocMemEx          := madCHookDLL.FindExport('AllocMemEx');
  FreeMemEx           := madCHookDLL.FindExport('FreeMemEx');
  ProcessIdToFileName := madCHookDLL.FindExport('ProcessIdToFileName');
  GetCallingModule    := madCHookDLL.FindExport('GetCallingModule');
  AmUsingInputDesktop := madCHookDLL.FindExport('AmUsingInputDesktop');
end;

procedure AutoUnhook (moduleHandle: dword); stdcall;
var
  _AutoUnhook:  procedure (moduleHandle: dword); stdcall;
begin
  _AutoUnhook := madCHookDLL.FindExport('AutoUnhook');
  _AutoUnhook (moduleHandle);
end;


initialization
  Initial;
  InitTryExceptFinally;
  IsMultiThread := true;
finalization
  AutoUnhook(HInstance);
end.
