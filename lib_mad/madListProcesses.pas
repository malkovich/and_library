// ***************************************************************
//  madListProcesses.pas      version:  1.0   ·  date: 2005-07-09
//  -------------------------------------------------------------
//  madExcept plugin "list of running processes"
//  -------------------------------------------------------------
//  Copyright (C) 1999 - 2005 www.madshi.net, All Rights Reserved
// ***************************************************************

// 2005-07-09 1.0  initial release

unit madListProcesses;

{$I mad.inc}

interface

// ***************************************************************

// process list with the following format:
// "processID fileName [sessionID] priority filePath"
function GetProcessList : string;

// ***************************************************************

implementation

uses Windows, madExcept, madStrings, madTypes;

// ***************************************************************
// enumeration of processes/threads/modules in win9x

type
  TProcessEntry32 = record
    size          : cardinal;
    usage         : cardinal;
    process       : cardinal;  // this process (ID)
    defaultHeap   : cardinal;  // default heap (ID)
    module        : cardinal;
    threadCount   : cardinal;
    parentProcess : cardinal;  // this process's parent process (ID)
    basePriority  : integer;   // Base priority of process's threads
    flags         : cardinal;
    exeFile       : array [0..MAX_PATH - 1] of char;
  end;

var
  Process32First           : function (snap: dword; var pe: TProcessEntry32) : bool stdcall = nil;
  Process32Next            : function (snap: dword; var pe: TProcessEntry32) : bool stdcall = nil;
  CreateToolhelp32Snapshot : function (flags, pid: dword) : dword stdcall = nil;

// ***************************************************************
// enumeration of processes/threads/modules in winNT

type
  TNtProcessInfo = record
    offset      : cardinal;
    numThreads  : cardinal;
    d1          : array [2..14] of cardinal;
    name        : PWideChar;
    d2          : cardinal;
    pid         : cardinal;
    parentPid   : cardinal;
    handleCount : cardinal;
    sessionId   : cardinal;
  end;

var
  NtQuerySystemInformation : function (infoClass: dword; buf: pointer; size: dword; retSize: TPCardinal) : dword stdcall = nil;
  GetModuleFileNameExA     : function (process, module: dword; fileName: pchar; size: dword) : dword stdcall = nil;

// ***************************************************************

function GetProcessList : string;
var count  : integer;
    items  : array of record
               fileName : string;
               path     : string;
               pid      : dword;
               sid      : dword;
               priority : integer;
             end;

  procedure AddItem(exeFile: string; pid, sid: dword);
  var i1 : integer;
      ph : dword;
  begin
    if Length(items) = count then
      if count = 0 then
           SetLength(items, 64       )
      else SetLength(items, count * 2);
    items[count].pid      := pid;
    items[count].sid      := sid;
    items[count].fileName := exeFile;
    items[count].path     := '';
    items[count].priority := -1;
    for i1 := Length(exeFile) downto 1 do
      if exeFile[i1] = '\' then begin
        items[count].path     := Copy(exeFile, 1, i1 - 1);
        items[count].fileName := Copy(exeFile, i1 + 1, maxInt);
        break;
      end;
    ph := OpenProcess(PROCESS_QUERY_INFORMATION, false, pid);
    if ph <> 0 then begin
      items[count].priority := integer(GetPriorityClass(ph));
      CloseHandle(ph);
    end;
    inc(count);
  end;

  procedure EnumProcesses9x;
  var c1 : cardinal;
      pe : TProcessEntry32;
  begin
    if @CreateToolhelp32Snapshot = nil then begin
      c1 := GetModuleHandle(kernel32);
      CreateToolhelp32Snapshot := GetProcAddress(c1, 'CreateToolhelp32Snapshot');
      Process32First           := GetProcAddress(c1, 'Process32First');
      Process32Next            := GetProcAddress(c1, 'Process32Next');
    end;
    c1 := CreateToolHelp32Snapshot(2, 0);
    if c1 <> INVALID_HANDLE_VALUE then begin
      pe.size := sizeOf(TProcessEntry32);
      if Process32First(c1, pe) then
        repeat
          AddItem(pe.exeFile, pe.process, 0);
        until not Process32Next(c1, pe);
      CloseHandle(c1);
    end;
  end;

  procedure EnumProcessesNt;
  var c1, c2 : cardinal;
      p1     : pointer;
      npi    : ^TNtProcessInfo;
      s1     : string;
      ph     : dword;
      arrCh  : array [0..MAX_PATH] of char;
  begin
    if @NtQuerySystemInformation = nil then
      NtQuerySystemInformation := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQuerySystemInformation');
    if @GetModuleFileNameExA = nil then
      GetModuleFileNameExA := GetProcAddress(LoadLibrary('psapi.dll'), 'GetModuleFileNameExA');
    c1 := 0;
    NtQuerySystemInformation(5, nil, 0, @c1);
    p1 := nil;
    if c1 = 0 then begin
      c1 := $10000;
      repeat
        c1 := c1 * 2;
        LocalFree(dword(p1));
        dword(p1) := LocalAlloc(LPTR, c1);
        c2 := NtQuerySystemInformation(5, p1, c1, nil);
      until (c2 = 0) or (c1 = $400000);
    end else begin
      c1 := c1 * 2;
      dword(p1) := LocalAlloc(LPTR, c1);
      c2 := NtQuerySystemInformation(5, p1, c1, nil);
    end;
    if c2 = 0 then begin
      npi := p1;
      while true do begin
        if npi^.name <> nil then begin
          s1 := npi^.name;
          if @GetModuleFileNameExA <> nil then begin
            ph := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, npi^.pid);
            if ph <> 0 then begin
              if GetModuleFileNameExA(ph, 0, arrCh, MAX_PATH) > 0 then begin
                s1 := arrCh;
                if PosTextIs1('\SystemRoot\System32\', s1) then begin
                  GetSystemDirectory(arrCh, MAX_PATH);
                  s1 := arrCh + Copy(s1, 21, maxInt);
                end else
                  if PosStrIs1('\??\', s1) then
                    Delete(s1, 1, 4)
              end;
              CloseHandle(ph);
            end;
          end;
          AddItem(s1, npi^.pid, npi^.sessionId);
        end else
          AddItem('Idle', npi^.pid, npi^.sessionId);
        if npi^.offset = 0 then
          break;
        dword(npi) := dword(npi) + npi^.offset;
      end;
    end;
    LocalFree(dword(p1));
  end;

  function PriorityClassToStr(priority: integer) : string;
  const ABOVE_NORMAL_PRIORITY_CLASS = $00008000;
        BELOW_NORMAL_PRIORITY_CLASS = $00004000;
  begin
    case priority of
      IDLE_PRIORITY_CLASS         : result := 'idle';
      BELOW_NORMAL_PRIORITY_CLASS : result := 'below normal';
      NORMAL_PRIORITY_CLASS       : result := 'normal';
      ABOVE_NORMAL_PRIORITY_CLASS : result := 'above normal';
      HIGH_PRIORITY_CLASS         : result := 'high';
      REALTIME_PRIORITY_CLASS     : result := 'realtime';
      -1, 0                       : result := '';
      else                          result := IntToHexEx(priority);
    end;
  end;

var pidLen, sidLen, priorityLen, fileNameLen : integer;
    i1 : integer;
    s1 : string;
begin
  result := '';
  count := 0;
  items := nil;
  if GetVersion and $80000000 = 0 then
       EnumProcessesNt
  else EnumProcesses9x;
  pidLen := 0;
  sidLen := 0;
  priorityLen := 0;
  fileNameLen := 0;
  for i1 := 0 to count - 1 do
    with items[i1] do begin
      if sid <> 0 then begin
        s1 := IntToStrEx(sid);
        if Length(s1) > sidLen then
          sidLen := Length(s1);
      end;
      s1 := IntToHexEx(pid);
      if Length(s1) > pidLen then
        pidLen := Length(s1);
      s1 := PriorityClassToStr(priority);
      if Length(s1) > priorityLen then
        priorityLen := Length(s1);
      if Length(fileName) > fileNameLen then
        fileNameLen := Length(fileName);
    end;
  for i1 := 0 to count - 1 do
    with items[i1] do begin
      result := result + #$D#$A + RetDelete(IntToHexEx(pid, pidLen - 1), 1, 1) + ' ' + FillStr(fileName, -fileNameLen - 1);
      if sidLen > 0 then
        result := result + IntToStrEx(sid, -sidLen - 1, ' ');
      result := result + FillStr(PriorityClassToStr(priority), -priorityLen - 1) + path;
    end;
  Delete(result, 1, 2);
end;

// ***************************************************************

initialization
  RegisterBugReportPlugin('processes', 'list of running processes', GetProcessList);
finalization
  UnregisterBugReportPlugin('processes');
end.
