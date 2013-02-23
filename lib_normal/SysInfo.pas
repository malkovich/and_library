unit Unit15;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm15 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  public
    procedure ShowLockedProcess(FileName: String);
  end;

var
  Form15: TForm15;

implementation

{$R *.dfm}

type
  NT_STATUS = Cardinal;

  TFileDirectoryInformation = packed record
    NextEntryOffset: ULONG;
    FileIndex: ULONG;
    CreationTime: LARGE_INTEGER;
    LastAccessTime: LARGE_INTEGER;
    LastWriteTime: LARGE_INTEGER;
    ChangeTime: LARGE_INTEGER;
    EndOfFile: LARGE_INTEGER;
    AllocationSize: LARGE_INTEGER;
    FileAttributes: ULONG;
    FileNameLength: ULONG;
    FileName: array[0..0] of WideChar;
  end;
  FILE_DIRECTORY_INFORMATION = TFileDirectoryInformation;
  PFileDirectoryInformation = ^TFileDirectoryInformation;
  PFILE_DIRECTORY_INFORMATION = PFileDirectoryInformation;

  PSYSTEM_THREADS = ^SYSTEM_THREADS;
  SYSTEM_THREADS  = packed record
    KernelTime: LARGE_INTEGER;
    UserTime: LARGE_INTEGER;
    CreateTime: LARGE_INTEGER;
    WaitTime: ULONG;
    StartAddress: Pointer;
    UniqueProcess: DWORD;
    UniqueThread: DWORD;
    Priority: Integer;
    BasePriority: Integer;
    ContextSwitchCount: ULONG;
    State: Longint;
    WaitReason: Longint;
  end;

  PSYSTEM_PROCESS_INFORMATION = ^SYSTEM_PROCESS_INFORMATION;
  SYSTEM_PROCESS_INFORMATION = packed record
    NextOffset: ULONG;
    ThreadCount: ULONG;
    Reserved1: array [0..5] of ULONG; 
    CreateTime: FILETIME;
    UserTime: FILETIME;
    KernelTime: FILETIME;
    ModuleNameLength: WORD;
    ModuleNameMaxLength: WORD;
    ModuleName: PWideChar;
    BasePriority: ULONG;
    ProcessID: ULONG;
    InheritedFromUniqueProcessID: ULONG;
    HandleCount: ULONG;
    Reserved2 : array[0..1] of ULONG; 
    PeakVirtualSize : ULONG;
    VirtualSize : ULONG;
    PageFaultCount : ULONG;
    PeakWorkingSetSize : ULONG;
    WorkingSetSize : ULONG;
    QuotaPeakPagedPoolUsage : ULONG;
    QuotaPagedPoolUsage : ULONG;
    QuotaPeakNonPagedPoolUsage : ULONG;
    QuotaNonPagedPoolUsage : ULONG;
    PageFileUsage : ULONG;
    PeakPageFileUsage : ULONG;
    PrivatePageCount : ULONG;
    ReadOperationCount : LARGE_INTEGER;
    WriteOperationCount : LARGE_INTEGER;
    OtherOperationCount : LARGE_INTEGER;
    ReadTransferCount : LARGE_INTEGER;
    WriteTransferCount : LARGE_INTEGER;
    OtherTransferCount : LARGE_INTEGER;
    ThreadInfo: array [0..0] of SYSTEM_THREADS;
  end;

  PSYSTEM_HANDLE_INFORMATION = ^SYSTEM_HANDLE_INFORMATION;
  SYSTEM_HANDLE_INFORMATION = packed record
    ProcessId: DWORD;
    ObjectTypeNumber: Byte;
    Flags: Byte;
    Handle: Word;
    pObject: Pointer;
    GrantedAccess: DWORD;
  end;

  PSYSTEM_HANDLE_INFORMATION_EX = ^SYSTEM_HANDLE_INFORMATION_EX;
  SYSTEM_HANDLE_INFORMATION_EX = packed record
    NumberOfHandles: dword;
    Information: array [0..0] of SYSTEM_HANDLE_INFORMATION;
  end;

  PFILE_NAME_INFORMATION = ^FILE_NAME_INFORMATION;
  FILE_NAME_INFORMATION = packed record
    FileNameLength: ULONG;
    FileName: array [0..MAX_PATH - 1] of WideChar;
  end;

  PUNICODE_STRING = ^TUNICODE_STRING;
  TUNICODE_STRING = packed record
    Length : WORD;
    MaximumLength : WORD;
    Buffer : array [0..MAX_PATH - 1] of WideChar;
  end;

  POBJECT_NAME_INFORMATION = ^TOBJECT_NAME_INFORMATION;
  TOBJECT_NAME_INFORMATION = packed record
    Name : TUNICODE_STRING;
  end;

  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;
  IO_STATUS_BLOCK = packed record
    Status: NT_STATUS;
    Information: DWORD;
  end;

  PGetFileNameThreadParam = ^TGetFileNameThreadParam;
  TGetFileNameThreadParam = packed record
    hFile: THandle;
    Data: array [0..MAX_PATH - 1] of Char;
    Status: NT_STATUS;
  end;

const
  STATUS_SUCCESS = NT_STATUS($00000000);
  STATUS_INVALID_INFO_CLASS = NT_STATUS($C0000003);
  STATUS_INFO_LENGTH_MISMATCH = NT_STATUS($C0000004);
  STATUS_INVALID_DEVICE_REQUEST = NT_STATUS($C0000010);
  ObjectNameInformation = 1;
  FileDirectoryInformation = 1;
  FileNameInformation = 9;
  SystemProcessesAndThreadsInformation = 5;
  SystemHandleInformation = 16;

  function ZwQuerySystemInformation(ASystemInformationClass: DWORD;
    ASystemInformation: Pointer; ASystemInformationLength: DWORD;
    AReturnLength: PDWORD): NT_STATUS; stdcall; external 'ntdll.dll';

  function NtQueryInformationFile(FileHandle: THandle;
    IoStatusBlock: PIO_STATUS_BLOCK; FileInformation: Pointer;
    Length: DWORD; FileInformationClass: DWORD): NT_STATUS;
    stdcall; external 'ntdll.dll';

  function NtQueryObject(ObjectHandle: THandle;
    ObjectInformationClass: DWORD; ObjectInformation: Pointer;
    ObjectInformationLength: ULONG;
    ReturnLength: PDWORD): NT_STATUS; stdcall; external 'ntdll.dll';

  function GetLongPathNameA(lpszShortPath, lpszLongPath: PChar;
    cchBuffer: DWORD): DWORD; stdcall; external kernel32;

procedure TForm15.Button1Click(Sender: TObject);
begin
  ShowLockedProcess('');
end;

procedure TForm15.ShowLockedProcess(FileName: String);

  function GetInfoTable(ATableType: DWORD): Pointer;
  var
    dwSize: DWORD;
    pPtr: Pointer;
    ntStatus: NT_STATUS;
  begin
    Result := nil;
    dwSize := WORD(-1);
    GetMem(pPtr, dwSize);
    ntStatus := ZwQuerySystemInformation(ATableType, pPtr, dwSize, nil);
    while ntStatus = STATUS_INFO_LENGTH_MISMATCH do
    begin
      dwSize := dwSize * 2;
      ReallocMem(pPtr, dwSize);
      ntStatus := ZwQuerySystemInformation(ATableType, pPtr, dwSize, nil);
    end;
    if ntStatus = STATUS_SUCCESS then
      Result := pPtr
    else
      FreeMem(pPtr);
  end;

  function GetFileNameThread(lpParameters: Pointer): DWORD; stdcall;
  var
    FileNameInfo: FILE_NAME_INFORMATION;
    ObjectNameInfo: TOBJECT_NAME_INFORMATION;
    IoStatusBlock: IO_STATUS_BLOCK;
    pThreadParam: TGetFileNameThreadParam;
    dwReturn: DWORD;
  begin
    ZeroMemory(@FileNameInfo, SizeOf(FILE_NAME_INFORMATION));
    pThreadParam := PGetFileNameThreadParam(lpParameters)^;
    Result := NtQueryInformationFile(pThreadParam.hFile, @IoStatusBlock,
      @FileNameInfo, MAX_PATH * 2, FileNameInformation);
    if Result = STATUS_SUCCESS then
    begin
      Result := NtQueryObject(pThreadParam.hFile, ObjectNameInformation,
        @ObjectNameInfo, MAX_PATH * 2, @dwReturn);
      if Result = STATUS_SUCCESS then
      begin
        pThreadParam.Status := Result;
        WideCharToMultiByte(CP_ACP, 0,
          @ObjectNameInfo.Name.Buffer[ObjectNameInfo.Name.MaximumLength -
          ObjectNameInfo.Name.Length],
          ObjectNameInfo.Name.Length, @pThreadParam.Data[0],
          MAX_PATH, nil, nil);
      end
      else
      begin
        pThreadParam.Status := STATUS_SUCCESS;
        Result := STATUS_SUCCESS;
        WideCharToMultiByte(CP_ACP, 0,
          @FileNameInfo.FileName[0], IoStatusBlock.Information,
          @pThreadParam.Data[0],
          MAX_PATH, nil, nil);
      end;
    end;
    PGetFileNameThreadParam(lpParameters)^ := pThreadParam;
    ExitThread(Result);
  end;

  function GetFileNameFromHandle(hFile: THandle): String;
  var
    lpExitCode: DWORD;
    pThreadParam: TGetFileNameThreadParam;
    hThread: THandle;
  begin
    Result := '';
    ZeroMemory(@pThreadParam, SizeOf(TGetFileNameThreadParam));
    pThreadParam.hFile := hFile;
    hThread := CreateThread(nil, 0, @GetFileNameThread, @pThreadParam, 0, PDWORD(nil)^);
    if hThread <> 0 then
    try
      case WaitForSingleObject(hThread, 100) of
        WAIT_OBJECT_0:
        begin
          GetExitCodeThread(hThread, lpExitCode);
          if lpExitCode = STATUS_SUCCESS then
            Result := pThreadParam.Data;
        end;
        WAIT_TIMEOUT:
          TerminateThread(hThread, 0);
      end;
    finally
      CloseHandle(hThread);
    end;
  end;

  function SetDebugPriv: Boolean;
  var
    Token: THandle;
    tkp: TTokenPrivileges;
  begin
    Result := false;
    if OpenProcessToken(GetCurrentProcess,
      TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token) then
    begin
      if LookupPrivilegeValue(nil, PChar('SeDebugPrivilege'),
        tkp.Privileges[0].Luid) then
      begin
        tkp.PrivilegeCount := 1;
        tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        Result := AdjustTokenPrivileges(Token, False,
          tkp, 0, PTokenPrivileges(nil)^, PDWord(nil)^);
      end;
    end;
  end;

type
  DriveQueryData = record
    DiskLabel: String;
    DiskDosQuery: String;
    DosQueryLen: Integer;
  end;

var
  hFile, hProcess: THandle;
  pHandleInfo: PSYSTEM_HANDLE_INFORMATION_EX;
  I, Drive: Integer;
  ObjectTypeNumber: Byte;
  FileDirectory, FilePath, ProcessName: String;
  SystemInformation, TempSI: PSYSTEM_PROCESS_INFORMATION;
  DosDevices: array [0..25] of DriveQueryData;
  LongFileName, TmpFileName: String;
begin
  SetLength(LongFileName, MAX_PATH);
  GetLongPathNameA(PChar(FileName), @LongFileName[1], MAX_PATH);

  for Drive := 0 to 25 do
  begin
    DosDevices[Drive].DiskLabel := Chr(Drive + Ord('a')) + ':';
    SetLength(DosDevices[Drive].DiskDosQuery, MAXCHAR);
    ZeroMemory(@DosDevices[Drive].DiskDosQuery[1], MAXCHAR);
    QueryDosDevice(PChar(DosDevices[Drive].DiskLabel),
      @DosDevices[Drive].DiskDosQuery[1], MAXCHAR);
    DosDevices[Drive].DosQueryLen := Length(PChar(DosDevices[Drive].DiskDosQuery));
    SetLength(DosDevices[Drive].DiskDosQuery, DosDevices[Drive].DosQueryLen);
  end;

  ObjectTypeNumber := 0;
  SetDebugPriv;
  hFile := CreateFile('NUL', GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
  if hFile = INVALID_HANDLE_VALUE then RaiseLastOSError;
  try
    pHandleInfo := GetInfoTable(SystemHandleInformation);
    if pHandleInfo = nil then RaiseLastOSError;
    try
      for I := 0 to pHandleInfo^.NumberOfHandles - 1 do
        if pHandleInfo^.Information[I].Handle = hFile then
          if pHandleInfo^.Information[I].ProcessId = GetCurrentProcessId then
          begin
            ObjectTypeNumber := pHandleInfo^.Information[I].ObjectTypeNumber;
            Break;
          end;
    finally
      FreeMem(pHandleInfo);
    end;
  finally
    CloseHandle(hFile);
  end;

  SystemInformation := GetInfoTable(SystemProcessesAndThreadsInformation);
  if SystemInformation <> nil then
  try
    pHandleInfo := GetInfoTable(SystemHandleInformation);
    if pHandleInfo <> nil then
    try
      ProgressBar1.Position := 0;
      ProgressBar1.Max := pHandleInfo^.NumberOfHandles;
      for I := 0 to pHandleInfo^.NumberOfHandles - 1 do
      begin
        if pHandleInfo^.Information[I].ObjectTypeNumber = ObjectTypeNumber then
        begin
          hProcess := OpenProcess(PROCESS_DUP_HANDLE, True,
            pHandleInfo^.Information[I].ProcessId);
          if hProcess > 0 then
          try
            if DuplicateHandle(hProcess, pHandleInfo^.Information[I].Handle,
              GetCurrentProcess, @hFile, 0, False, DUPLICATE_SAME_ACCESS) then
            try
              if Application.Terminated then Exit;
              
              FilePath := GetFileNameFromHandle(hFile);
              if FilePath <> '' then
              begin
                FileDirectory := '';
                for Drive := 0 to 25 do
                  if DosDevices[Drive].DosQueryLen > 0 then
                    if Copy(FilePath, 1, DosDevices[Drive].DosQueryLen) =
                      DosDevices[Drive].DiskDosQuery then
                    begin
                      FileDirectory := DosDevices[Drive].DiskLabel;
                      Delete(FilePath, 1, DosDevices[Drive].DosQueryLen);
                      Break;
                    end;

                if FileDirectory = '' then Continue;    

                TempSI := SystemInformation;
                repeat
                  if TempSI^.ProcessID =
                    pHandleInfo^.Information[I].ProcessId then
                  begin
                    ProcessName := TempSI^.ModuleName;
                    Break;
                  end;
                  TempSI := Pointer(DWORD(TempSI) + TempSI^.NextOffset);
                until TempSI^.NextOffset = 0;

                SetLength(TmpFileName, MAX_PATH);
                GetLongPathNameA(PChar(FileDirectory + FilePath), @TmpFileName[1], MAX_PATH);
                Memo1.Lines.Add(ProcessName + ': ' + TmpFileName);  
              end;
            finally
              CloseHandle(hFile);
            end;
          finally
            CloseHandle(hProcess);
          end;
        end;
        ProgressBar1.Position := ProgressBar1.Position + 1;
        Application.ProcessMessages;
      end;
    finally
      FreeMem(pHandleInfo);
    end;
  finally
    FreeMem(SystemInformation);
  end;
  ShowMessage('All handles found.');
end;

end.

