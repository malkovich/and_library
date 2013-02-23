unit BuildinFunctionTree;

//{$DEFINE USE_MADCHOOK}
//{$DEFINE USE_MADSHELL}
//{$DEFINE USE_MADKERNEL}
//{$DEFINE USE_PROCESS_UNIT}

interface
uses
  SysUtils, Windows, TlHelp32, DbgLoger, ShellApi,
  {$IFDEF USE_MADCHOOK}
  madCHook,
  {$ENDIF}
  {$IFDEF USE_MADSHELL}
  madShell,
  {$ENDIF}
  {$IFDEF USE_MADKERNEL}
  madKernel,
  {$ENDIF}
  ShellWMI, WMIUnit,
  {$IFDEF USE_PROCESS_UNIT}
  ProcessUnit,
  {$ENDIF}
  HashTblUnit, TraceRt2, EchoIpcUnit,
  EnumDLLFuncs, DLLDataBase, ActiveX,
  RunAsUserUnit;

function GetLibraryEntry: Pointer; Stdcall;

procedure LoadWmiMachineInfo_Raw;
procedure LoadWmiMachineInfo_Shell;

implementation

function FormatTraceRouter(Host: PChar): PChar; Stdcall;
begin
  Result := FormatTraceRouter2 (Host);
end;

function _StrCopy(Dest: PChar; Source: PChar): PChar; Stdcall;
begin
  Result := SysUtils.StrCopy (Dest, Source);
end;

function _StrEnd(Str: PChar): PChar; stdcall;
begin
  Result := SysUtils.StrEnd (Str);
end;

function _StrLen(Str: PChar): Cardinal; stdcall;
begin
  Result := SysUtils.StrLen (Str);
end;

function _StrCat(Dest: PChar; Source: PChar): PChar; Stdcall;
begin
  Result := SysUtils.StrCat (Dest, Source);
end;

function _StrComp(Str1, Str2: PChar): Integer; Stdcall;
begin
  Result := SysUtils.StrComp (Str1, Str2);
end;

function __AllocMem (Size: LongWord): Pointer; Stdcall;
begin
  Result := AllocMem (Size);
end;

procedure __FreeMem (var Ptr: Pointer); Stdcall;
begin
  FreeMem (Ptr);
  Ptr := nil;
end;

function _GetRandomGUID (Buffer: PChar): Integer; Stdcall;
var
  GuidStr: string;
  GUID: TGUID;
begin
  CreateGuid (GUID);
  GuidStr := GuidToString (GUID);
  StrCopy (Buffer, PChar(GuidStr));
  Result := Length(GuidStr);
  Buffer[Result] := #0;
end;

function _ForceDirectories(Dir: PChar): LongBool; Stdcall;
begin
  Result := SysUtils.ForceDirectories(Dir);
end;

function _ExtractFilePath(FileName: PChar; OutFileNameBuffer: PChar): Integer; Stdcall;
var
  ResultName: string;
  ResultSize: Integer;
begin
  ResultName := SysUtils.ExtractFilePath(FileName);
  ResultSize := Length (ResultName);
  StrLCopy (OutFileNameBuffer, PChar(ResultName), ResultSize);
  OutFileNameBuffer[ResultSize] := #0;
  Result := ResultSize;
end;

function IsVista: boolean;
begin
   Result := not (Win32MajorVersion <6);
end;

{$IFDEF USE_MADKERNEL}

{$ENDIF}

{$IFDEF USE_MADSHELL}
function _GetSpecialFolder(sf: TSpecialFolder; PathBuffer: Pointer; MaxPathSize: Integer) : LongBool;
var
  Path: string;
  PathSize: Integer;
begin
  Result := GetSpecialFolder (sf, Path);
  PathSize := Length(Path);
  if PathSize > MaxPathSize then
  begin
    Result := False;
    Exit;
  end;
  CopyMemory (PathBuffer, @Path[1], PathSize);
end;
{$ENDIF}

{$IFDEF USE_MADCHOOK}
function _SendIpcMessage(ipc: pchar; messageBuf: pointer; messageLen: dword; answerBuf: pointer; answerLen: dword; answerTimeOut: dword; handleMessages: bool) : bool; stdcall;
begin
  Result := madCHook.SendIpcMessage (ipc, messageBuf, MessageLen, answerBuf, answerLen, answerTimeOut, handleMessages);
end;
{$ENDIF}
{$I HASH_FunctionDefine.inc}

procedure InitialLibrary;
begin

  {$IFDEF USE_PROCESS_UNIT}    
  SaveFuncEntry (HASH_MeLibrary, HASH_ProceIsStillValid, @ProceIsStillValid);
  SaveFuncEntry (HASH_MeLibrary, HASH_ProceIsValid, @ProceIsValid);
  SaveFuncEntry (HASH_MeLibrary, HASH_ProceIsListing, @ProceIsListing);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetModuleMD5, @GetModuleMD5);
  SaveFuncEntry (HASH_MeLibrary, HASH_ProceIDToName, @ProceIDToName);

  SaveFuncEntry (HASH_MeLibrary, HASH_EnumerateProcess, @EnumerateProcess);
  SaveFuncEntry (HASH_MeLibrary, HASH_EnumerateProceDllFuns, @EnumerateProceDllFuns);
  SaveFuncEntry (HASH_MeLibrary, HASH_EnumerateProceHandles, @EnumerateProceHandles);
  SaveFuncEntry (HASH_MeLibrary, HASH_EnumerateProceWindows, @EnumerateProceWindows);
  SaveFuncEntry (HASH_MeLibrary, HASH_EnumerateProceLocalFiles, @EnumerateProceLocalFiles);
  {$ENDIF}
  SaveFuncEntry (HASH_MeLibrary, HASH_HashTbl_Create, @HashTbl_Create);
  SaveFuncEntry (HASH_MeLibrary, HASH_HashTbl_Destroy, @HashTbl_Destroy);
  SaveFuncEntry (HASH_MeLibrary, HASH_HashTbl_Delete, @HashTbl_Delete);
  SaveFuncEntry (HASH_MeLibrary, HASH_HashTbl_Clear, @HashTbl_Clear);
  SaveFuncEntry (HASH_MeLibrary, HASH_HashTbl_Find, @HashTbl_Find);
  SaveFuncEntry (HASH_MeLibrary, HASH_HashTbl_Insert, @HashTbl_Insert);
  SaveFuncEntry (HASH_MeLibrary, HASH_HashTbl_Count, @HashTbl_Count);

  SaveFuncEntry (HASH_MeLibrary, HASH_DebugPrinter, @DebugPrinter);
  {$IFDEF USE_MADSHELL}
  SaveFuncEntry (HASH_MeLibrary, HASH_GetSpecialFolder, @_GetSpecialFolder);
  {$ENDIF}
  
  {$IFDEF USE_MADCHOOK}
  SaveFuncEntry (HASH_MeLibrary, HASH_SendIpcMessage, @_SendIpcMessage);
  {$ENDIF}

  SaveFuncEntry (HASH_MeLibrary, HASH_SendEchoMessage, @SendEchoMessage);
  SaveFuncEntry (HASH_MeLibrary, HASH_RunAsDefault, @RunAsDefault);

  SaveFuncEntry (HASH_MeLibrary, HASH_GetRandomGUID, @_GetRandomGUID);
  SaveFuncEntry (HASH_MeLibrary, HASH_ForceDirectories, @_ForceDirectories);
  SaveFuncEntry (HASH_MeLibrary, HASH_ExtractFilePath, @_ExtractFilePath);

  SaveFuncEntry (HASH_MeLibrary, HASH_StrEnd, @_StrEnd);
  SaveFuncEntry (HASH_MeLibrary, HASH_StrCat, @_StrCat);
  SaveFuncEntry (HASH_MeLibrary, HASH_StrCopy, @_StrCopy);
  SaveFuncEntry (HASH_MeLibrary, HASH_StrLen, @_StrLen);
  SaveFuncEntry (HASH_MeLibrary, HASH_StrComp, @_StrComp);

  SaveFuncEntry (HASH_MeLibrary, HASH_AllocMem, @__AllocMem );
  SaveFuncEntry (HASH_MeLibrary, HASH_FreeMem,  @__FreeMem );
  SaveFuncEntry (HASH_MeLibrary, HASH_FormatTraceRouter,  @FormatTraceRouter);

  SaveFuncEntry (HASH_MeLibrary, HASH_GetUpperNameHash,  @GetUpperNameHash);

end;

procedure LoadWmiMachineInfo_Shell;
begin
  SaveFuncEntry (HASH_MeLibrary, HASH_GetBaseBoard,  @ShellWMI.One_GetBaseBoard);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetSMBiosVersion,  @ShellWMI.One_GetSMBiosVersion);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetBiosName,  @ShellWMI.One_GetBiosName);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetBIOSSerialNumber,  @ShellWMI.One_GetBIOSSerialNumber);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetFirstDiskInfo,  @ShellWMI.One_GetFirstDiskInfo);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetProcessorInfo,  @ShellWMI.One_GetProcessorInfo);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetPhysicalMemoryInfo,  @ShellWMI.One_GetPhysicalMemoryInfo);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetOperatingSystem,  @ShellWMI.One_GetOperatingSystem);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetVideoCardName,  @ShellWMI.One_GetVideoCardName);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetSoundCardName,  @ShellWMI.One_GetSoundCardName);
end;

procedure LoadWmiMachineInfo_Raw;
begin
  CoInitialize (nil);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetBaseBoard,  @WMIUnit.GetBaseBoard);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetSMBiosVersion,  @WMIUnit.GetSMBiosVersion);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetBiosName,  @WMIUnit.GetBiosName);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetBIOSSerialNumber,  @WMIUnit.GetBIOSSerialNumber);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetFirstDiskInfo,  @WMIUnit.GetFirstDiskInfo);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetProcessorInfo,  @WMIUnit.GetProcessorInfo);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetPhysicalMemoryInfo,  @WMIUnit.GetPhysicalMemoryInfo);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetOperatingSystem,  @WMIUnit.GetOperatingSystem);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetVideoCardName,  @WMIUnit.GetVideoCardName);
  SaveFuncEntry (HASH_MeLibrary, HASH_GetSoundCardName,  @WMIUnit.GetSoundCardName);
end;

procedure EnumDLLRoutine (LibName: PChar; FuncName: PChar; FuncEntry: Pointer); stdcall;
begin
  SaveFuncEntry (LibName, FuncName, FuncEntry);
end;

procedure RegistDLLExports;
begin
  EnumDLLFunction (Loadlibrary (kernel32),    EnumDLLRoutine);
  EnumDLLFunction (Loadlibrary (user32),      EnumDLLRoutine);
  EnumDLLFunction (Loadlibrary ('ntdll.dll'), EnumDLLRoutine);
  EnumDLLFunction (Loadlibrary ('IPHLPAPI.DLL'), EnumDLLRoutine);
end;

var
  IsInitial: BOOL = false;

function GetLibraryEntry: Pointer; Stdcall;
begin
  Result := @FindFuncEntry;
  if IsInitial then exit;
  IsInitial := True;

  InitialLibrary;
  RegistDLLExports;  
end;

end.
