unit GlobalType;

interface


const
  PAGE_NOACCESS                         = 1;
  PAGE_READONLY                         = 2;
  PAGE_READWRITE                        = 4;
  PAGE_WRITECOPY                        = 8;
  PAGE_EXECUTE                          = $10;
  PAGE_EXECUTE_READ                     = $20;
  PAGE_EXECUTE_READWRITE                = $40;
  PAGE_EXECUTE_WRITECOPY                = $80;
  PAGE_GUARD                            = $100;
  PAGE_NOCACHE                          = $200;
  MEM_COMMIT                            = $1000;
  MEM_RESERVE                           = $2000;
  MEM_DECOMMIT                          = $4000;
  MEM_RELEASE                           = $8000;
  MEM_FREE                              = $10000;
  MEM_PRIVATE                           = $20000;
  MEM_MAPPED                            = $40000;
  MEM_RESET                             = $80000;
  MEM_TOP_DOWN                          = $100000;

type
  BOOL = LongBool;
  DWORD = LongWord;
  LPCSTR = PAnsiChar;
  FARPROC = Pointer;
  THandle = LongWord;
  HINST = THandle;
  HMODULE = HINST;
  ULONG = Cardinal;

  PDWORD = ^DWORD;
  PInt64 = ^Int64;


  PListEntry = ^TListEntry;
  _LIST_ENTRY = record
    Flink : PListEntry;
    Blink : PListEntry;
  end;
  TListEntry = _LIST_ENTRY;

  LONGLONG = Int64;
  PLargeInteger = ^TLargeInteger;
  _LARGE_INTEGER = record
    case Integer of
      0 : (
        LowPart : DWORD;
        HighPart : Longint);
      1 : (
        QuadPart : LONGLONG);
  end;
  TLargeInteger = Int64;
  LARGE_INTEGER = _LARGE_INTEGER;

  ///////////////////////////////////////////////////////////////////////////
  ///
  ///  维护DLL信息的头
  ///
  //////////////////////////////////////////////////////////////////////////

  //---------------------------------------  节段信息

  LPSection = ^TSection;
  TSection = packed record
    Base : POINTER;
    RVA : LONGWORD;
    Size : LONGWORD;
    Characteristics : LONGWORD;
  end;

  PSections = ^TSections;
  TSections = array of TSection;

  //---------------------------------------  依赖的DLL库列表

  TNameOrID = (niName, niID);

  LPExternalLibrary = ^TExternalLibrary;
  TExternalLibrary = packed record
    LibraryName : array[0..99] of CHAR;
    LibraryHandle : HINST;
  end;

  PExternalLibrarys = ^TExternalLibrarys;
  TExternalLibrarys = array of TExternalLibrary;

  //---------------------------------------   输入表信息

  LPDLLFunctionImport = ^TDLLFunctionImport;
  TDLLFunctionImport = packed record
    NameOrID : TNameOrID;
    Name : PCHAR;
    ID : INTEGER;
  end;

  LPDLLFunctionImports = ^TDLLFunctionImports;
  TDLLFunctionImports = array of TDLLFunctionImport;

  LPDLLImport = ^TDLLImport;
  TDLLImport = packed record
    LibraryName : PCHAR;
    LibraryHandle : HINST;
    Entries : Pointer;                  //PSimArrayObject .> LPDLLFunctionImports
  end;

  PImports = ^TImports;
  TImports = array of TDLLImport;

  //---------------------------------------

  LPDLLFunctionExport = ^TDLLFunctionExport;
  TDLLFunctionExport = packed record
    Name : PCHAR;
    Index : INTEGER;
    FunctionPointer : POINTER;
  end;

  PExports = ^TExports;
  TExports = array of TDLLFunctionExport;

  //---------------------------------------

  TThreadStartRoutine = function(lpThreadParameter : LPDLLFunctionExport) : Integer stdcall;

  TDLLEntryProc = function(hinstDLL : HMODULE; dwReason : LONGWORD; lpvReserved : POINTER) : THandle; STDCALL;


  /////////////////////////////////////////////
  ///  线程信息块、进程信息块
  ////////////////////////////////////////////

type
  PClientId = ^TClientId;
  TClientId = record
    UniqueProcess : THandle;
    UniqueThread : THandle;
  end;

  PPebFreeBlock = ^TPebFreeBlock;
  _PEB_FREE_BLOCK = record
    Next : PPebFreeBlock;
    Size : Cardinal;
  end;
  TPebFreeBlock = _PEB_FREE_BLOCK;

  PNtAnsiString = ^TNtAnsiString;
  TNtAnsiString = packed record
    Length : Word;
    MaximumLength : Word;
    Buffer : PAnsiChar;
  end;

  PRtlDriveLetterCurDir = ^TRtlDriveLetterCurDir;
  _RTL_DRIVE_LETTER_CURDIR = packed record
    Flags : Word;
    Length : Word;
    TimeStamp : Cardinal;
    DosPath : TNtAnsiString;
  end;
  TRtlDriveLetterCurDir = _RTL_DRIVE_LETTER_CURDIR;

  PNtUnicodeString = ^TNtUnicodeString;
  TNtUnicodeString = packed record
    Length : Word;
    MaximumLength : Word;
    Buffer : PWideChar;
  end;

  PCurDir = ^TCurDir;
  _CURDIR = packed record
    DosPath : TNtUnicodeString;
    Handle : THandle;
  end;
  TCurDir = _CURDIR;

  PRtlUserProcessParameters = ^TRtlUserProcessParameters;
  _RTL_USER_PROCESS_PARAMETERS = record
    MaximumLength : Cardinal;
    Length : Cardinal;
    Flags : Cardinal;
    DebugFlags : Cardinal;
    ConsoleHandle : THandle;
    ConsoleFlags : Cardinal;
    StandardInput : THandle;
    StandardOutput : THandle;
    StandardError : THandle;
    CurrentDirectory : TCurDir;
    DllPath : TNtUnicodeString;
    ImagePathName : TNtUnicodeString;
    CommandLine : TNtUnicodeString;
    Environment : Pointer;
    StartingX : Cardinal;
    StartingY : Cardinal;
    CountX : Cardinal;
    CountY : Cardinal;
    CountCharsX : Cardinal;
    CountCharsY : Cardinal;
    FillAttribute : Cardinal;
    WindowFlags : Cardinal;
    ShowWindowFlags : Cardinal;
    WindowTitle : TNtUnicodeString;
    DesktopInfo : TNtUnicodeString;
    ShellInfo : TNtUnicodeString;
    RuntimeData : TNtUnicodeString;
    CurrentDirectores : array[0..31] of TRtlDriveLetterCurDir;
  end;
  TRtlUserProcessParameters = _RTL_USER_PROCESS_PARAMETERS;

  PPebLdrData = ^TPebLdrData;
  _PEB_LDR_DATA = packed record
    Length : Cardinal;
    Initialized : LongBool;
    SsHandle : THandle;
    InLoadOrderModuleList : TListEntry;
    InMemoryOrderModuleList : TListEntry;
    InInitializationOrderModuleList : TListEntry;
  end;
  TPebLdrData = _PEB_LDR_DATA;

  PPeb = ^TPeb;
  _PEB = packed record
    InheritedAddressSpace : Boolean;
    ReadImageFileExecOptions : Boolean;
    BeingDebugged : Boolean;
    SpareBool : Boolean;
    Mutant : Pointer;                   // THandle
    ImageBaseAddress : Pointer;
    Ldr : PPebLdrData;
    ProcessParameters : PRtlUserProcessParameters;
    SubSystemData : Pointer;
    ProcessHeap : Pointer;              // THandle
    FastPebLock : Pointer;
    FastPebLockRoutine : Pointer;
    FastPebUnlockRoutine : Pointer;
    EnvironmentUpdateCount : Cardinal;
    KernelCallbackTable : Pointer;
    case Integer of
      4 : (
        EventLogSection : Pointer;      // THandle
        EventLog : Pointer);            // THandle
      5 : (
        SystemReserved : array[0..1] of Cardinal;
  { end; }
        FreeList : PPebFreeBlock;
        TlsExpansionCounter : Cardinal;
        TlsBitmap : Pointer;
        TlsBitmapBits : array[0..1] of Cardinal;
        ReadOnlySharedMemoryBase : Pointer;
        ReadOnlySharedMemoryHeap : Pointer;
        ReadOnlyStaticServerData : ^Pointer;
        AnsiCodePageData : Pointer;
        OemCodePageData : Pointer;
        UnicodeCaseTableData : Pointer;
        NumberOfProcessors : Cardinal;
        NtGlobalFlag : Cardinal;
        Unknown : Cardinal;
        CriticalSectionTimeout : _LARGE_INTEGER;
        HeapSegmentReserve : Cardinal;
        HeapSegmentCommit : Cardinal;
        HeapDeCommitTotalFreeThreshold : Cardinal;
        HeapDeCommitFreeBlockThreshold : Cardinal;
        NumberOfHeaps : Cardinal;
        MaximumNumberOfHeaps : Cardinal;
        ProcessHeaps : ^Pointer;
        GdiSharedHandleTable : Pointer;
        ProcessStarterHelper : Pointer;
        GdiDCAttributeList : Cardinal;
        LoaderLock : Pointer;
        OSMajorVersion : Cardinal;
        OSMinorVersion : Cardinal;
        OSBuildNumber : Word;
        OSCSDVersion : Word;
        OSPlatformId : Cardinal;
        ImageSubsystem : Cardinal;
        ImageSubsystemMajorVersion : Cardinal;
        ImageSubsystemMinorVersion : Cardinal;
        ImageProcessAffinityMask : Cardinal;
        GdiHandleBuffer : array[0..33] of Cardinal;
    { Windows 2000 - begin }
        PostProcessInitRoutine : ^Pointer; // ^function
        TlsExpansionBitmap : Pointer;
        TlsExpansionBitmapBits : array[0..31] of Cardinal;
        SessionId : Cardinal;
        AppCompatInfo : Pointer;
        CSDVersion : TNtUnicodeString);
    { Windows 2000 - end }
  end;
  TPeb = _PEB;

  PNtTib = ^TNtTib;
  _NT_TIB = record
    ExceptionList : Pointer;            // ^_EXCEPTION_REGISTRATION_RECORD
    StackBase : Pointer;
    StackLimit : Pointer;
    SubSystemTib : Pointer;
    case Integer of
      0 : (FiberData : Pointer);
      1 : (Version : ULONG;
  { end; }
        ArbitraryUserPointer : Pointer;
        Self : PNtTib);
  end;
  TNtTib = _NT_TIB;

  PTeb = ^TTeb;
  _TEB = record
    Tib : TNtTib;
    Environment : PWideChar;
    ClientId : TClientId;
    RpcHandle : THandle;
    ThreadLocalStorage : Pointer;       // PPointer
    Peb : PPeb;
    LastErrorValue : DWORD;
  end;
  TTeb = _TEB;

  PLDR_MODULE = ^LDR_MODULE;
  LDR_MODULE = packed record
    InLoadOrderModuleList : TListEntry;
    InMemoryOrderModuleList : TListEntry;
    InInitializationOrderModuleList : TListEntry;
    BaseAddress : POINTER;
    EntryPoint : POINTER;
    SizeOfImage : LongWord;
    FullDllName : PWideChar;
    BaseDllName : PWideChar;
    Flags : LongWord;
    LoadCount : Word;
    TlsIndex : Word;
    HashTableEntry : TListEntry;
    TimeDateStamp : LongWord;
  end;


  //////////////////////////////////////////////////////////////////////////
  ///  函数重定位结构
  //////////////////////////////////////////////////////////////////////////

  LPRelocFunctionTable = ^TRelocFunctionTable;
  TRelocFunctionTable = packed record
    LoadLibrary : function(lpLibFileName : PChar) : HMODULE; stdcall;
    FreeLibrary : function(hLibModule : HMODULE) : BOOL; stdcall;
    GetProcAddress : function(hModule : HMODULE; lpProcName : LPCSTR) : FARPROC; stdcall;
    VirtualAlloc : function(lpvAddress : Pointer; dwSize, flAllocationType, flProtect : DWORD) : Pointer; stdcall;
    VirtualFree : function(lpAddress : Pointer; dwSize, dwFreeType : DWORD) : BOOL; stdcall;
    VirtualProtect : function(lpAddress : Pointer; dwSize, flNewProtect : DWORD; var OldProtect : DWORD) : BOOL; stdcall;
  end;

procedure FillChar(var Dest; count : Integer; Value : Char);
inline;
procedure CopyMemory(Dest, Source : Pointer; count : Integer);
inline;
function StrLCopy(Dest : PChar; const Source : PChar; MaxLen : Cardinal) : PChar;
inline;
function StrSame(Str1, Str2 : PChar) : LongBool;
inline;

implementation

procedure CopyMemory(Dest, Source : Pointer; count : Integer);
var
  S, D                                  : PChar;
  I                                     : Integer;
begin
  S := Source;
  D := Dest;
  if S = D then Exit;
  if Cardinal(D) > Cardinal(S) then
    for I := count - 1 downto 0 do
      D[I] := S[I]
  else
    for I := 0 to count - 1 do
      D[I] := S[I];
end;


procedure FillChar(var Dest; count : Integer; Value : Char);
inline;
var
  I   : Integer;
  P   : PChar;
begin
  P := PChar(@Dest);
  for I := count - 1 downto 0 do
    P[I] := Value;
end;


function StrSame(Str1, Str2 : PChar) : LongBool;
inline;
begin
  result := false;
  while Str1^ = Str2^ do
  begin
    if Str1^ = #0 then
    begin
      result := True;
      exit;
    end;
    Inc(Str1);
    Inc(Str2);
  end;
end;

function StrLCopy(Dest : PChar; const Source : PChar; MaxLen : Cardinal) : PChar;
inline;
var
  I                                     : INTEGER;
begin
  for I := 0 to MaxLen - 1 do
  begin
    Dest[I] := Source[I];
    if Source[I] = #0 then break;
  end;
  Result := Dest;
end;



end.

