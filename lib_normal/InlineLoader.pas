unit InlineLoader;

interface
uses windows, classes, SysUtils;


function DLLLoader_Load(Buffer : Pointer; Len : LongInt; lpResersed : Pointer) : Integer; inline;
procedure DLLLoader_Load_End;

type
  ////////////////////////////
  ///  函数重定位结构
  LPRelocFunctionTable = ^TRelocFunctionTable;
  TRelocFunctionTable = packed record
    LoadLibrary      :function (lpLibFileName: PChar): LongWord; stdcall;
    FreeLibrary      :function (hLibModule: LongWord): LongBool; stdcall;
    GetProcAddress   :function (hModule: LongWord; lpProcName: PChar): POINTER; stdcall;
    VirtualAlloc     :function (lpvAddress: Pointer; dwSize, flAllocationType, flProtect: LongWord): Pointer; stdcall;
    VirtualFree      :function (lpAddress: Pointer; dwSize, dwFreeType: LongWord): LongBool; stdcall;
    VirtualProtect   :function (lpAddress: Pointer; dwSize, flNewProtect: LongWord; var OldProtect: LongWord): LongBool; stdcall;
  end;

//////////////////////////////////////////////////////////////////////////////
///
///            PEHead
///
//////////////////////////////////////////////////////////////////////////////

const
  IMAGE_REL_BASED_ABSOLUTE              = 0;
  IMAGE_REL_BASED_HIGH                  = 1;
  IMAGE_REL_BASED_LOW                   = 2;
  IMAGE_REL_BASED_HIGHLOW               = 3;
  IMAGE_REL_BASED_HIGHADJ               = 4;
  IMAGE_REL_BASED_MIPS_JMPADDR          = 5;
  IMAGE_REL_BASED_SECTION               = 6;
  IMAGE_REL_BASED_REL32                 = 7;

  IMAGE_REL_BASED_MIPS_JMPADDR16        = 9;
  IMAGE_REL_BASED_IA64_IMM64            = 9;
  IMAGE_REL_BASED_DIR64                 = 10;
  IMAGE_REL_BASED_HIGH3ADJ              = 11;

type

  PWordArray = ^TWordArray;
  TWordArray = array[0..(2147483647 div SIZEOF(WORD)) - 1] of WORD;

  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..(2147483647 div SIZEOF(LONGWORD)) - 1] of LONGWORD;

  PImageDOSHeader = ^TImageDOSHeader;
  TImageDOSHeader = packed record
    Signature : WORD;
    PartPag : WORD;
    PageCnt : WORD;
    ReloCnt : WORD;
    HdrSize : WORD;
    MinMem : WORD;
    MaxMem : WORD;
    ReloSS : WORD;
    ExeSP : WORD;
    ChkSum : WORD;
    ExeIP : WORD;
    ReloCS : WORD;
    TablOff : WORD;
    Overlay : WORD;
    Reserved : packed array[0..3] of WORD;
    OEMID : WORD;
    OEMInfo : WORD;
    Reserved2 : packed array[0..9] of WORD;
    LFAOffset : LONGWORD;
  end;

  PImageSectionHeaders = ^TImageSectionHeaders;
  TImageSectionHeaders = array[0..(2147483647 div SIZEOF(TImageSectionHeader)) - 1] of TImageSectionHeader;


  PImageBaseRelocation = ^TImageBaseRelocation;
  TImageBaseRelocation = packed record
    VirtualAddress : LONGWORD;
    SizeOfBlock : LONGWORD;
  end;
                    

  /////////////////////////////////////////////
  ///  线程信息块、进程信息块
  ////////////////////////////////////////////

type
  PClientId = ^TClientId;
  TClientId = record
    UniqueProcess: LongWord;
    UniqueThread: LongWord;
  end;

  PPebFreeBlock = ^TPebFreeBlock;
  _PEB_FREE_BLOCK = record
    Next: PPebFreeBlock;
    Size: Cardinal;
  end;
  TPebFreeBlock = _PEB_FREE_BLOCK;

  PNtAnsiString = ^TNtAnsiString;
  TNtAnsiString = packed record
    Length: Word;
    MaximumLength: Word;
    Buffer: PAnsiChar;
  end;

  PRtlDriveLetterCurDir = ^TRtlDriveLetterCurDir;
  _RTL_DRIVE_LETTER_CURDIR = packed record
    Flags    : Word;
    Length   : Word;
    TimeStamp: Cardinal;
    DosPath  : TNtAnsiString;
  end;
  TRtlDriveLetterCurDir = _RTL_DRIVE_LETTER_CURDIR;

  PNtUnicodeString = ^TNtUnicodeString;
  TNtUnicodeString = packed record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWideChar;
  end;

  PCurDir = ^TCurDir;
  _CURDIR = packed record
    DosPath: TNtUnicodeString;
    Handle : LongWord;
  end;
  TCurDir = _CURDIR;

  PRtlUserProcessParameters = ^TRtlUserProcessParameters;
  _RTL_USER_PROCESS_PARAMETERS = record
    MaximumLength    : Cardinal;
    Length           : Cardinal;
    Flags            : Cardinal;
    DebugFlags       : Cardinal;
    ConsoleHandle    : LongWord;
    ConsoleFlags     : Cardinal;
    StandardInput    : LongWord;
    StandardOutput   : LongWord;
    StandardError    : LongWord;
    CurrentDirectory : TCurDir;
    DllPath          : TNtUnicodeString;
    ImagePathName    : TNtUnicodeString;
    CommandLine      : TNtUnicodeString;
    Environment      : Pointer;
    StartingX        : Cardinal;
    StartingY        : Cardinal;
    CountX           : Cardinal;
    CountY           : Cardinal;
    CountCharsX      : Cardinal;
    CountCharsY      : Cardinal;
    FillAttribute    : Cardinal;
    WindowFlags      : Cardinal;
    ShowWindowFlags  : Cardinal;
    WindowTitle      : TNtUnicodeString;
    DesktopInfo      : TNtUnicodeString;
    ShellInfo        : TNtUnicodeString;
    RuntimeData      : TNtUnicodeString;
    CurrentDirectores: array [0..31] of TRtlDriveLetterCurDir;
  end;
  TRtlUserProcessParameters = _RTL_USER_PROCESS_PARAMETERS;

  PPebLdrData = ^TPebLdrData;
  _PEB_LDR_DATA = packed record
    Length                         : Cardinal;
    Initialized                    : LongBool;
    SsHandle                       : LongWord;
    InLoadOrderModuleList          : TListEntry;
    InMemoryOrderModuleList        : TListEntry;
    InInitializationOrderModuleList: TListEntry;
  end;
  TPebLdrData = _PEB_LDR_DATA;

  PPeb = ^TPeb;
  _PEB = packed record
    InheritedAddressSpace         : Boolean;
    ReadImageFileExecOptions      : Boolean;
    BeingDebugged                 : Boolean;
    SpareBool                     : Boolean;
    Mutant                        : Pointer;  // LongWord
    ImageBaseAddress              : Pointer;
    Ldr                           : PPebLdrData;
    ProcessParameters             : PRtlUserProcessParameters;
    SubSystemData                 : Pointer;
    ProcessHeap                   : Pointer;  // LongWord
    FastPebLock                   : Pointer;
    FastPebLockRoutine            : Pointer;
    FastPebUnlockRoutine          : Pointer;
    EnvironmentUpdateCount        : Cardinal;
    KernelCallbackTable           : Pointer;
    case Integer of
      4: (
        EventLogSection           : Pointer;   // LongWord
        EventLog                  : Pointer);  // LongWord
      5: (
        SystemReserved            : array [0..1] of Cardinal;
  { end; }
    FreeList                      : PPebFreeBlock;
    TlsExpansionCounter           : Cardinal;
    TlsBitmap                     : Pointer;
    TlsBitmapBits                 : array [0..1] of Cardinal;
    ReadOnlySharedMemoryBase      : Pointer;
    ReadOnlySharedMemoryHeap      : Pointer;
    ReadOnlyStaticServerData      : ^Pointer;
    AnsiCodePageData              : Pointer;
    OemCodePageData               : Pointer;
    UnicodeCaseTableData          : Pointer;
    NumberOfProcessors            : Cardinal;
    NtGlobalFlag                  : Cardinal;
    Unknown                       : Cardinal;
    CriticalSectionTimeout        : _LARGE_INTEGER;
    HeapSegmentReserve            : Cardinal;
    HeapSegmentCommit             : Cardinal;
    HeapDeCommitTotalFreeThreshold: Cardinal;
    HeapDeCommitFreeBlockThreshold: Cardinal;
    NumberOfHeaps                 : Cardinal;
    MaximumNumberOfHeaps          : Cardinal;
    ProcessHeaps                  : ^Pointer;
    GdiSharedHandleTable          : Pointer;
    ProcessStarterHelper          : Pointer;
    GdiDCAttributeList            : Cardinal;
    LoaderLock                    : Pointer;
    OSMajorVersion                : Cardinal;
    OSMinorVersion                : Cardinal;
    OSBuildNumber                 : Word;
    OSCSDVersion                  : Word;
    OSPlatformId                  : Cardinal;
    ImageSubsystem                : Cardinal;
    ImageSubsystemMajorVersion    : Cardinal;
    ImageSubsystemMinorVersion    : Cardinal;
    ImageProcessAffinityMask      : Cardinal;
    GdiHandleBuffer               : array [0..33] of Cardinal;
    { Windows 2000 - begin }
    PostProcessInitRoutine        : ^Pointer;  // ^function
    TlsExpansionBitmap            : Pointer;
    TlsExpansionBitmapBits        : array [0..31] of Cardinal;
    SessionId                     : Cardinal;
    AppCompatInfo                 : Pointer;
    CSDVersion                    : TNtUnicodeString);
    { Windows 2000 - end }
  end;
  TPeb = _PEB;


type
  PStreamData = ^TStreamData;
  TStreamData = record
    RelocFnTable :LPRelocFunctionTable;
    Memory:Pointer;
    Size :Longint;
    Position :Longint;
    Capacity:LongInt;   
  end;

Type
  PDLLLoadedObject = ^TDLLLoadedObject;
  TDLLLoadedObject = packed record
    RelocFnTable : TRelocFunctionTable;
    ImageBase : POINTER;
    DLLProc : FUNCTION(DllHandle:LongWord;dwReason:LONGWORD;lpvReserved:POINTER):INTEGER; STDCALL;
  end;

implementation


procedure CopyMemory(Dest, Source:Pointer; count : Integer); inline;
var
  S, D: PChar;
  I: Integer;
begin
  S := Source;
  D := Dest;
  if S = D then Exit;
  if Cardinal(D) > Cardinal(S) then
    for I := count-1 downto 0 do
      D[I] := S[I]
  else
    for I := 0 to count-1 do
      D[I] := S[I];
end;



function MemStream_Seek(This:PStreamData; Offset: Longint; Origin: Word): Longint; inline;
begin
  case Origin of
    soFromBeginning: This.Position := Offset;
    soFromCurrent: Inc(This.Position, Offset);
    soFromEnd: This.Position := This.Size + Offset;
  end;
  Result := This.Position;
end;

function MemStream_Read(This:PStreamData; var Buffer; Count: Longint): Longint; inline;
begin
  if (This.Position >= 0) and (Count >= 0) then
  begin
    Result := This.Size - This.Position;
    if Result > 0 then
    begin
      if Result > Count then Result := Count;
      CopyMemory(@Buffer, Pointer(Longint(This.Memory) + This.Position), Result);
      Inc(This.Position, Result);
      Exit;
    end;
  end;
  Result := 0;
end;

function MemStream_Write(This:PStreamData; const Buffer; Count: Longint): Longint; inline;
var
  Pos: Longint;
begin
  if (This.Position >= 0) and (Count >= 0) then
  begin
    Pos := This.Position + Count;
    if Pos > 0 then
    begin
      if Pos > This.Size then
      begin
        result := 0;
        exit;
      end;
      CopyMemory(Pointer(Longint(This.Memory) + This.Position), @Buffer, Count);
      This.Position := Pos;
      Result := Count;
      Exit;
    end;
  end;
  Result := 0;
end;

function MemStream_Create(RelocFnTable :LPRelocFunctionTable):PStreamData; inline;
begin
  Result := RelocFnTable.VirtualAlloc(nil,  SizeOf(TStreamData), MEM_COMMIT,PAGE_READWRITE);
  Result.Memory := nil;
  Result.Size := 0;
  Result.Position := 0;
  Result.RelocFnTable := RelocFnTable;
end;
procedure MemStream_Destroy(This:PStreamData); inline;
begin
  This.RelocFnTable.VirtualFree(This,0,MEM_FREE);
end;

//////////////////////////////////////////////////////////////////////////////
///
///            RelocFuncUnit
///
//////////////////////////////////////////////////////////////////////////////

function GetPEB_asm:Pointer;
asm
  mov eax,fs:[$30]      // 648B0530000000C3
end;


function GetPEB:Pointer; inline;
var
  Buffer:array[0..7] of byte;
  I:Integer;
  pFnx : function():Pointer;
begin
  I := 0;
  Buffer[I] := $64; inc(I);
  Buffer[I] := $8B; inc(I);
  Buffer[I] := $05; inc(I);
  Buffer[I] := $30; inc(I);
  Buffer[I] := $00; inc(I);
  Buffer[I] := $00; inc(I);
  Buffer[I] := $00; inc(I);
  Buffer[I] := $C3;

  pFnx := @Buffer[0];
  Result := pFnx;
  for I := 0 to 7 do Buffer[I] := 0;
end;

function GetKernel32Base_PEB:Pointer; inline;
var
  m_Peb   :PPeb;
  m_Ldr   : PPebLdrData;
  ListEntry :PListEntry;
  BaseAddress :PPointer;
begin
  m_Peb := GetPEB;
  m_Ldr := m_Peb.Ldr;
  ListEntry := m_Ldr.InInitializationOrderModuleList.Flink.Flink;
  BaseAddress := POINTER ( LongWord(ListEntry) + SizeOf(TListEntry));
  result := BaseAddress^;
end;

function GetSEH_asm:Pointer;
asm
  mov eax,fs:[0]      // 648B0500000000C3
end;

function GetSEH:Pointer; inline;
var
  Buffer:array[0..7] of byte;
  I:Integer;
  pFnx : function():Pointer;
begin
  I := 0;
  Buffer[I] := $64; inc(I);
  Buffer[I] := $8B; inc(I);
  Buffer[I] := $05; inc(I);
  Buffer[I] := $00; inc(I);
  Buffer[I] := $00; inc(I);
  Buffer[I] := $00; inc(I);
  Buffer[I] := $00; inc(I);
  Buffer[I] := $C3;

  pFnx := @Buffer[0];
  Result := pFnx();
  for I := 0 to 7 do Buffer[I] := 0;
end;


function GetKernel32FunctionAddress:Pointer; inline;
var
  pException :Pointer;
begin
  pException :=GetSEH;
  repeat
    result := PPointer(LongWord(pException) + SizeOf(Pointer))^;
    pException := PPointer(pException)^;
  until pException = Pointer($FFFFFFFF);
end;

function GetKernel32Base_SEH:Pointer; inline;

var
  lpUnhandle:Pointer;
  dwBase :LongWord;
  ImageDosHeader       : PImageDosHeader;
  ImageNTHeaders       : PImageNTHeaders;
  pSignature           : Pchar;
begin
  lpUnhandle := GetKernel32FunctionAddress;
  dwBase := LongWord(lpUnhandle);
  repeat
    dwBase := dwBase  and $FFFF0000;
    ImageDosHeader:=Pointer(dwBase);

    pSignature := @ImageDosHeader.Signature;
    if pSignature[0] = 'M' then
    if pSignature[1] = 'Z' then
    begin
      ImageNTHeaders:=Pointer(dwBase+ImageDosHeader.LFAOffset);
      pSignature := @ImageNTHeaders.Signature;
      if pSignature[0] = 'P' then
      if pSignature[1] = 'E' then
      begin
        result := ImageDosHeader;
        exit;
      end;
    end;
    dec(dwBase);
  until dwBase < $00450000;
  result := nil;
end;

function IsGetProcAddressStr(Source :Pchar):LongBool; inline;
var
  index :integer;
begin
  result := false;          
  index := 0;
  if Not (Source[index] = 'G') then exit; inc(index);
  if Not (Source[index] = 'e') then exit; inc(index);
  if Not (Source[index] = 't') then exit; inc(index);
  if Not (Source[index] = 'P') then exit; inc(index);
  if Not (Source[index] = 'r') then exit; inc(index);
  if Not (Source[index] = 'o') then exit; inc(index);
  if Not (Source[index] = 'c') then exit; inc(index);
  if Not (Source[index] = 'A') then exit; inc(index);
  if Not (Source[index] = 'd') then exit; inc(index);
  if Not (Source[index] = 'd') then exit; inc(index);
  if Not (Source[index] = 'r') then exit; inc(index);
  if Not (Source[index] = 'e') then exit; inc(index);
  if Not (Source[index] = 's') then exit; inc(index);
  if Not (Source[index] = 's') then exit; inc(index);
  if NOt (Source[index] = #0) then exit;
  result := true;
end;

FUNCTION GetPrimeFunction(Kernel32Module:Cardinal; var GetProcAddress, LoadlibraryA:Pointer) : LongBool; stdcall; inline;
VAR
  ExportName           : pChar;
  Address              : Cardinal;
  J                    : Cardinal;
  ImageDosHeader       : PImageDosHeader;
  ImageNTHeaders       : PImageNTHeaders;
  ImageExportDirectory : PImageExportDirectory;
  LoadlibraryAStr      : array[0..12] of char;
  fnGetProcAddress     : function (hModule: LongWord; lpProcName: PChar): POINTER; stdcall;
BEGIN
  result := false;
  GetProcAddress := nil;
  LoadlibraryA := nil;
  ImageDosHeader:=Pointer(Kernel32Module);
  ImageNTHeaders:=Pointer(Kernel32Module+ImageDosHeader.LFAOffset);
  ImageExportDirectory:=Pointer(ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress+Kernel32Module);
  J:=0;
  Address:=0;
  REPEAT
    ExportName:=Pointer(Cardinal(Pointer(Cardinal(ImageExportDirectory.AddressOfNames)+Kernel32Module+J*4)^)+Kernel32Module);

    if IsGetProcAddressStr(ExportName) then
    BEGIN
      Address:=Cardinal(Pointer(Word(Pointer(J SHL 1+Cardinal(
               ImageExportDirectory.AddressOfNameOrdinals)+Kernel32Module)^) AND
               $0000FFFF SHL 2+Cardinal(ImageExportDirectory.AddressOfFunctions)
               +Kernel32Module)^)+Kernel32Module;
    END;
    Inc(J);
  UNTIL (Address<>0)OR(J=ImageExportDirectory.NumberOfNames);

  if Address = 0 then exit;
  GetProcAddress := Pointer(Address);

  J := 0;
  LoadlibraryAStr[J] := 'L'; inc(J);
  LoadlibraryAStr[J] := 'o'; inc(J);
  LoadlibraryAStr[J] := 'a'; inc(J);
  LoadlibraryAStr[J] := 'd'; inc(J);
  LoadlibraryAStr[J] := 'L'; inc(J);
  LoadlibraryAStr[J] := 'i'; inc(J);
  LoadlibraryAStr[J] := 'b'; inc(J);
  LoadlibraryAStr[J] := 'r'; inc(J);
  LoadlibraryAStr[J] := 'a'; inc(J);
  LoadlibraryAStr[J] := 'r'; inc(J);
  LoadlibraryAStr[J] := 'y'; inc(J);
  LoadlibraryAStr[J] := 'A'; inc(J);
  LoadlibraryAStr[J] := #0;

  fnGetProcAddress := GetProcAddress;
  LoadlibraryA := fnGetProcAddress(Kernel32Module, LoadlibraryAStr);

  Result:= Assigned(LoadlibraryA);
END;


function MakeFunctinTable(var RelocFunTable:TRelocFunctionTable):LongBool; inline;
var
  Loadlibrary   :function (lpLibFileName: PChar): LongWord; stdcall;
  GetProcAddress:function (hModule: LongWord; lpProcName: PChar): POINTER; stdcall;
  Buffer :array[0..15] of char;
  I :Integer;
  hKernel32 :LongWord;
begin
  Result := False;
  hKernel32 := LongWord(GetKernel32Base_SEH());
  if not GetPrimeFunction(hKernel32, @GetProcAddress, @Loadlibrary) then exit;

  RelocFunTable.GetProcAddress := @GetProcAddress;
  RelocFunTable.Loadlibrary := @Loadlibrary;

  I := 0;
  Buffer[I] := 'F'; Inc(I);
  Buffer[I] := 'r'; Inc(I);
  Buffer[I] := 'e'; Inc(I);
  Buffer[I] := 'e'; Inc(I);
  Buffer[I] := 'L'; Inc(I);
  Buffer[I] := 'i'; Inc(I);
  Buffer[I] := 'b'; Inc(I);
  Buffer[I] := 'r'; Inc(I);
  Buffer[I] := 'a'; Inc(I);
  Buffer[I] := 'r'; Inc(I);
  Buffer[I] := 'y'; Inc(I);
  Buffer[I] := #0;

  RelocFunTable.FreeLibrary := GetProcAddress(hKernel32, Buffer);
  if not Assigned(RelocFunTable.FreeLibrary) then exit;

  I := 0;
  Buffer[I] := 'V'; Inc(I);
  Buffer[I] := 'i'; Inc(I);
  Buffer[I] := 'r'; Inc(I);
  Buffer[I] := 't'; Inc(I);
  Buffer[I] := 'u'; Inc(I);
  Buffer[I] := 'a'; Inc(I);
  Buffer[I] := 'l'; Inc(I);
  Buffer[I] := 'A'; Inc(I);
  Buffer[I] := 'l'; Inc(I);
  Buffer[I] := 'l'; Inc(I);
  Buffer[I] := 'o'; Inc(I);
  Buffer[I] := 'c'; Inc(I);
  Buffer[I] := #0;

  RelocFunTable.VirtualAlloc := GetProcAddress(hKernel32, Buffer);
  if not Assigned(RelocFunTable.VirtualAlloc) then exit;

  I := 0;
  Buffer[I] := 'V'; Inc(I);
  Buffer[I] := 'i'; Inc(I);
  Buffer[I] := 'r'; Inc(I);
  Buffer[I] := 't'; Inc(I);
  Buffer[I] := 'u'; Inc(I);
  Buffer[I] := 'a'; Inc(I);
  Buffer[I] := 'l'; Inc(I);
  Buffer[I] := 'F'; Inc(I);
  Buffer[I] := 'r'; Inc(I);
  Buffer[I] := 'e'; Inc(I);
  Buffer[I] := 'e'; Inc(I);
  Buffer[I] := #0;

  RelocFunTable.VirtualFree := GetProcAddress(hKernel32, Buffer);
  if not Assigned(RelocFunTable.VirtualFree) then exit;

  I := 0;
  Buffer[I] := 'V'; Inc(I);
  Buffer[I] := 'i'; Inc(I);
  Buffer[I] := 'r'; Inc(I);
  Buffer[I] := 't'; Inc(I);
  Buffer[I] := 'u'; Inc(I);
  Buffer[I] := 'a'; Inc(I);
  Buffer[I] := 'l'; Inc(I);
  Buffer[I] := 'P'; Inc(I);
  Buffer[I] := 'r'; Inc(I);
  Buffer[I] := 'o'; Inc(I);
  Buffer[I] := 't'; Inc(I);
  Buffer[I] := 'e'; Inc(I);
  Buffer[I] := 'c'; Inc(I);
  Buffer[I] := 't'; Inc(I);
  Buffer[I] := #0;

  RelocFunTable.VirtualProtect := GetProcAddress(hKernel32, Buffer);
  if not Assigned(RelocFunTable.VirtualProtect) then exit;

  Result := True;
end;

procedure MakeFunctinTable_end_sign;
begin
end;


//////////////////////////////////////////////////////////////////////////////
///
///            DLL Loader
///
//////////////////////////////////////////////////////////////////////////////


function GetAllocMemFlags(Characteristics : LongWord) : LongWord;inline;
var
  Flags                                 : LongWord;
begin
  Flags := 0;
  if (Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0 then
  begin

    if (Characteristics and IMAGE_SCN_MEM_READ) <> 0 then
    begin
      if (Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
        Flags := Flags or PAGE_EXECUTE_READWRITE
      else
        Flags := Flags or PAGE_EXECUTE_READ;
    end
    else
      if (Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
        Flags := Flags or PAGE_EXECUTE_WRITECOPY
      else
        Flags := Flags or PAGE_EXECUTE;

  end
  else
    if (Characteristics and IMAGE_SCN_MEM_READ) <> 0 then
    begin

      if (Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
        Flags := Flags or PAGE_READWRITE
      else
        Flags := Flags or PAGE_READONLY;

    end
    else
      if (Characteristics and IMAGE_SCN_MEM_WRITE) <> 0 then
        Flags := Flags or PAGE_WRITECOPY
      else
        Flags := Flags or PAGE_NOACCESS;

  if (Characteristics and IMAGE_SCN_MEM_NOT_CACHED) <> 0 then
    Flags := Flags or PAGE_NOCACHE;

  Result := Flags;
end;

function ConvertPointer(SectionHeaders : PImageSectionHeaders; RVA : LONGWORD) : POINTER; inline;
var
  I                                     : INTEGER;
  Section                               : PImageSectionHeader;
  SectionCount                          : LongWord;
  SectionBase                           : LongWord;
begin
  RESULT := nil;

  SectionCount := SectionHeaders^[0].NumberOfRelocations;
  for I := 0 to SectionCount - 1 do
  begin
    Section := @SectionHeaders^[I];
    SectionBase := Section.PointerToRelocations;

    if (RVA < (Section.VirtualAddress + Section.SizeOfRawData)) and (RVA >= Section.VirtualAddress) then
    begin
      RESULT := POINTER(SectionBase + RVA - Section.VirtualAddress);
      EXIT;
    end;
  end;
end;

//***************************************************************************//
// load dll
//***************************************************************************//

function DLLLoader_Load(Buffer : Pointer; Len : LongInt; lpResersed : Pointer) : Integer; inline;
label
  QUIT_STREAM, QUIT_SECTION;
var
  ImageDOSHeader                        : TImageDOSHeader;
  ImageNTHeaders                        : TImageNTHeaders;
  Section                               : PImageSectionHeader;
  SectionHeaders                        : PImageSectionHeaders;
  SectionCount                          : LongWord;
  SectionBase                           : POINTER;
  SectionSize                           : LongWord;
  Characteristics                       : LONGWORD;
  Flags                                 : LONGWORD;
  OldProtect                            : LONGWORD;
  smData                                : PStreamData;
  OldPosition                           : INTEGER;
  I                                     : INTEGER;
  Ret                                   : TDLLLoadedObject;
  _RelocFunTable                        : TRelocFunctionTable;
  Relocations                           : PCHAR;
  Position                              : LONGWORD;
  BaseRelocation                        : PImageBaseRelocation;
  Base                                  : POINTER;
  NumberOfRelocations                   : LONGWORD;
  Relocation                            : PWordArray;
  RelocationCounter                     : LONGINT;
  RelocationPointer                     : POINTER;
  RelocationType                        : LONGWORD;
begin
  // 参数输入检查和配置
  RESULT := 0;
  if not ASSIGNED(Buffer) then EXIT;
  if not (Len > 0) then EXIT;

  // 重要函数重定位
  if not MakeFunctinTable(_RelocFunTable) then exit;

  // 生成DllLoader对象
  FillChar(Ret, SizeOf(TDLLLoadedObject), #0);
  Ret.ImageBase := nil;
  Ret.DLLProc := nil;
  Ret.RelocFnTable := _RelocFunTable;

  //变量初始化
  smData := MemStream_Create(@_RelocFunTable);
  smData.Memory := Buffer;
  smData.Size := Len;

  // 读取PE文件头入局部变量,并校验是否合法

  FILLCHAR(ImageNTHeaders, SIZEOF(TImageNTHeaders), #0);
  MemStream_Seek(smData, 0, soFromBeginning);
  if MemStream_Read(smData, ImageDOSHeader, SIZEOF(TImageDOSHeader)) <> SIZEOF(TImageDOSHeader) then goto QUIT_STREAM;
  if ImageDOSHeader.Signature <> $5A4D then goto QUIT_STREAM;
  if MemStream_Seek(smData, ImageDOSHeader.LFAOffset, soFromBeginning) <> LONGINT(ImageDOSHeader.LFAOffset) then goto QUIT_STREAM;
  if MemStream_Read(smData, ImageNTHeaders.Signature, SIZEOF(LONGWORD)) <> SIZEOF(LONGWORD) then goto QUIT_STREAM;
  if ImageNTHeaders.Signature <> $00004550 then goto QUIT_STREAM;
  if MemStream_Read(smData, ImageNTHeaders.FileHeader, SIZEOF(TImageFileHeader)) <> SIZEOF(TImageFileHeader) then goto QUIT_STREAM;
  if ImageNTHeaders.FileHeader.Machine <> $14C then goto QUIT_STREAM;
  if MemStream_Read(smData, ImageNTHeaders.OptionalHeader, ImageNTHeaders.FileHeader.SizeOfOptionalHeader) <> ImageNTHeaders.FileHeader.SizeOfOptionalHeader then goto QUIT_STREAM;

  // 初始化pe文件映象

  if not (ImageNTHeaders.FileHeader.NumberOfSections > 0) then goto QUIT_STREAM;
  Ret.ImageBase := Ret.RelocFnTable.VirtualAlloc(nil, ImageNTHeaders.OptionalHeader.SizeOfImage, MEM_RESERVE, PAGE_NOACCESS);
  SectionBase := Ret.RelocFnTable.VirtualAlloc(Ret.ImageBase, ImageNTHeaders.OptionalHeader.SizeOfHeaders, MEM_COMMIT, PAGE_READWRITE);
  OldPosition := smData.Position;
  MemStream_Seek(smData, 0, soFromBeginning);
  MemStream_Read(smData, SectionBase^, ImageNTHeaders.OptionalHeader.SizeOfHeaders);
  Ret.RelocFnTable.VirtualProtect(SectionBase, ImageNTHeaders.OptionalHeader.SizeOfHeaders, PAGE_READONLY, OldProtect);
  MemStream_Seek(smData, OldPosition, soFromBeginning);

  //读取section信息

  if not (ImageNTHeaders.FileHeader.NumberOfSections > 0) then goto QUIT_STREAM;
  SectionCount := ImageNTHeaders.FileHeader.NumberOfSections;
  SectionHeaders := Ret.RelocFnTable.VirtualAlloc(nil, SectionCount * SIZEOF(TImageSectionHeader), MEM_COMMIT, PAGE_READWRITE);
  if MemStream_Read(smData, SectionHeaders^, SectionCount * SIZEOF(TImageSectionHeader)) <> (ImageNTHeaders.FileHeader.NumberOfSections * SIZEOF(TImageSectionHeader)) then goto QUIT_SECTION;

  // 将section并映射到内存

  for I := 0 to SectionCount - 1 do
  begin
    Section := @SectionHeaders^[I];
    // 申请内存
    SectionSize := Section.SizeOfRawData;
    if SectionSize < Section.Misc.VirtualSize then
      SectionSize := Section.Misc.VirtualSize;
    SectionBase := Ret.RelocFnTable.VirtualAlloc(POINTER(LONGWORD(Section.VirtualAddress + LONGWORD(Ret.ImageBase))), SectionSize, MEM_COMMIT, PAGE_READWRITE);
    // 复制节段数据
    FILLCHAR(SectionBase^, SectionSize, #0);
    if Section.PointerToRawData <> 0 then
    begin
      MemStream_Seek(smData, Section.PointerToRawData, soFromBeginning);
      if MemStream_Read(smData, SectionBase^, Section.SizeOfRawData) <> LONGINT(Section.SizeOfRawData) then goto QUIT_SECTION;
    end;
    // 利用无用字段暂存实际地址和大小 ,供后面的地址查询用
    Section.PointerToRelocations := LongWord(SectionBase);
    Section.PointerToLinenumbers := SectionSize;
    Section.NumberOfRelocations := SectionCount;
  end;

  // 处理DLL重定位信息  ProcessRelocations

  if ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress <> 0 then
  begin
    Relocations := ConvertPointer(SectionHeaders, ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress);
    Position := 0;
    while ASSIGNED(Relocations) and (Position < ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size) do
    begin
      BaseRelocation := PImageBaseRelocation(Relocations);
      Base := ConvertPointer(SectionHeaders, BaseRelocation^.VirtualAddress);
      if not ASSIGNED(Base) then goto QUIT_SECTION;
      NumberOfRelocations := (BaseRelocation^.SizeOfBlock - SIZEOF(TImageBaseRelocation)) div SIZEOF(WORD);
      Relocation := POINTER(LONGWORD(LONGWORD(BaseRelocation) + SIZEOF(TImageBaseRelocation)));
      for RelocationCounter := 0 to NumberOfRelocations - 1 do
      begin
        RelocationPointer := POINTER(LONGWORD(LONGWORD(Base) + (Relocation^[RelocationCounter] and $FFF)));
        RelocationType := Relocation^[RelocationCounter] shr 12;
        case RelocationType of
          IMAGE_REL_BASED_ABSOLUTE : ;
          IMAGE_REL_BASED_HIGH : PWORD(RelocationPointer)^ := (LONGWORD(((LONGWORD(PWORD(RelocationPointer)^ + LONGWORD(Ret.ImageBase) - ImageNTHeaders.OptionalHeader.ImageBase)))) shr 16) and $FFFF;
          IMAGE_REL_BASED_LOW : PWORD(RelocationPointer)^ := LONGWORD(((LONGWORD(PWORD(RelocationPointer)^ + LONGWORD(Ret.ImageBase) - ImageNTHeaders.OptionalHeader.ImageBase)))) and $FFFF;
          IMAGE_REL_BASED_HIGHLOW : PPOINTER(RelocationPointer)^ := POINTER((LONGWORD(LONGWORD(PPOINTER(RelocationPointer)^) + LONGWORD(Ret.ImageBase) - ImageNTHeaders.OptionalHeader.ImageBase)));
          IMAGE_REL_BASED_HIGHADJ : ;
          IMAGE_REL_BASED_MIPS_JMPADDR : ;
        end;
      end;
      Relocations := POINTER(LONGWORD(LONGWORD(Relocations) + BaseRelocation^.SizeOfBlock));
      INC(Position, BaseRelocation^.SizeOfBlock);
    end;
  end;

  // 设置section保护属性

  for I := 0 to SectionCount - 1 do
  begin
    Section := @SectionHeaders^[I];
    SectionBase := POINTER(Section.PointerToRelocations);
    SectionSize := Section.PointerToLinenumbers;

    Flags := GetAllocMemFlags(Section.Characteristics);
    Ret.RelocFnTable.VirtualProtect(SectionBase, SectionSize, Flags, OldProtect);
  end;

   // 调用DLL入口

  @Ret.DLLProc := ConvertPointer(SectionHeaders, ImageNTHeaders.OptionalHeader.AddressOfEntryPoint);
  RESULT := Ret.DLLProc(CARDINAL(Ret.ImageBase), DLL_PROCESS_ATTACH, lpResersed);

  QUIT_SECTION :
  Ret.RelocFnTable.VirtualFree(SectionHeaders, 0, MEM_FREE);
  QUIT_STREAM :
  MemStream_Destroy(smData);
end;

procedure DLLLoader_Load_End;
begin
end;

end.
