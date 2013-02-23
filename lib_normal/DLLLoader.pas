{*************************************************************************
 *                                                                       *
 * DLL linking is provided by Benjamin Rosseaux, www.0ok.de,             *
 * mailto:benjamin@0ok.de                                                *
 *                                                                       *
 * This DLL Loader code is coyyrighted: (C) 2004, Benjamin Rosseaux      *
 *                                                                       *
 *************************************************************************}

 UNIT DLLLoader;

INTERFACE

USES Windows,Classes, SysUtils, SimpleDLLLoader;

CONST IMPORTED_NAME_OFFSET=$00000002;
      IMAGE_ORDINAL_FLAG32=$80000000;
      IMAGE_ORDINAL_MASK32=$0000FFFF;

      RTL_CRITSECT_TYPE=0;
      RTL_RESOURCE_TYPE=1;

      DLL_PROCESS_ATTACH=1;
      DLL_THREAD_ATTACH=2;
      DLL_THREAD_DETACH=3;
      DLL_PROCESS_DETACH=0;

      IMAGE_SizeHeader=20;

      IMAGE_FILE_RELOCS_STRIPPED=$0001;
      IMAGE_FILE_EXECUTABLE_IMAGE=$0002;
      IMAGE_FILE_LINE_NUMS_STRIPPED=$0004;
      IMAGE_FILE_LOCAL_SYMS_STRIPPED=$0008;
      IMAGE_FILE_AGGRESIVE_WS_TRIM=$0010;
      IMAGE_FILE_BYTES_REVERSED_LO=$0080;
      IMAGE_FILE_32BIT_MACHINE=$0100;
      IMAGE_FILE_DEBUG_STRIPPED=$0200;
      IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP=$0400;
      IMAGE_FILE_NET_RUN_FROM_SWAP=$0800;
      IMAGE_FILE_SYSTEM=$1000;
      IMAGE_FILE_DLL=$2000;
      IMAGE_FILE_UP_SYSTEM_ONLY=$4000;
      IMAGE_FILE_BYTES_REVERSED_HI=$8000;

      IMAGE_FILE_MACHINE_UNKNOWN=0;
      IMAGE_FILE_MACHINE_I386=$14C;
      IMAGE_FILE_MACHINE_R3000=$162;
      IMAGE_FILE_MACHINE_R4000=$166;
      IMAGE_FILE_MACHINE_R10000=$168;
      IMAGE_FILE_MACHINE_ALPHA=$184;
      IMAGE_FILE_MACHINE_POWERPC=$1F0;

      IMAGE_NUMBEROF_DIRECTORY_ENTRIES=16;

      IMAGE_SUBSYSTEM_UNKNOWN=0;
      IMAGE_SUBSYSTEM_NATIVE=1;
      IMAGE_SUBSYSTEM_WINDOWS_GUI=2;
      IMAGE_SUBSYSTEM_WINDOWS_CUI=3;
      IMAGE_SUBSYSTEM_OS2_CUI=5;
      IMAGE_SUBSYSTEM_POSIX_CUI=7;
      IMAGE_SUBSYSTEM_RESERVED=8;

      IMAGE_DIRECTORY_ENTRY_EXPORT=0;
      IMAGE_DIRECTORY_ENTRY_IMPORT=1;
      IMAGE_DIRECTORY_ENTRY_RESOURCE=2;
      IMAGE_DIRECTORY_ENTRY_EXCEPTION=3;
      IMAGE_DIRECTORY_ENTRY_SECURITY=4;
      IMAGE_DIRECTORY_ENTRY_BASERELOC=5;
      IMAGE_DIRECTORY_ENTRY_DEBUG=6;
      IMAGE_DIRECTORY_ENTRY_COPYRIGHT=7;
      IMAGE_DIRECTORY_ENTRY_GLOBALPTR=8;
      IMAGE_DIRECTORY_ENTRY_TLS=9;
      IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG=10;
      IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT=11;
      IMAGE_DIRECTORY_ENTRY_IAT=12;

      IMAGE_SIZEOF_SHORT_NAME=8;

      IMAGE_SCN_TYIMAGE_REG=$00000000;
      IMAGE_SCN_TYIMAGE_DSECT=$00000001;
      IMAGE_SCN_TYIMAGE_NOLOAD=$00000002;
      IMAGE_SCN_TYIMAGE_GROUP=$00000004;
      IMAGE_SCN_TYIMAGE_NO_PAD=$00000008;
      IMAGE_SCN_TYIMAGE_COPY=$00000010;
      IMAGE_SCN_CNT_CODE=$00000020;
      IMAGE_SCN_CNT_INITIALIZED_DATA=$00000040;
      IMAGE_SCN_CNT_UNINITIALIZED_DATA=$00000080;
      IMAGE_SCN_LNK_OTHER=$00000100;
      IMAGE_SCN_LNK_INFO=$00000200;
      IMAGE_SCN_TYIMAGE_OVER=$0000400;
      IMAGE_SCN_LNK_REMOVE=$00000800;
      IMAGE_SCN_LNK_COMDAT=$00001000;
      IMAGE_SCN_MEM_PROTECTED=$00004000;
      IMAGE_SCN_MEM_FARDATA=$00008000;
      IMAGE_SCN_MEM_SYSHEAP=$00010000;
      IMAGE_SCN_MEM_PURGEABLE=$00020000;
      IMAGE_SCN_MEM_16BIT=$00020000;
      IMAGE_SCN_MEM_LOCKED=$00040000;
      IMAGE_SCN_MEM_PRELOAD=$00080000;
      IMAGE_SCN_ALIGN_1BYTES=$00100000;
      IMAGE_SCN_ALIGN_2BYTES=$00200000;
      IMAGE_SCN_ALIGN_4BYTES=$00300000;
      IMAGE_SCN_ALIGN_8BYTES=$00400000;
      IMAGE_SCN_ALIGN_16BYTES=$00500000;
      IMAGE_SCN_ALIGN_32BYTES=$00600000;
      IMAGE_SCN_ALIGN_64BYTES=$00700000;
      IMAGE_SCN_LNK_NRELOC_OVFL=$01000000;
      IMAGE_SCN_MEM_DISCARDABLE=$02000000;
      IMAGE_SCN_MEM_NOT_CACHED=$04000000;
      IMAGE_SCN_MEM_NOT_PAGED=$08000000;
      IMAGE_SCN_MEM_SHARED=$10000000;
      IMAGE_SCN_MEM_EXECUTE=$20000000;
      IMAGE_SCN_MEM_READ=$40000000;
      IMAGE_SCN_MEM_WRITE=LONGWORD($80000000);

      IMAGE_REL_BASED_ABSOLUTE=0;
      IMAGE_REL_BASED_HIGH=1;
      IMAGE_REL_BASED_LOW=2;
      IMAGE_REL_BASED_HIGHLOW=3;
      IMAGE_REL_BASED_HIGHADJ=4;
      IMAGE_REL_BASED_MIPS_JMPADDR=5;
      IMAGE_REL_BASED_SECTION=6;
      IMAGE_REL_BASED_REL32=7;

      IMAGE_REL_BASED_MIPS_JMPADDR16=9;
      IMAGE_REL_BASED_IA64_IMM64=9;
      IMAGE_REL_BASED_DIR64=10;
      IMAGE_REL_BASED_HIGH3ADJ=11;

      PAGE_NOACCESS=1;
      PAGE_READONLY=2;
      PAGE_READWRITE=4;
      PAGE_WRITECOPY=8;
      PAGE_EXECUTE=$10;
      PAGE_EXECUTE_READ=$20;
      PAGE_EXECUTE_READWRITE=$40;
      PAGE_EXECUTE_WRITECOPY=$80;
      PAGE_GUARD=$100;
      PAGE_NOCACHE=$200;
      MEM_COMMIT=$1000;
      MEM_RESERVE=$2000;
      MEM_DECOMMIT=$4000;
      MEM_RELEASE=$8000;
      MEM_FREE=$10000;
      MEM_PRIVATE=$20000;
      MEM_MAPPED=$40000;
      MEM_RESET=$80000;
      MEM_TOP_DOWN=$100000;
      SEC_FILE=$800000;
      SEC_IMAGE=$1000000;
      SEC_RESERVE=$4000000;
      SEC_COMMIT=$8000000;
      SEC_NOCACHE=$10000000;
      MEM_IMAGE=SEC_IMAGE;
      
TYPE PPOINTER=^POINTER;

     PLONGWORD=^LONGWORD;
     PPLONGWORD=^PLONGWORD;

     PWORD=^WORD;
     PPWORD=^PWORD;

     HINST=LONGWORD;
     HMODULE=HINST;

     PWordArray=^TWordArray;
     TWordArray=ARRAY[0..(2147483647 DIV SIZEOF(WORD))-1] OF WORD;

     PLongWordArray=^TLongWordArray;
     TLongWordArray=ARRAY [0..(2147483647 DIV SIZEOF(LONGWORD))-1] OF LONGWORD;

     PImageDOSHeader=^TImageDOSHeader;
     TImageDOSHeader=PACKED RECORD
      Signature:WORD;
      PartPag:WORD;
      PageCnt:WORD;
      ReloCnt:WORD;
      HdrSize:WORD;
      MinMem:WORD;
      MaxMem:WORD;
      ReloSS:WORD;
      ExeSP:WORD;
      ChkSum:WORD;
      ExeIP:WORD;
      ReloCS:WORD;
      TablOff:WORD;
      Overlay:WORD;
      Reserved:PACKED ARRAY[0..3] OF WORD;
      OEMID:WORD;
      OEMInfo:WORD;
      Reserved2:PACKED ARRAY[0..9] OF WORD;
      LFAOffset:LONGWORD;
     END;

     TISHMisc=PACKED RECORD
      CASE INTEGER OF
       0:(PhysicalAddress:LONGWORD);
       1:(VirtualSize:LONGWORD);
     END;

     PImageExportDirectory=^TImageExportDirectory;
     TImageExportDirectory=PACKED RECORD
      Characteristics:LONGWORD;
      TimeDateStamp:LONGWORD;
      MajorVersion:WORD;
      MinorVersion:WORD;
      Name:LONGWORD;
      Base:LONGWORD;
      NumberOfFunctions:LONGWORD;
      NumberOfNames:LONGWORD;
      AddressOfFunctions:PPLONGWORD;
      AddressOfNames:PPLONGWORD;
      AddressOfNameOrdinals:PPWORD;
     END;

     PImageSectionHeader=^TImageSectionHeader;
     TImageSectionHeader=PACKED RECORD
      Name:PACKED ARRAY[0..IMAGE_SIZEOF_SHORT_NAME-1] OF BYTE;
      Misc:TISHMisc;
      VirtualAddress:LONGWORD;
      SizeOfRawData:LONGWORD;
      PointerToRawData:LONGWORD;
      PointerToRelocations:LONGWORD;
      PointerToLinenumbers:LONGWORD;
      NumberOfRelocations:WORD;
      NumberOfLinenumbers:WORD;
      Characteristics:LONGWORD;
     END;

     PImageSectionHeaders=^TImageSectionHeaders;
     TImageSectionHeaders=ARRAY[0..(2147483647 DIV SIZEOF(TImageSectionHeader))-1] OF TImageSectionHeader;

     PImageDataDirectory=^TImageDataDirectory;
     TImageDataDirectory=PACKED RECORD
      VirtualAddress:LONGWORD;
      Size:LONGWORD;
     END;

     PImageFileHeader=^TImageFileHeader;
     TImageFileHeader=PACKED RECORD
      Machine:WORD;
      NumberOfSections:WORD;
      TimeDateStamp:LONGWORD;
      PointerToSymbolTable:LONGWORD;
      NumberOfSymbols:LONGWORD;
      SizeOfOptionalHeader:WORD;
      Characteristics:WORD;
     END;

     PImageOptionalHeader=^TImageOptionalHeader;
     TImageOptionalHeader=PACKED RECORD
      Magic:WORD;
      MajorLinkerVersion:BYTE;
      MinorLinkerVersion:BYTE;
      SizeOfCode:LONGWORD;
      SizeOfInitializedData:LONGWORD;
      SizeOfUninitializedData:LONGWORD;
      AddressOfEntryPoint:LONGWORD;
      BaseOfCode:LONGWORD;
      BaseOfData:LONGWORD;
      ImageBase:LONGWORD;
      SectionAlignment:LONGWORD;
      FileAlignment:LONGWORD;
      MajorOperatingSystemVersion:WORD;
      MinorOperatingSystemVersion:WORD;
      MajorImageVersion:WORD;
      MinorImageVersion:WORD;
      MajorSubsystemVersion:WORD;
      MinorSubsystemVersion:WORD;
      Win32VersionValue:LONGWORD;
      SizeOfImage:LONGWORD;
      SizeOfHeaders:LONGWORD;
      CheckSum:LONGWORD;
      Subsystem:WORD;
      DllCharacteristics:WORD;
      SizeOfStackReserve:LONGWORD;
      SizeOfStackCommit:LONGWORD;
      SizeOfHeapReserve:LONGWORD;
      SizeOfHeapCommit:LONGWORD;
      LoaderFlags:LONGWORD;
      NumberOfRvaAndSizes:LONGWORD;
      DataDirectory:PACKED ARRAY[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] OF TImageDataDirectory;
     END;

     PImageNTHeaders=^TImageNTHeaders;
     TImageNTHeaders=PACKED RECORD
      Signature:LONGWORD;
      FileHeader:TImageFileHeader;
      OptionalHeader:TImageOptionalHeader;
     END;

     PImageImportDescriptor=^TImageImportDescriptor;
     TImageImportDescriptor=PACKED RECORD
      OriginalFirstThunk:LONGWORD;
      TimeDateStamp:LONGWORD;
      ForwarderChain:LONGWORD;
      Name:LONGWORD;
      FirstThunk:LONGWORD;
     END;

     PImageBaseRelocation=^TImageBaseRelocation;
     TImageBaseRelocation=PACKED RECORD
      VirtualAddress:LONGWORD;
      SizeOfBlock:LONGWORD;
     END;

     PImageThunkData=^TImageThunkData;
     TImageThunkData=PACKED RECORD
      ForwarderString:LONGWORD;
      Funktion:LONGWORD;
      Ordinal:LONGWORD;
      AddressOfData:LONGWORD;
     END;

     PSection=^TSection;
     TSection=PACKED RECORD
      Base:POINTER;
      RVA:LONGWORD;
      Size:LONGWORD;
      Characteristics:LONGWORD;
     END;

     TSections=ARRAY OF TSection;

     TDLLEntryProc=FUNCTION(hinstDLL:HMODULE;dwReason:LONGWORD;lpvReserved:POINTER):LONGWORD; STDCALL;

     TNameOrID=(niName,niID);

     TExternalLibrary=RECORD
      LibraryName:STRING;
      LibraryHandle:HINST;
     END;

     TExternalLibrarys=ARRAY OF TExternalLibrary;

     PDLLFunctionImport=^TDLLFunctionImport;
     TDLLFunctionImport=RECORD
      NameOrID:TNameOrID;
      Name:STRING;
      ID:INTEGER;
     END;

     PDLLImport=^TDLLImport;
     TDLLImport=RECORD
      LibraryName:STRING;
      LibraryHandle:HINST;
      Entries:ARRAY OF TDLLFunctionImport;
     END;

     TImports=ARRAY OF TDLLImport;

     PDLLFunctionExport=^TDLLFunctionExport;
     TDLLFunctionExport=RECORD
      Name:STRING;
      Index:INTEGER;
      FunctionPointer:POINTER;
     END;

     TExports=ARRAY OF TDLLFunctionExport;

     TExportTreeLink=POINTER;

     PExportTreeNode=^TExportTreeNode;
     TExportTreeNode=RECORD
      TheChar:CHAR;
      Link:TExportTreeLink;
      LinkExist:BOOLEAN;
      Prevoius,Next,Up,Down:PExportTreeNode;
     END;

     TExportTree=CLASS
      PRIVATE
       Root:PExportTreeNode;
      PUBLIC
       CONSTRUCTOR Create;
       DESTRUCTOR Destroy; OVERRIDE;
       PROCEDURE Dump;
       FUNCTION Add(FunctionName:STRING;Link:TExportTreeLink):BOOLEAN;
       FUNCTION Delete(FunctionName:STRING):BOOLEAN;
       FUNCTION Find(FunctionName:STRING;VAR Link:TExportTreeLink):BOOLEAN;
     END;

     TDLLLoader=CLASS
      PRIVATE
       ImageBaseDelta:INTEGER;
       ExternalLibraryArray:TExternalLibrarys;
       ImportArray:TImports;
       ExportArray:TExports;
       Sections:TSections;
       ExportTree:TExportTree;
       FUNCTION FindExternalLibrary(LibraryName:STRING):INTEGER;
       FUNCTION LoadExternalLibrary(LibraryName:STRING):INTEGER;
       FUNCTION GetExternalLibraryHandle(LibraryName:STRING):HINST;
       FUNCTION ConvertPointer(RVA:LONGWORD):POINTER;
      PROTECTED                     
       FUNCTION ReadImageHeaders (Stream:TStream; Out ImageDOSHeader:TImageDOSHeader; Out ImageNTHeaders:TImageNTHeaders):BOOLEAN; VIRTUAL;
       FUNCTION InitializeImage (Stream:TStream; ImageNTHeaders:TImageNTHeaders; Out OldProtect:LONGWORD):BOOLEAN; VIRTUAL;
       FUNCTION ReadSections (Stream:TStream; ImageNTHeaders:TImageNTHeaders):BOOLEAN; VIRTUAL;
       FUNCTION ProcessRelocations (ImageNTHeaders:TImageNTHeaders):BOOLEAN; VIRTUAL;
       FUNCTION ProcessImports (ImageNTHeaders: TImageNTHeaders):BOOLEAN; VIRTUAL;
       FUNCTION ProtectSections (ImageNTHeaders:TImageNTHeaders; OldProtect:LONGWORD):BOOLEAN; VIRTUAL;
       FUNCTION ProcessExports (ImageNTHeaders: TImageNTHeaders):BOOLEAN; VIRTUAL;
       FUNCTION InitializeLibrary(ImageNTHeaders: TImageNTHeaders; Reserved:Pointer):BOOLEAN; VIRTUAL;
       FUNCTION LoadLibrary (lpLibFileName: PChar): HMODULE;  VIRTUAL;
       FUNCTION GetProcAddress (hModule: HMODULE; lpProcName: LPCSTR): POINTER; VIRTUAL;
      PUBLIC
       OnBeforeAttach: PROCEDURE (Sender: Pointer; ImageBase: Pointer; ImageSize: Integer);
       OnRelocWORD: PROCEDURE (Sender: Pointer; RelocAddr: Pointer; var RelocOffset: Integer);
       OnRelocPOINTER: PROCEDURE (Sender: Pointer; RelocAddr: Pointer; var RelocOffset: Integer);
       NewLoadLibrary: FUNCTION (lpLibFileName: PChar): HMODULE; stdcall;
       NewGetProcAddress: FUNCTION (hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
      PUBLIC
       Data: POINTER;
       ImageBase:POINTER;
       ImageSize: Integer;      
       AttachResult: LONGWORD;
       IsSkipDLLProc: BOOL;
       IsSkipRelocation: BOOL;
       IsSkipImports: BOOL;
       DLLProc:TDLLEntryProc;
       CONSTRUCTOR Create;
       DESTRUCTOR Destroy; OVERRIDE;
       FUNCTION Load(Stream:TStream; Reserved:Pointer=NIL):BOOLEAN;
       FUNCTION Unload(Reserved:Pointer=NIL):BOOLEAN;
       FUNCTION FindExport(FunctionName:STRING):POINTER;
       FUNCTION FindExportPerIndex(FunctionIndex:INTEGER):POINTER;
     END;

     TEnumProc = FUNCTION (Sender:POINTER; Name:STRING; Index:INTEGER; FunctionPointer:POINTER):BOOLEAN;

     TDLLLoaderEx = CLASS(TDLLLoader)
      PRIVATE
        DependLoaders: TStringList;
      PROTECTED
        FUNCTION LoadLibrary (lpLibFileName: PChar): HMODULE;  OVERRIDE;
        FUNCTION GetProcAddress (hModule: HMODULE; lpProcName: LPCSTR): POINTER; OVERRIDE;
        FUNCTION GetDependLibrary (LibName: PChar): THandle;
        FUNCTION GetDependFunction (LibHandle: THandle; ProcName: PChar): Pointer;
      PUBLIC
        FUNCTION GetExportList:TStringList;
        PROCEDURE EnumExportList(Sender:POINTER; Enum: TEnumProc);
        PROCEDURE AddDependLoader (LibName: STRING; DLL: TDLLLoader);
        FUNCTION LoadDLL(DLL:STRING; Reserved:Pointer=NIL):BOOLEAN;
        FUNCTION LoadRES(ResType, ResName: String; Reserved:Pointer=NIL):BOOLEAN;
     END;


FUNCTION LoadLibraryExport(Sender:POINTER; Enum: TEnumProc; DLL:STRING; Reserved:Pointer=NIL):LongBool;
FUNCTION LoadLibraryOnly(DLL:STRING; Reserved:Pointer=NIL):LongBool;
FUNCTION GetFileName(hModule:THandle):STRING;
function IsPeFile(FileName: string): Boolean;

IMPLEMENTATION


function IsPeFile(FileName: string): Boolean;
var
  PEDosHead: TImageDosHeader;
  PENTHead: TImageNtHeaders;
  PeFile: integer;
begin
  IsPeFile := False;
  PeFile := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  try
    FileSeek(PeFile, 0, soFromBeginning);
    FileRead(PeFile, PEDosHead, SizeOf(PEDosHead));
    FileSeek(PeFile, PEDosHead.LFAOffset, soFromBeginning);
    FileRead(PeFile, PENTHead, SizeOf(PENTHead));

  finally
    FileClose(PeFile);
  end;

  if (PENTHead.Signature = IMAGE_NT_SIGNATURE) then
  begin
    IsPeFile := true;
  end;
end;

function GetFileName(hModule:THandle):string;
var
  Buffer:array[byte] of char;
begin
  ZeroMemory(@buffer[0], 256);
  GetModuleFileName(hModule, @Buffer, 256);
  result := buffer;
end;


function FileExists(const FileName: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(FileName));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code = 0);
end;    

FUNCTION LoadLibraryOnly(DLL:STRING; Reserved:Pointer=NIL):LongBool;
VAR
  FileStream :TFileStream;
BEGIN
  result := False;
  IF NOT FileExists(DLL) THEN exit;
  FileStream :=TFileStream.Create(DLL, 0);
  TRY
    Result := TDLLLoader.Create.Load(FileStream, Reserved);
  FINALLY
    FileStream.Free;
  END;
END;

FUNCTION LoadLibraryExport(Sender:POINTER; Enum: TEnumProc; DLL:STRING; Reserved:Pointer=NIL):LongBool;
BEGIN
  WITH TDLLLoaderEx.Create DO
  BEGIN
    Result := LOADDLL(DLL, Reserved);
    IF Result THEN
      EnumExportList(Sender, Enum);
  END;
END;

FUNCTION StrToInt(S:STRING):INTEGER;
VAR C:INTEGER;
BEGIN
 VAL(S,RESULT,C);
END;

FUNCTION CreateExportTreeNode(AChar:CHAR):PExportTreeNode;
BEGIN
 GETMEM(RESULT,SIZEOF(TExportTreeNode));
 RESULT^.TheChar:=AChar;
 RESULT^.Link:=NIL;
 RESULT^.LinkExist:=FALSE;
 RESULT^.Prevoius:=NIL;
 RESULT^.Next:=NIL;
 RESULT^.Up:=NIL;
 RESULT^.Down:=NIL;
END;

PROCEDURE DestroyExportTreeNode(Node:PExportTreeNode);
BEGIN
 IF ASSIGNED(Node) THEN BEGIN
  DestroyExportTreeNode(Node^.Next);
  DestroyExportTreeNode(Node^.Down);
  FREEMEM(Node);
 END;
END;

CONSTRUCTOR TExportTree.Create;
BEGIN
 INHERITED Create;
 Root:=NIL;
END;

DESTRUCTOR TExportTree.Destroy;
BEGIN
 DestroyExportTreeNode(Root);
 INHERITED Destroy;
END;

PROCEDURE TExportTree.Dump;
VAR Ident:INTEGER;
 PROCEDURE DumpNode(Node:PExportTreeNode);
 VAR SubNode:PExportTreeNode;
     IdentCounter,IdentOld:INTEGER;
 BEGIN
  FOR IdentCounter:=1 TO Ident DO WRITE(' ');
  WRITE(Node^.TheChar);
  IdentOld:=Ident;
  SubNode:=Node^.Next;
  WHILE ASSIGNED(SubNode) DO BEGIN
   WRITE(SubNode.TheChar);
   IF NOT ASSIGNED(SubNode^.Next) THEN BREAK;
   INC(Ident);
   SubNode:=SubNode^.Next;
  END;
  WRITELN;
  INC(Ident);
  WHILE ASSIGNED(SubNode) AND (SubNode<>Node) DO BEGIN
   IF ASSIGNED(SubNode^.Down) THEN DumpNode(SubNode^.Down);
   SubNode:=SubNode^.Prevoius;
   DEC(Ident);
  END;
  Ident:=IdentOld;
  IF ASSIGNED(Node^.Down) THEN DumpNode(Node^.Down);
 END;
BEGIN
 Ident:=0;
 DumpNode(Root);
END;

FUNCTION TExportTree.Add(FunctionName:STRING;Link:TExportTreeLink):BOOLEAN;
VAR StringLength,Position,PositionCounter:INTEGER;
    NewNode,LastNode,Node:PExportTreeNode;
    StringChar,NodeChar:CHAR;
BEGIN
 RESULT:=FALSE;
 StringLength:=LENGTH(FunctionName);
 IF StringLength>0 THEN BEGIN
  LastNode:=NIL;
  Node:=Root;
  FOR Position:=1 TO StringLength DO BEGIN
   StringChar:=FunctionName[Position];
   IF ASSIGNED(Node) THEN BEGIN
    NodeChar:=Node^.TheChar;
    IF NodeChar=StringChar THEN BEGIN
     LastNode:=Node;
     Node:=Node^.Next;
   END ELSE BEGIN
     WHILE (NodeChar<StringChar) AND ASSIGNED(Node^.Down) DO BEGIN
      Node:=Node^.Down;
      NodeChar:=Node^.TheChar;
     END;
     IF NodeChar=StringChar THEN BEGIN
      LastNode:=Node;
      Node:=Node^.Next;
     END ELSE BEGIN
      NewNode:=CreateExportTreeNode(StringChar);
      IF NodeChar<StringChar THEN BEGIN
       NewNode^.Down:=Node^.Down;
       NewNode^.Up:=Node;
       IF ASSIGNED(NewNode^.Down) THEN BEGIN
        NewNode^.Down^.Up:=NewNode;
       END;
       NewNode^.Prevoius:=Node^.Prevoius;
       Node^.Down:=NewNode;
      END ELSE IF NodeChar>StringChar THEN BEGIN
       NewNode^.Down:=Node;
       NewNode^.Up:=Node^.Up;
       IF ASSIGNED(NewNode^.Up) THEN BEGIN
        NewNode^.Up^.Down:=NewNode;
       END;
       NewNode^.Prevoius:=Node^.Prevoius;
       IF NOT ASSIGNED(NewNode^.Up) THEN BEGIN
        IF ASSIGNED(NewNode^.Prevoius) THEN BEGIN
         NewNode^.Prevoius^.Next:=NewNode;
        END ELSE BEGIN
         Root:=NewNode;
        END;
       END;
       Node^.Up:=NewNode;
      END;
      LastNode:=NewNode;
      Node:=LastNode^.Next;
     END;
    END;
   END ELSE BEGIN
    FOR PositionCounter:=Position TO StringLength DO BEGIN
     NewNode:=CreateExportTreeNode(FunctionName[PositionCounter]);
     IF ASSIGNED(LastNode) THEN BEGIN
      NewNode^.Prevoius:=LastNode;
      LastNode^.Next:=NewNode;
      LastNode:=LastNode^.Next;
     END ELSE BEGIN
      IF NOT ASSIGNED(Root) THEN BEGIN
       Root:=NewNode;
       LastNode:=Root;
      END;
     END;
    END;
    BREAK;
   END;
  END;
  IF ASSIGNED(LastNode) THEN BEGIN
   IF NOT LastNode^.LinkExist THEN BEGIN
    LastNode^.Link:=Link;
    LastNode^.LinkExist:=TRUE;
    RESULT:=TRUE;
   END;
  END;
 END;
END;

FUNCTION TExportTree.Delete(FunctionName:STRING):BOOLEAN;
VAR StringLength,Position:INTEGER;
    Node:PExportTreeNode;
    StringChar,NodeChar:CHAR;
BEGIN
 RESULT:=FALSE;
 StringLength:=LENGTH(FunctionName);
 IF StringLength>0 THEN BEGIN
  Node:=Root;
  FOR Position:=1 TO StringLength DO BEGIN
   StringChar:=FunctionName[Position];
   IF ASSIGNED(Node) THEN BEGIN
    NodeChar:=Node^.TheChar;
    WHILE (NodeChar<>StringChar) AND ASSIGNED(Node^.Down) DO BEGIN
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    END;
    IF NodeChar=StringChar THEN BEGIN
     IF (Position=StringLength) AND Node^.LinkExist THEN BEGIN
      Node^.LinkExist:=FALSE;
      RESULT:=TRUE;
      BREAK;
     END;
     Node:=Node^.Next;
    END;
   END ELSE BEGIN
    BREAK;
   END;
  END;
 END;
END;

FUNCTION TExportTree.Find(FunctionName:STRING;VAR Link:TExportTreeLink):BOOLEAN;
VAR StringLength,Position:INTEGER;
    Node:PExportTreeNode;
    StringChar,NodeChar:CHAR;
BEGIN
 RESULT:=FALSE;
 StringLength:=LENGTH(FunctionName);
 IF StringLength>0 THEN BEGIN
  Node:=Root;
  FOR Position:=1 TO StringLength DO BEGIN
   StringChar:=FunctionName[Position];
   IF ASSIGNED(Node) THEN BEGIN
    NodeChar:=Node^.TheChar;
    WHILE (NodeChar<>StringChar) AND ASSIGNED(Node^.Down) DO BEGIN
     Node:=Node^.Down;
     NodeChar:=Node^.TheChar;
    END;
    IF NodeChar=StringChar THEN BEGIN
     IF (Position=StringLength) AND Node^.LinkExist THEN BEGIN
      Link:=Node^.Link;
      RESULT:=TRUE;
      BREAK;
     END;
     Node:=Node^.Next;
    END;
   END ELSE BEGIN
    BREAK;
   END;
  END;
 END;
END;

CONSTRUCTOR TDLLLoader.Create;
BEGIN
 INHERITED Create;
 ImageBase:=NIL;
 DLLProc:=NIL;
 ExternalLibraryArray:=NIL;
 ImportArray:=NIL;
 ExportArray:=NIL;
 Sections:=NIL;
 ExportTree:=NIL;
 IsSkipDLLProc := False;
 IsSkipRelocation := False;
 IsSkipImports := False;
END;

DESTRUCTOR TDLLLoader.Destroy;
BEGIN
 IF @DLLProc<>NIL THEN Unload;
 IF ASSIGNED(ExportTree) THEN ExportTree.Destroy;
 INHERITED Destroy;
END;

FUNCTION TDLLLoader.FindExternalLibrary(LibraryName:STRING):INTEGER;
VAR I:INTEGER;
BEGIN
 RESULT:=-1;
 FOR I:=0 TO LENGTH(ExternalLibraryArray)-1 DO BEGIN
  IF ExternalLibraryArray[I].LibraryName=LibraryName THEN BEGIN
   RESULT:=I;
   EXIT;
  END;
 END;
END;

FUNCTION TDLLLoader.LoadExternalLibrary(LibraryName:STRING):INTEGER;
BEGIN
 RESULT:=FindExternalLibrary(LibraryName);
 IF RESULT<0 THEN BEGIN
  RESULT:=LENGTH(ExternalLibraryArray);
  SETLENGTH(ExternalLibraryArray,LENGTH(ExternalLibraryArray)+1);
  ExternalLibraryArray[RESULT].LibraryName:=LibraryName;
  if Assigned (NewLoadLibrary) then
    ExternalLibraryArray[RESULT].LibraryHandle:=NewLoadLibrary(PCHAR(LibraryName))
  else
    ExternalLibraryArray[RESULT].LibraryHandle:=LoadLibrary(PCHAR(LibraryName));
 END;
END;

FUNCTION TDLLLoader.GetExternalLibraryHandle(LibraryName:STRING):LONGWORD;
VAR I:INTEGER;
BEGIN
 RESULT:=0;
 FOR I:=0 TO LENGTH(ExternalLibraryArray)-1 DO BEGIN
  IF ExternalLibraryArray[I].LibraryName=LibraryName THEN BEGIN
   RESULT:=ExternalLibraryArray[I].LibraryHandle;
   EXIT;
  END;
 END;
END;


FUNCTION TDLLLoader.LoadLibrary (lpLibFileName: PChar): HMODULE;
begin
  Result := Windows.LoadLibrary (lpLibFileName);
end;

FUNCTION TDLLLoader.GetProcAddress (hModule: HMODULE; lpProcName: LPCSTR): FARPROC;
begin
  Result := Windows.GetProcAddress (hModule, lpProcName);
end;

FUNCTION TDLLLoader.ConvertPointer(RVA:LONGWORD):POINTER;
VAR I:INTEGER;
BEGIN
  RESULT:=NIL;
  FOR I:=0 TO LENGTH(Sections)-1 DO BEGIN
   IF (RVA<(Sections[I].RVA+Sections[I].Size)) AND (RVA>=Sections[I].RVA) THEN BEGIN
    RESULT:=POINTER(LONGWORD((RVA-LONGWORD(Sections[I].RVA))+LONGWORD(Sections[I].Base)));
    EXIT;
   END;
  END;
END;

FUNCTION TDLLLoader.ProcessImports (ImageNTHeaders: TImageNTHeaders):BOOLEAN;
VAR ImportDescriptor:PImageImportDescriptor;
   ThunkData:PLONGWORD;
   Name:PCHAR;
   DLLImport:PDLLImport;
   DLLFunctionImport:PDLLFunctionImport;
   FunctionPointer:POINTER;
BEGIN
  if IsSkipImports then
  begin
    Result := True;
    Exit;
  end;

  IF ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress<>0 THEN
  BEGIN
     ImportDescriptor:=ConvertPointer(ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
     IF ASSIGNED(ImportDescriptor) THEN
     BEGIN
        SETLENGTH(ImportArray,0);
        WHILE ImportDescriptor^.Name<>0 DO
        BEGIN
           Name:=ConvertPointer(ImportDescriptor^.Name);
           SETLENGTH(ImportArray,LENGTH(ImportArray)+1);
           DLLImport:=@ImportArray[LENGTH(ImportArray)-1];
           DLLImport^.LibraryName:=Name;

           LoadExternalLibrary(Name);
           DLLImport^.LibraryHandle:=GetExternalLibraryHandle(Name);
           
           DLLImport^.Entries:=NIL;
           IF ImportDescriptor^.TimeDateStamp=0 THEN
           BEGIN
              ThunkData:=ConvertPointer(ImportDescriptor^.FirstThunk);
           END ELSE BEGIN
              ThunkData:=ConvertPointer(ImportDescriptor^.OriginalFirstThunk);
           END;
           WHILE ThunkData^<>0 DO
           BEGIN
              SETLENGTH(DLLImport^.Entries,LENGTH(DLLImport^.Entries)+1);
              DLLFunctionImport:=@DLLImport^.Entries[LENGTH(DLLImport^.Entries)-1];
              IF (ThunkData^ AND IMAGE_ORDINAL_FLAG32)<>0 THEN
              BEGIN
                 DLLFunctionImport^.NameOrID:=niID;
                 DLLFunctionImport^.ID:=ThunkData^ AND IMAGE_ORDINAL_MASK32;
                 DLLFunctionImport^.Name:='';
                 if Assigned (NewGetProcAddress) then
                   FunctionPointer:=NewGetProcAddress(DLLImport^.LibraryHandle,PCHAR(ThunkData^ AND IMAGE_ORDINAL_MASK32))
                 else
                   FunctionPointer:=GetProcAddress(DLLImport^.LibraryHandle,PCHAR(ThunkData^ AND IMAGE_ORDINAL_MASK32));
              END ELSE
              BEGIN
                 Name:=ConvertPointer(LONGWORD(ThunkData^)+IMPORTED_NAME_OFFSET);
                 DLLFunctionImport^.NameOrID:=niName;
                 DLLFunctionImport^.ID:=0;
                 DLLFunctionImport^.Name:=Name;
                 if Assigned (NewGetProcAddress) then
                   FunctionPointer:=NewGetProcAddress(DLLImport^.LibraryHandle,Name)
                 else
                   FunctionPointer:=GetProcAddress(DLLImport^.LibraryHandle,Name);
              END;
              PPOINTER(Thunkdata)^:=FunctionPointer;
              INC(ThunkData);
           END;
           INC(ImportDescriptor);
        END;
     END;
  END;
  RESULT:=TRUE;
END;


FUNCTION TDLLLoader.ProcessExports (ImageNTHeaders: TImageNTHeaders):BOOLEAN;
 VAR I:INTEGER;
     ExportDirectory:PImageExportDirectory;
     ExportDirectorySize:LONGWORD;
     FunctionNamePointer:POINTER;
     FunctionName:PCHAR;
     FunctionIndexPointer:POINTER;
     FunctionIndex:LONGWORD;
     FunctionPointer:POINTER;
     ForwarderCharPointer:PCHAR;
     ForwarderString:STRING;
     ForwarderLibrary:STRING;
     ForwarderLibraryHandle:HINST;
  FUNCTION ParseStringToNumber(AString:STRING):LONGWORD;
  VAR CharCounter:INTEGER;
  BEGIN
   RESULT:=0;
   FOR CharCounter:=0 TO LENGTH(AString)-1 DO BEGIN
    IF AString[CharCounter] IN ['0'..'9'] THEN BEGIN
     RESULT:=(RESULT*10)+BYTE(BYTE(AString[CharCounter])-BYTE('0'));
    END ELSE BEGIN
     EXIT;
    END;
   END;
  END;
 BEGIN
  IF ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress<>0 THEN BEGIN
   ExportTree:=TExportTree.Create;
   ExportDirectory:=ConvertPointer(ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
   IF ASSIGNED(ExportDirectory) THEN BEGIN
    ExportDirectorySize:=ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
    SETLENGTH(ExportArray,ExportDirectory^.NumberOfNames);
    FOR I:=0 TO ExportDirectory^.NumberOfNames-1 DO BEGIN
     FunctionNamePointer:=ConvertPointer(LONGWORD(ExportDirectory^.AddressOfNames));
     FunctionNamePointer:=ConvertPointer(PLongWordArray(FunctionNamePointer)^[I]);
     FunctionName:=FunctionNamePointer;
     FunctionIndexPointer:=ConvertPointer(LONGWORD(ExportDirectory^.AddressOfNameOrdinals));
     FunctionIndex:=PWordArray(FunctionIndexPointer)^[I];
     FunctionPointer:=ConvertPointer(LONGWORD(ExportDirectory^.AddressOfFunctions));
     FunctionPointer:=ConvertPointer(PLongWordArray(FunctionPointer)^[FunctionIndex]);
     ExportArray[I].Name:=FunctionName;
     ExportArray[I].Index:=FunctionIndex;
     IF (LONGWORD(ExportDirectory)<LONGWORD(FunctionPointer)) AND (LONGWORD(FunctionPointer)<(LONGWORD(ExportDirectory)+ExportDirectorySize)) THEN
     BEGIN
        ForwarderCharPointer:=FunctionPointer;
        ForwarderString:=ForwarderCharPointer;
        WHILE ForwarderCharPointer^<>'.' DO INC(ForwarderCharPointer);
        ForwarderLibrary:=COPY(ForwarderString,1,POS('.',ForwarderString)-1);

        LoadExternalLibrary(ForwarderLibrary);
        ForwarderLibraryHandle:=GetExternalLibraryHandle(ForwarderLibrary);

        IF ForwarderCharPointer^='#' THEN
        BEGIN
           INC(ForwarderCharPointer);
           ForwarderString:=ForwarderCharPointer;
           ForwarderCharPointer:=ConvertPointer(ParseStringToNumber(ForwarderString));
           ForwarderString:=ForwarderCharPointer;
        END ELSE
        BEGIN
           ForwarderString:=ForwarderCharPointer;
           if Assigned (NewGetProcAddress) then
             ExportArray[I].FunctionPointer:=NewGetProcAddress(ForwarderLibraryHandle,PCHAR(ForwarderString))
           else begin
             ExportArray[I].FunctionPointer:=GetProcAddress(ForwarderLibraryHandle,PCHAR(ForwarderString));
           end;
        END;
     END ELSE BEGIN
        ExportArray[I].FunctionPointer:=FunctionPointer;
     END;
//     WriteLn ('ProcessExports ', ExportArray[I].Name, #9, ExportArray[I].Index, #9, IntToHex(DWORD(ExportArray[I].FunctionPointer), 8));
     ExportTree.Add(ExportArray[I].Name,ExportArray[I].FunctionPointer);
    END
   END;
  END;
  RESULT:=TRUE;
END;

FUNCTION TDLLLoader.InitializeLibrary(ImageNTHeaders: TImageNTHeaders; Reserved:Pointer):BOOLEAN;
var
  TransParam: TTransParam;
BEGIN
  RESULT:=TRUE;
  Repeat
    DLLProc:=ConvertPointer(ImageNTHeaders.OptionalHeader.AddressOfEntryPoint);
    IF NOT ASSIGNED (DLLProc) THEN Break;

    IF ASSIGNED (OnBeforeAttach) then
      OnBeforeAttach (Self, ImageBase, ImageSize);

    IF IsSkipDLLProc then Break;

    TransParam.lpParameter := Reserved;
    TransParam.lpResult := 0;
    AttachResult := DLLProc(CARDINAL(ImageBase),DLL_PROCESS_ATTACH, @TransParam);
    if TransParam.lpResult > 0 then
      AttachResult := TransParam.lpResult;

    RESULT:= AttachResult > 0;
  Until True;
END;

FUNCTION TDLLLoader.ReadImageHeaders (Stream:TStream; Out ImageDOSHeader:TImageDOSHeader; Out ImageNTHeaders:TImageNTHeaders):BOOLEAN;
 BEGIN
  RESULT:=FALSE;
  IF Stream.Size>0 THEN BEGIN
   FILLCHAR(ImageNTHeaders,SIZEOF(TImageNTHeaders),#0);
   IF Stream.Read(ImageDOSHeader,SIZEOF(TImageDOSHeader))<>SIZEOF(TImageDOSHeader) THEN EXIT;
   IF ImageDOSHeader.Signature<>$5A4D THEN EXIT;
   IF Stream.Seek(ImageDOSHeader.LFAOffset, soFromBeginning)<>LONGINT(ImageDOSHeader.LFAOffset) THEN EXIT;
   IF Stream.Read(ImageNTHeaders.Signature,SIZEOF(LONGWORD))<>SIZEOF(LONGWORD) THEN EXIT;
   IF ImageNTHeaders.Signature<>$00004550 THEN EXIT;
   IF Stream.Read(ImageNTHeaders.FileHeader,SIZEOF(TImageFileHeader))<>SIZEOF(TImageFileHeader) THEN EXIT;
   IF ImageNTHeaders.FileHeader.Machine<>$14C THEN EXIT;
   IF Stream.Read(ImageNTHeaders.OptionalHeader,ImageNTHeaders.FileHeader.SizeOfOptionalHeader)<>ImageNTHeaders.FileHeader.SizeOfOptionalHeader THEN EXIT;
   RESULT:=TRUE;
  END;
END;

FUNCTION TDLLLoader.InitializeImage (Stream:TStream; ImageNTHeaders:TImageNTHeaders; Out OldProtect:LONGWORD):BOOLEAN;
 VAR SectionBase:POINTER;
     OldPosition:INTEGER;
 BEGIN
  RESULT:=FALSE;
  IF ImageNTHeaders.FileHeader.NumberOfSections>0 THEN BEGIN
   ImageSize:= ImageNTHeaders.OptionalHeader.SizeOfImage;
   ImageBase:=VirtualAlloc(NIL,ImageSize,MEM_RESERVE,PAGE_NOACCESS);
   ImageBaseDelta:=LONGWORD(ImageBase)-ImageNTHeaders.OptionalHeader.ImageBase;
   SectionBase:=VirtualAlloc(ImageBase,ImageNTHeaders.OptionalHeader.SizeOfHeaders,MEM_COMMIT,PAGE_READWRITE);
   OldPosition:=Stream.Position;
   Stream.Seek(0, soFromBeginning);
   Stream.Read(SectionBase^,ImageNTHeaders.OptionalHeader.SizeOfHeaders);
   VirtualProtect(SectionBase,ImageNTHeaders.OptionalHeader.SizeOfHeaders,PAGE_READONLY,OldProtect);
   Stream.Seek(OldPosition, soFromBeginning);
   RESULT:=TRUE;
  END;
END;

FUNCTION TDLLLoader.ReadSections (Stream:TStream; ImageNTHeaders:TImageNTHeaders):BOOLEAN;
 VAR I:INTEGER;
     Section:TImageSectionHeader;
     SectionHeaders:PImageSectionHeaders;
 BEGIN
  RESULT:=FALSE;
  IF ImageNTHeaders.FileHeader.NumberOfSections>0 THEN BEGIN
   GETMEM(SectionHeaders,ImageNTHeaders.FileHeader.NumberOfSections*SIZEOF(TImageSectionHeader));
   IF Stream.Read(SectionHeaders^,(ImageNTHeaders.FileHeader.NumberOfSections*SIZEOF(TImageSectionHeader)))<>(ImageNTHeaders.FileHeader.NumberOfSections*SIZEOF(TImageSectionHeader)) THEN EXIT;
   SETLENGTH(Sections,ImageNTHeaders.FileHeader.NumberOfSections);
   FOR I:=0 TO ImageNTHeaders.FileHeader.NumberOfSections-1 DO BEGIN
    Section:=SectionHeaders^[I];
    Sections[I].RVA:=Section.VirtualAddress;
    Sections[I].Size:=Section.SizeOfRawData;
    IF Sections[I].Size<Section.Misc.VirtualSize THEN BEGIN
     Sections[I].Size:=Section.Misc.VirtualSize;
    END;
    Sections[I].Characteristics:=Section.Characteristics;
    Sections[I].Base:=VirtualAlloc(POINTER(LONGWORD(Sections[I].RVA+LONGWORD(ImageBase))),Sections[I].Size,MEM_COMMIT,PAGE_READWRITE);
    FILLCHAR(Sections[I].Base^,Sections[I].Size,#0);
    IF Section.PointerToRawData<>0 THEN BEGIN
     Stream.Seek(Section.PointerToRawData, soFromBeginning);
     IF Stream.Read(Sections[I].Base^,Section.SizeOfRawData)<>LONGINT(Section.SizeOfRawData) THEN EXIT;
    END;
   END;
   FREEMEM(SectionHeaders);
   RESULT:=TRUE;
  END;
END;


FUNCTION TDLLLoader.ProcessRelocations (ImageNTHeaders:TImageNTHeaders):BOOLEAN;
 VAR Relocations:PCHAR;
     Position:LONGWORD;
     BaseRelocation:PImageBaseRelocation;
     Base:POINTER;
     NumberOfRelocations:LONGWORD;
     Relocation:PWordArray;
     RelocationCounter:LONGINT;
     RelocationPointer:POINTER;
     RelocationType:LONGWORD;
     RelocationOffset: LONGWORD;
     RelocValWORD: WORD;
     RelocValPOINTER: POINTER;
     RelocOffset: Integer;
 BEGIN
  if IsSkipRelocation then
  begin
    Result := True;
    Exit;
  end;


  IF ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress<>0 THEN
  BEGIN
    RESULT:=FALSE;
    Relocations:=ConvertPointer(ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress);
    RelocationOffset := LONGWORD(ImageBase)-ImageNTHeaders.OptionalHeader.ImageBase;
    Position:=0;
    WHILE ASSIGNED(Relocations) AND (Position<ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size) DO
    BEGIN
      BaseRelocation:=PImageBaseRelocation(Relocations);
      Base:=ConvertPointer(BaseRelocation^.VirtualAddress);
      IF NOT ASSIGNED(Base) THEN EXIT;
      NumberOfRelocations:=(BaseRelocation^.SizeOfBlock-SIZEOF(TImageBaseRelocation)) DIV SIZEOF(WORD);
      Relocation:=POINTER(LONGWORD(LONGWORD(BaseRelocation)+SIZEOF(TImageBaseRelocation)));  
      FOR RelocationCounter:=0 TO NumberOfRelocations-1 DO
      BEGIN
        RelocationPointer:=POINTER(LONGWORD(LONGWORD(Base)+(Relocation^[RelocationCounter] AND $FFF)));
        RelocationType:=Relocation^[RelocationCounter] SHR 12;
        RelocOffset := 0;
        CASE RelocationType OF
          IMAGE_REL_BASED_ABSOLUTE:BEGIN
          END;
          IMAGE_REL_BASED_HIGH:BEGIN
            RelocValWORD := (LONGWORD(((LONGWORD(PWORD(RelocationPointer)^+RelocationOffset)))) SHR 16) AND $FFFF;
            if ASSIGNED (OnRelocWORD) then
            begin
              OnRelocWORD (Self, RelocationPointer, RelocOffset);
              RelocValWORD := RelocValWORD + RelocOffset;
            end;
            PWORD(RelocationPointer)^ := RelocValWORD;
          END;
          IMAGE_REL_BASED_LOW:BEGIN
            RelocValWORD := LONGWORD(((LONGWORD(PWORD(RelocationPointer)^+RelocationOffset)))) AND $FFFF;
            if ASSIGNED (OnRelocWORD) then
            begin
              OnRelocWORD (Self, RelocationPointer, RelocOffset);
              RelocValWORD := RelocValWORD + RelocOffset;
            end;
            PWORD(RelocationPointer)^:= RelocValWORD;
          END;
          IMAGE_REL_BASED_HIGHLOW:BEGIN
            RelocValPOINTER := POINTER((LONGWORD(LONGWORD(PPOINTER(RelocationPointer)^)+RelocationOffset)));
            if ASSIGNED (OnRelocPOINTER) then
            begin
              OnRelocPOINTER (Self, RelocationPointer, RelocOffset);
              RelocValPOINTER := POINTER (Integer(RelocValPOINTER) + RelocOffset);
            end;
            PPOINTER(RelocationPointer)^ := RelocValPOINTER;
          END;
          IMAGE_REL_BASED_HIGHADJ:BEGIN
           // ???
          END;
          IMAGE_REL_BASED_MIPS_JMPADDR:BEGIN
           // Only for MIPS CPUs ;)
          END;
        END;
      END;
      Relocations:=POINTER(LONGWORD(LONGWORD(Relocations)+BaseRelocation^.SizeOfBlock));
      INC(Position,BaseRelocation^.SizeOfBlock);
    END;
  END;
  RESULT:=TRUE;
END;

FUNCTION TDLLLoader.ProtectSections (ImageNTHeaders:TImageNTHeaders; OldProtect:LONGWORD):BOOLEAN;
 VAR I:INTEGER;
     Characteristics:LONGWORD;
     Flags:LONGWORD;
 BEGIN
  RESULT:=FALSE;
  IF ImageNTHeaders.FileHeader.NumberOfSections>0 THEN BEGIN
   FOR I:=0 TO ImageNTHeaders.FileHeader.NumberOfSections-1 DO BEGIN
    Characteristics:=Sections[I].Characteristics;
    Flags:=0;
    IF (Characteristics AND IMAGE_SCN_MEM_EXECUTE)<>0 THEN BEGIN
     IF (Characteristics AND IMAGE_SCN_MEM_READ)<>0 THEN BEGIN
      IF (Characteristics AND IMAGE_SCN_MEM_WRITE)<>0 THEN BEGIN
       Flags:=Flags OR PAGE_EXECUTE_READWRITE;
      END ELSE BEGIN
       Flags:=Flags OR PAGE_EXECUTE_READ;
      END;
     END ELSE IF (Characteristics AND IMAGE_SCN_MEM_WRITE)<>0 THEN BEGIN
      Flags:=Flags OR PAGE_EXECUTE_WRITECOPY;
     END ELSE BEGIN
      Flags:=Flags OR PAGE_EXECUTE;
     END;
    END ELSE IF (Characteristics AND IMAGE_SCN_MEM_READ)<>0 THEN BEGIN
     IF (Characteristics AND IMAGE_SCN_MEM_WRITE)<>0 THEN BEGIN
      Flags:=Flags OR PAGE_READWRITE;
     END ELSE BEGIN
      Flags:=Flags OR PAGE_READONLY;
     END;
    END ELSE IF (Characteristics AND IMAGE_SCN_MEM_WRITE)<>0 THEN BEGIN
     Flags:=Flags OR PAGE_WRITECOPY;
    END ELSE BEGIN
     Flags:=Flags OR PAGE_NOACCESS;
    END;
    IF (Characteristics AND IMAGE_SCN_MEM_NOT_CACHED)<>0 THEN BEGIN
     Flags:=Flags OR PAGE_NOCACHE;
    END;
    VirtualProtect(Sections[I].Base,Sections[I].Size,Flags,OldProtect);
   END;
   RESULT:=TRUE;
  END;
END;


FUNCTION TDLLLoader.Load(Stream:TStream; Reserved:Pointer=NIL):BOOLEAN;
VAR ImageDOSHeader:TImageDOSHeader;
    ImageNTHeaders:TImageNTHeaders;
    OldProtect:LONGWORD;
BEGIN
 RESULT:=FALSE;
 IF ASSIGNED(Stream) THEN BEGIN
  Stream.Seek(0, soFromBeginning);
  IF Stream.Size>0 THEN BEGIN
   IF ReadImageHeaders (Stream, ImageDOSHeader, ImageNTHeaders) THEN BEGIN
    IF InitializeImage (Stream, ImageNTHeaders, OldProtect) THEN BEGIN
     IF ReadSections (Stream, ImageNTHeaders) THEN BEGIN
      IF ProcessRelocations (ImageNTHeaders) THEN BEGIN
       IF ProcessImports (ImageNTHeaders) THEN BEGIN
        IF ProtectSections (ImageNTHeaders, OldProtect) THEN BEGIN
         IF InitializeLibrary(ImageNTHeaders, Reserved) THEN BEGIN
          IF ProcessExports (ImageNTHeaders) THEN BEGIN
           RESULT:=TRUE;
          END;
         END;
        END;
       END;
      END;
     END;
    END;
   END;
  END;
 END;
END;



FUNCTION TDLLLoader.Unload (Reserved:Pointer=NIL):BOOLEAN;
VAR I,J:INTEGER;
BEGIN
 RESULT:=TRUE;

 if not IsSkipDLLProc then
   IF @DLLProc<>NIL THEN
    DLLProc(LONGWORD(ImageBase),DLL_PROCESS_DETACH,Reserved);

 FOR I:=0 TO LENGTH(Sections)-1 DO
   IF ASSIGNED(Sections[I].Base) THEN
     if not VirtualFree(Sections[I].Base,0,MEM_RELEASE) then
       ;
//       OutputDebugString(PChar('VirtualFree Sections error ' + SysErrorMessage(GetLastError)));
 SETLENGTH(Sections,0);

 FOR I:=0 TO LENGTH(ExternalLibraryArray)-1 DO
 BEGIN
  ExternalLibraryArray[I].LibraryName:='';
  FreeLibrary(ExternalLibraryArray[I].LibraryHandle);
 END;
 SETLENGTH(ExternalLibraryArray,0);

 try
   FOR I:=0 TO LENGTH(ImportArray)-1 DO BEGIN
    FOR J:=0 TO LENGTH(ImportArray[I].Entries)-1 DO BEGIN
     ImportArray[I].Entries[J].Name:='';
    END;
    SETLENGTH(ImportArray[I].Entries,0);
   END;
   SETLENGTH(ImportArray,0);
 except
   OutputDebugString('Free ImportArray Exception');
 end;

 FOR I:=0 TO LENGTH(ExportArray)-1 DO ExportArray[I].Name:='';
 SETLENGTH(ExportArray,0);
 if not VirtualFree(ImageBase,0,MEM_RELEASE) then
   OutputDebugString('VirtualFree ImageBase error');
   
 IF ASSIGNED(ExportTree) THEN BEGIN
  ExportTree.Destroy;
  ExportTree:=NIL;
 END;

END;

FUNCTION TDLLLoader.FindExport(FunctionName:STRING):POINTER;
VAR I:INTEGER;
BEGIN
 RESULT:=NIL;
 IF ASSIGNED(ExportTree) THEN BEGIN   
  ExportTree.Find(FunctionName,RESULT);
//  WriteLn ('FindExport ', FunctionName, ' $', IntToHex(DWORD(RESULT), 8));
 END ELSE BEGIN
  FOR I:=0 TO LENGTH(ExportArray)-1 DO BEGIN
   IF ExportArray[I].Name=FunctionName THEN BEGIN
    RESULT:=ExportArray[I].FunctionPointer;
    EXIT;
   END;
  END;
 END;
END;

FUNCTION TDLLLoader.FindExportPerIndex(FunctionIndex:INTEGER):POINTER;
VAR I:INTEGER;
BEGIN
 RESULT:=NIL;
 FOR I:=0 TO LENGTH(ExportArray)-1 DO BEGIN
  IF ExportArray[I].Index=FunctionIndex THEN BEGIN
   RESULT:=ExportArray[I].FunctionPointer;
   EXIT;
  END;
 END;
END;

FUNCTION TDLLLoaderEx.GetExportList:TStringList;
VAR I:INTEGER;
BEGIN
 RESULT:=TStringList.Create;
 FOR I:=0 TO LENGTH(ExportArray)-1 DO RESULT.Add(ExportArray[I].Name);
 RESULT.Sort;
END;

PROCEDURE TDLLLoaderEx.EnumExportList(Sender:POINTER; Enum: TEnumProc);
VAR I:INTEGER;
BEGIN
  FOR I:=0 TO LENGTH(ExportArray)-1 DO
    Enum(Sender, ExportArray[I].Name, ExportArray[I].Index, ExportArray[I].FunctionPointer);
END;

FUNCTION TDLLLoaderEx.LoadRES(ResType, ResName: String; Reserved:Pointer=NIL):BOOLEAN;
VAR
  RS :TResourceStream;
begin
  RS := TResourceStream.create(hInstance, ResName, PChar(ResType));
  try
    RESULT := Load(RS, Reserved);
  finally
    RS.Free;
  end;
end;

FUNCTION TDLLLoaderEx.LoadDLL(DLL:STRING; Reserved:Pointer=NIL):BOOLEAN;
VAR
  MM :TMemoryStream;
BEGIN
  result := False;
  IF NOT FileExists(DLL) THEN exit;
  MM :=TMemoryStream.Create();
  TRY
    MM.LoadFromFile(DLL);
    RESULT := Load(MM, Reserved);
  FINALLY
    MM.Free;
  END;
END;

PROCEDURE TDLLLoaderEx.AddDependLoader (LibName: STRING; DLL: TDLLLoader);
begin
  if Not Assigned (DependLoaders) then
    DependLoaders := TStringList.Create;  
  DependLoaders.AddObject(LibName, DLL);
end;


FUNCTION TDLLLoaderEx.GetDependLibrary (LibName: PChar): THandle;
var
  Index: Integer;
  DLL: TDLLLoader;
begin
  Result := 0;
  if Assigned (DependLoaders) then
  begin
    Index := DependLoaders.IndexOf(StrPas(LibName));
    if Index >= 0 then
    begin
      DLL := DependLoaders.Objects[Index] as TDLLLoader;
      if Assigned (DLL) then
        Result := THandle (DLL.ImageBase);
    end;
  end;
end;

FUNCTION TDLLLoaderEx.GetDependFunction (LibHandle: THandle; ProcName: PChar): Pointer;
var
  Index, FunIndex: Integer;
  DLL: TDLLLoader;
  IterHandle: THandle;
begin
  Result := NIL;
  if Assigned (DependLoaders) then
  begin
    for Index := 0 to DependLoaders.Count - 1 do
    begin
      DLL := DependLoaders.Objects[Index] as TDLLLoader;
      if Assigned (DLL) then
      begin
        IterHandle := THandle (DLL.ImageBase);
        if IterHandle = LibHandle then
        begin
          FunIndex := Integer(ProcName);
          if FunIndex < 1000 then
          begin
            Result := DLL.FindExportPerIndex(FunIndex);
            WriteLn ('GetDependFunction ', FunIndex, #9, IntToHex(DWORD(Result), 8));
          end else
            Result := DLL.FindExport(StrPas(ProcName));
          Break;
        end;
      end;
    end;
  end;
end;

FUNCTION TDLLLoaderEx.LoadLibrary (lpLibFileName: PChar): HMODULE;
begin
  Result := GetDependLibrary (lpLibFileName);
  if Result = 0 then
    Result := Inherited LoadLibrary (lpLibFileName);
end;

FUNCTION TDLLLoaderEx.GetProcAddress (hModule: HMODULE; lpProcName: LPCSTR): POINTER;
begin
  Result := GetDependFunction (hModule, lpProcName);
  if Result = NIL then
    Result := Inherited GetProcAddress (hModule, lpProcName);
end;

END.
