unit DLLLoaderUnit;

interface

uses
  Windows;

type
  TImportItem = record
    Name: string;
    PProcVar: ^Pointer;
  end;

  TwordArr = array [0..0] of word;
  PwordArr = ^TwordArr;
  TdwordArr = array [0..0] of dword;
  PdwordArr = ^TdwordArr;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    OriginalFirstThunk: dword;
    TimeDateStamp: dword;
    ForwarderChain: dword;
    Name: dword;
    FirstThunk: dword;
  end;

  PImageBaseRelocation= ^TImageBaseRelocation;
  TImageBaseRelocation = packed record
    VirtualAddress: cardinal;
    SizeOfBlock: cardinal;
  end;

  TDllEntryProc = function(hinstDLL: HMODULE; dwReason: dword; lpvReserved: Pointer): Boolean; stdcall;

  TStringArray = array of string;

  TLibInfo = record
    ImageBase: Pointer;
    DllProc: TDllEntryProc;
    LibsUsed: TStringArray;
  end;

  PLibInfo = ^TLibInfo;
  PPointer = ^Pointer;

  TSections = array [0..100000] of TImageSectionHeader;

const
  IMPORTED_NAME_OFFSET = $00000002;
  IMAGE_ORDINAL_FLAG32 = $80000000;
  IMAGE_ORDINAL_MASK32 = $0000FFFF;

function xLoadLibrary(Src: Pointer; Imports: array of TImportItem): TLibInfo;
function xFreeLibrary(LoadedLib: TLibInfo): boolean;

implementation

function xFreeLibrary(LoadedLib: TLibInfo): boolean;
var
  ObjectLoop: integer;
begin
  Result := False;
  with LoadedLib do
  begin
    if @DllProc <> nil then
    begin
       DllProc(HModule(LoadedLib.ImageBase), DLL_PROCESS_DETACH, nil);
    end;
    for ObjectLoop := 0 to Length(LibsUsed) - 1 do
    begin
      if ObjectLoop >= Length(LibsUsed) then Exit;
      FreeLibrary(GetModuleHandle(pchar(LibsUsed[ObjectLoop])));
    end;
    SetLength(LibsUsed, 0);
  end;
  VirtualFree(LoadedLib.ImageBase, 0, MEM_RELEASE);
  Result := True;
end;

function xLoadLibrary(Src: Pointer; Imports: array of TImportItem): TLibInfo;
var
  ImageBase: pointer;
  ImageBaseDelta: integer;
  ImageNtHeaders: PImageNtHeaders;
  PSections: ^TSections;
  SectionLoop: integer;
  SectionBase: pointer;
  VirtualSectionSize, RawSectionSize: cardinal;
  OldProtect: cardinal;
  NewLibInfo: TLibInfo;

  function StrToInt(S: string): integer;
  begin
   Val(S, Result, Result);
  end;

  procedure Add(Strings: TStringArray; Text: string);
  begin
    SetLength(Strings, Length(Strings) + 1);
    Strings[Length(Strings)-1] := Text;
  end;

  function Find(Strings: array of string; Text: string; var Index: integer): boolean;
  var
    StringLoop: integer;
  begin
    Result := False;
    for StringLoop := 0 to Length(Strings) - 1 do
    begin
      if lstrcmpi(pchar(Strings[StringLoop]), pchar(Text)) = 0 then
      begin
        Index := StringLoop;
        Result := True;
      end;
    end;
  end;

  function GetSectionProtection(ImageScn: cardinal): cardinal;
  begin
    Result := 0;
    if (ImageScn and IMAGE_SCN_MEM_NOT_CACHED) <> 0 then
    begin
    Result := Result or PAGE_NOCACHE;
    end;
    if (ImageScn and IMAGE_SCN_MEM_EXECUTE) <> 0 then
    begin
      if (ImageScn and IMAGE_SCN_MEM_READ)<> 0 then
      begin
        if (ImageScn and IMAGE_SCN_MEM_WRITE)<> 0 then
        begin
          Result := Result or PAGE_EXECUTE_READWRITE
        end
        else
        begin
          Result := Result or PAGE_EXECUTE_READ
        end;
      end
      else if (ImageScn and IMAGE_SCN_MEM_WRITE) <> 0 then
      begin
        Result := Result or PAGE_EXECUTE_WRITECOPY
      end
      else
      begin
        Result := Result or PAGE_EXECUTE
      end;
    end
    else if (ImageScn and IMAGE_SCN_MEM_READ)<> 0 then
    begin
      if (ImageScn and IMAGE_SCN_MEM_WRITE) <> 0 then
      begin
        Result := Result or PAGE_READWRITE
      end
      else
      begin
        Result := Result or PAGE_READONLY
      end
    end
    else if (ImageScn and IMAGE_SCN_MEM_WRITE) <> 0 then
    begin
      Result := Result or PAGE_WRITECOPY
    end
    else
    begin
      Result := Result or PAGE_NOACCESS;
    end;
  end;

  procedure ProcessExports(PExports: PImageExportDirectory; BlockSize: cardinal);
  var
    ExportLoop: byte;
    ImportedFn: cardinal;
    PFnName: pchar;
    FnIndex: dword;

    function IsForwarderString(Data: pchar): boolean;
    begin
      Result := Data > PExports;
      if Result then Result := cardinal(Data - PExports) < BlockSize;
    end;

    function GetForwardedSymbol(ForwarderString: pchar):pointer;
    var
      sForwarderString, DllName: string;
      ForwarderLoop: integer;
      LibHandle: HModule;
    begin
      sForwarderString := ForwarderString;
      while ForwarderString^ <> '.' do
      begin
        Inc(ForwarderString);
      end;
      DllName := Copy(sForwarderString, 1, pos('.', sForwarderString) - 1);
      if not Find(NewLibInfo.LibsUsed, DllName, ForwarderLoop) then
      begin
        LibHandle := LoadLibrary(pchar(DllName));
        Add(NewLibInfo.LibsUsed, DllName);
      end
      else
      begin
        LibHandle := cardinal(NewLibInfo.LibsUsed[ForwarderLoop]);
      end;
      if ForwarderString^ = '#' then ForwarderString := pointer(StrToInt((ForwarderString + 1)));
      Result := GetProcAddress(LibHandle, ForwarderString);
    end;

  begin
    for ExportLoop := 0 to PExports.NumberOfNames - 1 do
    begin
      PFnName := pchar(PdwordArr(cardinal(PExports.AddressOfNames) + cardinal(ImageBase))^[ExportLoop] + cardinal(ImageBase));
      for ImportedFn := low(Imports) to high(Imports) do
      begin
        if Imports[ImportedFn].Name = PFnName then
        begin
          FnIndex := PwordArr(cardinal(PExports.AddressOfNameOrdinals) + cardinal(ImageBase))^[ExportLoop];
          Imports[ImportedFn].PProcVar^ := pointer(PdwordArr(cardinal(PExports.AddressOfFunctions) + cardinal(ImageBase))^[FnIndex] + cardinal(ImageBase));
          if IsForwarderString(Imports[ImportedFn].PProcVar^)then
          begin
            Imports[ImportedFn].PProcVar^ := GetForwardedSymbol(Imports[ImportedFn].PProcVar^);
          end;
        end;
      end;
    end;
  end;

  procedure ProcessRelocs(PRelocs:PImageBaseRelocation);
  var
    PReloc: PImageBaseRelocation;
    RelocsSize: cardinal;
    Reloc: PWord;
    ModCount: cardinal;
    RelocLoop: cardinal;
  begin
    PReloc := PRelocs;
    RelocsSize := ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size;
    while cardinal(PReloc) - cardinal(PRelocs) < RelocsSize do
    begin
      ModCount := (PReloc.SizeOfBlock-Sizeof(PReloc^)) div 2;
      Reloc := pointer(cardinal(PReloc)+sizeof(PReloc^));
      for RelocLoop := 0 to ModCount - 1 do
      begin
        if Reloc^ and $f000 <> 0 then Inc(pdword(cardinal(ImageBase) + PReloc.VirtualAddress + (Reloc^ and $0fff))^, ImageBaseDelta);
        Inc(Reloc);
      end;
      PReloc := pointer(Reloc);
    end;
  end;

  procedure ProcessImports(PImports: PImageImportDescriptor);
  var
    PImport: PImageImportDescriptor;
    Import: LPDword;
    PImportedName: pchar;
    LibHandle: HModule;
    ProcAddress: pointer;
    PLibName: pchar;
    ImportLoop: integer;

    function IsImportByOrdinal(ImportDescriptor: dword; HLib: THandle): boolean;
    begin
      Result := (ImportDescriptor and IMAGE_ORDINAL_FLAG32) <> 0;
    end;

  begin
    PImport := PImports;
    while PImport.Name<>0 do
    begin
      PLibName := pchar(cardinal(PImport.Name) + cardinal(ImageBase));
      if not Find(NewLibInfo.LibsUsed, PLibName, ImportLoop) then
      begin
        LibHandle := LoadLibrary(PLibName);
        Add(NewLibInfo.LibsUsed, PLibName);
      end
      else
      begin
        LibHandle := cardinal(NewLibInfo.LibsUsed[ImportLoop]);
      end;
      if PImport.TimeDateStamp = 0 then
      begin
        Import := LPDword(pImport.FirstThunk+cardinal(ImageBase))
      end
      else
      begin
        Import := LPDword(pImport.OriginalFirstThunk + cardinal(ImageBase));
      end;
      while Import^ <> 0 do
      begin
        if IsImportByOrdinal(Import^, LibHandle) then
        begin
          ProcAddress := GetProcAddress(LibHandle, pchar(Import^ and $ffff))
        end
        else
        begin
          PImportedName := pchar(Import^ + cardinal(ImageBase) + IMPORTED_NAME_OFFSET);
          ProcAddress := GetProcAddress(LibHandle, PImportedName);
        end;
        PPointer(Import)^ := ProcAddress;
        Inc(Import);
      end;
      Inc(PImport);
    end;
  end;

begin
  ImageNtHeaders := pointer(int64(cardinal(Src)) + PImageDosHeader(Src)._lfanew);
  ImageBase := VirtualAlloc(nil, ImageNtHeaders.OptionalHeader.SizeOfImage, MEM_RESERVE, PAGE_NOACCESS);
  ImageBaseDelta := cardinal(ImageBase) - ImageNtHeaders.OptionalHeader.ImageBase;
  SectionBase := VirtualAlloc(ImageBase, ImageNtHeaders.OptionalHeader.SizeOfHeaders, MEM_COMMIT, PAGE_READWRITE);
  Move(Src^, SectionBase^, ImageNtHeaders.OptionalHeader.SizeOfHeaders);
  VirtualProtect(SectionBase, ImageNtHeaders.OptionalHeader.SizeOfHeaders, PAGE_READONLY, OldProtect);
  PSections := pointer(pchar(@(ImageNtHeaders.OptionalHeader)) + ImageNtHeaders.FileHeader.SizeOfOptionalHeader);
  for SectionLoop := 0 to ImageNtHeaders.FileHeader.NumberOfSections - 1 do
  begin
    VirtualSectionSize := PSections[SectionLoop].Misc.VirtualSize;
    RawSectionSize := PSections[SectionLoop].SizeOfRawData;
    if VirtualSectionSize < RawSectionSize then
    begin
      VirtualSectionSize := VirtualSectionSize xor RawSectionSize;
      RawSectionSize := VirtualSectionSize xor RawSectionSize;
      VirtualSectionSize := VirtualSectionSize xor RawSectionSize;
    end;
    SectionBase := VirtualAlloc(PSections[SectionLoop].VirtualAddress + pchar(ImageBase), VirtualSectionSize, MEM_COMMIT, PAGE_READWRITE);
    FillChar(SectionBase^, VirtualSectionSize, 0);
    Move((pchar(src) + PSections[SectionLoop].PointerToRawData)^, SectionBase^, RawSectionSize);
  end;
  NewLibInfo.DllProc := TDllEntryProc(ImageNtHeaders.OptionalHeader.AddressOfEntryPoint + cardinal(ImageBase));
  NewLibInfo.ImageBase := ImageBase;
  SetLength(NewLibInfo.LibsUsed, 0);
  if ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress <> 0 then ProcessRelocs(pointer(ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress + cardinal(ImageBase)));
  if ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress <> 0 then ProcessImports(pointer(ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress + cardinal(ImageBase)));
  for SectionLoop := 0 to ImageNtHeaders.FileHeader.NumberOfSections - 1 do
  begin
    VirtualProtect(PSections[SectionLoop].VirtualAddress + pchar(ImageBase), PSections[SectionLoop].Misc.VirtualSize, GetSectionProtection(PSections[SectionLoop].Characteristics), OldProtect);
  end;
  if @NewLibInfo.DllProc <> nil then
  begin
    if not NewLibInfo.DllProc(cardinal(ImageBase), DLL_PROCESS_ATTACH, nil) then
    begin
      NewLibInfo.DllProc := nil;
      xFreeLibrary(Result);
    end;
  end;
  if ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress <> 0 then ProcessExports(pointer(ImageNtHeaders..Dat OptionalHeader aDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress + cardinal(ImageBase)), ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size);
  Result := NewLibInfo;
end;

end.

//---- END DELPHI CODE ----
