unit FindAddress;

interface
uses
  windows, classes, SysUtils, Math;

type
  TReportProc = Procedure (Sender: Pointer; Index: Integer; Size: Integer; var BreakReport: BOOL); Stdcall;
  TReportAddress = Procedure (Sender: Pointer; Found: Pointer; var BreakReport: BOOL); Stdcall;
  TSectionCallback = procedure (Sender: Pointer; CodeBase: Pointer; CodeSize: Integer); Stdcall;
  TRelocEnumProc = function (Sender: Pointer; RelocType: DWORD; RelocAddr: Pointer): BOOL; Stdcall;
  TExportEnumProc = function (Sender: Pointer; Name: PChar; Index: Integer; Entry: Pointer): BOOL; Stdcall;


function MakeTemplate (Base: Pointer; Size: Integer): THandle; stdcall;
function FreeTemplate (TmpHandle: THandle): LongBool; Stdcall;
function CompareTemplate (Sender: Pointer; TmpA, TmpB: THandle; Report: TReportProc): LongBool; Stdcall;
function FineAddress (TmpHandle: THandle; ToFind: PChar; ToFindSize: Integer; var List: TList): LongBool; Stdcall;
function SearchAddress (TmpHandle: THandle; Sender: Pointer; ToFind: PChar; ToFindSize: Integer; Report: TReportAddress): LongBool; Stdcall;

Function GetCodeSectionMemory (Handle: THandle; CallBack, Sender: Pointer):LongBool;stdcall;
Function GetDataSectionMemory (Handle: THandle; CallBack, Sender: Pointer):LongBool;stdcall;
Function GetSectionMemory (Handle: THandle; SectionName: PChar; var CodeBase: Pointer; var CodeSize: Integer):LongBool;stdcall;
function GetEntryPoint (Handle: THandle): Pointer; Stdcall;
function GetModuleSize (Handle: THandle): Integer; Stdcall;
function GetImageNtHeaders (Handle: THandle): PImageNtHeaders; Stdcall;
function GetSectionHeaders(Handle: THandle; var NumberOfSection: Integer):PImageSectionHeader; STDCALL;
function GetImageBase(CodeAddr: Pointer):Pointer; Stdcall;
function EnumRelocations (Handle: THandle; EnumProc, Sender: Pointer):BOOLEAN; STDCALL;
function EnumExports (Handle: THandle; EnumProc, Sender: Pointer):BOOLEAN; STDCALL;

implementation

function IsPEHead (DosHeader: PImageDosHeader): BOOL;
var
  ImageNTHeaders       : PImageNTHeaders;
  pSignature           : Pchar;
begin
  Result := False;
  pSignature := @DosHeader.e_magic;
  if pSignature[0] = 'M' then
  if pSignature[1] = 'Z' then
  begin
    ImageNTHeaders:=Pointer(DWORD(DosHeader) + DWORD(DosHeader._lfanew));
    pSignature := @ImageNTHeaders.Signature;
    if pSignature[0] = 'P' then
    if pSignature[1] = 'E' then
    begin
      result := True;
    end;
  end;
end;

function GetImageBase(CodeAddr: Pointer):Pointer; Stdcall;
var
  dwBase, MinBase :dword;
  mbi_thunk            :MEMORY_BASIC_INFORMATION;
begin
  Result := nil;
  if VirtualQuery (CodeAddr, mbi_thunk, sizeof(mbi_thunk)) <> sizeof(mbi_thunk) Then Exit;
  dwBase := DWORD(mbi_thunk.BaseAddress);
  MinBase := DWORD(mbi_thunk.AllocationBase);

  repeat
    dwBase := dwBase  and $FFFF0000;
    if dwBase = 0 then Break;

    if IsPEHead (Pointer(dwBase)) then
    begin
      Result := Pointer(dwBase);
      Exit;
    end;

    if IsPEHead (Pointer(dwBase + $28)) then
    begin
      Result := Pointer(dwBase + $28);
      Exit;
    end;

    dec(dwBase);
  until dwBase <= MinBase;
end;

function GetSectionHeaders(Handle: THandle; var NumberOfSection: Integer):PImageSectionHeader; STDCALL;
var
  ImageBase: PChar;
  ImageDosHeader: PImageDosHeader;
  ImageNtHeaders: PImageNtHeaders;
  NtHeaderOffset, SectionOffset: WORD;
begin
  if Handle = 0 then Handle := GetModuleHandle(nil);
  ImageBase := Pointer(Handle);

  ImageDosHeader := @ImageBase[0];
  NtHeaderOffset := ImageDosHeader._lfanew;
  ImageNtHeaders := @ImageBase[NtHeaderOffset];
  NumberOfSection := ImageNtHeaders.FileHeader.NumberOfSections;  
  SectionOffset := NtHeaderOffset + ImageNtHeaders.FileHeader.SizeOfOptionalHeader + SizeOf(TImageFileHeader) + SizeOf(DWORD);
  Result := @ImageBase[SectionOffset];
end;


type
  LPTSectionArray = ^TSectionArray;
  TSectionArray = array[byte] of TImageSectionHeader;

function GetSections(ModuleHandle: THandle; var NumberOfSection: Integer):LPTSectionArray;
var
  ImageBase: PChar;
  ImageDosHeader: PImageDosHeader;
  ImageNtHeaders: PImageNtHeaders;
  NtHeaderOffset, SectionOffset, SectionHeadSize: WORD;
begin
  if ModuleHandle = 0 then ModuleHandle := GetModuleHandle(nil);
  ImageBase := Pointer(ModuleHandle);
  
  ImageDosHeader := @ImageBase[0];
  NtHeaderOffset := ImageDosHeader._lfanew;
  ImageNtHeaders := @ImageBase[NtHeaderOffset];
  NumberOfSection := ImageNtHeaders.FileHeader.NumberOfSections;

  SectionOffset := NtHeaderOffset + ImageNtHeaders.FileHeader.SizeOfOptionalHeader + SizeOf(TImageFileHeader) + SizeOf(DWORD);

  SectionHeadSize := NumberOfSection * SizeOf(TImageSectionHeader);
  Result := AllocMem (SectionHeadSize);
  CopyMemory(Result, @ImageBase[SectionOffset], SectionHeadSize);
end;

function GetImageNtHeaders (Handle: THandle): PImageNtHeaders; Stdcall;
var
  ImageDosHeader: PImageDosHeader;
  NtHeaderOffset: WORD;
  ImageBase: PChar;
begin
  if Handle = 0 then Handle := GetModuleHandle(nil);
  ImageBase := Pointer(Handle);
  ImageDosHeader := Pointer(Handle);
  NtHeaderOffset := ImageDosHeader._lfanew;
  Result := @ImageBase[NtHeaderOffset];
end;

function GetEntryPoint (Handle: THandle): Pointer; Stdcall;
var
  ImageNtHeaders: PImageNtHeaders;
begin
  ImageNtHeaders := GetImageNtHeaders (Handle);
  Result := Pointer(Handle + ImageNtHeaders.OptionalHeader.AddressOfEntryPoint);
end;

function GetModuleSize (Handle: THandle): Integer; Stdcall;
var
  ImageNtHeaders: PImageNtHeaders;
begin
  ImageNtHeaders := GetImageNtHeaders (Handle);
  Result := ImageNtHeaders.OptionalHeader.SizeOfImage;
end;

Function GetSectionMemory (Handle: THandle; SectionName: PChar; var CodeBase: Pointer; var CodeSize: Integer):LongBool;stdcall;
var
  SectionBase: LPTSectionArray;
  SectionHeader: PImageSectionHeader;
  SectionCount, I: Integer;
  SectName, InputSectName: String;
begin
  Result := False;
  if Handle = 0 then Handle := GetModuleHandle(nil);
  SectionBase := GetSections(Handle, SectionCount);
  InputSectName := UpperCase(StrPas(SectionName));
  
  if assigned (SectionBase) then
  begin
    for I := 0 to SectionCount -1 do
    begin
      SectionHeader := @SectionBase[I];
      SectName := UpperCase(StrPas(@SectionHeader.Name[0]));

      if SectName = InputSectName then
      begin
        CodeBase := POINTER( Handle + SectionHeader.VirtualAddress);
        CodeSize := SectionHeader.SizeOfRawData;
        Result := True;
        Exit;
      end;
    end;
  end;

  FreeMem(SectionBase);
end;

FUNCTION ConvertPointer(Image: THandle; Header: PImageSectionHeader; NumberOfSection: Integer; RVA:LONGWORD):POINTER;
VAR
  Index:INTEGER;
  SectionBase: LONGWORD;
BEGIN
  RESULT:=NIL;
  FOR Index:=0 TO NumberOfSection - 1 DO
  BEGIN
    IF (RVA<(Header.VirtualAddress+Header.SizeOfRawData)) AND (RVA>=Header.VirtualAddress) THEN
    BEGIN
      SectionBase := Header.VirtualAddress + Image;
      RESULT:=POINTER(RVA-Header.VirtualAddress + SectionBase);
      EXIT;
    END;
    INC (Header);
  END;
END;

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

Type
     PLongWordArray=^TLongWordArray;
     TLongWordArray=ARRAY [0..(2147483647 DIV SIZEOF(LONGWORD))-1] OF LONGWORD;

FUNCTION EnumExports (Handle: THandle; EnumProc, Sender: Pointer):BOOLEAN; STDCALL;
 VAR I:INTEGER;
     ImageNTHeaders: PImageNTHeaders;
     ExportDirectory:PImageExportDirectory;
     SectionHeader: PImageSectionHeader;
     ExportDirectorySize:LONGWORD;
     FunctionNamePointer:POINTER;
     FunctionName:PCHAR;
     FunctionIndexPointer:POINTER;
     FunctionIndex:LONGWORD;
     FunctionPointer:POINTER;
     ForwarderCharPointer:PCHAR;   
     NumberOfSection: Integer;
     ForwarderString:STRING;
     ForwarderLibrary:STRING;
     ForwarderLibraryHandle:HINST;
     RelocEnumer: TExportEnumProc absolute EnumProc;
BEGIN
  Result := False;
  if Handle = 0 then Handle := GetModuleHandle(nil);
  ImageNTHeaders := GetImageNtHeaders (Handle);
  SectionHeader := GetSectionHeaders (Handle, NumberOfSection);

  IF ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress<>0 THEN
  BEGIN
    ExportDirectory:=ConvertPointer(Handle, SectionHeader, NumberOfSection, ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
    IF ASSIGNED(ExportDirectory) THEN
    BEGIN
      ExportDirectorySize:=ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
      FOR I:=0 TO ExportDirectory^.NumberOfNames-1 DO
      BEGIN
        FunctionNamePointer:=ConvertPointer(Handle, SectionHeader, NumberOfSection, LONGWORD(ExportDirectory^.AddressOfNames));
        FunctionNamePointer:=ConvertPointer(Handle, SectionHeader, NumberOfSection, PLongWordArray(FunctionNamePointer)^[I]);
        FunctionName:=FunctionNamePointer;
        FunctionIndexPointer:=ConvertPointer(Handle, SectionHeader, NumberOfSection, LONGWORD(ExportDirectory^.AddressOfNameOrdinals));
        FunctionIndex:=PWordArray(FunctionIndexPointer)^[I];
        FunctionPointer:=ConvertPointer(Handle, SectionHeader, NumberOfSection, LONGWORD(ExportDirectory^.AddressOfFunctions));
        FunctionPointer:=ConvertPointer(Handle, SectionHeader, NumberOfSection, PLongWordArray(FunctionPointer)^[FunctionIndex]);
        IF (LONGWORD(ExportDirectory)<LONGWORD(FunctionPointer)) AND (LONGWORD(FunctionPointer)<(LONGWORD(ExportDirectory)+ExportDirectorySize)) THEN
        BEGIN
          ForwarderCharPointer:=FunctionPointer;
          ForwarderString:=ForwarderCharPointer;
          WHILE ForwarderCharPointer^<>'.' DO INC(ForwarderCharPointer);

          IF ForwarderCharPointer^='#' THEN
          BEGIN
             INC(ForwarderCharPointer);
             ForwarderString:=ForwarderCharPointer;
             ForwarderCharPointer:=ConvertPointer(Handle, SectionHeader, NumberOfSection, ParseStringToNumber(ForwarderString));
             ForwarderString:=ForwarderCharPointer;
          END ELSE
          BEGIN
             ForwarderLibrary:=COPY(ForwarderString,1,POS('.',ForwarderString)-1);
             ForwarderString:=ForwarderCharPointer;
             ForwarderLibraryHandle:=LoadLibrary (PChar(ForwarderLibrary));
             FunctionPointer:=GetProcAddress(ForwarderLibraryHandle,PCHAR(ForwarderString));
             FreeLibrary(ForwarderLibraryHandle);
          END;
        END;
        if not RelocEnumer (Sender, FunctionName, FunctionIndex, FunctionPointer) then Exit;
      END
    END;
  END;
  RESULT:=TRUE;
END;


Type
  PImageBaseRelocation=^TImageBaseRelocation;
  TImageBaseRelocation=PACKED RECORD
    VirtualAddress:LONGWORD;
    SizeOfBlock:LONGWORD;
  END;

FUNCTION EnumRelocations (Handle: THandle; EnumProc, Sender: Pointer):BOOLEAN; STDCALL;
 VAR ImageNTHeaders:PImageNtHeaders;
     Relocations:PCHAR;
     Position:LONGWORD;
     BaseRelocation:PImageBaseRelocation;
     Base:POINTER;
     NumberOfRelocations:LONGWORD;
     Relocation:PWordArray;
     RelocationCounter:LONGINT;
     RelocationPointer:POINTER;
     RelocationType:LONGWORD;
     NumberOfSection: Integer;
     SectionHeader: PImageSectionHeader;
     RelocEnumer: TRelocEnumProc absolute EnumProc;
BEGIN
  Result := False;
  if Handle = 0 then Handle := GetModuleHandle(nil);
  ImageNTHeaders := GetImageNtHeaders (Handle);
  SectionHeader := GetSectionHeaders (Handle, NumberOfSection);

  IF ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress<>0 THEN
  BEGIN
    Relocations:=ConvertPointer(Handle, SectionHeader, NumberOfSection, ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress);
    Position:=0;
    WHILE ASSIGNED(Relocations) AND (Position<ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size) DO
    BEGIN
      BaseRelocation:=PImageBaseRelocation(Relocations);
      Base:=ConvertPointer(Handle, SectionHeader, NumberOfSection, BaseRelocation^.VirtualAddress);
      IF NOT ASSIGNED(Base) THEN EXIT;
      NumberOfRelocations:=(BaseRelocation^.SizeOfBlock-SIZEOF(TImageBaseRelocation)) DIV SIZEOF(WORD);
      Relocation:=POINTER(LONGWORD(LONGWORD(BaseRelocation)+SIZEOF(TImageBaseRelocation)));  
      FOR RelocationCounter:=0 TO NumberOfRelocations-1 DO
      BEGIN
        RelocationPointer:=POINTER(LONGWORD(LONGWORD(Base)+(Relocation^[RelocationCounter] AND $FFF)));
        RelocationType:=Relocation^[RelocationCounter] SHR 12;
        if not RelocEnumer (Sender, RelocationType, RelocationPointer) then Exit;
      END;
      Relocations:=POINTER(LONGWORD(LONGWORD(Relocations)+BaseRelocation^.SizeOfBlock));
      INC(Position,BaseRelocation^.SizeOfBlock);
    END;
  END;
  RESULT:=TRUE;
END;


Function GetCodeSectionMemory (Handle: THandle; CallBack, Sender: Pointer):LongBool;stdcall;
var
  SectionBase: LPTSectionArray;
  SectionHeader: PImageSectionHeader;
  SectionCount, I: Integer;
  SectionCallback: TSectionCallback;
  CodeBase: Pointer;
  CodeSize: Integer;
begin
  if Handle = 0 then Handle := GetModuleHandle(nil);
  SectionCallback := CallBack;
  SectionBase := GetSections(Handle, SectionCount);

  if assigned (SectionBase) then
  begin
    for I := 0 to SectionCount -1 do
    begin
      SectionHeader := @SectionBase[I];
      if (SectionHeader.Characteristics AND IMAGE_SCN_MEM_EXECUTE) <> 0 then
      begin
        CodeBase := POINTER( Handle + SectionHeader.VirtualAddress);
        CodeSize := SectionHeader.SizeOfRawData;
        SectionCallback (Sender, CodeBase, CodeSize);
      end;
    end;
  end;

  FreeMem(SectionBase);
  Result := SectionCount > 0;
end;

Function GetDataSectionMemory (Handle: THandle; CallBack, Sender: Pointer):LongBool;stdcall;
var
  SectionBase: LPTSectionArray;
  SectionHeader: PImageSectionHeader;
  SectionCount, I: Integer;
  SectionCallback: TSectionCallback;
  CodeBase: Pointer;
  CodeSize: Integer;
begin
  if Handle = 0 then Handle := GetModuleHandle(nil);
  SectionCallback := CallBack;
  SectionBase := GetSections(Handle, SectionCount);

  if assigned (SectionBase) then
  begin
    for I := 0 to SectionCount -1 do
    begin
      SectionHeader := @SectionBase[I];
      if (SectionHeader.Characteristics AND IMAGE_SCN_MEM_WRITE) <> 0 then
      begin
        CodeBase := POINTER( Handle + SectionHeader.VirtualAddress);
        CodeSize := SectionHeader.SizeOfRawData;
        SectionCallback (Sender, CodeBase, CodeSize);
      end;
    end;
  end;

  FreeMem(SectionBase);
  Result := SectionCount > 0;
end;

///////////////////////////////
///
///
///////////////////////////////

Type
  LPTTemplateStru = ^TTemplateStru;
  TTemplateStru = record
    Base: Pointer;
    Size: Integer;
    Template: PChar;
    TemplateSrc: PChar;
  end;

function MakeTemplate (Base: Pointer; Size: Integer): THandle; stdcall;
var
  TemplateStru: LPTTemplateStru;
begin
  TemplateStru := AllocMem (SizeOf(TTemplateStru));
  TemplateStru.Base := Base;
  TemplateStru.Size := Size;
  TemplateStru.TemplateSrc := AllocMem (Size + 1024);
  TemplateStru.Template := @TemplateStru.TemplateSrc[512];
  CopyMemory (TemplateStru.Template, Base, Size);
  Result := THandle (TemplateStru);
end;

function FreeTemplate (TmpHandle: THandle): LongBool; Stdcall;
var
  TemplateStru: LPTTemplateStru absolute TmpHandle;
begin
  Result := True;
  Try
    FreeMem (TemplateStru.TemplateSrc);
    FreeMem (TemplateStru);
  Except
    Result := False;
  end;
end;

function CompareTemplate (Sender: Pointer; TmpA, TmpB: THandle; Report: TReportProc): LongBool; Stdcall;
var
  TemplateA: LPTTemplateStru absolute TmpA;
  TemplateB: LPTTemplateStru absolute TmpB;
  I, CompareSize: Integer;
  AimIndex: Integer;
  BreakReport: BOOL;
begin
  Result := False;
  CompareSize := Min (TemplateA.Size, TemplateB.Size);

  AimIndex := -1;
  for I := 0 to CompareSize - 1 do
  begin
    if AimIndex = -1 then
    begin
      if TemplateA.Template[I] = TemplateB.Template[I] then Continue;
      AimIndex := I;
    end else
    begin
      if TemplateA.Template[I] <> TemplateB.Template[I] then Continue;
      Result := True;
      BreakReport := False;
      Report (Sender, AimIndex, I - AimIndex, BreakReport);
      if BreakReport then Exit;
      AimIndex := -1;
    end;
  end;
end;

function FineAddress (TmpHandle: THandle; ToFind: PChar; ToFindSize: Integer; var List: TList): LongBool; Stdcall;
var
  TemplateStru: LPTTemplateStru absolute TmpHandle;
  I: Integer;
  Aim: Pointer;
begin
  I := 0;
  List.Clear;
  Repeat
    Aim := @TemplateStru.Template [I];
    if CompareMem (Aim, ToFind, ToFindSize) then
    begin
      Aim := Pointer (DWORD(TemplateStru.Base) + DWORD(I));
      List.Add(Aim);
      Inc (I, ToFindSize);
    end else
      Inc (I);   
  until I >= (TemplateStru.Size - ToFindSize);

  Result := List.Count > 0;
end;

function SearchAddress (TmpHandle: THandle; Sender: Pointer; ToFind: PChar; ToFindSize: Integer; Report: TReportAddress): LongBool; Stdcall;
var
  TemplateStru: LPTTemplateStru absolute TmpHandle;
  I: Integer;
  Aim: Pointer;
  BreakReport: BOOL;
begin
  Result := false;
  I := 0;
  Repeat
    Aim := @TemplateStru.Template [I];
    if CompareMem (Aim, ToFind, ToFindSize) then
    begin
      Result := True;
      Aim := Pointer (DWORD(TemplateStru.Base) + DWORD(I));
      BreakReport := False;
      Report (Sender, Aim, BreakReport);
      if BreakReport then Break;
      Inc (I, ToFindSize);
    end else
      Inc (I);   
  until I >= (TemplateStru.Size - ToFindSize);
end;


end.
