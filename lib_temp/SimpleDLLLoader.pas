UNIT SimpleDLLLoader;

INTERFACE

USES PEHead, SimArray, GlobalType, MemoryStream, RelocFuncUnit;

TYPE
  PDLLLoadedObject = ^TDLLLoadedObject;
  TDLLLoadedObject = packed record
    Sign                :Array[0..11] of char;
    ImageBase           :POINTER;
    RelocFnTable        :LPRelocFunctionTable;
    ImageBaseDelta      :INTEGER;
    DLLProc             :TDLLEntryProc;
    DLLResult           :DWORD;
    ExternalLibraryArray:PSimArrayObject;
    ImportArray         :PSimArrayObject;
    ExportArray         :PSimArrayObject;
    Sections            :PSimArrayObject;
  end;

Type
  LPTransParam = ^TTransParam;
  TTransParam = packed record
    lpParameter: Pointer;
    lpResult: DWORD;
  end;

function DLLLoader_Load(hKernel32: THandle; Buffer:Pointer; Len:LongInt; lpReserved: Pointer):PDLLLoadedObject; INLINE;
function DLLLoader_Unload(This:PDLLLoadedObject):BOOLEAN; STDCALL; INLINE;

function FindExportPerIndex(This:PDLLLoadedObject; FunctionIndex:INTEGER):POINTER; STDCALL; INLINE;
function FindExport(This:PDLLLoadedObject; FunctionName:PCHAR):POINTER; STDCALL; INLINE;
function GetExportFuncList(This:PDLLLoadedObject; CallBack:TThreadStartRoutine):DWORD; STDCALL;
function GetExternalLibraryHandle(This:PDLLLoadedObject; LibraryName:PCHAR):LONGWORD; STDCALL;


IMPLEMENTATION


//***************************************************************************//
// inline function
//***************************************************************************//


function LoadExternalLibrary(This :PDLLLoadedObject; LibraryName:PCHAR):LONGWORD; inline; 
var
  I, Index:INTEGER;
  pExternalLibrary :LPExternalLibrary;
begin
 Index := -1;

 for I:=0 TO This.ExternalLibraryArray.Count-1 DO
 begin
  pExternalLibrary := Array_Item(This.ExternalLibraryArray, I);
  if StrSame(LibraryName, @pExternalLibrary.LibraryName[0]) then
  begin
    Index := I;
    break;
  end;
 end;

 if Index<0 then
 begin
  pExternalLibrary := Array_Append( This.ExternalLibraryArray);
  StrLCopy(@pExternalLibrary.LibraryName[0], LibraryName, 100);

  pExternalLibrary.LibraryHandle:=This.RelocFnTable.LoadLibrary(PCHAR(LibraryName));
  Index := This.ExternalLibraryArray.Count - 1;
 end;

 pExternalLibrary := Array_Item(This.ExternalLibraryArray, Index);
 RESULT :=pExternalLibrary.LibraryHandle;
end;

 function ConvertPointer(This:PDLLLoadedObject; RVA:LONGWORD):POINTER; inline;
 var
   I:INTEGER;
   pSection :LPSection;
 begin
  RESULT:=nil;

  for I:=0 TO This.Sections.Count-1 DO
  begin
   pSection := Array_Item(This.Sections, I);
   if (RVA<(pSection.RVA+ pSection.Size)) AND (RVA>=pSection.RVA) then begin
    RESULT:=POINTER(LONGWORD((RVA-LONGWORD(pSection.RVA))+LONGWORD(pSection.Base)));
    EXIT;
   end;
  end;
 end;

function ParseStringToNumber(AString:PCHAR):LONGWORD; inline;
var
  CharCounter:INTEGER;
  Len :Integer;
  forwardChar:Pchar;
begin
   RESULT:=0;

   forwardChar := AString;
   while forwardChar^<>#0 do Inc(forwardChar);
   Len := LongInt(forwardChar) - LongInt(AString);

   for CharCounter:=0 TO Len-1 DO begin
    if AString[CharCounter] IN ['0'..'9'] then begin
     RESULT:=(RESULT*10)+BYTE(BYTE(AString[CharCounter])-BYTE('0'));
    end ELSE begin
     EXIT;
    end;
   end;
end;

//***************************************************************************//
// load dll
//***************************************************************************//



function DLLLoader_Load(hKernel32: THandle; Buffer:Pointer; Len:LongInt; lpReserved: Pointer):PDLLLoadedObject; INLINE;
LABEL QUIT;
var ImageDOSHeader:TImageDOSHeader;
    ImageNTHeaders:TImageNTHeaders;
    OldProtect:LONGWORD;
    smData :PStreamData;
    TEMP_RESULT :BOOLEAN;
 var SectionBase:POINTER;
     OldPosition:INTEGER;
//read section
 var I:INTEGER;
     Section:TImageSectionHeader;
     SectionHeaders:PImageSectionHeaders;
     pSection :LPSection;
//Process Relocations
 var Relocations:PCHAR;
     Position:LONGWORD;
     BaseRelocation:PImageBaseRelocation;
     Base:POINTER;
     NumberOfRelocations:LONGWORD;
     Relocation:PWordArray;
     RelocationCounter:LONGINT;
     RelocationPointer:POINTER;
     RelocationType:LONGWORD;
//ProcessImports
 var ImportDescriptor:PImageImportDescriptor;
     ThunkData:PLONGWORD;
     Name:PCHAR;
     DLLImport:LPDLLImport;
     DLLFunctionImport:LPDLLFunctionImport;
     FunctionPointer:POINTER;
// protect sections
 var Characteristics:LONGWORD;
     Flags:LONGWORD;
// Initial library
 var
     TransParam: TTransParam;
//ProcessExports
 var ExportDirectory:PImageExportDirectory;
     ExportDirectorySize:LONGWORD;
     pDLLFunctExport :LPDLLFunctionExport;
     FunctionNamePointer:POINTER;
     FunctionName:PCHAR;
     FunctionIndexPointer:POINTER;
     FunctionIndex:LONGWORD;
     TempCharArray:Pchar;
     TempCharPointer:PCHAR;
     ForwarderCharPointer:PCHAR;
     ForwarderLibraryHandle:HINST;
var
  Ret :PDLLLoadedObject;
  _RelocFunTable:TRelocFunctionTable;
  
begin
  // 参数输入检查和配置
  RESULT:= nil;
  if NOT ASSIGNED(Buffer) then EXIT;
  if NOT (Len>0) then EXIT;

  // 重要函数重定位
  if not MakeFunctinTableDEP(hKernel32, _RelocFunTable) then exit;

  // 生成DllLoader对象
  Ret := _RelocFunTable.VirtualAlloc(nil, SizeOf(TDLLLoadedObject), MEM_COMMIT,PAGE_READWRITE);
  FillChar(Ret^, SizeOf(TDLLLoadedObject), #0);
  Ret.ImageBase:=nil;
  Ret.DLLProc:=nil;
  Ret.Sign[0] := 'I';
  Ret.Sign[1] := '''';
  Ret.Sign[2] := 'A';
  Ret.Sign[3] := 'M';
  Ret.Sign[4] := ' ';
  Ret.Sign[5] := 'H';
  Ret.Sign[6] := 'A';
  Ret.Sign[7] := 'N';
  Ret.Sign[8] := 'D';
  Ret.Sign[9] := 'L';
  Ret.Sign[10] := 'E';
  Ret.Sign[11] := #0;

  // 保存重定位信息到DLLLoader对象
  Ret.RelocFnTable := _RelocFunTable.VirtualAlloc(nil, SizeOf(TRelocFunctionTable), MEM_COMMIT, PAGE_READWRITE);
  Ret.RelocFnTable^ := _RelocFunTable;

  //初始化保存DLL关键信息的数组

  Ret.ExternalLibraryArray := Array_Create(Ret.RelocFnTable, SizeOf(TExternalLibrary), 50);
  Ret.ImportArray := Array_Create(Ret.RelocFnTable, SizeOf(TDLLImport), 50);
  Ret.ExportArray := Array_Create(Ret.RelocFnTable, SizeOf(TDLLFunctionExport), 1000);
  Ret.Sections := Array_Create(Ret.RelocFnTable, SizeOf(TSection), 20);

  //变量初始化
  smData := MemStream_Create(Ret.RelocFnTable);
  smData.Memory := Buffer;
  smData.Size := Len;
  TempCharArray := Ret.RelocFnTable.VirtualAlloc(nil, 256, MEM_COMMIT,PAGE_READWRITE);

  // 读取PE文件头入局部变量,并校验是否合法

  FILLCHAR(ImageNTHeaders,SIZEOF(TImageNTHeaders),#0);
  MemStream_Seek(smData, 0, soFromBeginning);
  if MemStream_Read(smData, ImageDOSHeader,SIZEOF(TImageDOSHeader))<>SIZEOF(TImageDOSHeader) then GOTO QUIT;
  if ImageDOSHeader.Signature<>$5A4D then GOTO QUIT;
  if MemStream_Seek(smData, ImageDOSHeader.LFAOffset, soFromBeginning)<>LONGINT(ImageDOSHeader.LFAOffset) then GOTO QUIT;
  if MemStream_Read(smData, ImageNTHeaders.Signature,SIZEOF(LONGWORD))<>SIZEOF(LONGWORD) then GOTO QUIT;
  if ImageNTHeaders.Signature<>$00004550 then GOTO QUIT;
  if MemStream_Read(smData, ImageNTHeaders.FileHeader,SIZEOF(TImageFileHeader))<>SIZEOF(TImageFileHeader) then GOTO QUIT;
  if ImageNTHeaders.FileHeader.Machine<>$14C then GOTO QUIT;
  if MemStream_Read(smData, ImageNTHeaders.OptionalHeader,ImageNTHeaders.FileHeader.SizeOfOptionalHeader)<>ImageNTHeaders.FileHeader.SizeOfOptionalHeader then GOTO QUIT;

  // 初始化pe文件映象

  if NOT (ImageNTHeaders.FileHeader.NumberOfSections>0) then GOTO QUIT;
  Ret.ImageBase:=Ret.RelocFnTable.VirtualAlloc(nil,ImageNTHeaders.OptionalHeader.SizeOfImage,MEM_RESERVE,PAGE_NOACCESS);
  Ret.ImageBaseDelta:=LONGWORD(Ret.ImageBase)-ImageNTHeaders.OptionalHeader.ImageBase;
  SectionBase:=Ret.RelocFnTable.VirtualAlloc(Ret.ImageBase,ImageNTHeaders.OptionalHeader.SizeOfHeaders,MEM_COMMIT,PAGE_READWRITE);
  OldPosition:=smData.Position;
  MemStream_Seek(smData, 0, soFromBeginning);
  MemStream_Read(smData, SectionBase^,ImageNTHeaders.OptionalHeader.SizeOfHeaders);
  Ret.RelocFnTable.VirtualProtect(SectionBase,ImageNTHeaders.OptionalHeader.SizeOfHeaders,PAGE_READONLY,OldProtect);
  MemStream_Seek(smData, OldPosition, soFromBeginning);

  //读取session信息

  if NOT (ImageNTHeaders.FileHeader.NumberOfSections>0) then GOTO QUIT;
  SectionHeaders := Ret.RelocFnTable.VirtualAlloc(nil, ImageNTHeaders.FileHeader.NumberOfSections*SIZEOF(TImageSectionHeader), MEM_COMMIT,PAGE_READWRITE);
  if MemStream_Read(smData, SectionHeaders^,(ImageNTHeaders.FileHeader.NumberOfSections*SIZEOF(TImageSectionHeader)))<>(ImageNTHeaders.FileHeader.NumberOfSections*SIZEOF(TImageSectionHeader)) then GOTO QUIT;

  for I:=0 TO ImageNTHeaders.FileHeader.NumberOfSections-1 DO
  begin
    Section:=SectionHeaders^[I];
    pSection := Array_Append(Ret.Sections);
    pSection.RVA := Section.VirtualAddress;
    pSection.Size:=Section.SizeOfRawData;

    if pSection.Size<Section.Misc.VirtualSize then begin
     pSection.Size:=Section.Misc.VirtualSize;
    end;
    pSection.Characteristics:=Section.Characteristics;
    pSection.Base:=Ret.RelocFnTable.VirtualAlloc(POINTER(LONGWORD(pSection.RVA+LONGWORD(Ret.ImageBase))),pSection.Size,MEM_COMMIT,PAGE_READWRITE);
    FILLCHAR(pSection.Base^,pSection.Size,#0);
    if Section.PointerToRawData<>0 then begin
     MemStream_Seek(smData, Section.PointerToRawData, soFromBeginning);
     if MemStream_Read(smData, pSection.Base^,Section.SizeOfRawData)<>LONGINT(Section.SizeOfRawData) then GOTO QUIT;
    end;
  end;
  Ret.RelocFnTable.VirtualFree(SectionHeaders,0,MEM_FREE);

  // 处理DLL重定位信息  ProcessRelocations

  if ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress<>0 then
  begin
   Relocations:=ConvertPointer(Ret, ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress);
   Position:=0;
   WHILE ASSIGNED(Relocations) AND (Position<ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size) DO begin
    BaseRelocation:=PImageBaseRelocation(Relocations);
    Base:=ConvertPointer(Ret, BaseRelocation^.VirtualAddress);
    if NOT ASSIGNED(Base) then GOTO QUIT;
    NumberOfRelocations:=(BaseRelocation^.SizeOfBlock-SIZEOF(TImageBaseRelocation)) DIV SIZEOF(WORD);
    Relocation:=POINTER(LONGWORD(LONGWORD(BaseRelocation)+SIZEOF(TImageBaseRelocation)));
    for RelocationCounter:=0 TO NumberOfRelocations-1 DO begin
     RelocationPointer:=POINTER(LONGWORD(LONGWORD(Base)+(Relocation^[RelocationCounter] AND $FFF)));
     RelocationType:=Relocation^[RelocationCounter] SHR 12;
     CASE RelocationType OF
      IMAGE_REL_BASED_ABSOLUTE:begin
      end;
      IMAGE_REL_BASED_HIGH:begin
       PWORD(RelocationPointer)^:=(LONGWORD(((LONGWORD(PWORD(RelocationPointer)^+LONGWORD(Ret.ImageBase)-ImageNTHeaders.OptionalHeader.ImageBase)))) SHR 16) AND $FFFF;
      end;
      IMAGE_REL_BASED_LOW:begin
       PWORD(RelocationPointer)^:=LONGWORD(((LONGWORD(PWORD(RelocationPointer)^+LONGWORD(Ret.ImageBase)-ImageNTHeaders.OptionalHeader.ImageBase)))) AND $FFFF;
      end;
      IMAGE_REL_BASED_HIGHLOW:begin
       PPOINTER(RelocationPointer)^:=POINTER((LONGWORD(LONGWORD(PPOINTER(RelocationPointer)^)+LONGWORD(Ret.ImageBase)-ImageNTHeaders.OptionalHeader.ImageBase)));
      end;
      IMAGE_REL_BASED_HIGHADJ:begin
       // ???
      end;
      IMAGE_REL_BASED_MIPS_JMPADDR:begin
       // Only for MIPS CPUs ;)
      end;
     end;
    end;
    Relocations:=POINTER(LONGWORD(LONGWORD(Relocations)+BaseRelocation^.SizeOfBlock));
    INC(Position,BaseRelocation^.SizeOfBlock);
   end;
  end;

  // 处理输入表   ProcessImports

  if ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress<>0 then begin
   ImportDescriptor:=ConvertPointer(Ret, ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
   if ASSIGNED(ImportDescriptor) then
   begin
    WHILE ImportDescriptor^.Name<>0 DO
    begin
     Name:=ConvertPointer(Ret, ImportDescriptor^.Name);

     DLLImport := Array_Append(Ret.ImportArray);

     DLLImport^.LibraryName:=Name;
     DLLImport^.LibraryHandle:=LoadExternalLibrary(Ret, Name);
     DLLImport^.Entries:=nil;
     if ImportDescriptor^.TimeDateStamp=0 then
      ThunkData:=ConvertPointer(Ret, ImportDescriptor^.FirstThunk)
     ELSE
      ThunkData:=ConvertPointer(Ret, ImportDescriptor^.OriginalFirstThunk);

     WHILE ThunkData^<>0 DO
     begin
       DLLImport^.Entries := Array_Create(Ret.RelocFnTable, SizeOf(TDLLFunctionImport), 100);
       DLLFunctionImport := Array_Append(DLLImport^.Entries);

       if (ThunkData^ AND IMAGE_ORDINAL_FLAG32)<>0 then begin
         DLLFunctionImport^.NameOrID:=niID;
         DLLFunctionImport^.ID:=ThunkData^ AND IMAGE_ORDINAL_MASK32;
         DLLFunctionImport^.Name:='';
         FunctionPointer:=Ret.RelocFnTable.GetProcAddress(DLLImport^.LibraryHandle,PCHAR(ThunkData^ AND IMAGE_ORDINAL_MASK32));
       end ELSE begin
         Name:=ConvertPointer(Ret, LONGWORD(ThunkData^)+IMPORTED_NAME_OFFSET);
         DLLFunctionImport^.NameOrID:=niName;
         DLLFunctionImport^.ID:=0;
         DLLFunctionImport^.Name:=Name;          
         FunctionPointer:=Ret.RelocFnTable.GetProcAddress(DLLImport^.LibraryHandle,Name);
        end;
        PPOINTER(Thunkdata)^:=FunctionPointer;
        INC(ThunkData);
      end;
      INC(ImportDescriptor);
    end;
   end;
  end;

  // 设置各section的内存保护属性

  if NOT (ImageNTHeaders.FileHeader.NumberOfSections>0) then GOTO QUIT;
  for I:=0 TO ImageNTHeaders.FileHeader.NumberOfSections-1 DO
  begin
    pSection := Array_Item(Ret.Sections, I);
    Characteristics:=pSection.Characteristics;
    Flags:=0;
    if (Characteristics AND IMAGE_SCN_MEM_EXECUTE)<>0 then begin
     if (Characteristics AND IMAGE_SCN_MEM_READ)<>0 then begin
      if (Characteristics AND IMAGE_SCN_MEM_WRITE)<>0 then begin
       Flags:=Flags OR PAGE_EXECUTE_READWRITE;
      end ELSE begin
       Flags:=Flags OR PAGE_EXECUTE_READ;
      end;
     end ELSE if (Characteristics AND IMAGE_SCN_MEM_WRITE)<>0 then begin
      Flags:=Flags OR PAGE_EXECUTE_WRITECOPY;
     end ELSE begin
      Flags:=Flags OR PAGE_EXECUTE;
     end;
    end ELSE if (Characteristics AND IMAGE_SCN_MEM_READ)<>0 then begin
     if (Characteristics AND IMAGE_SCN_MEM_WRITE)<>0 then begin
      Flags:=Flags OR PAGE_READWRITE;
     end ELSE begin
      Flags:=Flags OR PAGE_READONLY;
     end;
    end ELSE if (Characteristics AND IMAGE_SCN_MEM_WRITE)<>0 then begin
     Flags:=Flags OR PAGE_WRITECOPY;
    end ELSE begin
     Flags:=Flags OR PAGE_NOACCESS;
    end;
    if (Characteristics AND IMAGE_SCN_MEM_NOT_CACHED)<>0 then begin
     Flags:=Flags OR PAGE_NOCACHE;
    end;
    Ret.RelocFnTable.VirtualProtect(pSection.Base, pSection.Size,Flags,OldProtect);
   end;

   // 初始化DLL，调用DLL入口

  @Ret.DLLProc:=ConvertPointer(Ret, ImageNTHeaders.OptionalHeader.AddressOfEntryPoint);

  TransParam.lpParameter := lpReserved;
  TransParam.lpResult := 0;
  Ret.DLLResult := Ret.DLLProc(CARDINAL(Ret.ImageBase),DLL_PROCESS_ATTACH, @TransParam);
  if TransParam.lpResult > 0 then
    Ret.DLLResult := TransParam.lpResult;
  if Ret.DLLResult = 0 then GOTO QUIT;

  // 收集处理输出表信息  ProcessExports

  if ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress<>0 then begin
   ExportDirectory:=ConvertPointer(Ret, ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
   if ASSIGNED(ExportDirectory) then begin
    ExportDirectorySize:=ImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].Size;

    for I:=0 TO ExportDirectory^.NumberOfNames-1 DO begin
     // 在输出目录节中，顺序读输出函数信息
     FunctionNamePointer:=ConvertPointer(Ret, LONGWORD(ExportDirectory^.AddressOfNames));
     FunctionNamePointer:=ConvertPointer(Ret, PLongWordArray(FunctionNamePointer)^[I]);
     FunctionName:=FunctionNamePointer;
     FunctionIndexPointer:=ConvertPointer(Ret, LONGWORD(ExportDirectory^.AddressOfNameOrdinals));
     FunctionIndex:=PWordArray(FunctionIndexPointer)^[I];
     FunctionPointer:=ConvertPointer(Ret, LONGWORD(ExportDirectory^.AddressOfFunctions));
     FunctionPointer:=ConvertPointer(Ret, PLongWordArray(FunctionPointer)^[FunctionIndex]);
     // 保存供外部查找和调用
     pDLLFunctExport := Array_Append(Ret.ExportArray);
     pDLLFunctExport.Name := FunctionName;
     pDLLFunctExport.Index := FunctionIndex;

     if (LONGWORD(ExportDirectory)<LONGWORD(FunctionPointer)) AND (LONGWORD(FunctionPointer)<(LONGWORD(ExportDirectory)+ExportDirectorySize)) then
     begin
       ForwarderCharPointer:=FunctionPointer;

       FillChar(TempCharArray[0], 256, #0);
       TempCharPointer := TempCharArray;
       WHILE ForwarderCharPointer^<>'.' DO
       begin
         TempCharPointer^ := ForwarderCharPointer^;
         INC(TempCharPointer);
         INC(ForwarderCharPointer);
       end;
       ForwarderLibraryHandle:=LoadExternalLibrary(Ret, TempCharPointer);

       Inc(ForwarderCharPointer);
       if ForwarderCharPointer^='#' then begin
         INC(ForwarderCharPointer);
         ForwarderCharPointer:=ConvertPointer(Ret, ParseStringToNumber(ForwarderCharPointer));
         FunctionPointer :=ForwarderCharPointer;
       end ELSE begin
         FunctionPointer :=Ret.RelocFnTable.GetProcAddress(ForwarderLibraryHandle,ForwarderCharPointer);
       end;
       pDLLFunctExport.FunctionPointer := FunctionPointer;
     end ELSE
     begin
      pDLLFunctExport.FunctionPointer:=FunctionPointer;
     end;
    end
   end;
  end;

  RESULT:= Ret;

QUIT:
  Ret.RelocFnTable.VirtualFree(TempCharArray,0,MEM_FREE);
  MemStream_Destroy(smData);
end;

function DLLLoader_Unload(This:PDLLLoadedObject):BOOLEAN;
var
  I:INTEGER;
  pSection :LPSection;
  pExternalLibrary :LPExternalLibrary;
  pDLLImport :LPDLLImport;
  pDLLFunctionExport :LPDLLFunctionExport;
  VirtualFree : function(lpAddress : Pointer; dwSize, dwFreeType : DWORD) : BOOL; stdcall;
begin
 VirtualFree := This.RelocFnTable.VirtualFree;

 if @This.DLLProc<>nil then
 begin
  This.DLLProc(LONGWORD(This.ImageBase),DLL_PROCESS_DETACH,nil);
  @This.DLLProc := nil;
 end;

 for I:=0 TO This.Sections.Count-1 DO
 begin
   pSection := Array_Item(This.Sections, I);
   if ASSIGNED(pSection.Base) then
     VirtualFree(pSection.Base,0,MEM_RELEASE);
 end;
 Array_Destroy(This.Sections);

 for I:=0 TO This.ExternalLibraryArray.Count-1 DO
 begin
   pExternalLibrary := Array_Item(This.ExternalLibraryArray, I);
   This.RelocFnTable.FreeLibrary(pExternalLibrary.LibraryHandle);
 end;
 Array_Destroy(This.ExternalLibraryArray);

 for I:=0 TO This.ImportArray.Count-1 DO
 begin
   pDLLImport := Array_Item(This.ImportArray, I);
   Array_Destroy(pDLLImport.Entries);
 end;
 Array_Destroy(This.ImportArray);

 for I:=0 TO This.ExportArray.Count-1 DO
 begin
   pDLLFunctionExport := Array_Item(This.ExportArray, I);
   pDLLFunctionExport.Name := nil;
 end;
 Array_Destroy(This.ExportArray);

 Result := VirtualFree(This.ImageBase,0,MEM_RELEASE);

 Result := Result and VirtualFree(This.RelocFnTable,0,MEM_RELEASE);

 Result := Result and VirtualFree(This,0,MEM_RELEASE);
end;

function FindExport(This:PDLLLoadedObject; FunctionName:PCHAR):POINTER;
var
  I:INTEGER;
  pDLLFunctionExport :LPDLLFunctionExport;
begin
 RESULT:=nil;
 for I:=0 TO This.ExportArray.Count-1 DO
 begin
   pDLLFunctionExport := Array_Item(This.ExportArray, I);
   if StrSame(pDLLFunctionExport.Name, FunctionName) then
   begin
     RESULT:= pDLLFunctionExport.FunctionPointer;
     EXIT;
   end;
 end;
end;

function GetExportFuncList(This:PDLLLoadedObject; CallBack:TThreadStartRoutine):DWORD; STDCALL;
var
  I:INTEGER;
  pDLLFunctionExport :LPDLLFunctionExport;
begin
  RESULT:=0;
  for I:=0 TO This.ExportArray.Count-1 DO
  begin
    pDLLFunctionExport := Array_Item(This.ExportArray, I);
    CallBack(pDLLFunctionExport);
  end;
end;


function FindExportPerIndex(This:PDLLLoadedObject; FunctionIndex:INTEGER):POINTER;
var
  I:INTEGER;
  pDLLFunctionExport :LPDLLFunctionExport;
begin
 RESULT:=nil;

 for I:=0 TO This.ExportArray.Count-1 DO
 begin
   pDLLFunctionExport := Array_Item(This.ExportArray, I);
   if pDLLFunctionExport.Index = FunctionIndex then
   begin
     RESULT:= pDLLFunctionExport.FunctionPointer;
     EXIT;
   end;
 end;
end;


function GetExternalLibraryHandle(This:PDLLLoadedObject; LibraryName:PCHAR):LONGWORD;
var
  I:INTEGER;
  pExternalLibrary :LPExternalLibrary;
begin
 RESULT:=0;
 for I:=0 TO This.ExternalLibraryArray.Count-1 DO
 begin
   pExternalLibrary := Array_Item(This.ExternalLibraryArray, I);
   if StrSame(pExternalLibrary.LibraryName, LibraryName) then
   begin
     RESULT:=pExternalLibrary.LibraryHandle;
     EXIT;
   end;
 end;
end;


end.
