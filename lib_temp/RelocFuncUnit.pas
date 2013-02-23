unit RelocFuncUnit;

interface
uses GlobalType;

function GetKernel32Base_PEB:Pointer; inline;
function GetKernel32Base_SEH:Pointer; inline;
FUNCTION GetPrimeFunction(Kernel32Module:Cardinal; var GetProcAddress, LoadlibraryA:Pointer) : LongBool; stdcall; inline;

function MakeFunctinTable(var RelocFunTable:TRelocFunctionTable):LongBool; inline;
function MakeFunctinTableDEP (hKernel32: THandle; var RelocFunTable:TRelocFunctionTable):LongBool; inline;
procedure end_sign;

implementation

uses PEHead;


////////////////////////////////////////
///                                  ///
///   relocation inline function     ///
///                                  ///
////////////////////////////////////////

function GetPEB_asm:Pointer;
asm
  mov eax,fs:[$30]      // 648B0530000000C3
end;


function GetPEB:Pointer; inline;
var
  Buffer:array[0..7] of byte;
  pFnx : function():Pointer;
begin
  PInt64 (@Buffer[0])^ := Int64($C300000030058B64);
  pFnx := @Buffer[0];
  Result := pFnx;
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
  BaseAddress := POINTER ( DWORD(ListEntry) + SizeOf(TListEntry));
  result := BaseAddress^;
end;


///////////////////////////////////////////////////////////////////////////////


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
  dwBase :dword;
  ImageDosHeader       : PImageDosHeader;
  ImageNTHeaders       : PImageNTHeaders;
  pSignature           : Pchar;
begin
  lpUnhandle := GetKernel32FunctionAddress;
  dwBase := DWORD(lpUnhandle);
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


///////////////////////////////////////////////////////////////////////////////

FUNCTION GetPrimeFunction(Kernel32Module:Cardinal; var GetProcAddress, LoadlibraryA:Pointer) : LongBool; stdcall; inline;
VAR
  ExportName           : pChar;
  Address              : Cardinal;
  J                    : Cardinal;
  ImageDosHeader       : PImageDosHeader;
  ImageNTHeaders       : PImageNTHeaders;
  ImageExportDirectory : PImageExportDirectory;
  LoadlibraryAStr      : array[0..12] of char;
  fnGetProcAddress     : function (hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
BEGIN
  result := false;
  ImageDosHeader:=Pointer(Kernel32Module);
  ImageNTHeaders:=Pointer(Kernel32Module+ImageDosHeader.LFAOffset);
  ImageExportDirectory:=Pointer(ImageNtHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress+Kernel32Module);
  J:=0;
  Address:=0;
  REPEAT
    ExportName:=Pointer(Cardinal(Pointer(Cardinal(ImageExportDirectory.AddressOfNames)+Kernel32Module+J*4)^)+Kernel32Module);

    //ÅÐ¶ÏÊÇ²»ÊÇGetProcAddress×Ö·û´®
    if PDWORD(@ExportName[0])^ = $50746547 then
    if PDWORD(@ExportName[4])^ = $41636F72 then
    if PDWORD(@ExportName[8])^ = $65726464 then
    if PWORD(@ExportName[12])^ = $7373 then
    if ExportName[14] = #0 then
    BEGIN
      Address:=Cardinal(Pointer(Word(Pointer(J SHL 1+Cardinal(
               ImageExportDirectory.AddressOfNameOrdinals)+Kernel32Module)^) AND
               $0000FFFF SHL 2+Cardinal(ImageExportDirectory.AddressOfFunctions)
               +Kernel32Module)^)+Kernel32Module;
    END;
    Inc(J);
  UNTIL (Address<>0)OR(J=ImageExportDirectory.NumberOfNames);

  if Address = 0 then exit;      

  PDWORD(@LoadlibraryAStr[0])^ := $64616F4C;
  PDWORD(@LoadlibraryAStr[4])^ := $7262694C;
  PDWORD(@LoadlibraryAStr[8])^ := $41797261;
  LoadlibraryAStr[12] := #0;

  GetProcAddress := Pointer(Address);
  fnGetProcAddress := GetProcAddress;
  LoadlibraryA := fnGetProcAddress(Kernel32Module, LoadlibraryAStr);

  Result:= Assigned(LoadlibraryA);
END;

///////////////////////////////////////////////
///
///
///////////////////////////////////////////////

function MakeFunctinTable(var RelocFunTable:TRelocFunctionTable):LongBool; inline;
var
  hKernel32 :THandle;
begin
  hKernel32 := THandle(GetKernel32Base_PEB());    //  GetKernel32Base_SEH
  Result := MakeFunctinTableDEP (hKernel32, RelocFunTable);
end;

function MakeFunctinTableDEP(hKernel32: THandle; var RelocFunTable:TRelocFunctionTable):LongBool; inline;
var
  Loadlibrary   :function (lpLibFileName: PChar): HMODULE; stdcall;
  GetProcAddress:function (hModule: HMODULE; lpProcName: LPCSTR): FARPROC; stdcall;
  Buffer :array[0..15] of char;
  I :Integer;
begin
  Result := False;
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

procedure end_sign;
begin
end;


end.
