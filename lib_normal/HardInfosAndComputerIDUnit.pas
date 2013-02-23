unit HardInfosAndComputerIDUnit;

interface
uses windows, sysutils, HardInfosAndComputerID, FindAddress, classes, TCodeEventNotify;

//dllout types
type
  LPTHddOut = ^THddOut;
  THddOut = record
    dn,
    dserial,
    drev: integer;
    n,
    serial,
    rev: pchar;
    mem,
    heads,
    tpc,
    spt,
    bps,
    cyl: cardinal;
  end;


  TSystemOut = record
    lbusslots,
    lmemMslots,
    lmemDslots,
    cpufreqinMHz,
    cpuexternalfreqinMHz: integer;
    cpuname,
    cpusocketname,
    cpumanuf,
    cpuvendor,
    cpuid,
    sysmodel,
    sysmanuf,
    sysserial,
    sysUUID,
    mbmodel,
    mbmanuf,
    mbversion,
    mbserial,
    BbDate,
    BbVendor,
    BbVersion,
    SMBbVersion,
    SMBbRevision: pchar;
    memmodules,
    memdevices,
    sslots: pointer;
  end;

  Tslotout = record
    free: integer;
    name: pchar;
  end;

  TmemMout = record
    size: word;
    socket: pchar;
  end;

  TmemDout = record
    size: integer;
    DeviceLocator, BankLocator: pchar;
  end;

  TID = array[1..8] of cardinal; 
  macres = array[0..5] of byte;
  
function GetHardSys: TSystemout; stdcall;
function GetHddsInfos (var recs: pointer): integer; stdcall;
function GetVCard: pchar; stdcall;
function GetMacAddress : macres; stdcall;
function SmBiosRes : pchar; stdcall;
function BiosMem (var n: integer): pchar; stdcall;
function MakeIDFromStr (lID: integer; lStr: integer; s: pchar): TID; stdcall;
function ComputerID (l, fl: integer): TID; stdcall;
function AllstrID (flags: integer; var n: integer): pchar; stdcall;
function Licence: pchar; stdcall;
                                     
implementation

var
  IsInitial: BOOL;
  Src_gethardsys: function: TSystemout; stdcall;
  Src_gethddsinfos: function (var recs: pointer): integer; stdcall;
  Src_getvcard: function : pchar; stdcall;
  Src_getmacaddress: function : macres; stdcall;
  Src_smbiosres: function : pchar; stdcall;
  Src_biosmem: function (var n: integer): pchar; stdcall;
  Src_MakeIDFromStr: function (lID: integer; lStr: integer; s: pchar): TID; stdcall;
  Src_ComputerID: function (l, fl: integer): TID; stdcall;
  Src_AllstrID: function (flags: integer; var n: integer): pchar; stdcall;
  Src_Licence: function: pchar; stdcall;

PROCEDURE BeforeAttachHandler (ImageBase: Pointer; ImageSize: Integer);
//003DB32C   .  6A 00              push 0                                                 ; /Style = MB_OK|MB_APPLMODAL
//003DB32E   .  68 0CB63D00        push HardInfo.003DB60C                                 ; |Title = "LICENCE OF HardInfosAndComputerID.dll"
//003DB333   .  A1 ECC83D00        mov eax,dword ptr ds:[3DC8EC]                          ; |
//003DB338   .  8B00               mov eax,dword ptr ds:[eax]                             ; |
//003DB33A   .  50                 push eax                                               ; |Text
//003DB33B   .  6A 00              push 0                                                 ; |hOwner = NULL
//003DB33D   .  E8 3A93FFFF        call <jmp.&User32.MessageBoxA>                         ; \MessageBoxA
//003DB342   .  33C0               xor eax,eax
//003DB344   .  5A                 pop edx
//003DB345   .  59                 pop ecx
//003DB346   .  59                 pop ecx
//003DB347   .  64:8910            mov dword ptr fs:[eax],edx
//003DB34A   .  68 57B33D00        push HardInfo.003DB357
//003DB34F   >  C3                 retn

const
  Modify_Mark : array[0..5] of byte = ($8B,$00,$50,$6A,$00,$E8);
  Modify_Offset : Integer = $003DB32C - $003DB338;
  Modify_Value : array[1..$003DB342 - $003DB32C] of byte = ($90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90,$90);
var
  TmpHandle: THandle;
  List: TList;
  ModifyAddress: PChar;
  FindCount: Integer;
begin
  TmpHandle := MakeTemplate (ImageBase, ImageSize);
  List:= TList.Create;

  if FineAddress (TmpHandle, @Modify_Mark, SizeOf(Modify_Mark), List) then
  begin
    FindCount := List.Count;
    if FindCount = 1 then
    begin
      ModifyAddress := Pointer (Integer(List[0]) + Modify_Offset);
      WriteMemory (ModifyAddress, @Modify_Value, SizeOf(Modify_Value));
    end;
  end;

  List.Free();
  FreeTemplate (TmpHandle);
end;

procedure InitDLL;
begin
  if IsInitial then exit;
  INITIALIZATION_DLL (@BeforeAttachHandler);

  Src_gethardsys    := HardInfosAndComputerIDDLL.FindExport('gethardsys');
  Src_gethddsinfos  := HardInfosAndComputerIDDLL.FindExport('gethddsinfos');
  Src_getvcard      := HardInfosAndComputerIDDLL.FindExport('getvcard');
  Src_getmacaddress := HardInfosAndComputerIDDLL.FindExport('getmacaddress');
  Src_smbiosres     := HardInfosAndComputerIDDLL.FindExport('smbiosres');
  Src_biosmem       := HardInfosAndComputerIDDLL.FindExport('biosmem');
  Src_MakeIDFromStr := HardInfosAndComputerIDDLL.FindExport('MakeIDFromStr');
  Src_ComputerID    := HardInfosAndComputerIDDLL.FindExport('ComputerID');
  Src_AllstrID      := HardInfosAndComputerIDDLL.FindExport('AllstrID');
  Src_Licence       := HardInfosAndComputerIDDLL.FindExport('Licence');
  IsInitial := True;
end;

function gethardsys: TSystemout; stdcall;
begin
  InitDLL;
  Result := Src_gethardsys ();
end;
function gethddsinfos (var recs: pointer): integer; stdcall;
begin
  InitDLL;
  Result := Src_gethddsinfos (recs);
end;
function getvcard: pchar; stdcall;
begin
  InitDLL;
  Result := Src_getvcard ();
end;
function getmacaddress : macres; stdcall;
begin
  InitDLL;
  Result := Src_getmacaddress ();
end;
function smbiosres : pchar; stdcall;
begin
  InitDLL;
  Result := Src_smbiosres ();
end;
function biosmem (var n: integer): pchar; stdcall;
begin
  InitDLL;
  Result := Src_biosmem (n);
end;
function MakeIDFromStr (lID: integer; lStr: integer; s: pchar): TID; stdcall;
begin
  InitDLL;
  Result := Src_MakeIDFromStr (lID, lStr, s);
end;
function ComputerID (l, fl: integer): TID; stdcall;
begin
  InitDLL;
  Result := Src_ComputerID (l, fl);
end;
function AllstrID (flags: integer; var n: integer): pchar; stdcall;
begin
  InitDLL;
  Result := Src_AllstrID (flags, n);
end;
function Licence: pchar; stdcall;
begin
  InitDLL; 
  Result := Src_Licence ();
end;

end.
