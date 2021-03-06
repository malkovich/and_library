unit FuncToImageBase;

interface
uses windows;

function GetDLLHandle (FuncEntry: Pointer): THandle; Stdcall;
function EnsureFuncEntry (FuncEntry: Pointer): Pointer; Stdcall;

implementation

uses madKernel, SysUtils, madDisAsm;

function EnsureFuncEntry (FuncEntry: Pointer): Pointer; Stdcall;
var
  CI, Tar: TCodeInfo;
  FuncInfo : TFunctionInfo;
begin
  Result := nil;
  CI := ParseCode (FuncEntry);
  if Not CI.IsValid then exit;

  if CI.Call or CI.Jmp then
  begin
    Tar := ParseCode (CI.Target);
    if not Tar.IsValid then exit;

    FuncInfo := ParseFunction (Tar.This);
    if FuncInfo.IsValid then
    begin
      Result := CI.Target;
      Exit;
    end;
  end;

  Result := FuncEntry;
end;

type
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

  PImageNTHeaders = ^TImageNTHeaders;
  TImageNTHeaders = packed record
    Signature : LONGWORD;
    FileHeader : TImageFileHeader;
    OptionalHeader : TImageOptionalHeader;
  end;

function _GetDLLHandle (DllsFunc: Pointer):THandle;
var
  dwBase :LongWord;
  ImageDosHeader       : PImageDosHeader;
  ImageNTHeaders       : PImageNTHeaders;
  pSignature           : Pchar;
begin
  result := 0;
  Try
    dwBase := LongWord(DllsFunc);
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
          result := THandle (ImageDosHeader);
          exit;
        end;
      end;
      dec(dwBase);
    until dwBase < $00450000;
  Except
    result := 0;
  End;
end;

function GetDLLHandle (FuncEntry: Pointer): THandle;  Stdcall;
var
  DllHandle: THandle;
  IM: IModule;
  IEE: IExportEntry;
begin
  Result := 0;
  
  DllHandle := _GetDLLHandle (FuncEntry);
  if DllHandle = 0 then exit;

  IM := Module (DllHandle);
  if not IM.IsValid then exit;

  IEE := IM.ExportList.FindItem (FuncEntry);
  if not IEE.IsValid then exit;

  Result := DllHandle;
end;



end.
