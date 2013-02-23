unit ApiDrager;

interface
uses Windows, Classes, SysUtils, DLLLoader;

Function DragApiTo (SrcPeFile: PChar; HostImage, DragFrom, DragTo, RecvBuff: Pointer; RecvSize: Integer): Integer; Stdcall; overload;
Function DragApiTo (SrcPeFile: String; HostImage, DragFrom: Pointer; var DragTo: Pointer): Integer; Stdcall; overload;
Function DragApiTo (MapedDLL, DllFunc: String; var DragTo: Pointer): Integer; Stdcall; overload;
Function DragApiTo (ApiEntry: Pointer): Pointer; Stdcall; Overload;

function GetDragApiSize (SrcPeFile: PChar; HostImage, DragFrom: Pointer): Integer; Stdcall;
function CheckRealEntry (ApiEntry: Pointer): Pointer;

implementation

uses madDisasm;

//修正PE文件内部的调用和跳转
Procedure FixCallOffset (CodeBase: Pointer; CodeSize, ModifyOffset: Integer);
var
  CodeInfo: TCodeInfo;
  IterCodeBase: Pointer;
  OffsetPtr: PInteger;
begin
  IterCodeBase := CodeBase;
  Repeat
    CodeInfo := ParseCode (IterCodeBase);
    if CodeInfo.Call or CodeInfo.Jmp then
    begin
      if CodeInfo.RelTarget then
        if CodeInfo.TargetSize = 4 then
        begin
          OffsetPtr := Pointer (Integer(CodeInfo.Next) - CodeInfo.TargetSize);
          OffsetPtr^ := Integer (OffsetPtr^) + ModifyOffset;
        end;
    end;
    IterCodeBase := CodeInfo.Next;
  until Integer(IterCodeBase) >= (Integer(CodeBase) + CodeSize);
end;

Type
  LPTRelocInfo = ^TRelocInfo;
  TRelocInfo = record
    HostImage: Pointer;
    DragFrom: Pointer;
    DragTo: Pointer;
  end;

//修正使用偏移的资源从定位
PROCEDURE RelocWORD (Sender: Pointer; RelocAddr: Pointer; var RelocOffset: Integer);
var
  SenderDLL: TDLLLoader2 absolute Sender;
  RelocInfo: LPTRelocInfo;
  ModifyOffset: Integer;
begin
  RelocInfo := SenderDLL.Data;
  ModifyOffset := Integer (RelocInfo.DragFrom) - Integer(RelocInfo.DragTo);
  RelocOffset := ModifyOffset;
end;

//修正使用指针定位的资源的从定位
PROCEDURE RelocPOINTER (Sender: Pointer; RelocAddr: Pointer; var RelocOffset: Integer);
var
  SenderDLL: TDLLLoader2 absolute Sender;
  RelocInfo: LPTRelocInfo;
  ModifyOffset: Integer;
begin
  RelocInfo := SenderDLL.Data;
  ModifyOffset := Integer (RelocInfo.HostImage) - Integer(SenderDLL.ImageBase);
  RelocOffset := ModifyOffset;
end;

Function DragApiTo (SrcPeFile: PChar; HostImage, DragFrom, DragTo, RecvBuff: Pointer; RecvSize: Integer): Integer; Stdcall;
var
  TmpDLL: TDLLLoader2;
  RelocInfo: TRelocInfo;
  FuncBase: Pointer;
  RealCopySize, TmpMemPtr: DWORD;
  ModifyOffset: Integer;
begin
  RelocInfo.DragFrom := DragFrom;
  RelocInfo.DragTo := DragTo;
  RelocInfo.HostImage := HostImage;

  TmpDLL := TDLLLoader2.Create;
  TmpDLL.OnRelocWORD := RelocWORD;
  TmpDLL.OnRelocPOINTER := RelocPOINTER;
  TmpDLL.Data := @RelocInfo;
  TmpDLL.IsSkipDLLProc := True;
  TmpDLL.LoadDLL(SrcPeFile);

  FuncBase := Pointer (Integer(TmpDLL.ImageBase) + (Integer(DragFrom) - Integer(HostImage)));

  RealCopySize := RecvSize;
  While IsBadReadPtr (FuncBase, RealCopySize) do
  begin
    TmpMemPtr := DWORD(FuncBase) + RealCopySize;
    if (TmpMemPtr mod $1000) > 0 then
      TmpMemPtr := TmpMemPtr - (TmpMemPtr mod $1000)
    else
      TmpMemPtr := TmpMemPtr - $1000;
    RealCopySize := DWORD (TmpMemPtr) -  DWORD(FuncBase);
  end;

  CopyMemory (RecvBuff, FuncBase, RealCopySize);
  Result := RealCopySize;
  TmpDLL.Free;

  ModifyOffset := Integer (RelocInfo.DragFrom) - Integer(RelocInfo.DragTo);
  FixCallOffset (RecvBuff, Result, ModifyOffset);
end;

function GetDragApiSize (SrcPeFile: PChar; HostImage, DragFrom: Pointer): Integer; Stdcall;
var
  RecvBuff: Pointer;
  FuncInfo: TFunctionInfo;
begin
  RecvBuff := AllocMem ($2000);
  Try
    DragApiTo (SrcPeFile, HostImage, DragFrom, RecvBuff, RecvBuff, $2000);
    FuncInfo := ParseFunction (RecvBuff);
    Result := FuncInfo.CodeLen;
  except
    Result := -1;
  end;
  FreeMem (RecvBuff);        
end;

Function DragApiTo (SrcPeFile: String; HostImage, DragFrom: Pointer; var DragTo: Pointer): Integer; Stdcall;
var
  RecvBuff: Pointer;
  RecvSize: Integer;
begin
  Result := GetDragApiSize (PChar(SrcPeFile), HostImage, DragFrom);
  if Result = -1 then exit;

  RecvBuff := AllocMem (Result + 1);
  RecvSize := DragApiTo (PChar(SrcPeFile), HostImage, DragFrom, RecvBuff, RecvBuff, Result);

  if RecvSize <> Result then
  begin
    FreeMem (RecvBuff);
    Result := -1;
    Exit;
  end;

  DragTo := RecvBuff;
end;

Function DragApiTo (MapedDLL, DllFunc: String; var DragTo: Pointer): Integer; Stdcall;
var
  DllHandle: THandle;
  SrcPeFile: String;
  HostImage, DragFrom: Pointer;
begin
  Result := -1;
  DllHandle := GetModuleHandle (PChar(MapedDLL));
  if DllHandle = 0 then Exit;

  SrcPeFile := GetModuleName (DllHandle);
  DragFrom := GetProcAddress (DllHandle, PChar(DllFunc));
  if Not assigned (DragFrom) then exit;
  
  HostImage := Pointer (DllHandle);
  Result := DragApiTo (SrcPeFile, HostImage, DragFrom, DragTo);
end;

function GetAimProcModuleName (AimProc: Pointer; var AimHandle: THandle): string;
var
  SignWord: PWORD;
  Buffer:array[byte] of char;
begin
  SignWord := AimProc;
  SignWord := Pointer (DWORD (SignWord) And $FFFFF000);

  while SignWord^ <> $5A4D do
    SignWord := Pointer (DWORD(SignWord) - $1000);

  AimHandle := THandle (SignWord);

  ZeroMemory(@buffer[0], 256);
  GetModuleFileName(AimHandle, @Buffer, 256);
  result := buffer;
end;

function CheckRealEntry (ApiEntry: Pointer): Pointer;
var
  CodeInfo: TCodeInfo;
begin
  Repeat
    CodeInfo := ParseCode (ApiEntry);
    if not CodeInfo.Jmp then break;
    ApiEntry := CodeInfo.Target;
  until False;
  Result := ApiEntry;
end;

Function DragApiTo (ApiEntry: Pointer): Pointer; Stdcall;
var
  DllHandle: THandle;
  SrcPeFile: String;
  HostImage, DragFrom, DragTo: Pointer;
begin
  Result := nil;
  ApiEntry := CheckRealEntry (ApiEntry);
  SrcPeFile := GetAimProcModuleName (ApiEntry, DllHandle);
  if DllHandle = 0 then Exit;
  if SrcPeFile = '' then Exit;

  HostImage := Pointer(DllHandle);
  DragFrom := ApiEntry;  
  if DragApiTo (SrcPeFile, HostImage, DragFrom, DragTo) = -1 then Exit;

  Result := DragTo;
end;

end.



