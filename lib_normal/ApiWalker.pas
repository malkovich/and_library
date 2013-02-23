unit ApiWalker;

interface
uses Windows, Classes, SysUtils, DLLLoader;
                    
Function ApiWalkerTo (SrcDLL: String; SrcDLLBase: Pointer; WalkFrom, WalkTo, RecvBuff: Pointer; RecvSize: Integer): Integer; Stdcall;

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
    SrcDLLBase: Pointer;
    WalkFrom: Pointer;
    WalkTo: Pointer;
  end;

//修正使用偏移的资源从定位
PROCEDURE RelocWORD (Sender: Pointer; RelocAddr: Pointer; var RelocOffset: Integer);
var
  SenderDLL: TDLLLoader2 absolute Sender;
  RelocInfo: LPTRelocInfo;
  ModifyOffset: Integer;
begin
  RelocInfo := SenderDLL.Data;
  ModifyOffset := Integer (RelocInfo.WalkFrom) - Integer(RelocInfo.WalkTo);
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
  ModifyOffset := Integer (RelocInfo.SrcDLLBase) - Integer(SenderDLL.ImageBase);
  RelocOffset := ModifyOffset;
end;

Function ApiWalkerTo (SrcDLL: String; SrcDLLBase: Pointer; WalkFrom, WalkTo, RecvBuff: Pointer; RecvSize: Integer): Integer; Stdcall;
var
  TmpDLL: TDLLLoader2;
  RelocInfo: TRelocInfo;
  FuncBase: Pointer;
  RealCopySize, TmpMemPtr: DWORD;
  ModifyOffset: Integer;
begin
  RelocInfo.WalkFrom := WalkFrom;
  RelocInfo.WalkTo := WalkTo;
  RelocInfo.SrcDLLBase := SrcDLLBase;

  TmpDLL := TDLLLoader2.Create;
  TmpDLL.OnRelocWORD := RelocWORD;
  TmpDLL.OnRelocPOINTER := RelocPOINTER;
  TmpDLL.Data := @RelocInfo;
  TmpDLL.IsSkipDLLProc := True;
  TmpDLL.LoadDLL(SrcDLL);

  FuncBase := Pointer (Integer(TmpDLL.ImageBase) + (Integer(WalkFrom) - Integer(SrcDLLBase)));

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

  ModifyOffset := Integer (RelocInfo.WalkFrom) - Integer(RelocInfo.WalkTo);
  FixCallOffset (RecvBuff, Result, ModifyOffset);
end;

end.
