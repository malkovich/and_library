unit GetRpcListUnit;

interface
uses windows, sysutils, classes;

function GetRpcList: Pointer; Stdcall;
procedure AddRpcFunction (RpcName: String; RpcEntry: Pointer);

implementation

Type
  LPTRpcElmt = ^TRpcElmt;
  TRpcElmt = packed record
    RpcEntry: Pointer;
    RpcName: Array [0..31] of char;
    Next: LPTRpcElmt;
  end;

  LPTRpcElmtArray = ^TRpcElmtArray;
  TRpcElmtArray = Array[WORD] of TRpcElmt;

var
  RpcRecordList: TStringList;
  RpcList: LPTRpcElmtArray;

procedure AddRpcFunction (RpcName: String; RpcEntry: Pointer);
begin
  if RpcRecordList.IndexOf(RpcName) = -1 then
    RpcRecordList.AddObject(RpcName, RpcEntry);
end;


function GetRpcList: Pointer; Stdcall;
var
  Index, RpcCount: Integer;
  RpcName: String;
  RpcFunc: Pointer;
begin
  result := nil;
  if Assigned (RpcList) then
    FreeMem (RpcList);

  RpcCount := RpcRecordList.Count;
  if RpcCount = 0 then exit;

  RpcList := AllocMem (SizeOf(TRpcElmt) * (RpcCount + 1));

  for Index := 0 to RpcCount - 1 do
  begin
    RpcName := RpcRecordList.Strings[Index];
    RpcFunc := RpcRecordList.Objects[Index];
    RpcList[Index].RpcEntry := RpcFunc;
    CopyMemory (@RpcList[Index].RpcName[0], PChar(RpcName), Length(RpcName));
    RpcList[Index].Next := @RpcList[Index + 1];
  end;
  RpcList[RpcCount - 1].Next := nil;

  Result := RpcList;
end;

initialization
  RpcRecordList := TStringList.Create;

finalization
  RpcRecordList.Free;

end.
