unit TransBufferByUDP;

interface
uses windows, sysutils;

const
  DEFAULT_BLOCK_LENGTH    = 1024;
  DEFAULT_BUFFER_TIMEOUT  = 18*1000;
  DEFAULT_HASH_TABLE_SIZE = 3000;
  DEFAULT_BUFFER_CLEAR_INTERNAL = 3000;

function BufferTransPort (CmdLine: String): String;

function MakeABuffer (Const Buffer; Size: Integer): String;
               
implementation

//生成端
//>>>BufferTrans MakeBuffer rterbtertgtfyun45srntnwefgdfbgdsfgbdf...
//<<<{ACA2261D-C32F-4938-A5E6-B6ED31535F7B}

//分发Guid
//>>>GetQuestion
//<<<GetQuestion Suc {ACA2261D-C32F-4938-A5E6-B6ED31535F7B}

//取出端
//>>>BufferTrans GetBufferHead {ACA2261D-C32F-4938-A5E6-B6ED31535F7B}
//<<<{ACA2261D-C32F-4938-A5E6-B6ED31535F7B} 200000 1000 2CAF132BBC634ACEBE965BA40BD201B1 /Err
//>>>BufferTrans GetBufferBlock {ACA2261D-C32F-4938-A5E6-B6ED31535F7B} 1
//<<<rterbtertgtfyun45srntnwefgdfbgdsfgbdf... /Err
//>>>BufferTrans CloseBuffer {ACA2261D-C32F-4938-A5E6-B6ED31535F7B}
//<<<Suc/Err

uses
  classes, HashTblUnit, TDHshChn, TDHshBse, TDStkQue, Md5, SyncObjs, Base64Unit,
  ThreadStringList;

function SeperateCmd (Cmd: String; out CmdVerb, CmdTail: String): LongBool;
var
    I, J        : Integer;
    FCmdLen: Integer;
begin
    FCmdLen := Length (Cmd);

    { Skip leading spaces }
    I := 1;
    while (I < FCmdLen) and (Cmd[I] in [' ']) do
        Inc(I);

    { Find separator and separe CmdVerb and CmdTail }
    J := I;
    while TRUE do begin
        if (J >= FCmdLen) then begin
            SetLength(CmdVerb, FCmdLen - I + 1);
            Move(Cmd[I], CmdVerb[1], Length(CmdVerb));
            CmdTail := '';
            break;
        end;

        if Cmd[J] in [' '] then begin
            SetLength(CmdVerb, J - I);
            Move(Cmd[I], CmdVerb[1], Length(CmdVerb));
            SetLength(CmdTail, FCmdLen - J );
            Move(Cmd[J+1], CmdTail[1], Length(CmdTail));
            break;
        end;
        Inc(J);
    end;

    CmdVerb := UpperCase (Trim (CmdVerb));
    CmdTail := Trim (CmdTail);

    Result := CmdVerb <> '';
end;


var
  CmdList: TStringList;
  HashTbl: TtdHashTableChained;
  TimeQue: TtdArrayQueue;
  DaemonThreadID: DWORD;

function MakeGuid: String;
var
  Guid: TGUID;
begin
  CreateGUID (Guid);
  Result := UpperCase(GuidToString (Guid));
end;  

function MakeBuffer (Base64Data: String): String;
var
  Guid, LineBlock, Md5Str: String;
  aItem: Pointer;
  DataSL: TThreadStringList;
  HandleIndex, TotalSize, ModSize: DWORD;
begin
  repeat
    Guid := MakeGuid;
    if not HashTbl.Find(Guid, aItem) then break;
    sleep(100);
  until False;

  DataSL := TThreadStringList.Create;

  TotalSize := Length(Base64Data);
  Md5Str := UpperCase (StringMD5Str (Base64Data));
  DataSl.AddObject(format('%s %d %d %s', [Guid, TotalSize, DEFAULT_BLOCK_LENGTH, Md5Str]), Pointer(GetTickCount));

  HandleIndex := 0;
  SetLength (LineBlock, DEFAULT_BLOCK_LENGTH);
  while TotalSize  - HandleIndex >= DEFAULT_BLOCK_LENGTH do
  begin
    Move (Base64Data[HandleIndex+1], LineBlock[1], DEFAULT_BLOCK_LENGTH);
    DataSl.Add(LineBlock);
    Inc (HandleIndex, DEFAULT_BLOCK_LENGTH);
  end;

  ModSize := TotalSize mod DEFAULT_BLOCK_LENGTH;    
  if ModSize > 0 then
  begin
    SetLength (LineBlock, ModSize);
    Move (Base64Data[HandleIndex+1], LineBlock[1], ModSize);
    DataSL.Add(LineBlock);
  end;

  SetLength (LineBlock, 0);

  HashTbl.Insert(Guid, DataSL);
  TimeQue.Enqueue(DataSL);
  Result := Guid;
end;

function GetBufferHead (Guid: String): String;
var
  DataSL: TThreadStringList;
  aItem: Pointer;
begin
  Result := 'ERR';
  if not HashTbl.Find(Guid, aItem) then exit;
  DataSL := aItem;
  Result := DataSL.Strings[0];
end;

function GetBufferBlock (Parma: String): String;
var
  Guid, BlockIndex: String;
  Index: Integer;
  DataSL: TThreadStringList;
  aItem: Pointer;
begin
  Result := 'ERR';
  if not SeperateCmd (Parma, Guid, BlockIndex) then exit;
  if not TryStrToInt (BlockIndex, Index) then exit;
  if not HashTbl.Find(Guid, aItem) then exit;
  DataSL := aItem;
  if (Index < 1) or (Index > DataSL.Count) then exit;
  Result := DataSL.Strings [Index];
end;

function CloseBuffer (Guid: String): String;
var
  DataSL: TThreadStringList;
  aItem: Pointer;
begin
  Result := 'ERR';
  if not HashTbl.Find(Guid, aItem) then exit;
  HashTbl.Delete(Guid);
  DataSL := aItem;
  DataSL.Clear;
  Result := 'SUC';
end;

function DaemonThread (parma: Pointer): Integer; stdcall;
var
  TimeOutEvent: TSimpleEvent;
  DataSL: TThreadStringList;
  SL: TStringList;
  MakeTime: DWORD;
  Guid, HeadStr: String;
begin
  TimeOutEvent:= TSimpleEvent.Create(nil, false, false, '');
  SL:= TStringList.Create;
  while wrTimeout = TimeOutEvent.WaitFor(DEFAULT_BUFFER_CLEAR_INTERNAL) do
  begin
    while True do
    begin
      if TimeQue.IsEmpty then Break;
      DataSL := TimeQue.Examine;   

      if DataSL.Count = 0 then
      begin
        DataSL.Free;
        TimeQue.Dequeue;
        Continue;
      end;

      MakeTime := DWORD(DataSL.Objects[0]);
      if GetTickCount - MakeTime > DEFAULT_BUFFER_TIMEOUT then
      begin
        SL.Clear;
        HeadStr := DataSL.Strings[0];
        if HeadStr <> '' then
        begin
          ExtractStrings ([' '], [' '], PChar(HeadStr), SL);
          Guid := SL[0];
          HashTbl.Delete(Guid);
        end;
        DataSL.Free;
        TimeQue.Dequeue;
        Continue;
      end;

      Break;
    end;
  end;
  SL.Free;
  Result := 0;
end;

procedure InitialThePort; inline;
begin
  if Assigned (CmdList) then Exit;
  CmdList := TStringList.Create;
  CmdList.AddObject('MakeBuffer', @MakeBuffer);
  CmdList.AddObject('GetBufferHead', @GetBufferHead);
  CmdList.AddObject('GetBufferBlock', @GetBufferBlock);
  CmdList.AddObject('CloseBuffer', @CloseBuffer);

  HashTbl := TtdHashTableChained.Create(DEFAULT_HASH_TABLE_SIZE, TDPJWHash, nil);
  TimeQue := TtdArrayQueue.Create (nil, 1000);
  CreateThread (nil, 0, @DaemonThread, nil, 0, DaemonThreadID);
end;


function BufferTransPort (CmdLine: String): String;
var
  Cmd, Parma: String;
  CmdIndex: Integer;
  CmdRuner: function (Parma: String): String;
begin
  InitialThePort;
  Result := 'ERR';
  if SeperateCmd (CmdLine, Cmd, Parma) then
  begin
    CmdIndex := CmdList.IndexOf(Cmd);
    if CmdIndex = -1 then Exit;
    CmdRuner := Pointer (CmdList.Objects [CmdIndex]);
    Result := CmdRuner (Parma);
  end;
end;


function MakeABuffer (Const Buffer; Size: Integer): String;
begin
  Result := BufferTransPort ('MakeBuffer ' + EncodeBase64 (Buffer, Size));
end;


end.
