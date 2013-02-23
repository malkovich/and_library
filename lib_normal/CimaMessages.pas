{
  CimaMessages 0.2

  Interprocess Messages using FileMappings.
  By David Cimadevilla (http://david.cimadevilla.com)

  This code allows you to send messages between different
  processes. It is based on Win32 API FileMappings, Mutexes
  and Events. The message queue buffer is "circular", in
  case there is not enough space and after a timeout period
  the older messages are overwritten. This grants always
  the access to the message queue even if no one read the
  messages.

  It seems to be quite quick and it is easy to use:

  ## To create the queue from the monitor application:

  CimaMessageQueue :=
    TCimaMessageQueue.Create('QueueName',Handle,MsgId);

  Handle refers to the window who receives a notification
  when a new message is send to the queue. MsgId is the
  message identifier for that Windows message.


  ## To send a message

  SendCimaMessage('QueueName',Msg,MsgSize);

  It returns True if done.


  ## To get a message

  CimaMessageQueue.GetMessage (Buffer, BufferSize);

  It returns the message size or 0 if no messages are available.


  There is also optional parameters to control the buffersize
  and timeouts. You can get the source code and example project
  from http:\\david.cimadevilla.com\CimaMessages.zip
}

unit CimaMessages;

interface

uses Classes;

type
  TQueueHeader = record
    BufSize: Integer;
    BeginOffset, DataLen: Integer;
  end;
  PQueueHeader = ^TQueueHeader;

const
  KDefTimeOutSend = 200;
  kDefTimeOutSendBufferFull = 1000;
  kTimeOutSendBufferFullMinTime = 10;
  kDefTimeOutGet = 1000;
  kNotifyLoopTime = 1000;
  kDefBufferSize = $8000; //$1000 - sizeof(TQueueHeader);

type
  TMsgEventThread = class(TThread)
  protected
    FEvent: THandle;
    FHWND: THandle;
    FMsg: Cardinal;
  public
    constructor Create( stEventName: string; hHWND: THandle= 0; uMsg: cardinal = 0 );
    destructor Destroy; override;
    procedure Execute;override;
  end;

  TCimaMessageQueue = class
  protected
    FTimeOutGet: integer;
    FThread: TMsgEventThread;
    FMapping: THandle;
    FMutex: THandle;
    FQueueBuffer: PQueueHeader;
  public
    property TimeOutGet: integer read FTimeOutGet write FTimeOutGet;
    constructor Create ( stQueueName: string; HWND: THandle= 0
      ; uMsg: cardinal = 0; iBufferSize: Integer = kDefBufferSize);
    {
    HWND -> Window handle where notify messages are sent.
    uMsg -> Send notify message ID.
    iBufferSize -> Queue buffer size.
    }
    destructor Destroy; override;
    function GetMessage(var Buffer; iBufferSize: Integer): Integer;
  end;

  function SendCimaMessage ( stQueueName: string; var MsgBuf
    ; MsgSize: Integer; iTimeOut: integer = kDefTimeoutSend
    ; iTimeOutBufferFull: integer = kDefTimeOutSendBufferFull): boolean;
  {
  iTimeOut -> milliseconds before fail if the queue is
    locked by another process.

  iTimeOutBufferFull -> milliseconds before overwrite
    the older message if the queue buffer is full.
  }

var
  CimaMessageQueue: TCimaMessageQueue;


implementation

uses Windows, SysUtils;


procedure ReadFromBuffer( Buffer: PQueueHeader; iOffset: Integer; var Data; iDataSize: Integer);
var
  pSrc, pDst: pointer;
  iBytes: Integer;
begin
  with Buffer^ do
  begin
    //Before the circular border
    if iOffset + iDataSize > BufSize then
      iBytes := BufSize - iOffset
    else
      iBytes := iDataSize;

    //From and To addresses
    pSrc := pointer(integer(Buffer) + sizeof(TQueueHeader) + iOffset);
    pDst := @Data;

    //Read
    move(pSrc^,pDst^,iBytes);

    //Crossing the circular border
    if iBytes < iDataSize then
    begin
      //Continue from the beginning
      pSrc := pointer(integer(Buffer) + sizeof(TQueueHeader));
      inc (pbyte(pDst), iBytes);
      iBytes := iDataSize - iBytes;

      //Read
      move(pSrc^,pDst^,iBytes);
    end;
  end;
end;


procedure WriteToBuffer( Buffer: PQueueHeader; var Data; iDataSize: Integer);
var
  iOffset: integer;
  pSrc, pDst: pointer;
  iBytes: Integer;
begin
  with Buffer^ do
  begin
    //Offset
    iOffset := BeginOffset + DataLen;
    iOffset := iOffset mod BufSize;

    //Before the circular border
    if iOffset + iDataSize > BufSize then
      iBytes := BufSize - iOffset
    else
      iBytes := iDataSize;

    //Data addresses
    pSrc := @Data;
    pDst := pointer(integer(Buffer) + sizeof(TQueueHeader) + iOffset);

    //Write
    move(pSrc^,pDst^,iBytes);

    //Crossing the circular border
    if iBytes < iDataSize then
    begin
      //Continue from the beginning
      inc (pbyte(pSrc), iBytes);
      pDst := pointer(integer(Buffer) + sizeof(TQueueHeader));
      iBytes := iDataSize - iBytes;

      //Write
      move(pSrc^,pDst^,iBytes);
    end;

    //New data len
    inc(DataLen,iDataSize);
  end;
end;


function SendCimaMessage ( stQueueName: string; var MsgBuf; MsgSize: Integer
  ; iTimeOut: integer; iTimeOutBufferFull: integer): boolean;
var
  QueueBuffer: PQueueHeader;
  hMapping: THandle;
  iMsgBufSize, iPending: Integer;
  hMutex: THandle;
  uErrorBackup: Cardinal;
  hEvent: THandle;

  procedure FreeSpace ( iFreeWanted: integer );
  var
    pBufAddr: integer;
    iMsgSize: integer;
  begin
    with QueueBuffer^ do
    begin
      pBufAddr := integer(QueueBuffer) + SizeOf(TQueueHeader);
      while BufSize - DataLen < iFreeWanted do
      begin
        iMsgSize := PInteger(pBufAddr + BeginOffset)^ + sizeOf(Integer);
        inc( BeginOffset, iMsgSize );
        BeginOffset := BeginOffset mod BufSize;
        dec( DataLen, iMsgSize );
      end;
    end;
  end;

begin
  //Preserve last error
  uErrorBackUp := GetLastError;
  try
    //Failed by default
    Result := False;
    try
      //Open mutex
      hMutex := OpenMutex(MUTEX_ALL_ACCESS,False,pchar(stQueueName + 'Mutex'));
      if hMutex = 0 then
        Exit;
      try

        //Wait for Mutex
        if WaitForSingleObject(hMutex,iTimeOut) <> WAIT_OBJECT_0 then
          Exit;

        //Open mapping (I cannot use OpenFileMapping because it fails with MapViewOfFile + FILE_MAP_WRITE	at least on my XP SP2)
        hMapping := CreateFileMapping(INVALID_HANDLE_VALUE,
          nil,
          PAGE_READWRITE,
          0,
          1,
          PChar(stQueueName));

        if hMapping = 0 then
          exit;
        if GetLastError <> ERROR_ALREADY_EXISTS then
        begin
          CloseHandle(hMapping);
          Exit;
        end;

        try
          //Map memory
          QueueBuffer := MapViewOfFile(hMapping,
            FILE_MAP_ALL_ACCESS,
            0,
            0,
            0);

          if not Assigned(QueueBuffer) then
            Exit;

          //Write message to the buffer
          try
            with QueueBuffer^ do
            begin
              //Message header (size)
              iMsgBufSize := sizeof(Integer) + MsgSize;

              //Enough space?
              if iMsgBufSize > BufSize then
                Exit;

              //If buffer full wait a bit
              iPending := iTimeOutBufferFull;
              while (iPending>0) and (BufSize - DataLen < iMsgBufSize) do
              begin
                //Allows reading buffer
                ReleaseMutex(hMutex);
                //Wait
                Sleep ( kTimeOutSendBufferFullMinTime );
                //Wait for Mutex
                if WaitForSingleObject(hMutex,iTimeOut) <> WAIT_OBJECT_0 then
                  Exit;
                //Next
                dec(iPending,kTimeOutSendBufferFullMinTime);
              end;

              //Frees required buffer memory
              FreeSpace ( iMsgBufSize );

              //Write Msg
              WriteToBuffer (QueueBuffer, MsgSize, sizeof(MsgSize));
              WriteToBuffer (QueueBuffer, MsgBuf, MsgSize);

              //Message sent
              Result := True;
            end;
          finally
            UnMapViewOfFile(QueueBuffer);
          end;
        finally
          CloseHandle(hMapping);
        end;

      finally
        ReleaseMutex(hMutex);
        CloseHandle(hMutex);
      end;

      //Open event
      hEvent := OpenEvent(EVENT_MODIFY_STATE,False,pchar(stQueueName + 'Event'));
      if hEvent = 0 then
        Exit;
      try
        //Launch event
        SetEvent(hEvent);
      finally
        CloseHandle(hEvent);
      end;

    except
      //We don't want exceptions when sending messages
    end;
  finally
    SetLastError (uErrorBackUp);
  end;
end;

{ TCimaMessageQueue }

constructor TCimaMessageQueue.Create(stQueueName: string;
  HWND: THandle; uMsg: cardinal; iBufferSize: Integer);
begin
  FTimeOutGet := kDefTimeOutGet;

  //Create Event Thread
  if uMsg <> 0 then
    FThread := TMsgEventThread.Create(stQueueName + 'Event', HWND, uMsg);

  //Create mutex
  FMutex := CreateMutex(nil,True,pchar(stQueueName + 'Mutex'));
  if FMutex = 0 then
    RaiseLastOSError;
  if GetLastError = ERROR_ALREADY_EXISTS then
    Raise exception.Create('Queue already exist');

  try
    //Create file mapping
    FMapping := CreateFileMapping(INVALID_HANDLE_VALUE,
      nil,
      PAGE_READWRITE,
      0,
      SizeOf(TQueueHeader) + iBufferSize,
      PChar(stQueueName));
    if FMapping = 0 then
      RaiseLastOSError;
    if GetLastError = ERROR_ALREADY_EXISTS then
      Raise exception.Create('Queue already exist');

    //Map memory
    FQueueBuffer := MapViewOfFile(FMapping,
      FILE_MAP_ALL_ACCESS,
      0,
      0,
      0);

    //Initialize
    with FQueueBuffer^ do
    begin
      BeginOffset := 0;
      DataLen := 0;
      BufSize := iBufferSize;
    end;
  finally
    //Allow access
    ReleaseMutex(FMutex);
  end;
end;

destructor TCimaMessageQueue.Destroy;
begin
  UnmapViewOfFile(@FQueueBuffer);
  CloseHandle(FMapping);
  CloseHandle(FMutex);
  if Assigned(FThread) then
    FreeAndNil(FThread);
  inherited;
end;

function TCimaMessageQueue.GetMessage(var Buffer;
  iBufferSize: Integer): Integer;
var
  dwResult: Cardinal;
begin
  //Wait for Mutex
  dwResult := WaitForSingleObject(FMutex,FTimeOutGet);
  if dwResult = WAIT_FAILED then
    RaiseLastOSError
  else if dwResult <> WAIT_OBJECT_0 then
    Raise Exception.Create('TCimaMessageQueue.GetMessage: Timeout or abandoned mutex');

  //Read message
  try
    with FQueueBuffer^ do
    begin
      //Queue is empty
      if DataLen = 0 then
      begin
        Result := 0;
        Exit;
      end;

      //Read msg size
      ReadFromBuffer( FQueueBuffer, BeginOffset, Result, sizeof(Result) );
      if Result > iBufferSize then
        Raise exception.Create('TCimaMessageQueue.GetMessage: buffer is not enough large');
      //Next offset
      inc(BeginOffset, sizeOf(Result));
      BeginOffset := BeginOffset mod BufSize;
      //Read msg
      ReadFromBuffer( FQueueBuffer, BeginOffset, Buffer, Result );
      //Next offset
      inc(BeginOffset, Result);
      BeginOffset := BeginOffset mod BufSize;
      //Discount data len
      dec(DataLen, Result + sizeof(integer));
    end;
  finally
    ReleaseMutex(FMutex);
  end;
end;

{ TMsgEventThread }

constructor TMsgEventThread.Create(stEventName: string;
  hHWND: THandle= 0; uMsg: cardinal = 0);
begin
  //Create event
  FEvent := CreateEvent(nil,False,False,pchar(stEventName));
  if FEvent = 0 then
    Raise exception.Create('Queue already exist');

  //Notified window
  FHWND := hHWND;
  FMsg := uMsg;

  //Launch!
  inherited Create(False);
end;

destructor TMsgEventThread.Destroy;
begin
  PulseEvent(FEvent);
  Terminate;
  WaitFor;
  CloseHandle(FEvent);
  inherited;
end;

procedure TMsgEventThread.Execute;
var
  dwResult: Cardinal;
begin
  repeat
    dwResult := WaitForSingleObject(FEvent,kNotifyLoopTime);
    if Terminated then
      Exit;
    if dwResult = WAIT_OBJECT_0	then
      PostMessage(FHWND,FMsg,0,0);
  until False;
end;


end.
