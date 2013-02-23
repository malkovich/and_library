{-----------------------------------------------------------------------------
 Unit Name: uMsgThread
 Author:    xwing
 eMail :    xwing@263.Net ; MSN : xwing1979@hotmail.com
 Purpose:   Thread with message Loop
 History: 编程大本营 

 2003-7-15  Write thread class without use Delphi own TThread.
 2003-6-19, add function to Send Thread Message.            ver 1.0
            use Event List and waitforsingleObject
            your can use WindowMessage or ThreadMessage
 2003-6-18, Change to create a window to Recving message
 2003-6-17, Begin.
-----------------------------------------------------------------------------}
unit uMsgThread; 

interface
{$WARN SYMBOL_DEPRECATED OFF} 
{$DEFINE USE_WINDOW_MESSAGE}
uses
    Classes, windows, messages, forms, sysutils;

const
    NM_EXECPROC = $8FFF;
type
    EMsgThreadErr = class(Exception);
    
    TMsgThreadMethod = procedure of object;

    TMsgThread = class
    private
        SyncWindow  : HWND;
        FMethod     : TMsgThreadMethod;
        procedure SyncWindowProc(var Message: TMessage);

    private
        m_hThread   : THandle;
        threadid    : DWORD;

        {$IFDEF USE_WINDOW_MESSAGE}
        FWinName    : string;
        FMSGWin     : HWND;
        {$ELSE}
        FEventList  : TList;
        FCtlSect    : TRTLCriticalSection;
        {$ENDIF} 

        FException  : Exception;
        fDoLoop     : Boolean;
        FWaitHandle : THandle;

        {$IFDEF USE_WINDOW_MESSAGE}
        procedure MSGWinProc(var Message: TMessage);
        {$ELSE}
        procedure ClearSendMsgEvent;
        {$ENDIF} 

        procedure SetDoLoop(const Value: Boolean);
        procedure Execute; 
    protected
        Msg         :tagMSG; 
        {$IFNDEF USE_WINDOW_MESSAGE}
        uMsg        :TMessage;
        fSendMsgComp:THandle;
        {$ENDIF}

        procedure HandleException;
        procedure DoHandleException;virtual;

        //Inherited the Method to process your own Message
        procedure DoProcessMsg(var Msg:TMessage);virtual; 

        //if DoLoop = true then loop this procedure
        //Your can use the method to do some work needed loop.
        procedure DoMsgLoop;virtual; 

        //Initialize Thread before begin message loop
        procedure DoInit;virtual;
        procedure DoUnInit;virtual;

        procedure PostMsg(Msg:Cardinal;wParam:Integer;lParam:Integer);
        //When Use SendMsg method Must not use Synchronize Method in ThreadLoop !!!
        //otherwise will caurse DeadLock
        function SendMsg(Msg:Cardinal;wParam:Integer;lParam:Integer):Integer;

    public
        constructor Create(Loop:Boolean=False;ThreadName: string='');
        destructor destroy;override;

        // Return TRUE if the thread exists. FALSE otherwise
        function ThreadExists: BOOL; 

        procedure Synchronize(syncMethod:TMsgThreadMethod);

        function WaitFor:Longword;
        function WaitTimeOut(timeout:DWORD=4000):Longword;

        //postMessage to Quit,and Free(if FreeOnTerminater = true)
        //can call this in thread loop, don't use terminate property.
        procedure QuitThread;

        //just like Application.processmessage.
        procedure ProcessMessage;

        //enable thread loop, no waitfor message
        property DoLoop: Boolean read fDoLoop Write SetDoLoop;

    end; 

implementation

function msgThdInitialThreadProc(pv:Pointer):DWORD;stdcall;
var
    obj:TMsgThread;
begin
    obj := TMsgThread(pv);
    obj.execute;
    Result := 0;
end;

{ TMsgThread }
{//////////////////////////////////////////////////////////////////////////////}
constructor TMsgThread.Create(Loop:Boolean;ThreadName:string);
begin
    {$IFDEF USE_WINDOW_MESSAGE}
    if ThreadName <> '' then
        FWinName := ThreadName
    else
        FWinName := 'Thread Window';
    {$ELSE}
    FEventList := TList.Create;
    InitializeCriticalSection(fCtlSect);
    fSendMsgComp := CreateEvent(nil, True, False, nil);
    {$ENDIF}

    FDoLoop := Loop;            //default disable thread loop 

    //Create a Window for sync method
    SyncWindow := CreateWindow('STATIC','SyncWindow',WS_POPUP,0,0,0,0,0,0,hInstance,nil);
    SetWindowLong(SyncWindow, GWL_WNDPROC, Longint(MakeObjectInstance(SyncWindowProc)));   

    FWaitHandle := CreateEvent(nil, True, False, nil);
    //Create Thread
    m_hThread := CreateThread(nil,0,@msgThdInitialThreadProc,Self,0,threadid);
    if m_hThread = 0 then
        raise EMsgThreadErr.Create('不能创建线程。');
    //Wait until thread Message Loop started    
    WaitForSingleObject(FWaitHandle,INFINITE);
end;

{------------------------------------------------------------------------------}
destructor TMsgThread.destroy;
begin
    if m_hThread <> 0 then
        QuitThread;
    waitfor;

    //Free Sync Window
    DestroyWindow(SyncWindow);
    FreeObjectInstance(Pointer(GetWindowLong(SyncWindow, GWL_WNDPROC))); 

    {$IFDEF USE_WINDOW_MESSAGE}
    {$ELSE}
    FEventList.Free;
    DeleteCriticalSection(FCtlSect);
    CloseHandle(fSendMsgComp);
    {$ENDIF}
    
    inherited;
end;

{//////////////////////////////////////////////////////////////////////////////}
procedure TMsgThread.Execute;
var
    mRet:Boolean;
    aRet:Boolean;
begin
{$IFDEF USE_WINDOW_MESSAGE}
    FMSGWin := CreateWindow('STATIC',PChar(FWinName),WS_POPUP,0,0,0,0,0,0,hInstance,nil);
    SetWindowLong(FMSGWin, GWL_WNDPROC, Longint(MakeObjectInstance(MSGWinProc)));
{$ELSE}
    PeekMessage(Msg,0,WM_USER,WM_USER,PM_NOREMOVE); //Force system alloc a msgQueue
{$ENDIF}
    mRet := True;
    try
        DoInit;
        //notify Conctructor can returen.
        SetEvent(FWaitHandle);
        CloseHandle(FWaitHandle); 

        while mRet do   //Message Loop
        begin
            if fDoLoop then
            begin
                aRet := PeekMessage(Msg,0,0,0,PM_REMOVE);
                if aRet and (Msg.message <> WM_QUIT) then
                begin
                    {$IFDEF USE_WINDOW_MESSAGE}
                    TranslateMessage(Msg);
                    DispatchMessage(Msg); 

                    {$ELSE}
                    uMsg.Msg := Msg.message;
                    uMsg.wParam := Msg.wParam;
                    uMsg.lParam := Msg.lParam;
                    DoProcessMsg(uMsg);
                    {$ENDIF}
                    if Msg.message = WM_QUIT then
                        mRet := False;
                end;
                {$IFNDEF USE_WINDOW_MESSAGE}
                ClearSendMsgEvent;      //Clear SendMessage Event
                {$ENDIF}
                DoMsgLoop;
            end
            else begin
                mRet := GetMessage(Msg,0,0,0);
                if mRet then
                begin
                    {$IFDEF USE_WINDOW_MESSAGE}
                    TranslateMessage(Msg);
                    DispatchMessage(Msg);
                    {$ELSE}
                    uMsg.Msg := Msg.message; 

                    uMsg.wParam := Msg.wParam;
                    uMsg.lParam := Msg.lParam;
                    DoProcessMsg(uMsg);
                    ClearSendMsgEvent;      //Clear SendMessage Event
                    {$ENDIF}
                end;
            end;
        end;
        DoUnInit;
        {$IFDEF USE_WINDOW_MESSAGE}
        DestroyWindow(FMSGWin);
        FreeObjectInstance(Pointer(GetWindowLong(FMSGWin, GWL_WNDPROC)));
        {$ENDIF}
    except
        HandleException;
    end;
end;

{------------------------------------------------------------------------------}
{$IFNDEF USE_WINDOW_MESSAGE}
procedure TMsgThread.ClearSendMsgEvent;
var
    aEvent:PHandle;
begin
    EnterCriticalSection(FCtlSect);
    try
        if FEventList.Count <> 0 then
        begin
            aEvent := FEventList.Items[0];
            if aEvent <> nil then
            begin
                SetEvent(aEvent^);
                CloseHandle(aEvent^);
                Dispose(aEvent);
                WaitForSingleObject(fSendMsgComp,INFINITE);
            end;
            FEventList.Delete(0);
        end;
    finally
        LeaveCriticalSection(FCtlSect);
    end;
end;
{$ENDIF}

{------------------------------------------------------------------------------}
procedure TMsgThread.HandleException;
begin
    FException := Exception(ExceptObject);  //Get Current Exception object
    try
        if not (FException is EAbort) then
            Synchronize(DoHandleException);
    finally
        FException := nil;
    end;
end;
{------------------------------------------------------------------------------}
procedure TMsgThread.DoHandleException;
begin
    if FException is Exception then
        Application.ShowException(FException)
    else
        SysUtils.ShowException(FException, nil);
end;

{//////////////////////////////////////////////////////////////////////////////}
{$IFDEF USE_WINDOW_MESSAGE}
procedure TMsgThread.MSGWinProc(var Message: TMessage);
begin
    DoProcessMsg(Message);
    if Message.Msg < wm_user then
        with Message do
            Result:=DefWindowProc(FMSGWin,Msg,wParam,lParam);
end;
{$ENDIF}

{------------------------------------------------------------------------------}
procedure TMsgThread.DoProcessMsg(var Msg:TMessage);
begin 

end; 


{------------------------------------------------------------------------------}
procedure TMsgThread.ProcessMessage;
{$IFNDEF USE_WINDOW_MESSAGE}
var
    uMsg:TMessage;
{$ENDIF}
begin
    while PeekMessage(Msg,0,0,0,PM_REMOVE) do
    if Msg.message <> WM_QUIT then
    begin
        {$IFDEF USE_WINDOW_MESSAGE}
        TranslateMessage(Msg);
        DispatchMessage(msg);
        {$ELSE}
        uMsg.Msg := Msg.message;
        uMsg.wParam := Msg.wParam;
        uMsg.lParam := Msg.lParam;
        DoProcessMsg(uMsg);
        {$ENDIF}
    end; 

end;

{//////////////////////////////////////////////////////////////////////////////}
procedure TMsgThread.DoInit;
begin
end;

procedure TMsgThread.DoUnInit;
begin
end;

procedure TMsgThread.DoMsgLoop;
begin
    Sleep(0);
end; 

{//////////////////////////////////////////////////////////////////////////////}
function TMsgThread.ThreadExists: BOOL;
begin
    if m_hThread = 0 then
        Result := false
    else
        Result := True;
end;

{------------------------------------------------------------------------------}
procedure TMsgThread.QuitThread;
begin
    {$IFDEF USE_WINDOW_MESSAGE}
    PostMessage(FMSGWin,WM_QUIT,0,0);
    {$ELSE}
    PostThreadMessage(ThreadID,WM_QUIT,0,0);
    {$ENDIF}
end;

{------------------------------------------------------------------------------}
procedure TMsgThread.SetDoLoop(const Value: Boolean);
begin
    if Value = fDoLoop then Exit;
    fDoLoop := Value;
    if fDoLoop then
        PostMsg(WM_USER,0,0);
end;

{------------------------------------------------------------------------------}
function TMsgThread.WaitTimeOut(timeout:dword):Longword;
var
    xStart:Cardinal;
    H: THandle;
begin
    H := m_hThread;
    xStart:=GetTickCount;
    while WaitForSingleObject(h, 10) = WAIT_TIMEOUT do
    begin
        Application.ProcessMessages;
        if GetTickCount > (xStart + timeout) then
        begin
            TerminateThread(h, 0);
            Break;
        end;
    end;
    GetExitCodeThread(H, Result);    
end;

{------------------------------------------------------------------------------}
function TMsgThread.WaitFor: Longword;
var
    Msg: TMsg;
    H: THandle;
begin
    H := m_hThread;
    if GetCurrentThreadID = MainThreadID then
        while MsgWaitForMultipleObjects(1, H, False, INFINITE, QS_SENDMESSAGE) = WAIT_OBJECT_0 + 1 do
            PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE)
    else
        WaitForSingleObject(H, INFINITE);
    GetExitCodeThread(H, Result);
end;

{------------------------------------------------------------------------------}
procedure TMsgThread.PostMsg(Msg: Cardinal; wParam, lParam: Integer);
begin
    {$IFDEF USE_WINDOW_MESSAGE}
    postMessage(FMSGWin,Msg,wParam,lParam);
    {$ELSE}
    EnterCriticalSection(FCtlSect);
    try
        FEventList.Add(nil);
        PostThreadMessage(ThreadID,Msg,wParam,lParam);
    finally
        LeaveCriticalSection(FCtlSect);
    end;
    {$ENDIF}
end;

{------------------------------------------------------------------------------}
function TMsgThread.SendMsg(Msg: Cardinal; wParam, lParam: Integer):Integer;
{$IFNDEF USE_WINDOW_MESSAGE}
var
    aEvent:PHandle;
{$ENDIF}
begin
    {$IFDEF USE_WINDOW_MESSAGE}
    Result := SendMessage(FMSGWin,Msg,wParam,lParam);
    {$ELSE}
    EnterCriticalSection(FCtlSect);
    try
        New(aEvent);
        aEvent^ := CreateEvent(nil, True, False, nil);
        FEventList.Add(aEvent);
        PostThreadMessage(ThreadID,Msg,wParam,lParam);
    finally
        LeaveCriticalSection(FCtlSect);
    end;
    WaitForSingleObject(aEvent^,INFINITE); 

    Result := uMsg.Result;
    SetEvent(fSendMsgComp);
    {$ENDIF}
end; 


{------------------------------------------------------------------------------}
procedure TMsgThread.Synchronize(syncMethod: TMsgThreadMethod);
begin
    FMethod := syncMethod;
    SendMessage(SyncWindow,NM_EXECPROC,0,Longint(Self));
end; 

{------------------------------------------------------------------------------}
procedure TMsgThread.SyncWindowProc(var Message: TMessage);
begin
    case Message.Msg of
        NM_EXECPROC:
        with TMsgThread(Message.lParam) do
        begin
            Message.Result := 0;
            try
                FMethod;
            except
                raise EMsgThreadErr.Create('执行同步线程方法错误。');
            end;
        end; 

        else
            Message.Result:=DefWindowProc(SyncWindow,Message.Msg,Message.wParam,Message.lParam);
    end;
end;


end.

