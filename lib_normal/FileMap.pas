unit   FileMap;   
    
  interface   
    
  uses   
      Windows,Messages,SysUtils,Classes,Graphics,Controls,Forms,StdCtrls,Dialogs;   
    
  type
      TFileMap=class(TComponent)   
      private   
          FMapHandle:THandle;                   //内存映射文件句柄   
          FMutexHandle:THandle;               //互斥句柄   
          FMapName:string;                         //内存映射对象   
          FSynchMessage:string;               //同步消息   
          FMapStrings:TStringList;         //存储映射文件信息   
          FSize:DWord;                                 //映射文件大小   
          FMessageID:DWord;                       //注册的消息号   
          FMapPointer:PChar;                     //映射文件的数据区指针   
          FLocked:Boolean;                         //锁定   
          FIsMapOpen:Boolean;                   //文件是否打开   
          FExistsAlready:Boolean;           //是否已经建立过映射文件   
          FReading:Boolean;                       //是否正在读取内存文件数据   
          FAutoSynch:Boolean;                   //是否同步   
          FOnChange:TNotifyEvent;           //当内存数据区内容改变时   
          FFormHandle:Hwnd;                       //存储本窗口的窗口句柄   
          FPNewWndHandler:Pointer;   
          FPOldWndHandler:Pointer;   
          procedure   SetMapName(Value:string);   
          procedure   SetMapStrings(Value:TStringList);   
          procedure   SetSize(Value:DWord);   
          procedure   SetAutoSynch(Value:Boolean);   
          procedure   EnterCriticalSection;   
          procedure   LeaveCriticalSection;   
          procedure   MapStringsChange(Sender:TObject);   
          procedure   NewWndProc(var   FMessage:TMessage);   
      public   
          constructor   Create(AOwner:TComponent);override;   
          destructor   Destroy;override;   
          procedure   OpenMap;   
          procedure   CloseMap;   
          procedure   ReadMap;   
          procedure   WriteMap;   
          property   ExistsAlready:Boolean   read   FExistsAlready;   
          property   IsMapOpen:Boolean   read   FIsMapOpen;   
      published   
          property   MaxSize:DWord   read   FSize   write   SetSize;   
          property   AutoSynchronize:Boolean   read   FAutoSynch   write   SetAutoSynch;   
          property   MapName:string   read   FMapName   write   SetMapName;   
          property   MapStrings:TStringList   read   FMapStrings   write   SetMapStrings;   
          property   OnChange:TNotifyEvent   read   FOnChange   write   FOnChange;   
      end;   
  implementation   
    
  //构造函数   
  constructor   TFileMap.Create(AOwner:TComponent);   
  begin   
      inherited   Create(AOwner);   
      FAutoSynch:=True;   
      FSize:=4096;   
      FReading:=False;   
      FMapStrings:=TStringList.Create;   
      FMapStrings.OnChange:=MapStringsChange;   
      FMapName:='Unique   &   Common   name';   
      FSynchMessage:=FMapName+'Synch-Now';   
      if   AOwner   is   TForm   then   
      begin   
          FFormHandle:=(AOwner   as   TForm).Handle;   
          //得到窗口处理过程的地址   
          FPOldWndHandler:=Ptr(GetWindowLong(FFormHandle,GWL_wNDPROC));   
          FPNewWndHandler:=MakeObjectInstance(NewWndProc);   
          if   FPNewWndHandler=nil   then   
              raise   Exception.Create('超出资源');   
          //设置窗口处理过程的新地址   
          SetWindowLong(FFormHandle,GWL_WNDPROC,Longint(FPNewWndHandler));   
      end   
      else   raise   Exception.Create('组件的所有者应该是TForm');   
  end;   
    
  //析构函数   
  destructor   TFileMap.Destroy;   
  begin   
      CloseMap;   
      //还原Windows处理过程地址   
      SetWindowLong(FFormHandle,GWL_WNDPROC,Longint(FPOldWndHandler));   
      if   FPNewWndHandler<>nil   then   
          FreeObjectInstance(FPNewWndHandler);   
      //释放对象   
      FMapStrings.Free;   
      FMapStrings:=nil;   
      inherited   destroy;   
  end;

//打开文件映射，并映射到进程空间   
  procedure   TFileMap.OpenMap;   
  var   
      TempMessage:array[0..255]   of   Char;   
  begin   
      if   (FMapHandle=0)   and   (FMapPointer=nil)   then   
      begin   
          FExistsAlready:=False;   
          //创建文件映射对象   
          FMapHandle:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,FSize,PChar(FMapName));   
          if   (FMapHandle=INVALID_HANDLE_VALUE)   or   (FMapHandle=0)   then   
              raise   Exception.Create('创建文件映射对象失败!')   
          else   
          begin   
          //判断是否已经建立文件映射了   
              if   (FMapHandle<>0)   and   (GetLastError=ERROR_ALREADY_EXISTS)   then   
                  FExistsAlready:=True;   //如果已经建立的话，就设它为TRUE；   
              //映射文件的使徒到进程的地址空间   
              FMapPointer:=MapViewOfFile(FMapHandle,FILE_MAP_ALL_ACCESS,0,0,0);   
              if   FMapPointer=nil   then   
                  raise   Exception.Create('映射文件的视图到进程的地址空间失败')   
              else   
              begin   
                  StrPCopy(TempMessage,FSynchMessage);   
                  //在WINDOWS中注册消息常量   
                  FMessageID:=RegisterWindowMessage(TempMessage);   
                  if   FMessageID=0   then   
                      raise   Exception.Create('注册消息失败')   
              end   
          end;   
          //创建互斥对象，在写文件映射空间时用到它，以保持数据同步   
          FMutexHandle:=Windows.CreateMutex(nil,False,PChar(FMapName+'.Mtx'));   
          if   FMutexHandle=0   then   
              raise   Exception.Create('创建互斥对象失败');   
          FIsMapOpen:=True;   
          if   FExistsAlready   then   //判断内存文件映射是否已打开   
              ReadMap   
          else   
              WriteMap;   
      end;   
  end;   
    
  //解除文件视图和内存映射空间的关系，并关闭文件映射   
  procedure   TFileMap.CloseMap;   
  begin   
      if   FIsMapOpen   then   
      begin   
          //释放互斥对象   
          if   FMutexHandle<>0   then   
          begin   
              CloseHandle(FMutexHandle);   
              FMutexHandle:=0;   
          end;   
          //关闭内存对象   
          if   FMapPointer<>nil   then   
          begin   
          //解除文件视图和内存映射空间的关系   
              UnMapViewOfFile(FMapPointer);   
              FMapPointer:=nil;   
          end;   
          if   FMapHandle<>0   then   
          begin   
          //并关闭文件映射   
              CloseHandle(FMapHandle);   
              FMapHandle:=0;   
          end;   
          FIsMapOpen:=False;   
      end;   
  end;   
    
  //读取内存文件映射内容   
  procedure   TFileMap.ReadMap;   
  begin   
      FReading:=True;   
      if(FMapPointer<>nil)   then   FMapStrings.SetText(FMapPointer);   
  end;   
    
  //向内存映射文件里写   
  procedure   TFileMap.WriteMap;   
  var   
      StringsPointer:PChar;   
      HandleCounter:integer;   
      SendToHandle:HWnd;   
  begin   
      if   FMapPointer<>nil   then   
      begin   
          StringsPointer:=FMapStrings.GetText;   
          //进入互斥状态，防止其他线程进入同步区域代码   
          EnterCriticalSection;   
          if   StrLen(StringsPointer)+1<=FSize   
              then   System.Move(StringsPointer^,FMapPointer^,StrLen(StringsPointer)+1)   
          else   
              raise   Exception.Create('写字符串失败，字符串太大！');   
          //离开互斥状态   
          LeaveCriticalSection;   
          //广播消息，表示内存映射文件内容已经修改   
          SendMessage(HWND_BROADCAST,FMessageID,FFormHandle,0);   
          //释放StringsPointer   
          StrDispose(StringsPointer);   
      end;   
  end;   
    
  //当MapStrings值改变时   
  procedure   TFileMap.MapStringsChange(Sender:TObject);   
  begin   
      if   FReading   and   Assigned(FOnChange)   then   
          FOnChange(Self)   
      else   if   (not   FReading)   and   FIsMapOpen   and   FAutoSynch   then   
          WriteMap;   
  end;   
    
  //设置MapName属性值   
  procedure   TFileMap.SetMapName(Value:string);   
  begin   
      if   (FMapName<>Value)   and   (FMapHandle=0)   and   (Length(Value)<246)   then   
      begin   
          FMapName:=Value;   
          FSynchMessage:=FMapName+'Synch-Now';   
      end;   
  end;   
    
  //设置MapStrings属性值   
  procedure   TFileMap.SetMapStrings(Value:TStringList);   
  begin   
      if   Value.Text<>FMapStrings.Text   then   
      begin   
          if   Length(Value.Text)<=FSize   then   
              FMapStrings.Assign(Value)   
          else   
              raise   Exception.Create('写入值太大');   
      end;   
  end;   
    
  //设置内存文件大小   
  procedure   TFileMap.SetSize(Value:DWord);   
  var   
      StringsPointer:PChar;   
  begin   
      if   (FSize<>Value)   and   (FMapHandle=0)   then   
      begin   
          StringsPointer:=FMapStrings.GetText;   
          if   (Value<StrLen(StringsPointer)+1)   then   
              FSize:=StrLen(StringsPointer)+1   
          else   FSize:=Value;   
          if   FSize<32   then   FSize:=32;   
          StrDispose(StringsPointer);   
      end;   
  end;   
    
  //设置是否同步   
  procedure   TFileMap.SetAutoSynch(Value:Boolean);   
  begin   
      if   FAutoSynch<>Value   then   
      begin   
          FAutoSynch:=Value;   
          if   FAutoSynch   and   FIsMapOpen   then   WriteMap;   
      end;   
  end;   
    
  //进入互斥，使得被同步的代码不能被别的线程访问   
  procedure   TFileMap.EnterCriticalSection;   
  begin   
      if     (FMutexHandle<>0)   and   not   FLocked   then   
      begin   
          FLocked:=(WaitForSingleObject(FMutexHandle,INFINITE)=WAIT_OBJECT_0);   
      end;   
  end;   
    
  //解除互斥关系，可以进入保护的同步代码区   
  procedure   TFileMap.LeaveCriticalSection;   
  begin   
      if   (FMutexHandle<>0)   and   FLocked   then   
      begin   
          ReleaseMutex(FMutexHandle);   
          FLocked:=False;   
      end;   
  end;   

  //消息捕获过程   
  procedure   TFileMap.NewWndProc(var   FMessage:TMessage);   
  begin   
      with   FMessage   do   
      begin   
          if   FIsMapOpen   then     //内存文件打开   
          {如果消息是FMessageID,且WParam不是FFormHandle，就调用   
            ReadMap去读取内存映射文件的内容，表示内存映射文件的   
            内容已变}   
              if   (Msg=FMessageID)   and   (WParam<>FFormHandle)   then   
                  ReadMap;   
          Result:=CallWindowProc(FPOldWndHandler,FFormHandle,Msg,wParam,lParam);   
      end;   
  end;   
    
  end.   

