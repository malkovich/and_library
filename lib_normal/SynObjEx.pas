//---------------------------------------------------------------------------   
  Unit   SynObjEx;   
    
  Interface   
    
  Uses   Classes,   Windows,   Sysutils;   
  //---------------------------------------------------------------------------   
  Type   
  //---------------------------------------------------------------------------   
      ESyncObjsError   =   Exception;   
      THandle   =   Integer;   
  //---------------------------------------------------------------------------   
  //   THandleObject   have   defined   in   syncObjs   but   FHandle   declared   
  //   in   Private   Section   so   it   useless   in   other   hierarchy   object   
  //---------------------------------------------------------------------------   
      THandleObject   =   Class(   TObject   )   
      Protected   
          FName   :String;   
          FHandle   :THandle;   
          FLastError   :Integer;   
      Public   
          Constructor   Create;   
          Destructor   Destroy;   Override;   
    
          Property   LastError   :Integer   Read   FLastError;   
          Property   Handle   :THandle   Read   FHandle;   
          Property   Name   :String   Read   FName;   
      End;   
  //---------------------------------------------------------------------------   
      TWaitResult   =   (   wrSignaled,   wrTimeout,   wrAbandoned,   wrError   );   
    
      TEvent   =   Class(   THandleObject   )   
      Public   
          Constructor   Create(   EventAttributes   :PSecurityAttributes;   ManualReset,   
              InitialState   :Boolean;   Const   Name   :String   );   
          Function   WaitFor(   Timeout   :DWORD   )   :TWaitResult;   
          Procedure   SetEvent;   
          Procedure   ResetEvent;   
      End;   
  //---------------------------------------------------------------------------   
      TMutex   =   Class(   THandleObject   )   
      Private   
          FFirstCreated   :   Boolean;   
      Public   
          Constructor   Create(   Const   Name   :String   );   
          Function   Get(   TimeOut   :Integer   )   :Boolean;   
          Function   Release   :Boolean;   
    
          Property   FirstCreated   :Boolean   Read   FFirstCreated;   
      End;   
  //---------------------------------------------------------------------------   
      TSharedMemory   =   Class(   THandleObject   )   
      Private   
          FSize   :Integer;   
          FFirstCreated   :Boolean;   
          FFileView   :Pointer;   
      Public   
          Constructor   Create(Const   Name   :String;   Size   :Integer);   
          Destructor   Destroy;   Override;   
    
          Property   Size   :Integer   Read   FSize;   
          Property   Buffer   :Pointer   Read   FFileView;   
          Property   FirstCreated   :Boolean   Read   FFirstCreated;   
      End;   
  //---------------------------------------------------------------------------   
  //   TBasicEvent   
  //   Win32   events   are   very   basic.     They   are   either   signaled   or   non-signaled.   
  //   The   TBasicEvent   class   creates   a   "typed"   TEvent,   by   using   a   block   of   shared   
  //   memory   to   hold   an   "EventKind"   property.     The   shared   memory   is   also   used   
  //   to   hold   an   ID,   which   is   important   when   running   multiple   clients,   and   
  //   a   Data   area   for   communicating   data   along   with   the   event   
  //---------------------------------------------------------------------------   
      TEventKind   =   1..$7FFFFFFF;   
  Type   
      PBasicEventInfo   =   ^TBasicEventInfo;   
      TBasicEventInfo   =   Record   
          Kind   :TEventKind;   
          Data   :Pointer;   
      End;   
    
      TBasicEvent   =   Class(   TEvent   )   
      Private   
          FSharedMem   :TSharedMemory;   
          FEventInfo   :PBasicEventInfo;   
    
          Function   GetKind   :TEventKind;   
          Procedure   SetKind(   Value   :TEventKind   );   
          Function   GetData   :Pointer;   
          Procedure   SetData(   Value   :Pointer   );   
      Public   
          Constructor   Create(   Const   Name   :String;   Manual   :Boolean   );   
          Destructor   Destroy;   Override;   
          Procedure   SetEvent(   EventKind   :TEventKind   );   Overload;   
    
          Property   Kind   :TEventKind   Read   GetKind   Write   SetKind;   
          Property   Data   :Pointer   Read   GetData   Write   SetData;   
      End;   
  //---------------------------------------------------------------------------   
  //   TCriticalSection   
  //---------------------------------------------------------------------------   
      TCriticalSection   =   Class(   TObject   )   
      Private   
          FSection:   TRTLCriticalSection;   
      Public   
          Constructor   Create;   
          Destructor   Destroy;   Override;   
          Procedure   Enter;   
          Procedure   Leave;   
          Procedure   TryEnter;   
      End;   
  //---------------------------------------------------------------------------   
  Implementation   
  Uses   TypInfo;   
  //---------------------------------------------------------------------------   
    
    
  //---------------------------------------------------------------------------   
  //   {   DONE   -oBin.   -cSynObjs   :   THandleObject   Class   Implementation   
  //---------------------------------------------------------------------------   
  Constructor   THandleObject.Create;   
  Begin   
      Inherited   Create;   
  End;   
  //---------------------------------------------------------------------------   
  Destructor   THandleObject.Destroy;   
  Begin   
      If   FHandle   <>   0   Then   CloseHandle(   FHandle   );   
  End;   
  //---------------------------------------------------------------------------   
    
    
  //---------------------------------------------------------------------------   
  //   {   DONE   -oBin.   -cSynObjs   :   TEvent   Class   Implementation   
  //---------------------------------------------------------------------------   
  Constructor   TEvent.Create(   EventAttributes   :PSecurityAttributes;   ManualReset,   
      InitialState   :Boolean;   Const   Name   :String   );   
  Begin   
      Inherited   Create;   
      FHandle   :=   CreateEvent(   EventAttributes,   ManualReset,   
          InitialState,   PChar(Name)   );   
      If   FHandle   =   0   Then   Abort;   
  End;   
  //---------------------------------------------------------------------------   
  Function   TEvent.WaitFor(   Timeout:   DWORD   )   :TWaitResult;   
  Begin   
      Case   WaitForSingleObject(   FHandle,   Timeout   )   Of   
          WAIT_ABANDONED:   Result   :=   wrAbandoned;   
          WAIT_OBJECT_0:   Result   :=   wrSignaled;   
          WAIT_TIMEOUT:   Result   :=   wrTimeout;   
          WAIT_FAILED:   
          Begin   
              Result   :=   wrError;   
              FLastError   :=   GetLastError;   
          End;   
      Else   
          Result   :=   wrError;   
      End;   
  End;   
  //---------------------------------------------------------------------------   
  Procedure   TEvent.SetEvent;   
  Begin   
      Windows.SetEvent(   FHandle   );   
  End;   
  //---------------------------------------------------------------------------   
  Procedure   TEvent.ResetEvent;   
  Begin   
      Windows.ResetEvent(   FHandle   );   
  End;   
  //---------------------------------------------------------------------------   
    
    
  //---------------------------------------------------------------------------   
  //   {   DONE   -oBin.   -cSynObjs   :   TMutex   Class   Implementation   
  //---------------------------------------------------------------------------   
  Constructor   TMutex.Create(   Const   Name   :String   );   
  Begin   
      Inherited   Create;   
      FHandle   :=   CreateMutex(   Nil,   False,   PChar(   Name   )   );   
      If   FHandle   =   0   Then   Abort;   
      FFirstCreated   :=   (   GetLastError   =   0   );   
  End;   
  //---------------------------------------------------------------------------   
  Function   TMutex.Get(   TimeOut   :Integer   )   :Boolean;   
  Begin   
      Result   :=   (   WaitForSingleObject(   FHandle,   TimeOut   )   =   WAIT_OBJECT_0   );   
  End;   
  //---------------------------------------------------------------------------   
  Function   TMutex.Release   :Boolean;   
  Begin   
      Result   :=   ReleaseMutex(   FHandle   );   
  End;   
  //---------------------------------------------------------------------------   
    
    
  //---------------------------------------------------------------------------   
  {   TSharedMem   Class   Implementation   }   
  //---------------------------------------------------------------------------   
  Constructor   TSharedMemory.Create(   Const   Name   :String;   Size   :Integer   );   
  Begin   
      Inherited   Create;   
      FName   :=   Name;   
      FSize   :=   Size;   
      //   CreateFileMapping,   when   called   with   $FFFFFFFF   for   the   hanlde   value,   
      //   creates   a   region   of   shared   memory   
      FHandle   :=   CreateFileMapping(   $FFFFFFFF,   Nil,   PAGE_READWRITE,   0,   
          Size,   PChar(   Name   )   );   
      If   FHandle   =   0   Then   Abort;   
      FFirstCreated   :=   (   GetLastError   =   0   );   
      //   We   still   need   to   map   a   pointer   to   the   handle   of   the   shared   memory   region   
      FFileView   :=   MapViewOfFile(   FHandle,   FILE_MAP_WRITE,   0,   0,   Size   );   
      If   FFileView   =   Nil   Then   Abort;   
  End;   
  //---------------------------------------------------------------------------   
  Destructor   TSharedMemory.Destroy;   
  Begin   
      If   FFileView   <>   Nil   Then   UnmapViewOfFile(   FFileView   );   
      Inherited   Destroy;   
  End;   
  //---------------------------------------------------------------------------   
    
    
  //---------------------------------------------------------------------------   
  //   {   DONE   -oBin.   -cSynObjs   :   TBasicEvent   Class   Implementation   
  //---------------------------------------------------------------------------   
  Constructor   TBasicEvent.Create(   Const   Name   :String;   Manual   :Boolean   );   
  Begin   
      Inherited   Create(   Nil,   Manual,   False,   Name   );   
      FSharedMem   :=   TSharedMemory.Create(   Format(   'BasicEvent.%s',   [Name]   ),   
          SizeOf(   TBasicEventInfo   )   );   
      FEventInfo   :=   FSharedMem.Buffer;   
  End;   
  //---------------------------------------------------------------------------   
  Destructor   TBasicEvent.Destroy;   
  Begin   
      FSharedMem.Free;   
      Inherited   Destroy;   
  End;   
  //---------------------------------------------------------------------------   
  Procedure   TBasicEvent.SetEvent(   EventKind   :TEventKind   );   
  Begin   
      FEventInfo.Kind   :=   EventKind;   
      Inherited   SetEvent;   
  End;   
  //---------------------------------------------------------------------------   
  Function   TBasicEvent.GetData   :Pointer;   
  Begin   
      Result   :=   FEventInfo.Data;   
  End;   
  //---------------------------------------------------------------------------   
  Procedure   TBasicEvent.SetData(   Value   :Pointer   );   
  Begin   
      FEventInfo.Data   :=   Value;   
  End;   
  //---------------------------------------------------------------------------   
  Function   TBasicEvent.GetKind   :TEventKind;   
  Begin   
      Result   :=   FEventInfo.Kind;   
  End;   
  //---------------------------------------------------------------------------   
  Procedure   TBasicEvent.SetKind(   Value   :TEventKind   );   
  Begin   
      FEventInfo.Kind   :=   Value;   
  End;   
  //---------------------------------------------------------------------------   
    
    
  //---------------------------------------------------------------------------   
  //   {   TODO   -oBin.   -cSynObjs   :   TCriticalSection   Class   Implementation   
  //---------------------------------------------------------------------------   
  Constructor   TCriticalSection.Create;   
  Begin   
      InitializeCriticalSection(   FSection   );   
  End;   
  //---------------------------------------------------------------------------   
  Destructor   TCriticalSection.Destroy;   
  Begin   
      DeleteCriticalSection(   FSection   );   
      Inherited   Destroy;   
  End;   
  //---------------------------------------------------------------------------   
  Procedure   TCriticalSection.Enter;   
  Begin   
      EnterCriticalSection(   FSection   );   
  End;   
  //---------------------------------------------------------------------------   
  Procedure   TCriticalSection.Leave;   
  Begin   
      LeaveCriticalSection(   FSection   );   
  End;   
  //---------------------------------------------------------------------------   
  procedure   TCriticalSection.TryEnter;   
  Begin   
      TryEnterCriticalSection(   FSection   );   
  End;   
  //---------------------------------------------------------------------------   
  End.