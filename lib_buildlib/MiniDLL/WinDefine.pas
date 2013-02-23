unit WinDefine;

interface

const
  MAX_ADAPTER_NAME_LENGTH   =   256;
  MAX_ADAPTER_DESCRIPTION_LENGTH   =   128;
  MAX_ADAPTER_ADDRESS_LENGTH   =   8;

  RESULT_TYPE_RET_STRING :Integer = 0;
  RESULT_TYPE_MSG_STRING :Integer = 1;

const
  HEAP_NO_SERIALIZE               = $00000001;
  HEAP_GROWABLE                   = $00000002;
  HEAP_GENERATE_EXCEPTIONS        = $00000004;
  HEAP_ZERO_MEMORY                = $00000008;

  STATUS_WAIT_0     = $00000000;
  WAIT_OBJECT_0     = ((STATUS_WAIT_0 ) + 0 );
  STATUS_TIMEOUT    = $00000102;
  WAIT_TIMEOUT      = STATUS_TIMEOUT;

  MAX_PATH = 260;
  INVALID_HANDLE_VALUE = DWORD(-1);
  FILE_ATTRIBUTE_DIRECTORY            = $00000010;
  
type
  PFileTime = ^TFileTime;
  _FILETIME = record
    dwLowDateTime: DWORD;
    dwHighDateTime: DWORD;
  end;
  TFileTime = _FILETIME;

  PWin32FindDataA = ^TWin32FindDataA;
  PWin32FindDataW = ^TWin32FindDataW;
  PWin32FindData = PWin32FindDataA;
  _WIN32_FIND_DATAA = record
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: array[0..MAX_PATH - 1] of AnsiChar;
    cAlternateFileName: array[0..13] of AnsiChar;
  end;
  _WIN32_FIND_DATAW = record
    dwFileAttributes: DWORD;
    ftCreationTime: TFileTime;
    ftLastAccessTime: TFileTime;
    ftLastWriteTime: TFileTime;
    nFileSizeHigh: DWORD;
    nFileSizeLow: DWORD;
    dwReserved0: DWORD;
    dwReserved1: DWORD;
    cFileName: array[0..MAX_PATH - 1] of WideChar;
    cAlternateFileName: array[0..13] of WideChar;
  end;
  _WIN32_FIND_DATA = _WIN32_FIND_DATAA;
  TWin32FindDataA = _WIN32_FIND_DATAA;
  TWin32FindDataW = _WIN32_FIND_DATAW;
  TWin32FindData = TWin32FindDataA;

  LongRec = packed record
    case Integer of
      0: (Lo, Hi: Word);
      1: (Words: array [0..1] of Word);
      2: (Bytes: array [0..3] of Byte);
  end;

type
  POverlapped = ^TOverlapped;
  _OVERLAPPED = record
    Internal: DWORD;
    InternalHigh: DWORD;
    Offset: DWORD;
    OffsetHigh: DWORD;
    hEvent: THandle;
  end;
  TOverlapped = _OVERLAPPED;
  OVERLAPPED = _OVERLAPPED;

    TIP_ADDRESS_STRING   =   record 
          IPstring:   array   [0..15]   of   Char; 
    end; 
    PIP_ADDRESS_STRING   =   ^TIP_ADDRESS_STRING; 
    TIP_MASK_STRING   =   TIP_ADDRESS_STRING; 
    PIP_MASK_STRING   =   ^TIP_MASK_STRING;

    PIP_ADDR_STRING   =   ^TIP_ADDR_STRING;
    TIP_ADDR_STRING   =   record
      Next:   PIP_ADDR_STRING;
      IpAddress:   TIP_ADDRESS_STRING;     //IPµØÖ·×Ö·û´®
      IpMask:   TIP_MASK_STRING;     //×ÓÍøÑÚÂë×Ö·û´®
      Context:   DWORD;   //Netword   table   entry
    end;
    PIP_ADAPTER_INFO   =   ^TIP_ADAPTER_INFO;
    TIP_ADAPTER_INFO   =   packed   record
      Next:   PIP_ADAPTER_INFO;
      ComboIndex:   DWORD;
      AdapterName:   array   [0..MAX_ADAPTER_NAME_LENGTH   +   4-1]   of   Char;
      Description:   array   [0..MAX_ADAPTER_DESCRIPTION_LENGTH   +   4-1]   of   Char;
      AddressLength:   UINT;
      Address:   array   [0..MAX_ADAPTER_ADDRESS_LENGTH-1]   of   BYTE;
      Index:   DWORD;
      dwType:   UINT;
      DhcpEnabled:   UINT;
      CurrentIpAddress:   PIP_ADDR_STRING;
      IpAddressList:   TIP_ADDR_STRING;
      GatewayList:   TIP_ADDR_STRING;
      DhcpServer:   TIP_ADDR_STRING;
      HaveWins:   LongBool;
      PrimaryWinsServer:   TIP_ADDR_STRING;
      SecondaryWinsServer:   TIP_ADDR_STRING; 
    end;

  u_char = Char;
  u_short = Word;
  u_int = Integer;
  u_long = Longint;
  PBOOL = ^BOOL;
  LPDWORD = PDWORD;

  LPCTSTR = PAnsiChar; { should be PWideChar if UNICODE }
  LPTSTR = PAnsiChar; { should be PWideChar if UNICODE }

  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;

  SunW = packed record
    s_w1, s_w2: u_short;
  end;

  PInAddr = ^TInAddr;
  in_addr = record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;
  TInAddr = in_addr;

  TFarProc = Pointer;
  TFNThreadStartRoutine = TFarProc;
  PSecurityAttributes = ^TSecurityAttributes;
  _SECURITY_ATTRIBUTES = record
    nLength: DWORD;
    lpSecurityDescriptor: Pointer;
    bInheritHandle: BOOL;
  end;
  TSecurityAttributes = _SECURITY_ATTRIBUTES;

  PSystemTime = ^TSystemTime;
  _SYSTEMTIME = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
  TSystemTime = _SYSTEMTIME;

  LONGLONG = Int64;
  PLargeInteger = ^TLargeInteger;
  _LARGE_INTEGER = record
    case Integer of
    0: (
      LowPart: DWORD;
      HighPart: Longint);
    1: (
      QuadPart: LONGLONG);
  end;

  TLargeInteger = Int64;
  LARGE_INTEGER = _LARGE_INTEGER;


  ULARGE_INTEGER = record
    case Integer of
    0: (
      LowPart: DWORD;
      HighPart: DWORD);
    1: (
      QuadPart: LONGLONG);
  end;

  PULargeInteger = ^TULargeInteger;
  TULargeInteger = ULARGE_INTEGER;


Const
  DLL_PROCESS_ATTACH  =	1;
  DLL_PROCESS_DETACH	= 0;

Type
  LPPointerArray = ^TPointerArray;
  TPointerArray = array[word] of POINTER;

  LPByteArray = ^TByteArray;
  TByteArray = array[word] of byte;

  
implementation

end.
