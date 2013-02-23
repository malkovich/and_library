//ICMP_Define.PAS
unit ICMP_Define;

interface
uses winsock;
type
DWORD=LongWord;
THandle=LongWord;

THostTraceMultiReply=record
                 dwError : DWORD; //GetLastError for this host
Address : in_addr; //The IP address of the replier
        minRTT  : DWORD;  //Minimum round trip time in milliseconds
                 avgRTT  : DWORD;  //Average round trip time in milliseconds
                 maxRTT  : DWORD;  //Maximum round trip time in milliseconds
                 end;

THostTraceSingleReply=record
                      dwError:DWORD;  //GetLastError for this replier
             Address:in_addr;  //The IP address of the replier
             RTT:DWORD; //Round Trip time in milliseconds for this replier
                      end;

PIPOptionInformation = ^TIPOptionInformation;

TIPOptionInformation =
  record
        TTL: Byte;
     TOS: Byte;
        Flags: Byte;
     OptionsSize: Byte;
        OptionsData: PChar;
  end;

PIcmpEchoReply = ^TIcmpEchoReply;
TIcmpEchoReply =
  record
        Address: DWORD;
        Status: DWORD;
        RTT: DWORD;
        DataSize:Word;
        Reserved: Word;
        Data: Pointer;
        Options: TIPOptionInformation;
  end;
const
    ULONG_MAX=1024;
function IcmpCreateFile():THandle;stdcall external 'ICMP.dll';
function IcmpCloseHandle(Handle:THandle):Boolean;stdcall external 'ICMP.dll';
function IcmpSendEcho(Handle:THandle;DestAddr:DWORD;
         RequestData: Pointer;RequestSize: Word;RequestOptions: PIPOptionInformation;
         ReplyBuffer: Pointer;ReplySize: DWORD;Timeout: DWORD): DWORD;stdcall external 'ICMP.dll';

implementation

end.