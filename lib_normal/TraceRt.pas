unit TraceRt;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, winsock2;

procedure TraceRouter (SelfIP, RemoteIP: String);

implementation

const
  PACKET_SIZE     = 32;
  MAX_PACKET_SIZE = 512;
  TRACE_PORT      = 34567;
  LOCAL_PORT      = 5555;

type
  s32     = Integer;
  u32     = DWORD;
  u8      = Byte;
  u16     = word;       PU16 = ^U16;

  //
  //IP Packet Header
  //
  PIPHeader = ^YIPHeader;
  YIPHeader = record
    u8verlen    : u8;//4bits ver, 4bits len, len*4=true length
    u8tos       : u8;//type of service, 3bits 优先权(现在已经被忽略), 4bits TOS, 最多只能有1bit为1
    u16totallen : u16;//整个IP数据报的长度，以字节为单位。
    u16id       : u16;//标识主机发送的每一份数据报。
    u16offset   : u16;//3bits 标志，13bits片偏移
    u8ttl       : u8;//生存时间字段设置了数据报可以经过的最多路由器数。
    u8protol    : u8;//协议类型，6表示传输层是TCP协议。
    u16checksum : u16;//首部检验和。
    u32srcaddr  : u32;//源IP地址，不是‘xxx.xxx.xxx.xxx’的形势哦
    u32destaddr : u32;//目的IP地址，同上
  end;

  //
  //ICMP Packet Header
  //
  PICMPHeader = ^YICMPHeader;
  YICMPHeader = record
    u8type      : u8;
    u8code      : u8;
    u16chksum   : u16;
    u16id       : u16;
    u16seq      : u16;
  end;

function DecodeIcmpReply( pbuf: PChar; var seq: s32 ): string;
var
  pIpHdr   : PChar;
  pIcmphdr : PICMPHeader;
  sip      : string;
  ttl      : integer;
begin
  pIpHdr := pbuf;
  sip := inet_ntoa( TInAddr( PIPHeader(pIpHdr)^.u32srcaddr ) );
  ttl := PIPHeader(pIpHdr)^.u8ttl;

  Inc( pIpHdr, (PIPHeader(pIpHdr)^.u8verlen and $0F) * 4 );
  pIcmpHdr := PICMPHeader(pIpHdr);

  result := '';
  if pIcmpHdr^.u8type = 3 then  //目的不可达信息，Trace完成
     seq := 0;
  if pIcmpHdr^.u8type = 11 then  //超时信息，正在Trace
     result := Format( '%4d%32s%8d', [seq, sip, ttl] );
end;

procedure ErrMsg( msg: string );
begin
  MessageBox( 0, PChar(msg), 'Ping Program Error', MB_ICONERROR );
end;

procedure InitialWinSocket;
var
  wsa : TWSAData;
begin
  if WSAStartup( $0202, wsa ) <> 0 then
     ErrMsg( 'Windows socket is not responed.' );
end;

procedure FinallyWinSocket;
begin
  if WSACleanup <> 0 then
     ErrMsg( 'Windows socket can not be closed.' );
end;

procedure TraceRouter (SelfIP, RemoteIP: String);
const
  SIO_RCVALL = IOC_IN or IOC_VENDOR or 1;
var
  rawsock  : TSocket;
  pRecvBuf : PChar;
  FromAdr  : TSockAddr;
  FromLen  : s32;
  fd_read  : TFDSet;
  timev    : TTimeVal;
  sReply   : string;
  udpsock  : TSocket;
  ret      : s32;
  DestAdr  : TSockAddr;
  pSendBuf : PChar;
  ttl, opt : s32;
  pHost    : PHostEnt;
begin
  //创建一个RAWSOCK接收回应ICMP包
  rawsock := socket( AF_INET, SOCK_RAW, IPPROTO_ICMP );

  FromAdr.sin_family := AF_INET;
  FromAdr.sin_port := htons(0);
  FromAdr.sin_addr.S_addr := inet_addr(PChar(SelfIP));  //换成你的IP

  //如果不bind就无法接收包了~~~因为下面还要创建一个UDPSOCK
  bind( rawsock, @FromAdr, SizeOf(FromAdr) );

  Opt := 1;
  WSAIoctl( rawsock, SIO_RCVALL, @Opt, SizeOf(Opt), nil, 0, @ret, nil, nil );

  //接收ICMP回应包的缓冲区
  pRecvBuf := AllocMem( MAX_PACKET_SIZE );

  //创建一个UDPSOCK发送探测包
  udpsock := socket( AF_INET, SOCK_DGRAM, IPPROTO_UDP );

  //要发送的UDP数据
  pSendBuf := AllocMem( PACKET_SIZE );
  FillChar( pSendBuf^, PACKET_SIZE, 'C' );

  FillChar( DestAdr, sizeof(DestAdr), 0 );
  DestAdr.sin_family := AF_INET;
  DestAdr.sin_port := htons( TRACE_PORT );
  DestAdr.sin_addr.S_addr := inet_addr( PChar(RemoteIP) );

  //如果edit1.text不是IP地址，则尝试解析域名
  if DestAdr.sin_addr.S_addr = INADDR_NONE then
  begin
    pHost := gethostbyname( PChar(RemoteIP) );
    if pHost <> nil then
    begin
      move( pHost^.h_addr^^, DestAdr.sin_addr, pHost^.h_length );
      DestAdr.sin_family := pHost^.h_addrtype;
      DestAdr.sin_port := htons( TRACE_PORT );
      OutputDebugString(PChar( RemoteIP +'IP地址->'+ inet_ntoa(DestAdr.sin_addr) ));
    end else
    begin
      OutputDebugString(PChar( '解析域名: ' + RemoteIP + '出错。' ));
      closesocket( rawsock );
      closesocket(udpsock);
      FreeMem( pSendBuf );
      FreeMem( pRecvBuf );
      exit;
    end;
  end;

  OutputDebugString(PChar( 'Trace route ' + RemoteIP + '......' ));

  //开始Trace!!!
  ttl := 1;
  while True do
  begin
    //设置TTL，使我们发送的UDP包的TTL依次累加
    setsockopt( udpsock, IPPROTO_IP, IP_TTL, @ttl, sizeof(ttl) );
    //发送UDP包到HOST
    sendto( udpsock, pSendBuf^, PACKET_SIZE, 0, DestAdr, sizeof(DestAdr) );

    FD_ZERO( fd_read );
    FD_SET( rawsock, fd_read );
    timev.tv_sec  := 5;
    timev.tv_usec := 0;

    if select( 0, @fd_read, nil, nil, @timev ) < 1 then
       break;

    if FD_ISSET( rawsock, fd_read ) then
    begin
      FillChar( pRecvBuf^, MAX_PACKET_SIZE, 0 );
      FillChar( FromAdr, sizeof(FromAdr), 0 );
      FromAdr.sin_family := AF_INET;
      FromLen := sizeof( FromAdr );
      recvfrom( rawsock, pRecvBuf^, MAX_PACKET_SIZE, 0, FromAdr, FromLen );

      sReply := DecodeIcmpReply( pRecvBuf, ttl );
      if sReply <> '' then
      begin
        OutputDebugString(PChar( sReply ));
      end;
      if ttl = 0 then //如果收到目标主机的相应包，DecodeIcmpReply会把ttl==0
         break;
    end;

    Inc( ttl );
    Sleep( 110 );
  end; //while not bStop do

  OutputDebugString( '追踪路由完成。' );
  OutputDebugString( ' ' );

  closesocket( rawsock );
  closesocket(udpsock);
  FreeMem( pSendBuf );
  FreeMem( pRecvBuf );
end;

end.


