unit TraceRt2;

interface

uses
  Windows, Messages, SysUtils, Classes, Winsock,ICMP_Define;


function TraceRouter(Host: String; var Routers: String): Integer;
function FormatTraceRouter(Host: String): String;
function FormatTraceRouter2(Host: PChar): PChar; Stdcall;

implementation

procedure DBG (msg: string);
begin
  outputdebugstring (pchar(msg));
end;


function Ping(dwAddr:DWORD;TimeOut:Word;var htsr:THostTraceSingleReply;nTTL:Byte):boolean;
var
      IPOpt:TIPOptionInformation;// IP Options for packet to send
      pReqData,pRevData:PChar;
      pIPE:PIcmpEchoReply;// ICMP Echo reply buffer
      FSize: DWORD;
      BufferSize:DWORD;
      temp:Integer;
      hICMP:THandle;
begin
        Result:=false;
        hICMP:=IcmpCreateFile();
        if hICMP=INVALID_HANDLE_VALUE then
           begin
                //Could not get a valid icmp handle
                exit;
           end;
        FSize := 40; //package size
        BufferSize := SizeOf(TICMPEchoReply) + FSize;
        GetMem(pRevData,FSize);
        GetMem(pIPE,BufferSize);

        //set up the option structure
        ZeroMemory(@IPOpt,SizeOf(TIPOptionInformation));
        IPOpt.TTL:=nTTL;

        FillChar(pIPE^, SizeOf(pIPE^),0);
        pIPE^.Data := pRevData;

        GetMem(pReqData,5);//data package size = 5 byte
        FillChar(pReqData^,5,65);

        temp:=IcmpSendEcho(hICMP, dwAddr, pReqData, 5,
                    @IPOpt,pIPE, BufferSize, TimeOut);
        if temp=0 then
             begin
                  htsr.dwError:=GetLastError();
             end

        else
            begin
                 //ping success,copy info to return structure;
                 htsr.Address.S_addr:=pIPE^.Address;
                 htsr.RTT:=pIPE^.RTT;
                 Result:=true;
            end;
        //Free up the memory we allocated
        FreeMem(pRevData);
        FreeMem(pIPE);
        //Close the ICMP handle
        IcmpCloseHandle(hIcmp);
end;


function Tracert(dwAddr:DWORD; dwPingsPerHost:DWORD):String;
var
     dwTimeOut : DWORD;
         nHops : Byte;
  bReachedHost : Boolean;
           i,j : Byte;
          htrr : THostTraceMultiReply;
          htsr : THostTraceSingleReply;
      totalRTT : DWORD;
    bPingError : Boolean;
begin
     Result := '';
     //set init value
     dwTimeOut:=3000;//this value changed according the net condition
     nHops:=30;
     bReachedHost:=false;
//
//   DBG(Format(¡¯Tracing route to %s ¡¯#13#10¡¯over a maximum of %d hpos¡¯,
//                                 [edtIP.Text,nHops]));

     for i:=1 to nHops do
         begin
              if bReachedHost then
                 begin
                      DBG('Trace Complete');
                      Break;
                 end;
              htrr.dwError := 0;
              htrr.minRTT  := ULONG_MAX;
              htrr.avgRTT  := 0;
              htrr.maxRTT  := 0;

              //Iterate through all the pings for each host
              totalRTT := 0;
              htsr.Address.S_addr := 0;
              htsr.dwError := 0;
              bPingError:=false;

              for j:=1 to dwPingsPerHost do
                  begin
                       if bPingError Then break;
                       if (Ping(dwAddr,dwTimeOut,htsr,i))then
                               if (htsr.dwError=0)then
                                  begin
                                       inc(totalRTT,htsr.RTT);//acumulate total time
                                       //Store away the RTT etc
                                       if (htsr.RTT < htrr.minRTT)then htrr.minRTT:=htsr.RTT;
                                       if (htsr.RTT > htrr.maxRTT)then htrr.maxRTT:=htsr.RTT;
                                  end //if (htsr.dwError=0)then
                               else //if (htsr.dwError=0)then
                                    begin
                                         htrr.dwError:=htsr.dwError;
                                         bPingError:=true;
                                    end
                       else//if (Ping(dwAddr,dwTimeOut,htsr,i))then
                              begin//ping failed
                                   DBG(inttostr(i)+' Ping failed');
                              end;
                  end;// for j:=1 to dwPingsPerHost do
              htrr.Address := htsr.Address;
              if (htrr.dwError = 0)then
                 htrr.avgRTT := Round(totalRTT / dwPingsPerHost)
              else
                  begin
                       htrr.minRTT := 0;
                       htrr.avgRTT := 0;
                       htrr.maxRTT := 0;
                  end;

              //show trace result here
              if htrr.dwError=0 then
                 begin
                   if Length (Result) > 0  then
                     Result := Result + ',';

                   Result := Result + inet_ntoa (htrr.Address);

//                 DBG(Format('%d  %d ms  %d ms  %d ms  %d.%d.%d.%d'#13#10,
//                 [i,
//                 htrr.minRTT,
//                 htrr.avgRTT,
//                 htrr.maxRTT,
//                 ord(htrr.Address.S_un_b.s_b1),
//                 ord(htrr.Address.S_un_b.s_b2),
//                 ord(htrr.Address.S_un_b.s_b3),
//                 ord(htrr.Address.S_un_b.s_b4)]));

                 end
              else
                  DBG(Format('%d    Error:%d',[i,htrr.dwError]));

              if (dwaddr = DWORD (htrr.Address.S_addr))then
              //reach the final host
                 bReachedHost:=true;

         end;// of for i:=1 to nHops do


end;


function TraceRouter(Host: String; var Routers: String): Integer;
var
   WSAData:TWSAData;
   dwAddr:DWORD;
   hp:phostent;
   SL:TStringList;
begin
     Result := 0;
     //init winsock dll
     if (WSAStartup(MAKEWORD(2,0),WSAData) <> 0) then
        raise Exception.Create('Winsock Version Error');
     ZeroMemory(Addr(dwAddr),sizeof(dwAddr));
     //resolve IP
     //convert form dotted address
     dwAddr:=inet_addr(pchar(Host));
     if (dwAddr = DWORD(INADDR_NONE)) then
        begin
             hp:=gethostbyname(pchar(Host));
             if hp=NIL then
                begin
                     DBG('Failed to resolve host IP');
                     Exit//Failed to resolve host;
                end
             else
                 CopyMemory(Addr(dwAddr),hp.h_addr^,hp.h_length);
        end;
//     DBG(Format(¡¯Resolve Target: %d.%d.%d.%d¡¯,[LoByte(LoWord(dwAddr)),
//                                                               HiByte(LoWord(dwAddr)),
//                                                               LoByte(HiWord(dwAddr)),
//                                                               HiByte(HiWord(dwAddr))]));
     //trace route
     //icmp function must be declared.
     Routers := Tracert(dwAddr,1);
     SL := TStringList.Create;
     Result := ExtractStrings ([','],[], PChar(Routers), SL);
     SL.Free;
     //release winsock dll
     WSACleanUP;
end;

function FormatTraceRouter(Host: String): String;
var
  Routers: String;
  Count: Integer;
begin
  Result := '';
  Host := Trim(Host);
  Count := TraceRouter (Host, Routers);
  if Count > 0 then
  begin
    Result := 'Host=' + Host;
    Result := Result + ' Count=' + IntToStr(Count);
    Result := Result + ' Routers=' + Routers;
  end;
end;

var
  CacheStr: String;

function FormatTraceRouter2(Host: PChar): PChar; Stdcall;
begin
  Result := nil;
  CacheStr := FormatTraceRouter (Host);
  if CacheStr <> '' then
    Result := PChar(CacheStr);
end;

end.
