unit mailtest;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFMailForm = class(TForm)
    BodyEdit: TMemo;
    MailToBtn: TButton;
    MapiBtn: TButton;
    SubjectEdit: TEdit;
    MailFromEdit: TEdit;
    Label1: TLabel;
    MailToEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ScreenShotCheck: TCheckBox;
    Label5: TLabel;
    Bevel31: TBevel;
    Shape16: TShape;
    Bevel32: TBevel;
    Label22: TLabel;
    Label23: TLabel;
    AccountLabel: TLabel;
    PasswordLabel: TLabel;
    Label26: TLabel;
    SmtpServerEdit: TEdit;
    UseAuthCheck: TCheckBox;
    SmtpAccountEdit: TEdit;
    SmtpPasswordEdit: TEdit;
    SmtpPortEdit: TEdit;
    Bevel1: TBevel;
    Shape1: TShape;
    Bevel2: TBevel;
    Label6: TLabel;
    SmtpClientBtn: TButton;
    SmtpServerBtn: TButton;
    procedure MailToBtnClick(Sender: TObject);
    procedure MapiBtnClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormCreate(Sender: TObject);
    procedure SmtpClientBtnClick(Sender: TObject);
    procedure SmtpServerBtnClick(Sender: TObject);
    procedure UseAuthCheckClick(Sender: TObject);
    procedure CheckSmtpClientButton(Sender: TObject);
  private
    { Private-Deklarationen }
    function ScreenShotFile : string;
    function GetSmtpClientData (var user, psw: string; var port: dword) : boolean;
  public
    { Public-Deklarationen }
  end;

var FMailForm : TFMailForm;

implementation

{$R *.dfm}

uses madExcept, madTools, madTypes, madCrypt, madStrings, madNVBitmap, WinSock;

// ***************************************************************************

var WaitAlert : TForm;
procedure OpenWaitAlert;
var label1 : TLabel;
begin
  WaitAlert := TForm.CreateNew(nil);
  with WaitAlert do begin
    Caption := 'am mailing...';
    label1 := TLabel.Create(WaitAlert);
    label1.Parent := WaitAlert;
    label1.Left := 20;
    label1.Top := 20;
    label1.Caption := 'please wait a moment...';
    label1.AutoSize := true;
    ClientWidth := label1.Width + 40;
    ClientHeight := label1.Height + 40;
    Position := poScreenCenter;
    BorderStyle := bsToolWindow;
    BorderIcons := [];
    Application.MainForm.Enabled := false;
    Screen.Cursor := crHourglass;
    Show;
    Update;
  end;
end;

procedure CloseWaitAlert;
begin
  with WaitAlert do begin
    Application.MainForm.Enabled := true;
    Release;
    Screen.Cursor := crDefault;
  end;
end;

// ***************************************************************************

const CEncodeMailto : TSChar = [#$D, #$A, ' ', '#', '<', '>', '"', '%', '?', '+'];
      CEncodeSmtp   : TSChar = [#0..#255] - ['a'..'z', 'A'..'Z', '0'..'9'];
function EncodeStr(str: string; encodeChars: TSChar; encodeChar: char) : string;

  function FourBitToHex(fourBit: integer) : char;
  begin
    if fourBit < $a then
         result := chr(ord('0') - $0 + fourBit)
    else result := chr(ord('A') - $a + fourBit);
  end;

var i1, i2 : integer;
begin
  i2 := Length(str);
  for i1 := length(str) downto 1 do
    if str[i1] in encodeChars then
      inc(i2, 2);
  SetLength(result, i2);
  i2 := 1;
  for i1 := 1 to Length(str) do
    if str[i1] in encodeChars then begin
      result[i2    ] := encodeChar;
      result[i2 + 1] := FourBitToHex(ord(str[i1]) shr  4);
      result[i2 + 2] := FourBitToHex(ord(str[i1]) and $F);
      inc(i2, 3);
    end else begin
      result[i2] := str[i1];
      inc(i2);
    end;
end;

function MailAddrCount(var mail: string) : integer;
begin
  ReplaceStr(mail, ';', ',');
  FormatSubStrs(mail, ',');
  result := SubStrCount(mail, ',');
end;

function FormatMailAddr(mail: string; allowFullName: boolean) : string;
var i1 : integer;
begin
  i1 := PosStr('<', mail);
  if i1 = 0 then
    result := '<' + mail + '>'
  else
    if allowFullName then
      result := mail
    else
      result := Copy(mail, i1, maxInt);
end;

// ***************************************************************************

function AmOnline : boolean;
var igcs    : function (var flags: dword; reserved: dword = 0) : bool; stdcall;
    dll, c1 : dword;
begin
  dll := LoadLibrary('wininet.dll');
  igcs := GetProcAddress(dll, 'InternetGetConnectedState');
  result := (@igcs <> nil) and igcs(c1);
  FreeLibrary(dll);
end;

function SendSmtpMail(var log          : string;
                      mailFrom, rcptTo : string;
                      subject,  body   : string;
                      attachFiles      : string = '';
                      server           : string = '';
                      authUserName     : string = '';
                      authPassword     : string = '';
                      port             : dword  = 25) : boolean;
const CBoundary = 'www.madshi.net SMTP Boundary';

  function CompName : string;
  var c1    : dword;
      arrCh : array [0..MAX_PATH] of char;
  begin
    c1 := MAX_PATH;
    GetComputerName(arrCh, c1);
    result := arrCh;
  end;

  function SendStr(sock: TSocket; sendBuf: string) : boolean;
  var i1, i2 : integer;
  begin
    result := false;
    i1 := 0;
    while i1 < length(sendBuf) do begin
      i2 := length(sendBuf) - i1;
      if i2 > 1024 then
        i2 := 1024;
      if send(sock, sendBuf[i1 + 1], i2, 0) < 0 then
        exit;
      inc(i1, i2);
    end;
    result := true;
  end;

  function RecvStr(sock: TSocket; wantedReply: string; auth: TPBoolean = nil) : boolean;
  var buf    : array [0..1023] of char;
      i1     : integer;
      s1, s2 : string;
      fds    : TFDSet;
      tv     : TTimeVal;
      stop   : boolean;
  begin
    s1 := '';
    repeat
      stop := true;
      fds.fd_count := 1;
      fds.fd_array[0] := sock;
      tv.tv_sec := 7;
      tv.tv_usec := 0;
      if select(0, @fds, nil, nil, @tv) > 0 then begin
        i1 := recv(sock, buf, 1024, 0);
        SetString(s2, buf, i1);
        s1 := s1 + s2;
        result := Length(s1) > Length(wantedReply);
        if result then begin
          for i1 := 1 to Length(wantedReply) do
            if s1[i1] <> wantedReply[i1] then begin
              result := false;
              break;
            end;
          if result then begin
            for i1 := 1 to Length(s1) do
              if s1[i1] = '=' then
                s1[i1] := ' ';
            repeat
              stop := s1[4] <> '-';
              i1 := PosStr(#$D#$A, s1);
              if i1 = 0 then
                i1 := maxInt - 1;
              if (auth <> nil) and (not auth^) then begin
                s2 := Copy(s1, 5, i1 - 5);
                if PosTextIs1('AUTH ', s2) and (PosText(' LOGIN ', Copy(s2, 5, maxInt) + ' ') > 0) then
                  auth^ := true;
              end;
              Delete(s1, 1, i1 + 1);
            until stop or (Length(s1) < 4);
          end;
        end;
      end else
        result := false;
    until stop;
  end;

  function SendRecv(sock: TSocket; sendBuf, wantedReply: string; auth: TPBoolean = nil) : boolean;
  begin
    result := ( (sendBuf = '') or SendStr(sock, sendBuf + #$D#$A) ) and
              RecvStr(sock, wantedReply, auth);
  end;

  function SendFile(sock: TSocket; attachFile: string) : boolean;
  var fh, c1, c2, c3 : dword;
      arrCh          : array [0..56] of char;
      first          : boolean;
      fileType       : string;
  begin
    fh := CreateFile(pchar(attachFile), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
    result := fh <> INVALID_HANDLE_VALUE;
    if result then begin
      c1 := GetFileSize(fh, nil);
      first := true;
      while c1 > 0 do begin
        if c1 > 57 then c2 := 57
        else            c2 := c1;
        if (not ReadFile(fh, arrCh, c2, c3, nil)) or (c2 <> c3) then begin
          if not first then
            SendStr(sock, #$D#$A);
          result := false;
          break;
        end;
        if IsTextEqual(CopyR(attachFile, 4), '.png') then
             fileType := 'image/png'
        else fileType := 'application/octet-stream';
        if first then
          SendStr(sock, '--' + CBoundary + #$D#$A +
                        'Content-Type: ' + fileType + '; ' +
                        'name="' + ExtractFileName(attachFile) + '"' + #$D#$A +
                        'Content-Transfer-Encoding: base64' + #$D#$A +
                        'Content-Disposition: attachment; ' +
                        'filename="' + ExtractFileName(attachFile) + '"' + #$D#$A +
                        #$D#$A);
        SendStr(sock, Encode(arrCh, c2) + #$D#$A);
        dec(c1, c2);
        first := false;
      end;
      CloseHandle(fh);
    end;
  end;

  function GetDateTimeStr : string;
  const CMonths : array [1..12] of string =
          ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  var st : TSystemTime;
  begin
    GetSystemTime(st);
    with st do
      result := IntToStrEx(dword(wDay)) + ' ' + CMonths[wMonth] + ' ' +
                IntToStrEx(dword(wYear)) + ' ' + IntToStrEx(dword(wHour), 2) + ':' +
                IntToStrEx(dword(wMinute), 2) + ':' + IntToStrEx(dword(wSecond), 2) + ' UT';
  end;

  function MailIt(mailFrom, rcptTo, subject,  body, attachFiles, server: string; var log: string) : boolean;

    function MxLookup(email: string) : string;

      function RegReadStr(key: dword; path: string; name: string = '') : string;
      var key2   : HKEY;
          c1, c2 : dword;
          p1     : pchar;
      begin
        result := '';
        if RegOpenKeyEx(key, pchar(path), 0, KEY_READ, key2) = 0 then begin
          c1 := 0;
          RegQueryValueEx(key2, pchar(name), nil, nil, nil, @c1);
          if c1 <> 0 then begin
            c1 := c1 * 2;
            p1 := pointer(LocalAlloc(LPTR, c1));
            if RegQueryValueEx(key2, pchar(name), nil, @c2, pointer(p1), @c1) = 0 then
              if c2 = REG_SZ then
                   result := p1
              else SetString(result, p1, c1);
            LocalFree(dword(p1));
          end;
          RegCloseKey(key2);
        end;
      end;

      function GetDnsIps : TDAString;
      type
        TDns = packed record
          next   : pointer;
          ipAddr : array [0..15] of char;
          ipMask : array [0..15] of char;
          ctxt   : dword;
        end;
        TFixedInfo = packed record
          hostName      : array [1..132] of char;
          domainName    : array [1..132] of char;
          currentDns    : ^TDns;
          dnsList       : TDns;
          nodeType      : dword;
          scopeId       : array [1..260] of char;
          enableRouting : dword;
          enableProxy   : dword;
          enableDns     : dword;
        end;
      var dll : dword;
          gnp : function (buf: pointer; var len: integer) : dword; stdcall;
          len : integer;
          buf : ^TFixedInfo;
          s1  : string;
          ch1 : char;
          i1  : integer;
          dns : ^TDns;
      begin
        result := nil;
        if OS.Enum in [osWin95, osWin95osr2, osWinNTOld, osWinNT4] then begin
          if GetVersion and $80000000 = 0 then
               s1 := 'Tcpip\Parameters'
          else s1 := 'VxD\MSTCP';
          s1 := 'System\CurrentControlSet\Services\' + s1;
          s1 := RegReadStr(HKEY_LOCAL_MACHINE, s1, 'DhcpNameServer');
          if s1 = '' then
            s1 := RegReadStr(HKEY_LOCAL_MACHINE, s1, 'NameServer');
          if s1 <> '' then begin
            if PosStr(',', s1) > 0 then
                 ch1 := ','
            else ch1 := ' ';
            SetLength(result, SubStrCount(s1, ch1));
            for i1 := 0 to high(result) do
              result[i1] := SubStr(s1, i1 + 1, ch1);
          end;
        end else begin
          dll := LoadLibrary('IpHlpApi.dll');
          if dll <> 0 then begin
            gnp := GetProcAddress(dll, 'GetNetworkParams');
            if @gnp <> nil then begin
              len := 0;
              gnp(nil, len);
              if len <> 0 then begin
                len := len shl 1;
                GetMem(buf, len);
                if gnp(buf, len) = 0 then begin
                  if buf^.currentDns <> nil then begin
                    SetLength(result, 1);
                    result[0] := buf^.currentDns^.ipAddr;
                  end;
                  dns := @buf^.dnsList;
                  repeat
                    s1 := dns.ipAddr;
                    if (s1 <> '') and ((result = nil) or (s1 <> result[0])) then begin
                      SetLength(result, Length(result) + 1);
                      result[high(result)] := s1;
                    end;
                    dns := dns^.Next;
                  until dns = nil;
                end;
                FreeMem(buf);
              end;
            end;
            FreeLibrary(dll);
          end;
        end;
      end;

      procedure nslookup(email: string; const querys: array of string; type_: byte; var as1, as2: TDAString);
      var buf : array [0..511] of byte;

        function GetLabel(var pos: integer; len: integer) : string;
        var i1 : integer;
        begin
          result := '';
          while pos < len do
            if buf[pos] and $c0 = $c0 then begin
              i1 := (buf[pos + 1] + buf[pos] shl 8) and $3fff;
              result := result + '.' + GetLabel(i1, len);
              inc(pos, 2);
              break;
            end else
              if buf[pos] > 0 then begin
                result := result + '.' + PShortString(@buf[pos])^;
                inc(pos, buf[pos] + 1);
              end else begin
                inc(pos);
                break;
              end;
          Delete(result, 1, 1);
        end;

        function GetServers(len: integer; var pos: integer) : TDAString;
        var i1, i2 : integer;
        begin
          SetLength(result, len);
          for i1 := 0 to high(result) do begin
            GetLabel(pos, maxInt);
            if type_ = 15 then
                 i2 := pos + 12
            else i2 := pos + 10;
            inc(pos, buf[pos + 9] + buf[pos + 8] shl 8 + 10);
            result[i1] := GetLabel(i2, pos);
          end;
        end;

        function StrToSAddr(str: string) : integer;
        var phe : PHostEnt;
        begin
          result := integer(inet_addr(pchar(str)));
          if result = integer(INADDR_NONE) then begin
            phe := gethostbyname(pchar(str));
            if phe <> nil then
              result := TPInteger(phe^.h_addr_list^)^;
          end;
        end;

      var sock   : TSocket;
          sin    : TSockAddrIn;
          addr   : integer;
          pos    : integer;
          i1, i2 : integer;
          s1     : string;
          count  : integer;
          fds    : TFDSet;
          tv     : TTimeVal;
      begin
        as1 := nil;
        as2 := nil;
        sock := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
        if sock <> INVALID_SOCKET then begin
          sin.sin_family      := AF_INET;
          sin.sin_addr.s_addr := INADDR_ANY;
          sin.sin_port        := 0;
          if bind(sock, sin, sizeOf(sin)) <> SOCKET_ERROR then begin
            ZeroMemory(@buf, sizeOf(buf));
            buf[1] := 1;
            buf[2] := 1;  // recursive lookup allowed
            buf[5] := 1;
            s1 := '.' + email;
            i1 := 1;
            while true do begin
              i2 := PosStr('.', s1, i1 + 1);
              if (i2 > 0) and (i2 < Length(s1)) then begin
                byte(s1[i1]) := i2 - i1 - 1;
                i1 := i2;
              end else begin
                byte(s1[i1]) := Length(s1) - i1;
                break;
              end;
            end;
            Move(s1[1], buf[12], Length(s1));
            pos := 12 + Length(s1);
            buf[pos + 2] := type_;
            buf[pos + 4] := 1;
            inc(pos, 5);
            count := 0;
            for i1 := 0 to high(querys) do
              if querys[i1] <> '' then begin
                addr := StrToSAddr(querys[i1]);
                if addr <> integer(INADDR_NONE) then begin
                  sin.sin_family      := AF_INET;
                  sin.sin_port        := htons(53);
                  sin.sin_addr.s_addr := addr;
                  if sendto(sock, buf, pos, 0, sin, sizeOf(sin)) <> SOCKET_ERROR then
                    inc(count);
                end;
              end;
            fds.fd_count := 1;
            fds.fd_array[0] := sock;
            tv.tv_sec := 5;
            tv.tv_usec := 0;
            while (count > 0) and (select(0, @fds, nil, nil, @tv) > 0) and
                  (recvfrom(sock, buf, 512, 0, TSockAddrIn(nil^), integer(nil^)) <> SOCKET_ERROR) do begin
              if buf[4] and $0f = 0 then begin
                as1 := GetServers(TPWord(@buf[7])^, pos);
                if type_ = 2 then
                  as2 := GetServers(TPWord(@buf[9])^, pos);
                if (as1 <> nil) or (as2 <> nil) then
                  break;
              end;
              dec(count);
            end;
          end;
          closesocket(sock);
        end;
      end;

    var wd            : TWsaData;
        as1, as2, as3 : TDAString;
        subEmail      : string;
        i1, i2        : integer;
        dnsIps        : TDAString;
    { $define oneServerOnly}
    begin
      result := '';
      dnsIps := nil;
      Delete(email, 1, PosStr('@', email));
      if email[Length(email)] = '>' then
        Delete(email, Length(email), 1);
      subEmail := email;
      i2 := 0;
      for i1 := Length(subEmail) downto 1 do
        if subEmail[i1] = '.' then
          if i2 = 2 then begin
            Delete(subEmail, 1, i1);
            break;
          end else
            inc(i2);
      if WSAStartup($101, wd) = 0 then begin
        dnsIps := GetDnsIps;
        if dnsIps <> nil then begin
          log := log + 'OS reported dns ips: ';
          for i1 := 0 to high(dnsIps) do
            log := log + dnsIps[i1] + ', ';
          DeleteR(log, 2);
          log := log + #$D#$A;
          {$ifdef oneServerOnly}
            nslookup(email, [dnsIps[0]], 15, as2, as3);  // mx
          {$else}
            nslookup(email, dnsIps, 15, as2, as3);  // mx
          {$endif}
          if as2 <> nil then
            result := as2[0];
          if (result = '') and (email <> subEmail) then begin
            {$ifdef oneServerOnly}
              nslookup(subEmail, [dnsIps[0]], 15, as2, as3);  // mx
            {$else}
              nslookup(subEmail, dnsIps, 15, as2, as3);  // mx
            {$endif}
            if as2 <> nil then
              result := as2[0];
          end;
          if result <> '' then
               log := log + 'mx lookup succeeded (' + result + ')' + #$D#$A
          else log := log + 'mx lookup failed' + #$D#$A;
        end else begin
          nslookup(subEmail, ['A.ROOT-SERVERS.NET', 'K.ROOT-SERVERS.NET'], 2, as1, as2);  // nameserver
          if as2 <> nil then begin
            {$ifdef oneServerOnly}
              nslookup(subEmail, [as2[0]], 2, as1, as3);  // nameserver
            {$else}
              nslookup(subEmail, as2, 2, as1, as3);  // nameserver
            {$endif}
            if as1 = nil then
              case Length(as3) of
                0 :  log := log + '2nd level lookup failed' + #$D#$A;
                1 :  log := log + '2nd level lookup succeeded (' + as3[0] + ')' + #$D#$A;
                else log := log + '2nd level lookup succeeded (' + as3[0] + ', ...)' + #$D#$A;
              end;
            for i1 := 1 to 10 do
              if (as1 = nil) and (as3 <> nil) then begin
                {$ifdef oneServerOnly}
                  nslookup(subEmail, [as3[0]], 2, as1, as2); // nameserver
                {$else}
                  nslookup(subEmail, as3, 2, as1, as2); // nameserver
                {$endif}
                if as1 = nil then
                  case Length(as3) of
                    0 :  log := log + '3rd level run ' + IntToStrEx(i1) + ' lookup failed' + #$D#$A;
                    1 :  log := log + '3rd level run ' + IntToStrEx(i1) + ' lookup succeeded (' + as3[0] + ')' + #$D#$A;
                    else log := log + '3rd level run ' + IntToStrEx(i1) + ' lookup succeeded (' + as3[0] + ', ...)' + #$D#$A;
                  end;
                as3 := as2;
              end else
                break;
            if (as1 = nil) and (as2 <> nil) then
              as1 := as2;
            if as1 <> nil then
              case Length(as1) of
                0 :  log := log + 'no final name server found' + #$D#$A;
                1 :  log := log + 'final name server found (' + as1[0] + ')' + #$D#$A;
                else log := log + 'final name servers found (' + as1[0] + ', ...)' + #$D#$A;
              end;
            {$ifdef oneServerOnly}
              for i1 := high(as1) downto 0 do begin
                nslookup(email, [as1[i1]], 15, as2, as3);  // mx
                if as2 <> nil then begin
                  result := as2[0];
                  break;
                end;
              end;
              if (result = '') and (email <> subEmail) then
                for i1 := 0 to high(as1) do begin
                  nslookup(subEmail, [as1[i1]], 15, as2, as3);  // mx
                  if as2 <> nil then begin
                    result := as2[0];
                    break;
                  end;
                end;
            {$else}
              nslookup(email, as1, 15, as2, as3);  // mx
              if as2 <> nil then
                result := as2[0];
              if (result = '') and (email <> subEmail) then begin
                nslookup(subEmail, as1, 15, as2, as3);  // mx
                if as2 <> nil then
                  result := as2[0];
              end;
            {$endif}
            if result <> '' then
                 log := log + 'mx lookup succeeded (' + result + ')' + #$D#$A
            else log := log + 'mx lookup failed' + #$D#$A;
          end else log := log + 'root server lookup failed' + #$D#$A;
        end;
        WSACleanup;
      end else log := log + 'dns lookup: socket open failed' + #$D#$A;
    end;

  var sock : TSocket;
      addr : integer;
      sin  : TSockAddrIn;
      phe  : PHostEnt;
      auth : boolean;
      i1   : integer;
      s1   : string;
      b1   : boolean;
  begin
    result := false;
    sock := socket(AF_INET, SOCK_STREAM, 0);
    if sock <> INVALID_SOCKET then begin
      if server = '' then
        server := MxLookup(rcptTo);
      if server <> '' then begin
        addr := integer(inet_addr(pchar(server)));
        if addr = integer(INADDR_NONE) then begin
          phe := gethostbyname(pchar(server));
          if phe <> nil then
            addr := TPInteger(phe^.h_addr_list^)^;
        end;
        if addr <> integer(INADDR_NONE) then begin
          log := log + 'server name resolution succeeded' + #$D#$A;
          sin.sin_family      := AF_INET;
          sin.sin_port        := htons(port);
          sin.sin_addr.s_addr := addr;
          auth := false;
          if (connect(sock, sin, sizeOf(sin)) <> SOCKET_ERROR) and
             RecvStr(sock, '220') then begin
            log := log + 'server connect succeeded' + #$D#$A;
            if SendRecv(sock, 'EHLO ' + CompName, '250', @auth) or
               SendRecv(sock, 'HELO ' + CompName, '250'       ) then begin
              log := log + 'HELO succeeded' + #$D#$A;
              if mailFrom = '' then
                mailFrom := rcptTo;
              if ( ( (authUserName = '') and
                     (authPassword = '')     ) or
                   ( auth and
                     SendRecv(sock, 'AUTH LOGIN',           '334') and
                     SendRecv(sock, Encode(authUserName), '334') and
                     SendRecv(sock, Encode(authPassword), '235')     ) ) then begin
                if SendRecv(sock, 'MAIL FROM:' + FormatMailAddr(mailFrom, false), '250') then begin
                  b1 := false;
                  for i1 := 1 to MailAddrCount(rcptTo) do
                    if SendRecv(sock, 'RCPT TO:' + FormatMailAddr(SubStr(rcptTo, i1, ','), false), '250') then begin
                      log := log + 'receiver "' + FormatMailAddr(SubStr(rcptTo, i1, ','), false) + '" accepted' + #$D#$A;
                      b1 := true;
                    end else
                      log := log + 'receiver "' + FormatMailAddr(SubStr(rcptTo, i1, ','), false) + '" rejected' + #$D#$A;
                  if b1 then
                    if (subject <> '') or (body <> '') then begin
                      ReplaceStr(body, #$D#$A + '.' + #$D#$A, #$D#$A + '. ' + #$D#$A);
                      subject := '=?ISO-8859-1?Q?' + EncodeStr(subject, CEncodeSmtp, '=') + '?=';
                      if SendRecv(sock, 'DATA', '354') then begin
                        log := log + 'server is ready to receive data' + #$D#$A;
                        if SendStr(sock, 'DATE: '    + GetDateTimeStr + #$D#$A +
                                         'TO: '      + FormatMailAddr(rcptTo,   true) + #$D#$A +
                                         'FROM: '    + FormatMailAddr(mailFrom, true) + #$D#$A +
                                         'SUBJECT: ' + subject + #$D#$A) then begin
                          log := log + 'mail header sent successfully' + #$D#$A;
                          FormatSubStrs(attachFiles);
                          if attachFiles <> '' then begin
                            b1 := SendStr(sock, 'MIME-Version: 1.0' + #$D#$A +
                                                'Content-Type: multipart/mixed; boundary="' + CBoundary + '"' + #$D#$A +
                                                #$D#$A +
                                                'This is a multi-part message in MIME format.' + #$D#$A +
                                                #$D#$A +
                                                '--' + CBoundary + #$D#$A +
                                                'Content-Type: text/plain; charset="iso-8859-1"' + #$D#$A +
                                                'Content-Transfer-Encoding: 8bit' + #$D#$A +
                                                #$D#$A +
                                                body + #$D#$A);
                            if b1 then begin
                              log := log + 'multipart body sent successfully' + #$D#$A;
                              for i1 := 1 to SubStrCount(attachFiles) do begin
                                s1 := SubStr(attachFiles, i1);
                                if (GetFileAttributes(pchar(s1)) <> dword(-1)) and (not SendFile(sock, s1)) then begin
                                  b1 := false;
                                  break;
                                end;
                              end;
                            end;
                            if b1 then
                              if SendStr(sock, '--' + CBoundary + '--' + #$D#$A) then
                                   log := log + 'attachment sent successfully' + #$D#$A
                              else log := log + 'attachment sending failed' + #$D#$A;
                            result := true;
                          end else begin
                            result := SendStr(sock, #$D#$A + body);
                            if result then
                                 log := log + 'body sent successfully' + #$D#$A
                            else log := log + 'body sending failed' + #$D#$A;
                          end;
                        end else log := log + 'mail header sending failed' + #$D#$A;
                        b1 := SendRecv(sock, #$D#$A + '.', '250');
                        result := result and b1;
                        if b1 then
                             log := log + 'mail finalization sent successfully' + #$D#$A
                        else log := log + 'mail finalization sending failed' + #$D#$A;
                      end else log := log + 'data sending failed' + #$D#$A;
                    end else
                      SendRecv(sock, 'RSET', '250');
                end else log := log + 'mail from address rejected' + #$D#$A;
              end else log := log + 'authentification rejected' + #$D#$A;
              SendRecv(sock, 'QUIT', '221');
            end else log := log + 'HELO failed' + #$D#$A;
          end else log := log + 'server connect failed' + #$D#$A;
        end else log := log + 'server name resolution failed' + #$D#$A;
      end;
      closesocket(sock);
    end else log := log + 'socket open failed' + #$D#$A;
  end;

var wd : WSAData;
    i1 : integer;
begin
  result := false;
  if AmOnline then begin
    log := log + 'wininet claims we''re online' + #$D#$A;
    if (rcptTo <> '') and (WSAStartup($101, wd) = 0) then begin
      if server = '' then begin
        for i1 := 1 to MailAddrCount(rcptTo) do
          if MailIt(mailFrom, SubStr(rcptTo, i1, ','), subject, body, attachFiles, server, log) then
            result := true;
      end else
        if MailIt(mailFrom, rcptTo, subject, body, attachFiles, server, log) then
          result := true;
      WSACleanup;
    end else log := 'socket initialization failed' + #$D#$A;
  end else log := 'wininet claims we''re offline' + #$D#$A;
end;

function SendShellMail(var log: string; rcptTo, subject, body: string) : boolean;
// send a mail via CreateProcess('mailto:')
const CMaxCmdLine = 32 * 1024 - 2;
var s1, s2 : string;
    key    : HKEY;
    len    : dword;
    arrCh  : array [0..MAX_PATH] of char;
    si     : TStartupInfo;
    pi     : TProcessInformation;
begin
  log := '';
  result := false;
  len := MAX_PATH;
  if RegOpenKeyEx(HKEY_CLASSES_ROOT, 'mailto\shell\open\command', 0, KEY_READ, key) = 0 then begin
    log := log + 'mailto: registry key opened successfully' + #$D#$A;
    if RegQueryValueEx(key, '', nil, nil, @arrCh, @len) = 0 then begin
      log := log + 'mailto registry value read successfully' + #$D#$A;
      arrCh[len] := #0;
      s1 := arrCh;
      ExpandEnvironmentStrings(pchar(s1), arrCh, MAX_PATH);
      s1 := arrCh;
      if s1 <> '' then begin
        log := log + 'mailto application is "' + s1 + '"' + #$D#$A;
        subject := EncodeStr(subject, CEncodeMailto, '%');
        body    := EncodeStr(body,    CEncodeMailto, '%');
        MailAddrCount(rcptTo);
        s2 := 'mailto:' + rcptTo + '?subject=' + subject + '&body=' + body;
        if not ReplaceStr(s1, '%1', s2) then
          if s1[Length(s1)] <> ' ' then
               s1 := s1 + ' ' + s2
          else s1 := s1       + s2;
        // the max supported parameter length for CreateProcess in win9x is
        // unlimited, however in NT based systems it is 32kb
        if (GetVersion and $80000000 = 0) and (length(s1) > CMaxCmdLine) then
          if      s1[CMaxCmdLine    ] = '%' then SetLength(s1, CMaxCmdLine - 1)
          else if s1[CMaxCmdLine - 1] = '%' then SetLength(s1, CMaxCmdLine - 2)
          else                                   SetLength(s1, CMaxCmdLine    );
        ZeroMemory(@si, sizeOf(si));
        si.cb := sizeOf(si);
        if CreateProcess(nil, pchar(s1), nil, nil, false, 0, nil, nil, si, pi) then begin
          if pi.hThread  <> 0 then CloseHandle(pi.hThread );
          if pi.hProcess <> 0 then CloseHandle(pi.hProcess);
          log := log + 'mailto application start succeeded' + #$D#$A;
          result := true;
        end else
          log := log + 'mailto application start failed' + #$D#$A;
      end else log := log + 'seemingly no mailto application is registered' + #$D#$A;
    end else log := log + 'mailto registry value reading failed' + #$D#$A;
    RegCloseKey(key);
  end else log := log + 'mailto registry key opening failed' + #$D#$A;
end;

function SendMapiMail(var log: string; rcptTo, subject, body: string; attachFiles: string = '';
                      parentWindow: dword = 0; hidden: boolean = false) : boolean;
// send a mail via Mapi

  procedure SplitEMailAddr(email: string; var name, address: string);
  var i1, i2 : integer;
  begin
    i1 := PosStr('<', email);
    if i1 > 0 then begin
      i2 := PosStr('>', email, i1 + 1);
      if i2 > 0 then
        Delete(email, i2, maxInt);
      name    := Copy(email, 1, i1 - 1);
      address := Copy(email, i1 + 1, maxInt);
    end else begin
      name    := email;
      address := email;
    end;
    address := 'SMTP:' + address;
  end;

type
  TMapiReceiver = packed record
    d1        : dword;
    class_    : dword;
    name      : pchar;
    address   : pchar;
    d2        : array [0..1] of dword;
  end;
  TMapiAttach = packed record
    d1        : dword;
    d2        : dword;
    position  : dword;  // must be -1
    pathName  : pchar;
    d3        : dword;
    d4        : dword;
  end;
  TMapiMail = packed record
    d1        : dword;
    subject   : pchar;
    body      : pchar;
    d2        : array [0..4] of dword;
    noOfRec   : dword;
    receiver  : ^TMapiReceiver;
    attachCnt : dword;
    attachs   : ^TMapiAttach;
  end;
var mail     : TMapiMail;
    rec1     : array of array [0..1] of string;
    rec2     : array of TMapiReceiver;
    att1     : array of string;
    att2     : array of TMapiAttach;
    flags    : dword;
    sendproc : function (session, uiParam: dword; var msg: TMapiMail; flags, reserved: dword) : dword; stdcall;
    i1       : integer;
begin
  log := '';
  result := true;
  sendproc := GetProcAddress(LoadLibrary('mapi32.dll'), 'MAPISendMail');
  if @sendproc <> nil then begin
    log := log + 'mapi dll opened successfully' + #$D#$A;
    ZeroMemory(@mail, sizeOf(mail));
    rec2 := nil;
    SetLength(rec1, MailAddrCount(rcptTo));
    SetLength(rec2, length(rec1));
    for i1 := 0 to high(rec1) do begin
      SplitEMailAddr(SubStr(rcptTo, i1 + 1, ','), rec1[i1][0], rec1[i1][1]);
      rec2[i1].class_  := 1;  // MAPI_TO = 1
      rec2[i1].name    := pchar(rec1[i1][0]);
      rec2[i1].address := pchar(rec1[i1][1]);
    end;
    mail.subject  := pchar(subject);
    mail.body     := pchar(body   );
    mail.noOfRec  := length (rec2);
    mail.receiver := pointer(rec2);
    att2 := nil;
    FormatSubStrs(attachFiles);
    SetLength(att1, SubStrCount(attachFiles));
    SetLength(att2, length(att1));
    for i1 := 0 to high(att1) do begin
      att1[i1]          := SubStr(attachFiles, i1 + 1);
      att2[i1].position := dword(-1);
      att2[i1].pathName := pchar(att1[i1]);
    end;
    mail.attachCnt := length (att2);
    mail.attachs   := pointer(att2);
    if hidden and (rcptTo <> '') then
         flags := 0
    else flags := $9;  // MAPI_DIALOG = 8; MAPI_LOGON_UI = 1;
    case sendProc(0, parentWindow, mail, flags, 0) of
      0  : begin
             result := true;
             log := log + 'mapi reports success' + #$D#$A;
           end;
      1  : log := log + 'mapi reports user abort' + #$D#$A;
      else log := log + 'mapi reports error' + #$D#$A;
    end;
  end else log := log + 'mapi dll opening failed' + #$D#$A;
end;

// ***************************************************************************

function TFMailForm.ScreenShotFile : string;
var s1  : string;
    fh  : dword;
    len : dword;
    b1  : boolean;
begin
  result := '';
  if ScreenShotCheck.Checked then begin
    s1 := Screenshot(false).AsPngStr(pf300kb);
    if s1 <> '' then begin
      result := ExtractFilePath(ParamStr(0)) + 'ScreenShot.png';
      fh := CreateFile(pchar(result), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
      b1 := WriteFile(fh, s1[1], Length(s1), len, nil);
      CloseHandle(fh);
      if not b1 then begin
        DeleteFile(result);
        result := '';
      end;
    end;
  end;
end;

function TFMailForm.GetSmtpClientData (var user, psw: string; var port: dword) : boolean;
begin
  port := StrToIntDef(SmtpPortEdit.Text, 0);
  result := (port <> 0) and (SmtpServerEdit.Text <> '');
  if UseAuthCheck.Checked then begin
    user := SmtpAccountEdit.Text;
    psw  := SmtpPasswordEdit.Text;
    result := user <> '';
  end else begin
    user := '';
    psw  := '';
  end;
end;

procedure TFMailForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then begin
    Key := #0;
    Close;
  end;
end;

procedure TFMailForm.FormCreate(Sender: TObject);
begin
  BodyEdit.Text := CreateBugReport;
end;

procedure TFMailForm.UseAuthCheckClick(Sender: TObject);
begin
  if UseAuthCheck.Checked then begin
    AccountLabel.Font.Color := clWindowText;
    PasswordLabel.Font.Color := clWindowText;
    SmtpAccountEdit.Color := clWindow;
    SmtpPasswordEdit.Color := clWindow;
    SmtpAccountEdit.Enabled := true;
    SmtpPasswordEdit.Enabled := true;
  end else begin
    AccountLabel.Font.Color := clBtnShadow;
    PasswordLabel.Font.Color := clBtnShadow;
    SmtpAccountEdit.Color := clBtnFace;
    SmtpPasswordEdit.Color := clBtnFace;
    SmtpAccountEdit.Enabled := false;
    SmtpPasswordEdit.Enabled := false;
  end;
  CheckSmtpClientButton(nil);
end;

procedure TFMailForm.CheckSmtpClientButton(Sender: TObject);
var user, psw : string;
    port      : dword;
begin
  SmtpClientBtn.Enabled := GetSmtpClientData(user, psw, port);
end;

// ***************************************************************************

procedure ShowMailResult(success: boolean; log: string);
begin
  if success then
    MessageBox(0, pchar('mail sending succeeded' + #$D#$A + #$D#$A + 'LOG:' + #$D#$A + log),
               'mail result...', MB_ICONINFORMATION)
  else
    MessageBox(0, pchar('mail sending failed'    + #$D#$A + #$D#$A + 'LOG:' + #$D#$A + log),
               'mail result...', MB_ICONERROR);
end;

procedure TFMailForm.MapiBtnClick(Sender: TObject);
var log : string;
    b1  : boolean;
begin
  OpenWaitAlert;
  b1 := SendMapiMail(log, MailToEdit.Text, SubjectEdit.Text, BodyEdit.Text, ScreenShotFile, Application.Handle);
  CloseWaitAlert;
  ShowMailResult(b1, log);
end;

procedure TFMailForm.MailToBtnClick(Sender: TObject);
var log : string;
    b1  : boolean;
begin
  OpenWaitAlert;
  b1 := SendShellMail(log, MailToEdit.Text, SubjectEdit.Text, BodyEdit.Text);
  CloseWaitAlert;
  ShowMailResult(b1, log);
end;

procedure TFMailForm.SmtpClientBtnClick(Sender: TObject);
var user, psw : string;
    port      : dword;
    log       : string;
    b1        : boolean;
begin
  OpenWaitAlert;
  if not GetSmtpClientData(user, psw, port) then begin
    b1 := false;
    log := 'invalid smpt client server data' + #$D#$A;
  end else
    b1 := SendSmtpMail(log, MailFromEdit.Text, MailToEdit.Text, SubjectEdit.Text, BodyEdit.Text,
                       ScreenShotFile, SmtpServerEdit.Text, user, psw, port);
  CloseWaitAlert;
  ShowMailResult(b1, log);
end;

procedure TFMailForm.SmtpServerBtnClick(Sender: TObject);
var log : string;
    b1  : boolean;
begin
  OpenWaitAlert;
  b1 := SendSmtpMail(log, MailFromEdit.Text, MailToEdit.Text, SubjectEdit.Text, BodyEdit.Text, ScreenShotFile);
  CloseWaitAlert;
  ShowMailResult(b1, log);
end;

// ***************************************************************************

end.
