unit UdpServer;

interface
uses
  Messages, Windows, SysUtils, Classes, WSocket, WinSock, LineServerUnit;

Type
  TUdpServer = class(TWSocket)
  public
    constructor CreateSrv(Addr, Port: String);
    procedure UDPDataAvailable(Sender: TObject; ErrCode: Word);
    function CommandInterpreter (Src: TSockAddrIn; FCommand: PChar; FCmdLen: Integer): string;
    function CmdHandler(Src: TSockAddrIn; CommandVerb ,CommandTail : String): String; virtual;
  end;



implementation

function TUdpServer.CmdHandler (Src: TSockAddrIn; CommandVerb ,CommandTail : String): String;
begin   
end;    

constructor TUdpServer.CreateSrv(Addr, Port: String);
begin
  inherited Create(NIL);      
  self.Proto   := 'udp';
  self.Addr    := Addr;
  self.Port    := Port;
  self.OnDataAvailable := UDPDataAvailable;
  self.Listen;
end;

procedure TUdpServer.UDPDataAvailable(Sender: TObject; ErrCode: Word);
var
    Buffer : array [0..1023] of char;
    Len    : Integer;
    Src    : TSockAddrIn;
    SrcLen : Integer;
    WSocket: TWSocket absolute Sender;
    RetStr: String;
begin                      
    SrcLen := SizeOf(Src);
    Len    := WSocket.ReceiveFrom(@Buffer[0], 1023, Src, SrcLen);
    Buffer[Len] := #0;
                               
    if Len > 0 then
    begin
      RetStr := CommandInterpreter (Src, Buffer, Len);
      WSocket.SendTo(Src, SrcLen, Pchar(RetStr), Length(RetStr));
    end;
end;

function TUdpServer.CommandInterpreter (Src: TSockAddrIn; FCommand: PChar; FCmdLen: Integer): String;
var
    CommandVerb : String;
    CommandTail : String;
    I, J        : Integer;
begin
    { Skip leading spaces }
    I := 0;
    while (I < FCmdLen) and (FCommand[I] in [' ', #9]) do
        Inc(I);

    { Find separator and separe CommandVerb and CommandTail }
    J := I;
    while TRUE do begin
        if (J >= FCmdLen) then begin
            SetLength(CommandVerb, FCmdLen - I);
            Move(FCommand[I], CommandVerb[1], Length(CommandVerb));
            CommandTail := '';
            break;
        end;

        if FCommand[J] in [' ', #9] then begin
            SetLength(CommandVerb, J - I);
            Move(FCommand[I], CommandVerb[1], Length(CommandVerb));
            SetLength(CommandTail, FCmdLen - J);
            Move(FCommand[J], CommandTail[1], Length(CommandTail));
            break;
        end;
        Inc(J);
    end;

    Result := CmdHandler (Src, CommandVerb, CommandTail);
end;

end.
