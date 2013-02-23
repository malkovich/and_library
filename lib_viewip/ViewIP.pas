unit ViewIP;

interface           

function GetLocation(ip:pchar):string; stdcall;

implementation

function _GetAddress(ip:pchar):ppchar;cdecl;external 'ipsearcher.dll';

function GetLocation(ip:pchar):string; stdcall;
var  
  loc:string;   
  p:ppchar;   
begin
  p:= _GetAddress(ip);
  loc:=p^;
  inc(p);
  if assigned (p) then
    if assigned (p^) then
      loc:=loc + p^;
  result:=loc;
end;


end.
