unit MyRSA;

interface
uses
  FGIntRSA,SysUtils,Windows,FGInt;

Procedure RSAEncryptBuf(const P_buf; P_len : dword; 
                        Var E_buf; var E_len : dword;
                        Var exp, modb : TFGInt); overload;

Procedure RSAEncryptBuf(const P_buf; P_len : dword;
                        Var E_buf; var E_len : dword;
                         Var expHexString, modbHexString : string); overload;

Procedure RSADecryptBuf(const E_buf; E_len : dword;
                        Var D_buf; var D_len : dword;
                        Var exp, modb, d_p, d_q, p, q : TFGInt); overload;

Procedure RSADecryptBuf(const E_buf; E_len : dword;
                        Var D_buf; var D_len : dword;
                        Var expHexString, modbHexString :string); overload;

implementation

Procedure RSAEncryptBuf(const P_buf; P_len : dword; 
                        Var E_buf; var E_len : dword;
                        Var exp, modb : TFGInt);
var
  inputStr,outputStr:string;
begin
  setlength(inputStr, P_len);
  Move(P_buf, inputStr[1], P_len);
  RSAEncrypt(inputStr, exp, modb, outputStr);
  Move(outputStr[1], E_buf, length(outputStr));
  E_len:= length(outputStr);
end;

Procedure RSAEncryptBuf(const P_buf; P_len : dword;
                        Var E_buf; var E_len : dword;
                         Var expHexString, modbHexString : string);
var
  str256:string;
  e,n:tfgint;
begin
    ConvertHexStringToBase256String(expHexString, str256);
    Base256StringToFGInt(str256, e);

    ConvertHexStringToBase256String(modbHexString, str256);
    Base256StringToFGInt(str256, n);

    RSAEncryptBuf(P_buf, P_len,E_buf, E_len ,e, n);

    FGIntDestroy(e);
    FGIntDestroy(n);  
end;

Procedure RSADecryptBuf(const E_buf; E_len : dword;
                        Var D_buf; var D_len : dword;
                        Var exp, modb, d_p, d_q, p, q : TFGInt);
var
  inputStr,outputStr:string;
begin
  setlength(inputStr, E_len);
  Move(E_buf, inputStr[1], E_len);
  RSADecrypt(inputStr, exp, modb, d_p, d_q, p, q, outputStr);
  Move(outputStr[1], D_buf, length(outputStr));
  D_len:= length(outputStr);
end;

Procedure RSADecryptBuf(const E_buf; E_len : dword;
                        Var D_buf; var D_len : dword;
                        Var expHexString, modbHexString :string);  
var
  str256:string;
  d,n,nilgint:tfgint;
begin
    ConvertHexStringToBase256String(expHexString, str256);
    Base256StringToFGInt(str256, d);

    ConvertHexStringToBase256String(modbHexString, str256);
    Base256StringToFGInt(str256, n);

    RSADecryptBuf(E_buf, E_len, D_buf, D_len,d, n, Nilgint, Nilgint, Nilgint, Nilgint);

    FGIntDestroy(d);
    FGIntDestroy(n);
    FGIntDestroy(nilgint);
end;


end.
