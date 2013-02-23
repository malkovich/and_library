const
  // special flags
  ALL_SESSIONS     = $FFFFFFED;  // apps of all sessions
  CURRENT_SESSION  = $FFFFFFEC;  // apps of current session
  CURRENT_USER     = $FFFFFFEB;  // apps of current user

  // the following flags may only be used in combination with the first 3 flags
  SYSTEM_PROCESSES = $10;  // include this flag to include system processes + services
  CURRENT_PROCESS  = $8;   // exclude this flag to exclude injection into yourself

type
   PRegInfo = ^TRegInfo;
   TRegInfo = packed record
     EDI,
     ESI,
     EBP,
     ESP,
     EBX,
     EDX,
     ECX,
     EAX:DWORD;
   end;
   
   TCallBackFunc        = function(pReg:pointer):DWORD;stdcall;
   TCheckCanBeHook      = function (HookAddr: Pointer):LongBool;stdcall;

  // this is how you get notified about incoming ipc messages
  // you have to write a function which fits to this type definition
  // and then you give it into "CreateIpcQueue"
  TIpcCallback = procedure (name       : pchar;
                            messageBuf : pointer;
                            messageLen : dword;
                            answerBuf  : pointer;
                            answerLen  : dword); stdcall;

  // this is how your remote function must look like
  TRemoteExecuteFunction = function (params: pointer) : dword; stdcall;

  PCodeInfomation = ^TCodeInfomation;
  TCodeInfomation = packed record
    IsValid     : ByteBool;   // was the specified code pointer valid?
    Opcode      : word;      // Opcode, one byte ($00xx) or two byte ($0fxx)
    ModRm       : byte;      // ModRm byte, if available, otherwise 0
    Call        : LongBool;   // is this instruction a call?
    Jmp         : LongBool;   // is this instruction a jmp?
    RelTarget   : LongBool;   // is this target relative (or absolute)?
    Target      : pointer;   // absolute target address
    PTarget     : pointer;   // pointer to the target information in the code
    PPTarget    : PPointer; // pointer to pointer to the target information
    TargetSize  : integer;   // size of the target information in bytes (1/2/4)
    Enlargeable : LongBool;   // can the target size of this opcode be extended?
    This        : pointer;   // where does this instruction begin?
    Next        : pointer;   // next code location
  end;

  PFunctionInfomation = ^TFunctionInfomation;
  TFunctionInfomation = packed record
    IsValid        : LongBool;
    EntryPoint     : pointer;
    CodeBegin      : pointer;
    CodeLen        : integer;
  end;