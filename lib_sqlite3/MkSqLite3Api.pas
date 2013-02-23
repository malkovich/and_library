unit MkSqLite3Api;

interface

uses Windows, Classes, SysUtils;

const

  SQLITEDLL: PChar  = 'sqlite3.dll';

  SQLITE_OK         =  0;   // Successful result
  SQLITE_ERROR      =  1;   // SQL error or missing database
  SQLITE_INTERNAL   =  2;   // An internal logic error in SQLite
  SQLITE_PERM       =  3;   // Access permission denied
  SQLITE_ABORT      =  4;   // Callback routine requested an abort
  SQLITE_BUSY       =  5;   // The database file is locked
  SQLITE_LOCKED     =  6;   // A table in the database is locked
  SQLITE_NOMEM      =  7;   // A malloc() failed
  SQLITE_READONLY   =  8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT  =  9;   // Operation terminated by sqlite_interrupt()
  SQLITE_IOERR      = 10;   // Some kind of disk I/O error occurred
  SQLITE_CORRUPT    = 11;   // The database disk image is malformed
  SQLITE_NOTFOUND   = 12;   // (Internal Only) Table or record not found
  SQLITE_FULL       = 13;   // Insertion failed because database is full
  SQLITE_CANTOPEN   = 14;   // Unable to open the database file
  SQLITE_PROTOCOL   = 15;   // Database lock protocol error
  SQLITE_EMPTY      = 16;   // (Internal Only) Database table is empty
  SQLITE_SCHEMA     = 17;   // The database schema changed
  SQLITE_TOOBIG     = 18;   // Too much data for one row of a table
  SQLITE_CONSTRAINT = 19;   // Abort due to contraint violation
  SQLITE_MISMATCH   = 20;   // Data type mismatch
  SQLITE_MISUSE     = 21;   // Library used incorrectly
  SQLITE_NOLFS      = 22;   // Uses OS features not supported on host
  SQLITE_AUTH       = 23;   // Authorization denied
  SQLITE_FORMAT     = 24;   // Auxiliary database format error
  SQLITE_RANGE      = 25;   // 2nd parameter to sqlite_bind out of range
  SQLITE_NOTADB     = 26;   // File opened that is not a database file
  SQLITE_ROW        = 100;  // sqlite_step() has another row ready
  SQLITE_DONE       = 101;  //  sqlite_step() has finished executing

  SQLITE_INTEGER=1;
  SQLITE_FLOAT=2;
  SQLITE_TEXT=3;
  SQLITE_BLOB=4;
  SQLITE_NULL=5;

{
** These are the allowed values for the eTextRep argument to
** sqlite3_create_collation and sqlite3_create_function.
}
SQLITE_UTF8    =1;
SQLITE_UTF16LE =2;
SQLITE_UTF16BE =3;
SQLITE_UTF16   =4;    // Use native byte order
SQLITE_ANY=5;    // sqlite3_create_function only

SQLITE_STATIC=0;
SQLITE_TRANSIENT=-1;

type

ppchar=^pchar;
ppvalue=^pvalue;
pvalue=pointer;

TDestructor=procedure(data:pointer); cdecl;
TExecCallBack=function(user:pointer;
                       ncols:integer;
                       values:ppchar;
                       names:ppchar):integer; cdecl;

TBusyHandler=function(user:pointer; count:integer):integer; cdecl;
TFuncHandler=procedure(context:pointer; nArgs:integer; args:ppvalue); cdecl;
TFuncFinalizer=procedure(context:pointer); cdecl;
TUserCollation=function(user:pointer;
                        lenA:integer;
                        a:pchar;
                        lenB:integer;
                        b:pchar):integer; cdecl;
                        
TUserCollationNeeded=procedure(user:pointer;
                               db:pointer;
                               eTextRep:integer;
                               zName:pchar); cdecl;

var

sqlite3_libVersion: function(): PChar; cdecl;

sqlite3_close: function(db: Pointer):integer; cdecl;
sqlite3_exec: function(db: Pointer;
                       SQLStatement: PChar;
                       CallbackPtr: TExecCallBack;
                       CbParam: pointer;
                       ErrMsg: PPChar): integer; cdecl;
                       
sqlite3_last_insert_rowid: function(db: Pointer): int64; cdecl;
sqlite3_changes: function(db: Pointer): integer; cdecl;
sqlite3_total_changes: function(db: Pointer): integer; cdecl;
sqlite3_interrupt: procedure(db: Pointer); cdecl;
sqlite3_complete: function(P: PChar): integer; cdecl;
sqlite3_busy_handler: function(db: Pointer;
                               CallbackPtr:TBusyHandler;
                               user:pointer):integer; cdecl;
                               
sqlite3_busy_timeout: function(db: Pointer; TimeOut: integer):integer; cdecl;
sqlite3_free: procedure(P: PChar); cdecl;
sqlite3_open: function(dbname: PChar; var db:pointer):integer; cdecl;
sqlite3_errcode:function(db:pointer):integer; cdecl;
sqlite3_errmsg:function(db:pointer):pchar; cdecl;

sqlite3_prepare:function(db:pointer;
                         Sql:pchar;
                         nBytes:integer;
                         var stmt:pointer;
                         var pzTail:pchar):integer; cdecl;

sqlite3_bind_double:function(stmt:pointer; idx:integer; value:double):integer; cdecl;
sqlite3_bind_int:function(stmt:pointer; idx:integer; value:integer):integer; cdecl;
sqlite3_bind_int64:function(stmt:pointer; idx:integer; value:int64):integer; cdecl;
sqlite3_bind_null:function(stmt:pointer; idx:integer):integer; cdecl;
//sqlite3_bind_value:function(stmt:pointer; idx:integer; value:pointer):integer; cdecl;
sqlite3_bind_text:function(stmt:pointer;
                           idx:integer;
                           value:pchar;
                           size:integer;
                           xDel:Integer):integer; cdecl;
sqlite3_bind_blob:function(stmt:pointer;
                           idx:integer;
                           value:pointer;
                           size:integer;
                           xDel:integer):integer; cdecl;

sqlite3_bind_parameter_count:function(stmt:pointer):integer; cdecl;
sqlite3_bind_parameter_name:function(stmt:pointer; idx:integer):pchar; cdecl;

sqlite3_bind_parameter_index:function(stmt:pointer; zName:pchar):integer; cdecl;

sqlite3_column_count:function(pStmt:pointer):integer; cdecl;
sqlite3_column_name:function(pStmt:pointer; idx:integer):pchar; cdecl;
sqlite3_column_decltype:function(pStmt:pointer; idx:integer):pchar; cdecl;
sqlite3_step:function(pStmt:pointer):integer; cdecl;

sqlite3_data_count:function(pStmt:pointer):integer; cdecl;

sqlite3_column_blob:function(pStmt:pointer; col:integer):pointer; cdecl;
sqlite3_column_bytes:function(pStmt:pointer; col:integer):integer; cdecl;
sqlite3_column_double:function(pStmt:pointer; col:integer):double; cdecl;
sqlite3_column_int:function(pStmt:pointer; col:integer):integer; cdecl;
sqlite3_column_int64:function(pStmt:pointer; col:integer):int64; cdecl;
sqlite3_column_text:function(pStmt:pointer; col:integer):pchar; cdecl;
sqlite3_column_type:function(pStmt:pointer; col:integer):integer; cdecl;

sqlite3_finalize:function(pStmt:pointer):integer; cdecl;
sqlite3_reset:function(pStmt:pointer):integer; cdecl;

sqlite3_create_function:function(
  db:pointer;
  zFunctionName:pchar;
  nArg:integer;
  eTextRep:integer;
  userData:pointer;
  xFunc,
  xStep:TFuncHandler;
  xFinal:TFuncFinalizer):integer; cdecl;

sqlite3_aggregate_count:function(sqlite3_context:pointer):integer;  cdecl;

sqlite3_value_blob:function(v:pvalue):pointer; cdecl;
sqlite3_value_bytes:function(v:pvalue):integer; cdecl;
sqlite3_value_double:function(v:pvalue):double; cdecl;
sqlite3_value_int:function(v:pvalue):integer; cdecl;
sqlite3_value_int64:function(v:pvalue):int64; cdecl;
sqlite3_value_text:function(v:pvalue):pchar; cdecl;
sqlite3_value_type:function(v:pvalue):integer; cdecl;

sqlite3_aggregate_context:function(context:pointer; nBytes:integer):pointer; cdecl;

sqlite3_user_data:function(context:pointer):pointer; cdecl;

sqlite3_get_auxdata:function(context:pointer; idx:integer):pointer; cdecl;
sqlite3_set_auxdata:procedure(context:pointer; idx:integer;
                              data:pointer;
                              xDel:integer); cdecl;

sqlite3_result_blob:procedure(context:pointer; value:pointer; size:integer;
                              xDel:integer); cdecl;
sqlite3_result_double:procedure(context:pointer; value:double); cdecl;
sqlite3_result_error:procedure(context:pointer; msg:pchar; len:integer); cdecl;
sqlite3_result_int:procedure(context:pointer; value:integer); cdecl;
sqlite3_result_int64:procedure(context:pointer; value:int64); cdecl;
sqlite3_result_null:procedure(context:pointer); cdecl;
sqlite3_result_text:procedure(context:pointer; value:pchar; len:integer;
                              xDel:integer); cdecl;
sqlite3_result_value:procedure(context:pointer; value:pvalue); cdecl;

sqlite3_create_collation:function(db:pointer;
  zName:pchar;
  eTextRep:integer;
  userData:pointer;
  func:TUserCollation):integer; cdecl;

sqlite3_collation_needed:function(db:pointer;
  userData:pointer;
  func:TUserCollationNeeded):integer; cdecl;

procedure CheckSqliteLoaded;

Var
  DefineSqliteDLL: String;

implementation

var
 dllhandle:THandle=0;
 dllinit:Boolean=false;

function GetSelfModuleName: string;
var
  SignWord: PWORD;
  SelfHandle: THandle;
begin
  SignWord := @GetSelfModuleName;
  SignWord := Pointer (DWORD (SignWord) And $FFFFF000);

  while SignWord^ <> $5A4D do
    SignWord := Pointer (DWORD(SignWord) - $1000);

  SelfHandle := THandle (SignWord);
  Result := GetModuleName (SelfHandle);
end;

function GetSqlite3DLL: String;
begin
  Result := GetSelfModuleName;
  Result := ExtractFilePath (Result) + StrPas(SQLITEDLL);
end;

procedure loadlibs;

 function loadfunc(name:string):Pointer;
  begin
   Result:=GetProcAddress(dllhandle,pchar(name));
   if not Assigned(result) then
    raise exception.createFmt('sqlite missing function %s',[name]);
//   showmessagefmt('missinbg %s',[name]);
  end;

begin
 if FileExists (DefineSqliteDLL) then
 begin
   DLLHandle := safeLoadLibrary(DefineSqliteDLL);
 end else
 begin
   DLLHandle := safeLoadLibrary(SQLITEDLL);
   if DLLHandle=0 then
   begin
     DLLHandle := safeLoadLibrary(GetSqlite3DLL);
     if DLLHandle=0 then
       raise exception.create('sqlite3.dll not found in path');
   end;
 end;

@sqlite3_libversion:=loadfunc('sqlite3_libversion');
@sqlite3_close:=loadfunc('sqlite3_close');
@sqlite3_exec:=loadfunc('sqlite3_exec');
@sqlite3_last_insert_rowid:=loadfunc('sqlite3_last_insert_rowid');
@sqlite3_changes:=loadfunc('sqlite3_changes');
@sqlite3_total_changes:=loadfunc('sqlite3_total_changes');
@sqlite3_interrupt:=loadfunc('sqlite3_interrupt');
@sqlite3_complete:=loadfunc('sqlite3_complete');
@sqlite3_busy_handler:=loadfunc('sqlite3_busy_handler');
@sqlite3_busy_timeout:=loadfunc('sqlite3_busy_timeout');
@sqlite3_free:=loadfunc('sqlite3_free');
@sqlite3_open:=loadfunc('sqlite3_open');
@sqlite3_errcode:=loadfunc('sqlite3_errcode');
@sqlite3_errmsg:=loadfunc('sqlite3_errmsg');
@sqlite3_prepare:=loadfunc('sqlite3_prepare');
@sqlite3_bind_double:=loadfunc('sqlite3_bind_double');
@sqlite3_bind_int:=loadfunc('sqlite3_bind_int');
@sqlite3_bind_int64:=loadfunc('sqlite3_bind_int64');
@sqlite3_bind_null:=loadfunc('sqlite3_bind_null');
//@sqlite3_bind_value:=loadfunc('sqlite3_bind_value');
@sqlite3_bind_text:=loadfunc('sqlite3_bind_text');
@sqlite3_bind_blob:=loadfunc('sqlite3_bind_blob');
@sqlite3_column_count:=loadfunc('sqlite3_column_count');
@sqlite3_column_name:=loadfunc('sqlite3_column_name');
@sqlite3_column_decltype:=loadfunc('sqlite3_column_decltype');
@sqlite3_step:=loadfunc('sqlite3_step');
@sqlite3_data_count:=loadfunc('sqlite3_data_count');
@sqlite3_column_blob:=loadfunc('sqlite3_column_blob');
@sqlite3_column_bytes:=loadfunc('sqlite3_column_bytes');
@sqlite3_column_double:=loadfunc('sqlite3_column_double');
@sqlite3_column_int:=loadfunc('sqlite3_column_int');
@sqlite3_column_int64:=loadfunc('sqlite3_column_int64');
@sqlite3_column_text:=loadfunc('sqlite3_column_text');
@sqlite3_column_type:=loadfunc('sqlite3_column_type');
@sqlite3_finalize:=loadfunc('sqlite3_finalize');
@sqlite3_reset:=loadfunc('sqlite3_reset');
@sqlite3_create_function:=loadfunc('sqlite3_create_function');
@sqlite3_aggregate_count:=loadfunc('sqlite3_aggregate_count');
@sqlite3_value_blob:=loadfunc('sqlite3_value_blob');
@sqlite3_value_bytes:=loadfunc('sqlite3_value_bytes');
@sqlite3_value_double:=loadfunc('sqlite3_value_double');
@sqlite3_value_int:=loadfunc('sqlite3_value_int');
@sqlite3_value_int64:=loadfunc('sqlite3_value_int64');
@sqlite3_value_text:=loadfunc('sqlite3_value_text');
@sqlite3_value_type:=loadfunc('sqlite3_value_type');
@sqlite3_aggregate_context:=loadfunc('sqlite3_aggregate_context');
@sqlite3_user_data:=loadfunc('sqlite3_user_data');
@sqlite3_get_auxdata:=loadfunc('sqlite3_get_auxdata');
@sqlite3_set_auxdata:=loadfunc('sqlite3_set_auxdata');
@sqlite3_result_blob:=loadfunc('sqlite3_result_blob');
@sqlite3_result_double:=loadfunc('sqlite3_result_double');
@sqlite3_result_error:=loadfunc('sqlite3_result_error');
@sqlite3_result_int:=loadfunc('sqlite3_result_int');
@sqlite3_result_int64:=loadfunc('sqlite3_result_int64');
@sqlite3_result_null:=loadfunc('sqlite3_result_null');
@sqlite3_result_text:=loadfunc('sqlite3_result_text');
@sqlite3_result_value:=loadfunc('sqlite3_result_value');
@sqlite3_create_collation:=loadfunc('sqlite3_create_collation');
@sqlite3_collation_needed:=loadfunc('sqlite3_collation_needed');
@sqlite3_bind_parameter_count:=loadfunc('sqlite3_bind_parameter_count');
@sqlite3_bind_parameter_name:=loadfunc('sqlite3_bind_parameter_name');
@sqlite3_bind_parameter_index:=loadfunc('sqlite3_bind_parameter_index');
 dllinit:=True;
end;

procedure checkSqliteLoaded;
 begin
  if not dllinit then loadlibs;
 end;

initialization

finalization
 if DLLHandle<>0 then FreeLibrary(DLLHandle);

end.




