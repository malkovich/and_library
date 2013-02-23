{ $DEFINE FULL}
unit MkSqLite3;

{$HINTS OFF}
{$WARNINGS OFF}

interface

uses
 Windows, Classes, variants,Contnrs,db,MkSqLite3Api
 {$IFDEF FULL},UHom{$ENDIF};

type

TMkSqlite=class;
TMkSqlStmt=class;
TMkSqliteExtension=class;

TmkSqlFieldDef=class
 owner:TMkSqlStmt;
 name:string;
 datatype:tfieldtype;
 datatypeStr:string;
 size:Integer;
 constructor create(rs:TMkSqlStmt; idx:integer);
 procedure parseType;
end;

IMkSqlStmt=interface
['{3E60785B-D60F-4418-8DF5-919875026EDE}']
  function _instance:pointer;
  function dataBase:TMkSqlite;
  function fieldCount:integer;
  function fieldIndex(a:string):integer;
  function get_field(i:integer):TMkSqlFieldDef;
  function get_valueof(aName:string):variant;
  function get_value(col:integer):variant;
  procedure set_value(i: integer; value: variant);
  function get_eof:boolean;
  function get_handle:pointer;
  procedure next;
  procedure first;
  function  isNull(i: integer): boolean;
  function  storageClass(i:integer):integer;
  procedure bindParam(const param:tparam; idx:integer);
  procedure bindParamByName(const param:tparam); 
  procedure bindParams(const params:tparams);
  function rowCount:integer;
  function get_recno:integer;
  procedure set_recno(value:integer);
  function get_retain:boolean;
  procedure set_retain(value:boolean);
  function get_serverCursor:boolean;
  procedure set_serverCursor(value:boolean);

  procedure open(sql:string; params:tparams=nil);
  procedure close;
  function get_active:boolean;
  procedure reset;
  
  property serverCursor:boolean read get_serverCursor write set_serverCursor;
  property retainOnEof:boolean read get_retain write set_retain;
  property active:boolean read get_active;

  property Field[i:Integer]:TmkSqlFieldDef read get_Field;
  property value[i:integer]:variant read get_value write set_value; default;
  property valueof[aName:string]:variant read get_valueof;
  property eof:boolean read get_eof;
  property handle:pointer read get_handle;
  property recno:integer read get_recno write set_recno;
end;

TMkSqlStmt=class(TInterfacedObject,IMkSqlStmt)
 private
  Fdb:TMkSqlite;
  ffielddefs:TObjectList;
  FServerCursor:boolean;
  FEof,
  FRetain,
  FActive:boolean;
  Fst:pointer;
  FRows:Tlist;
  FRecno:integer;
  procedure getColumns;
  procedure step;
  procedure clearRows;
  procedure getColumnData(idx: integer;
                          var data: variant;
                          var cls: integer);
  procedure checkCursor(value: boolean);
  procedure checkActive(value: boolean);
  procedure fetchAll;
  procedure closeCursor;
 protected
  function _instance:pointer;
  procedure open(sql:string; params:tparams=nil);
  procedure close;
  procedure reset;
  function get_eof:boolean;
  function get_value(i:integer):variant;
  function get_valueof(aName:string):variant;
  procedure set_value(i: integer; value: variant);
  function isNull(i: integer): boolean;
  procedure next;
  procedure first;
  function fieldCount:integer;
  function fieldIndex(a:string):integer;
  function get_field(i:integer):TMkSqlFieldDef;
  function get_handle:pointer;
  procedure bindParam(const param:tparam; idx:integer);
  procedure bindParamByName(const param:tparam); 
  procedure bindParams(const aParams:tparams);
  function dataBase:TMkSqlite;
  function storageClass(i:integer):integer;
  function rowCount:integer;
  function get_recno:integer;
  procedure set_recno(value:integer);
  function get_retain:boolean;
  procedure set_retain(value:boolean);
  function get_serverCursor:boolean;
  procedure set_serverCursor(value:boolean);
  function get_active:boolean;
 public
  constructor create(ADatabase:TMkSqlite);
  destructor destroy; override;
end;

TMkExecCallBack=function(acolumns: Integer;
                         aColumnValues,
                         aColumnNames:Ppchar):integer of object;
PMkExecCallBack=^TMkExecCallBack;

TMkSqlite=class(TComponent)
 private
  fdb:Pointer;
  FdbName:string;
  FFullColumnNames,
  FActive,
  FUseDataTypes,
  FEmptyResultSets: Boolean;
  FtransactionLevel: Integer;
  fudf:Tobjectlist;
  FStatementTimeout:integer;
  FBusyTimeout:integer;
  FAutoCloseCursors: boolean;
  FCursors:tlist;
  FExtendedLike: boolean;
  procedure checkactive;
  procedure SetEmptyResultSets(const Value: Boolean);
  function  getdb:pointer;
  procedure setPragma(name:string; value:Boolean);
  procedure setPragmavalue(name:string; value:integer);
  function getPragmaValue(name:string):integer;
  function getSynchronous: Integer;
  procedure SetFullColumnNames(const Value: boolean);
  procedure setSynchronous(const Value: integer);
  function getCacheSize: Integer;
  function getDefCacheSize: Integer;
  procedure setCacheSize(const Value: Integer);
  procedure setDefCacheSize(const Value: Integer);
  procedure SetBusyTimeout(const Value: integer);
  procedure check(i: integer);
  procedure closeCursors;
 protected
 public
  constructor create(aowner:TComponent); override;
  destructor destroy; override;
  procedure open;
  procedure close;

//the _exec form does not raise exceptions on errors
  function _exec(sql:string; cb:TMkExecCallback=nil):integer;
  
  function  exec(Sql: string; params: Tparams=nil): IMkSqlStmt;
  function  execV(Sql:string; params:Variant):IMkSqlStmt;
  procedure execCmd(sql:string);

  function createStmt:IMksqlStmt;

  function version:string;
  function changeCount:Integer;
  function totalChangeCount:integer;
  function LastInsertRowId: integer;
  procedure checkIntegrity;

  function SchemaInfo:IMkSqlStmt;
  function SchemaTableInfo(TableName:string):IMkSqlStmt;
  function SchemaIndexList(TableName:string):IMkSqlStmt;
  function SchemaIndexInfo(IndexName:string):IMkSqlStmt;
  function SchemaForeignkeyInfo(TableName:string):IMkSqlStmt;

{$IFDEF FULL}
  function buildSchema:THomNode;
{$ENDIF}

  procedure beginTransaction;
  procedure Commit;
  procedure RollBack;

  procedure registerExtensions(funcs:array of TMkSqliteExtension);

  property active:boolean read factive;
  property TransactionLevel:Integer read FtransactionLevel;
  property handle:pointer read fdb;

  property synchronous:integer read getSynchronous write setSynchronous;
  property cacheSize:Integer read getCacheSize write setCacheSize;
  property DefaultCacheSize:Integer read getDefCacheSize write setDefCacheSize;
 published
  property dbName:string read Fdbname write fdbname;
  property FullColumnNames:boolean read FFullColumnNames write SetFullColumnNames default false;
  property EmptyResultSets:Boolean read FEmptyResultSets write SetEmptyResultSets default true;
  property BusyRetryTimeout:integer read FbusyTimeout write SetBusyTimeout default 10;
  property StatementTimeout:integer read FStatementTimeout write FStatementTimeout default -1;
  property UseDataTypes:boolean read FUseDataTypes write FUseDataTypes default true;
  property AutoCloseCursorsOnTrans:boolean read FAutoCloseCursors write FAutoCloseCursors default false;
  property ExtendedLike:boolean read FExtendedLike write FExtendedLike default true; 
end;

TMkSqliteExtension=class
 protected
  Fname:string;
  Fowner:TMkSqlite;
  function add:boolean; virtual; abstract;
 public
  constructor create(aOwner:TMkSqlite; aName:string);
end;

function mkStrToDate(a:string):TdateTime;
function mkDateToStr(d:tdatetime):string;

implementation

uses sysutils {$IFDEF FULL},mksqlite3func{$ENDIF};

var fmt:TFormatSettings;

type

PSqlRow=^TSqlRow;
TsqlRow=record
 data:array of variant;
 cls:array of integer;
end;

function mkdateToStr(d: tdatetime): string;
begin
 result:=FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz',d,fmt);
end;

function mkstrToDate(a: string): TdateTime;
begin
 result:=sysutils.StrToDateTime(a,fmt);
end;

//stub functions

function _ExecCallback(cbParam: PMkexecCallBack;
                       Columns: Integer;
                       ColumnValues,
                       ColumnNames: Ppchar): integer; cdecl;
 begin
  try
   Result:=cbparam^(columns,columnValues,columnNames);
  except
   result:=1;
  end;
 end;

{ TmkSqlField }

constructor TmkSqlFieldDef.create(rs:TMkSqlStmt; idx:integer);
begin
 owner:=rs;
 name:=sqlite3_column_name(owner.Fst,idx);
 datatypestr:=lowercase(sqlite3_column_declType(owner.fst,idx));
 parseType;
end;

procedure TmkSqlFieldDef.parseType;
 const tokens:array[0..5] of record
               token:string;
               dtype:TFieldType;
              end=
           (
            (token:'integer'; dtype:ftInteger),
            (token:'double'; dtype:ftFloat),
            (token:'date'; dtype:ftDateTime),
            (token:'char'; dtype:ftstring),
            (token:'clob'; dtype:ftmemo),
            (token:'blob'; dtype:ftBlob)
           );
 var i,j,k:integer;
     a:string;
begin
 if owner.Fdb.FUseDataTypes then
  for i:=low(tokens) to high(tokens) do with tokens[i] do
   if Pos(token,datatypestr)<>0 then
    begin
     datatype:=dtype;
     if datatype=ftstring then
      begin
       j:=pos('(',datatypestr);
       if j>0 then
        begin
         k:=pos(')',datatypestr);
         if k>j then
          begin
           a:=copy(datatypestr,j+1,k-j-1);
           size:=StrToIntDef(a,size);
          end
        end
      end;
     exit;
    end;
 datatype:=ftString;
end;

{ TMkSqlStmt }

constructor TMkSqlStmt.create(ADatabase:TMkSqlite);
begin
 inherited create;
 Fdb:=Adatabase;
end;

destructor TMkSqlStmt.destroy;
begin
 close;
 inherited;
end;

function TMkSqlStmt.fieldIndex(a: string): integer;
begin
 if assigned(ffieldDefs) then
  for result:=0 to ffielddefs.count-1 do
   if sametext(a,get_field(result).name) then exit;
 Result:=-1;
end;

function TMkSqlStmt.get_Field(i: Integer): TmkSqlFieldDef;
begin
 if assigned(ffielddefs) then result:=tmksqlfieldDef(ffielddefs[i])
 else result:=nil;
end;

function TMkSqlStmt.FieldCount: Integer;
begin
 if assigned(ffielddefs) then result:=ffielddefs.count
 else result:=0;
end;

function TMkSqlStmt.database:TmkSqlite;
begin
 result:=fDb;
end;

function TMkSqlStmt._instance: pointer;
begin
 result:=pointer(self);
end;

function TMkSqlStmt.get_eof: boolean;
begin
 result:=feof;
end;

procedure TMkSqlStmt.first;
begin
 checkCursor(false);
 set_recno(1)
end;

procedure TMkSqlStmt.step;
var t,ferr:integer;
begin
 if not assigned(fst) then 
  begin
   Feof:=true;
   exit
  end;
 with fdb do
  begin
   if FStatementTimeout>0 then
    t:=integer(GetTickCount)+FStatementTimeout
   else t:=0;
   repeat
    FErr:=sqlite3_step(fst);
   until (FErr=SQLITE_ROW) or
         (Ferr=SQLITE_DONE) or
          not (Ferr in [SQLITE_BUSY,SQLITE_LOCKED]) or
          (FStatementTimeout=0) or
          ((FStatementTimeout>0) and (integer(GetTickCount)>=t));
   case FErr of
    SQLITE_ROW:FEof:=false;
    SQLITE_DONE:feof:=true;
    else
     begin
      Feof:=true;
      check(ferr);
     end; 
   end;
  end;
end;

procedure TMkSqlStmt.getColumns;
var i,n:integer;
begin
 n:=sqlite3_column_count(fst);
 if n=0 then exit;
 FFieldDefs:=TObjectlist.Create;
 for i:=0 to n-1 do
  ffielddefs.Add(TmkSqlFieldDef.create(self,i));
end;

function TMkSqlStmt.get_handle: pointer;
begin
 result:=fst;
end;

procedure TMkSqlStmt.getColumnData(idx:integer;
                                   var data:variant;
                                   var cls:integer);
var f:TmkSqlFieldDef;
    size:integer;
    a:string;
    p:pchar;
begin
 cls:=sqlite3_column_type(fst,idx);
 if cls=SQLITE_NULL then
  begin
   data:=null;
   exit
  end;
 f:=get_field(idx);
 case f.datatype of
  ftInteger:data:=sqlite3_column_int(fst,idx);
  ftFloat:data:=sqlite3_column_double(fst,idx);
  ftDateTime:if cls in [SQLITE_FLOAT,SQLITE_INTEGER] then
              data:=TdateTime(sqlite3_column_double(fst,idx))
             else
              begin
               a:=sqlite3_column_text(fst,idx);
               try
                data:=mkstrToDate(a);
               except
                data:=a;
               end;
              end;
  ftBlob:begin
          size:=sqlite3_column_bytes(fst,idx);
          data:=varArrayCreate([0,size-1],varByte);
          p:=varArrayLock(data);
          try
           if size>0 then
            move(sqlite3_column_blob(fst,idx)^,p^,size);
          finally
           varArrayUnlock(data)
          end;
         end;
  else data:=string(sqlite3_column_text(fst,idx));
 end;
end;

function TMkSqlStmt.storageClass(i: integer): integer;
begin
 if feof then result:=SQLITE_NULL
 else if FserverCursor then
  result:=sqlite3_column_type(fst,i)
 else if assigned(frows) then
  result:=psqlrow(frows[Frecno]).cls[i]
 else result:=SQLITE_NULL;
end;

function TMkSqlStmt.isNull(i:integer):boolean;
begin
 result:=storageClass(i)=SQLITE_NULL;
end;

function TMkSqlStmt.get_value(i: integer): variant;
var dt:integer;
begin
 if Feof then result:=unassigned
 else if FserverCursor then getColumnData(i,result,dt)
 else if assigned(frows) then
  result:=psqlrow(frows[Frecno]).data[i]
 else result:=unassigned;
end;

procedure TMkSqlStmt.set_value(i:integer; value:variant);
begin
 checkCursor(false);
 if feof then raise exception.create('at eof');
 if assigned(frows) then
  psqlrow(frows[Frecno]).data[i]:=value;
end;

function TMkSqlStmt.get_valueof(aname:string): variant;
var i:integer;
begin
 i:=fieldIndex(aname);
 if i<0 then raise exception.createFmt('no such field : %s',[aname]);
 result:=get_value(i);
end;

procedure TMkSqlStmt.bindParam(const param: tparam; idx: integer);
var i:integer;
    a:string;
    v:variant;
    p:pchar;
begin
 if not assigned(fst) then exit;
 inc(idx); // 1-based
 with param do
  if isNull then sqlite3_bind_null(fst,idx)
  else case datatype of
   ftUnknown:sqlite3_bind_null(fst,idx);

   ftString,
   ftWideString,
   ftFixedChar,
   ftMemo:begin
           a:=asString;
           sqlite3_bind_text(fst,idx,pchar(a),length(a),SQLITE_TRANSIENT);
          end;

   ftLargeInt:sqlite3_bind_int64(fst,idx,value);

   ftSmallInt,
   ftInteger,
   ftWord:sqlite3_bind_int(fst,idx,asInteger);

   ftBoolean:begin
              if asBoolean then i:=1 else i:=0;
              sqlite3_bind_int(fst,idx,i);
             end;

   ftBcd,
   ftFloat,
   ftCurrency:sqlite3_bind_double(fst,idx,asFloat);
(*
   ftDate,
   ftTime,
   ftDateTime:sqlite3_bind_double(fst,idx,asDateTime);
*)
   ftDate,
   ftTime,
   ftDateTime:begin
               a:=mkdateToStr(asDateTime);
               sqlite3_bind_text(fst,idx,pchar(a),length(a),SQLITE_TRANSIENT);
              end;

   ftBlob,
   ftBytes,
   ftVarBytes:
    begin
     v:=param.value;
     if varType(v)<>(varArray or VarByte) then
      raise exception.create('invalid format for blob data');
     i:=varArrayHighBound(v,1)+1;
     p:=varArraylock(v);
     try
      fdb.check(sqlite3_bind_blob(fst,idx,p,i,SQLITE_TRANSIENT));
     finally
      varArrayUnlock(v)
     end;
    end;
   else
    raise exception.createfmt('unknown param type : %d',[ord(dataType)]);
  end;
end;

procedure TMkSqlStmt.bindParamByName(const param: tparam);
var idx:integer;
begin
 idx:=sqlite3_bind_parameter_index(fst,pchar(param.Name));
 if idx=0 then raise exception.createFmt('parameter not found : %s',[param.Name]);
 BindParam(param,idx-1);
end;

procedure TMkSqlStmt.bindParams(const aParams: tparams);
var i,idx:integer;
    p:tparam;
begin
 if assigned(aParams) then
  for i:=0 to aParams.count-1 do
   begin
    p:=aparams[i];
(* bind by name is problematic because the params.parseSql does
not support re-using the same name, but sqlite does. so use
positional parameters always
*)    
(*    if p.name>'' then
     begin
      idx:=sqlite3_bind_parameter_index(fst,pchar(':'+p.name));
      if idx=0 then idx:=i else idx:=idx-1;
     end
    else *)
     idx:=i; 
    bindParam(p,idx);
   end;
end;

procedure TMkSqlStmt.checkCursor(value:boolean);
begin
 if FServerCursor<>value then
  raise exception.create('wrong cursor state');
end;

procedure TMkSqlStmt.checkActive(value:boolean);
begin
 if Factive<>value then raise exception.create('wrong active state');
end;

function TMkSqlStmt.get_recno: integer;
begin
 if Fservercursor or not assigned(frows) or not factive then
  result:=-1
 else
  result:=frecno+1;
end;

function TMkSqlStmt.rowCount: integer;
begin
 if FServerCursor or not assigned(frows) or not factive then
  result:=-1
 else
  result:=frows.count
end;

procedure TMkSqlStmt.set_recno(value: integer);
begin
 if not assigned(frows) then
  begin
   feof:=true;
   exit;
  end; 
 Frecno:=pred(value);
 feof:=(Frecno<0) or (Frecno>=Frows.Count);
end;

function TMkSqlStmt.get_retain: boolean;
begin
 result:=fRetain;
end;

procedure TMkSqlStmt.set_retain(value: boolean);
begin
 fretain:=value;
end;

function TMkSqlStmt.get_serverCursor: boolean;
begin
 result:=FserverCursor;
end;

procedure TMkSqlStmt.set_serverCursor(value: boolean);
begin
 FserverCursor:=value;
end;

function TMkSqlStmt.get_active: boolean;
begin
 result:=factive;
end;

procedure TMkSqlStmt.reset;
begin
 checkCursor(true);
 checkActive(true);
 if not assigned(fst) then raise exception.Create('cursor closed');
 fdb.check(sqlite3_reset(fst));
 feof:=false;
end;

procedure TMkSqlStmt.closeCursor;
begin
 if assigned(fst) then
  begin
   sqlite3_finalize(fst);
   fst:=nil;
   if FserverCursor then fdb.FCursors.Remove(self);
  end;
end;

procedure TMkSqlStmt.clearRows;
var i:integer;
begin
 if not assigned(frows) then exit;
 for i:=frows.count-1 downto 0 do dispose(PSqlRow(frows[i]));
 freeandNil(frows);
end;

procedure TMkSqlStmt.close;
begin
 closeCursor;
 clearRows;
 freeAndNil(ffieldDefs);
 FEof:=true;
 FActive:=false;
end;

procedure TMkSqlStmt.open(sql:string; params:tparams);
var p,tail:pchar;
begin
 checkactive(false);
 close; //just in case
 p:=pchar(sql);
 repeat
  fst:=nil;
  fdb.check(sqlite3_prepare(fdb.fdb,p,-1,fst,tail));
  if assigned(fst) then
   begin
    bindParams(params);
    step; //execute
   end
  else
   feof:=true;
  while (tail^<>#0) and (tail^<=' ') do inc(tail);
  if tail^<>#0 then
   begin
    close; // multiple statements, so release cursor
    p:=tail;
   end;
 until tail^=#0;
 if not assigned(fst) then exit;
 getColumns;
 FActive:=true;
 if FserverCursor then fdb.FCursors.Add(self)
 else
  begin
   if not feof and (fieldCount>0) then fetchall;
   closeCursor;
  end;
end;

procedure TMkSqlStmt.fetchAll;
var i,n:integer;
    p:pSqlRow;
begin
 frows:=tlist.create;
 n:=fieldcount;
 while not Feof do
  begin
   new(p);
   with p^ do
    begin
     setlength(data,n);
     setlength(cls,n);
     for i:=0 to n-1 do
      getColumnData(i,data[i],cls[i]);
    end;
   frows.add(p);
   step;
  end;
 FRecno:=0;
 FEof:=Frows.count=0;
end;

procedure TMkSqlStmt.next;
begin
 if feof then exit;
 if FserverCursor then
  begin
   step;
   if Feof and not Fretain then closeCursor;
  end
 else if assigned(frows) then
  begin
   inc(frecno);
   feof:=frecno>=frows.count
  end
 else feof:=true;
end;

{ TMkSqlite }

constructor TMkSqlite.create(aowner: TComponent);
begin
 inherited;
 Fudf:=tobjectlist.create(true);
 FCursors:=tlist.Create;
 FEmptyResultSets:=true;
 FBusyTimeOut:=10;
 FStatementTimeout:=-1;
 FUseDataTypes:=true;
 FExtendedLike:=true;
{$IFDEF FULL}
 registerExtensions([
                    TansiUpper.create(self,'upper'),
                    TAnsiLower.create(self,'lower'),
                    TSubString.create(self,'substring',3),
                    TNow.create(self,'now',0),
                    tDate.create(self,'date',1),
                    ttime.create(self,'time',1),
                    tDay.create(self,'day',1),
                    tmonth.create(self,'month',1),
                    tyear.create(self,'year',1),
                    TMatches.create(self,'matches',2),
                    TAnsiCollation.create(self,'ANSINOCASE'),
                    TMixedCollation.create(self,'MIXED')
                    ]);
{$ENDIF}
end;

destructor TMkSqlite.destroy;
begin
 close;
 freeAndNil(Fcursors);
 freeAndNil(Fudf);
 inherited;
end;

procedure TMkSqlite.closeCursors;
var i:integer;
begin
 for i:=fcursors.Count-1 downto 0 do
  TMkSqlStmt(Fcursors[i]).close;
 Fcursors.clear; 
end;

procedure TMkSqlite.checkactive;
begin
 if not assigned(fdb) then raise exception.create('Database is closed');
end;

function TMkSqlite.version: string;
begin
 checksqliteLoaded;
 result:=sqlite3_libversion;
end;

procedure TMkSqlite.close;
begin
 try
  if Assigned(fdb) then
   begin
    closeCursors;
    check(sqlite3_Close(fdb));
   end; 
 finally
  fdb:=nil;
  factive:=false;
 end
end;

procedure TMkSqLite.check(i:integer);
 begin
  if i<>sqlite_OK then
   raise Exception.CreateFmt('%d:%s',[i,sqlite3_errmsg(fdb)])
 end;

procedure TMkSqlite.open;
 var i:Integer;
     a:string;
begin
 checksqliteLoaded;
 close;
 a:=fdbname;
 if a='' then a:=':memory:';
 check(sqlite3_Open(pchar(a),fdb));
{$IFDEF FULL}
 if FExtendedLike then
  fudf.add(TLike.create(self,'like',2));
{$ENDIF}
 for i:=0 to fudf.Count-1 do with TMkSqliteExtension(fudf[i]) do
  if not add then
   raise Exception.CreateFmt('Failed to add Extension %s',[fname]);
 FActive:=True;
 if FEmptyResultSets then setEmptyResultSets(true);
 if FFullColumnNames then SetFullColumnNames(true);
 SetBusyTimeout(FBusyTimeout);
end;

function TMkSqlite._exec(sql:string; cb:TMkExecCallback):integer;
 var t:Integer;
begin
 if FStatementTimeout>0 then
  t:=integer(GetTickCount)+FStatementTimeout
 else t:=0;
 repeat
  if assigned(cb) then
   result:=sqlite3_Exec(Getdb,pchar(sql),@_execCallBack,@@cb,nil)
  else
   result:=sqlite3_Exec(getdb,pchar(sql),nil,nil,nil);
 until (result=SQLITE_OK) or
        not (result in [SQLITE_BUSY,SQLITE_LOCKED]) or
        (FStatementTimeout=0) or
        ((FStatementTimeout>0) and (integer(GetTickCount)>=t));
end;

function TMkSqlite.createStmt: IMksqlStmt;
var st:TmksqlStmt;
begin
 st:=Tmksqlstmt.create(self);
 result:=st;
end;

function TMkSqlite.exec(Sql: string; params: Tparams=nil): IMkSqlStmt;
begin
 result:=createStmt;
 result.open(sql,params);
end;

Function VariantToParams(params:Olevariant):TParams;
 var i:integer;

  procedure AddParam(const p:olevariant);
   var param:tparam;
   begin
    param:=result.CreateParam(ftUnknown,'',ptInput);
    param.value:=p;
   end;

 begin
  if varisEmpty(params) then  result:=nil
  else
  begin
   result:=TParams.Create;
   try
    if not varIsArray(Params) then addParam(params)
    else
     for i:=VarArrayLowBound(Params,1) to varArrayHighBound(Params,1) do
      addParam(params[i])
   except
    Result.Free;
    raise;
   end;
  end;
 end;

function TMkSqlite.execV(Sql: string; params: Variant): IMkSqlStmt;
var p:tparams;
begin
 p:=variantToParams(params);
 try
  Result:=exec(sql,p);
 finally
  p.free
 end;
end;

procedure TMkSqlite.execCmd(sql: string);
begin
 check(_exec(sql));
end;

procedure TMkSqlite.SetEmptyResultSets(const Value: Boolean);
begin
 if factive then setPragma('empty_result_callbacks',value);
 FEmptyResultSets := Value;
end;

function TMkSqlite.changeCount: Integer;
begin
 result:=sqlite3_Changes(getdb);
end;

function TMkSqlite.totalChangeCount: integer;
begin
 result:=sqlite3_total_Changes(getdb);
end;

function TMkSqlite.LastInsertRowId:integer;
 begin
  result:=sqlite3_Last_Insert_RowId(getDb)
 end;

function TMkSqlite.getdb: pointer;
begin
 checkactive;
 result:=fdb
end;

procedure TMkSqlite.setPragma(name: string; value: Boolean);
 var a:string;
begin
  if value then a:='ON' else a:='OFF';
  execCmd(Format('PRAGMA %s=%s',[name,a]));
end;

function TMkSqlite.getPragmaValue(name: string): integer;
 var rs:IMkSqlStmt;
begin
 rs:=exec('PRAGMA '+name);
 if Assigned(rs) and (rs.fieldCount>0) then Result:=StrToInt(Rs[0])
 else
  result:=-1;
end;

procedure TMkSqlite.setPragmavalue(name: string; value: integer);
begin
 execCmd(Format('PRAGMA %s=%d',[name,value]));
end;

function TMkSqlite.getSynchronous: integer;
begin
 result:=getpragmaValue('synchronous');
end;

procedure TMkSqlite.SetFullColumnNames(const Value: boolean);
begin
 if FActive then setpragma('full_column_names',value);
 FFullColumnNames:=Value;
end;

procedure TMkSqlite.setSynchronous(const Value: integer);
begin
 setpragmaValue('synchronous',value);
end;

function TMkSqlite.getCacheSize: Integer;
begin
 result:=getPragmaValue('cache_size');
end;

function TMkSqlite.getDefCacheSize: Integer;
begin
 result:=getPragmaValue('default_cache_size');
end;

procedure TMkSqlite.setCacheSize(const Value: Integer);
begin
 setPragmaValue('cache_size',value);
end;

procedure TMkSqlite.setDefCacheSize(const Value: Integer);
begin
 setPragmaValue('default_cache_size',value);
end;

procedure TMkSqlite.SetBusyTimeout(const Value: integer);
begin
  FbusyTimeout := Value;
  if factive then sqlite3_busy_timeout(fdb,FbusyTimeout);
end;

procedure TMkSqlite.checkIntegrity;
 var rs:IMkSqlStmt;
begin
 rs:=exec('PRAGMA integrity_check');
 if not Assigned(rs) then raise Exception.Create('integrity check failed');
 if rs[0]<>'ok' then raise Exception.Create(rs[0]);
end;

function TMkSqlite.SchemaIndexInfo(IndexName: string):IMkSqlStmt;
begin
 result:=exec(format('PRAGMA index_info(''%s'')',[indexname]));
end;

function TMkSqlite.SchemaIndexList(TableName: string):IMkSqlStmt;
begin
 result:=exec(format('PRAGMA index_list(''%s'')',[tablename]));
end;

function TMkSqlite.SchemaInfo:IMkSqlStmt;
begin
 result:=exec('select * from sqlite_master order by tbl_name,name');
end;

function TMkSqlite.SchemaTableInfo(TableName: string):IMkSqlStmt;
begin
 result:=exec(format('PRAGMA table_info(''%s'')',[tablename]));
end;

function TMkSqlite.SchemaForeignkeyInfo(TableName: string): IMkSqlStmt;
begin
 result:=exec(format('PRAGMA foreign_key_list(''%s'')',[tablename]));
end;

{$IFDEF FULL}
function TMkSqlite.buildSchema: THomNode;

 var info:IMkSqlStmt;
     tables,views,newItem:THomNode;
     itemType:string;

 procedure addItem;
 var columns,column:thomnode;
     rs:IMkSqlStmt;
     fname:string;
 begin
  if itemtype='table' then
   begin
    if not assigned(tables) then tables:=result.newchild('tables');
    newitem:=tables.newchild(itemtype)
   end
  else if itemtype='view' then
   begin
    if not assigned(views) then views:=result.newchild('views');
    newitem:=views.newchild(itemtype)
   end
  else exit;
  fname:=info.valueof['name'];
  newitem['id']:=fname;
  newitem.NewChild('ddl').Value:=info.valueof['sql'];
  columns:=newitem.NewChild('columns');
  rs:=schemaTableInfo(fname);
  while not rs.eof do
   begin
    column:=columns.newchild('column');
    column['id']:=rs.valueof['name'];
    column['type']:=rs.valueof['type'];
    rs.next;
   end;
 end;

 procedure addIndexes;
  var indexes,index,col:THomNode;
      rsidxs,rsidx,tmp:IMkSqlStmt;
      indexname:string;
 begin
  rsidxs:=schemaIndexList(newitem['id']);
  indexes:=nil;
  if assigned(rsidxs) then while not rsidxs.eof do
   begin
    if not Assigned(indexes) then indexes:=newitem.NewChild('indexes');
    index:=indexes.NewChild('index');
    indexname:=rsidxs.valueof['name'];
    index['id']:=indexname;
    index['unique']:=rsidxs.valueof['unique'];
    tmp:=exec(format('select * from sqlite_master where type=''index'' and tbl_name=''%s''',
                   [string(newitem['id'])]));
    index.NewChild('ddl').Value:=tmp.valueof['sql'];
    rsIdx:=schemaIndexInfo(indexName);
    if Assigned(rsidx) then while not rsidx.eof do
     begin
      col:=index.newchild('column');
      col['id']:=rsIdx.valueof['name'];
      rsidx.next;
     end;
    rsIdx:=nil;
    rsIdxs.next;
   end;
 end;

 procedure addForeignKeys;
  var fks,fk,col:THomNode;
      rsfk:Imksqlstmt;
      id:string;
 begin
  fks:=nil;
  rsfk:=schemaForeignKeyInfo(newitem['id']);
  if assigned(rsfk) then while not rsfk.eof do
   begin
    if not Assigned(fks) then fks:=newitem.NewChild('foreignKeys');
    fk:=fks.NewChild('foreignKey');
    id:=rsfk.valueof['id'];
    fk['id']:=id;
    fk['table']:=rsfk.valueof['table'];
    while not rsfk.eof and (rsfk.valueof['id']=id) do
     begin
      col:=fk.NewChild('column');
      col['from']:=rsfk.valueof['from'];
      col['to']:=rsfk.valueof['to'];
      rsfk.next;
     end;
   end;
 end;

 procedure addTriggers;
  var rs:IMkSqlStmt;
      tgs,t:THomNode;
 begin
  rs:=exec(format('select * from sqlite_master where type=''trigger'' and tbl_name=''%s''',
                   [string(newitem['id'])]));
  tgs:=nil;
  if assigned(rs) then while not rs.eof do
   begin
    if not Assigned(tgs) then tgs:=newitem.NewChild('triggers');
    t:=tgs.NewChild('trigger');
    t['id']:=rs.valueof['name'];
    t.NewChild('ddl').Value:=rs.valueof['sql'];
    rs.next;
   end;
 end;

begin // buildSchema
 result:=thomnode.Create('schema');
 tables:=nil;
 views:=nil;
 //execute a dummy statement to clear "schema changed" state
 _exec('select name from sqlite_master');
 info:=exec('select * from sqlite_master where type=''table'' or type=''view'' order by name');
 if assigned(info) then while not info.eof do
  begin
   itemType:=info.valueof['type'];
   addItem;
   if itemType='table' then
    begin
     addIndexes;
     addForeignkeys;
    end;
   addTriggers;
   info.next;
  end;
end;

{$ENDIF}

procedure TMkSqlite.beginTransaction;
begin
 if FtransactionLevel=0 then
  begin
   if FAutoCloseCursors then closeCursors;
   execCmd('begin');
  end;
 inc(FtransactionLevel);
end;

procedure TMkSqlite.Commit;
begin
 if FtransactionLevel>0 then
  begin
   if FtransactionLevel=1 then execCmd('commit');
   dec(FtransactionLevel);
  end
end;

procedure TMkSqlite.RollBack;
begin
 if ftransactionlevel>0 then execCmd('rollback');
 FtransactionLevel:=0
end;

procedure TMkSqlite.registerExtensions(funcs: array of TMkSqliteExtension);
 var i:integer;
begin
 for i:=low(funcs) to high(funcs) do fudf.Add(funcs[i]);
end;

{ TMkSqliteExtension }

constructor TMkSqliteExtension.create(aOwner: TMkSqlite; aName: string);
begin
 fowner:=aowner;
 fname:=aname;
end;

initialization

 getLocaleFormatSettings(syslocale.DefaultLCID,fmt);
 fmt.DateSeparator:='-';
 fmt.TimeSeparator:=':';
 fmt.DecimalSeparator:='.';
 fmt.ShortDateFormat:='yyyy-mm-dd';

end.

