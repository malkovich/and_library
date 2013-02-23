unit ShareMapMemory;

interface

uses windows, sysUtils, Classes;

function CreateThrShare (InitData: TMemoryStream): String; overload;
function CreateTheShare (ShareName: String; DataSize: Integer): Pointer; overload;
function CreateTheShare (ShareName: String; InitData: Pointer; DataSize: Integer): Pointer; overload;
function DestroyTheShare (ShareName: String): BOOL;

function IsOnService (ShareName: String): BOOL;

function OpenTheShare (ShareName: String; var DataSize: Integer): Pointer; overload;
function OpenTheShare (ShareName: String): TMemoryStream; overload;
function CloseTheShare (ShareName: String): BOOL;


function NewSA: PSecurityAttributes;
procedure DisposeSA (sa: PSecurityAttributes);

implementation

uses comObj;



function NewSA: PSecurityAttributes;
var
  SecurityDescriptor: PSecurityDescriptor;
begin
  New (SecurityDescriptor);
  New (Result);
  InitializeSecurityDescriptor(SecurityDescriptor,SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDACL(SecurityDescriptor,true,nil,false);
  Result.nLength := sizeof(SECURITY_ATTRIBUTES);
  Result.lpSecurityDescriptor := SecurityDescriptor;
  Result.bInheritHandle := false;
end;

procedure DisposeSA (sa: PSecurityAttributes);
begin
  Dispose (sa.lpSecurityDescriptor);
  Dispose (sa);
end;

Type
  LPTMapBlock = ^TMapBlock;
  TMapBlock = record
    IsOnService: BOOL;
    Size: integer;
    Buffer: Array[0..0] of char;
  end;

  LPTStroeRecord = ^TStroeRecord;
  TStroeRecord = record
    FMapHandle: THandle;
    MapHead: LPTMapBlock;
    Data: Pointer;
    DataSize: integer;
  end;



var
  CreateList: TStringList;

function CreateTheShare (ShareName: String; DataSize: Integer): Pointer;
var
  MapHead: LPTMapBlock;
  FMapHandle: THandle;
  SA: PSecurityAttributes;
  Item: LPTStroeRecord;
begin
  Result := nil;
  if DataSize <= 0 then exit;
  if Length(ShareName) < 4 then exit;

  SA := NewSA;
  FMapHandle := CreateFileMapping(INVALID_HANDLE_VALUE, SA, PAGE_READWRITE, 0, DataSize + SizeOf(TMapBlock), PChar(ShareName));
  DisposeSA (SA);
  if FMapHandle = 0 then  Exit;

  MapHead := MapViewOfFile (FMapHandle, FILE_MAP_WRITE or FILE_MAP_READ, 0, 0, 0);
  if MapHead = nil then
  begin
    CloseHandle (FMapHandle);
    Exit;
  end;

  MapHead.Size:= DataSize;
  MapHead.IsOnService := True;
  Result := @MapHead.Buffer[0];

  if not Assigned (CreateList) then
    CreateList := TStringList.Create;

  New (Item);
  Item.FMapHandle := FMapHandle;
  Item.MapHead := MapHead;
  Item.Data := Result;
  Item.DataSize := DataSize;
  CreateList.AddObject(ShareName, Pointer(Item));
end;
  
function CreateTheShare (ShareName: String; InitData: Pointer; DataSize: Integer): Pointer;
begin
  Result := CreateTheShare (ShareName, DataSize);
  if Assigned (Result) then
    CopyMemory (Result, InitData, DataSize);
end;

function CreateThrShare (InitData: TMemoryStream): String;
begin
  Result := CreateClassID;
  if nil = CreateTheShare (Result, InitData.Memory, InitData.Size) then
    Result := '';
end;

function DestroyTheShare (ShareName: String): BOOL;
var
  Index: Integer;
  Item: LPTStroeRecord;
begin
  Result := False;
  if not Assigned (CreateList) then exit;
  if Length(ShareName) < 4 then exit;

  Index := CreateList.IndexOf(ShareName);
  if Index = -1 then exit;

  Item := Pointer(CreateList.Objects[Index]);

  Item.MapHead.IsOnService := False;
  UnMapViewOfFile(Item.MapHead);
  CloseHandle (Item.FMapHandle);
  Dispose (Item);
  CreateList.Delete(Index);      
  Result := True;
end;

function OpenTheShare (ShareName: String): TMemoryStream;
var
  Data: Pointer;
  DataSize: integer;
begin
  Result := TMemoryStream.Create;
  Data := OpenTheShare (ShareName, DataSize);
  if Assigned (Data) then
  begin
    Result.Seek(0, soFromBeginning);
    Result.WriteBuffer(Data^, DataSize);
    Result.Seek(0, soFromBeginning);
  end;
end;

var
  OpenList: TStringList;

function OpenTheShare (ShareName: String; var DataSize: Integer): Pointer;
var
  MapHead: LPTMapBlock;
  FMapHandle: THandle;
  Item: LPTStroeRecord;
  Index: Integer;
begin
  Result := nil;

  if not Assigned (OpenList) then
    OpenList := TStringList.Create;

  Index := OpenList.IndexOf(ShareName);
  if Index >= 0 then
  begin
    Item := Pointer(OpenList.Objects[Index]);

    if Item.MapHead.IsOnService then
    begin
      Result := Item.Data;
      DataSize := Item.DataSize;
    end;
    
    Exit;
  end;                 

  FMapHandle := OpenFileMapping (FILE_MAP_WRITE or FILE_MAP_READ, False, PChar(ShareName));
  if FMapHandle = 0 then
  begin
    Exit;
  end;

  MapHead := MapViewOfFile (FMapHandle, FILE_MAP_WRITE or FILE_MAP_READ, 0, 0, 0);
  if MapHead = nil then
  begin
    CloseHandle (FMapHandle);
    Exit;
  end;

  if not MapHead.IsOnService then
  begin
    UnMapViewOfFile(MapHead);
    CloseHandle (FMapHandle);   
    Exit;
  end;


  DataSize := MapHead.Size;
  Result := @MapHead.Buffer[0];

  New (Item);
  Item.FMapHandle := FMapHandle;
  Item.MapHead := MapHead;
  Item.Data := Result;
  Item.DataSize := DataSize;
  OpenList.AddObject(ShareName, Pointer(Item));
end;

function IsOnService (ShareName: String): BOOL;
var
  MapHead: LPTMapBlock;
  FMapHandle: THandle;
begin
  Result := False;

  FMapHandle := OpenFileMapping (FILE_MAP_WRITE or FILE_MAP_READ, False, PChar(ShareName));
  if FMapHandle = 0 then
  begin
    Exit;
  end;

  MapHead := MapViewOfFile (FMapHandle, FILE_MAP_WRITE or FILE_MAP_READ, 0, 0, 0);
  if MapHead = nil then
  begin
    CloseHandle (FMapHandle);
    Exit;
  end;

  Result := MapHead.IsOnService;

  UnMapViewOfFile(MapHead);
  CloseHandle (FMapHandle);
end;

function CloseTheShare (ShareName: String): BOOL;
var
  Index: Integer;
  Item: LPTStroeRecord;
begin
  Result := False;
  if not Assigned (OpenList) then exit;

  Index := OpenList.IndexOf(ShareName);
  if Index = -1 then exit;

  Item := Pointer(OpenList.Objects[Index]);

  UnMapViewOfFile(Item.MapHead);
  CloseHandle (Item.FMapHandle);
  Dispose (Item);
  OpenList.Delete(Index);
  Result := True;
end;



end.