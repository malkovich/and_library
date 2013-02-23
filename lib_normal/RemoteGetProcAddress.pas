unit RemoteGetProcAddress;

interface

uses
  Windows, SysUtils, TlHelp32;

function PidProcesso(NomeProcesso: string): Cardinal;
function IntToHex(dwValue, dwDigits: Cardinal): String; stdcall;
function RemoteGetModuleHandle(
               hProcess: Cardinal; //handle del processo
               nomeModulo: PAnsiChar; //nome del modulo
               var BaseAddr: Cardinal //indirizzo di base
               ): Boolean;
function RemoteGetProcAddress(
             //handle al processo
             hProcess: Cardinal;
             //Indirizzo di base del modulo (ottenuto con RemoteGetModuleHandle)
             hModule: Cardinal;
             //NomeModulo: string;
             //NomeFunzione: string;
             //puntatore ad una stringa che definisce il nome della funzione
             ProcName: PAnsiChar;
             var FuncAddress: Cardinal): Boolean;
function OpenProcessEx(
             dwDesiredAccess: Cardinal;
             bInheritableHandle: LongBool;
             dwProcessId: Cardinal
             ): Cardinal;

implementation

type

  pCardinal = ^Cardinal;

//definizioni e dichiarazioni per le Native API
  PANSI_STRING = ^ANSI_STRING;
  ANSI_STRING = record
    Length: Word;
    MaximumLength: Word;
    Buffer: PAnsiChar;
  end;

  PUNICODE_STRING = ^UNICODE_STRING;
  UNICODE_STRING = record
    Length: Word;
    MaximumLength: Word;
    Buffer: PWideChar;
  end;

  LDR_MODULE = record // not packed!
    InLoadOrderLinks: LIST_ENTRY;
    InMemoryOrderLinks: LIST_ENTRY;
    InInitializationOrderLinks: LIST_ENTRY;
    DllBase: Cardinal;
    EntryPoint: Cardinal;
    SizeOfImage: Cardinal;
    FullDllName: UNICODE_STRING;
    BaseDllName: UNICODE_STRING;
    Flags: Cardinal;
    LoadCount: SmallInt;
    TlsIndex: SmallInt;
    HashLinks: LIST_ENTRY;
    TimeDateStamp: Cardinal;
    LoadedImports: Pointer;
    EntryPointActivationContext: Pointer; // PACTIVATION_CONTEXT
    PatchInformation: Pointer;
  end;

  PPEB_LDR_DATA = ^PEB_LDR_DATA;
  PPPEB_LDR_DATA = ^PPEB_LDR_DATA;
  PEB_LDR_DATA = record // not packed!
  (*000*)Length: ULONG;
  (*004*)Initialized: BOOLEAN;
  (*008*)SsHandle: Pointer;
  (*00c*)InLoadOrderModuleList: LIST_ENTRY;
  (*014*)InMemoryOrderModuleList: LIST_ENTRY;
  (*01c*)InInitializationOrderModuleList: LIST_ENTRY;
  (*024*)EntryInProgress: Pointer;
  end;

function  RtlAnsiStringToUnicodeString(
    DestinationString : PUNICODE_STRING;
    SourceString : PANSI_STRING;
    AllocateDestinationString : Boolean
  ): Integer; stdcall; external 'ntdll.dll';

procedure RtlFreeUnicodeString(
    UnicodeString : PUNICODE_STRING
     ); stdcall; external 'ntdll.dll';

function  LdrGetDllHandle(
    pwPath : PWORD;
    Unused : pointer;
    ModuleFileName : PUNICODE_STRING;
    pHModule : pCardinal
  ): Integer; stdcall; external 'ntdll.dll';

function  LdrLoadDll(
    PathToFile : PWideChar;
    Flags : Cardinal;
    ModulFileName : PUNICODE_STRING;
    ModuleHandle : pCardinal
  ): integer; stdcall; external 'ntdll.dll';

function  LdrUnloadDll(
    ModuleHandle: Cardinal
  ): Integer; stdcall; external 'ntdll.dll';

function  NtQueryInformationProcess(
    ProcessHandle : Cardinal;
    ProcessInformationClass : Byte;
    ProcessInformation : Pointer;
    ProcessInformationLength : Cardinal;
    ReturnLength : PCardinal
  ): Integer; stdcall; external 'ntdll.dll';

function RtlNtStatusToDosError(const Status : Integer
  ): Cardinal; stdcall; external 'ntdll.dll';


var
  status: Integer;
  error_string: string;

function PidProcesso(NomeProcesso: string): LongWord;
var
  pe: TProcessEntry32;
  hSnap: THandle;
begin
  Result := 0;

  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);

  pe.dwSize := sizeof(TProcessEntry32);

  //Prelevo informazioni sul primo processo nello snapshot di sistema
  Process32First(hSnap, pe);
  repeat  //loop sui processi
    if (LowerCase(pe.szExeFile) = LowerCase(NomeProcesso)) then
      begin
        Result := pe.th32ProcessID;
        break;
      end;
  until (not (Process32Next(hSnap, pe)) ) ;

  CloseHandle(hSnap);

end;

function IntToHex(dwValue, dwDigits: Cardinal): String; stdcall;
const
  hex: array[0..$F] of char = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
begin
  if (dwDigits > 8) then
    dwDigits := 8;
  Result := Copy(
       hex[(dwValue and $F0000000) shr 28]+
       hex[(dwValue and $0F000000) shr 24]+
       hex[(dwValue and $00F00000) shr 20]+
       hex[(dwValue and $000F0000) shr 16]+
       hex[(dwValue and $0000F000) shr 12]+
       hex[(dwValue and $00000F00) shr 8]+
       hex[(dwValue and $000000F0) shr 4]+
       hex[(dwValue and $0000000F) shr 0],9-dwDigits,dwDigits);
end;

function ErrStrNative(nomeFunc: string; status: Integer): Boolean;
var
  error_code: Cardinal;
  error_description: string;
begin
  result := False;
  error_string := nomeFunc + ': ' +
                  'NTSTATUSCODErr=' + IntToHex(status,8);
  error_code := RtlNtStatusToDosError(status);
  error_description := SysErrorMessage(error_code);
  error_string := error_string +
                  '; CODErr=' + IntToStr(error_code) +
                  '; Descr=' + error_description;
  result := True;
end;

function ErrStr(nomeFunc: string): Boolean;
var
  error_code: Cardinal;
  error_description: string;
begin
  result := False;
  error_code := GetLastError;
  error_description := SysErrorMessage(error_code);
  error_string := nomeFunc + ': ' +
                  'CODErr=' + IntToStr(error_code) +
                  ' Descr=' + error_description;

  result := True;
end;

function ModificaPrivilegio(szPrivilege: pChar; fEnable: Boolean): Boolean;
var
  NewState: TTokenPrivileges;
  luid: TLargeInteger;
  hToken: Cardinal;
  ReturnLength: Cardinal;
begin

  Result := False;
  hToken := 0;

  try

    if not OpenThreadToken(
                           GetCurrentThread(),
                           TOKEN_ADJUST_PRIVILEGES,
                           False,
                           hToken) then
      begin
        if GetLastError = ERROR_NO_TOKEN then
          begin //non ho ottenuto il token asociato al thread e allora provo col processo
            if not OpenProcessToken(
                                    GetCurrentProcess(),
                                    TOKEN_ADJUST_PRIVILEGES,
                                    hToken) then
              begin
                ErrStr('OpenProcessExToken');
                Exit;
              end;
          end
        else
          begin
            ErrStr('OpenThreadToken');
            Exit;
          end;
      end;

    //ricavo il LUID (Locally Unique Identifier) corrispondente al privilegio
    //specificato: si tratta in sostanza di un identificativo univoco del privilegio;
    //varia da sessione a sessione ed anche tra un riavvio e l' altro del sistema
    if not LookupPrivilegeValue(nil, szPrivilege, luid) then
      begin
        ErrStr('LookupPrivilegeValue');
        Exit;
      end;

    //lavoro su NewState (di tipo TTokenPrivileges). Rappresenta un elenco di privilegi;
    //nel caso specifico conterr?un solo privilegio (ProvilegeCount = 1). L' arrary
    //Privileges contiene oggetti con 2 campi: il luid del privilegio (Luid) ed
    //il livello di abilitazione del medesimo (Attributes)
    NewState.PrivilegeCount := 1;
    NewState.Privileges[0].Luid := luid;
    if fEnable then    //abilitiamo il privilegio
      NewState.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
    else //disabilitiamo il privilegio
      NewState.Privileges[0].Attributes := 0;

    //eseguiamo la modifica sullo stato di abilitazione del privilegio
    //nel contesto del token di accesso aperto
    if not AdjustTokenPrivileges(
                                hToken,
                                FALSE,
                                NewState,
                                sizeof(NewState),
                                nil,
                                ReturnLength) then
      begin
        ErrStr('AdjustTokenPrivileges');
        Exit;
      end;

    Result := True;

  finally

    //chiudo l' handle al token di accesso aperto
    if hToken <> 0 then
     begin
       if not CloseHandle(hToken) then
         begin
           ErrStr('CloseHandle');
         end;
     end;

  end;

end;

function OpenProcessEx(dwDesiredAccess: Cardinal; bInheritableHandle: LongBool; dwProcessId: Cardinal): Cardinal;
var
  hProcess: Cardinal;
begin
  hProcess := OpenProcess(dwDesiredAccess, bInheritableHandle, dwProcessId); //provo ad ottenere un handle al processo
  if hProcess = 0 then //se non ci riesco provo ad assegnare il privilegio di debug
    begin
      if ModificaPrivilegio('SeDebugPrivilege', True) then
        begin
          hProcess := OpenProcess(dwDesiredAccess, bInheritableHandle, dwProcessId);
        end;
    end;
  result := hProcess;
end;

//N.B. il PEB si trova sempre all'indirizzo 7ffdf000
function GetPEBptr(
                   hProcess: Cardinal; //handle del processo
                   var addr: Cardinal //indirizzo del PEB
                   ): Boolean;
type
  TProcessBasicInformation = record
    ExitStatus: Integer;
    PebBaseAddress: Cardinal;
    AffinityMask: Integer;
    BasePriority: Integer;
    UniqueProcessID: Integer;
    InheritedFromUniqueProcessID: Integer;
  end;
var
  ProcInfo: TProcessBasicInformation;
  status: Integer;
begin
  result := False;
  //prelevo le informazioni di base relative al processo
  status := NtQueryInformationProcess(hProcess,
                                      0,
                                      @ProcInfo,
                                      SizeOf(TProcessBasicInformation),
                                      nil);
  if status <> 0 then
    begin
      ErrStrNative('NtQueryInformationProcess', status);
      Exit;
    end;
  addr := ProcInfo.PebBaseAddress;
  Result := True;
end;

function GetPebLdrData(
                       hProcess: Cardinal; //handle del processo
                       var addr: Cardinal //indirizzo del Loader Data
                       ): Boolean;
var
  PEBptr: Cardinal;
  BytesRead: dword;
begin
  Result := False;
  if not GetPEBptr(hProcess, PEBptr) then
    begin
      Exit;
    end;
  if not ReadProcessMemory(
                           hProcess,
                           pointer(PEBptr + 12),
                           @addr,
                           4,
                           BytesRead
                           ) then
    begin
      ErrStr('ReadProcessMemory');
      Exit;
    end;
  result := True;
end;

function RemoteGetModuleHandle(
               hProcess: Cardinal; //handle al processo
               nomeModulo: PAnsiChar; //nome del modulo
               var BaseAddr: Cardinal //indirizzo di base
               ): Boolean;
var
  pPeb_ldr_data: Cardinal;
  LdrData: PEB_LDR_DATA;
  LdrModule: LDR_MODULE;
  dwSize: Cardinal;
  readAddr, readAddrHead: Pointer;
  loadCount: SHORT;
  BaseDllName: PWideChar;

  cnt: Cardinal;
  Offset: Byte;

begin

  result := False;

  //verifica sui valori dei parametri
  if (hProcess = 0) or
     (PAnsiChar(NomeModulo) = nil) then
    begin
      Exit;
    end;

  if not GetPebLdrData(
              hProcess,
              pPeb_ldr_data
              ) then
    begin
      Exit;
    end;

	//prelevo PEB_LDR_DATA
	if not ReadProcessMemory(
             hProcess,
             Pointer(pPeb_ldr_data),
             @LdrData,
             sizeof(LdrData),
             dwSize
             ) then
    begin
      ErrStr('ReadProcessMemory');
      Exit;
    end;

  readAddrHead := Pointer(pPeb_ldr_data + 12);
  readAddr := Pointer(LdrData.InLoadOrderModuleList.Flink);

  cnt := 0;
  repeat
    Inc(cnt);

    //
    readAddr := Pointer(Cardinal(readAddr));

    //estraggo il modulo
    if not ReadProcessMemory(
                       hProcess,
                       readAddr,
                       @LdrModule,
                       sizeof(LdrModule),
                       dwSize
                       ) then
      begin
        ErrStr('ReadProcessMemory');
        Exit;
      end;

    //leggo il nome della DLL
    GetMem(BaseDllName, LdrModule.BaseDllName.MaximumLength);
		if ReadProcessMemory(
            hProcess,
            LdrModule.BaseDllName.Buffer,
            BaseDllName,
            LdrModule.BaseDllName.MaximumLength,
            dwSize
            ) then
      begin
        if lstrcmpiA(
                  nomeModulo,
                  PAnsiChar(WideCharToString(BaseDllName))
                  ) = 0 then
          begin
            BaseAddr := LdrModule.DllBase;
            Result := True;
            Break;
          end;
      end;

    FreeMem(BaseDllName);

		//* passo al successivo LDR_MODULE */
    readAddr := Pointer(LdrModule.InLoadOrderLinks.Flink);
    //
	until readAddr = readAddrHead;

end;

function RemoteGetProcAddress(
             //handle al processo
             hProcess: Cardinal;
             //Indirizzo di base del modulo (ottenuto con RemoteGetModuleHandle)
             hModule: Cardinal;
             //NomeModulo: string;
             //NomeFunzione: string;
             //puntatore ad una stringa che definisce il nome della funzione
             ProcName: PAnsiChar;
             var FuncAddress: Cardinal): Boolean;
var
  DosHeader          : TImageDosHeader;  //Dos Header
  NtHeaders           : TImageNtHeaders; //PE Header
  DataDirectory      : TImageDataDirectory;  //Data Directory
  ExportDirectory    : TImageExportDirectory; //Export Directory
  BytesRead: Cardinal; //var per ReadProcessMemory

  //estremo inferiore e superiore della Export Directory
  ExportDataDirectoryLow, ExportDataDirectoryHigh: cardinal;

  //valore Base di IMAGE_EXPORT_DIRECTORY: valore base degli Ordinal
  BaseOrdinal: Cardinal;

  //
  NumberOfFunctions: Cardinal;
  NumberOfNames: Cardinal;

  //puntatori ai 3 array
  First_AddressOfFunctions: Cardinal;
  First_AddressOfNames: Cardinal;
  First_AddressOfNameOrdinals: Cardinal;

  Actual_AddressOfFunctions: Cardinal;
  Actual_AddressOfNames: Cardinal;
  Actual_AddressOfNameOrdinals: Cardinal;
  //

  //indice della funzione nell'array puntato da
  //IMAGE_EXPORT_DIRECTORY.AddressOfFunctions:
  //l'elemento dell'array che si trova in questa
  //posizione contiene l'RVA della funzione
  FunctionIndex: Cardinal;

  //RVA presente in un elemento dell'array puntato
  //da IMAGE_EXPORT_DIRECTORY.AddressOfNames
  FunctionNameRVA: Cardinal;

  //nome puntato dall'RVA presente in un
  //elemento dell'array puntato da
  //IMAGE_EXPORT_DIRECTORY.AddressOfNames
  FunctionName: PAnsiChar;

  FunctionNameFound: Boolean;

  //RVA della funzione che cerchiamo: ?presente
  //in un elemento dell'array puntato da
  //IMAGE_EXPORT_DIRECTORY.AddressOfFunctions
  FunctionRVA: Cardinal;

  //Forwarding
  FunctionForwardName: PAnsiChar;

  ForwardPuntoPos: Cardinal;

  ExportForward: Boolean;

  FunctionForward_ModuleName: string;
  FunctionForward_FunctionName: string;
  FunctionForward_FunctionOrdinal: Word;
  FunctionForwardByOrdinal: Boolean;
  FunctionForward: Pointer;

  FunctionForward_ModuleBaseAddr: Cardinal;
  FunctionForward_FunctionAddr: Cardinal;
  //

  i: Cardinal;

begin

  Result := False;

  //verifica sui valori dei parametri
  if (hProcess = 0) or
     (ProcName = nil) then
    begin
      Exit;
    end;

  try
    //prelevo il Dos Header
    if not ReadProcessMemory(
               hProcess,
               Pointer(hModule),
               @DosHeader,
               sizeof(DosHeader),
               BytesRead
               ) then
      begin
        ErrStr('ReadProcessMemory');
        Exit;
      end;
    //verifica della validit?
    if (DosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
      Exit;

    //prelevo il PE Header
    if not ReadProcessMemory(
               hProcess,
               Pointer(hModule + DosHeader._lfanew),
               @NtHeaders,
               sizeof(NtHeaders),
               BytesRead
               ) then
      begin
        ErrStr('ReadProcessMemory');
        Exit;
      end;
    //verifica della validit?
    if (NtHeaders.Signature <> IMAGE_NT_SIGNATURE) then
      Exit;

    //se il modulo non ha la directory di Export allora esco
    //valuto l' RVA della directory di export
    if NTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress = 0 then
      Exit;

    //calcolo il range di definizione della Export Directory
    //mi servir?per valutare se l'RVA di una funzione punta alla definizione
    //della funzione (valore esterno all'intervallo) oppure punta ad una stringa
    //del tipo <nome_dll>.<nome_funzione> (valore interno all'intervallo)
    with NTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT] do
      begin
        ExportDataDirectoryLow := VirtualAddress;
        ExportDataDirectoryHigh := VirtualAddress + Size;
      end;

    //prelevo la Export Directory
    if not ReadProcessMemory(
               hProcess,
               Pointer(hModule + ExportDataDirectoryLow),
               @ExportDirectory,
               sizeof(ExportDirectory),
               BytesRead
               ) then
      begin
        ErrStr('ReadProcessMemory');
        Exit;
      end;

    //Determino il valore base degli Ordinal
    BaseOrdinal := ExportDirectory.Base;

    //
    NumberOfFunctions := ExportDirectory.NumberOfFunctions;
    NumberOfNames := ExportDirectory.NumberOfNames;

    First_AddressOfFunctions := hModule +
                 Cardinal(ExportDirectory.AddressOfFunctions);
    First_AddressOfNames := hModule +
                 Cardinal(ExportDirectory.AddressOfNames);
    First_AddressOfNameOrdinals := hModule +
                 Cardinal(ExportDirectory.AddressOfNameOrdinals);

    //allocazione di memoria per puntatori
    //che riceveranno stringhe di caratteri
    GetMem(FunctionName, 100);
    GetMem(FunctionForwardName, 100);

    FunctionIndex := 0;
    if Cardinal(ProcName) <= $FFFF then
    //ho passato l'Ordinal della funzione
      begin
        FunctionIndex := Cardinal(ProcName) - BaseOrdinal;
        //verifico di aver passato un Ordinal valido
        if (FunctionIndex < 0) or
           (FunctionIndex > NumberOfFunctions) then
          Exit;
      end
    else
      //ho passato il puntatore ad una stringa
      //che rappresenta il nome della funzione
      begin
        //scanno l'array puntato da IMAGE_EXPORT_DIRECTORY.AddressOfNames
        //che contiene i nomi delle funzioni esportate con associato un nome;
        //ogni elemento dell'array ?4 byte (?infatti l'RVA del nome della funzione)
        FunctionNameFound := False;
        for i := 0 to NumberOfNames - 1 do {for each export do}
          begin
            Actual_AddressOfNames := First_AddressOfNames + SizeOf(Cardinal) * i;

            //prelevo l'RVA del nome: FunctionNameRVA
            ReadProcessMemory(
                hProcess,
                Pointer(Actual_AddressOfNames) ,
                @FunctionNameRVA,
                4,
                BytesRead);

            //prelevo il nome: FunctionName
            ReadProcessMemory(
                hProcess,
                Pointer(hModule + FunctionNameRVA) ,
                FunctionName,
                100,
                BytesRead
                );

            //vado a vedere se il nome che ho
            //trovato equivale a quello che cerco
            if lstrcmpiA(FunctionName, ProcName) = 0 then
              begin
                //vado nell'array puntato da IMAGE_EXPORT_DIRECTORY.AddressOfNameOrdinals:
                //l'elemento di posizione i contiene l'indice della funzione nell'array
                //puntato da IMAGE_EXPORT_DIRECTORY.AddressOfFunctions
                Actual_AddressOfNameOrdinals := First_AddressOfNameOrdinals + SizeOf(Word) * i;

                //prelevo l'indice: FunctionIndex
                ReadProcessMemory(
                    hProcess,
                    Pointer(Actual_AddressOfNameOrdinals) ,
                    @FunctionIndex,
                    2,
                    BytesRead
                    );
                FunctionNameFound := True;
                Break;
              end;
          end;
        //verifico di aver trovato il nome specificato tra i nomi di funzione
        //in caso contrario esco
        if not FunctionNameFound then
          Exit;
      end;

    //Perfetto ho ottenuto l'indice nell'array puntato
    //da IMAGE_EXPORT_DIRECTORY.AddressOfFunctions; a questo
    //punto posso prelevare l'RVA della funzione

    Actual_AddressOfFunctions := First_AddressOfFunctions + SizeOf(Cardinal) * FunctionIndex;

    //prelevo l'RVA
    ReadProcessMemory(
        hProcess,
        Pointer(Actual_AddressOfFunctions) ,
        @FunctionRVA,
        4,
        BytesRead
        );

    //Bene: ho l'RVA della funzione; devo ora vedere se
    //rientra nella Export Directory: in tal caso punta
    //ad una stringa del tipo <nome_dll>.<nome_funzione> o
    //<nome_dll>.#Ordinal ossia si ?di fronte ad un
    //Forwarding e dovremo chiamare RemoteGetProcAddress
    //su questi nuovi valori

    if (FunctionRVA > ExportDataDirectoryLow) and
       (FunctionRVA <= ExportDataDirectoryHigh) then
      //questo ?un forwarding
      begin
        //prelevo la stringa modello <nome_dll>.<nome_funzione> o
        //<nome_dll>.#Ordinal
        ReadProcessMemory(
            hProcess,
            Pointer(hModule + FunctionRVA) ,
            FunctionForwardName,
            100,
            BytesRead
            );

        //estraggo nome del modulo e della funzione
        ForwardPuntoPos := Pos('.', FunctionForwardName);

        if (ForwardPuntoPos > 0) then
          begin
            FunctionForward_ModuleName := Copy(
                                            FunctionForwardName,
                                            1,
                                            ForwardPuntoPos - 1
                                            ) + '.dll';
            FunctionForward_FunctionName := Copy(
                                              FunctionForwardName,
                                              ForwardPuntoPos + 1,
                                              Length(FunctionForwardName)
                                              );
            //Vado a vedere se FunctionForward_FunctionName ?del tipo #Ordinal
            //in tal caso significa che la funzione a cui si punta ?definita
            //tramite il suo Ordinal
            FunctionForwardByOrdinal := False;
            if string(FunctionForward_FunctionName)[1] = '#' then
              begin
                FunctionForwardByOrdinal := True;
                FunctionForward_FunctionOrdinal := StrToInt(Copy(
                                         FunctionForward_FunctionName,
                                         2,
                                         Length(FunctionForward_FunctionName)
                                         ));
              end;

            //vado a rieseguire GetRemoteProcAddress sui valori di Forwarding

            //prima di tutto calcolo il base address del nuovo modulo:
            //deve esistere per forza, per?non si sa mai, sempre meglio
            //gestire il caso in cui la funzione RemoteGetModuleHandle
            //per qualche motivo fallisce
            if not RemoteGetModuleHandle(
                         hProcess,
                         PAnsiChar(FunctionForward_ModuleName),
                         FunctionForward_ModuleBaseAddr) then
              Exit;

            //una volta trovato il base address del modulo, chiamo
            //RemoteGetProcAddress
            if FunctionForwardByOrdinal then
              //<nome_dll>.#Ordinal
              FunctionForward := Pointer(FunctionForward_FunctionOrdinal)
            else
              //<nome_dll>.<nome_funzione>
              FunctionForward := PAnsiChar(FunctionForward_FunctionName);

            if not RemoteGetProcAddress(
                         hProcess,
                         FunctionForward_ModuleBaseAddr,
                         FunctionForward,
                         FunctionForward_FunctionAddr
                         ) then
              Exit;

            //se tutto ?andato OK
            FuncAddress := FunctionForward_FunctionAddr;

          end;
      end
    else  //non si tratta di un Forwarding
      begin
        //sommo all'RVA della funzione il Base Address del modulo:
        //in questo modo ottengo il Virtual Address della funzione
        //ossia il risultato finale
        FuncAddress := hModule + FunctionRVA;

      end;

    Result := True;

  finally
    if FunctionName <> nil then
      FreeMem(Pointer(FunctionName));
    if FunctionForwardName <> nil then
      FreeMem(Pointer(FunctionForwardName));
  end;
end;

end.