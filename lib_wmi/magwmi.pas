unit magwmi;

{
Magenta Systems WMI and SMART Component v5.0
Updated by Angus Robertson, Magenta Systems Ltd, England, 22nd October 2005
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright 2005, Magenta Systems Ltd

partly based on articles in Delphi Developers Magazine by Craig Murphy and work by
Denis Blondeau (both needed the same bug fixes to read variant arrays properly)

note - WbemScripting_TLB was created by importing the Microsoft WMI Scripting type library

This component contains WMI, SMART and SCSI Pass Through functions, of particular use
for getting hard disk information and configuring network adaptors, but also for many
other general uses.   

24 Nov 2003 - Release 4.93
26 Nov 2003 - removed some duplicate subs
1 Dec 2003  - added MagWmiGetOSInfo and MagWmiGetProcInfo
19 Dec 2003 - added MagWmiNetSetDHCP
2 Jan 2004  - using common subs and types from magsubs1
10 Jan 2004 - added SMART stuff to read hard disk serial which WMI only does for XP and later
15 Jul 2004 - use SMART stuff to get disk failure info and attributes
14 Oct 2004 - added MagWmiScsiDiskInfo seems to work with firewire drives SMART dislikes (but not USB)
21 Oct 2004 - more error handling in MagWmiSmartDiskFail looking for range error, AttrRawValue now Int64
9  Jan 2005 - added MagWmiCloseWin to close down windows
14 Jan 2005 - added MagWmiGetMemory


}


{ an alternate name space has other stuff:

root\wmi
MSNdis_HardwareStatus
MSNdis_80211_TransmitPowerLevel
MSNdis_80211_ReceivedSignalStrength
MSNdis_MediaConnectStatus
MSTapeDriveParam
MSRedbook_DriverInformation
MSSerial_PortName
MSStorageDriver_FailurePredictStatus
MSStorageDriver_ATAPISmartData
}

interface

uses
  Windows, Messages, SysUtils, Classes, WbemScripting_TLB, magsubs1, smartapi ;


const
    RootNameSpace = 'root\CIMV2' ;
    MaxSmartAttr = NUM_ATTRIBUTE_STRUCTS + 1 ;

type
    TSmartResult = Record
        DriveNum: integer ;
        CapacityNum: int64 ;
        VendorUnique: array [1..3] of USHORT ;
        SerialNumber: string ;
        FirmwareRev: string ;
        ModelNumber: string ;
        MoreVendorUnique: integer ;
        Temperature: integer ;
        TempWorst: integer ;
        TempLow: integer ;
        HoursRunning: integer ;
        ReallocSector: integer ;
        PowerCycles: integer ;
        SmartFailTot: integer ;
        SmartWarnTot: integer ;
        TotalAttrs: integer ;
        AttrNum: array [0..MaxSmartAttr] of integer ;
        AttrName: array [0..MaxSmartAttr] of string ;
        AttrPreFail: array [0..MaxSmartAttr] of boolean ;
        AttrEvents: array [0..MaxSmartAttr] of boolean ;
        AttrErrorRate: array [0..MaxSmartAttr] of boolean ;
        AttrCurValue: array [0..MaxSmartAttr] of integer ;
        AttrWorstVal: array [0..MaxSmartAttr] of integer ;
        AttrThreshold: array [0..MaxSmartAttr] of integer ;
        AttrRawValue: array [0..MaxSmartAttr] of Int64 ;   // 21 Oct 2004, was int
        AttrState: array [0..MaxSmartAttr] of string ;
    end ;

    TWmiMemoryRec = Record
        FreePhysicalMemory: Int64 ;
        FreeSpaceInPagingFiles: Int64 ;
        FreeVirtualMemory: Int64 ;
        SizeStoredInPagingFiles: Int64 ;
        TotalSwapSpaceSize: Int64 ;
        TotalVirtualMemorySize: Int64 ;
        TotalVisibleMemorySize: Int64 ;
    end ;

    function MagWmiDate2DT (S: string; var UtcOffset: integer): TDateTime ;
    function MagWmiGetPropStr (wmiProp: ISWbemProperty): string ;
    function MagWmiGetInfo (const Comp, NameSpace, User, Pass, Arg: widestring ;
                       var WmiResults: T2DimStrArray; var instances: integer): integer ;
    function MagWmiGetOneG (const Arg, Prop: widestring ; var ResStr: string): integer ;
    function MagWmiGetOneQ (const Arg, Prop: widestring ; var ResStr: string): integer ;
    function MagWmiSearchIdx (const WmiResults: T2DimStrArray; const Prop: string): integer ;
    function MagWmiSearch1 (const WmiResults: T2DimStrArray; const Prop: string): string ;
    function MagWmiGetBaseBoard: string ;
    function MagWmiGetSMBIOS: string ;
    function MagWmiGetLastBootDT: TDateTime ;
    function MagWmiGetDiskSerial (drive: integer): string ;
    function MagWmiGetDiskModel (drive: integer): string ;
    function MagWmiFindAdaptor (var AdapterName: string): integer ;
    function MagWmiNetSetIPAddr (const AdapNum: integer; const IPAddresses,
                                                SubnetMasks: StringArray): integer ;
    function MagWmiNetSetGateway (const AdapNum: integer; const IPGateways: StringArray;
                                           const GatewayCosts: TIntegerArray): integer ;
    function MagWmiNetSetDHCP (const AdapNum: integer): integer ;
    function MagWmiRenameComp (const NewName, UserName, Password: string): integer ;
    function MagWmiGetOSInfo (item: string): string ;
    function MagWmiGetProcInfo (item: string): string ;
    function MagWmiSmartDiskInfo (drivenr: integer; var errinfo, model,
                                      serial: string ; var diskbytes: Int64): boolean ;
    function MagWmiSmartDiskFail (drivenr: integer; var SmartResult: TSmartResult ;
                                                     var errinfo: string): boolean ;
    function MagWmiScsiDiskInfo (drivenr: integer; var errinfo, model,
                                    serial: string ; var diskbytes: Int64): boolean ;
    function MagWmiCloseWin (const Comp, User, Pass: widestring ; reboot: boolean;
                                                var errinfo: string): integer ;
    function MagWmiGetMemory: TWmiMemoryRec ;


implementation

uses ActiveX, ComObj, Variants;

(* ************************************************************************* *)

function MagWmiDate2DT (S: string; var UtcOffset: integer): TDateTime ;
// yyyymmddhhnnss.zzzzzzsUUU  +60 means 60 mins of UTC time
// 20030709091030.686000+060
// 1234567890123456789012345
var
    yy, mm, dd, hh, nn, ss, zz: integer ;
    timeDT: TDateTime ;

    function GetNum (offset, len: integer): integer ;
    var
        E: Integer;
    begin
        Val (copy (S, offset, len), result, E) ;
    end ;

begin
    result := 0 ;
    UtcOffset := 0 ;
    if length (S) <> 25 then exit ;   // fixed length
    yy := GetNum (1, 4) ;
    mm := GetNum (5, 2) ;
    if (mm = 0) or (mm > 12) then exit ;
    dd := GetNum (7, 2) ;
    if (dd = 0) or (dd > 31) then exit ;
    if NOT TryEncodeDate (yy, mm, dd, result) then     // D6 and later
    begin
        result := -1 ;
        exit ;
    end ;
  { try
        result := EncodeDate (yy, mm, dd) ;
    except
        result := -1 ;
        exit ;
    end ;    }
    hh := GetNum (9, 2) ;
    nn := GetNum (11, 2) ;
    ss := GetNum (13, 2) ;
    zz := 0 ;
    if Length (S) >= 18 then zz := GetNum (16, 3) ;
    if NOT TryEncodeTime (hh, nn, ss, zz, timeDT) then exit ;   // D6 and later
    result := result + timeDT ;
    UtcOffset := GetNum (22, 4) ; // including sign
 {   try
        result := result + EncodeTime (hh, nn, ss, zz) ;
    except
        result := -1 ;
        exit ;
    end ; }
end ;

(* ************************************************************************* *)

function MagWmiGetPropStr (wmiProp: ISWbemProperty): string ;
var
    I: integer ;
begin
    result := '';
    if VarIsNull(wmiProp.Get_Value) then
        result := 'NULL'
    else
    begin
        case wmiProp.CIMType of
            wbemCimtypeSint8, wbemCimtypeUint8, wbemCimtypeSint16,
            wbemCimtypeUint16, wbemCimtypeSint32, wbemCimtypeUint32,
            wbemCimtypeSint64:
                if VarIsArray(wmiProp.Get_Value) then
                begin
                    for I := 0 to VarArrayHighBound (wmiProp.Get_Value, 1) do
                    begin
                        if I > 0 then result := result + '|' ;
                        result := result + IntToStr (wmiProp.Get_Value [I]) ;
                    end ;
                end
                else
                    result := IntToStr (wmiProp.Get_Value);

            wbemCimtypeReal32, wbemCimtypeReal64:
                result := FloatToStr (wmiProp.Get_Value);

            wbemCimtypeBoolean:
                if wmiProp.Get_Value then result := 'True' else result := 'False';

            wbemCimtypeString, wbemCimtypeUint64:
                if VarIsArray(wmiProp.Get_Value) then
                begin
                    for I := 0 to VarArrayHighBound (wmiProp.Get_Value, 1) do
                    begin
                        if I > 0 then result := result + '|' ;
                        result := result + wmiProp.Get_Value [I] ;
                    end ;
                end
                else
                    result := wmiProp.Get_Value;

            wbemCimtypeDatetime:
                result := wmiProp.Get_Value;

            wbemCimtypeReference:
            begin
                result := wmiProp.Get_Value ;
            // Services.Get(result, 0, nil).GetObjectText_(0));  another query
            end;

            wbemCimtypeChar16:
                result := '<16-bit character>';

            wbemCimtypeObject:
                result := '<CIM Object>';
        end ;
    end;
end ;

(* ************************************************************************* *)

// Comp may be blank for local computer, user and pass optional
// results returned in two dimensioned array, properties as instance/column 0
// Instances is 1 or greater, result is number of rows, 0 for none, -1 for error
// The size of the dynamic array may also be checked with Low/High

function MagWmiGetInfo (const Comp, NameSpace, User, Pass, Arg: widestring ;
                       var WmiResults: T2DimStrArray; var Instances: integer): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject: ISWbemObject;
    propSet: ISWbemPropertySet;
    wmiProp: ISWbemProperty;
    propEnum, Enum: IEnumVariant;
    ovVar: OleVariant;
    lwValue: LongWord;
    sValue: String;
    inst, row: integer ;
    dimmed: boolean ;
begin
    result := 0 ;
    Instances := 0 ;
    SetLength (WmiResults, 0, 0) ;
    dimmed := false ;
    wmiLocator := TSWbemLocator.Create (Nil) ;
    try
    try
        wmiServices := wmiLocator.ConnectServer (Comp, Namespace, User, Pass,
                                                                        '', '', 0, nil) ;
        if Pos ('SELECT', Arg) = 1 then
            wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL', wbemFlagReturnImmediately, nil)
        else
            wmiObjectSet := wmiServices.InstancesOf (Arg, wbemFlagReturnImmediately or
                                                             wbemQueryFlagShallow, nil) ;
        Instances := wmiObjectSet.Count ;
        if Instances = 0 then exit ;

        // Replicate VBScript's "for each" construct
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        inst := 0 ;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            propSet := wmiObject.Properties_;
            result := propSet.Count ;
            if NOT dimmed then
            begin
                SetLength (WmiResults, Instances + 1, result + 1) ;
                WmiResults [0, 0] := 'Instance' ;
                dimmed := true ;
            end ;
            propEnum := (propSet._NewEnum) as IEnumVariant;
            inc (inst) ;
            row := 1 ;
            WmiResults [inst, 0] := IntToStr (inst) ;

       // Replicate VBScript's "for each" construct
            while (propEnum.Next (1, ovVar, lwValue) = S_OK) do
            begin
                wmiProp := IUnknown(ovVar) as SWBemProperty;
                sValue := MagWmiGetPropStr (wmiProp) ;
                if inst = 1 then WmiResults [0, row] := wmiProp.Name ;
                WmiResults [inst, row] := sValue ;
                inc (row) ;
            end;
        end;
    except
        result := -1 ;
    end ;
    finally
        wmiLocator.Free;
    end;
end;

(* ************************************************************************* *)

// get a single property, argument is Get which seems very unprectictable

function MagWmiGetOneG (const Arg, Prop: widestring ; var ResStr: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObject: ISWbemObject;
    wmiProp: ISWbemProperty;
begin
    result := 0 ;
    ResStr := '' ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                        '', '', 0, nil);
        wmiObject := wmiServices.Get (Arg, 0, Nil) ;
        wmiProp := wmiObject.Properties_.Item (Prop, 0) ;
        if wmiProp.Name <> Prop then exit ;
        ResStr := MagWmiGetPropStr (wmiProp) ;
        if ResStr <> 'NULL' then result := 1 ;
    except
        result := -1 ;
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// get a single property, argument is a WQL query, lengthy, but reliable
// fails if more than one instance returned by query

function MagWmiGetOneQ (const Arg, Prop: widestring ; var ResStr: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar: OleVariant;
    lwValue: LongWord;
    Enum: IEnumVariant;
begin
    ResStr := '' ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        result := wmiObjectSet.Count ;
        if (result <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiProp := wmiObject.Properties_.Item (Prop, 0) ;
            if wmiProp.Name <> Prop then exit ;
            ResStr := MagWmiGetPropStr (wmiProp) ;
            if ResStr <> 'NULL' then result := 1 ;
        end ;
    except
        result := -1 ;
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

function MagWmiSearchIdx (const WmiResults: T2DimStrArray; const Prop: string): integer ;
var
    I: integer ;
begin
    result := 0 ;
    for I := 1 to High (WmiResults [0]) do
    begin
        if WmiResults [0, I] = Prop then
        begin
            result := I ;
            exit ;
        end ;
    end ;
end ;

(* ************************************************************************* *)

function MagWmiSearch1 (const WmiResults: T2DimStrArray; const Prop: string): string ;
var
    I: integer ;
begin
    result := '' ;
    I := MagWmiSearchIdx (WmiResults, Prop) ;
    if I >= 1 then result := WmiResults [1, I] ;
end ;

(* ************************************************************************* *)

function MagWmiGetBaseBoard: string ;
var
    rows, instances: integer ;
    WmiResults: T2DimStrArray ;
begin
    result := '' ;
    try
        rows := MagWmiGetInfo ('', RootNameSpace, '', '',
            'SELECT Manufacturer, Product FROM Win32_BaseBoard', WmiResults, instances) ;
        if (instances = 1) and (rows > 1) then
            result := MagWmiSearch1 (WmiResults, 'Manufacturer') + ' ' +
                                                MagWmiSearch1 (WmiResults, 'Product') ;
    finally
        WmiResults := Nil ;
    end ;
end ;

(* ************************************************************************* *)

function MagWmiGetSMBIOS: string ;
var
    rows, instances: integer ;
    WmiResults: T2DimStrArray ;
begin
    result := '' ;
    try
        rows := MagWmiGetInfo ('', RootNameSpace, '', '', 'Win32_BIOS',
                                                                 WmiResults, instances) ;
        if (instances = 1) and (rows > 1) then
            result := MagWmiSearch1 (WmiResults, 'SMBIOSBIOSVersion') + ' v' +
                        MagWmiSearch1 (WmiResults, 'SMBIOSMajorVersion') + '.' +
                               MagWmiSearch1 (WmiResults, 'SMBIOSMinorVersion') ;
    finally
        WmiResults := Nil ;
    end ;
end ;

(* ************************************************************************* *)

function MagWmiGetLastBootDT: TDateTime ;
var
    rawdate: string ;
    utcoffset: integer ;
begin
    result := 0 ;
    if MagWmiGetOneQ ('SELECT LastBootUpTime FROM Win32_OperatingSystem',
                                         'LastBootUpTime', rawdate) <> 1 then exit ;
    result := MagWmiDate2DT (rawdate, utcoffset) ;
end ;

(* ************************************************************************* *)

// XP and W2K3 only !!!

function MagWmiGetDiskSerial (drive: integer): string ;
begin
    result := '' ;
    MagWmiGetOneQ ('SELECT SerialNumber FROM Win32_PhysicalMedia WHERE ' +
       'Tag = "\\\\.\\PHYSICALDRIVE' + IntToStr (drive) + '"', 'SerialNumber', Result) ;
end ;

(* ************************************************************************* *)

function MagWmiGetDiskModel (drive: integer): string ;
begin
    result := '' ;
    MagWmiGetOneQ ('SELECT Model FROM Win32_DiskDrive WHERE ' +
       'Name = "\\\\.\\PHYSICALDRIVE' + IntToStr (drive) + '"', 'Model', Result) ;
end ;

(* ************************************************************************* *)

function MagWmiGetProcInfo (item: string): string ;
begin
    result := '' ;
    MagWmiGetOneQ ('SELECT ' + item + ' FROM Win32_Processor', item, Result) ;
end ;

(* ************************************************************************* *)

function MagWmiGetOSInfo (item: string): string ;
begin
    result := '' ;
    MagWmiGetOneQ ('SELECT ' + item + ' FROM Win32_OperatingSystem', item, Result) ;
end ;

(* ************************************************************************* *)

// find the name and index of a unique Ethernet 802.3 adaptor (except 1394 Net Adapter)
// there may be other hidden adapters but they have null AdapterTypes (luckily)

function MagWmiFindAdaptor (var AdapterName: string): integer ;
var
    I, rows, instances: integer ;
    WmiResults: T2DimStrArray ;
begin
    result := -1 ;
    AdapterName := '' ;
    try
        rows := MagWmiGetInfo ('', RootNameSpace, '', '',
            'SELECT Name, Index FROM Win32_NetworkAdapter ' +
               'WHERE AdapterType = "Ethernet 802.3" AND Name <> "1394 Net Adapter"',
                                                                 WmiResults, instances) ;
        if (instances = 1) and (rows > 1) then
        begin
            I := MagWmiSearchIdx (WmiResults, 'Name') ;
            if I >= 1 then AdapterName := WmiResults [1, I] ;
            I := MagWmiSearchIdx (WmiResults, 'Index') ;
            if I >= 1 then result := AscToInt (WmiResults [1, I]) ;
        end ;
    finally
        WmiResults := Nil ;
    end ;
end ;

(* ************************************************************************* *)

// change network adaptor static IP addresses and masks
// this disables DHCP, multiple IP addresses may be supplied, with matching masks

function MagWmiNetSetIPAddr (const AdapNum: integer; const IPAddresses,
                                                SubnetMasks: StringArray): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiInst, wmiInParams, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar, propValue: OleVariant;
    Enum: IEnumVariant;
    wmiMethod: SWbemMethod;
    wmiProperty: SWbemProperty;
    Arg: widestring ;
    lwValue: LongWord;
    ArrayValue: Variant;
begin
    Result := -1 ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        Arg := 'SELECT * FROM Win32_NetworkAdapterConfiguration WHERE Index = "' +
                                                             IntToStr (AdapNum) + '"' ;
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        if (wmiObjectSet.Count <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('EnableStatic', 0) ;
            wmiInParams := wmiMethod.InParameters ;
            wmiInst := wmiInParams.SpawnInstance_(0) ;
            wmiProperty := wmiInst.Properties_.Add ('IPAddress', wbemCimtypeString, True, 0) ;
            DynArrayToVariant (ArrayValue, IPAddresses, TypeInfo (StringArray)) ;
            propValue := ArrayValue ;
            wmiProperty.Set_Value (propValue);
            wmiProperty := wmiInst.Properties_.Add ('SubnetMask', wbemCimtypeString, True, 0) ;
            DynArrayToVariant (ArrayValue, SubnetMasks, TypeInfo (StringArray)) ;
            propValue := ArrayValue ;
            wmiProperty.Set_Value (propValue);
            wmiOutParams := wmiObject.ExecMethod_ ('EnableStatic', wmiInst, 0, nil) ;
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            result := wmiProp.Get_Value ;
         // 0=OK no reboot, 1=ok reboot, 64 to 100 various errors, see MSDN
         // 68 = bad input parameter, 84=IP not enabled (ie wrong adaptor)
            exit ;
        end ;
    except
        result := -1 ;
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// change network adaptor static gateway IP addresses and metric
// this only works if DHCP is disabled, multiple IP addresses may be supplied

function MagWmiNetSetGateway (const AdapNum: integer; const IPGateways: StringArray;
                                          const GatewayCosts: TIntegerArray): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiInst, wmiInParams, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar, propValue: OleVariant;
    Enum: IEnumVariant;
    wmiMethod: SWbemMethod;
    wmiProperty: SWbemProperty;
    Arg: widestring ;
    lwValue: LongWord;
    ArrayValue: Variant;
begin
    Result := -1 ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        Arg := 'SELECT * FROM Win32_NetworkAdapterConfiguration WHERE Index = "' +
                                                             IntToStr (AdapNum) + '"' ;
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        if (wmiObjectSet.Count <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('SetGateways', 0) ;
            wmiInParams := wmiMethod.InParameters ;
            wmiInst := wmiInParams.SpawnInstance_(0) ;
            wmiProperty := wmiInst.Properties_.Add ('DefaultIPGateway', wbemCimtypeString, True, 0) ;
            DynArrayToVariant (ArrayValue, IPGateways, TypeInfo (StringArray)) ;
            propValue := ArrayValue ;
            wmiProperty.Set_Value (propValue);
            wmiProperty := wmiInst.Properties_.Add ('GatewayCostMetric', wbemCimtypeUint16, True, 0) ;
            DynArrayToVariant (ArrayValue, GatewayCosts, TypeInfo (TIntegerArray)) ;
            propValue := ArrayValue ;
            wmiProperty.Set_Value (propValue);
            wmiOutParams := wmiObject.ExecMethod_ ('SetGateways', wmiInst, 0, nil) ;
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            result := wmiProp.Get_Value ;
         // 0=OK no reboot, 1=ok reboot, 64 to 100 various errors, see MSDN
         // 68 = bad input parameter, 84=IP not enabled (ie wrong adaptor)
            exit ;
        end ;
    except
        result := -1 ;
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// change network adaptor to enable DHCP

function MagWmiNetSetDHCP (const AdapNum: integer): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar: OleVariant;
    Enum: IEnumVariant;
    wmiMethod: SWbemMethod;
    Arg: widestring ;
    lwValue: LongWord;
begin
    Result := -1 ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        Arg := 'SELECT * FROM Win32_NetworkAdapterConfiguration WHERE Index = "' +
                                                             IntToStr (AdapNum) + '"' ;
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        if (wmiObjectSet.Count <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('EnableDHCP', 0) ;
            wmiOutParams := wmiObject.ExecMethod_ ('EnableDHCP', Nil, 0, nil) ;
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            result := wmiProp.Get_Value ;
         // 0=OK no reboot, 1=ok reboot, 64 to 100 various errors, see MSDN
         // 68 = bad input parameter, 84=IP not enabled (ie wrong adaptor)
            exit ;
        end ;
    except
        result := -1 ;
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// rename Computer Name, always needs reboot - account info is for domain controller
// XP and W2K3 only

function MagWmiRenameComp (const NewName, UserName, Password: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiInst, wmiInParams, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar, propValue: OleVariant;
    Enum: IEnumVariant;
    wmiMethod: SWbemMethod;
    wmiProperty: SWbemProperty;
    lwValue: LongWord;
begin
    Result := -1 ;
    if NewName = '' then exit ;
 // should validate new name for no control chars, spaces, / \ [ ]
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        wmiObjectSet := wmiServices.InstancesOf ('Win32_ComputerSystem',
                             wbemFlagReturnImmediately or wbemQueryFlagShallow, nil) ;
        if (wmiObjectSet.Count <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('Rename', 0) ;
            wmiInParams := wmiMethod.InParameters ;
            wmiInst := wmiInParams.SpawnInstance_(0) ;
            wmiProperty := wmiInst.Properties_.Add ('Name', wbemCimtypeString, False, 0) ;
            propValue := NewName ;
            wmiProperty.Set_Value (propValue);
            if UserName <> '' then
            begin
                wmiProperty := wmiInst.Properties_.Add ('UserName', wbemCimtypeString, False, 0) ;
                propValue := UserName ;
                wmiProperty.Set_Value (propValue);
                wmiProperty := wmiInst.Properties_.Add ('Password', wbemCimtypeString, False, 0) ;
                propValue := Password ;
                wmiProperty.Set_Value (propValue);
            end ;
            wmiOutParams := wmiObject.ExecMethod_ ('Rename', wmiInst, 0, nil) ;
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            result := wmiProp.Get_Value ;
         // 0=OK reboot needed, non 0 various errors, see MSDN
            exit ;
        end ;
    except
        result := -1 ;
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// locally connected ATA drives only, no SCSI, USB or firewire

function MagWmiSmartDiskInfo (drivenr: integer; var errinfo, model,
                                    serial: string ; var diskbytes: Int64): boolean ;
var
    hSMARTIOCTL: THandle;
    cbBytesReturned: DWORD ;
    VersionParams: TGetVersionOutParams;
    scip: TSendCmdInParams;
    IdOutCmd: PSendCmdOutParams ;
    IdSector: PIdSector ;
    bIDCmd: byte ;
    curdrive: integer ;
    S: string ;

    function ChangeByteOrders (S: string): string  ;
    var
        I: integer ;
    begin
        SetLength (result, Length (S)) ;
        I := 1 ;
        while I < Length (S) do
        begin
            result [I + 1] := S [I] ;
            result [I] :=  S [I + 1] ;
            inc (I, 2) ;
        end ;
        result := trim (result) ;
    end ;

begin
    result := false;
    errinfo := '' ;
    model := '' ;
    serial := '' ;
    diskbytes := 0 ;          

 // open device driver - 9x returns info on up to four drives
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    begin
        hSMARTIOCTL := CreateFile( '\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0) ;
        if hSMARTIOCTL = INVALID_HANDLE_VALUE then
        begin
            errinfo := 'Unable to open SMARTVSD: ' + SysErrorMessage (GetLastError) ;
            exit ;
        end ;
    end

 // NT+ only returns info for a specified single physical drive
    else if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
        S := '\\.\PhysicalDrive' + IntToStr (drivenr) ;
        hSMARTIOCTL := CreateFile(Pchar (S), Generic_Read or Generic_Write,
                    File_Share_Read or File_Share_Write, nil, Open_Existing, 0, 0) ;
        if hSMARTIOCTL = INVALID_HANDLE_VALUE then
        begin
            errinfo := 'Unable to open physical drive: ' + SysErrorMessage (GetLastError) ;
            exit ;
        end ;
    end
    else exit ;

    GetMem (IdOutCmd, IdOutLen) ;
    try  // protect handle and memory

     // get SMART version
        cbBytesReturned := 0 ;
        result := DeviceIoControl (hSMARTIOCTL, DFP_GET_VERSION, Nil, 0,
                  @VersionParams, SizeOf (VersionParams), cbBytesReturned, nil) ;
        if NOT result then
        begin
            errinfo := 'DFP_GET_VERSION failed: ' + SysErrorMessage (GetLastError) ;
            exit ;
        end ;
        for curdrive := 0 to Pred (MAX_IDE_DRIVES) do
        begin

        // If there is a IDE device at number "i" issue commands to the device.
        // enable SMART for the drive, so we can read stuff from it
            if ((VersionParams.bIDEDeviceMap shr curdrive) AND $1) = $1 then
            begin
                if ((VersionParams.bIDEDeviceMap shr curdrive) AND $10) <> $10 then
                begin
                    FillChar (scip, sizeof (scip), 0) ;
                    SCIP.cBufferSize := 0 ;
                    SCIP.irDriveRegs.bFeaturesReg := SMART_ENABLE_SMART_OPERATIONS;
                    SCIP.irDriveRegs.bSectorCountReg := 1;
                    SCIP.irDriveRegs.bSectorNumberReg := 1;
                    SCIP.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
                    SCIP.irDriveRegs.bCylHighReg := SMART_CYL_HI;
                // Compute the drive number.
                    SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((curdrive AND 1) shl 4) ;
                    SCIP.irDriveRegs.bCommandReg := IDE_EXECUTE_SMART_FUNCTION;
                    SCIP.bDriveNumber := curdrive ;
                    FillChar (IdOutCmd^, IdOutLen, 0) ;
                    cbBytesReturned := 0 ;
                    result := DeviceIoControl (hSMARTIOCTL, DFP_SEND_DRIVE_COMMAND,
                           @SCIP, sizeof (SCIP) - 1, IdOutCmd, IdOutLen - 1,
                                                           cbBytesReturned, Nil );
                    if NOT result then
                    begin
                        errinfo := 'SMART Enable Command Failed, Drive: ' +
                                IntToStr (curdrive) + ' : ' + SysErrorMessage (GetLastError) ;
                    end ;
                end ;

            // Now, get the ID sector for all IDE devices in the system.
            // If the device is ATAPI use the IDE_ATAPI_ID command,
            // otherwise use the IDE_ID_FUNCTION command.
                FillChar (scip, sizeof (scip), 0) ;
                if ((VersionParams.bIDEDeviceMap shr curdrive) AND $10) = $10 then
                    bIDCmd := IDE_ATAPI_ID
                else
                    bIDCmd := IDE_ID_FUNCTION ;
                SCIP.cBufferSize := IDENTIFY_BUFFER_SIZE ;  //  IDENTIFY_BUFFER_SIZE ;
                SCIP.irDriveRegs.bFeaturesReg := 0;
                SCIP.irDriveRegs.bSectorCountReg := 1;
                SCIP.irDriveRegs.bSectorNumberReg := 1;
                SCIP.irDriveRegs.bCylLowReg := 0;
                SCIP.irDriveRegs.bCylHighReg := 0;
                SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((curdrive AND 1) shl 4) ;

        // The command can either be IDE identify or ATAPI identify.
                SCIP.irDriveRegs.bCommandReg := bIDCmd;
                SCIP.bDriveNumber := curdrive ;
                FillChar (IdOutCmd^, IdOutLen, 0) ;
                IdSector := @IdOutCmd.bBuffer ;
                IdOutCmd^.cBufferSize := IDENTIFY_BUFFER_SIZE ;
                cbBytesReturned := 0 ;
                result := DeviceIoControl (hSMARTIOCTL, DFP_RECEIVE_DRIVE_DATA,
                        @SCIP, sizeof (SCIP) - 1, IdOutCmd, IdOutLen {- 1},
                                                                cbBytesReturned, Nil) ;
                if result then
                begin
                    with IdSector^ do
                    begin
                        model := ChangeByteOrders (sModelNumber) ;
                        // ChangeByteOrders (sFirmwareRev)) ;
                        serial := ChangeByteOrders (sSerialNumber) ;
                        diskbytes := Int64 (ulTotalAddressableSectors) * 512 ;
                        break ; // finished
                     end ;
                end
                else
                    errinfo := 'Identify Command Failed on Drive: ' +
                            DriverErrorStr [IdOutCmd.DriverStatus.bDriverError] +
                        ' bIDEStatus=' + IntToHex (IdOutCmd.DriverStatus.bIDEStatus, 4)
                                              + ' : ' + SysErrorMessage (GetLastError) ;
            end ;
        end ;
    finally
        Freemem (IdOutCmd) ;
        CloseHandle (hSMARTIOCTL) ;
    end ;
end ;

(* ************************************************************************* *)

// locally connected ATA drives only, no SCSI, USB or firewire

function MagWmiSmartDiskFail (drivenr: integer; var SmartResult: TSmartResult ;
                                                     var errinfo: string): boolean ;
var
    hSMARTIOCTL: THandle;
    cbBytesReturned: DWORD ;
    VersionParams: TGetVersionOutParams;
    scip: TSendCmdInParams;
    IdOutCmd: PSendCmdOutParams ;
    IdSector: PIdSector ;
    bIDCmd: byte ;
    curdrive, I, J, totattrs, basevalue: integer ;
    ThreshOutCmd: PSendCmdOutParams ;
    SAttrThreshold: PAttrThreshold ;
    AttrOutCmd: PSendCmdOutParams ;
    SDriveAttribute: PDriveAttribute ;
    S: string ;
    Attr: byte ;
    BufPtr: PChar ;
    Raw64: int64 ;

    function ChangeByteOrders (S: string): string  ;
    var
        I: integer ;
    begin
        SetLength (result, Length (S)) ;
        I := 1 ;
        while I < Length (S) do
        begin
            result [I + 1] := S [I] ;
            result [I] :=  S [I + 1] ;
            inc (I, 2) ;
        end ;
        result := trim (result) ;
    end ;

begin
    result := false;
    errinfo := '' ;
    try
    with SmartResult do
    begin
        DriveNum := -1 ;
        CapacityNum := 0 ;
        for I := 1 to 3 do VendorUnique [I] := 0 ;
        SerialNumber := '' ;
        FirmwareRev := '' ;
        ModelNumber := '' ;
        MoreVendorUnique := 0 ;
        Temperature := 0 ;
        TempWorst := 0 ;
        TempLow := 0 ;
        HoursRunning := 0 ;
        ReallocSector := 0 ;
        PowerCycles := 0 ;
        SmartFailTot := 0 ;
        SmartWarnTot := 0 ;
        TotalAttrs := 0 ;
        for I := 0 to MaxSmartAttr do AttrNum [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrName [I] := '' ;
        for I := 0 to MaxSmartAttr do AttrPreFail [I] := false ;
        for I := 0 to MaxSmartAttr do AttrEvents [I] := false ;
        for I := 0 to MaxSmartAttr do AttrErrorRate [I] := false ;
        for I := 0 to MaxSmartAttr do AttrCurValue [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrWorstVal [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrThreshold [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrRawValue [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrState [I] := '' ;
    end ;

 // open device driver - 9x returns info on up to four drives
    if Win32Platform = VER_PLATFORM_WIN32_WINDOWS then
    begin
        hSMARTIOCTL := CreateFile( '\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0) ;
        if hSMARTIOCTL = INVALID_HANDLE_VALUE then
        begin
            errinfo := 'Unable to open SMARTVSD: ' + SysErrorMessage (GetLastError) ;
            exit ;
        end ;
    end

 // NT+ only returns info for a specified single physical drive
    else if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
        S := '\\.\PhysicalDrive' + IntToStr (drivenr) ;
        hSMARTIOCTL := CreateFile(Pchar (S), Generic_Read or Generic_Write,
                    File_Share_Read or File_Share_Write, nil, Open_Existing, 0, 0) ;
        if hSMARTIOCTL = INVALID_HANDLE_VALUE then
        begin
            errinfo := 'Unable to open physical drive: ' + SysErrorMessage (GetLastError) ;
            exit ;
        end ;
    end
    else exit ;

    GetMem (IdOutCmd, IdOutLen) ;
    GetMem (AttrOutCmd, AttrOutLen) ;
    GetMem (ThreshOutCmd, ThreshOutLen) ;
    try  // protect handle and memory

     // get SMART version
        cbBytesReturned := 0 ;
        result := DeviceIoControl (hSMARTIOCTL, DFP_GET_VERSION, Nil, 0,
                  @VersionParams, SizeOf (VersionParams), cbBytesReturned, nil) ;
        if NOT result then
        begin
            errinfo := 'DFP_GET_VERSION failed: ' + SysErrorMessage (GetLastError) ;
            exit ;
        end ;
        for curdrive := 0 to Pred (MAX_IDE_DRIVES) do
        begin

        // If there is a IDE device at number "i" issue commands to the device.
        // enable SMART for the drive, so we can read stuff from it
            if ((VersionParams.bIDEDeviceMap shr curdrive) AND $1) = $1 then
            begin
                if ((VersionParams.bIDEDeviceMap shr curdrive) AND $10) <> $10 then
                begin
                    FillChar (scip, sizeof (scip), 0) ;
                    SCIP.cBufferSize := 0 ;
                    SCIP.irDriveRegs.bFeaturesReg := SMART_ENABLE_SMART_OPERATIONS;
                    SCIP.irDriveRegs.bSectorCountReg := 1;
                    SCIP.irDriveRegs.bSectorNumberReg := 1;
                    SCIP.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
                    SCIP.irDriveRegs.bCylHighReg := SMART_CYL_HI;
                // Compute the drive number.
                    SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((curdrive AND 1) shl 4) ;
                    SCIP.irDriveRegs.bCommandReg := IDE_EXECUTE_SMART_FUNCTION;
                    SCIP.bDriveNumber := curdrive ;
                    FillChar (IdOutCmd^, IdOutLen, 0) ;
                    cbBytesReturned := 0 ;
                    result := DeviceIoControl (hSMARTIOCTL, DFP_SEND_DRIVE_COMMAND,
                           @SCIP, sizeof (SCIP) - 1, IdOutCmd, IdOutLen - 1,
                                                           cbBytesReturned, Nil );
                    if NOT result then
                    begin
                        errinfo := 'SMART Enable Command Failed, Drive: ' +
                                IntToStr (curdrive) + ' : ' + SysErrorMessage (GetLastError) ;
                  //   may fail, but still returns SMART info ...
                    end ;
                end ;

            // Now, get the ID sector for all IDE devices in the system.
            // If the device is ATAPI use the IDE_ATAPI_ID command,
            // otherwise use the IDE_ID_FUNCTION command.
                FillChar (scip, sizeof (scip), 0) ;
                if ((VersionParams.bIDEDeviceMap shr curdrive) AND $10) = $10 then
                    bIDCmd := IDE_ATAPI_ID
                else
                    bIDCmd := IDE_ID_FUNCTION ;
                SCIP.cBufferSize := IDENTIFY_BUFFER_SIZE ;  //  IDENTIFY_BUFFER_SIZE ;
                SCIP.irDriveRegs.bFeaturesReg := 0;
                SCIP.irDriveRegs.bSectorCountReg := 1;
                SCIP.irDriveRegs.bSectorNumberReg := 1;
                SCIP.irDriveRegs.bCylLowReg := 0;
                SCIP.irDriveRegs.bCylHighReg := 0;
                SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((curdrive AND 1) shl 4) ;

        // The command can either be IDE identify or ATAPI identify.
                SCIP.irDriveRegs.bCommandReg := bIDCmd;
                SCIP.bDriveNumber := curdrive ;
                FillChar (IdOutCmd^, IdOutLen, 0) ;
                IdSector := @IdOutCmd.bBuffer ;
                IdOutCmd^.cBufferSize := IDENTIFY_BUFFER_SIZE ;
                cbBytesReturned := 0 ;
                result := DeviceIoControl (hSMARTIOCTL, DFP_RECEIVE_DRIVE_DATA,
                        @SCIP, sizeof (SCIP) - 1, IdOutCmd, IdOutLen,
                                                                cbBytesReturned, Nil) ;
                if NOT result then
                begin
                    errinfo := 'Identify Command Failed on Drive: ' +
                            DriverErrorStr [IdOutCmd.DriverStatus.bDriverError] +
                        ' bIDEStatus=' + IntToHex (IdOutCmd.DriverStatus.bIDEStatus, 4)
                                              + ' : ' + SysErrorMessage (GetLastError) ;
                    continue ;
                end ;

           // get fixed info
                with SmartResult, IdSector^ do
                begin
                    DriveNum := curdrive ;
                    CapacityNum := Int64 (ulTotalAddressableSectors) * 512 ;
                //    VendorUnique := wVendorUnique ;
                    SerialNumber := ChangeByteOrders (sSerialNumber) ; ;
                    FirmwareRev := ChangeByteOrders (sFirmwareRev) ;
                    ModelNumber := ChangeByteOrders (sModelNumber) ;
                    MoreVendorUnique := wMoreVendorUnique ;
                end ;

            // read SMART attributes
                FillChar (scip, sizeof (scip), 0) ;
                SCIP.cBufferSize := READ_ATTRIBUTE_BUFFER_SIZE ;
                SCIP.irDriveRegs.bFeaturesReg := SMART_READ_ATTRIBUTE_VALUES;
                SCIP.irDriveRegs.bSectorCountReg := 1;
                SCIP.irDriveRegs.bSectorNumberReg := 1;
                SCIP.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
                SCIP.irDriveRegs.bCylHighReg := SMART_CYL_HI;
        // Compute the drive number.
                SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((curdrive AND 1) shl 4) ;
                SCIP.irDriveRegs.bCommandReg := IDE_EXECUTE_SMART_FUNCTION;
                SCIP.bDriveNumber := curdrive ;
                FillChar (AttrOutCmd^, AttrOutLen, 0) ;
                cbBytesReturned := 0 ;
                result := DeviceIoControl (hSMARTIOCTL, DFP_RECEIVE_DRIVE_DATA,
                       @SCIP, sizeof (SCIP) - 1, AttrOutCmd, AttrOutLen - 1,
                                                       cbBytesReturned, Nil );
                if NOT result then
                begin
                    errinfo := 'SMART Read Attr Command Failed, Drive: ' +
                               IntToStr (curdrive) + ' : ' + SysErrorMessage (GetLastError) +
                               '; DriverStatus: ' +
                               DriverErrorStr [AttrOutCmd.DriverStatus.bDriverError] +
                               '; bIDEStatus=' + IntToHex (AttrOutCmd.DriverStatus.bIDEStatus, 4) ;
                    continue ;  // no more
                end ;
                FillChar (scip, sizeof (scip), 0) ;
                SCIP.cBufferSize := READ_THRESHOLD_BUFFER_SIZE ;
                SCIP.irDriveRegs.bFeaturesReg := SMART_READ_ATTRIBUTE_THRESHOLDS;
                SCIP.irDriveRegs.bSectorCountReg := 1;
                SCIP.irDriveRegs.bSectorNumberReg := 1;
                SCIP.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
                SCIP.irDriveRegs.bCylHighReg := SMART_CYL_HI;
        // Compute the drive number.
                SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((curdrive AND 1) shl 4) ;
                SCIP.irDriveRegs.bCommandReg := IDE_EXECUTE_SMART_FUNCTION;
                SCIP.bDriveNumber := curdrive ;
                FillChar (ThreshOutCmd^, ThreshOutLen, 0) ;
                cbBytesReturned := 0 ;
                result := DeviceIoControl (hSMARTIOCTL, DFP_RECEIVE_DRIVE_DATA,
                       @SCIP, sizeof (SCIP) - 1, ThreshOutCmd, ThreshOutLen - 1,
                                                       cbBytesReturned, Nil );
                if NOT result then
                begin
                    errinfo := 'SMART Read Threshold Command Failed, Drive: ' +
                               IntToStr (curdrive) + ' : ' + SysErrorMessage (GetLastError) +
                               '; DriverStatus: ' +
                               DriverErrorStr [AttrOutCmd.DriverStatus.bDriverError] +
                               '; bIDEStatus=' + IntToHex (AttrOutCmd.DriverStatus.bIDEStatus, 4) ;
                    continue ;
                end  ;

        // loop through the structures, getting attributes
                BufPtr := @ThreshOutCmd.bBuffer ;
                Pointer (SAttrThreshold) := BufPtr + 2 ;
                BufPtr := @AttrOutCmd.bBuffer ;
                Pointer (SDriveAttribute) := BufPtr + 2 ;
                totattrs := 0 ;
                basevalue := 100 ;
                for J := 0 to Pred (NUM_ATTRIBUTE_STRUCTS) do
                begin
                    with SmartResult do
                    begin
                        Raw64 := 0 ;
                        Move (SDriveAttribute^.bRawValue [1], Raw64, 6) ;
                        Attr := SDriveAttribute^.bAttrID ;
                        if Attr = 0 then continue ;
                        AttrName [totattrs] := 'Unknown' ;
                        if Attr <= ATTR_POWER_CYCLE_COUNT then
                            AttrName [totattrs] := pAttrNames [Attr]
                        else if (Attr >= Attr_Emergency_Retract_Cycle) and (Attr <= Attr_Offline_Seek_Perf) then
                            AttrName [totattrs] := pAttrNames2 [Attr - Attr_Emergency_Retract_Cycle] ;
                        AttrNum [totattrs] := SDriveAttribute^.bAttrID ;
                        AttrPreFail [totattrs] := (SDriveAttribute^.wStatusFlags AND
                                            PRE_FAILURE_WARRANTY) = PRE_FAILURE_WARRANTY ;
                        AttrEvents [totattrs] := (SDriveAttribute^.wStatusFlags AND
                                            EVENT_COUNT_ATTRIBUTE) = EVENT_COUNT_ATTRIBUTE ;
                        AttrErrorRate [totattrs] := (SDriveAttribute^.wStatusFlags AND
                                            ERROR_RATE_ATTRIBUTE) = ERROR_RATE_ATTRIBUTE ;
                        AttrCurValue [totattrs] := SDriveAttribute^.bAttrValue ;
                        AttrWorstVal [totattrs] := SDriveAttribute^.bWorstValue ;
                        AttrThreshold [totattrs] := SAttrThreshold^.bWarrantyThreshold ;
                        AttrRawValue [totattrs] := Raw64 ;

                     // keep some more interesting raw values
                        if (Raw64 > 0) and (Raw64 < MaxLongInt) then
                        begin
                            if Attr = ATTR_REALLOC_SECTOR_COUNT then ReallocSector := Raw64 ;
                            if Attr = ATTR_POWER_ON_COUNT then
                                           HoursRunning := (Raw64 div 60) + 1 ;  // assume minutes
                            if Attr = ATTR_POWER_CYCLE_COUNT then PowerCycles := Raw64 ;
                        end ;

                     // try and set base or highest value for attribute
                        if ((Attr = ATTR_SPIN_UP_TIME) or (Attr = ATTR_START_STOP_COUNT)) and
                               (AttrCurValue [totattrs] > 230) then basevalue := 253 ;

                     // check if drive is OK, failing or warn
                        AttrState [totattrs] := '-' ; // 'Aging' ;
                        if AttrPreFail [totattrs] and (AttrThreshold [totattrs] <> 0) then
                        begin
                            AttrState [totattrs] := 'OK' ;
                            if AttrCurValue [totattrs] < AttrThreshold [totattrs] then
                            begin
                                inc (SmartFailTot) ;
                                AttrState [totattrs] := 'Failing Now' ;
                            end
                            else if AttrWorstVal [totattrs] < AttrThreshold [totattrs] then
                            begin
                                inc (SmartFailTot) ;
                                AttrState [totattrs] := 'Failed In Past' ;
                            end
                            else
                            begin
                              // increase threshold for warning
                                I := ((basevalue - AttrThreshold [totattrs]) div 2) +
                                                            AttrThreshold [totattrs] ;
                                if (AttrCurValue [totattrs] < I) OR
                                   ((Attr = ATTR_REALLOC_SECTOR_COUNT) and (Raw64 > 500)) then
                                begin
                                    inc (SmartWarnTot) ;
                                    AttrState [totattrs] := 'Warning' ;
                                end ;
                            end ;
                        end ;
                        if Attr = Attr_Temperature_Celcius then
                        begin
                             Temperature := SDriveAttribute^.bRawValue [1] ;
                             TempLow := SDriveAttribute^.bRawValue [3] ;   // IBM and Fujitsu
                             TempWorst := SDriveAttribute^.bRawValue [5] ; // IBM and Fujitsu
                           // Seagate drives store worst temperature
                             if Temperature = SDriveAttribute^.bAttrValue then
                                         TempWorst := SDriveAttribute^.bWorstValue ;
                        end ;
                    end ;
                    inc (totattrs) ;
                    if totattrs >= MaxSmartAttr then break ;
                    Inc (SAttrThreshold) ;   // next record
                    Inc (SDriveAttribute) ;
                end ;
                SmartResult.TotalAttrs := totattrs ;
                result := true ;
                break ; // finished, got details for single drive
            end ;
        end ;
    finally
        Freemem (IdOutCmd) ;
        Freemem (AttrOutCmd) ;
        Freemem (ThreshOutCmd) ;
        CloseHandle (hSMARTIOCTL) ;
    end ;
    except
        errinfo := 'SMART Failed - ' + GetExceptMess (ExceptObject) ;
    end ;
end ;

(* ************************************************************************* *)

function MagWmiScsiDiskInfo (drivenr: integer; var errinfo, model,
                                    serial: string ; var diskbytes: Int64): boolean ;
{$ALIGN ON}
type
	TScsiPassThrough = record
		Length             : Word;
		ScsiStatus         : Byte;
		PathId             : Byte;
		TargetId           : Byte;
		Lun                : Byte;
		CdbLength          : Byte;
		SenseInfoLength    : Byte;
		DataIn             : Byte;
		DataTransferLength : ULONG;
		TimeOutValue       : ULONG;
		DataBufferOffset   : DWORD;
		SenseInfoOffset    : ULONG;
		Cdb                : Array[0..15] of Byte;
	end;
	TScsiPassThroughWithBuffers = record
		spt : TScsiPassThrough;
		bSenseBuf : Array[0..31] of Byte;
		bDataBuf : Array[0..191] of Byte;
	end;
{ALIGN OFF}
var
    hDevice: THandle ;
	dwReturned: DWORD;
    len: DWORD;
    S: string ;
	Buffer: Array [0..SizeOf (TScsiPassThroughWithBuffers) +
                            SizeOf (TScsiPassThrough) -1 ] of Byte;
	sptwb: TScsiPassThroughWithBuffers absolute Buffer;
begin
	result := false;
    errinfo := '' ;
    serial := '' ;
    model := '' ;
    diskbytes := 0 ;
    S := '\\.\PhysicalDrive' + IntToStr (drivenr) ;
	hDevice := CreateFile( PChar(S), GENERIC_READ or GENERIC_WRITE,
				FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0 ) ;
    if hDevice = INVALID_HANDLE_VALUE then
    begin
        errinfo := 'Unable to open physical drive: ' + SysErrorMessage (GetLastError) ;
        exit ;
    end ;
    try
	FillChar (Buffer, SizeOf(Buffer), #0) ;
	with sptwb.spt do
	begin
		Length := SizeOf (TScsiPassThrough) ;
		CdbLength := 6 ; // CDB6GENERIC_LENGTH
		SenseInfoLength := 24 ;
		DataIn := 1 ; // SCSI_IOCTL_DATA_IN
		DataTransferLength := 192 ;
		TimeOutValue := 2 ;
		DataBufferOffset := PChar (@sptwb.bDataBuf) - PChar (@sptwb) ;
		SenseInfoOffset := PChar (@sptwb.bSenseBuf) - PChar (@sptwb) ;
		Cdb[0] := $12 ; //	OperationCode := SCSIOP_INQUIRY;
		Cdb[1] := $01 ; //	Flags := CDB_INQUIRY_EVPD;  Vital product data
		Cdb[2] := $80 ; //	PageCode            Unit serial number
		Cdb[4] := 192 ; // AllocationLength
	end;
	len := sptwb.spt.DataBufferOffset + sptwb.spt.DataTransferLength;
    result := DeviceIoControl (hDevice, $0004d004, @sptwb, SizeOf(TScsiPassThrough),
                                                         @sptwb, len, dwReturned, nil) ;
    if NOT result then
    begin
         errinfo := 'SCSI_IOCTL Command Failed: ' + SysErrorMessage (GetLastError) ;
         CloseHandle (hDevice) ;
         exit ;
    end ;
    len := Ord((PChar(@sptwb.bDataBuf)+3)^) ;
	if ((PChar (@sptwb.bDataBuf) + 1)^ = #$80) and (len > 0) then
    begin
        SetString (serial, PChar (@sptwb.bDataBuf) + 4, len) ;
        serial := Trim (serial) ;
    end
    else
    begin
        result := false ;
        errinfo := 'No SCSI Drive Serial Number Returned' ;
    end ;
 {   for len := 4 to 104 do
    begin
        if sptwb.bDataBuf [len] < $20 then sptwb.bDataBuf [len] := $20 ;
    end ;
    SetString (model, PChar (@sptwb.bDataBuf) + 4, 100) ;
    model := Trim (model) ;}
    except
        errinfo := 'Exception Getting SCSI Drive Serial Number' ;
        result := false ;
    end ;
    CloseHandle (hDevice) ;
end;

(* ************************************************************************* *)

// reboot or close down PC

function MagWmiCloseWin (const Comp, User, Pass: widestring ; reboot: boolean;
                                                var errinfo: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiInst, wmiInParams, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    wmiProperty: SWbemProperty;
    wmiMethod: SWbemMethod;
    ovVar, propValue: OleVariant;
    Enum: IEnumVariant;
    Arg: widestring ;
    lwValue, flags: LongWord;
begin
    errinfo := '' ;
    Result := -1 ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer (Comp, RootNameSpace, User, Pass,
                                                                      '', '', 0, nil);
        wmiServices.Security_.Privileges.Add(wbemPrivilegeShutdown, True);
        Arg := 'SELECT * FROM Win32_OperatingSystem WHERE Primary=True' ;
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        if (wmiObjectSet.Count < 1) then exit ;  // expect a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('Win32Shutdown', 0) ;
            wmiInParams := wmiMethod.InParameters ;
            wmiInst := wmiInParams.SpawnInstance_(0) ;
            flags := EWX_SHUTDOWN + EWX_FORCE ;  // forced shutdown
            if reboot then flags := EWX_REBOOT + EWX_FORCE ;  // forced reboot
            wmiProperty := wmiInst.Properties_.Add ('Flags', wbemCimtypeSint32, False, 0) ;
            propValue := flags ;
            wmiProperty.Set_Value (propValue);
            wmiOutParams := wmiObject.ExecMethod_ ('Win32Shutdown', wmiInst, 0, nil);
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            result := wmiProp.Get_Value ;
            if result <> 0 then errinfo := 'PC Close Down Failed: ' +
                         SysErrorMessage (result) + ' [' + IntToCStr (result) + ']' ;
            exit ;
        end ;
    except
        result := -1 ;
        errinfo := 'Exception in MagWmiCloseWin' ;

    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

function MagWmiGetMemory: TWmiMemoryRec ;
var
    rows, instances: integer ;
    WmiResults: T2DimStrArray ;

    function GetResInt64 (Info: string): Int64 ;
    begin
        result := AscToInt64 (MagWmiSearch1 (WmiResults, Info)) * KBYTE ;  
    end ;

begin
    with result do
    begin
        FreePhysicalMemory := 0 ;
        FreeSpaceInPagingFiles := 0 ;
        FreeVirtualMemory := 0 ;
        SizeStoredInPagingFiles := 0 ;
        TotalSwapSpaceSize := 0 ;
        TotalVirtualMemorySize := 0 ;
        TotalVisibleMemorySize := 0 ;
    end ;
    try
        rows := MagWmiGetInfo ('', RootNameSpace, '', '',
          'SELECT FreePhysicalMemory, FreeSpaceInPagingFiles, FreeVirtualMemory, ' +
          'SizeStoredInPagingFiles, TotalSwapSpaceSize, TotalVirtualMemorySize, ' +
          'TotalVisibleMemorySize FROM Win32_OperatingSystem', WmiResults, instances) ;
        if (instances = 1) and (rows > 1) then
        begin
            with result do
            begin
                FreePhysicalMemory := GetResInt64 ('FreePhysicalMemory') ;
                FreeSpaceInPagingFiles := GetResInt64 ('FreeSpaceInPagingFiles') ;
                FreeVirtualMemory := GetResInt64 ('FreeVirtualMemory') ;
                SizeStoredInPagingFiles := GetResInt64 ('SizeStoredInPagingFiles') ;
                TotalSwapSpaceSize := GetResInt64 ('TotalSwapSpaceSize') ;
                TotalVirtualMemorySize := GetResInt64 ('TotalVirtualMemorySize') ;
                TotalVisibleMemorySize := GetResInt64 ('TotalVisibleMemorySize') ;
            end ;
        end ;    
    finally
        WmiResults := Nil ;
    end ;
end ;


(* ************************************************************************* *)


end.
