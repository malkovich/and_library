unit smartapi ;

{ ****************************************************************************
*                                                                           *
* THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY     *
* KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE       *
* IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR     *
* PURPOSE.                                                                  *
*                                                                           *
* Copyright 1993-98  Microsoft Corporation.  All Rights Reserved.           *
*                                                                           *
****************************************************************************/

/ ****************************************************************************
*
* PROGRAM: SMART.H
*
* PURPOSE: Structure definitions for an application that calls SMART Ioctls

Converted from C to pascal by Angus Robertson, Magenta Systems Ltd
18th July 2004
*
****************************************************************************  }

interface

uses
  Windows, Messages, SysUtils ;

// # ifndef SMARTIOCTL_INCLUDED
// # define SMARTIOCTL_INCLUDED

// Miscellaneous


type
    USHORT = Word ;   // Angus   16 unsigned bits


const
    MAX_IDE_DRIVES = 4 ;	// Max number of drives assuming primary/secondary, master/slave topology
    READ_ATTRIBUTE_BUFFER_SIZE = 512 ;
    IDENTIFY_BUFFER_SIZE = 512 ;
    READ_THRESHOLD_BUFFER_SIZE = 512 ;

//
// IOCTL commands
//
    DFP_GET_VERSION		   = $00074080 ;
    DFP_SEND_DRIVE_COMMAND = $0007c084 ;
    DFP_RECEIVE_DRIVE_DATA = $0007c088 ;

//---------------------------------------------------------------------
// GETVERSIONOUTPARAMS contains the data returned from the
// Get Driver Version function.
//---------------------------------------------------------------------
type
    PGetVersionOutParams = ^TGetVersionOutParams ;
    TGetVersionOutParams = Packed Record
	    bVersion: byte ;		// Binary driver version.
	    bRevision: byte ;		// Binary driver revision.
	    bReserved: byte ;		// Not used.
	    bIDEDeviceMap: byte ;	// Bit map of IDE devices.
	    fCapabilities: DWORD ;	// Bit mask of driver capabilities.
	    dwReserved: array [1..4] of DWORD ;	// For future use.
     end ;

//
// Bits returned in the fCapabilities member of GETVERSIONOUTPARAMS
//
const
    CAP_IDE_ID_FUNCTION	= 1 ;   	     // ATA ID command supported
    CAP_IDE_ATAPI_ID = 2 ;	             // ATAPI ID command supported
    CAP_IDE_EXECUTE_SMART_FUNCTION = 4 ; // SMART commannds supported

//---------------------------------------------------------------------
// IDE registers
//---------------------------------------------------------------------

type
    PIDERegs = ^TIDERegs ;
    TIDERegs = Packed Record
	    bFeaturesReg: byte ;		// Used for specifying SMART "commands".
	    bSectorCountReg: byte ;	    // IDE sector count register
        bSectorNumberReg: byte ;	// IDE sector number register
	    bCylLowReg: byte ;			// IDE low order cylinder value
	    bCylHighReg: byte ;		    // IDE high order cylinder value
	    bDriveHeadReg: byte ;		// IDE drive/head register
	    bCommandReg: byte ;		    // Actual IDE command.
	    bReserved: byte ;			// reserved for future use.  Must be zero.
    end ;

//---------------------------------------------------------------------
// SENDCMDINPARAMS contains the input parameters for the
// Send Command to Drive function.
//---------------------------------------------------------------------

    PSendCmdInParams = ^TSendCmdInParams ;
    TSendCmdInParams = Packed Record
	    cBufferSize: DWORD ;	    // Buffer size in bytes
    	irDriveRegs: TIDERegs ;     // Structure with drive register values.
	    bDriveNumber: byte;		    // Physical drive number to send command to (0,1,2,3).
    	bReserved: array [1..3] of byte ;	// Reserved for future expansion.
	    dwReserved: array [1..4] of DWORD ;	// For future use.
        bBuffer: array [1..1] of byte ;		// Input buffer.
    end ;

//
// Valid values for the bCommandReg member of IDEREGS.
//
const
    IDE_ATAPI_ID				= $A1 ;	// Returns ID sector for ATAPI.
    IDE_ID_FUNCTION				= $EC ;	// Returns ID sector for ATA.
    IDE_EXECUTE_SMART_FUNCTION	= $B0 ;	// Performs SMART cmd.
											// Requires valid bFeaturesReg,
											// bCylLowReg, and bCylHighReg
//
// Cylinder register values required when issuing SMART command
//
    SMART_CYL_LOW	= $4F ;
    SMART_CYL_HI	= $C2 ;

//---------------------------------------------------------------------
// Status returned from driver
//---------------------------------------------------------------------

type
    PDriverStatus = ^TDriverStatus ;
    TDriverStatus = Packed Record
	    bDriverError: byte ;	// Error code from driver, or 0 if no error.
	    bIDEStatus: byte ;		// Contents of IDE Error register.
                                // Only valid when bDriverError is SMART_IDE_ERROR.
	    bReserved: array [1..2] of byte;		// Reserved for future expansion.
	    dwReserved: array [1..2] of DWORD;		// Reserved for future expansion.
    end ;

//
// bDriverError values
//
const
    SMART_NO_ERROR		   = 0 ;	// No error
    SMART_IDE_ERROR		   = 1 ;	// Error from IDE controller
    SMART_INVALID_FLAG	   = 2 ;	// Invalid command flag
    SMART_INVALID_COMMAND  = 3 ;	// Invalid command byte
    SMART_INVALID_BUFFER   = 4 ;	// Bad buffer (null, invalid addr..)
    SMART_INVALID_DRIVE	   = 5 ;	// Drive number not valid
    SMART_INVALID_IOCTL	   = 6 ;	// Invalid IOCTL
    SMART_ERROR_NO_MEM	   = 7 ;	// Could not lock user's buffer
    SMART_INVALID_REGISTER = 8 ;	// Some IDE Register not valid
    SMART_NOT_SUPPORTED	   = 9 ;	// Invalid cmd flag set
    SMART_NO_IDE_DEVICE	   = 10;	// Cmd issued to device not present
									// although drive number is valid
// 11-255 reserved

var
    DriverErrorStr: array [0..10] of string = (
        'No error', 'Error from IDE controller', 'Invalid command flag',
        'Invalid command byte', 'Bad buffer (null, invalid addr..)',
        'Drive number not valid', 'Invalid IOCTL', 'Could not lock user"s buffer',
        'Some IDE Register not valid', 'Invalid cmd flag set',
        'Cmd issued to device not present although drive number is valid') ;


//---------------------------------------------------------------------
// Structure returned by SMART IOCTL for several commands
//---------------------------------------------------------------------

type
    PSendCmdOutParams = ^TSendCmdOutParams ;
    TSendCmdOutParams = Packed Record
	    cBufferSize: DWORD ;	    	// Size of bBuffer in bytes
	    DriverStatus: TDriverStatus ;  	// Driver status structure.
	    bBuffer: array [1..1] of byte ; 	// Buffer of arbitrary length in which to store the data read from the drive.
    end ;

//---------------------------------------------------------------------
// Feature register defines for SMART "sub commands"
//---------------------------------------------------------------------

const
    SMART_READ_ATTRIBUTE_VALUES	            = $D0 ;	// ATA4: Renamed SMART READ DATA
    SMART_READ_ATTRIBUTE_THRESHOLDS			= $D1 ;	// Obsoleted in ATA4!
    SMART_ENABLE_DISABLE_ATTRIBUTE_AUTOSAVE	= $D2 ;
    SMART_SAVE_ATTRIBUTE_VALUES				= $D3 ;
    SMART_EXECUTE_OFFLINE_IMMEDIATE			= $D4 ;	// ATA4
    SMART_READ_LOG_SECTOR                   = $D5 ;	// ATA5
    SMART_WRITE_LOG_SECTOR                  = $D6 ;	// ATA5
    SMART_WRITE_THRESHOLDS                  = $D7 ;	// Obsoleted ??
// Vendor specific commands:
    SMART_ENABLE_SMART_OPERATIONS			= $D8 ;
    SMART_DISABLE_SMART_OPERATIONS			= $D9 ;
    SMART_RETURN_SMART_STATUS				= $DA ;

//---------------------------------------------------------------------
// The following structure defines the structure of a Drive Attribute
//---------------------------------------------------------------------

type
    PDriveAttribute = ^TDriveAttribute ;
    TDriveAttribute = Packed Record
	    bAttrID: byte ;		// Identifies which attribute
	    wStatusFlags: word ;	// see bit definitions below
	    bAttrValue: byte ;		// Current normalized value
	    bWorstValue: byte;	    // How bad has it ever been?
	    bRawValue: array [1..6] of byte ;	// Un-normalized value
	    bReserved: byte ;		// ...
    end ;

//---------------------------------------------------------------------
// The following structure defines the structure of a Warranty Threshold
// Obsoleted in ATA4!
//---------------------------------------------------------------------
    PAttrThreshold = ^TAttrThreshold ;
    TAttrThreshold = Packed Record
    	bAttrID: byte ;			// Identifies which attribute
    	bWarrantyThreshold: byte ;	// Triggering value
	    bReserved: array [1..10] of byte;	// ...
    end ;

//---------------------------------------------------------------------
// The following struct defines the interesting part of the IDENTIFY
// buffer:
//---------------------------------------------------------------------
type
    PIdSector = ^TIdSector ;
    TIdSector = Packed Record
	    wGenConfig: USHORT ;
	    wNumCyls: USHORT ;
	    wReserved: USHORT ;
	    wNumHeads: USHORT ;
	    wBytesPerTrack: USHORT ;
	    wBytesPerSector: USHORT ;
	    wSectorsPerTrack: USHORT ;
	    wVendorUnique: array [1..3] of USHORT ;
	    sSerialNumber: array [1..20] of char ;
	    wBufferType: USHORT ;
	    wBufferSize: USHORT ;
	    wECCSize: USHORT ;
	    sFirmwareRev: array [1..8] of char ;
	    sModelNumber: array [1..40] of char ;
	    wMoreVendorUnique: USHORT ;
	    wDoubleWordIO: USHORT ;
	    wCapabilities: USHORT ;
	    wReserved1: USHORT ;
	    wPIOTiming: USHORT ;
	    wDMATiming: USHORT ;
        wBS: USHORT ;
	    wNumCurrentCyls: USHORT ;
	    wNumCurrentHeads: USHORT ;
	    wNumCurrentSectorsPerTrack: USHORT ;
	    ulCurrentSectorCapacity: ULONG ;
	    wMultSectorStuff: USHORT ;
	    ulTotalAddressableSectors: ULONG ;
	    wSingleWordDMA: USHORT ;
	    wMultiWordDMA: USHORT ;
	    bReserved: array [1..128] of byte ;
    end ;


//---------------------------------------------------------------------
// Valid Attribute IDs
//---------------------------------------------------------------------

const
    ATTR_INVALID				= 0 ;
    ATTR_READ_ERROR_RATE		= 1 ;
    ATTR_THROUGHPUT_PERF		= 2 ;
    ATTR_SPIN_UP_TIME			= 3 ;
    ATTR_START_STOP_COUNT		= 4 ;
    ATTR_REALLOC_SECTOR_COUNT	= 5 ;
    ATTR_READ_CHANNEL_MARGIN	= 6 ;
    ATTR_SEEK_ERROR_RATE		= 7 ;
    ATTR_SEEK_TIME_PERF			= 8 ;
    ATTR_POWER_ON_COUNT	        = 9 ;
    ATTR_SPIN_RETRY_COUNT		= 10 ;
    ATTR_CALIBRATION_RETRY_COUNT   = 11 ;
    ATTR_POWER_CYCLE_COUNT		   = 12 ;
    Attr_Emergency_Retract_Cycle   = 192 ; // C0
    Attr_Load_Cycle_Count          = 193 ; // C1
    Attr_Temperature_Celcius       = 194 ; // C2
    Attr_Hardware_ECC              = 195 ; // C3
    Attr_Reallocation_Event_Count  = 196 ; // C4
    Attr_Current_Pending_Sector    = 197 ; // C5
    Attr_Off_line_Uncorrectable    = 198 ; // C6
    Attr_Ultra_ATA_CRC_Error_Rate  = 199 ; // C7
    Attr_Multi_Zone_Error_Rate     = 200 ; // C8
    Attr_Soft_Read_Error_Rate      = 201 ; // C9
    Attr_Off_Track_Errors          = 201 ; // C9 - alternative Maxtor
    Attr_TA_Increase_Count         = 202 ; // CA
    Attr_Run_Out_Cancel            = 203 ; // CB
    Attr_ECC_Errors                = 203 ; // CB - alternative Maxtor
    Attr_Shock_Count_Write_Ops     = 204 ; // CC
    Attr_Shock_Rate_Write_Ops      = 205 ; // CD
    Attr_Unknown_Attribute         = 206 ; // CE
    Attr_Spin_High_Current         = 207 ; // CE
    Attr_Spin_Buzz                 = 208 ; // CF
    Attr_Offline_Seek_Perf         = 209 ; // D0


//---------------------------------------------------------------------
// Status Flags Values
//---------------------------------------------------------------------
    PRE_FAILURE_WARRANTY		= $1 ;
    ON_LINE_COLLECTION			= $2 ;
    PERFORMANCE_ATTRIBUTE		= $4 ;
    ERROR_RATE_ATTRIBUTE		= $8 ;
    EVENT_COUNT_ATTRIBUTE		= $10 ;
    SELF_PRESERVING_ATTRIBUTE	= $20 ;

    NUM_ATTRIBUTE_STRUCTS		= 30 ;


// Declare a global structure to help print the data.
// NOTE: Per ATA3 and ATA4 specs, these attribute definitions are defined by the drive vendor
// and hence their attributes may vary between vendors.
const
    MAX_KNOWN_ATTRIBUTES = 12 ;

var
    pAttrNames: array [0..13] of string = (
	    'No Attribute Here','Raw Read Error Rate','Throughput Performance',
    	'Spin Up Time','Start/Stop Count','Reallocated Sector Count',
    	'Read Channel Margin','Seek Error Rate','Seek Time Performance',
    	'Power On Count','Spin Retry Count','Calibration Retry Count',
    	'Power Cycle Count','(Unknown attribute)') ;
    pAttrNames2: array [0..17] of string = (
        'Emergency Retract Cycle','Load Cycle Count','Temperature Celcius',
        'Hardware ECC','Reallocation Event Count','Current Pending Sector',
        'Off-line Uncorrectable','Ultra ATA CRC Error Rate','Multi Zone Error Rate',
        'Off Track Errors','TA Increase Count','ECC Errors',
        'Shock Count Write Ops','Shock Rate Write Ops','Unknown Attribute',
        'Spin High Current','Spin Buzz','Offline Seek Perf') ;

// buffer sizes
	AttrOutLen: integer = sizeof (TSendCmdOutParams) + READ_ATTRIBUTE_BUFFER_SIZE  ;
    ThreshOutLen: integer = sizeof (TSendCmdOutParams) + READ_THRESHOLD_BUFFER_SIZE ;
    IdOutLen: integer = sizeof (TSendCmdOutParams) + IDENTIFY_BUFFER_SIZE ;

implementation

end .
