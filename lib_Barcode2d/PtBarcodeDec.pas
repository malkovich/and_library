unit PtBarcodeDec;

interface

uses Windows, PtBarcodeImage;

///////////////////////////////////////////////////////////////////////////////////////////////////
//PDF417 symbol reading APIs and definitions
///////////////////////////////////////////////////////////////////////////////////////////////////
 const
  PT_PDF417DECODE_FAIL              =$00000000; //An error occured in an operation.
  PT_PDF417DECODE_SUCCESS           =$00000001; //An operation is successful.
  PT_PDF417DECODE_ALLOC_ERROR       =$00000300; //Error while allocating the memory.
  PT_PDF417DECODE_IMAGE_INVALID     =$00000301; //The image to be decode is invalid.
  PT_PDF417DECODE_PARAMETERS_INVALID=$00000302; //The parameters input to a function are invalid.

Type
  pPTDECODEPARASTRUCT=^PTDECODEPARASTRUCT ;
  PTDECODEPARASTRUCT = Record
    dwStartX:  DWORD;     //The start X-coordinate in pixels of the search window in the image to decode the symbol.
    dwStartY:  DWORD;     //The start Y-coordinate in pixels of the search window in the image to decode the symbol.
    dwEndX:    DWORD;     //The end X-coordinate in pixels of the search window in the image to decode the symbol.
    dwEndY:    DWORD;     //The end Y-coordinate in pixels of the search window in the image to decode the symbol.
    dwMaxCount:DWORD;     //The maximal number of symbols to be searched. If it's set to 0 then search the all symbols.
  End;

  pPTBARCODEINFOSTRUCT=^PTBARCODEINFOSTRUCT;
  PTBARCODEINFOSTRUCT = Record
    dwX1, dwY1,              //four corners' coordinates in pixels of the barcode
    dwX2, dwY2,
    dwX3, dwY3,
    dwX4, dwY4   : DWORD;
    pData        : Pointer; //Pointer to the buffer that contains the barcode's data.
    dwDataLen    : DWORD;   //The barcode data's length in bytes.
  End;


  pPTTOTALBARCODEINFOSTRUCT=^PTTOTALBARCODEINFOSTRUCT ;
  PTTOTALBARCODEINFOSTRUCT = Record
    pInfoList     : pPTBARCODEINFOSTRUCT; //Pointer to the start address of the list of barcodes' info.
    dwTotalCount  : DWORD;                //The number of barcode that have been decoded.
  End;

var
  PtPDF417DecodeRegister: Procedure (KeyStr : String) ; stdcall;
  PtPDF417DecodeInit: Procedure (pDecode : pPTTOTALBARCODEINFOSTRUCT) ; stdcall;
  PtPDF417Decode: Function (pImage : pPTIMAGESTRUCT; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtPDF417DecodeFromFile: Function (fileName : String; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtPDF417DecodeFromBitmap: Function ( hBitmap : HBITMAP; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtPDF417DecodeFree: Procedure (pDecode : pPTTOTALBARCODEINFOSTRUCT); stdcall;
  




///////////////////////////////////////////////////////////////////////////////////////////////////
//QR symbol reading APIs and definitions
///////////////////////////////////////////////////////////////////////////////////////////////////
 const
  PT_QRDECODE_FAIL              =$00000000; //An error occured in an operation.
  PT_QRDECODE_SUCCESS           =$00000001; //An operation is successful.
  PT_QRDECODE_ALLOC_ERROR       =$00000300; //Error while allocating the memory.
  PT_QRDECODE_IMAGE_INVALID     =$00000301; //The image to be decode is invalid.
  PT_QRDECODE_PARAMETERS_INVALID=$00000302; //The parameters input to a function are invalid.

var
  PtQRDecodeRegister: Procedure (KeyStr : String) ; stdcall;
  PtQRDecodeInit: Procedure (pDecode : pPTTOTALBARCODEINFOSTRUCT) ; stdcall;
  PtQRDecode: Function (pImage : pPTIMAGESTRUCT; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtQRDecodeFromFile: Function (fileName : String; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtQRDecodeFromBitmap: Function ( hBitmap : HBITMAP; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtQRDecodeFree: Procedure (pDecode : pPTTOTALBARCODEINFOSTRUCT); stdcall;





///////////////////////////////////////////////////////////////////////////////////////////////////
//Data Matrix symbol reading APIs and definitions
///////////////////////////////////////////////////////////////////////////////////////////////////
 const
  PT_DMDECODE_FAIL              =$00000000; //An error occured in an operation.
  PT_DMDECODE_SUCCESS           =$00000001; //An operation is successful.
  PT_DMDECODE_ALLOC_ERROR       =$00000300; //Error while allocating the memory.
  PT_DMDECODE_IMAGE_INVALID     =$00000301; //The image to be decode is invalid.
  PT_DMDECODE_PARAMETERS_INVALID=$00000302; //The parameters input to a function are invalid.

var
  PtDMDecodeRegister: Procedure (KeyStr : String) ; stdcall;
  PtDMDecodeInit: Procedure (pDecode : pPTTOTALBARCODEINFOSTRUCT) ; stdcall;
  PtDMDecode: Function (pImage : pPTIMAGESTRUCT; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtDMDecodeFromFile: Function (fileName : String; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtDMDecodeFromBitmap: Function ( hBitmap : HBITMAP; pPara: pPTDECODEPARASTRUCT; pDecode:pPTTOTALBARCODEINFOSTRUCT) : Integer; stdcall;
  PtDMDecodeFree: Procedure (pDecode : pPTTOTALBARCODEINFOSTRUCT); stdcall;

implementation
end.



