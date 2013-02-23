unit PtBarcodeEnc;

interface
uses Windows, PtBarcodeImage;

////////////////////////////////////////////////////////////////////////////////
//PDF417 symbol writing APIs and definitions
////////////////////////////////////////////////////////////////////////////////
const
  PT_PDF417ENCODE_FAIL              =$00000000; //An operation is Failed.
  PT_PDF417ENCODE_SUCCESS           =$00000001; //An operation is successful.
  PT_PDF417ENCODE_ALLOC_ERROR       =$00000200; //Error while allocating the memory.
  PT_PDF417ENCODE_DATA_BIG          =$00000201; //Data to be encoded is too big.
  PT_PDF417ENCODE_SIZE_SMALL        =$00000202; //The size of image to be pasted the symbol is too small.
  PT_PDF417ENCODE_IMAGE_INVALID     =$00000203; //The image to be pasted is invalid.

  //PDF417 ECC level constants
  PT_PDF417_ECCLEVEL_0 = 0;       //Use ECC level 0. This uses 2   codewords for error correction.
  PT_PDF417_ECCLEVEL_1 = 1;       //Use ECC level 1. This uses 4   codewords for error correction.
  PT_PDF417_ECCLEVEL_2 = 2;       //Use ECC level 2. This uses 8   codewords for error correction.
  PT_PDF417_ECCLEVEL_3 = 3;       //Use ECC level 3. This uses 16  codewords for error correction.
  PT_PDF417_ECCLEVEL_4 = 4;       //Use ECC level 4. This uses 32  codewords for error correction.
  PT_PDF417_ECCLEVEL_5 = 5;       //Use ECC level 5. This uses 64  codewords for error correction.
  PT_PDF417_ECCLEVEL_6 = 6;       //Use ECC level 6. This uses 128 codewords for error correction.
  PT_PDF417_ECCLEVEL_7 = 7;       //Use ECC level 7. This uses 256 codewords for error correction.
  PT_PDF417_ECCLEVEL_8 = 8;       //Use ECC level 8. This uses 512 codewords for error correction.
  PT_PDF417_ECC_PERCENT= 9;       //Use the percentage to determine the ECC level.

Type
  pPTPDF417ENCODESTRUCT=^PTPDF417ENCODESTRUCT;
  PTPDF417ENCODESTRUCT = record
      pData :              PChar ;    //Pointer to the data to be encoded.
      nDataLength :        Integer	; //Length of the data in bytes.
      bisTruncated :       Integer	; //Writes truncated PDF417 symbols.
      wEccPercent :        Smallint	; //Determines the error correction level by percentage.
      wEccLevel  :         Smallint	; //Determines the ECC level for encoding a PDF417 symbol.
      wCols  :             Smallint	; //Number of columns in the symbol.
      wRows :              Smallint	; //Number of rows in the symbol.
      wAspectHeigh :       Smallint	; //The height part of the aspect ratio of the symbol.
      wAspectWidth :       Smallint	; //The width part of the aspect ratio of the symbol.
      wXModule :           Smallint	; //The smallest element width in pixels.
      wModuleAspectRatio : Smallint	; //The ratio of a row height to XModule .
      wLeftSpace :         Smallint	; //The left   space of the symbol in pixels while generating Image.
      wRightSpace :        Smallint	; //The right  space of the symbol in pixels while generating Image.
      wTopSpace :          Smallint	; //The top    space of the symbol in pixels while generating Image.
      wBottomSpace :       Smallint	; //The bottom space of the symbol in pixels while generating Image.
  End;

var
  PtPDF417EncodeRegister: Procedure (KeyStr : String) ; stdcall;
  PTPDF417EncodeInit: Procedure (pEncode : pPTPDF417ENCODESTRUCT); stdcall;
  PtPDF417Encode: Function (pEncode : pPTPDF417ENCODESTRUCT; pImage : pPTIMAGESTRUCT) : Integer; stdcall;
  PtPDF417EncodeToImage: Function (pEncode : pPTPDF417ENCODESTRUCT; pImage : pPTIMAGESTRUCT; StartX:Integer; StartY : Integer) : Integer; stdcall;


  


////////////////////////////////////////////////////////////////////////////////
//QR Code symbol writing APIs and definitions
////////////////////////////////////////////////////////////////////////////////
const
  PT_QRENCODE_FAIL             =$00000000; //An operation is Failed.
  PT_QRENCODE_SUCCESS          =$00000001; //An operation is successful.
  PT_QRENCODE_ALLOC_ERROR      =$00000200; //Error while allocating the memory.
  PT_QRENCODE_DATA_BIG         =$00000201; //Data to be encoded is too big.
  PT_QRENCODE_SIZE_SMALL       =$00000202; //The size of image to be pasted the symbol is too small.
  PT_QRENCODE_IMAGE_INVALID    =$00000203; //The image to be pasted is invalid.

  //QR Code ECC level constants
  PT_QR_ECCLEVEL_L	        =$0001; //Use ECC level L. (7% )
  PT_QR_ECCLEVEL_M          =$0000; //Use ECC level M. (15%)
  PT_QR_ECCLEVEL_Q          =$0003; //Use ECC level Q. (25%)
  PT_QR_ECCLEVEL_H	        =$0002; //Use ECC level H. (30%)

  //QR Code version constants
  PT_QR_VERSION_AUTO         =$0000; //Determine the version by the engine,then use the smallest version that can contain the data.

  //QR Code mask number constants
  PT_QR_MASKNUMBER_AUTO      =$0008; //Determine the mask number by the engine.

Type
    pPTQRENCODESTRUCT=^PTQRENCODESTRUCT;
    PTQRENCODESTRUCT = record
        pData :              PChar ;    //Pointer to the data to be encoded.
        nDataLength :        Integer ;  //Length of the data in bytes.
        wVersion:            Smallint ;  //The version of the QR Code.
        wMaskNumber:         Smallint ;	//The mask number of the QR Code.
        wEccLevel  :         Smallint	; //Determines the ECC level for encoding a QR Code symbol.
        wModule  :           Smallint	; //The smallest element size in pixels.
        wGroupTotal :        Smallint	; //The number of symbols that belong to the group.
        wGroupIndex :        Smallint	; //The index of the symbol in the group
        wLeftSpace :         Smallint	; //The left   space of the symbol in pixels while generating Image.
        wRightSpace :        Smallint	; //The right  space of the symbol in pixels while generating Image.
        wTopSpace :          Smallint	; //The top    space of the symbol in pixels while generating Image.
        wBottomSpace :       Smallint	; //The bottom space of the symbol in pixels while generating Image.
End;

var
  PtQREncodeRegister: Procedure (KeyStr : String); stdcall;
  PTQREncodeInit: Procedure (pEncode : pPTQRENCODESTRUCT) ; stdcall;
  PtQREncode: Function (pEncode : pPTQRENCODESTRUCT; pImage : pPTIMAGESTRUCT) : Integer; stdcall;
  PtQREncodeToImage: Function (pEncode : pPTQRENCODESTRUCT; pImage : pPTIMAGESTRUCT; StartX:Integer; StartY : Integer) : Integer; stdcall;





////////////////////////////////////////////////////////////////////////////////
//DataMatrix symbol writing APIs and definitions
////////////////////////////////////////////////////////////////////////////////
const
  PT_DMENCODE_FAIL             =$00000000; //An operation is Failed.
  PT_DMENCODE_SUCCESS          =$00000001; //An operation is successful.
  PT_DMENCODE_ALLOC_ERROR      =$00000200; //Error while allocating the memory.
  PT_DMENCODE_DATA_BIG         =$00000201; //Data to be encoded is too big.
  PT_DMENCODE_SIZE_SMALL       =$00000202; //The size of image to be pasted the symbol is too small.
  PT_DMENCODE_IMAGE_INVALID    =$00000203; //The image to be pasted is invalid.

  PT_DM_SQUARE_AUTO = 0;     //Use the smallest square size that can contain the data.
  PT_DM_10x10 = 1;           //Data Matrix Type 10x10.
  PT_DM_12x12 = 2;           //Data Matrix Type 12x12.
  PT_DM_14x14 = 3;           //Data Matrix Type 14x14.
  PT_DM_16x16 = 4;           //Data Matrix Type 16x16.
  PT_DM_18x18 = 5;           //Data Matrix Type 18x18.
  PT_DM_20x20 = 6;           //Data Matrix Type 20x20.
  PT_DM_22x22 = 7;           //Data Matrix Type 22x22.
  PT_DM_24x24 = 8;           //Data Matrix Type 24x24.
  PT_DM_26x26 = 9;           //Data Matrix Type 26x26.
  PT_DM_32x32 = 10;          //Data Matrix Type 32x32.
  PT_DM_36x36 = 11;          //Data Matrix Type 36x36.
  PT_DM_40x40 = 12;          //Data Matrix Type 40x40.
  PT_DM_44x44 = 13;          //Data Matrix Type 44x44.
  PT_DM_48x48 = 14;          //Data Matrix Type 48x48.
  PT_DM_52x52 = 15;          //Data Matrix Type 52x52.
  PT_DM_64x64 = 16;          //Data Matrix Type 64x64.
  PT_DM_72x72 = 17;          //Data Matrix Type 72x72.
  PT_DM_80x80 = 18;          //Data Matrix Type 80x80.
  PT_DM_88x88 = 19;          //Data Matrix Type 88x88.
  PT_DM_96x96 = 20;          //Data Matrix Type 96x96.
  PT_DM_104x104 = 21;        //Data Matrix Type 104x104.
  PT_DM_120x120 = 22;        //Data Matrix Type 120x120.
  PT_DM_132x132 = 23;        //Data Matrix Type 132x132.
  PT_DM_144x144 = 24;        //Data Matrix Type 144x144.
  PT_DM_8x18 = 25;           //Data Matrix Type 8x18.
  PT_DM_8x32 = 26;           //Data Matrix Type 8x32.
  PT_DM_12x26 = 27;          //Data Matrix Type 12x26.
  PT_DM_12x36 = 28;          //Data Matrix Type 12x36.
  PT_DM_16x36 = 29;          //Data Matrix Type 16x36.
  PT_DM_16x48 = 30;          //Data Matrix Type 16x48.
  PT_DM_RECTANGLE_AUTO = 31; //use the smallest rectangular size that can contain the data.

Type
    pPTDMENCODESTRUCT=^PTDMENCODESTRUCT;
    PTDMENCODESTRUCT = record
        pData :              PChar ;    //Pointer to the data to be encoded.
        nDataLength :        Integer ;  //Length of the data in bytes.
        wSymbolSize:         Smallint ; //The symbol size.
        wModule  :           Smallint	; //The smallest element size in pixels.
        wGroupTotal :        Smallint	; //The number of symbols that belong to the group.
        wGroupIndex :        Smallint	; //The index of the symbol in the group
        wFileIDHigh:         Smallint;  //The high byte of the file ID number.
        wFileIDLow:          Smallint;  //The low byte of the file ID number.
        wLeftSpace :         Smallint	; //The left   space of the symbol in pixels while generating Image.
        wRightSpace :        Smallint	; //The right  space of the symbol in pixels while generating Image.
        wTopSpace :          Smallint	; //The top    space of the symbol in pixels while generating Image.
        wBottomSpace :       Smallint	; //The bottom space of the symbol in pixels while generating Image.
  End;

var
  PtDMEncodeRegister: Procedure (KeyStr : String); stdcall;
  PTDMEncodeInit: Procedure (pEncode : pPTDMENCODESTRUCT); stdcall;
  PtDMEncode: Function (pEncode : pPTDMENCODESTRUCT; pImage : pPTIMAGESTRUCT) : Integer; stdcall;
  PtDMEncodeToImage: Function (pEncode : pPTDMENCODESTRUCT; pImage : pPTIMAGESTRUCT; StartX:Integer; StartY : Integer): Integer; stdcall;

implementation

end.
