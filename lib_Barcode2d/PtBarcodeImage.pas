unit PtBarcodeImage;

interface
uses windows;

//Image file reading/writing APIs and definitions.
const
  PT_IMAGERW_FAIL                   =$00000000; //An error occured in an operation.
  PT_IMAGERW_SUCCESS                =$00000001; //An operation is successful.
  PT_IMAGERW_ALLOC_ERROR            =$00000100; //Error while allocating memory.
  PT_IMAGERW_FORMAT_UNSUPPORTED     =$00000101; //The format of image is unsupported.

Type
    pPTIMAGESTRUCT=^PTIMAGESTRUCT  ;
    PTIMAGESTRUCT = record
        dwWidth:       DWORD;     //The width of the image in pixels.
        dwHeight:      DWORD;     //The height of the image in pixels.
        pBits:         PByte ;    //Pointer to the image data.
        pPalette:      PByte;     //Pointer to the palette data (RGBQUAD)for 1,4,8 bit image.
        wBitsPerPixel: Smallint   //Number of bits per pixel.
    End;

var
  PtInitImage: Procedure (pImage : pPTIMAGESTRUCT); stdcall;
  PtLoadImage: Function (fileName : String; pImage : pPTIMAGESTRUCT; FrameIndex: DWORD) : Integer; stdcall;
  PtSaveImage: Function ( fileName : String; pImage : pPTIMAGESTRUCT) : Integer; stdcall;
  PtShowImage: Function (pImage : pPTIMAGESTRUCT; hDC : HDC; StartX:integer; StartY : integer; scale : double) :Integer; stdcall;
  PtCreateImage: Function ( pImage : pPTIMAGESTRUCT; ImageSize: DWORD; PaletteSize:DWORD ) : Integer; stdcall;
  PtFreeImage: Procedure (pImage : pPTIMAGESTRUCT); stdcall;
  PtGetImageFrames: Function (fileName : String) : Integer; stdcall;

implementation

end.
