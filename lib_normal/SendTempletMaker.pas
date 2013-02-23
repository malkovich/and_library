unit SendTempletMaker;

interface
uses
  windows, classes;

CONST
  COVER_TYPE_BYTE   = 1;
  COVER_TYPE_WORD   = 2;
  COVER_TYPE_DWORD  = 4;
  COVER_TYPE_DOUBLE = 8;
  COVER_TYPE_STRING = 16;

  DESTRIPT_LENGTH   = 16;

type
  TMainTemplet = packed record
    Length :WORD;
    Destript  :array[0..DESTRIPT_LENGTH - 1] of char;
    Buffer :array[0..0] of char;
  end;

  TModifyPoint = packed record
    Length :WORD;
    AimOffset :WORD;
    CoverType :WORD;
    Destript  :array[0..DESTRIPT_LENGTH - 1] of char;
    CoverBuffer :array[0..0] of char;
  end;

  TTempletStruc = packed record
    Length      :WORD;
    BlockCount  :WORD;
    Buffer      :array[0..0] of char;
  end;

implementation

end.
