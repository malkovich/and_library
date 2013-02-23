(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDCmprBs                                                         *)
(* Standard types and constants for compression                     *)
(********************************************************************)

unit TDCmprBs;

{$I TDDefine.inc}

interface

uses
  SysUtils,
  TDBasics,
  TDStrms;

type
  TtdLZSignature = packed record {a 3-character signature string}
    case boolean of
      false : (AsLong   : longint);
      true  : (AsString : string[3]);
  end;

{Notes: the following constants define the range of values for the
        LZ77 distance and length encodings. Generally the distance
        shift value is 3, making the size of the sliding window 8192
        bytes and the maximum length to match 10 bytes. You can
        experiment by setting the shift value to 4 (sliding window is
        4096 bytes, max length is 18 bytes) or even 2 (window is 16384
        bytes, max length is 6 bytes). The bigger the sliding window,
        the slower the compression is since there's more to compare to
        get the longest match}
const
  tdcLZDistanceShift = 3;                                     {..3}
  tdcLZLengthMask = (1 shl tdcLZDistanceShift) - 1;           {..$7}
  tdcLZSlidingWindowSize = (1 shl (16 - tdcLZDistanceShift)); {..8192}
  tdcLZMaxMatchLength = tdcLZLengthMask + 3;                  {..10}
  tdcLZLookAheadSize = tdcLZMaxMatchLength;                   {..10}

type
  PtdEncodingNode = ^TtdEncodingNode;
  TtdEncodingNode = packed record
    enLeftInx  : integer;
    enRightInx : integer;
    enValue    : longint;
  end;

  TtdEncodingTree = class
    private
    protected
    public
      constructor Create;
      destructor Destroy; override;

      procedure SaveToBitStream(aBitStream : TtdOutputBitStream);
      procedure LoadFromBitStream(aBitStream : TtdInputBitStream);

      procedure GetCode(aValue : byte; aBits : PtdBooleanArray);
  end;

implementation

end.
