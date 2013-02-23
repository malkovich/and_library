(********************************************************************)
(* Tomes of Delphi: Algorithms and Data Structures                  *)
(* Source code copyright (c) Julian M Bucknall, 1999-2001           *)
(* All rights reserved                                              *)
(*------------------------------------------------------------------*)
(* TDStrRes.inc                                                     *)
(* String table resource constants                                  *)
(********************************************************************)

{Note: The actual resource strings are found in TDStrRes.RC and are
       compiled with BRCC to TDStrRes.R16 and TDStrRes.R32 (the 16-bit
       and 32-bit resource files respectively).}

const
  tdeBase = $5444;
  tdePointerLongSize   = tdeBase + 0;
  tdeIndexOutOfBounds  = tdeBase + 1;
  tdeNilItem           = tdeBase + 2;
  tdeAtMaxCapacity     = tdeBase + 3;
  tdeCapacityTooLarge  = tdeBase + 4;
  tdeInvalidClassType  = tdeBase + 5;
  tdeListCannotDelete  = tdeBase + 6;
  tdeListCannotExamine = tdeBase + 7;
  tdeListInvalidIndex  = tdeBase + 8;
  tdeListIsNotSorted   = tdeBase + 9;
  tdeStackIsEmpty      = tdeBase + 10;
  tdeQueueIsEmpty      = tdeBase + 11;
  tdeInvalidNode       = tdeBase + 12;
  tdeTListIsNil        = tdeBase + 13;
  tdeTListInvalidRange = tdeBase + 14;
  tdeHashTblNoHashFunc = tdeBase + 15;
  tdeHashTblKeyNotFound= tdeBase + 16;
  tdeHashTblKeyExists  = tdeBase + 17;
  tdeHashTblIsFull     = tdeBase + 18;
  tdeHashTblNoCompare  = tdeBase + 19;
  tdeHashTblNoDir      = tdeBase + 20;
  tdeHashTblNoBuckets  = tdeBase + 21;
  tdeHashTblNoRecords  = tdeBase + 22;
  tdeHashTblDeletedBkt = tdeBase + 23;
  tdeHashTblDeletedRec = tdeBase + 24;
  tdeRSOutOfBounds     = tdeBase + 25;
  tdeRSAlreadyDeleted  = tdeBase + 26;
  tdeRSNoHeaderRec     = tdeBase + 27;
  tdeRSBadHeaderRec    = tdeBase + 28;
  tdeRSReadError       = tdeBase + 29;
  tdeRSSeekError       = tdeBase + 30;
  tdeRSWriteError      = tdeBase + 31;
  tdeRSRecIsDeleted    = tdeBase + 32;
  tdeRSFlushError      = tdeBase + 33;
  tdeRandRangeError    = tdeBase + 34;
  tdeSkpLstDupItem     = tdeBase + 35;
  tdeSkpLstItemMissing = tdeBase + 36;
  tdeSkpLstNoCompare   = tdeBase + 37;
  tdeBinTree2Children  = tdeBase + 38;
  tdeBinTreeHasChild   = tdeBase + 39;
  tdeBinTreeItemMissing= tdeBase + 40;
  tdeBinTreeDupItem    = tdeBase + 41;
  tdePriQueueNoCompare = tdeBase + 42;
  tdeLZNoAction        = tdeBase + 43;
  tdeLZEncodeBadMethod = tdeBase + 44;
  tdeLZDecodeBadMethod = tdeBase + 45;
  tdeLZSWBadWrite      = tdeBase + 46;
  tdeLZBadEncodedStream= tdeBase + 47;
  tdeLZProblemReading  = tdeBase + 48;
  tdeLZProblemWriting  = tdeBase + 49;
  tdeInStreamWrite     = tdeBase + 50;
  tdeInStreamRead      = tdeBase + 51;
  tdeOutStreamRead     = tdeBase + 52;
  tdeOutStreamWrite    = tdeBase + 53;
  tdeStreamBadOrigin   = tdeBase + 54;
  tdeOutStreamSeek     = tdeBase + 55;
  tdeInBitStreamRead   = tdeBase + 56;
  tdeOutBitStreamWrite = tdeBase + 57;
  tdeHuffBadEncodedStrm= tdeBase + 58;
  tdeSplyBadEncodedStrm= tdeBase + 59;
  tdeStateMisMatchQuote= tdeBase + 60;
  tdeStateBadCSV       = tdeBase + 61;
  tdeDequeIsEmpty      = tdeBase + 62;
  tdeRegexParseError   = tdeBase + 63;
  tdeNoStream          = tdeBase + 64;

