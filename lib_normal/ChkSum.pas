//*****************************************************************//
//                                                                 //
//  Checksum Algorithms                                            //
//  Copyright?BrandsPatch LLC                                     //
//  http://www.explainth.at                                        //
//                                                                 //
//  All Rights Reserved                                            //
//                                                                 //
//  Permission is granted to use, modify and redistribute          //
//  the code in this Delphi unit on the condition that this        //
//  notice is retained unchanged.                                  //
//                                                                 //
//                                                                 //
//  BrandsPatch declines all responsibility for any losses,        //
//  direct or indirect, that may arise  as a result of using       //
//  this code.                                                     //
//                                                                 //
//*****************************************************************//
{
Checksum Algorithms
A checksum is a computed value that is representative of the data on which the computation is based. It is not possible to recover the original data from the checksum value. In Delphi applications, checksums can be used in a variety of ways. To mention just two

Use a 32 bit checksum at the start of data files streamed out to disk. When the user attempts to open the file, recompute the checksum for the data starting at byte offset 4 and compare it with the stored value. This way you can deal with corrupted files before you attempt to read them. This is far better than attempting to read the file anyway and then dealing with all the exceptional conditions that are liable to arise if it turns out to be corrupted.
In most applications there will typically be many ways in which a user can modify the underlying data. When this happens the user must be prompted to save the data before exiting the application. A naive approach is to set a Changed flag each time the user edits text, checks/unchecks a checkbox etc. The problem is that the user could just as easily undo the change by reversing the operation. However, the Changed flag would remain set and you would end up needlessly warning the user about the need to save his/her work. A better approach is to calculate an initial checksum for the data and only prompt for a save if it is observed to have changed.

}
unit ChkSum;

interface

uses Windows,SysUtils;

type TAdlerBytes = array[1..MAXINT] of Byte;
     TAdlerArray = array[0..16] of Integer;
     PAdlerBytes = ^TAdlerBytes;

function Adler32(PAB:PAdlerBytes;ACount:Integer):DWORD;
function Elf32(PAB:PAdlerBytes;ACount:Integer):DWORD;
function FNV32(PAB:PAdlerBytes;ACount:Integer):DWORD;

implementation

uses Classes;

const MaxWordPrime = 65521;

function Adler32(PAB:PAdlerBytes;ACount:Integer):DWORD;
var i,j,k:Integer;
begin
  i:=1;
  j:=0;

  for k:=1 to ACount do
  begin
    i:=(i + PAB^[k]) mod MaxWordPrime;
    j:= (i + j) mod MaxWordPrime;
  end;
  Result:=(j shl 16) + i;
  //Named after its inventor, Mark Adler
  //read up on this algorithm at http://en.wikipedia.org/wiki/Adler-32
end;

function FNV32(PAB:PAdlerBytes;ACount:Integer):DWORD;
var i,APrime:DWORD;
begin
  APrime:= 16777619;
  Result:=2166136261;
  for i:=1 to ACount do
  begin
    Result:=Result*APrime;
    Result:=Result xor PAB^[i];
  end;
  //FNV stands for Fowler-Noll-Vo.
  //Read up on FNV at http://www.isthe.com/chongo/tech/comp/fnv
end;

function Elf32(PAB:PAdlerBytes;ACount:Integer):DWORD;
var i,j:DWORD;
begin
  Result:=0;
  for i:=1 to ACount do
  begin
    Result:=(Result shl 4) + PAB^[i];
    j:=Result and $F0000000;
    if (j <> 0) then Result:=Result xor (j shr 24);
    Result:=Result and not(j);
  end;
  {The name ELF comes from UNIX where this algorithm was/(is?) used to
   test the validity of Executable and Linkable Format files}
end;

end.

