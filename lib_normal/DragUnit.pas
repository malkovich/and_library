unit DragUnit;

interface
uses
  windows,classes,forms ,shellapi;


//得到拖放的文件个数
function GetDragFileCount(hDrop: Cardinal): Integer;

//得到拖放的文件名，通过FileIndex来指定文件编号，默认为第一个文件 
function GetDragFileName(hDrop: Cardinal; FileIndex: Integer = 1): string;

implementation 


function GetDragFileCount(hDrop: Cardinal): Integer; 
const 
  DragFileCount=High(Cardinal); 
begin 
  Result:= DragQueryFile(hDrop, DragFileCount, nil, 0); 
end; 

function GetDragFileName(hDrop: Cardinal; FileIndex: Integer = 1): string; 
const 
  Size=255; 
var 
  Len: Integer; 
  FileName: string; 
begin 
  SetLength (FileName, Size); 
  Len:= DragQueryFile(hDrop, FileIndex-1, PChar(FileName), Size); 
  SetLength (FileName, Len); 
  Result:= FileName; 
end; 

end. 

