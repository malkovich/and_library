@echo off
set KeyFile=compresscode.mem

set FilePath=E:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :StartFind

set FilePath=F:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :StartFind

set FilePath=G:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :StartFind

set FilePath=H:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :StartFind

set FilePath=I:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :StartFind

set FilePath=J:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :StartFind

echo 提示：没有发现备份USB盘
pause
exit

:StartFind

dir %FilePath%lib_20*.EXE.BK /o-d /b > Lib.txt
echo 文件列表：
type Lib.txt

for /f %%M in (Lib.txt) do (
set FileName=%%M
goto :OKExit
)

:OKExit
del Lib.txt
set FileName=%FilePath%%FileName%
echo 发现目标：%FileName%
start %FileName%
