@echo off

set KeyFile=compresscode.mem
set ToPackDir=lib

call deldcu.bat



set TimeStr=%TIME::=-%
set TimeStr=%TimeStr:.=-%
set TimeStr=%date:~0,10%-%TimeStr%
set TimeStr=%TimeStr: =-%

set FilePath=E:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :RunWinRAR

set FilePath=F:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :RunWinRAR

set FilePath=G:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :RunWinRAR

set FilePath=H:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :RunWinRAR

set FilePath=I:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :RunWinRAR

set FilePath=J:\
Set FileName=%FilePath%%KeyFile%
if exist %FileName% goto :RunWinRAR

echo 提示：没有发现备份USB盘
pause
exit

:RunWinRAR

set FileName=%ToPackDir%_%TimeStr%.EXE
set AimFileName=%FilePath%%FileName%
set WinrarPath=%ProgramFiles%\WinRAR\

echo FileName = %FileName%
echo ToPackDir = %ToPackDir%
echo AimFileName = %AimFileName%
echo WinrarPath = %WinrarPath%

path %WinrarPath%
WinRAR.exe a -sfxDefault.SFX -iiconlib.ico -r -m1 %FileName% ..\%ToPackDir%
WinRAR.exe c -zsetup.txt %FileName%
move %FileName% %AimFileName%.BK

