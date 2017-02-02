@echo off
rem
rem -- This batch automatically tests ccsc so that fatal bugs can be found more easily.
rem
set FAIL="bat\failed2.tmp"
pushd %0\..
cls

echo ---Making ccsc.hs---
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  ccsc
echo ---Finished---
echo.

call bat\setlogname optoresult
type nul > %LOG% 
if exist %FAIL% del %FAIL%
for /F "usebackq eol=; delims=" %%i in (`type bat\O24list.txt`)  do ( call :tester -C24 PCS      NL    O24test  %%i )

call bat\jigo optoresult
pause
exit
rem #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$
rem #$#$#$#$#$#$# MAIN THINGS END HERE $#$#$#$#$#$#$
rem #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$

rem -C48 CCS BF_c C48test %%i
:tester 
setlocal
set option=%1
set frm=%2
set to=%3
set tmp=%4
set name=%5
set num=%6
call bat\compile examples2 ccsc %option% -fincdecmerge  __%frm%\%name%__%frm%.txt   %tmp%%num%    __%to%\%name%__%to%.txt 
endlocal
exit /b