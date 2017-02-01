@echo off
rem
rem -- This batch automatically tests ccsc and ccsrc so that fatal bugs can be found more easily.
rem

pushd %0\..
cls

call :make ccsc
call :make ccsrc


call :setlogname
echo. > %LOG% 


rem  :compile compiler option source                           tmp                judger
call :compile ccsc     -C48   examples\xIsNumber__CCS.txt      bat\C48test.tmp    examples\xIsNumber__BF_c.txt
call :compile ccsc     -C88   examples\xIsNumber__BF.txt       bat\C88test.tmp    examples\xIsNumber__BF_c.txt
call :compile ccsc     -E     examples\xPreProcessTest__CS.txt bat\C11test.tmp    examples\xPreProcessed.txt
call :compile ccsrc    -C88   examples\xIsNumber__BF_c.txt     bat\R88test.tmp    examples\xIsNumber__BF_i.txt

echo %LOG%:
type %LOG%
del bat\*.tmp
echo ---Deleted temporary files.---

pause
exit

rem #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$
rem #$#$#$#$#$#$# MAIN THINGS END HERE $#$#$#$#$#$#$
rem #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$


rem --- call :output %errorlevel% string
:output
if     %1 == 0 echo succeeded %2 >> %LOG%
if not %1 == 0 echo FAILED    %2 >> %LOG%
exit /b

rem --- call :make file
:make
echo ---Making %1.hs---
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  %1

if not %errorlevel% == 0 (
echo ---Failed, retry.---
rem ---pause for a second
ping -n 2 localhost > nul 
call :make %1
)
if     %errorlevel% == 0 (
echo ---Successful--- 
)
exit /b


rem --- call :compile compiler option source tmp judger
:compile
%1 %2 %3 -o %4
fc /n %4 %5
call :output %errorlevel% "%1 %2 %3 -o %4"
exit /b


rem Log file name
:setlogname


rem if %time:~0,1% == ' ' somehow didn't work
echo.%time:~0,1% > bat\space.tmp
fc bat\space.tmp bat\space.txt >nul
if %errorlevel% == 0  ( 
set now_HHmmSS=0%time:~1,1%-%time:~3,2%-%time:~6,2%
) else (
set now_HHmmSS=%time:~0,2%-%time:~3,2%-%time:~6,2%
)

set today_YYYYMMDD=%date:~0,4%-%date:~5,2%-%date:~8,2%
set LOG="testresult%today_YYYYMMDD%_%now_HHmmSS%.log"

exit /b