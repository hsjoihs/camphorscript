@echo off
rem
rem -- This batch automatically tests ccsc and ccsrc so that fatal bugs can be found more easily.
rem

pushd %0\..
cls

call bat\makeHaskell ccsc
call bat\makeHaskell ccsrc

call bat\setlogname testresult
echo. > %LOG% 

rem  :compile compiler option source                           tmp                judger
call :compile ccsc     -C48   examples\xIsNumber__CCS.txt      bat\C48test.tmp    examples\xIsNumber__BF_c.txt
call :compile ccsc     -C88   examples\xIsNumber__BF.txt       bat\C88test.tmp    examples\xIsNumber__BF_c.txt
call :compile ccsc     -E     examples\xPreProcessTest__CS.txt bat\C11test.tmp    examples\xPreProcessed.txt
call :compile ccsrc    -C88   examples\xIsNumber__BF_c.txt     bat\R88test.tmp    examples\xIsNumber__BF_i.txt
call :compile ccsrc    -C87   examples\xIsNumber__BF_c.txt     bat\R87test.tmp    examples\xIsNumber__ND_rev.txt
call :compile ccsc     -C78   bat\R87test.tmp                  bat\C78R87test.tmp examples\xIsNumber__BF_c.txt

echo.
echo %LOG2%:
type %LOG%
del bat\*.tmp
echo ---Deleted temporary files.---

pause
exit
rem #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$
rem #$#$#$#$#$#$# MAIN THINGS END HERE $#$#$#$#$#$#$
rem #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$
:compile
echo testing %1 %2 %3:
%1 %2 %3 -o %4
fc /n %4 %5 > nul
call :output %errorlevel% "%1 %2 %3 -o %4"
exit /b
:output
if     %1 == 0 echo succeeded %2 >> %LOG%
if not %1 == 0 echo FAILED    %2 >> %LOG%
exit /b