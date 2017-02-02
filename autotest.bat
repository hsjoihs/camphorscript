@echo off
rem
rem -- This batch automatically tests ccsc and ccsrc so that fatal bugs can be found more easily.
rem
set FAIL="bat\failed.tmp"
pushd %0\..
cls

call bat\makeHaskell ccsc
call bat\makeHaskell ccsrc

call bat\setlogname testresult
type nul > %LOG% 
if exist %FAIL% del %FAIL%

rem  :compile compiler option source                  tmp        judger
call :compile ccsc     -C48   xIsNumber__CCS.txt      C48test    xIsNumber__BF_c.txt
call :compile ccsc     -C48   qEscSeq__CCS.txt        C48test2   qEscSeq__BF_c.txt
call :compile ccsc     -C48   qScope__CCS.txt         C48test3   qScope__BF_c.txt
call :compile ccsc     -C48   xAtoI__CCS.txt          C48test4   xAtoI__BF_c.txt

call :compile ccsc     -C88   xIsNumber__BF.txt       C88test    xIsNumber__BF_c.txt
call :compile ccsc     -C88   xIsNumber__BF_i.txt     C88test2   xIsNumber__BF_c.txt
call :compile ccsc     -C88   xAtoI__BF_i.txt         C88test3   xAtoI__BF_c.txt
call :compile ccsc     -C88   xEcho__BF_i.txt         C88test4   xEcho__BF_c.txt
call :compile ccsc     -C88   xEcho__BF.txt           C88test5   xEcho__BF_c.txt

call :compile ccsc     -C78   xIsNumber__ND_rev.txt   C78test    xIsNumber__BF_c.txt
call :compile ccsc     -C78   xIsNumber__ND.txt       C78test2   xIsNumber__BF_c.txt
call :compile ccsc     -C78   xAtoI__ND_rev.txt       C78test3   xAtoI__BF_c.txt
call :compile ccsc     -C78   xAtoI__ND.txt           C78test4   xAtoI__BF_c.txt
call :compile ccsc     -C78   xEcho__ND.txt           C78test5   xEcho__BF_c.txt
call :compile ccsc     -C78   qEscSeq__ND.txt         C78test6   qEscSeq__BF_c.txt
call :compile ccsc     -C78   qScope__ND.txt          C78test7   qScope__BF_c.txt
call :compile ccsc     -C78   xHello__ND_rev.txt      C78test8   xHello__BF_c.txt
call :compile ccsc     -C78   xtoLower__ND_rev.txt    C78test9   xtoLower__BF_c.txt

call :compile ccsc     -E     yPreProcess__CS.txt     C11test    yPreProcessed.txt
call :compile ccsc     -E     yInclude__CS.txt        C11test2   yIncluded.txt
call :compile ccsc     -E     yDualInclude__CS.txt    C11test3   yDualIncluded.txt
call :compile ccsc     -E     yRepInclude__CS.txt     C11test4   yRepIncluded.txt
call :compile ccsc     -E     yIfElseTrue__CS.txt     C11test5   yIfElseTrued.txt
call :compile ccsc     -E     yIfElseFalse__CS.txt    C11test6   yIfElseFalsed.txt

call :compile ccsrc    -C88   xIsNumber__BF_c.txt     R88test    xIsNumber__BF_i.txt
call :compile ccsrc    -C88   xEcho__BF_c.txt         R88test2   xEcho__BF_i.txt
call :compile ccsrc    -C88   xHello__BF_c.txt        R88test3   xHello__BF_i.txt
call :compile ccsrc    -C88   xtoLower__BF_c.txt      R88test4   xtoLower__BF_i.txt

call :compile ccsrc    -C87   xIsNumber__BF_c.txt     R87test    xIsNumber__ND_rev.txt

echo %LOG2%: > bat\testresult.tmp
type %LOG% >>  bat\testresult.tmp
echo. >>  bat\testresult.tmp
type testresult.log >> bat\testresult.tmp
type bat\testresult.tmp > testresult.log
echo.
echo %LOG2%:
type %LOG%
del %LOG%
if exist %FAIL% (
del %FAIL%
) else (
del bat\*.tmp
echo ---Deleted temporary files.---
) 

pause
exit
rem #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$
rem #$#$#$#$#$#$# MAIN THINGS END HERE $#$#$#$#$#$#$
rem #$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$#$
:compile
setlocal
set compiler=%1
set option=%2
set source=examples\%3
set tmp=bat\%4.tmp
set judger=examples\%5
set test=%compiler% %option% %source%

rem print without linefeed
set /p out=testing %test%: < nul
%test% -o %tmp%
fc /n %tmp% %judger% > nul
call :output %errorlevel% "%test% -o %tmp%"
endlocal
exit /b

:output
if     %1 == 0 (
echo success
echo success: %2 >> %LOG%
)
if not %1 == 0 (
echo FAILURE
echo FAILURE: %2 >> %LOG%
type nul > %FAIL%
) 
exit /b