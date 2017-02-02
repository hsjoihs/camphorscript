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

call :compile ccsc     -C88   xIsNumber__BF.txt       C88test    xIsNumber__BF_c.txt
call :compile ccsc     -C88   xIsNumber__BF_i.txt     C88test2   xIsNumber__BF_c.txt
call :compile ccsc     -C88   xAtoI__BF_i.txt         C88test3   xAtoI__BF_c.txt
call :compile ccsc     -C88   xEcho__BF_i.txt         C88test4   xEcho__BF_c.txt

call :compile ccsc     -C78   xIsNumber__ND_rev.txt   C78test    xIsNumber__BF_c.txt
call :compile ccsc     -C78   xIsNumber__ND.txt       C78test2   xIsNumber__BF_c.txt
call :compile ccsc     -C78   xAtoI__ND_rev.txt       C78test3   xAtoI__BF_c.txt
call :compile ccsc     -C78   xEcho__ND.txt           C78test4   xEcho__BF_c.txt
call :compile ccsc     -C78   xHello__ND_rev.txt      C78test5   xHello__BF_c.txt
call :compile ccsc     -C78   xtoLower__ND_rev.txt    C78test6   xtoLower__BF_c.txt

call :compile ccsc     -E     yPreProcess__CS.txt C11test    yPreProcessed.txt
call :compile ccsc     -E     yInclude__CS.txt        C11test2   yIncluded.txt
call :compile ccsc     -E     yDualInclude__CS.txt    C11test3   yDualIncluded.txt
call :compile ccsc     -E     yRepInclude__CS.txt    C11test3   yRepIncluded.txt

call :compile ccsrc    -C88   xIsNumber__BF_c.txt     R88test    xIsNumber__BF_i.txt
call :compile ccsrc    -C87   xIsNumber__BF_c.txt     R87test    xIsNumber__ND_rev.txt

echo.
echo %LOG2%:
type %LOG%
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

echo testing %test%:
%test% -o %tmp%
fc /n %tmp% %judger% > nul
call :output %errorlevel% "%test% -o %tmp%"
endlocal
exit /b

:output
if     %1 == 0 (
echo success: %2 >> %LOG%
)
if not %1 == 0 (
echo FAILURE: %2 >> %LOG%
type nul > %FAIL%
) 
exit /b