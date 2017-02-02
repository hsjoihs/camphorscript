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

for /F "usebackq delims=" %%i in (`type bat\autotestlist.txt`) do (
call :compile %%i 
)

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