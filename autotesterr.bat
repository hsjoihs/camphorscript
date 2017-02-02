@echo off
rem
rem -- This batch automatically tests ccsc and ccsrc so that fatal bugs can be found more easily.
rem

pushd %0\..
cls
set FAIL=bat\fail2.tmp

call bat\makeHaskell ccsc
call bat\makeHaskell ccsrc


call bat\setlogname terrresult
type nul > %LOG% 
if exist %FAIL% del %FAIL%

for /F "usebackq" %%i in (`dir /A-D /s /b examples\error\Step1_*.txt`) do (
 call :filename %%i -E
)

for /F "usebackq" %%i in (`dir /A-D /s /b examples\error\Step4_*.txt`) do (
 call :filename %%i -C44
)

for /F "usebackq" %%i in (`dir /A-D /s /b examples\error\Step7_*.txt`) do (
 call :filename %%i -C78
)

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

:filename
call :compileErr ccsc     %2     %~n1.txt 
exit /b

:compileErr
echo testing %3:
%1 %2 examples\error\%3  -o bat\k.tmp > bat\%~n3.tmp
call :output2 "%1 %2 examples\error\%3  -o bat\k.tmp"
exit /b

:output2
if exist bat\k.tmp (
echo SUCCEEDED %1 >> %LOG%
del bat\k.tmp
type nul > %FAIL%
) else (
echo failed    %1 >> %LOG%
)
exit /b