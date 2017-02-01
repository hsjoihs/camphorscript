@echo off
rem
rem -- This batch automatically tests ccsc and ccsrc so that fatal bugs can be found more easily.
rem

pushd %0\..
cls

call bat\makeHaskell ccsc
call bat\makeHaskell ccsrc


call bat\setlogname terrresult
echo. > %LOG% 

for /F "usebackq" %%i in (`dir /A-D /s /b examples\error\S*.txt`) do (
 call :filename %%i
)
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

:filename
call :compileErr ccsc     -E     %~n1.txt 
exit /b

:compileErr
echo testing %3:
%1 %2 examples\error\%3  -o bat\k.tmp > bat\%~n3.tmp
call :output2 "%1 %2 examples\error\%3  -o bat\k.tmp > bat\%~n3.tmp"
exit /b

:output2
if exist bat\k.tmp (
echo SUCCEEDED %1 >> %LOG%
del bat\k.tmp
) else (
echo failed    %1 >> %LOG%
)
exit /b