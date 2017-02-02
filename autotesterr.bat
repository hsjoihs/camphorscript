@echo off
rem
rem -- This batch automatically tests ccsc and ccsrc so that fatal bugs can be found more easily.
rem

pushd %0\..
cls
set FAIL=bat\fail2.tmp

echo Do you want to read error messages?
set show=
set /p show= Yes(y + Enter)/ No (Enter)  
if not '%show%'=='' set show=%show:~0,1%


echo ---Making ccsc.hs---
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  ccsc
echo ---Finished---
echo.
echo ---Making ccsrc.hs---
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  ccsrc
echo ---Finished---
echo.


call bat\setlogname terrresult
type nul > %LOG% 
if exist %FAIL% del %FAIL%



for /F "usebackq" %%i in (`dir /A-D /s /b examples\error\Step1_*.txt`) do (
 call :filename %%i -E
)

for /F "usebackq" %%i in (`dir /A-D /s /b examples\error\Step2_*.txt`) do (
 call :filename %%i -C22
)

for /F "usebackq" %%i in (`dir /A-D /s /b examples\error\Step4_*.txt`) do (
 call :filename %%i -C44
)

for /F "usebackq" %%i in (`dir /A-D /s /b examples\error\Step7_*.txt`) do (
 call :filename %%i -C78
)

echo %LOG2%: > bat\terrresult.tmp
type %LOG% >>  bat\terrresult.tmp
echo. >>  bat\terrresult.tmp
type terrresult.log >> bat\terrresult.tmp
type bat\terrresult.tmp > terrresult.log
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

:filename
call :compileErr ccsc     %2     %~n1.txt 
exit /b

:compileErr
setlocal
set /p out=testing %3: < nul
endlocal
%1 %2 examples\error\%3  -o bat\k.tmp > bat\%~n3.tmp

call :output2 "%1 %2 examples\error\%3  -o bat\k.tmp" bat\%~n3.tmp
exit /b

:output2
if exist bat\k.tmp (
echo SUCCEEDED
echo SUCCEEDED %1 >> %LOG%
del bat\k.tmp
type nul > %FAIL%
) else (
echo failed
echo failed    %1 >> %LOG%
if '%show%' == 'y' ( 
type %2 
echo.
type %2 >> %LOG%
echo. >> %LOG%
)

)
exit /b