@echo off
rem
rem -- This batch automatically tests ccsc and ccsrc so that fatal bugs can be found more easily.
rem
set FAIL="bat\failed.tmp"
pushd %0\..
cls

echo ---Making ccsc.hs---
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  ccsc
echo ---Finished---
echo.
echo ---Making ccsrc.hs---
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  ccsrc
echo ---Finished---
echo.

call bat\setlogname testresult
type nul > %LOG% 
if exist %FAIL% del %FAIL%
for /F "usebackq eol=; delims=" %%i in (`type bat\Elist.txt`)    do ( call :tester ccsc  -E   CS       PCS     C11test  %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C22list.txt`)  do ( call :tester ccsc  -C22 PCS      HCCS    C22test  %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C33list.txt`)  do ( call :tester ccsc  -C33 HCCS     CCS_for C33test  %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C48list.txt`)  do ( call :tester ccsc  -C48 CCS      BF_c    C48test  %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C48list2.txt`) do ( call :tester ccsc  -C48 CCS_rev  BF_c    C48testr %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C48list3.txt`) do ( call :tester ccsc  -C48 CCS_for  BF_c    C48testf %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C88list1.txt`) do ( call :tester ccsc  -C88 BF       BF_c    C88test  %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C88list2.txt`) do ( call :tester ccsc  -C88 BF_i     BF_c    C88testi %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C78list1.txt`) do ( call :tester ccsc  -C78 ND       BF_c    C78test  %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\C78list2.txt`) do ( call :tester ccsc  -C78 ND_rev   BF_c    C78testr %%i )

for /F "usebackq eol=; delims=" %%i in (`type bat\R88list.txt`)  do ( call :tester ccsrc -C88 BF_c     BF_i    R88test  %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\R87list.txt`)  do ( call :tester ccsrc -C87 BF_c     ND_rev  R87test  %%i )
for /F "usebackq eol=; delims=" %%i in (`type bat\R84list.txt`)  do ( call :tester ccsrc -C84 BF_c     CCS_rev R84test  %%i )

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
rem ccsc -C48 CCS BF_c C48test %%i
:tester 
setlocal
set compiler=%1
set option=%2
set frm=%3
set to=%4
set tmp=%5
set name=%6
set num=%7
call :compile %compiler%    %option%   __%frm%\%name%__%frm%.txt   %tmp%%num%    __%to%\%name%__%to%.txt 
endlocal
exit /b