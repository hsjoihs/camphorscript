@echo off
pushd %0\..
goto CCSC

:CCSC
echo ---Making ccsc---
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  ccsc

if not %errorlevel% == 0 goto OnError 
if     %errorlevel% == 0 goto OnSuccess

:OnError
echo ---Failed, retry.---
goto CCSC

:OnSuccess
echo ---Successful--- 
goto CCSRC

:CCSRC
echo ---Making ccsrc---
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  ccsrc

if not %errorlevel% == 0 goto OnErrorR 
if     %errorlevel% == 0 goto OnSuccessR

:OnErrorR
echo ---Failed, retry.---
goto CCSRC

:OnSuccessR
echo ---Successful---
goto TEST





:TEST
set LOG=testresult.log
echo. > %LOG%
ccsc -C48 examples/xIsNumber__CCS.txt -o examples/C48test.tmp
fc examples\C48test.tmp examples\xIsNumber__BF_c.txt
if     %errorlevel% == 0 echo correct compile in options "-C48 examples/xIsNumber__CCS.txt -o examples/C48test.tmp" >> %log%
if not %errorlevel% == 0 echo wrong   compile in options "-C48 examples/xIsNumber__CCS.txt -o examples/C48test.tmp" >> %log%


ccsc -C88 examples/xIsNumber__BF.txt -o examples/C88test.tmp
fc examples\C88test.tmp examples\xIsNumber__BF_c.txt
if     %errorlevel% == 0 echo correct compile in options "-C88 examples/xIsNumber__BF.txt -o examples/C88test.tmp" >> %log%
if not %errorlevel% == 0 echo wrong   compile in options "-C88 examples/xIsNumber__BF.txt -o examples/C88test.tmp" >> %log%


type %log%
del examples\*.tmp

pause
exit