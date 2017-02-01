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