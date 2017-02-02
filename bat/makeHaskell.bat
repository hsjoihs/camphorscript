con 2> nul
set notfound=%errorlevel%

echo ---Making %1.hs---
ghc --make  %1 > nul 2>&1
set err1=%errorlevel% 

rem -- if command is not found, retry.
if %err1% == %notfound% (
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make  %1
set err1=%errorlevel%
)

if not %errorlevel% == 0 (
echo ---Failed, retry.---
echo.
rem ---pause for a second
ping -n 2 localhost > nul 
call :make %1
)
if     %errorlevel% == 0 (
echo ---Successful--- 
echo.
)
exit /b