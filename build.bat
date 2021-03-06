@echo off
pushd %0\..

for /F "usebackq" %%i in (`cd`) do (
 set BAT_DIR="%%i"
)
call :a %BAT_DIR%

stack build :ccsc --copy-bins --local-bin-path .
echo finished
pause
exit

:a 
setlocal
set k=%~nx1
echo %k% > bat\ver.tmp
for /F "usebackq" %%j in (` orthotest --getver ^< bat\ver.tmp `) do (
 echo #define VERSION "%%j" > Camphor\version.h
 del bat\ver.tmp
)
endlocal
exit /b