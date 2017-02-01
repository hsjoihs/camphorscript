@echo off
pushd ..
pushd ..
:main
set name=
set /p name=name?
call :make ccsc -E %name%
goto main
pause
exit

:make
%1 %2 examples\error\%3.txt  > examples\error\Msg_%3.txt
echo created Msg_%3.txt
exit /b

