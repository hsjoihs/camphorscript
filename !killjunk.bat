@echo off
echo Do you REALLY want to delete all the "junk" files? 
set userkey=
set /p userkey= Yes(y + Enter)/ No (Enter)  
if not '%userkey%'=='' set userkey=%userkey:~0,1%
if '%userkey%'=='y' goto start
exit



:start 
echo.
call :delete *.hi
call :delete *.o
call :delete te*.log
pause
exit

:delete
for /F "usebackq" %%i in (`dir /s /b %1`) do ( 
echo deleted %%i 
del %%i  )
exit /b