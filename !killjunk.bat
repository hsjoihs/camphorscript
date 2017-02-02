@echo off
echo Do you REALLY want to delete all the "junk" files? 
set userkey=
set /p userkey= Yes(y + Enter)/ No (Enter)  
if not '%userkey%'=='' set userkey=%userkey:~0,1%
if '%userkey%'=='y' goto start
exit

:start 
rd _tmp
echo.
for /F "usebackq" %%i in (`dir /s /b *.hi *.o `) do ( 
echo deleted %%i 
del %%i  )
exit /b
pause
exit