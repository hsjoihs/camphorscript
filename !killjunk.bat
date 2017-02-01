@echo off

call :delete *.hi
call :delete *.o
call :delete testresult*.log

pause
exit


:delete
for /F "usebackq" %%i in (`dir /s /b %1`) do ( 
echo deleted %%i 
del %%i  )
exit /b