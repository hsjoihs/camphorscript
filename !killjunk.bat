@echo off
for /F "usebackq" %%i in (`dir /s /b *.hi`) do ( 
echo deleted %%i 
del %%i )
for /F "usebackq" %%i in (`dir /s /b *.o`) do ( 
echo deleted %%i 
del %%i )
pause
exit