@echo off
cd ..\
set failed=n
ccsc -C12 examples\__CS\xAtoI__CS.txt -o _tmp\tmp1.txt 
fc _tmp\tmp1.txt _tmp\tmp_xAtoI_dump.txt
if not %errorlevel% == 0 ( set failed=y ) 

ccsc -C12 examples\__CS\xIsNumber__CS.txt -o _tmp\tmp2.txt 
fc _tmp\tmp2.txt _tmp\tmp_xIsNumber_dump.txt

if not %errorlevel% == 0 ( set failed=y )

if not %failed%==y ( del _tmp\tmp1.txt _tmp\tmp2.txt )
pause