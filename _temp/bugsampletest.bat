@echo off
del bugres5.txt > nul 2>&1
cd ..\
ccsc -C22 _temp\bugsample.txt -o _temp\bugres5.txt
pause