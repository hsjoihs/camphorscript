@echo off
del colres5.txt
cd ..\
ccsc -C22 _temp\collision.txt -o _temp\colres5.txt
pause