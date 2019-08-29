@echo off
pushd %0\..
stack build :orthotest --copy-bins --local-bin-path .
echo finished
pause