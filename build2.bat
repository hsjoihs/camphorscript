@echo off
pushd %0\..
stack build :ccsrc --copy-bins --local-bin-path .
echo finished
pause