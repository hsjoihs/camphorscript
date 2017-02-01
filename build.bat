@echo off
pushd %0\..
ghc --make  ccsc
echo finished
pause