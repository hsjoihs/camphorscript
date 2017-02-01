@echo off
pushd %0\..
ghc --make  ccsc
ghc --make  ccsrc
echo finished
pause