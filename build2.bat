@echo off
pushd %0\..
ghc --make  ccsrc
echo finished
pause