@echo off
pushd %0\..
ghc --make orthotest
echo finished
pause