@echo off
pushd %0\..
%~d0\ghc2\ghc\2013.2.0.0\bin\ghc --make -O ccsc
echo finished
pause