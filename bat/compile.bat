setlocal
set folder=%1
set compiler=%2
set option=%3
set option2=%~4
set source=%folder%\%5
set tmp=bat\%6.tmp
set judger=%folder%\%7
set test=%compiler% %option% %option2% %source%

rem print without linefeed
set /p out=testing %test%: < nul
%test% -o %tmp%
fc /n %tmp% %judger% > nul
call :output %errorlevel% "%test% -o %tmp%"
endlocal
exit /b

:output
if     %1 == 0 (
echo success
echo success: %2 >> %LOG%
)
if not %1 == 0 (
echo FAILURE
echo FAILURE: %2 >> %LOG%
type nul > %FAIL%
) 
exit /b