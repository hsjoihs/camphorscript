setlocal
set file=%1
echo %LOG2%: > bat\%file%.tmp
type %LOG% >>  bat\%file%.tmp
echo. >>  bat\%file%.tmp
type %file%.log >> bat\%file%.tmp
type bat\%file%.tmp > %file%.log
echo.
echo %LOG2%:
type %LOG%
del %LOG%
if exist %FAIL% (
del %FAIL%
) else (
del bat\*.tmp
echo ---Deleted temporary files.---
) 
endlocal
exit /b
