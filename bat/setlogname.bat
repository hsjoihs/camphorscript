echo.%time:~0,1% > bat\space.tmp
fc bat\space.tmp bat\space.txt >nul
if %errorlevel% == 0  ( 
set now_HHmmSS=0%time:~1,1%-%time:~3,2%-%time:~6,2%
) else (
set now_HHmmSS=%time:~0,2%-%time:~3,2%-%time:~6,2%
)
set today_YYYYMMDD=%date:~0,4%-%date:~5,2%-%date:~8,2%
set LOG2=%1%today_YYYYMMDD%_%now_HHmmSS%.log
set LOG="%LOG2%"
exit /b