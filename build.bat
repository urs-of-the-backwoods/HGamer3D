@ECHO OFF

:: we need to check existence of used commands: cmake, visual 2017
where cmake.exe >nul 2>nul
if %errorlevel%==1 (
    @echo you need cmake in your path, see https://cmake.org/download
    call :halt 1
)
call :setvar VS2017_PATH scripts\win\aio.exe http://www.hgamer3d.org/tools/DetectVS.0717
if "%VS2017_PATH%"=="" (
	@echo you need Visual Studio 2017, see https://www.visualstudio.com/de/downloads
	call :halt 1
)

:: do the work
scripts\win\aio.exe http://www.hgamer3d.org/tools/Lua.0717 scripts\build.lua %1 %2 %3 %4 %5 %6
goto :eof

:: helper routines below

:: Sets the errorlevel and stops the batch immediately
:halt
call :__SetErrorLevel %1
call :__ErrorExit 2> nul
goto :eof

:__ErrorExit
:: Creates a syntax error, stops immediately
() 
goto :eof

:__SetErrorLevel
exit /b %1
goto :eof

:: Get command from argument 
:setvar
for /F "tokens=1,*" %%a in ("%*") do set cmd=%%b

:: Get output and set var
for /F "usebackq delims=" %%a in (`%cmd%`) do (
     ENDLOCAL
     set %1=%%a
)
goto :eof