REM windows batch file, to install distribution of runtime in APPFOLDER
REM usage "install.bat"
REM create directories
mkdir %APPDATA%\HGamer3D

REM copy files 
call copy_runtime.bat %APPDATA%\HGamer3D
call copy_build_output.bat %APPDATA%\HGamer3D

REM install haskell files
cabal install --extra-lib-dirs=%APPDATA%\HGamer3D ..\Build\Output\HGamer3D-0.6.0.tar.gz
