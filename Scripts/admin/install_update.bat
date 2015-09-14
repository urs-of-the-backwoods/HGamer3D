REM windows batch file, to install an update (dll and Haskell package) in APPFOLDER
REM usage "install_update.bat"

REM create directories
mkdir %APPDATA%\HGamer3D

REM copy only build output, runtime should be there 
call copy_build_output.bat %APPDATA%\HGamer3D

REM install haskell files
cabal install --extra-lib-dirs=%APPDATA%\HGamer3D ..\Build\Output\HGamer3D-0.6.0.tar.gz
