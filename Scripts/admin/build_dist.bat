REM windows batch file, to build distribution of runtime
REM usage "build_dist.bat <version> <target-folder>"
REM to be called from HGamer3D

REM create directories
mkdir %2\HGamer3D-Windows-%1

REM copy files 
xcopy /Y /S ..\Runtime-Binaries\* %2\HGamer3D-Windows-%1
xcopy /Y /S Source\Runtime\Data\* %2\HGamer3D-Windows-%1\Data
xcopy /Y Source\Runtime\README_RUNTIME.txt %2\HGamer3D-Windows-%1
xcopy /Y LICENSE.txt %2\HGamer3D-Windows-%1
xcopy /Y /S ..\Build\Haskell-Source\.stack-work\install\i386-windows\lts-3.4\7.10.2\bin\* %2\HGamer3D-Windows-%1
xcopy /Y /S C:\HGamer3D_RT\* %2\HGamer3D-Windows-%1
