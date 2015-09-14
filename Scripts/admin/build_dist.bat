REM windows batch file, to build distribution of runtime
REM usage "build_dist.bat <version> <target-folder>"

REM create directories
mkdir %2\HGamer3D-Windows-%1

REM copy files 
call copy_runtime.bat %2\HGamer3D-Windows-%1
call copy_build_output.bat %2\HGamer3D-Windows-%1
