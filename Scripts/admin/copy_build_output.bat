REM windows batch file, to copy build output to a distribution folder
REM usage "copy_build_output.bat <target-folder>"

xcopy /Y /S ..\Build\Output\* %1
