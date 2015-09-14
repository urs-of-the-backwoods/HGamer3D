REM windows batch file, to copy runtime to a distribution folder
REM usage "copy_runtime.bat <target-folder>"

xcopy /Y /S Data\* %1
xcopy /Y README_RUNTIME.txt %1
xcopy /Y ..\LICENSE.txt %1
xcopy /Y /S ..\..\Runtime-Binaries\* %1
