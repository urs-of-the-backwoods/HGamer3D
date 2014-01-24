@echo off
echo compiling %1.hs to hg3dexample.exe - Version <VersionHG3D>

rem set path to dll's
For /F  "Tokens=*" %%I in ('start /B hg3dpath<VersionHG3DWOD>.exe') Do Set HG3DPATH=%%I

rem compile and link your file
ghc -package HGamer3D -o %1.exe -optl-mwindows %1.hs "%HG3DPATH%\HGamer3DOgre<VersionOgre>.dll" "%HG3DPATH%\HGamer3DCEGUI<VersionCEGUI>.dll" "%HG3DPATH%\HGamer3DSFML<VersionSFML>.dll" "%HG3DPATH%\HGamer3DBullet<VersionBullet>.dll" "%HG3DPATH%\HGamer3DEnet<VersionEnet>.dll"

rem rename executable
if exist hg3dexample.exe (del hg3dexample.exe)
rename %1.exe hg3dexample.exe

rem delete intermediaries
del *.o *.hi 

pause
