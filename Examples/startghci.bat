@echo off
echo start ghci with HGamer3D Version <VersionHG3D>

rem set path to dll's
For /F  "Tokens=*" %%I in ('start /B hg3dpath<VersionHG3DWOD>.exe') Do Set HG3DPATH=%%I

set PATH=%PATH%;%HG3DPATH%

ghci -l"%HG3DPATH%\HGamer3DOgre<VersionOgre>" -l"%HG3DPATH%\HGamer3DCEGUI<VersionCEGUI>" -l"%HG3DPATH%\HGamer3DSFML<VersionSFML>" -l"%HG3DPATH%\HGamer3DBullet<VersionBullet>" -l"%HG3DPATH%\HGamer3DEnet<VersionEnet>"
