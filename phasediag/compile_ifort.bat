@echo off
if not exist run ( mkdir run )

:: In my windows, I have intel 2019 compiler, and it was supposed to work only with VS up to 2019...
:: however, I only have VS2022 BuildTools installed (no actual visual studio) and it works fine, I just had to change the 
:: %VS2019INSTALLDIR% environment variable to the path of VS2022 -> C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools

@call "%IFORT_COMPILER19%\bin\ipsxe-comp-vars.bat" intel64 vs2019

ifort /c tokenize.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c precision.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c string.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c input.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c output.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c chaos.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c simulation.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort tokenize.obj precision.obj string.obj input.obj output.obj chaos.obj simulation.obj main.f90 /O3 /Qparallel /fp:fast /Qsave /exe:run\isi.exe

del *.obj
:: del *.mod
:: C:\Windows\System32\cmd.exe /E:ON /V:ON /K ""C:\Program Files (x86)\IntelSWTools\compilers_and_libraries_2019.4.245\windows\bin\ipsxe-comp-vars.bat" intel64 vs2019"