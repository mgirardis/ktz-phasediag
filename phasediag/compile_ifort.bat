@echo off
if not exist run ( mkdir run )

@call "%IFORT_COMPILER19%\bin\ipsxe-comp-vars.bat" intel64 vs2019

ifort /c tokenize.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c precision.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c string.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c input.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c output.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c simulation.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c chaos.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort tokenize.obj precision.obj string.obj input.obj output.obj simulation.obj chaos.obj main.f90 /O3 /Qparallel /fp:fast /Qsave /exe:run\isi.exe

del *.obj
:: del *.mod