@echo off
mkdir run
ifort /c tokenize_module.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c precision_module.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c string_module.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c input_module.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c output_module.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort /c simulation_module.f90 /O3 /Qparallel /fp:fast /Qfast-transcendentals /Qsave /nodebug
ifort tokenize_module.obj precision_module.obj string_module.obj input_module.obj output_module.obj simulation_module.obj main.f90 /O3 /Qparallel /fp:fast /Qsave /exe:run\isi.exe

del *.obj
del *.mod