mkdir run
ifort -c tokenize_module.f90 -O3 -parallel -fast -static
ifort -c precision_module.f90 -O3 -parallel -fast -static
ifort -c string_module.f90 -O3 -parallel -fast -static
ifort -c input_module.f90 -O3 -parallel -fast -static
ifort -c output_module.f90 -O3 -parallel -fast -static
ifort -c simulation_module.f90 -O3 -parallel -fast -static
ifort tokenize_module.o precision_module.o string_module.o output_module.o input_module.o simulation_module.o main.f90 -O3 -parallel -fast -static -o run/isi.exe

rm *.obj *.mod
