mkdir run
gfortran.exe -c tokenize_module.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c precision_module.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c string_module.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c output_module.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c input_module.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c simulation_module.f90 -O3 -Ofast -fbounds-check
gfortran.exe tokenize_module.o precision_module.o string_module.o output_module.o input_module.o simulation_module.o main.f90 -O3 -Ofast -fbounds-check -o run/isi_g.exe

rm *.o
rm *.mod
