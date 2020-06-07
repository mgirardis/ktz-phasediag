mkdir run
gfortran -c tokenize_module.f90 -O3 -Ofast -fbounds-check
gfortran -c precision_module.f90 -O3 -Ofast -fbounds-check
gfortran -c string_module.f90 -O3 -Ofast -fbounds-check
gfortran -c output_module.f90 -O3 -Ofast -fbounds-check
gfortran -c input_module.f90 -O3 -Ofast -fbounds-check
gfortran -c simulation_module.f90 -O3 -Ofast -fbounds-check
gfortran tokenize_module.o precision_module.o string_module.o output_module.o input_module.o simulation_module.o main.f90 -O3 -Ofast -fbounds-check -o run/isi_g.exe

rm *.o *.mod
