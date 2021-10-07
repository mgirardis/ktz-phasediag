if [ ! -d "run" ]; then mkdir run; fi

gfortran -c tokenize.f90 -O3 -Ofast -fbounds-check
gfortran -c precision.f90 -O3 -Ofast -fbounds-check
gfortran -c string.f90 -O3 -Ofast -fbounds-check
gfortran -c output.f90 -O3 -Ofast -fbounds-check
gfortran -c input.f90 -O3 -Ofast -fbounds-check
gfortran -c simulation.f90 -O3 -Ofast -fbounds-check
gfortran tokenize.o precision.o string.o output.o input.o simulation.o main.f90 -O3 -Ofast -fbounds-check -o run/isi_g.exe

#rm *.o *.mod
rm *.o
