if (-not (Test-Path -Path run)) { mkdir run }

gfortran.exe -c tokenize.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c precision.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c string.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c output.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c input.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c simulation.f90 -O3 -Ofast -fbounds-check
gfortran.exe tokenize.o precision.o string.o output.o input.o simulation.o main.f90 -O3 -Ofast -fbounds-check -o run/isi_g.exe

rm *.o
#rm *.mod
