if [ ! -d "run" ]; then mkdir run; fi

ifort -c tokenize.f90 -O3 -parallel -fast -static
ifort -c precision.f90 -O3 -parallel -fast -static
ifort -c string.f90 -O3 -parallel -fast -static
ifort -c input.f90 -O3 -parallel -fast -static
ifort -c output.f90 -O3 -parallel -fast -static
ifort -c simulation.f90 -O3 -parallel -fast -static
ifort tokenize.o precision.o string.o output.o input.o simulation.o main.f90 -O3 -parallel -fast -static -o run/isi.exe

#rm *.obj *.mod
rm *.obj
