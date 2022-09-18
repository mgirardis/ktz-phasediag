if [ ! -d "debug" ]; then mkdir debug; fi

ifort -c tokenize.f90 -static -g
ifort -c precision.f90 -static -g
ifort -c string.f90 -static -g
ifort -c input.f90 -static -g
ifort -c simulation.f90 -static -g
ifort -c chaos.f90 -static -g
ifort tokenize.o precision.o string.o input.o simulation.o chaos.o main.f90 -static -o debug/isi.exe -g

#rm *.o *.mod
rm *.o
