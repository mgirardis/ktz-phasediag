if [ ! -d "run" ]; then mkdir run; fi

if ! command -v ifort &> /dev/null; then
    # set PATH so it includes user's private bin if it exists
    if [ -d "$HOME/intel" ] ; then
        . "$HOME/intel/oneapi/setvars.sh"
    else
        if [ -d "/opt/intel" ] ; then
            source /opt/intel/oneapi/setvars.sh intel64
        fi
    fi
fi

ifort -c tokenize.f90 -O3 -parallel -fast -static
ifort -c precision.f90 -O3 -parallel -fast -static
ifort -c string.f90 -O3 -parallel -fast -static
ifort -c input.f90 -O3 -parallel -fast -static
ifort -c output.f90 -O3 -parallel -fast -static
ifort -c chaos.f90 -O3 -parallel -fast -static
ifort -c simulation.f90 -O3 -parallel -fast -static
ifort tokenize.o precision.o string.o output.o input.o chaos.o simulation.o main.f90 -O3 -parallel -fast -static -o run/isi.exe

#rm *.obj *.mod
rm *.o
