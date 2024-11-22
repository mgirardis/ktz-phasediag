if [ ! -d "run" ]; then mkdir run; fi

# WARNING from ifort
# ifort: remark #10448: Intel(R) Fortran Compiler Classic (ifort) is now deprecated and will be discontinued late 2024.
# Intel recommends that customers transition now to using the LLVM-based Intel(R) Fortran Compiler (ifx) 
# for continued Windows* and Linux* support, new language support, new language features, and optimizations.
# Use '-diag-disable=10448' to disable this message.
#

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

ifort -c tokenize.f90   -O3 -parallel -fast -static -diag-disable=10448
ifort -c precision.f90  -O3 -parallel -fast -static -diag-disable=10448
ifort -c string.f90     -O3 -parallel -fast -static -diag-disable=10448
ifort -c input.f90      -O3 -parallel -fast -static -diag-disable=10448
ifort -c output.f90     -O3 -parallel -fast -static -diag-disable=10448
ifort -c chaos.f90      -O3 -parallel -fast -static -diag-disable=10448
ifort -c simulation.f90 -O3 -parallel -fast -static -diag-disable=10448
ifort tokenize.o precision.o string.o output.o input.o chaos.o simulation.o main.f90 -O3 -parallel -fast -static -diag-disable=10448 -o run/isi.exe

#rm *.obj *.mod
rm *.o
