Write-Host ""
Write-Host ""
Write-Host "Not compatible with gfortran because of the IFPORT module -- only intel compiler possible or work around the IFPORT"
Write-Host ""
Write-Host ""
exit

if (-not (Test-Path -Path debug)) { mkdir debug }

gfortran.exe -c tokenize.f90 -O3 -Og -fbounds-check
gfortran.exe -c precision.f90 -O3 -Og -fbounds-check
gfortran.exe -c string.f90 -O3 -Og -fbounds-check
gfortran.exe -c output.f90 -O3 -Ofast -fbounds-check
gfortran.exe -c input.f90 -O3 -Og -fbounds-check
gfortran.exe -c chaos.f90 -O3 -Og -fbounds-check
gfortran.exe -c simulation.f90 -O3 -Og -fbounds-check
gfortran.exe tokenize.o precision.o string.o output.o input.o chaos.o simulation.o main.f90 -O3 -Og -fbounds-check -fdump-core -o debug/isi.exe

rm *.o
#rm *.mod
