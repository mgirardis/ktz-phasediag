# ktz-phasediag
Generates phase diagram for the KTz Neuron

# compilation
use one of the provided scripts to compile your code...

if you use gfortran, it generates 'isi_g.exe'

if you use ifort, it generates 'isi.exe'

either of them in a subfolder called 'run' relative to the source path

# running
use

    isi.exe help

to get help with the parameters

# analyses
the script 'import_isiData.m' imports the data

the script 'averageISI.m' is used to generate a structure with useful information for plotting

# reference

code written for the paper

Girardi-Schappo M, Bortolotto GS, Stenzinger RV, Gonsalves JJ, Tragtenberg MHR (2017) Phase diagrams and dynamics of a computationally efficient map-based neuron model. PLoS ONE 12(3): e0174621. https://doi.org/10.1371/journal.pone.0174621
