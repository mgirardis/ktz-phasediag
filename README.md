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

code for the paper https://journals.plos.org/plosone/article/comments?id=10.1371/journal.pone.0174621
