#!/bin/bash
#PBS  -l walltime=12:00:00,cput=12:00:00 
#PBS -N dragonsim
#PBS -v DSROOT,DSSOURCE,DSBIN,INPUT,MITRAY,OUTPATH
cd $OUTPATH
export FFCARD="dragon.ffcards"
$DSSOURCE/dsbatch >& $PBS_JOBID.log 
##To run a series of jobs with different FFCARD command files dragon.ffcards:
##
## 1) create the command files in directories eg d1 d2 ... off of DSBIN which
##   has the dssub.csh script.
##
## 2)submit the jobs using dssub
## foreach i (d1 d2 ...)
## export OUTPATH="$DSBIN/$i"
## qsub dssub.csh
##end

