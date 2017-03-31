# Geant3

## Prerequisites:

The GEANT3 simulation of DRAGON requires [cernlib2006](http://cernlib.web.cern.ch/cernlib/index.html), [ROOT](https://root.cern.ch), and a g77 compiler.

## Quick Start:

To compile the batch and interactive versions of the simulation, cd to src and type
```
> make dsbatch && make dsinter
```

This will create the binary executables in the bin directory. To run an example simulation with 100 events using the 26Al(p,γ)27Si reaction
(i.e. - using the included 26alpg.dat file in the reaction definition card) type
```
> source dsinit.sh
> ./bin/dsbatch
```

This will produce the file dragon1.hbook. Convert it to a rootfile by typing
```
> h2root dragon1.hbook
```

## Detailed Instructions:

The detection efficiency of DRAGON's BGO γ-ray array as well as the DRAGON separator transmission can be calculated by running the GEANT3 
simulation of the DRAGON separator. To calculate these values for a given reaction at a given observation energy, one must set up an input 
file (e.g 26alpg.dat included in the repository). Up to 15 energy levels are allowed, each with up to 6 decay modes and branching ratios. 
Be sure to set the correct level energies (in MeV), lifetimes (in seconds), branching ratios (expressed in as a percentage of the sum of 
all the intensities in www.nndc.bnl.gov style units, so that all the branching ratios add up to 100%) and the decay modes. Only include 
the levels that actually have cascades from the resonance of interest, to minimize the number of levels. Be sure to set the variable 
'rstate' to the index of the energy level via which the resonance proceeds. Also be sure to set the C.M. resonance energy as Ex - Q. Check 
the file geant/src/angdist to see that the angular distribution is set to either 1 (isotropic) or a function (like the quadrupole one 
listed). Change to what you want and then (if necessary) recompile by typing
```
> make dsbatch && dsinter
```

Edit 'dsinit.sh' so that it lists the correct input file (i.e. - INPUT="$DSROOT/26alpg.dat"). Type 
```
> source dsinit.sh
```

Run about 5,000 events in GEANT (edit the 'TRIG" variable in 'dragon_2003.ffcards' to suit). One can run the simulation in the background 
using the command:
```
> nohup ./dsbatch &
```

When the run is finished, the output file will be called 'dragonXX.hbook', where XX is the number following RUNG defined in the
'dragon_2003.ffcards' file.  Convert this file to a root file of your choice by typing, for example:
```
> h2root dragon01.hbook 26alpg_368keV.root
```

Copy the output root file to the directory 'BGO Efficiency' in dragon@isdaq04:/home/dragon/
Submit the output root file to the DRAGON elog 'BGO Efficiency' in dragon@isdaq04:/home/dragon/
The file containing the thresholds, 'thresholds.root', is already in that directory, all you have to do is run the efficiency.C macro by 
typing:
```
root 26alpg_368keV.root
```

to start a ROOT session, then:
```
root> .L efficiency.C
```

to load the Macro, then:
```
root> efficiency("23napg_646keV.root")
```

You will be prompted to enter a number from 1 to 4 to choose which thresholds were used. Once this has been entered, the macro will spit 
out a number for the efficiency based on the total number of counts above threshold in all BGOs, plus a statistical error based on the 
number of detected counts. To run again for a different threshold, reload the macro and run again.


