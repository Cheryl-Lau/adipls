# adipls

MScR Project - Stellar rotation formalisms of γ Doradus stars from gravity-mode period spacings

(Updated 14/12/2019)

This file contains the source codes in mesa-r10398/adipls/adipack.c/adipls,
edited for controlling rotation computation with 2nd order perturbative method through MESA. 

The codes are coupled to the edited version of MESA astero src files.

Settings are controlled with the input file inlist_pulse_controls in astero work directories 
(e.g. work_bench_adipls), not the original adipls.c.in or adipls.c.pruned.in. 

For instructions on how to use, please refer to the astero repo README.md

Note that this repo does not include the object files, you will have to recompile the entire MESA 
(like how you downloaded it) before use. 





