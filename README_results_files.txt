The file names indicate both the outcome and the empirical data set
used for the simulations:
- NED = negative emotion differentiation
- PED = positive emotion differentiation
- emolive = primary data set
- EMOTIONS = replication data set

The main result files (e.g., sim_results_NED_emolive_Study.rda) 
contain the primary analysis (or replication analysis, respectively)
as well as the following sensitivity analyses due to the way the
simulation was programmed:
- ordered draws of measurement occasions
- Fisher's Z-transformed intraclass correlations
- item-set endorsement.

Sensitivity analyses varying the treatment of negative intraclass
correlations are stored in separate data frames (indicated by "exclude"
or "set zero" in the file names). 

Due to the way the simulation was programmed, all analyses were implemented
in a fully crossed manner (e.g., setting negative intraclass correlations
to zero combined with ordered draws of measurement occasions). However, the
manuscript reports only the individual sensitivity analyses (i.e.,
those comparing each sensitivity analysis with the primary analysis
using ranodm draws of measurement occasions and raw intraclass
correlations). The additional crossed sensitivity analyses are 
retained in the result files for completeness, but are not reported
in the manuscript.