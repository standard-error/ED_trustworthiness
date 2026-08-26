###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
##### Simulation Study: Robustness Check Negative ICC Handling ####   
###################################################################


###################################################################
#####             Positive Emotion Differentiation            #####
###################################################################


###################################################################
#####                    EMOTIONS Data                        #####
###################################################################



# Preparation: Load Data --------------------------------------------------
load("prepared data/EMOTIONS_benchmark_data.rda")

# Make sure that ID variable is numeric
# (relevant for ICC calculation: matrix can only store one data type)
is.numeric(bench$id)

# use only positive emotions:
bench <- bench[ , c("id", "occ_running",
                    "proud", "success", "superior",
                    "enthusiastic", "relaxed")]





# Source Functions --------------------------------------------------------
source("functions/function_simulation_study.R")
# simulation study function already sources the other functions needed
# also already loads packages that are needed (future, future.apply)




# Global Seed Item Set ----------------------------------------------------

##### IMPORTANT: IF ITEM SETS SHALL BE FIXED ACROSS REPLICATIONS
##### USE THE SAME SEED SO THAT THE SAME ITEM SETS
##### ARE DRAWN FOR OVERALL SIMULATION IF RUN TWICE 

# source("Global_Seed_Item_Sets.R")

# here: not necessary, because item sets shall be drawn in balanced manner
# across replications


# Run Simulation ----------------------------------------------------------
# Robustness checks: different ICC handlings (set to zero, exclude)



# '' Set negative ICCs to zero --------------------------------------------



tictoc::tic()
# n_occasions: 14, 20, ..., 70 in steps of 10
setzero <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                            occasions_drawn = c("random", "by order"), n_items = c(3,4,5),
                            n_iteration = 5000,
                            id.var = "id", all_items = c("proud", "success", "superior",
                                                         "enthusiastic", "relaxed"),
                            categories = NULL,
                            type = "consistency", unit = "single",
                            negative_icc_handling = "set to zero",
                            occ.running.var = "occ_running",
                            item_sets_across_replications = "balanced",
                            seed_item = 3591, seed_sim = 4713, cores = 11)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(setzero, file="results/02_revision_1/EMOTIONS study/PED/neg ICC handling/setzero/raw/sim_results_PED_set_neg_ICC_zero_EMOTIONS_Study.rda")




# '' Exclude Negative ICCs ------------------------------------------------
# n_occasions: 14, 20, ..., 70 in steps of 10
tictoc::tic()
excl <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                         occasions_drawn = c("random", "by order"), n_items = c(3,4,5),
                         n_iteration = 5000,
                         id.var = "id", all_items = c("proud", "success", "superior",
                                                      "enthusiastic", "relaxed"),
                         categories = NULL,
                         type = "consistency", unit = "single",
                         negative_icc_handling = "exclude",
                         occ.running.var = "occ_running",
                         item_sets_across_replications = "balanced",
                         seed_item = 1042, seed_sim = 1361918, cores = 11)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(excl, file="results/02_revision_1/EMOTIONS study/PED/neg ICC handling/exclude/raw/sim_results_PED_exclude_neg_ICC_EMOTIONS_Study.rda")




# Session Info ------------------------------------------------------------
sessionInfo()


# R version 4.5.3 (2026-03-11 ucrt)
# Platform: x86_64-w64-mingw32/x64
# Running under: Windows 11 x64 (build 26200)
# 
# Matrix products: default
#   LAPACK version 3.12.1
# 
# locale:
# [1] LC_COLLATE=German_Germany.utf8  LC_CTYPE=German_Germany.utf8   
# [3] LC_MONETARY=German_Germany.utf8 LC_NUMERIC=C                   
# [5] LC_TIME=German_Germany.utf8    
# 
# time zone: Europe/Berlin
# tzcode source: internal
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] future.apply_1.20.2 future_1.70.0      
# 
# loaded via a namespace (and not attached):
#  [1] compiler_4.5.3    tictoc_1.2.1      parallelly_1.46.1 parallel_4.5.3   
#  [5] tools_4.5.3       rstudioapi_0.18.0 listenv_0.10.1    codetools_0.2-20 
#  [9] irr_0.84.1        digest_0.6.39     globals_0.19.1    lpSolve_5.6.23

