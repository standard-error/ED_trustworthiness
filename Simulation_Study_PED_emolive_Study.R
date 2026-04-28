###################################################################
#####      Estimating trait emotion differentiation:          #####
#####          How many measurement occasions and             #####
#####               emotion items are needed?                 #####
###################################################################

###################################################################
#####                   Simulation Study                      #####
###################################################################


###################################################################
#####             Positive Emotion Differentiation            #####
###################################################################


###################################################################
#####                     emolive Data                        #####
###################################################################



# Preparation: Load Data --------------------------------------------------
load("prepared data/emolive_benchmark_data.rda")

# Make sure that ID variable is numeric
# (relevant for ICC calculation: matrix can only store one data type)
is.numeric(bench$SERIAL)


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
tictoc::tic()
# n_occasions: 14, 20, ..., 70 in steps of 10
res <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                        occasions_drawn = c("random", "by order"), n_items = c(4, 8, 12),
                        n_iteration = 5000,
                        id.var = "SERIAL", all_items = c('freude1', 'freude2', 'freude3',
                                                         'interesse1', 'interesse2', 'interesse3',
                                                         'liebe1', 'liebe2', 'liebe3',
                                                         'stolz1', 'stolz2', 'stolz3'),
                        categories = c('freude', 'freude', 'freude',
                                       'interesse', 'interesse', 'interesse',
                                       'liebe', 'liebe', 'liebe',
                                       'stolz', 'stolz', 'stolz'),
                        type = "consistency", unit = "single", occ.running.var = "occ_running",
                        item_sets_across_replications = "balanced",
                        seed_item = 260306, seed_sim = 666, cores = 11)
tictoc::toc()




# Save Results ------------------------------------------------------------
save(res, file="results/02_revision_1/sim_results_PED_emolive_Study.rda")



# Check Sufficient Number of Iterations -----------------------------------
# i.e., run simulation again with 5000 iterations and
# different seed
# -> check whether results are comparable
tictoc::tic()
res2 <- simulation_study(data = bench, n_occasions = c(14, seq(20, 70, 10)),
                         occasions_drawn = c("random", "by order"), n_items = c(4, 8, 12),
                         n_iteration = 5000,
                         id.var = "SERIAL", all_items = c('freude1', 'freude2', 'freude3',
                                                          'interesse1', 'interesse2', 'interesse3',
                                                          'liebe1', 'liebe2', 'liebe3',
                                                          'stolz1', 'stolz2', 'stolz3'),
                         categories = c('freude', 'freude', 'freude',
                                        'interesse', 'interesse', 'interesse',
                                        'liebe', 'liebe', 'liebe',
                                        'stolz', 'stolz', 'stolz'),
                         type = "consistency", unit = "single", occ.running.var = "occ_running",
                         
                         seed_item = 220224, seed_sim = 23, cores = 11)
tictoc::toc()


# save results
save(res2, file="results/02_revision_1/check nr of iterations/sim_results_PED_emolive_Study.rda")






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
